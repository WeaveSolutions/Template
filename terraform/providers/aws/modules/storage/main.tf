# Storage Module - S3 Buckets for assets, uploads, and backups
# Following Well-Architected Framework for secure and efficient storage

locals {
  assets_bucket_name  = "${var.project_name}-${var.environment}-assets"
  uploads_bucket_name = "${var.project_name}-${var.environment}-uploads"
  backups_bucket_name = "${var.project_name}-${var.environment}-backups"
}

# KMS Key for S3 encryption
resource "aws_kms_key" "s3" {
  description             = "KMS key for S3 bucket encryption"
  deletion_window_in_days = var.kms_deletion_window
  enable_key_rotation     = true

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-s3-kms"
    }
  )
}

resource "aws_kms_alias" "s3" {
  name          = "alias/${var.project_name}-${var.environment}-s3"
  target_key_id = aws_kms_key.s3.key_id
}

# Assets Bucket (for static files)
resource "aws_s3_bucket" "assets" {
  bucket = local.assets_bucket_name

  tags = merge(
    var.common_tags,
    {
      Name = local.assets_bucket_name
      Type = "assets"
    }
  )
}

resource "aws_s3_bucket_versioning" "assets" {
  bucket = aws_s3_bucket.assets.id
  
  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_server_side_encryption_configuration" "assets" {
  bucket = aws_s3_bucket.assets.id

  rule {
    apply_server_side_encryption_by_default {
      kms_master_key_id = aws_kms_key.s3.arn
      sse_algorithm     = "aws:kms"
    }
  }
}

resource "aws_s3_bucket_public_access_block" "assets" {
  bucket = aws_s3_bucket.assets.id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

# Uploads Bucket (for user uploads)
resource "aws_s3_bucket" "uploads" {
  bucket = local.uploads_bucket_name

  tags = merge(
    var.common_tags,
    {
      Name = local.uploads_bucket_name
      Type = "uploads"
    }
  )
}

resource "aws_s3_bucket_versioning" "uploads" {
  bucket = aws_s3_bucket.uploads.id
  
  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_lifecycle_configuration" "uploads" {
  bucket = aws_s3_bucket.uploads.id

  rule {
    id = "transition-to-ia"

    transition {
      days          = 30
      storage_class = "STANDARD_IA"
    }

    transition {
      days          = 90
      storage_class = "GLACIER"
    }

    status = "Enabled"
  }

  rule {
    id = "delete-old-versions"

    noncurrent_version_expiration {
      noncurrent_days = 90
    }

    status = "Enabled"
  }
}

resource "aws_s3_bucket_server_side_encryption_configuration" "uploads" {
  bucket = aws_s3_bucket.uploads.id

  rule {
    apply_server_side_encryption_by_default {
      kms_master_key_id = aws_kms_key.s3.arn
      sse_algorithm     = "aws:kms"
    }
  }
}

resource "aws_s3_bucket_public_access_block" "uploads" {
  bucket = aws_s3_bucket.uploads.id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

# CORS configuration for uploads bucket
resource "aws_s3_bucket_cors_configuration" "uploads" {
  bucket = aws_s3_bucket.uploads.id

  cors_rule {
    allowed_headers = ["*"]
    allowed_methods = ["GET", "PUT", "POST", "DELETE", "HEAD"]
    allowed_origins = var.cors_allowed_origins
    expose_headers  = ["ETag"]
    max_age_seconds = 3000
  }
}

# Backups Bucket
resource "aws_s3_bucket" "backups" {
  bucket = local.backups_bucket_name

  tags = merge(
    var.common_tags,
    {
      Name = local.backups_bucket_name
      Type = "backups"
    }
  )
}

resource "aws_s3_bucket_versioning" "backups" {
  bucket = aws_s3_bucket.backups.id
  
  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_lifecycle_configuration" "backups" {
  bucket = aws_s3_bucket.backups.id

  rule {
    id = "transition-backups"

    transition {
      days          = 7
      storage_class = "STANDARD_IA"
    }

    transition {
      days          = 30
      storage_class = "GLACIER"
    }

    transition {
      days          = 180
      storage_class = "DEEP_ARCHIVE"
    }

    expiration {
      days = var.backup_retention_days
    }

    status = "Enabled"
  }
}

resource "aws_s3_bucket_server_side_encryption_configuration" "backups" {
  bucket = aws_s3_bucket.backups.id

  rule {
    apply_server_side_encryption_by_default {
      kms_master_key_id = aws_kms_key.s3.arn
      sse_algorithm     = "aws:kms"
    }
  }
}

resource "aws_s3_bucket_public_access_block" "backups" {
  bucket = aws_s3_bucket.backups.id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

# S3 Bucket Policies
resource "aws_s3_bucket_policy" "assets" {
  bucket = aws_s3_bucket.assets.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid    = "AllowCloudFrontAccess"
        Effect = "Allow"
        Principal = {
          AWS = var.cloudfront_oai_arn
        }
        Action   = "s3:GetObject"
        Resource = "${aws_s3_bucket.assets.arn}/*"
      },
      {
        Sid    = "DenyInsecureTransport"
        Effect = "Deny"
        Principal = "*"
        Action = "*"
        Resource = [
          aws_s3_bucket.assets.arn,
          "${aws_s3_bucket.assets.arn}/*"
        ]
        Condition = {
          Bool = {
            "aws:SecureTransport" = "false"
          }
        }
      }
    ]
  })
}

# S3 Bucket Logging
resource "aws_s3_bucket_logging" "assets" {
  bucket = aws_s3_bucket.assets.id

  target_bucket = aws_s3_bucket.logs.id
  target_prefix = "s3-access-logs/assets/"
}

resource "aws_s3_bucket_logging" "uploads" {
  bucket = aws_s3_bucket.uploads.id

  target_bucket = aws_s3_bucket.logs.id
  target_prefix = "s3-access-logs/uploads/"
}

# Logs Bucket
resource "aws_s3_bucket" "logs" {
  bucket = "${var.project_name}-${var.environment}-logs"

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-logs"
      Type = "logs"
    }
  )
}

resource "aws_s3_bucket_acl" "logs" {
  bucket = aws_s3_bucket.logs.id
  acl    = "log-delivery-write"
}

resource "aws_s3_bucket_lifecycle_configuration" "logs" {
  bucket = aws_s3_bucket.logs.id

  rule {
    id = "delete-old-logs"

    expiration {
      days = 90
    }

    status = "Enabled"
  }
}

resource "aws_s3_bucket_public_access_block" "logs" {
  bucket = aws_s3_bucket.logs.id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

# S3 Bucket Notifications
resource "aws_s3_bucket_notification" "uploads" {
  bucket = aws_s3_bucket.uploads.id

  lambda_function {
    lambda_function_arn = var.upload_processor_lambda_arn
    events              = ["s3:ObjectCreated:*"]
    filter_prefix       = "incoming/"
  }
}

# CloudWatch Metrics
resource "aws_s3_bucket_metric" "assets" {
  bucket = aws_s3_bucket.assets.id
  name   = "entire-bucket"
}

resource "aws_s3_bucket_metric" "uploads" {
  bucket = aws_s3_bucket.uploads.id
  name   = "entire-bucket"
}

# S3 Inventory for compliance
resource "aws_s3_bucket_inventory" "assets" {
  bucket = aws_s3_bucket.assets.id
  name   = "weekly-inventory"

  included_object_versions = "Current"

  schedule {
    frequency = "Weekly"
  }

  destination {
    bucket {
      bucket_arn = aws_s3_bucket.logs.arn
      prefix     = "inventory"
      format     = "CSV"
      
      encryption {
        sse_kms {
          key_id = aws_kms_key.s3.arn
        }
      }
    }
  }

  optional_fields = [
    "Size",
    "LastModifiedDate",
    "StorageClass",
    "ETag",
    "IsMultipartUploaded",
    "ReplicationStatus",
    "EncryptionStatus"
  ]
}
