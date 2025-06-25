terraform {
  required_version = ">= 1.0.0"
  
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.0"
    }
  }
  
  backend "s3" {
    # This will be filled in by the GitHub Actions workflow
  }
}

# Configure the AWS Provider
provider "aws" {
  region = var.aws_region
  
  default_tags {
    tags = {
      Environment = var.environment
      Project     = var.project_name
      ManagedBy   = "Terraform"
    }
  }
}

# Get the current AWS account ID
data "aws_caller_identity" "current" {}

# Get the current AWS region
data "aws_region" "current" {}

# Create an S3 bucket for static website hosting
resource "aws_s3_bucket" "website" {
  count = var.enable_aws ? 1 : 0
  
  bucket = "${var.project_name}-${var.environment}-website"
  acl    = "private"
  
  versioning {
    enabled = true
  }
  
  server_side_encryption_configuration {
    rule {
      apply_server_side_encryption_by_default {
        sse_algorithm = "AES256"
      }
    }
  }
  
  website {
    index_document = "index.html"
    error_document = "index.html" # SPA fallback
  }
  
  lifecycle_rule {
    enabled = true
    
    noncurrent_version_expiration {
      days = 30
    }
  }
  
  tags = {
    Name = "${var.project_name}-${var.environment}-website"
  }
}

# CloudFront distribution for the S3 bucket
resource "aws_cloudfront_distribution" "website" {
  count = var.enable_aws ? 1 : 0
  
  origin {
    domain_name = aws_s3_bucket.website[0].bucket_regional_domain_name
    origin_id   = "S3-${aws_s3_bucket.website[0].id}"
    
    s3_origin_config {
      origin_access_identity = aws_cloudfront_origin_access_identity.website[0].cloudfront_access_identity_path
    }
  }
  
  enabled             = true
  is_ipv6_enabled     = true
  default_root_object = "index.html"
  
  aliases = var.custom_domains
  
  default_cache_behavior {
    allowed_methods  = ["GET", "HEAD", "OPTIONS"]
    cached_methods   = ["GET", "HEAD"]
    target_origin_id = "S3-${aws_s3_bucket.website[0].id}"
    
    forwarded_values {
      query_string = false
      
      cookies {
        forward = "none"
      }
    }
    
    viewer_protocol_policy = "redirect-to-https"
    min_ttl                = 0
    default_ttl            = 3600
    max_ttl                = 86400
  }
  
  # Price class for North America and Europe
  price_class = "PriceClass_100"
  
  # Custom error responses for SPA routing
  dynamic "custom_error_response" {
    for_each = [400, 403, 404, 405, 414, 416, 500, 501, 502, 503, 504]
    
    content {
      error_caching_min_ttl = 300
      error_code            = custom_error_response.value
      response_code         = custom_error_response.value == 403 ? 200 : custom_error_response.value == 404 ? 200 : null
      response_page_path    = "/index.html"
    }
  }
  
  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }
  
  viewer_certificate {
    acm_certificate_arn      = var.acm_certificate_arn
    ssl_support_method       = "sni-only"
    minimum_protocol_version = "TLSv1.2_2021"
  }
  
  tags = {
    Name = "${var.project_name}-${var.environment}-distribution"
  }
}

# CloudFront OAI for S3 bucket access
resource "aws_cloudfront_origin_access_identity" "website" {
  count = var.enable_aws ? 1 : 0
  
  comment = "OAI for ${var.project_name}-${var.environment}"
}

# S3 bucket policy to allow CloudFront access
resource "aws_s3_bucket_policy" "website" {
  count = var.enable_aws ? 1 : 0
  
  bucket = aws_s3_bucket.website[0].id
  
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid       = "AllowCloudFrontServicePrincipal"
        Effect    = "Allow"
        Principal = {
          Service = "cloudfront.amazonaws.com"
        }
        Action   = "s3:GetObject"
        Resource = "${aws_s3_bucket.website[0].arn}/*"
        Condition = {
          StringEquals = {
            "AWS:SourceArn" = "arn:aws:cloudfront::${data.aws_caller_identity.current.account_id}:distribution/${aws_cloudfront_distribution.website[0].id}"
          }
        }
      }
    ]
  })
}

# Route 53 record for the CloudFront distribution
resource "aws_route53_record" "website" {
  count = var.enable_aws && length(var.custom_domains) > 0 ? 1 : 0
  
  zone_id = var.route53_zone_id
  name    = var.custom_domains[0]
  type    = "A"
  
  alias {
    name                   = aws_cloudfront_distribution.website[0].domain_name
    zone_id                = aws_cloudfront_distribution.website[0].hosted_zone_id
    evaluate_target_health = false
  }
}

# Outputs
output "cloudfront_distribution_id" {
  description = "The ID of the CloudFront distribution"
  value       = var.enable_aws ? aws_cloudfront_distribution.website[0].id : ""
}

output "cloudfront_domain_name" {
  description = "The domain name of the CloudFront distribution"
  value       = var.enable_aws ? aws_cloudfront_distribution.website[0].domain_name : ""
}

output "s3_bucket_name" {
  description = "The name of the S3 bucket"
  value       = var.enable_aws ? aws_s3_bucket.website[0].id : ""
}

output "s3_bucket_arn" {
  description = "The ARN of the S3 bucket"
  value       = var.enable_aws ? aws_s3_bucket.website[0].arn : ""
}
