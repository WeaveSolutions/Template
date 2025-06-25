# Security Module - WAF, GuardDuty, and Security Hub
# Following Well-Architected Framework for security

# WAF v2 Web ACL
resource "aws_wafv2_web_acl" "main" {
  name  = "${var.project_name}-${var.environment}-waf"
  scope = "CLOUDFRONT"

  default_action {
    allow {}
  }

  # AWS Managed Core Rule Set
  rule {
    name     = "AWSManagedRulesCommonRuleSet"
    priority = 1

    override_action {
      none {}
    }

    statement {
      managed_rule_group_statement {
        name        = "AWSManagedRulesCommonRuleSet"
        vendor_name = "AWS"
      }
    }

    visibility_config {
      cloudwatch_metrics_enabled = true
      metric_name                = "AWSManagedRulesCommonRuleSetMetric"
      sampled_requests_enabled   = true
    }
  }

  # AWS Managed Known Bad Inputs Rule Set
  rule {
    name     = "AWSManagedRulesKnownBadInputsRuleSet"
    priority = 2

    override_action {
      none {}
    }

    statement {
      managed_rule_group_statement {
        name        = "AWSManagedRulesKnownBadInputsRuleSet"
        vendor_name = "AWS"
      }
    }

    visibility_config {
      cloudwatch_metrics_enabled = true
      metric_name                = "AWSManagedRulesKnownBadInputsRuleSetMetric"
      sampled_requests_enabled   = true
    }
  }

  # Rate limiting rule
  rule {
    name     = "RateLimitRule"
    priority = 3

    action {
      block {}
    }

    statement {
      rate_based_statement {
        limit              = 2000
        aggregate_key_type = "IP"
      }
    }

    visibility_config {
      cloudwatch_metrics_enabled = true
      metric_name                = "RateLimitRule"
      sampled_requests_enabled   = true
    }
  }

  # Geo-blocking rule (optional)
  dynamic "rule" {
    for_each = length(var.blocked_countries) > 0 ? [1] : []
    
    content {
      name     = "GeoBlockingRule"
      priority = 4

      action {
        block {}
      }

      statement {
        geo_match_statement {
          country_codes = var.blocked_countries
        }
      }

      visibility_config {
        cloudwatch_metrics_enabled = true
        metric_name                = "GeoBlockingRule"
        sampled_requests_enabled   = true
      }
    }
  }

  visibility_config {
    cloudwatch_metrics_enabled = true
    metric_name                = "${var.project_name}-${var.environment}-waf"
    sampled_requests_enabled   = true
  }

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-waf"
    }
  )
}

# WAF Logging
resource "aws_wafv2_web_acl_logging_configuration" "main" {
  resource_arn            = aws_wafv2_web_acl.main.arn
  log_destination_configs = [aws_kinesis_firehose_delivery_stream.waf_logs.arn]

  redacted_fields {
    single_header {
      name = "authorization"
    }
  }

  redacted_fields {
    single_header {
      name = "cookie"
    }
  }
}

# Kinesis Firehose for WAF logs
resource "aws_kinesis_firehose_delivery_stream" "waf_logs" {
  name        = "aws-waf-logs-${var.project_name}-${var.environment}"
  destination = "extended_s3"

  extended_s3_configuration {
    role_arn   = aws_iam_role.firehose.arn
    bucket_arn = aws_s3_bucket.waf_logs.arn
    prefix     = "waf-logs/"
    
    compression_format = "GZIP"
    
    cloudwatch_logging_options {
      enabled         = true
      log_group_name  = aws_cloudwatch_log_group.firehose.name
      log_stream_name = aws_cloudwatch_log_stream.firehose.name
    }
  }

  tags = var.common_tags
}

# S3 bucket for WAF logs
resource "aws_s3_bucket" "waf_logs" {
  bucket = "${var.project_name}-${var.environment}-waf-logs"

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-waf-logs"
    }
  )
}

resource "aws_s3_bucket_lifecycle_configuration" "waf_logs" {
  bucket = aws_s3_bucket.waf_logs.id

  rule {
    id = "delete-old-logs"

    expiration {
      days = 90
    }

    status = "Enabled"
  }
}

resource "aws_s3_bucket_public_access_block" "waf_logs" {
  bucket = aws_s3_bucket.waf_logs.id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

# IAM role for Firehose
resource "aws_iam_role" "firehose" {
  name = "${var.project_name}-${var.environment}-firehose-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "firehose.amazonaws.com"
        }
      }
    ]
  })

  tags = var.common_tags
}

resource "aws_iam_role_policy" "firehose" {
  name = "${var.project_name}-${var.environment}-firehose-policy"
  role = aws_iam_role.firehose.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "s3:AbortMultipartUpload",
          "s3:GetBucketLocation",
          "s3:GetObject",
          "s3:ListBucket",
          "s3:ListBucketMultipartUploads",
          "s3:PutObject"
        ]
        Resource = [
          aws_s3_bucket.waf_logs.arn,
          "${aws_s3_bucket.waf_logs.arn}/*"
        ]
      },
      {
        Effect = "Allow"
        Action = [
          "logs:PutLogEvents"
        ]
        Resource = aws_cloudwatch_log_stream.firehose.arn
      }
    ]
  })
}

# CloudWatch log group for Firehose
resource "aws_cloudwatch_log_group" "firehose" {
  name              = "/aws/kinesisfirehose/${var.project_name}-${var.environment}"
  retention_in_days = 7

  tags = var.common_tags
}

resource "aws_cloudwatch_log_stream" "firehose" {
  name           = "S3Delivery"
  log_group_name = aws_cloudwatch_log_group.firehose.name
}

# GuardDuty
resource "aws_guardduty_detector" "main" {
  enable = true

  datasources {
    s3_logs {
      enable = true
    }
    kubernetes {
      audit_logs {
        enable = false
      }
    }
    malware_protection {
      scan_ec2_instance_with_findings {
        ebs_volumes {
          enable = false
        }
      }
    }
  }

  tags = var.common_tags
}

# GuardDuty threat intel set (optional)
resource "aws_guardduty_threatintelset" "main" {
  count = var.threat_intel_list_url != "" ? 1 : 0

  activate    = true
  detector_id = aws_guardduty_detector.main.id
  format      = "TXT"
  location    = var.threat_intel_list_url
  name        = "${var.project_name}-${var.environment}-threat-intel"

  tags = var.common_tags
}

# EventBridge rule for GuardDuty findings
resource "aws_cloudwatch_event_rule" "guardduty" {
  name        = "${var.project_name}-${var.environment}-guardduty-findings"
  description = "Capture GuardDuty findings"

  event_pattern = jsonencode({
    source      = ["aws.guardduty"]
    detail-type = ["GuardDuty Finding"]
    detail = {
      severity = [
        { numeric = [">=", 7] }
      ]
    }
  })

  tags = var.common_tags
}

resource "aws_cloudwatch_event_target" "guardduty_sns" {
  rule      = aws_cloudwatch_event_rule.guardduty.name
  target_id = "SendToSNS"
  arn       = var.alarm_sns_topic_arn
}

# Security Hub
resource "aws_securityhub_account" "main" {}

resource "aws_securityhub_standards_subscription" "aws_foundational" {
  standards_arn = "arn:aws:securityhub:${var.aws_region}::standards/aws-foundational-security-best-practices/v/1.0.0"
}

resource "aws_securityhub_standards_subscription" "cis" {
  standards_arn = "arn:aws:securityhub:${var.aws_region}::standards/cis-aws-foundations-benchmark/v/1.2.0"
}

# Config Rules for Security Hub
resource "aws_config_configuration_recorder" "main" {
  name     = "${var.project_name}-${var.environment}-recorder"
  role_arn = aws_iam_role.config.arn

  recording_group {
    all_supported = true
  }
}

resource "aws_config_delivery_channel" "main" {
  name           = "${var.project_name}-${var.environment}-delivery"
  s3_bucket_name = aws_s3_bucket.config.id
}

resource "aws_config_configuration_recorder_status" "main" {
  name       = aws_config_configuration_recorder.main.name
  is_enabled = true

  depends_on = [aws_config_delivery_channel.main]
}

# S3 bucket for Config
resource "aws_s3_bucket" "config" {
  bucket = "${var.project_name}-${var.environment}-config"

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-config"
    }
  )
}

resource "aws_s3_bucket_public_access_block" "config" {
  bucket = aws_s3_bucket.config.id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

resource "aws_s3_bucket_policy" "config" {
  bucket = aws_s3_bucket.config.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Principal = {
          Service = "config.amazonaws.com"
        }
        Action   = "s3:GetBucketAcl"
        Resource = aws_s3_bucket.config.arn
      },
      {
        Effect = "Allow"
        Principal = {
          Service = "config.amazonaws.com"
        }
        Action   = "s3:PutObject"
        Resource = "${aws_s3_bucket.config.arn}/*"
        Condition = {
          StringEquals = {
            "s3:x-amz-acl" = "bucket-owner-full-control"
          }
        }
      },
      {
        Effect = "Allow"
        Principal = {
          Service = "config.amazonaws.com"
        }
        Action   = "s3:ListBucket"
        Resource = aws_s3_bucket.config.arn
      }
    ]
  })
}

# IAM role for Config
resource "aws_iam_role" "config" {
  name = "${var.project_name}-${var.environment}-config-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "config.amazonaws.com"
        }
      }
    ]
  })

  tags = var.common_tags
}

resource "aws_iam_role_policy_attachment" "config" {
  role       = aws_iam_role.config.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/ConfigRole"
}

# Secrets Manager rotation
resource "aws_secretsmanager_secret_rotation" "db_credentials" {
  count = var.enable_secret_rotation ? 1 : 0

  secret_id           = var.db_credentials_secret_id
  rotation_lambda_arn = aws_lambda_function.rotate_secret[0].arn

  rotation_rules {
    automatically_after_days = 30
  }
}

# Lambda for secret rotation (simplified)
resource "aws_lambda_function" "rotate_secret" {
  count = var.enable_secret_rotation ? 1 : 0

  filename      = data.archive_file.rotate_secret[0].output_path
  function_name = "${var.project_name}-${var.environment}-rotate-secret"
  role          = aws_iam_role.rotate_secret[0].arn
  handler       = "index.handler"
  runtime       = "python3.9"
  timeout       = 30

  environment {
    variables = {
      SECRETS_MANAGER_ENDPOINT = "https://secretsmanager.${var.aws_region}.amazonaws.com"
    }
  }

  tags = var.common_tags
}

data "archive_file" "rotate_secret" {
  count = var.enable_secret_rotation ? 1 : 0

  type        = "zip"
  output_path = "${path.module}/rotate-secret.zip"

  source {
    content  = <<-EOT
      import boto3
      import json

      def handler(event, context):
          # Simplified rotation logic
          # In production, implement full rotation logic
          service_client = boto3.client('secretsmanager')
          
          arn = event['SecretId']
          token = event['ClientRequestToken']
          step = event['Step']
          
          if step == "createSecret":
              # Generate new secret
              pass
          elif step == "setSecret":
              # Set secret in database
              pass
          elif step == "testSecret":
              # Test new secret
              pass
          elif step == "finishSecret":
              # Mark new secret as current
              pass
          
          return {"statusCode": 200}
    EOT
    filename = "index.py"
  }
}

resource "aws_iam_role" "rotate_secret" {
  count = var.enable_secret_rotation ? 1 : 0

  name = "${var.project_name}-${var.environment}-rotate-secret-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "lambda.amazonaws.com"
        }
      }
    ]
  })

  tags = var.common_tags
}

resource "aws_iam_role_policy" "rotate_secret" {
  count = var.enable_secret_rotation ? 1 : 0

  name = "${var.project_name}-${var.environment}-rotate-secret-policy"
  role = aws_iam_role.rotate_secret[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "secretsmanager:DescribeSecret",
          "secretsmanager:GetSecretValue",
          "secretsmanager:PutSecretValue",
          "secretsmanager:UpdateSecretVersionStage"
        ]
        Resource = var.db_credentials_secret_arn
      },
      {
        Effect = "Allow"
        Action = [
          "secretsmanager:GetRandomPassword"
        ]
        Resource = "*"
      },
      {
        Effect = "Allow"
        Action = [
          "logs:CreateLogGroup",
          "logs:CreateLogStream",
          "logs:PutLogEvents"
        ]
        Resource = "*"
      },
      {
        Effect = "Allow"
        Action = [
          "kms:Decrypt",
          "kms:DescribeKey"
        ]
        Resource = "*"
      }
    ]
  })
}
