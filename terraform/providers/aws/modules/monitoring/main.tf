# Monitoring Module - CloudWatch, X-Ray, and SNS
# Following Well-Architected Framework for operational excellence

# SNS Topic for Alarms
resource "aws_sns_topic" "alarms" {
  name              = "${var.project_name}-${var.environment}-alarms"
  kms_master_key_id = "alias/aws/sns"

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-alarms"
    }
  )
}

resource "aws_sns_topic_subscription" "email" {
  for_each = toset(var.alarm_email_endpoints)
  
  topic_arn = aws_sns_topic.alarms.arn
  protocol  = "email"
  endpoint  = each.value
}

resource "aws_sns_topic_subscription" "sms" {
  for_each = toset(var.alarm_sms_endpoints)
  
  topic_arn = aws_sns_topic.alarms.arn
  protocol  = "sms"
  endpoint  = each.value
}

# CloudWatch Log Groups
resource "aws_cloudwatch_log_group" "application" {
  name              = "/aws/apprunner/${var.project_name}-${var.environment}"
  retention_in_days = var.log_retention_days
  kms_key_id        = var.kms_key_arn

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-app-logs"
    }
  )
}

resource "aws_cloudwatch_log_group" "api_gateway" {
  name              = "/aws/apigateway/${var.project_name}-${var.environment}"
  retention_in_days = var.log_retention_days
  kms_key_id        = var.kms_key_arn

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-api-logs"
    }
  )
}

# CloudWatch Dashboard
resource "aws_cloudwatch_dashboard" "main" {
  dashboard_name = "${var.project_name}-${var.environment}-dashboard"

  dashboard_body = jsonencode({
    widgets = [
      {
        type   = "metric"
        x      = 0
        y      = 0
        width  = 12
        height = 6
        properties = {
          metrics = [
            ["AWS/AppRunner", "ActiveInstances", "ServiceName", var.app_runner_service_name],
            [".", "RequestCount", ".", "."],
            [".", "4xxStatusCodeCount", ".", "."],
            [".", "5xxStatusCodeCount", ".", "."]
          ]
          period = 300
          stat   = "Average"
          region = var.aws_region
          title  = "App Runner Metrics"
        }
      },
      {
        type   = "metric"
        x      = 12
        y      = 0
        width  = 12
        height = 6
        properties = {
          metrics = [
            ["AWS/RDS", "CPUUtilization", "DBInstanceIdentifier", var.db_instance_id],
            [".", "DatabaseConnections", ".", "."],
            [".", "FreeableMemory", ".", "."],
            [".", "FreeStorageSpace", ".", "."]
          ]
          period = 300
          stat   = "Average"
          region = var.aws_region
          title  = "RDS Metrics"
        }
      },
      {
        type   = "metric"
        x      = 0
        y      = 6
        width  = 12
        height = 6
        properties = {
          metrics = [
            ["AWS/CloudFront", "Requests", "DistributionId", var.cloudfront_distribution_id],
            [".", "BytesDownloaded", ".", "."],
            [".", "BytesUploaded", ".", "."],
            [".", "4xxErrorRate", ".", "."],
            [".", "5xxErrorRate", ".", "."]
          ]
          period = 300
          stat   = "Sum"
          region = "us-east-1"
          title  = "CloudFront Metrics"
        }
      },
      {
        type   = "log"
        x      = 12
        y      = 6
        width  = 12
        height = 6
        properties = {
          query   = "SOURCE '/aws/apprunner/${var.project_name}-${var.environment}' | fields @timestamp, @message | filter @message like /ERROR/ | sort @timestamp desc | limit 20"
          region  = var.aws_region
          title   = "Recent Errors"
        }
      }
    ]
  })
}

# X-Ray Service Map
resource "aws_xray_sampling_rule" "main" {
  rule_name      = "${var.project_name}-${var.environment}-sampling"
  priority       = 1000
  version        = 1
  reservoir_size = 1
  fixed_rate     = 0.05
  url_path       = "*"
  host           = "*"
  http_method    = "*"
  service_type   = "*"
  service_name   = "*"
  resource_arn   = "*"

  attributes = {}
}

# CloudWatch Synthetics Canary
resource "aws_synthetics_canary" "health_check" {
  name                 = "${var.project_name}-${var.environment}-health"
  artifact_s3_location = "s3://${var.synthetics_bucket_name}/canary-artifacts/"
  execution_role_arn   = aws_iam_role.synthetics.arn
  handler              = "pageLoadBlueprint.handler"
  zip_file             = data.archive_file.canary_script.output_path
  runtime_version      = "syn-nodejs-puppeteer-3.8"
  start_canary         = true

  success_retention_period = 2
  failure_retention_period = 14

  schedule {
    expression = "rate(5 minutes)"
  }

  vpc_config {
    subnet_ids         = var.canary_subnet_ids
    security_group_ids = var.canary_security_group_ids
  }

  run_config {
    timeout_in_seconds = 60
    memory_in_mb       = 960
  }

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-health-canary"
    }
  )
}

# IAM Role for Synthetics
resource "aws_iam_role" "synthetics" {
  name = "${var.project_name}-${var.environment}-synthetics-role"

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

resource "aws_iam_role_policy_attachment" "synthetics_lambda" {
  role       = aws_iam_role.synthetics.name
  policy_arn = "arn:aws:iam::aws:policy/CloudWatchSyntheticsFullAccess"
}

resource "aws_iam_role_policy" "synthetics_s3" {
  name = "${var.project_name}-${var.environment}-synthetics-s3-policy"
  role = aws_iam_role.synthetics.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "s3:PutObject",
          "s3:GetObject"
        ]
        Resource = "arn:aws:s3:::${var.synthetics_bucket_name}/*"
      },
      {
        Effect = "Allow"
        Action = [
          "s3:ListBucket"
        ]
        Resource = "arn:aws:s3:::${var.synthetics_bucket_name}"
      }
    ]
  })
}

# Canary Script Archive
data "archive_file" "canary_script" {
  type        = "zip"
  output_path = "${path.module}/canary.zip"

  source {
    content  = <<-EOT
      const synthetics = require('Synthetics');
      const log = require('SyntheticsLogger');

      const pageLoadBlueprint = async function () {
        const syntheticsConfiguration = synthetics.getConfiguration();
        syntheticsConfiguration.setConfig({
          screenshotOnStepStart: false,
          screenshotOnStepSuccess: false,
          screenshotOnStepFailure: true
        });

        const page = await synthetics.getPage();
        
        const stepName = 'Load home page';
        await synthetics.executeStep(stepName, async () => {
          const response = await page.goto('${var.app_url}', { waitUntil: 'networkidle0' });
          
          // Check if the response was successful
          if (!response || response.status() >= 400) {
            throw new Error(`Page returned status code $${response ? response.status() : 'null'}`);
          }
          
          // Check for specific elements
          await page.waitForSelector('body', { timeout: 30000 });
          
          log.info(`Page loaded successfully with status: $${response.status()}`);
        });
      };

      exports.handler = async () => {
        return await synthetics.runCanary(pageLoadBlueprint);
      };
    EOT
    filename = "nodejs/node_modules/pageLoadBlueprint.js"
  }
}

# EventBridge Rule for Cost Anomaly Detection
resource "aws_cloudwatch_event_rule" "cost_anomaly" {
  name        = "${var.project_name}-${var.environment}-cost-anomaly"
  description = "Trigger on cost anomaly detection"

  event_pattern = jsonencode({
    source      = ["aws.ce"]
    detail-type = ["Cost Anomaly Detected"]
  })

  tags = var.common_tags
}

resource "aws_cloudwatch_event_target" "cost_anomaly_sns" {
  rule      = aws_cloudwatch_event_rule.cost_anomaly.name
  target_id = "SendToSNS"
  arn       = aws_sns_topic.alarms.arn
}

# CloudWatch Insights Query Definitions
resource "aws_cloudwatch_query_definition" "error_analysis" {
  name = "${var.project_name}-${var.environment}-error-analysis"

  log_group_names = [
    aws_cloudwatch_log_group.application.name,
    aws_cloudwatch_log_group.api_gateway.name
  ]

  query_string = <<-EOQ
    fields @timestamp, @message
    | filter @message like /ERROR/
    | stats count() by bin(5m)
  EOQ
}

resource "aws_cloudwatch_query_definition" "performance_analysis" {
  name = "${var.project_name}-${var.environment}-performance-analysis"

  log_group_names = [
    aws_cloudwatch_log_group.application.name
  ]

  query_string = <<-EOQ
    fields @timestamp, @duration
    | filter @type = "REPORT"
    | stats avg(@duration), max(@duration), min(@duration) by bin(5m)
  EOQ
}

# CloudWatch Composite Alarms
resource "aws_cloudwatch_composite_alarm" "critical_system" {
  alarm_name          = "${var.project_name}-${var.environment}-critical-system-alarm"
  alarm_description   = "Composite alarm for critical system issues"
  alarm_actions       = [aws_sns_topic.alarms.arn]
  ok_actions          = [aws_sns_topic.alarms.arn]

  alarm_rule = join(" OR ", [
    "ALARM('${var.project_name}-${var.environment}-db-cpu-high')",
    "ALARM('${var.project_name}-${var.environment}-app-5xx-errors')",
    "ALARM('${var.project_name}-${var.environment}-cdn-origin-5xx')"
  ])

  tags = var.common_tags
}

# Application Insights
resource "aws_application_insights_application" "main" {
  resource_group_name = aws_resourcegroups_group.main.name
  
  ops_center_enabled = var.enable_ops_center
  ops_item_sns_topic_arn = var.enable_ops_center ? aws_sns_topic.alarms.arn : null
}

# Resource Group for Application Insights
resource "aws_resourcegroups_group" "main" {
  name = "${var.project_name}-${var.environment}-resources"

  resource_query {
    query = jsonencode({
      ResourceTypeFilters = [
        "AWS::EC2::Instance",
        "AWS::RDS::DBInstance",
        "AWS::Lambda::Function",
        "AWS::AppRunner::Service",
        "AWS::S3::Bucket"
      ]
      TagFilters = [
        {
          Key    = "Project"
          Values = [var.project_name]
        },
        {
          Key    = "Environment"
          Values = [var.environment]
        }
      ]
    })
  }

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-resource-group"
    }
  )
}
