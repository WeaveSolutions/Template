# Provider Configuration
provider "aws" {
  region = var.aws_region
  
  default_tags {
    tags = merge(
      {
        Environment = var.environment
        Project     = var.project_name
        Terraform   = "true"
      },
      var.tags
    )
  }
}

# VPC Module
module "vpc" {
  source  = "terraform-aws-modules/vpc/aws"
  version = "~> 3.0"

  name = "${var.project_name}-${var.environment}-vpc"
  cidr = var.vpc_cidr

  azs             = var.availability_zones
  private_subnets = var.private_subnet_cidrs
  public_subnets  = var.public_subnet_cidrs

  enable_nat_gateway   = true
  single_nat_gateway   = true
  enable_dns_hostnames = true

  tags = {
    Terraform   = "true"
    Environment = var.environment
  }
}

# API Gateway v2 (HTTP API)
resource "aws_apigatewayv2_api" "main" {
  name          = "${var.project_name}-${var.environment}-api"
  protocol_type = "HTTP"
  
  cors_configuration {
    allow_origins = var.allowed_origins
    allow_methods = ["*"]
    allow_headers = ["*"]
  }
}

# API Gateway Stage
resource "aws_apigatewayv2_stage" "default" {
  api_id      = aws_apigatewayv2_api.main.id
  name        = "$default"
  auto_deploy = true

  access_log_settings {
    destination_arn = aws_cloudwatch_log_group.api_gw.arn
    format = jsonencode({
      requestId        = "$context.requestId"
      ip               = "$context.identity.sourceIp"
      requestTime      = "$context.requestTime"
      httpMethod       = "$context.httpMethod"
      routeKey         = "$context.routeKey"
      status           = "$context.status"
      protocol         = "$context.protocol"
      responseLength   = "$context.responseLength"
    })
  }
}

# CloudWatch Log Group for API Gateway
resource "aws_cloudwatch_log_group" "api_gw" {
  name              = "/aws/api-gw/${var.project_name}-${var.environment}-api"
  retention_in_days = var.api_gateway_log_retention_days
}

# WAF Web ACL for API Gateway
resource "aws_wafv2_web_acl" "api_gateway" {
  count       = var.enable_waf ? 1 : 0
  name        = "${var.project_name}-${var.environment}-api-gw-waf"
  description = "WAF for API Gateway"
  scope       = "REGIONAL"

  default_action {
    allow {}
  }

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
      metric_name                = "AWSManagedRulesCommonRuleSet"
      sampled_requests_enabled   = true
    }
  }

  visibility_config {
    cloudwatch_metrics_enabled = true
    metric_name                = "${var.project_name}-${var.environment}-api-gw-waf"
    sampled_requests_enabled   = true
  }
}

# Associate WAF with API Gateway
resource "aws_wafv2_web_acl_association" "api_gateway" {
  count        = var.enable_waf ? 1 : 0
  resource_arn = "${aws_apigatewayv2_stage.default.execution_arn}/*"
  web_acl_arn  = aws_wafv2_web_acl.api_gateway[0].arn
}

# IAM Role for API Gateway to access other AWS services
resource "aws_iam_role" "api_gateway" {
  name = "${var.project_name}-${var.environment}-api-gw-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "apigateway.amazonaws.com"
        }
      }
    ]
  })
}

# Attach basic execution policy
resource "aws_iam_role_policy_attachment" "api_gateway" {
  role       = aws_iam_role.api_gateway.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonAPIGatewayPushToCloudWatchLogs"
}

# Outputs
output "api_gateway_url" {
  description = "The URL of the API Gateway"
  value       = aws_apigatewayv2_stage.default.invoke_url
}

output "api_gateway_id" {
  description = "The ID of the API Gateway"
  value       = aws_apigatewayv2_api.main.id
}

output "vpc_id" {
  description = "The ID of the VPC"
  value       = module.vpc.vpc_id
}

output "private_subnet_ids" {
  description = "List of private subnet IDs"
  value       = module.vpc.private_subnets
}

output "public_subnet_ids" {
  description = "List of public subnet IDs"
  value       = module.vpc.public_subnets
}
