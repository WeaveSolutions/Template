# Compute Module - AWS App Runner for Next.js Application
# Following Well-Architected Framework for serverless compute

locals {
  app_name = "${var.project_name}-${var.environment}-nextjs"
}

# ECR Repository for Docker images
resource "aws_ecr_repository" "nextjs" {
  name                 = local.app_name
  image_tag_mutability = "MUTABLE"

  image_scanning_configuration {
    scan_on_push = true
  }

  encryption_configuration {
    encryption_type = "AES256"
  }

  tags = merge(
    var.common_tags,
    {
      Name = local.app_name
    }
  )
}

# ECR Lifecycle Policy
resource "aws_ecr_lifecycle_policy" "nextjs" {
  repository = aws_ecr_repository.nextjs.name

  policy = jsonencode({
    rules = [
      {
        rulePriority = 1
        description  = "Keep last 10 images"
        selection = {
          tagStatus     = "tagged"
          tagPrefixList = ["v"]
          countType     = "imageCountMoreThan"
          countNumber   = 10
        }
        action = {
          type = "expire"
        }
      },
      {
        rulePriority = 2
        description  = "Remove untagged images after 1 day"
        selection = {
          tagStatus   = "untagged"
          countType   = "sinceImagePushed"
          countUnit   = "days"
          countNumber = 1
        }
        action = {
          type = "expire"
        }
      }
    ]
  })
}

# IAM Role for App Runner
resource "aws_iam_role" "apprunner_service" {
  name = "${local.app_name}-service-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "tasks.apprunner.amazonaws.com"
        }
      }
    ]
  })

  tags = var.common_tags
}

# IAM Policy for App Runner Service
resource "aws_iam_role_policy" "apprunner_service" {
  name = "${local.app_name}-service-policy"
  role = aws_iam_role.apprunner_service.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "s3:GetObject",
          "s3:PutObject",
          "s3:ListBucket"
        ]
        Resource = [
          var.assets_bucket_arn,
          "${var.assets_bucket_arn}/*"
        ]
      },
      {
        Effect = "Allow"
        Action = [
          "secretsmanager:GetSecretValue"
        ]
        Resource = var.secrets_arns
      },
      {
        Effect = "Allow"
        Action = [
          "xray:PutTraceSegments",
          "xray:PutTelemetryRecords"
        ]
        Resource = "*"
      }
    ]
  })
}

# IAM Role for App Runner ECR Access
resource "aws_iam_role" "apprunner_ecr_access" {
  name = "${local.app_name}-ecr-access-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "build.apprunner.amazonaws.com"
        }
      }
    ]
  })

  tags = var.common_tags
}

resource "aws_iam_role_policy_attachment" "apprunner_ecr_access" {
  role       = aws_iam_role.apprunner_ecr_access.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSAppRunnerServicePolicyForECRAccess"
}

# App Runner Service
resource "aws_apprunner_service" "nextjs" {
  service_name = local.app_name

  source_configuration {
    authentication_configuration {
      access_role_arn = aws_iam_role.apprunner_ecr_access.arn
    }

    image_repository {
      image_identifier      = "${aws_ecr_repository.nextjs.repository_url}:latest"
      image_repository_type = "ECR"
      
      image_configuration {
        port = var.app_port
        
        runtime_environment_variables = merge(
          var.environment_variables,
          {
            NODE_ENV        = var.environment
            NEXT_PUBLIC_API_URL = var.api_endpoint
          }
        )
        
        runtime_environment_secrets = var.environment_secrets
      }
    }

    auto_deployments_enabled = var.environment == "prod" ? false : true
  }

  instance_configuration {
    cpu               = var.cpu
    memory            = var.memory
    instance_role_arn = aws_iam_role.apprunner_service.arn
  }

  auto_scaling_configuration_arn = aws_apprunner_auto_scaling_configuration_version.nextjs.arn

  health_check_configuration {
    protocol            = "HTTP"
    path                = "/api/health"
    interval            = 10
    timeout             = 5
    healthy_threshold   = 1
    unhealthy_threshold = 5
  }

  observability_configuration {
    observability_enabled           = true
    observability_configuration_arn = aws_apprunner_observability_configuration.nextjs.arn
  }

  network_configuration {
    egress_configuration {
      egress_type       = "VPC"
      vpc_connector_arn = aws_apprunner_vpc_connector.main.arn
    }
  }

  tags = merge(
    var.common_tags,
    {
      Name = local.app_name
    }
  )
}

# Auto Scaling Configuration
resource "aws_apprunner_auto_scaling_configuration_version" "nextjs" {
  auto_scaling_configuration_name = "${local.app_name}-autoscaling"

  max_concurrency = var.max_concurrency
  max_size        = var.max_size
  min_size        = var.min_size

  tags = var.common_tags
}

# VPC Connector for App Runner
resource "aws_apprunner_vpc_connector" "main" {
  vpc_connector_name = "${local.app_name}-vpc-connector"
  subnets            = var.subnet_ids
  security_groups    = var.security_group_ids

  tags = var.common_tags
}

# Observability Configuration
resource "aws_apprunner_observability_configuration" "nextjs" {
  observability_configuration_name = "${local.app_name}-observability"

  trace_configuration {
    vendor = "AWSXRAY"
  }

  tags = var.common_tags
}

# Custom Domain Association
resource "aws_apprunner_custom_domain_association" "nextjs" {
  count = var.custom_domain != null ? 1 : 0

  domain_name = var.custom_domain
  service_arn = aws_apprunner_service.nextjs.arn

  enable_www_subdomain = var.enable_www_subdomain
}

# Lambda Functions for API
module "api_lambda" {
  source = "../lambda"

  function_name = "${var.project_name}-${var.environment}-api"
  runtime       = "nodejs18.x"
  handler       = "index.handler"
  
  environment_variables = {
    NODE_ENV     = var.environment
    DATABASE_URL = var.database_url
  }

  vpc_subnet_ids         = var.subnet_ids
  vpc_security_group_ids = var.security_group_ids

  common_tags = var.common_tags
}

# API Gateway
resource "aws_apigatewayv2_api" "main" {
  name          = "${var.project_name}-${var.environment}-api"
  protocol_type = "HTTP"
  
  cors_configuration {
    allow_origins     = var.allowed_origins
    allow_methods     = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    allow_headers     = ["*"]
    expose_headers    = ["*"]
    max_age           = 300
    allow_credentials = true
  }

  tags = var.common_tags
}

# API Gateway Lambda Integration
resource "aws_apigatewayv2_integration" "lambda" {
  api_id           = aws_apigatewayv2_api.main.id
  integration_type = "AWS_PROXY"
  
  connection_type           = "INTERNET"
  integration_method        = "POST"
  integration_uri           = module.api_lambda.function_arn
  payload_format_version    = "2.0"
}

# API Gateway Route
resource "aws_apigatewayv2_route" "lambda" {
  api_id    = aws_apigatewayv2_api.main.id
  route_key = "ANY /{proxy+}"
  target    = "integrations/${aws_apigatewayv2_integration.lambda.id}"
}

# API Gateway Stage
resource "aws_apigatewayv2_stage" "main" {
  api_id      = aws_apigatewayv2_api.main.id
  name        = var.environment
  auto_deploy = true

  access_log_settings {
    destination_arn = aws_cloudwatch_log_group.api_gateway.arn
    format = jsonencode({
      requestId      = "$context.requestId"
      ip             = "$context.identity.sourceIp"
      requestTime    = "$context.requestTime"
      httpMethod     = "$context.httpMethod"
      routeKey       = "$context.routeKey"
      status         = "$context.status"
      protocol       = "$context.protocol"
      responseLength = "$context.responseLength"
    })
  }

  default_route_settings {
    throttling_rate_limit  = var.api_rate_limit
    throttling_burst_limit = var.api_burst_limit
  }

  tags = var.common_tags
}

# CloudWatch Log Group for API Gateway
resource "aws_cloudwatch_log_group" "api_gateway" {
  name              = "/aws/apigateway/${var.project_name}-${var.environment}"
  retention_in_days = var.log_retention_days
  kms_key_id        = var.kms_key_id

  tags = var.common_tags
}

# Lambda Permission for API Gateway
resource "aws_lambda_permission" "api_gateway" {
  statement_id  = "AllowAPIGatewayInvoke"
  action        = "lambda:InvokeFunction"
  function_name = module.api_lambda.function_name
  principal     = "apigateway.amazonaws.com"
  source_arn    = "${aws_apigatewayv2_api.main.execution_arn}/*/*"
}
