# C++ API Service Infrastructure
# This module provisions the C++ Drogon API service

terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

# Local values for resource naming
locals {
  service_name    = "cpp-api"
  container_port  = 8110
  health_check_path = "/health"
  
  # Common tags
  common_tags = {
    Service     = local.service_name
    Framework   = "drogon"
    Language    = "cpp"
    Environment = var.environment
    Project     = var.project_name
  }
}

# ECS Task Definition
resource "aws_ecs_task_definition" "cpp_api" {
  count = var.enable_cpp_api ? 1 : 0
  
  family                   = "${var.project_name}-${var.environment}-${local.service_name}"
  network_mode             = "awsvpc"
  requires_compatibilities = ["FARGATE"]
  cpu                      = var.cpu
  memory                   = var.memory
  execution_role_arn       = aws_iam_role.ecs_execution_role[0].arn
  task_role_arn           = aws_iam_role.ecs_task_role[0].arn

  container_definitions = jsonencode([
    {
      name  = local.service_name
      image = var.container_image
      
      portMappings = [
        {
          containerPort = local.container_port
          protocol      = "tcp"
        }
      ]
      
      environment = [
        {
          name  = "NODE_ENV"
          value = var.environment
        },
        {
          name  = "PORT"
          value = tostring(local.container_port)
        },
        {
          name  = "SERVICE_NAME"
          value = local.service_name
        },
        {
          name  = "AUTH0_DOMAIN"
          value = var.auth0_domain
        },
        {
          name  = "AUTH0_AUDIENCE"
          value = var.auth0_audience
        },
        {
          name  = "DATABASE_URL"
          value = var.database_url
        },
        {
          name  = "REDIS_URL"
          value = var.redis_url
        },
        {
          name  = "KONG_ADMIN_URL"
          value = var.kong_admin_url
        },
        {
          name  = "KONG_PROXY_CACHE_TTL"
          value = tostring(var.kong_proxy_cache_ttl)
        },
        {
          name  = "KONG_RATE_LIMIT_MINUTE"
          value = tostring(var.kong_rate_limit_minute)
        },
        {
          name  = "KONG_RATE_LIMIT_HOUR"
          value = tostring(var.kong_rate_limit_hour)
        },
        {
          name  = "KONG_JWT_VALIDATION_ENABLED"
          value = tostring(var.kong_jwt_validation_enabled)
        },
        {
          name  = "KONG_CORS_ENABLED"
          value = tostring(var.kong_cors_enabled)
        },
        {
          name  = "DROGON_THREAD_NUM"
          value = tostring(var.drogon_thread_num)
        },
        {
          name  = "DROGON_LISTENER_COUNT"
          value = tostring(var.drogon_listener_count)
        },
        {
          name  = "DROGON_ENABLE_COMPRESSION"
          value = tostring(var.drogon_enable_compression)
        }
      ]
      
      secrets = [
        {
          name      = "AUTH0_CLIENT_SECRET"
          valueFrom = var.auth0_client_secret_arn
        },
        {
          name      = "JWT_SECRET"
          valueFrom = var.jwt_secret_arn
        },
        {
          name      = "DATABASE_PASSWORD"
          valueFrom = var.database_password_arn
        }
      ]
      
      healthCheck = {
        command = [
          "CMD-SHELL",
          "curl -f http://localhost:${local.container_port}${local.health_check_path} || exit 1"
        ]
        interval    = 30
        timeout     = 5
        retries     = 3
        startPeriod = 60
      }
      
      logConfiguration = {
        logDriver = "awslogs"
        options = {
          awslogs-group         = aws_cloudwatch_log_group.cpp_api[0].name
          awslogs-region        = var.aws_region
          awslogs-stream-prefix = "ecs"
        }
      }
      
      essential = true
    }
  ])

  tags = local.common_tags
}

# ECS Service
resource "aws_ecs_service" "cpp_api" {
  count = var.enable_cpp_api ? 1 : 0
  
  name            = "${var.project_name}-${var.environment}-${local.service_name}"
  cluster         = var.cluster_id
  task_definition = aws_ecs_task_definition.cpp_api[0].arn
  desired_count   = var.desired_count
  launch_type     = "FARGATE"
  
  network_configuration {
    subnets          = var.subnet_ids
    security_groups  = var.security_group_ids
    assign_public_ip = true
  }
  
  load_balancer {
    target_group_arn = aws_lb_target_group.cpp_api[0].arn
    container_name   = local.service_name
    container_port   = local.container_port
  }
  
  service_registries {
    registry_arn = aws_service_discovery_service.cpp_api[0].arn
  }
  
  depends_on = [aws_lb_listener_rule.cpp_api]
  
  tags = local.common_tags
}

# Load Balancer Target Group
resource "aws_lb_target_group" "cpp_api" {
  count = var.enable_cpp_api ? 1 : 0
  
  name        = "${var.project_name}-${var.environment}-cpp-api"
  port        = local.container_port
  protocol    = "HTTP"
  vpc_id      = var.vpc_id
  target_type = "ip"
  
  health_check {
    enabled             = true
    healthy_threshold   = 2
    unhealthy_threshold = 2
    timeout             = 5
    interval            = 30
    path                = local.health_check_path
    matcher             = "200"
    port                = "traffic-port"
    protocol            = "HTTP"
  }
  
  tags = local.common_tags
}

# Load Balancer Listener Rule
resource "aws_lb_listener_rule" "cpp_api" {
  count = var.enable_cpp_api ? 1 : 0
  
  listener_arn = var.listener_arn
  priority     = 8110
  
  action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.cpp_api[0].arn
  }
  
  condition {
    path_pattern {
      values = ["/api/v1/cpp/*"]
    }
  }
  
  tags = local.common_tags
}

# Service Discovery
resource "aws_service_discovery_service" "cpp_api" {
  count = var.enable_cpp_api ? 1 : 0
  
  name = local.service_name
  
  dns_config {
    namespace_id = var.service_discovery_namespace_id
    
    dns_records {
      ttl  = 10
      type = "A"
    }
    
    routing_policy = "MULTIVALUE"
  }
  
  health_check_grace_period_seconds = 60
  
  tags = local.common_tags
}

# CloudWatch Log Group
resource "aws_cloudwatch_log_group" "cpp_api" {
  count = var.enable_cpp_api ? 1 : 0
  
  name              = "/ecs/${var.project_name}/${var.environment}/${local.service_name}"
  retention_in_days = var.log_retention_days
  
  tags = local.common_tags
}

# IAM Role for ECS Task Execution
resource "aws_iam_role" "ecs_execution_role" {
  count = var.enable_cpp_api ? 1 : 0
  
  name = "${var.project_name}-${var.environment}-${local.service_name}-execution-role"
  
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "ecs-tasks.amazonaws.com"
        }
      }
    ]
  })
  
  tags = local.common_tags
}

# IAM Role for ECS Task
resource "aws_iam_role" "ecs_task_role" {
  count = var.enable_cpp_api ? 1 : 0
  
  name = "${var.project_name}-${var.environment}-${local.service_name}-task-role"
  
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "ecs-tasks.amazonaws.com"
        }
      }
    ]
  })
  
  tags = local.common_tags
}

# IAM Policy Attachment for ECS Task Execution
resource "aws_iam_role_policy_attachment" "ecs_execution_role_policy" {
  count = var.enable_cpp_api ? 1 : 0
  
  role       = aws_iam_role.ecs_execution_role[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

# IAM Policy for accessing secrets
resource "aws_iam_role_policy" "ecs_execution_secrets_policy" {
  count = var.enable_cpp_api ? 1 : 0
  
  name = "${var.project_name}-${var.environment}-${local.service_name}-secrets-policy"
  role = aws_iam_role.ecs_execution_role[0].id
  
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "secretsmanager:GetSecretValue"
        ]
        Resource = [
          var.auth0_client_secret_arn,
          var.jwt_secret_arn,
          var.database_password_arn
        ]
      }
    ]
  })
}

# Auto Scaling Target
resource "aws_appautoscaling_target" "cpp_api" {
  count = var.enable_cpp_api && var.enable_auto_scaling ? 1 : 0
  
  max_capacity       = var.auto_scaling_max_capacity
  min_capacity       = var.auto_scaling_min_capacity
  resource_id        = "service/${var.cluster_name}/${aws_ecs_service.cpp_api[0].name}"
  scalable_dimension = "ecs:service:DesiredCount"
  service_namespace  = "ecs"
  
  tags = local.common_tags
}

# Auto Scaling Policy - CPU
resource "aws_appautoscaling_policy" "cpp_api_cpu" {
  count = var.enable_cpp_api && var.enable_auto_scaling ? 1 : 0
  
  name               = "${var.project_name}-${var.environment}-${local.service_name}-cpu-scaling"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.cpp_api[0].resource_id
  scalable_dimension = aws_appautoscaling_target.cpp_api[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.cpp_api[0].service_namespace
  
  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ECSServiceAverageCPUUtilization"
    }
    target_value = var.auto_scaling_target_cpu
  }
}

# Auto Scaling Policy - Memory
resource "aws_appautoscaling_policy" "cpp_api_memory" {
  count = var.enable_cpp_api && var.enable_auto_scaling ? 1 : 0
  
  name               = "${var.project_name}-${var.environment}-${local.service_name}-memory-scaling"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.cpp_api[0].resource_id
  scalable_dimension = aws_appautoscaling_target.cpp_api[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.cpp_api[0].service_namespace
  
  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ECSServiceAverageMemoryUtilization"
    }
    target_value = var.auto_scaling_target_memory
  }
}

# PostHog Analytics Module
module "cpp_api_posthog" {
  count = var.enable_cpp_api && var.enable_posthog ? 1 : 0
  
  source = "./modules/posthog"
  
  service_name     = local.service_name
  environment      = var.environment
  project_name     = var.project_name
  
  # PostHog configuration
  posthog_api_key      = var.posthog_api_key
  posthog_host         = var.posthog_host
  posthog_project_id   = var.posthog_project_id
  
  # Analytics settings
  enable_posthog       = var.enable_posthog
  posthog_capture_pageviews = var.posthog_capture_pageviews
  posthog_capture_pageleaves = var.posthog_capture_pageleaves
  posthog_disable_session_recording = var.posthog_disable_session_recording
  
  # Privacy settings
  posthog_opt_out_capturing = var.posthog_opt_out_capturing
  posthog_anonymize_ips     = var.posthog_anonymize_ips
  
  # Performance settings
  posthog_batch_size        = var.posthog_batch_size
  posthog_request_timeout   = var.posthog_request_timeout
  
  # Custom events
  posthog_custom_events = var.posthog_custom_events
  
  # Webhook configuration
  webhook_endpoint = var.webhook_endpoint
  
  tags = local.common_tags
}
