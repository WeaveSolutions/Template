# TypeScript API Service Infrastructure
# This module manages the TypeScript API service deployment and infrastructure

terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    docker = {
      source  = "kreuzwerker/docker"
      version = "~> 3.0"
    }
  }
}

# Local values for configuration
locals {
  service_name = "typescript-api"
  container_port = 8020
  
  # Common tags
  common_tags = {
    Service     = local.service_name
    Backend     = "typescript"
    Port        = local.container_port
    Environment = var.environment
    Project     = var.project_name
  }
}

# ECS Task Definition
resource "aws_ecs_task_definition" "typescript_api" {
  count = var.enable_typescript_api ? 1 : 0
  
  family                   = "${var.project_name}-${local.service_name}"
  network_mode             = "awsvpc"
  requires_compatibilities = ["FARGATE"]
  cpu                      = var.cpu
  memory                   = var.memory
  execution_role_arn       = aws_iam_role.ecs_execution_role[0].arn
  task_role_arn           = aws_iam_role.ecs_task_role[0].arn

  container_definitions = jsonencode([
    {
      name      = local.service_name
      image     = var.container_image
      essential = true
      
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
          "curl -f http://localhost:${local.container_port}/health || exit 1"
        ]
        interval    = 30
        timeout     = 5
        retries     = 3
        startPeriod = 60
      }
      
      logConfiguration = {
        logDriver = "awslogs"
        options = {
          awslogs-group         = aws_cloudwatch_log_group.typescript_api[0].name
          awslogs-region        = var.aws_region
          awslogs-stream-prefix = "ecs"
        }
      }
    }
  ])
  
  tags = local.common_tags
}

# ECS Service
resource "aws_ecs_service" "typescript_api" {
  count = var.enable_typescript_api ? 1 : 0
  
  name            = local.service_name
  cluster         = var.cluster_id
  task_definition = aws_ecs_task_definition.typescript_api[0].arn
  desired_count   = var.desired_count
  launch_type     = "FARGATE"
  
  deployment_configuration {
    maximum_percent         = 200
    minimum_healthy_percent = 100
  }
  
  network_configuration {
    subnets          = var.subnet_ids
    security_groups  = var.security_group_ids
    assign_public_ip = true
  }
  
  load_balancer {
    target_group_arn = aws_lb_target_group.typescript_api[0].arn
    container_name   = local.service_name
    container_port   = local.container_port
  }
  
  service_registries {
    registry_arn = aws_service_discovery_service.typescript_api[0].arn
  }
  
  depends_on = [
    aws_lb_listener_rule.typescript_api[0]
  ]
  
  tags = local.common_tags
}

# Target Group for Load Balancer
resource "aws_lb_target_group" "typescript_api" {
  count = var.enable_typescript_api ? 1 : 0
  
  name        = "${var.project_name}-${local.service_name}-tg"
  port        = local.container_port
  protocol    = "HTTP"
  target_type = "ip"
  vpc_id      = var.vpc_id
  
  health_check {
    enabled             = true
    healthy_threshold   = 2
    unhealthy_threshold = 2
    timeout             = 5
    interval            = 30
    path                = "/health"
    matcher             = "200"
    port                = "traffic-port"
    protocol            = "HTTP"
  }
  
  tags = local.common_tags
}

# Load Balancer Listener Rule
resource "aws_lb_listener_rule" "typescript_api" {
  count = var.enable_typescript_api ? 1 : 0
  
  listener_arn = var.listener_arn
  priority     = 100
  
  action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.typescript_api[0].arn
  }
  
  condition {
    path_pattern {
      values = ["/api/v1/typescript/*"]
    }
  }
  
  tags = local.common_tags
}

# Service Discovery
resource "aws_service_discovery_service" "typescript_api" {
  count = var.enable_typescript_api ? 1 : 0
  
  name = local.service_name
  
  dns_config {
    namespace_id = var.service_discovery_namespace_id
    
    dns_records {
      ttl  = 10
      type = "A"
    }
    
    routing_policy = "MULTIVALUE"
  }
  
  health_check_grace_period_seconds = 30
  
  tags = local.common_tags
}

# CloudWatch Log Group
resource "aws_cloudwatch_log_group" "typescript_api" {
  count = var.enable_typescript_api ? 1 : 0
  
  name              = "/ecs/${var.project_name}-${local.service_name}"
  retention_in_days = var.log_retention_days
  
  tags = local.common_tags
}

# IAM Role for ECS Execution
resource "aws_iam_role" "ecs_execution_role" {
  count = var.enable_typescript_api ? 1 : 0
  
  name = "${var.project_name}-${local.service_name}-execution-role"
  
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

# IAM Role Policy Attachment for ECS Execution
resource "aws_iam_role_policy_attachment" "ecs_execution_role_policy" {
  count = var.enable_typescript_api ? 1 : 0
  
  role       = aws_iam_role.ecs_execution_role[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

# IAM Role for ECS Task
resource "aws_iam_role" "ecs_task_role" {
  count = var.enable_typescript_api ? 1 : 0
  
  name = "${var.project_name}-${local.service_name}-task-role"
  
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

# IAM Policy for ECS Task (custom permissions)
resource "aws_iam_role_policy" "ecs_task_policy" {
  count = var.enable_typescript_api ? 1 : 0
  
  name = "${var.project_name}-${local.service_name}-task-policy"
  role = aws_iam_role.ecs_task_role[0].id
  
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "secretsmanager:GetSecretValue",
          "ssm:GetParameter",
          "ssm:GetParameters",
          "ssm:GetParametersByPath"
        ]
        Resource = [
          var.auth0_client_secret_arn,
          var.jwt_secret_arn,
          var.database_password_arn
        ]
      },
      {
        Effect = "Allow"
        Action = [
          "logs:CreateLogStream",
          "logs:PutLogEvents"
        ]
        Resource = aws_cloudwatch_log_group.typescript_api[0].arn
      }
    ]
  })
}

# Auto Scaling Target
resource "aws_appautoscaling_target" "typescript_api" {
  count = var.enable_typescript_api && var.enable_auto_scaling ? 1 : 0
  
  max_capacity       = var.max_capacity
  min_capacity       = var.min_capacity
  resource_id        = "service/${var.cluster_name}/${aws_ecs_service.typescript_api[0].name}"
  scalable_dimension = "ecs:service:DesiredCount"
  service_namespace  = "ecs"
  
  tags = local.common_tags
}

# Auto Scaling Policy (CPU)
resource "aws_appautoscaling_policy" "typescript_api_cpu" {
  count = var.enable_typescript_api && var.enable_auto_scaling ? 1 : 0
  
  name               = "${var.project_name}-${local.service_name}-cpu-scaling"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.typescript_api[0].resource_id
  scalable_dimension = aws_appautoscaling_target.typescript_api[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.typescript_api[0].service_namespace
  
  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ECSServiceAverageCPUUtilization"
    }
    
    target_value       = var.cpu_target_value
    scale_in_cooldown  = 300
    scale_out_cooldown = 300
  }
}

# Auto Scaling Policy (Memory)
resource "aws_appautoscaling_policy" "typescript_api_memory" {
  count = var.enable_typescript_api && var.enable_auto_scaling ? 1 : 0
  
  name               = "${var.project_name}-${local.service_name}-memory-scaling"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.typescript_api[0].resource_id
  scalable_dimension = aws_appautoscaling_target.typescript_api[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.typescript_api[0].service_namespace
  
  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ECSServiceAverageMemoryUtilization"
    }
    
    target_value       = var.memory_target_value
    scale_in_cooldown  = 300
    scale_out_cooldown = 300
  }
}

# PostHog Analytics Integration
module "posthog_analytics" {
  source = "./modules/posthog"
  
  enable_posthog = var.enable_posthog && var.enable_typescript_api
  
  # Service configuration
  service_name = local.service_name
  project_name = var.project_name
  environment  = var.environment
  provider     = var.provider
  
  # PostHog configuration
  posthog_organization_id = var.posthog_organization_id
  parent_organization_id  = var.parent_organization_id
  posthog_host           = var.posthog_host
  parent_project_id      = var.parent_project_id
  
  # Analytics settings
  analytics_rollout_percentage = var.analytics_rollout_percentage
  sampling_rate               = var.sampling_rate
  data_retention_days         = var.data_retention_days
  
  # Performance settings
  batch_size        = var.batch_size
  flush_interval_ms = var.flush_interval_ms
  
  # Feature toggles
  enable_console_logs           = var.enable_console_logs
  enable_debug_logging          = var.enable_debug_logging
  enable_performance_monitoring = var.enable_performance_monitoring
  enable_error_tracking         = var.enable_error_tracking
  
  # Webhook configuration
  webhook_endpoint = var.webhook_endpoint
}
