# Go Beego API Service Infrastructure
# This module manages the Go Beego API service deployment and infrastructure

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
  service_name = "go-api"
  container_port = 8040
  
  # Common tags
  common_tags = {
    Service     = local.service_name
    Backend     = "go"
    Port        = local.container_port
    Environment = var.environment
    Project     = var.project_name
  }
}

# ECS Task Definition
resource "aws_ecs_task_definition" "go_api" {
  count = var.enable_go_api ? 1 : 0
  
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
          name  = "PORT"
          value = tostring(local.container_port)
        },
        {
          name  = "GO_ENV"
          value = var.environment
        },
        {
          name  = "GIN_MODE"
          value = var.environment == "production" ? "release" : "debug"
        },
        {
          name  = "DB_HOST"
          value = var.database_host
        },
        {
          name  = "DB_PORT"
          value = "5432"
        },
        {
          name  = "DB_NAME"
          value = var.database_name
        },
        {
          name  = "DB_USER"
          value = var.database_username
        },
        {
          name  = "DB_SSL_MODE"
          value = var.environment == "production" ? "require" : "disable"
        },
        {
          name  = "REDIS_HOST"
          value = var.redis_host
        },
        {
          name  = "REDIS_PORT"
          value = "6379"
        },
        {
          name  = "REDIS_DB"
          value = "0"
        },
        {
          name  = "AUTH0_DOMAIN"
          value = var.auth0_domain
        },
        {
          name  = "AUTH0_CLIENT_ID"
          value = var.auth0_client_id
        },
        {
          name  = "AUTH0_AUDIENCE"
          value = var.auth0_audience
        },
        {
          name  = "MINDSDB_HOST"
          value = var.mindsdb_host
        },
        {
          name  = "MINDSDB_PORT"
          value = "47334"
        },
        {
          name  = "KONG_GATEWAY_URL"
          value = var.kong_gateway_url
        },
        {
          name  = "POSTHOG_API_KEY"
          value = var.posthog_api_key
        },
        {
          name  = "POSTHOG_HOST"
          value = var.posthog_host
        },
        {
          name  = "LOG_LEVEL"
          value = var.environment == "production" ? "info" : "debug"
        },
        {
          name  = "CORS_ALLOWED_ORIGINS"
          value = var.cors_allowed_origins
        }
      ]
      
      secrets = [
        {
          name      = "DB_PASSWORD"
          valueFrom = var.database_password_secret_arn
        },
        {
          name      = "AUTH0_CLIENT_SECRET"
          valueFrom = var.auth0_client_secret_arn
        },
        {
          name      = "REDIS_PASSWORD"
          valueFrom = var.redis_password_secret_arn
        },
        {
          name      = "JWT_SECRET"
          valueFrom = var.jwt_secret_arn
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
          awslogs-group         = aws_cloudwatch_log_group.go_api_logs[0].name
          awslogs-region        = var.aws_region
          awslogs-stream-prefix = "ecs"
        }
      }
    }
  ])

  tags = local.common_tags
}

# ECS Service
resource "aws_ecs_service" "go_api" {
  count = var.enable_go_api ? 1 : 0
  
  name            = "${var.project_name}-${local.service_name}"
  cluster         = var.ecs_cluster_id
  task_definition = aws_ecs_task_definition.go_api[0].arn
  desired_count   = var.desired_count
  launch_type     = "FARGATE"
  
  platform_version = "LATEST"
  
  network_configuration {
    subnets          = var.private_subnet_ids
    security_groups  = [aws_security_group.go_api[0].id]
    assign_public_ip = false
  }
  
  load_balancer {
    target_group_arn = aws_lb_target_group.go_api[0].arn
    container_name   = local.service_name
    container_port   = local.container_port
  }
  
  service_registries {
    registry_arn = aws_service_discovery_service.go_api[0].arn
  }
  
  deployment_configuration {
    maximum_percent         = 200
    minimum_healthy_percent = 100
    
    deployment_circuit_breaker {
      enable   = true
      rollback = true
    }
  }
  
  enable_execute_command = var.enable_execute_command
  
  tags = local.common_tags
  
  depends_on = [
    aws_lb_listener_rule.go_api,
    aws_iam_role_policy_attachment.ecs_execution_role_policy,
    aws_iam_role_policy_attachment.ecs_task_role_policy
  ]
}

# Auto Scaling Target
resource "aws_appautoscaling_target" "go_api" {
  count = var.enable_go_api && var.enable_autoscaling ? 1 : 0
  
  max_capacity       = var.max_capacity
  min_capacity       = var.min_capacity
  resource_id        = "service/${var.ecs_cluster_name}/${aws_ecs_service.go_api[0].name}"
  scalable_dimension = "ecs:service:DesiredCount"
  service_namespace  = "ecs"
  
  tags = local.common_tags
}

# Auto Scaling Policy - CPU
resource "aws_appautoscaling_policy" "go_api_cpu" {
  count = var.enable_go_api && var.enable_autoscaling ? 1 : 0
  
  name               = "${var.project_name}-${local.service_name}-cpu"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.go_api[0].resource_id
  scalable_dimension = aws_appautoscaling_target.go_api[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.go_api[0].service_namespace
  
  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ECSServiceAverageCPUUtilization"
    }
    target_value = var.cpu_target_value
  }
}

# Auto Scaling Policy - Memory
resource "aws_appautoscaling_policy" "go_api_memory" {
  count = var.enable_go_api && var.enable_autoscaling ? 1 : 0
  
  name               = "${var.project_name}-${local.service_name}-memory"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.go_api[0].resource_id
  scalable_dimension = aws_appautoscaling_target.go_api[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.go_api[0].service_namespace
  
  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ECSServiceAverageMemoryUtilization"
    }
    target_value = var.memory_target_value
  }
}

# Security Group
resource "aws_security_group" "go_api" {
  count = var.enable_go_api ? 1 : 0
  
  name_prefix = "${var.project_name}-${local.service_name}-"
  vpc_id      = var.vpc_id
  
  ingress {
    from_port       = local.container_port
    to_port         = local.container_port
    protocol        = "tcp"
    security_groups = [var.alb_security_group_id]
  }
  
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  
  tags = merge(local.common_tags, {
    Name = "${var.project_name}-${local.service_name}-sg"
  })
}

# Load Balancer Target Group
resource "aws_lb_target_group" "go_api" {
  count = var.enable_go_api ? 1 : 0
  
  name     = "${var.project_name}-${local.service_name}-tg"
  port     = local.container_port
  protocol = "HTTP"
  vpc_id   = var.vpc_id
  
  target_type = "ip"
  
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
resource "aws_lb_listener_rule" "go_api" {
  count = var.enable_go_api ? 1 : 0
  
  listener_arn = var.alb_listener_arn
  priority     = var.listener_rule_priority
  
  action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.go_api[0].arn
  }
  
  condition {
    path_pattern {
      values = ["/api/v1/go/*"]
    }
  }
  
  tags = local.common_tags
}

# CloudWatch Log Group
resource "aws_cloudwatch_log_group" "go_api_logs" {
  count = var.enable_go_api ? 1 : 0
  
  name              = "/ecs/${var.project_name}-${local.service_name}"
  retention_in_days = var.log_retention_days
  
  tags = local.common_tags
}

# Service Discovery Service
resource "aws_service_discovery_service" "go_api" {
  count = var.enable_go_api ? 1 : 0
  
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

# IAM Role for ECS Execution
resource "aws_iam_role" "ecs_execution_role" {
  count = var.enable_go_api ? 1 : 0
  
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

# IAM Role for ECS Task
resource "aws_iam_role" "ecs_task_role" {
  count = var.enable_go_api ? 1 : 0
  
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

# IAM Policy Attachment for ECS Execution Role
resource "aws_iam_role_policy_attachment" "ecs_execution_role_policy" {
  count = var.enable_go_api ? 1 : 0
  
  role       = aws_iam_role.ecs_execution_role[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

# IAM Policy Attachment for ECS Task Role
resource "aws_iam_role_policy_attachment" "ecs_task_role_policy" {
  count = var.enable_go_api ? 1 : 0
  
  role       = aws_iam_role.ecs_task_role[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskRolePolicy"
}

# Custom IAM Policy for Secrets Manager
resource "aws_iam_policy" "secrets_manager_policy" {
  count = var.enable_go_api ? 1 : 0
  
  name        = "${var.project_name}-${local.service_name}-secrets-policy"
  description = "Policy for accessing secrets manager"
  
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "secretsmanager:GetSecretValue"
        ]
        Resource = [
          var.database_password_secret_arn,
          var.auth0_client_secret_arn,
          var.redis_password_secret_arn,
          var.jwt_secret_arn
        ]
      }
    ]
  })
  
  tags = local.common_tags
}

# Attach Secrets Manager Policy to Task Role
resource "aws_iam_role_policy_attachment" "secrets_manager_policy" {
  count = var.enable_go_api ? 1 : 0
  
  role       = aws_iam_role.ecs_task_role[0].name
  policy_arn = aws_iam_policy.secrets_manager_policy[0].arn
}

# Kong Gateway Integration
resource "aws_ssm_parameter" "kong_service_config" {
  count = var.enable_go_api && var.enable_kong_integration ? 1 : 0
  
  name  = "/kong/services/${local.service_name}/config"
  type  = "String"
  value = jsonencode({
    name     = local.service_name
    url      = "http://${local.service_name}.${var.service_discovery_namespace_name}:${local.container_port}"
    protocol = "http"
    host     = "${local.service_name}.${var.service_discovery_namespace_name}"
    port     = local.container_port
    path     = "/api/v1/go"
    plugins = [
      {
        name = "jwt"
        config = {
          secret_is_base64 = false
        }
      },
      {
        name = "rate-limiting"
        config = {
          minute = 100
          policy = "local"
        }
      },
      {
        name = "cors"
        config = {
          origins = ["*"]
          methods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
          headers = ["Accept", "Accept-Version", "Content-Length", "Content-MD5", "Content-Type", "Date", "X-Auth-Token", "Authorization"]
          credentials = true
        }
      },
      {
        name = "prometheus"
        config = {
          per_consumer = true
        }
      }
    ]
  })
  
  tags = local.common_tags
}

# PostHog Integration
module "posthog_integration" {
  source = "./modules/posthog"
  count  = var.enable_go_api && var.enable_posthog ? 1 : 0
  
  service_name    = local.service_name
  project_name    = var.project_name
  environment     = var.environment
  posthog_api_key = var.posthog_api_key
  posthog_host    = var.posthog_host
  
  common_tags = local.common_tags
}
