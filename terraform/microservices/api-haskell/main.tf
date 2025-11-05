# Haskell Servant API Service Infrastructure
# This module manages the Haskell Servant API service deployment and infrastructure

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
  service_name = "haskell-api"
  container_port = 8110
  
  # Common tags
  common_tags = {
    Service     = local.service_name
    Backend     = "haskell"
    Port        = local.container_port
    Environment = var.environment
    Project     = var.project_name
  }
}

# ECS Task Definition
resource "aws_ecs_task_definition" "haskell_api" {
  count = var.enable_haskell_api ? 1 : 0
  
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
          hostPort      = local.container_port
          protocol      = "tcp"
        }
      ]

      environment = [
        {
          name  = "PORT"
          value = tostring(local.container_port)
        },
        {
          name  = "ENVIRONMENT"
          value = var.environment
        }
      ]

      secrets = [
        {
          name      = "AUTH0_DOMAIN"
          valueFrom = var.auth0_domain_secret_arn
        },
        {
          name      = "AUTH0_AUDIENCE"
          valueFrom = var.auth0_audience_secret_arn
        },
        {
          name      = "AUTH0_CLIENT_ID"
          valueFrom = var.auth0_client_id_secret_arn
        },
        {
          name      = "AUTH0_CLIENT_SECRET"
          valueFrom = var.auth0_client_secret_secret_arn
        },
        {
          name      = "MINDSDB_HOST"
          valueFrom = var.mindsdb_host_secret_arn
        },
        {
          name      = "MINDSDB_PORT"
          valueFrom = var.mindsdb_port_secret_arn
        },
        {
          name      = "MINDSDB_USER"
          valueFrom = var.mindsdb_user_secret_arn
        },
        {
          name      = "MINDSDB_PASSWORD"
          valueFrom = var.mindsdb_password_secret_arn
        }
      ]

      logConfiguration = {
        logDriver = "awslogs"
        options = {
          "awslogs-create-group"  = "true"
          "awslogs-group"         = "/ecs/${var.project_name}/${local.service_name}"
          "awslogs-region"        = var.aws_region
          "awslogs-stream-prefix" = "ecs"
        }
      }

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
    }
  ])

  tags = local.common_tags
}

# ECS Service
resource "aws_ecs_service" "haskell_api" {
  count = var.enable_haskell_api ? 1 : 0
  
  name            = "${var.project_name}-${local.service_name}"
  cluster         = var.ecs_cluster_id
  task_definition = aws_ecs_task_definition.haskell_api[0].arn
  desired_count   = var.desired_count

  capacity_provider_strategy {
    capacity_provider = "FARGATE"
    weight           = 100
  }

  network_configuration {
    subnets          = var.private_subnet_ids
    security_groups  = [aws_security_group.haskell_api[0].id]
    assign_public_ip = false
  }

  load_balancer {
    target_group_arn = aws_lb_target_group.haskell_api[0].arn
    container_name   = local.service_name
    container_port   = local.container_port
  }

  service_registries {
    registry_arn = aws_service_discovery_service.haskell_api[0].arn
  }

  depends_on = [
    aws_lb_listener_rule.haskell_api,
    aws_iam_role_policy_attachment.ecs_execution_role_policy,
    aws_iam_role_policy_attachment.ecs_task_role_policy
  ]

  tags = local.common_tags
}

# Security Group
resource "aws_security_group" "haskell_api" {
  count = var.enable_haskell_api ? 1 : 0
  
  name_prefix = "${var.project_name}-${local.service_name}-"
  vpc_id      = var.vpc_id

  ingress {
    description = "API Port"
    from_port   = local.container_port
    to_port     = local.container_port
    protocol    = "tcp"
    cidr_blocks = [var.vpc_cidr]
  }

  egress {
    description = "All outbound traffic"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(local.common_tags, {
    Name = "${var.project_name}-${local.service_name}-sg"
  })
}

# Application Load Balancer Target Group
resource "aws_lb_target_group" "haskell_api" {
  count = var.enable_haskell_api ? 1 : 0
  
  name        = "${var.project_name}-${local.service_name}-tg"
  port        = local.container_port
  protocol    = "HTTP"
  vpc_id      = var.vpc_id
  target_type = "ip"

  health_check {
    enabled             = true
    healthy_threshold   = 2
    interval            = 30
    matcher             = "200"
    path                = "/health"
    port                = "traffic-port"
    protocol            = "HTTP"
    timeout             = 5
    unhealthy_threshold = 2
  }

  tags = local.common_tags
}

# Load Balancer Listener Rule
resource "aws_lb_listener_rule" "haskell_api" {
  count = var.enable_haskell_api ? 1 : 0
  
  listener_arn = var.alb_listener_arn
  priority     = var.listener_rule_priority

  action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.haskell_api[0].arn
  }

  condition {
    path_pattern {
      values = ["/api/haskell/*"]
    }
  }

  tags = local.common_tags
}

# Service Discovery Service
resource "aws_service_discovery_service" "haskell_api" {
  count = var.enable_haskell_api ? 1 : 0
  
  name = local.service_name

  dns_config {
    namespace_id = var.service_discovery_namespace_id

    dns_records {
      ttl  = 10
      type = "A"
    }

    routing_policy = "MULTIVALUE"
  }

  health_check_grace_period_seconds = 10

  tags = local.common_tags
}

# IAM Role for ECS Execution
resource "aws_iam_role" "ecs_execution_role" {
  count = var.enable_haskell_api ? 1 : 0
  
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
  count = var.enable_haskell_api ? 1 : 0
  
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
  count = var.enable_haskell_api ? 1 : 0
  
  role       = aws_iam_role.ecs_execution_role[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

# IAM Policy Attachment for ECS Task Role
resource "aws_iam_role_policy_attachment" "ecs_task_role_policy" {
  count = var.enable_haskell_api ? 1 : 0
  
  role       = aws_iam_role.ecs_task_role[0].name
  policy_arn = "arn:aws:iam::aws:policy/CloudWatchAgentServerPolicy"
}

# CloudWatch Log Group
resource "aws_cloudwatch_log_group" "haskell_api" {
  count = var.enable_haskell_api ? 1 : 0
  
  name              = "/ecs/${var.project_name}/${local.service_name}"
  retention_in_days = var.log_retention_days

  tags = local.common_tags
}
