# Java Spring Boot API Service Infrastructure
# This module manages the Java Spring Boot API service deployment and infrastructure

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
  service_name = "java-api"
  container_port = 8040
  
  # Common tags
  common_tags = {
    Service     = local.service_name
    Backend     = "java"
    Port        = local.container_port
    Environment = var.environment
    Project     = var.project_name
  }
}

# ECS Task Definition
resource "aws_ecs_task_definition" "java_api" {
  count = var.enable_java_api ? 1 : 0
  
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
          name  = "SPRING_PROFILES_ACTIVE"
          value = var.environment
        },
        {
          name  = "SERVER_PORT"
          value = tostring(local.container_port)
        },
        {
          name  = "SPRING_DATASOURCE_URL"
          value = var.database_url
        },
        {
          name  = "SPRING_REDIS_URL"
          value = var.redis_url
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
          name  = "MINDSDB_URL"
          value = var.mindsdb_url
        },
        {
          name  = "KONG_ADMIN_URL"
          value = var.kong_admin_url
        },
        {
          name  = "POSTHOG_API_KEY"
          value = var.posthog_api_key
        },
        {
          name  = "POSTHOG_HOST"
          value = var.posthog_host
        }
      ]
      
      logConfiguration = {
        logDriver = "awslogs"
        options = {
          "awslogs-group"         = aws_cloudwatch_log_group.java_api[0].name
          "awslogs-region"        = var.aws_region
          "awslogs-stream-prefix" = "ecs"
        }
      }
      
      healthCheck = {
        command     = ["CMD-SHELL", "curl -f http://localhost:${local.container_port}/actuator/health || exit 1"]
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
resource "aws_ecs_service" "java_api" {
  count = var.enable_java_api ? 1 : 0
  
  name            = "${var.project_name}-${local.service_name}"
  cluster         = var.ecs_cluster_id
  task_definition = aws_ecs_task_definition.java_api[0].arn
  desired_count   = var.desired_count
  launch_type     = "FARGATE"

  network_configuration {
    subnets          = var.private_subnet_ids
    security_groups  = [aws_security_group.java_api[0].id]
    assign_public_ip = false
  }

  load_balancer {
    target_group_arn = aws_lb_target_group.java_api[0].arn
    container_name   = local.service_name
    container_port   = local.container_port
  }

  depends_on = [aws_lb_listener_rule.java_api]
  
  tags = local.common_tags
}

# Security Group
resource "aws_security_group" "java_api" {
  count = var.enable_java_api ? 1 : 0
  
  name_prefix = "${var.project_name}-${local.service_name}-"
  vpc_id      = var.vpc_id

  ingress {
    from_port   = local.container_port
    to_port     = local.container_port
    protocol    = "tcp"
    cidr_blocks = [var.vpc_cidr]
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
resource "aws_lb_target_group" "java_api" {
  count = var.enable_java_api ? 1 : 0
  
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
    path                = "/actuator/health"
    matcher             = "200"
    protocol            = "HTTP"
  }

  tags = local.common_tags
}

# Load Balancer Listener Rule
resource "aws_lb_listener_rule" "java_api" {
  count = var.enable_java_api ? 1 : 0
  
  listener_arn = var.alb_listener_arn
  priority     = var.listener_rule_priority

  action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.java_api[0].arn
  }

  condition {
    path_pattern {
      values = ["/api/v1/java/*"]
    }
  }

  tags = local.common_tags
}

# CloudWatch Log Group
resource "aws_cloudwatch_log_group" "java_api" {
  count = var.enable_java_api ? 1 : 0
  
  name              = "/ecs/${var.project_name}/${local.service_name}"
  retention_in_days = var.log_retention_days

  tags = local.common_tags
}

# IAM Role for ECS Task Execution
resource "aws_iam_role" "ecs_execution_role" {
  count = var.enable_java_api ? 1 : 0
  
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

# IAM Role Policy Attachment for ECS Task Execution
resource "aws_iam_role_policy_attachment" "ecs_execution_role_policy" {
  count = var.enable_java_api ? 1 : 0
  
  role       = aws_iam_role.ecs_execution_role[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

# IAM Role for ECS Task
resource "aws_iam_role" "ecs_task_role" {
  count = var.enable_java_api ? 1 : 0
  
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

# Auto Scaling Target
resource "aws_appautoscaling_target" "java_api" {
  count = var.enable_java_api && var.enable_auto_scaling ? 1 : 0
  
  max_capacity       = var.max_capacity
  min_capacity       = var.min_capacity
  resource_id        = "service/${var.ecs_cluster_name}/${aws_ecs_service.java_api[0].name}"
  scalable_dimension = "ecs:service:DesiredCount"
  service_namespace  = "ecs"

  tags = local.common_tags
}

# Auto Scaling Policy - CPU
resource "aws_appautoscaling_policy" "java_api_cpu" {
  count = var.enable_java_api && var.enable_auto_scaling ? 1 : 0
  
  name               = "${var.project_name}-${local.service_name}-cpu-scaling"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.java_api[0].resource_id
  scalable_dimension = aws_appautoscaling_target.java_api[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.java_api[0].service_namespace

  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ECSServiceAverageCPUUtilization"
    }
    target_value = var.cpu_target_value
  }
}

# Auto Scaling Policy - Memory
resource "aws_appautoscaling_policy" "java_api_memory" {
  count = var.enable_java_api && var.enable_auto_scaling ? 1 : 0
  
  name               = "${var.project_name}-${local.service_name}-memory-scaling"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.java_api[0].resource_id
  scalable_dimension = aws_appautoscaling_target.java_api[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.java_api[0].service_namespace

  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ECSServiceAverageMemoryUtilization"
    }
    target_value = var.memory_target_value
  }
}
