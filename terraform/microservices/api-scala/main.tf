# Scala API Service Infrastructure
# This module manages the Scala API service deployment and infrastructure

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
  service_name = "scala-api"
  container_port = 8060
  
  # Common tags
  common_tags = {
    Service     = local.service_name
    Backend     = "scala"
    Port        = local.container_port
    Environment = var.environment
    Project     = var.project_name
  }
}

# ECS Task Definition
resource "aws_ecs_task_definition" "scala_api" {
  count = var.enable_scala_api ? 1 : 0
  
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
          name  = "NODE_ENV"
          value = var.environment
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
          name  = "AUTH0_CLIENT_ID"
          value = var.auth0_client_id
        },
        {
          name  = "AUTH0_CLIENT_SECRET"
          value = var.auth0_client_secret
        },
        {
          name  = "MINDSDB_HOST"
          value = var.mindsdb_host
        },
        {
          name  = "MINDSDB_PORT"
          value = tostring(var.mindsdb_port)
        },
        {
          name  = "KONG_PROXY_URL"
          value = var.kong_proxy_url
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
        },
        {
          name  = "LOG_LEVEL"
          value = var.log_level
        },
        {
          name  = "SCALA_OPTS"
          value = var.scala_opts
        },
        {
          name  = "JAVA_OPTS"
          value = var.java_opts
        },
        {
          name  = "PLAY_HTTP_SECRET_KEY"
          value = var.play_secret_key
        }
      ]
      
      logConfiguration = {
        logDriver = "awslogs"
        options = {
          "awslogs-group"         = aws_cloudwatch_log_group.scala_api[0].name
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

  tags = merge(local.common_tags, var.tags)
}

# CloudWatch Log Group
resource "aws_cloudwatch_log_group" "scala_api" {
  count = var.enable_scala_api ? 1 : 0
  
  name              = "/ecs/${var.project_name}-${local.service_name}"
  retention_in_days = var.log_retention_days
  
  tags = merge(local.common_tags, var.tags)
}

# ECS Service
resource "aws_ecs_service" "scala_api" {
  count = var.enable_scala_api ? 1 : 0
  
  name            = "${var.project_name}-${local.service_name}"
  cluster         = var.ecs_cluster_id
  task_definition = aws_ecs_task_definition.scala_api[0].arn
  desired_count   = var.desired_count
  launch_type     = "FARGATE"
  
  network_configuration {
    subnets         = var.private_subnet_ids
    security_groups = [aws_security_group.scala_api[0].id]
  }
  
  load_balancer {
    target_group_arn = aws_lb_target_group.scala_api[0].arn
    container_name   = local.service_name
    container_port   = local.container_port
  }
  
  depends_on = [aws_lb_target_group.scala_api]
  
  tags = merge(local.common_tags, var.tags)
}

# Security Group
resource "aws_security_group" "scala_api" {
  count = var.enable_scala_api ? 1 : 0
  
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
  
  tags = merge(local.common_tags, var.tags)
}

# Load Balancer Target Group
resource "aws_lb_target_group" "scala_api" {
  count = var.enable_scala_api ? 1 : 0
  
  name        = "${var.project_name}-${local.service_name}-tg"
  port        = local.container_port
  protocol    = "HTTP"
  vpc_id      = var.vpc_id
  target_type = "ip"
  
  health_check {
    enabled             = true
    healthy_threshold   = 2
    unhealthy_threshold = 3
    timeout             = 5
    interval            = 30
    path                = "/health"
    matcher             = "200"
    protocol            = "HTTP"
  }
  
  tags = merge(local.common_tags, var.tags)
}

# Load Balancer Listener Rule
resource "aws_lb_listener_rule" "scala_api" {
  count = var.enable_scala_api ? 1 : 0
  
  listener_arn = var.alb_listener_arn
  priority     = var.listener_priority
  
  action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.scala_api[0].arn
  }
  
  condition {
    path_pattern {
      values = ["/api/v1/scala/*"]
    }
  }
  
  tags = merge(local.common_tags, var.tags)
}

# IAM Role for ECS Task Execution
resource "aws_iam_role" "ecs_execution_role" {
  count = var.enable_scala_api ? 1 : 0
  
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
  
  tags = merge(local.common_tags, var.tags)
}

# IAM Role Policy Attachment for ECS Task Execution
resource "aws_iam_role_policy_attachment" "ecs_execution_role_policy" {
  count = var.enable_scala_api ? 1 : 0
  
  role       = aws_iam_role.ecs_execution_role[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

# IAM Role for ECS Task
resource "aws_iam_role" "ecs_task_role" {
  count = var.enable_scala_api ? 1 : 0
  
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
  
  tags = merge(local.common_tags, var.tags)
}

# Service Discovery
resource "aws_service_discovery_service" "scala_api" {
  count = var.enable_scala_api && var.enable_service_discovery ? 1 : 0
  
  name = local.service_name
  
  dns_config {
    namespace_id = var.service_discovery_namespace_id
    
    dns_records {
      ttl  = 10
      type = "A"
    }
  }
  
  health_check_grace_period_seconds = 30
  
  tags = merge(local.common_tags, var.tags)
}

# Auto Scaling Target
resource "aws_appautoscaling_target" "scala_api" {
  count = var.enable_scala_api && var.enable_auto_scaling ? 1 : 0
  
  max_capacity       = var.max_capacity
  min_capacity       = var.min_capacity
  resource_id        = "service/${var.ecs_cluster_name}/${aws_ecs_service.scala_api[0].name}"
  scalable_dimension = "ecs:service:DesiredCount"
  service_namespace  = "ecs"
  
  tags = merge(local.common_tags, var.tags)
}

# Auto Scaling Policy - CPU
resource "aws_appautoscaling_policy" "scala_api_cpu" {
  count = var.enable_scala_api && var.enable_auto_scaling ? 1 : 0
  
  name               = "${var.project_name}-${local.service_name}-cpu-scaling"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.scala_api[0].resource_id
  scalable_dimension = aws_appautoscaling_target.scala_api[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.scala_api[0].service_namespace
  
  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ECSServiceAverageCPUUtilization"
    }
    
    target_value = var.cpu_target_value
  }
}

# Auto Scaling Policy - Memory
resource "aws_appautoscaling_policy" "scala_api_memory" {
  count = var.enable_scala_api && var.enable_auto_scaling ? 1 : 0
  
  name               = "${var.project_name}-${local.service_name}-memory-scaling"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.scala_api[0].resource_id
  scalable_dimension = aws_appautoscaling_target.scala_api[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.scala_api[0].service_namespace
  
  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ECSServiceAverageMemoryUtilization"
    }
    
    target_value = var.memory_target_value
  }
}

# Kong Service Registration
resource "null_resource" "register_kong_service" {
  count = var.enable_scala_api && var.enable_kong_integration ? 1 : 0
  
  provisioner "local-exec" {
    command = <<-EOF
      curl -i -X POST ${var.kong_admin_url}/services \
        --data "name=${local.service_name}" \
        --data "url=http://${aws_lb_target_group.scala_api[0].arn}:${local.container_port}"
    EOF
  }
  
  depends_on = [aws_ecs_service.scala_api]
}

# Kong Route Registration
resource "null_resource" "register_kong_route" {
  count = var.enable_scala_api && var.enable_kong_integration ? 1 : 0
  
  provisioner "local-exec" {
    command = <<-EOF
      curl -i -X POST ${var.kong_admin_url}/services/${local.service_name}/routes \
        --data "paths[]=/api/v1/scala"
    EOF
  }
  
  depends_on = [null_resource.register_kong_service]
}
