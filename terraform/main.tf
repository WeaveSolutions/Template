# Nexpo Polyglot Backend Infrastructure
# Auto-generated Terraform configuration

terraform {
  required_version = ">= 1.0"
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

# AWS Provider Configuration
provider "aws" {
  region = var.aws_region
}

# Docker Provider Configuration  
provider "docker" {
  host = "unix:///var/run/docker.sock"
}

# Variables
variable "aws_region" {
  description = "AWS region for infrastructure"
  type        = string
  default     = "us-west-2"
}

variable "environment" {
  description = "Environment (staging, production)"
  type        = string
  default     = "staging"
}

variable "project_name" {
  description = "Project name prefix"
  type        = string
  default     = "nexpo"
}

# Backend Feature Flags

variable "enable_typescript_api" {
  description = "Enable TypeScript Express API"
  type        = bool
  default     = false
}
variable "enable_python_api" {
  description = "Enable Python FastAPI"
  type        = bool
  default     = false
}
variable "enable_go_api" {
  description = "Enable Go Beego API"
  type        = bool
  default     = false
}
variable "enable_rust_api" {
  description = "Enable Rust Actix API"
  type        = bool
  default     = false
}
variable "enable_scala_api" {
  description = "Enable Scala Play API"
  type        = bool
  default     = false
}
variable "enable_java_api" {
  description = "Enable Java Play API"
  type        = bool
  default     = false
}
variable "enable_r_api" {
  description = "Enable R Plumber API"
  type        = bool
  default     = false
}
variable "enable_julia_api" {
  description = "Enable Julia Genie API"
  type        = bool
  default     = false
}
variable "enable_cpp_api" {
  description = "Enable C++ Drogon API"
  type        = bool
  default     = false
}
variable "enable_dotnet_api" {
  description = "Enable .NET ASP.NET Core API"
  type        = bool
  default     = false
}
variable "enable_haskell_api" {
  description = "Enable Haskell Servant API"
  type        = bool
  default     = false
}

# VPC Configuration
resource "aws_vpc" "main" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = {
    Name        = "${var.project_name}-${var.environment}-vpc"
    Environment = var.environment
  }
}

# Internet Gateway
resource "aws_internet_gateway" "main" {
  vpc_id = aws_vpc.main.id

  tags = {
    Name        = "${var.project_name}-${var.environment}-igw"
    Environment = var.environment
  }
}

# Public Subnets
resource "aws_subnet" "public" {
  count = 2

  vpc_id                  = aws_vpc.main.id
  cidr_block              = "10.0.${count.index + 1}.0/24"
  availability_zone       = data.aws_availability_zones.available.names[count.index]
  map_public_ip_on_launch = true

  tags = {
    Name        = "${var.project_name}-${var.environment}-public-${count.index + 1}"
    Environment = var.environment
  }
}

# Route Table
resource "aws_route_table" "public" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.main.id
  }

  tags = {
    Name        = "${var.project_name}-${var.environment}-public-rt"
    Environment = var.environment
  }
}

# Route Table Association
resource "aws_route_table_association" "public" {
  count = length(aws_subnet.public)

  subnet_id      = aws_subnet.public[count.index].id
  route_table_id = aws_route_table.public.id
}

# Security Group for Backend APIs
resource "aws_security_group" "backend_apis" {
  name_prefix = "${var.project_name}-${var.environment}-backends"
  vpc_id      = aws_vpc.main.id

  # Allow HTTP traffic
  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  # Allow HTTPS traffic
  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  # Allow backend ports

  # TypeScript Express API (Port 8020)
  ingress {
    from_port   = 8020
    to_port     = 8020
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  # Python FastAPI (Port 8030)
  ingress {
    from_port   = 8030
    to_port     = 8030
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  # Go Beego API (Port 8040)
  ingress {
    from_port   = 8040
    to_port     = 8040
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  # Rust Actix API (Port 8050)
  ingress {
    from_port   = 8050
    to_port     = 8050
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  # Scala Play API (Port 8060)
  ingress {
    from_port   = 8060
    to_port     = 8060
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  # Java Play API (Port 8070)
  ingress {
    from_port   = 8070
    to_port     = 8070
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  # R Plumber API (Port 8080)
  ingress {
    from_port   = 8080
    to_port     = 8080
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  # Julia Genie API (Port 8090)
  ingress {
    from_port   = 8090
    to_port     = 8090
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  # Allow outbound traffic
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name        = "${var.project_name}-${var.environment}-backend-sg"
    Environment = var.environment
  }
}

# Data source for availability zones
data "aws_availability_zones" "available" {
  state = "available"
}

# Application Load Balancer
resource "aws_lb" "main" {
  name               = "${var.project_name}-${var.environment}-alb"
  internal           = false
  load_balancer_type = "application"
  security_groups    = [aws_security_group.backend_apis.id]
  subnets            = aws_subnet.public[*].id

  enable_deletion_protection = false

  tags = {
    Name        = "${var.project_name}-${var.environment}-alb"
    Environment = var.environment
  }
}

# ALB Listener
resource "aws_lb_listener" "main" {
  load_balancer_arn = aws_lb.main.arn
  port              = "80"
  protocol          = "HTTP"

  default_action {
    type = "fixed-response"
    fixed_response {
      content_type = "text/plain"
      message_body = "Nexpo API Gateway"
      status_code  = "200"
    }
  }
}

# ECS Cluster
resource "aws_ecs_cluster" "main" {
  name = "${var.project_name}-${var.environment}"

  setting {
    name  = "containerInsights"
    value = "enabled"
  }

  tags = {
    Name        = "${var.project_name}-${var.environment}-cluster"
    Environment = var.environment
  }
}

# Backend Service Modules

module "typescript_api" {
  count = var.enable_typescript_api ? 1 : 0
  
  source = "./modules/backend-service"
  
  service_name    = "typescript-api"
  container_image = "nexpo/api-typescript:latest"
  container_port  = 8020
  
  cluster_id        = aws_ecs_cluster.main.id
  vpc_id           = aws_vpc.main.id
  subnet_ids       = aws_subnet.public[*].id
  security_group_ids = [aws_security_group.backend_apis.id]
  load_balancer_arn = aws_lb.main.arn
  listener_arn     = aws_lb_listener.main.arn
  
  environment      = var.environment
  project_name     = var.project_name
  
  # Service-specific configuration
  desired_count    = 1
  cpu             = 256
  memory          = 512
  
  tags = {
    Backend     = "typescript"
    Port        = "8020"
    Environment = var.environment
  }
}
module "python_api" {
  count = var.enable_python_api ? 1 : 0
  
  source = "./modules/backend-service"
  
  service_name    = "python-api"
  container_image = "nexpo/api-python:latest"
  container_port  = 8030
  
  cluster_id        = aws_ecs_cluster.main.id
  vpc_id           = aws_vpc.main.id
  subnet_ids       = aws_subnet.public[*].id
  security_group_ids = [aws_security_group.backend_apis.id]
  load_balancer_arn = aws_lb.main.arn
  listener_arn     = aws_lb_listener.main.arn
  
  environment      = var.environment
  project_name     = var.project_name
  
  # Service-specific configuration
  desired_count    = 1
  cpu             = 256
  memory          = 512
  
  tags = {
    Backend     = "python"
    Port        = "8030"
    Environment = var.environment
  }
}
module "go_api" {
  count = var.enable_go_api ? 1 : 0
  
  source = "./modules/backend-service"
  
  service_name    = "go-api"
  container_image = "nexpo/api-go:latest"
  container_port  = 8040
  
  cluster_id        = aws_ecs_cluster.main.id
  vpc_id           = aws_vpc.main.id
  subnet_ids       = aws_subnet.public[*].id
  security_group_ids = [aws_security_group.backend_apis.id]
  load_balancer_arn = aws_lb.main.arn
  listener_arn     = aws_lb_listener.main.arn
  
  environment      = var.environment
  project_name     = var.project_name
  
  # Service-specific configuration
  desired_count    = 1
  cpu             = 256
  memory          = 512
  
  tags = {
    Backend     = "go"
    Port        = "8040"
    Environment = var.environment
  }
}
module "rust_api" {
  count = var.enable_rust_api ? 1 : 0
  
  source = "./modules/backend-service"
  
  service_name    = "rust-api"
  container_image = "nexpo/api-rust:latest"
  container_port  = 8050
  
  cluster_id        = aws_ecs_cluster.main.id
  vpc_id           = aws_vpc.main.id
  subnet_ids       = aws_subnet.public[*].id
  security_group_ids = [aws_security_group.backend_apis.id]
  load_balancer_arn = aws_lb.main.arn
  listener_arn     = aws_lb_listener.main.arn
  
  environment      = var.environment
  project_name     = var.project_name
  
  # Service-specific configuration
  desired_count    = 1
  cpu             = 256
  memory          = 512
  
  tags = {
    Backend     = "rust"
    Port        = "8050"
    Environment = var.environment
  }
}
module "scala_api" {
  count = var.enable_scala_api ? 1 : 0
  
  source = "./modules/backend-service"
  
  service_name    = "scala-api"
  container_image = "nexpo/api-scala:latest"
  container_port  = 8060
  
  cluster_id        = aws_ecs_cluster.main.id
  vpc_id           = aws_vpc.main.id
  subnet_ids       = aws_subnet.public[*].id
  security_group_ids = [aws_security_group.backend_apis.id]
  load_balancer_arn = aws_lb.main.arn
  listener_arn     = aws_lb_listener.main.arn
  
  environment      = var.environment
  project_name     = var.project_name
  
  # Service-specific configuration
  desired_count    = 1
  cpu             = 256
  memory          = 512
  
  tags = {
    Backend     = "scala"
    Port        = "8060"
    Environment = var.environment
  }
}
module "java_api" {
  count = var.enable_java_api ? 1 : 0
  
  source = "./modules/backend-service"
  
  service_name    = "java-api"
  container_image = "nexpo/api-java:latest"
  container_port  = 8070
  
  cluster_id        = aws_ecs_cluster.main.id
  vpc_id           = aws_vpc.main.id
  subnet_ids       = aws_subnet.public[*].id
  security_group_ids = [aws_security_group.backend_apis.id]
  load_balancer_arn = aws_lb.main.arn
  listener_arn     = aws_lb_listener.main.arn
  
  environment      = var.environment
  project_name     = var.project_name
  
  # Service-specific configuration
  desired_count    = 1
  cpu             = 256
  memory          = 512
  
  tags = {
    Backend     = "java"
    Port        = "8070"
    Environment = var.environment
  }
}
module "r_api" {
  count = var.enable_r_api ? 1 : 0
  
  source = "./modules/backend-service"
  
  service_name    = "r-api"
  container_image = "nexpo/api-r:latest"
  container_port  = 8080
  
  cluster_id        = aws_ecs_cluster.main.id
  vpc_id           = aws_vpc.main.id
  subnet_ids       = aws_subnet.public[*].id
  security_group_ids = [aws_security_group.backend_apis.id]
  load_balancer_arn = aws_lb.main.arn
  listener_arn     = aws_lb_listener.main.arn
  
  environment      = var.environment
  project_name     = var.project_name
  
  # Service-specific configuration
  desired_count    = 1
  cpu             = 256
  memory          = 512
  
  tags = {
    Backend     = "r"
    Port        = "8080"
    Environment = var.environment
  }
}
module "julia_api" {
  count = var.enable_julia_api ? 1 : 0
  
  source = "./modules/backend-service"
  
  service_name    = "julia-api"
  container_image = "nexpo/api-julia:latest"
  container_port  = 8090
  
  cluster_id        = aws_ecs_cluster.main.id
  vpc_id           = aws_vpc.main.id
  subnet_ids       = aws_subnet.public[*].id
  security_group_ids = [aws_security_group.backend_apis.id]
  load_balancer_arn = aws_lb.main.arn
  listener_arn     = aws_lb_listener.main.arn
  
  environment      = var.environment
  project_name     = var.project_name
  
  # Service-specific configuration
  desired_count    = 1
  cpu             = 256
  memory          = 512
  
  tags = {
    Backend     = "julia"
    Port        = "8090"
    Environment = var.environment
  }
}
module "cpp_api" {
  count = var.enable_cpp_api ? 1 : 0
  
  source = "./modules/backend-service"
  
  service_name    = "cpp-api"
  container_image = "nexpo/api-cpp:latest"
  container_port  = 8100
  
  cluster_id        = aws_ecs_cluster.main.id
  vpc_id           = aws_vpc.main.id
  subnet_ids       = aws_subnet.public[*].id
  security_group_ids = [aws_security_group.backend_apis.id]
  load_balancer_arn = aws_lb.main.arn
  listener_arn     = aws_lb_listener.main.arn
  
  environment      = var.environment
  project_name     = var.project_name
  
  # Service-specific configuration
  desired_count    = 1
  cpu             = 512
  memory          = 1024
  
  tags = {
    Backend     = "cpp"
    Port        = "8100"
    Environment = var.environment
  }
}

# .NET ASP.NET Core API Module
module "dotnet_api" {
  count = var.enable_dotnet_api ? 1 : 0
  
  source = "./microservices/api-dotnet"
  
  # Module configuration
  enable_dotnet_api = var.enable_dotnet_api
  project_name      = var.project_name
  environment       = var.environment
  aws_region        = var.aws_region
  
  # Networking
  vpc_id              = aws_vpc.main.id
  vpc_cidr            = aws_vpc.main.cidr_block
  private_subnet_ids  = aws_subnet.public[*].id
  ecs_cluster_id      = aws_ecs_cluster.main.id
  alb_listener_arn    = aws_lb_listener.main.arn
  
  # Service Discovery
  service_discovery_namespace_id = aws_service_discovery_private_dns_namespace.main.id
  
  # Auth0 and MindsDB secrets (these would need to be created)
  auth0_domain_secret_arn      = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:auth0-domain"
  auth0_audience_secret_arn    = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:auth0-audience"
  auth0_client_id_secret_arn   = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:auth0-client-id"
  auth0_client_secret_secret_arn = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:auth0-client-secret"
  mindsdb_host_secret_arn      = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:mindsdb-host"
  mindsdb_port_secret_arn      = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:mindsdb-port"
  mindsdb_user_secret_arn      = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:mindsdb-user"
  mindsdb_password_secret_arn  = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:mindsdb-password"
}

# Haskell Servant API Module
module "haskell_api" {
  count = var.enable_haskell_api ? 1 : 0
  
  source = "./microservices/api-haskell"
  
  # Module configuration
  enable_haskell_api = var.enable_haskell_api
  project_name       = var.project_name
  environment        = var.environment
  aws_region         = var.aws_region
  
  # Networking
  vpc_id              = aws_vpc.main.id
  vpc_cidr            = aws_vpc.main.cidr_block
  private_subnet_ids  = aws_subnet.public[*].id
  ecs_cluster_id      = aws_ecs_cluster.main.id
  alb_listener_arn    = aws_lb_listener.main.arn
  
  # Service Discovery
  service_discovery_namespace_id = aws_service_discovery_private_dns_namespace.main.id
  
  # Auth0 and MindsDB secrets (these would need to be created)
  auth0_domain_secret_arn      = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:auth0-domain"
  auth0_audience_secret_arn    = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:auth0-audience"
  auth0_client_id_secret_arn   = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:auth0-client-id"
  auth0_client_secret_secret_arn = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:auth0-client-secret"
  mindsdb_host_secret_arn      = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:mindsdb-host"
  mindsdb_port_secret_arn      = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:mindsdb-port"
  mindsdb_user_secret_arn      = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:mindsdb-user"
  mindsdb_password_secret_arn  = "arn:aws:secretsmanager:${var.aws_region}:123456789012:secret:mindsdb-password"
}

# Outputs
output "load_balancer_dns" {
  description = "DNS name of the load balancer"
  value       = aws_lb.main.dns_name
}

output "vpc_id" {
  description = "ID of the VPC"
  value       = aws_vpc.main.id
}

output "cluster_name" {
  description = "Name of the ECS cluster"
  value       = aws_ecs_cluster.main.name
}


output "typescript_api_service_name" {
  description = "TypeScript Express API service name"
  value       = var.enable_typescript_api ? module.typescript_api[0].service_name : null
}
output "python_api_service_name" {
  description = "Python FastAPI service name"
  value       = var.enable_python_api ? module.python_api[0].service_name : null
}
output "go_api_service_name" {
  description = "Go Beego API service name"
  value       = var.enable_go_api ? module.go_api[0].service_name : null
}
output "rust_api_service_name" {
  description = "Rust Actix API service name"
  value       = var.enable_rust_api ? module.rust_api[0].service_name : null
}
output "scala_api_service_name" {
  description = "Scala Play API service name"
  value       = var.enable_scala_api ? module.scala_api[0].service_name : null
}
output "java_api_service_name" {
  description = "Java Play API service name"
  value       = var.enable_java_api ? module.java_api[0].service_name : null
}
output "r_api_service_name" {
  description = "R Plumber API service name"
  value       = var.enable_r_api ? module.r_api[0].service_name : null
}
output "julia_api_service_name" {
  description = "Julia Genie API service name"
  value       = var.enable_julia_api ? module.julia_api[0].service_name : null
}
output "dotnet_api_service_name" {
  description = ".NET ASP.NET Core API service name"
  value       = var.enable_dotnet_api ? module.dotnet_api[0].service_name : null
}
output "haskell_api_service_name" {
  description = "Haskell Servant API service name"
  value       = var.enable_haskell_api ? module.haskell_api[0].service_name : null
}
