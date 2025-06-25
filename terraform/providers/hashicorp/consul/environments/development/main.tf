# HashiCorp Consul Infrastructure - Development Environment

terraform {
  required_version = ">= 1.5"
  required_providers {
    consul = {
      source  = "hashicorp/consul"
      version = "~> 2.20"
    }
  }
  
  backend "s3" {
    # Backend configuration will be provided during terraform init
    # bucket = "your-terraform-state-bucket"
    # key    = "consul/development/terraform.tfstate"
    # region = "us-east-1"
  }
}

# Configure the Consul Provider
provider "consul" {
  address    = var.consul_address
  token      = var.consul_token
  datacenter = var.consul_datacenter
}

# Local values
locals {
  environment = "development"
  project_name = "next-solito-expo"
  service_prefix = "${local.project_name}-${local.environment}"
  
  common_tags = {
    environment = local.environment
    project     = local.project_name
    managed_by  = "terraform"
  }
}

# Register Next.js Application Service
resource "consul_service" "nextjs_app" {
  count = var.enable_nextjs_service ? 1 : 0
  
  name = "${local.service_prefix}-nextjs"
  node = var.consul_node_name
  port = var.nextjs_port
  
  tags = [
    "frontend",
    "nextjs",
    local.environment,
    "urlprefix-/",
    "version=${var.nextjs_version}"
  ]
  
  meta = merge(local.common_tags, {
    service_type = "web-application"
    framework    = "nextjs"
    version      = var.nextjs_version
  })
  
  check {
    check_id = "nextjs-health"
    name     = "Next.js Health Check"
    http     = "http://${var.nextjs_host}:${var.nextjs_port}/api/health"
    interval = "30s"
    timeout  = "10s"
  }
  
  check {
    check_id = "nextjs-ready"
    name     = "Next.js Readiness Check"
    http     = "http://${var.nextjs_host}:${var.nextjs_port}/api/ready"
    interval = "10s"
    timeout  = "5s"
  }
}

# Register API Service
resource "consul_service" "api_service" {
  count = var.enable_api_service ? 1 : 0
  
  name = "${local.service_prefix}-api"
  node = var.consul_node_name
  port = var.api_port
  
  tags = [
    "api",
    "backend",
    local.environment,
    "urlprefix-/api",
    "version=${var.api_version}"
  ]
  
  meta = merge(local.common_tags, {
    service_type = "api-service"
    version      = var.api_version
    protocol     = "http"
  })
  
  check {
    check_id = "api-health"
    name     = "API Health Check"
    http     = "http://${var.api_host}:${var.api_port}/health"
    interval = "30s"
    timeout  = "10s"
  }
  
  check {
    check_id = "api-ready"
    name     = "API Readiness Check"
    http     = "http://${var.api_host}:${var.api_port}/ready"
    interval = "10s"
    timeout  = "5s"
  }
}

# Register Database Service
resource "consul_service" "database" {
  count = var.enable_database_service ? 1 : 0
  
  name = "${local.service_prefix}-database"
  node = var.consul_node_name
  port = var.database_port
  
  tags = [
    "database",
    "postgresql",
    local.environment
  ]
  
  meta = merge(local.common_tags, {
    service_type = "database"
    engine       = "postgresql"
    version      = var.database_version
  })
  
  check {
    check_id = "db-tcp"
    name     = "Database TCP Check"
    tcp      = "${var.database_host}:${var.database_port}"
    interval = "30s"
    timeout  = "10s"
  }
}

# Register Redis Service
resource "consul_service" "redis" {
  count = var.enable_redis_service ? 1 : 0
  
  name = "${local.service_prefix}-redis"
  node = var.consul_node_name
  port = var.redis_port
  
  tags = [
    "cache",
    "redis",
    local.environment
  ]
  
  meta = merge(local.common_tags, {
    service_type = "cache"
    engine       = "redis"
    version      = var.redis_version
  })
  
  check {
    check_id = "redis-tcp"
    name     = "Redis TCP Check"
    tcp      = "${var.redis_host}:${var.redis_port}"
    interval = "30s"
    timeout  = "5s"
  }
}

# Application Configuration in KV Store
resource "consul_keys" "app_config" {
  datacenter = var.consul_datacenter
  
  key {
    path  = "config/${local.service_prefix}/database/host"
    value = var.database_host
  }
  
  key {
    path  = "config/${local.service_prefix}/database/port"
    value = tostring(var.database_port)
  }
  
  key {
    path  = "config/${local.service_prefix}/database/name"
    value = var.database_name
  }
  
  key {
    path  = "config/${local.service_prefix}/redis/host"
    value = var.redis_host
  }
  
  key {
    path  = "config/${local.service_prefix}/redis/port"
    value = tostring(var.redis_port)
  }
  
  key {
    path  = "config/${local.service_prefix}/app/log_level"
    value = var.log_level
  }
  
  key {
    path  = "config/${local.service_prefix}/app/environment"
    value = local.environment
  }
  
  key {
    path  = "config/${local.service_prefix}/app/debug"
    value = "true"
  }
}

# Service Mesh Configuration (if enabled)
resource "consul_config_entry" "proxy_defaults" {
  count = var.enable_service_mesh ? 1 : 0
  
  kind = "proxy-defaults"
  name = "global"
  
  config_json = jsonencode({
    Config = {
      protocol = "http"
    }
  })
}

# Service Intentions for API to Database
resource "consul_config_entry" "api_to_database" {
  count = var.enable_service_mesh && var.enable_api_service && var.enable_database_service ? 1 : 0
  
  kind = "service-intentions"
  name = consul_service.database[0].name
  
  config_json = jsonencode({
    Sources = [{
      Name   = consul_service.api_service[0].name
      Action = "allow"
    }]
  })
}

# Service Intentions for API to Redis
resource "consul_config_entry" "api_to_redis" {
  count = var.enable_service_mesh && var.enable_api_service && var.enable_redis_service ? 1 : 0
  
  kind = "service-intentions"
  name = consul_service.redis[0].name
  
  config_json = jsonencode({
    Sources = [{
      Name   = consul_service.api_service[0].name
      Action = "allow"
    }]
  })
}

# Service Intentions for Next.js to API
resource "consul_config_entry" "nextjs_to_api" {
  count = var.enable_service_mesh && var.enable_nextjs_service && var.enable_api_service ? 1 : 0
  
  kind = "service-intentions"
  name = consul_service.api_service[0].name
  
  config_json = jsonencode({
    Sources = [{
      Name   = consul_service.nextjs_app[0].name
      Action = "allow"
    }]
  })
}
