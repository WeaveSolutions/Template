# Heroku Infrastructure - Development Environment

terraform {
  required_version = ">= 1.5"
  required_providers {
    heroku = {
      source  = "heroku/heroku"
      version = "~> 5.0"
    }
  }
  
  backend "s3" {
    # Backend configuration will be provided during terraform init
    # bucket = "your-terraform-state-bucket"
    # key    = "heroku/development/terraform.tfstate"
    # region = "us-east-1"
  }
}

# Configure the Heroku Provider
provider "heroku" {
  email   = var.heroku_email
  api_key = var.heroku_api_key
}

# Local values
locals {
  environment = "development"
  project_name = "next-solito-expo"
  app_prefix = "${local.project_name}-${local.environment}"
  
  common_config = {
    NODE_ENV = local.environment
    PROJECT_NAME = local.project_name
    ENVIRONMENT = local.environment
  }
}

# Main Web Application
resource "heroku_app" "web_app" {
  name   = "${local.app_prefix}-web"
  region = var.heroku_region
  stack  = var.heroku_stack
  
  config_vars = merge(local.common_config, {
    PORT = "3000"
    NEXT_PUBLIC_API_URL = "https://${local.app_prefix}-api.herokuapp.com"
    NEXT_PUBLIC_APP_ENV = local.environment
  })
  
  buildpacks = [
    "heroku/nodejs"
  ]
  
  organization {
    name = var.heroku_team
  }
}

# API Application
resource "heroku_app" "api_app" {
  count = var.enable_api_app ? 1 : 0
  
  name   = "${local.app_prefix}-api"
  region = var.heroku_region
  stack  = var.heroku_stack
  
  config_vars = merge(local.common_config, {
    PORT = "8000"
    CORS_ORIGIN = "https://${local.app_prefix}-web.herokuapp.com"
    API_VERSION = "v1"
  })
  
  buildpacks = [
    "heroku/nodejs"
  ]
  
  organization {
    name = var.heroku_team
  }
}

# PostgreSQL Database Add-on
resource "heroku_addon" "database" {
  app_id = heroku_app.web_app.id
  plan   = var.database_plan
}

# Attach database to API app if enabled
resource "heroku_addon_attachment" "api_database" {
  count = var.enable_api_app ? 1 : 0
  
  app_id   = heroku_app.api_app[0].id
  addon_id = heroku_addon.database.id
}

# Redis Add-on for caching
resource "heroku_addon" "redis" {
  count = var.enable_redis ? 1 : 0
  
  app_id = heroku_app.web_app.id
  plan   = var.redis_plan
}

# Attach Redis to API app if enabled
resource "heroku_addon_attachment" "api_redis" {
  count = var.enable_api_app && var.enable_redis ? 1 : 0
  
  app_id   = heroku_app.api_app[0].id
  addon_id = heroku_addon.redis[0].id
}

# Papertrail Add-on for logging (optional)
resource "heroku_addon" "logging" {
  count = var.enable_logging ? 1 : 0
  
  app_id = heroku_app.web_app.id
  plan   = var.logging_plan
}

# Domain configuration for web app
resource "heroku_domain" "web_domain" {
  count = var.custom_domain != "" ? 1 : 0
  
  app_id   = heroku_app.web_app.id
  hostname = "${local.environment}.${var.custom_domain}"
}

# Domain configuration for API app
resource "heroku_domain" "api_domain" {
  count = var.enable_api_app && var.custom_domain != "" ? 1 : 0
  
  app_id   = heroku_app.api_app[0].id
  hostname = "api-${local.environment}.${var.custom_domain}"
}

# Development-specific configuration vars for web app
resource "heroku_config" "web_development_config" {
  sensitive_vars = {
    DATABASE_URL = heroku_addon.database.config_vars["DATABASE_URL"]
    REDIS_URL    = var.enable_redis ? heroku_addon.redis[0].config_vars["REDIS_URL"] : ""
    JWT_SECRET   = var.jwt_secret
  }
  
  vars = {
    # Development-specific settings
    DEBUG = "true"
    LOG_LEVEL = "debug"
    ENABLE_HOT_RELOAD = "true"
    
    # Feature flags
    ENABLE_ANALYTICS     = "false"
    ENABLE_ERROR_TRACKING = "false"
    ENABLE_RATE_LIMITING = "false"
  }
}

# Development-specific configuration vars for API app
resource "heroku_config" "api_development_config" {
  count = var.enable_api_app ? 1 : 0
  
  sensitive_vars = {
    DATABASE_URL = heroku_addon.database.config_vars["DATABASE_URL"]
    REDIS_URL    = var.enable_redis ? heroku_addon.redis[0].config_vars["REDIS_URL"] : ""
    JWT_SECRET   = var.jwt_secret
    API_KEY      = var.api_key
  }
  
  vars = {
    # API-specific development settings
    DEBUG = "true"
    LOG_LEVEL = "debug"
    API_RATE_LIMIT = "1000"
    
    # Feature flags
    ENABLE_SWAGGER     = "true"
    ENABLE_CORS        = "true"
    ENABLE_REQUEST_LOGGING = "true"
  }
}

# Formation configuration for web app (hobby dyno for development)
resource "heroku_formation" "web" {
  app_id   = heroku_app.web_app.id
  type     = "web"
  quantity = 1
  size     = "hobby"
}

# Formation configuration for API app
resource "heroku_formation" "api" {
  count = var.enable_api_app ? 1 : 0
  
  app_id   = heroku_app.api_app[0].id
  type     = "web"
  quantity = 1
  size     = "hobby"
}
