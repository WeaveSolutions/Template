# DigitalOcean Infrastructure - Development Environment

terraform {
  required_version = ">= 1.5"
  required_providers {
    digitalocean = {
      source  = "digitalocean/digitalocean"
      version = "~> 2.34"
    }
  }
  
  backend "s3" {
    # Backend configuration will be provided during terraform init
    # bucket = "your-terraform-state-bucket"
    # key    = "digitalocean/development/terraform.tfstate"
    # region = "us-east-1"
  }
}

# Configure the DigitalOcean Provider
provider "digitalocean" {
  token = var.digitalocean_access_token
}

# Local values
locals {
  environment = "development"
  project_name = "next-solito-expo"
  project_prefix = "${local.project_name}-${local.environment}"
  
  common_tags = {
    "Environment" = local.environment
    "Project"     = local.project_name
    "ManagedBy"   = "terraform"
  }
}

# DigitalOcean Project
resource "digitalocean_project" "main" {
  name        = local.project_prefix
  description = "Infrastructure for ${local.project_name} ${local.environment} environment"
  purpose     = "Web Application"
  environment = "Development"
}

# VPC
resource "digitalocean_vpc" "main" {
  count = var.enable_vpc ? 1 : 0
  
  name     = "${local.project_prefix}-vpc"
  region   = var.digitalocean_region
  ip_range = var.digitalocean_vpc_cidr
}

# App Platform Application
resource "digitalocean_app" "main" {
  count = var.enable_app_platform ? 1 : 0
  
  spec {
    name   = local.project_prefix
    region = var.digitalocean_region
    
    # Next.js Service
    service {
      name               = "nextjs-app"
      instance_count     = var.app_platform_instance_count
      instance_size_slug = var.app_platform_instance_size
      
      git {
        repo_clone_url = var.app_platform_repo_url
        branch         = var.app_platform_branch
      }
      
      build_command = "pnpm install && pnpm build"
      run_command   = "pnpm start"
      
      http_port = 3000
      
      env {
        key   = "NODE_ENV"
        value = "development"
        scope = "RUN_AND_BUILD_TIME"
      }
      
      env {
        key   = "NEXT_PUBLIC_API_URL"
        value = "https://${local.project_prefix}.ondigitalocean.app/api"
        scope = "RUN_AND_BUILD_TIME"
      }
      
      health_check {
        http_path                = "/api/health"
        initial_delay_seconds    = 60
        period_seconds          = 10
        timeout_seconds         = 5
        success_threshold       = 1
        failure_threshold       = 3
      }
    }
    
    # Domain configuration
    dynamic "domain" {
      for_each = var.domain_name != "" ? [1] : []
      content {
        name = var.subdomain != "" ? "${var.subdomain}.${var.domain_name}" : var.domain_name
      }
    }
  }
}

# Managed Database
resource "digitalocean_database_cluster" "main" {
  count = var.enable_managed_database ? 1 : 0
  
  name       = "${local.project_prefix}-db"
  engine     = "pg"
  version    = "15"
  size       = var.database_cluster_size
  region     = var.digitalocean_region
  node_count = 1
}

# Database
resource "digitalocean_database_db" "main" {
  count      = var.enable_managed_database ? 1 : 0
  cluster_id = digitalocean_database_cluster.main[0].id
  name       = replace(local.project_name, "-", "_")
}

# Spaces Bucket for static assets
resource "digitalocean_spaces_bucket" "main" {
  count = var.enable_spaces ? 1 : 0
  
  name   = "${replace(local.project_prefix, "_", "-")}-storage"
  region = var.digitalocean_region
  
  acl           = "public-read"
  force_destroy = true  # Development environment
  
  versioning {
    enabled = false  # Not needed for development
  }
}

# Project resource assignment
resource "digitalocean_project_resources" "main" {
  project = digitalocean_project.main.id
  
  resources = concat(
    var.enable_app_platform ? [digitalocean_app.main[0].urn] : [],
    var.enable_managed_database ? [digitalocean_database_cluster.main[0].urn] : [],
    var.enable_spaces ? [digitalocean_spaces_bucket.main[0].urn] : []
  )
}
