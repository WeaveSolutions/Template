# DigitalOcean Infrastructure - Production Environment

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
    # key    = "digitalocean/production/terraform.tfstate"
    # region = "us-east-1"
  }
}

# Configure the DigitalOcean Provider
provider "digitalocean" {
  token = var.digitalocean_access_token
}

# Local values
locals {
  environment = "production"
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
  environment = "Production"
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
        value = "production"
        scope = "RUN_AND_BUILD_TIME"
      }
      
      env {
        key   = "NEXT_PUBLIC_API_URL"
        value = var.domain_name != "" ? "https://${var.domain_name}/api" : "https://${local.project_prefix}.ondigitalocean.app/api"
        scope = "RUN_AND_BUILD_TIME"
      }
      
      env {
        key   = "DATABASE_URL"
        value = var.enable_managed_database ? digitalocean_database_cluster.main[0].uri : ""
        scope = "RUN_TIME"
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
        name = var.domain_name
      }
    }
  }
}

# Managed Database with High Availability
resource "digitalocean_database_cluster" "main" {
  count = var.enable_managed_database ? 1 : 0
  
  name       = "${local.project_prefix}-db"
  engine     = "pg"
  version    = "15"
  size       = var.database_cluster_size
  region     = var.digitalocean_region
  node_count = var.database_node_count
  
  maintenance_window {
    day  = "sunday"
    hour = "02:00:00"
  }
  
  backup_restore {
    database_name = replace(local.project_name, "-", "_")
  }
}

# Database
resource "digitalocean_database_db" "main" {
  count      = var.enable_managed_database ? 1 : 0
  cluster_id = digitalocean_database_cluster.main[0].id
  name       = replace(local.project_name, "-", "_")
}

# Read-only replica for analytics/reporting
resource "digitalocean_database_replica" "analytics" {
  count      = var.enable_managed_database && var.enable_read_replica ? 1 : 0
  cluster_id = digitalocean_database_cluster.main[0].id
  name       = "${local.project_prefix}-db-replica"
  region     = var.replica_region != "" ? var.replica_region : var.digitalocean_region
  size       = var.replica_size
}

# Spaces Bucket for static assets
resource "digitalocean_spaces_bucket" "main" {
  count = var.enable_spaces ? 1 : 0
  
  name   = "${replace(local.project_prefix, "_", "-")}-storage"
  region = var.digitalocean_region
  
  acl           = "public-read"
  force_destroy = false  # Production - never force destroy
  
  versioning {
    enabled = true
  }
  
  lifecycle_rule {
    id      = "transition_to_ia"
    enabled = true
    
    transition {
      days          = 30
      storage_class = "STANDARD_IA"
    }
    
    expiration {
      days = 365  # Keep objects for 1 year in production
    }
    
    noncurrent_version_expiration {
      days = 90  # Keep old versions for 90 days
    }
  }
}

# CDN for Spaces
resource "digitalocean_cdn" "main" {
  count = var.enable_spaces && var.enable_cdn ? 1 : 0
  
  origin        = digitalocean_spaces_bucket.main[0].bucket_domain_name
  custom_domain = var.cdn_custom_domain
  ttl           = 86400  # 24 hours cache for production
}

# Load Balancer
resource "digitalocean_loadbalancer" "main" {
  count = var.enable_load_balancer ? 1 : 0
  
  name     = "${local.project_prefix}-lb"
  region   = var.digitalocean_region
  vpc_uuid = var.enable_vpc ? digitalocean_vpc.main[0].id : null
  
  size_unit = var.load_balancer_size_unit
  
  forwarding_rule {
    entry_protocol   = "http"
    entry_port       = 80
    target_protocol  = "http"
    target_port      = 3000
    redirect_http_to_https = true
  }
  
  forwarding_rule {
    entry_protocol   = "https"
    entry_port       = 443
    target_protocol  = "http"
    target_port      = 3000
    
    certificate_name = var.ssl_certificate_name
    tls_passthrough  = false
  }
  
  healthcheck {
    protocol                 = "http"
    port                     = 3000
    path                     = "/api/health"
    check_interval_seconds   = 10
    response_timeout_seconds = 5
    healthy_threshold        = 3
    unhealthy_threshold      = 3
  }
  
  algorithm = "round_robin"
  
  droplet_tag = "web-server"
}

# Firewall for production security
resource "digitalocean_firewall" "main" {
  count = var.enable_firewall ? 1 : 0
  
  name = "${local.project_prefix}-firewall"
  
  # SSH access (restricted)
  inbound_rule {
    protocol         = "tcp"
    port_range       = "22"
    source_addresses = var.ssh_allowed_ips
  }
  
  # HTTP/HTTPS access
  inbound_rule {
    protocol         = "tcp"
    port_range       = "80"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }
  
  inbound_rule {
    protocol         = "tcp"
    port_range       = "443"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }
  
  # Outbound rules
  outbound_rule {
    protocol              = "tcp"
    port_range            = "1-65535"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }
  
  outbound_rule {
    protocol              = "udp"
    port_range            = "53"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }
  
  outbound_rule {
    protocol              = "icmp"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }
}

# Project resource assignment
resource "digitalocean_project_resources" "main" {
  project = digitalocean_project.main.id
  
  resources = concat(
    var.enable_app_platform ? [digitalocean_app.main[0].urn] : [],
    var.enable_managed_database ? [digitalocean_database_cluster.main[0].urn] : [],
    var.enable_managed_database && var.enable_read_replica ? [digitalocean_database_replica.analytics[0].urn] : [],
    var.enable_spaces ? [digitalocean_spaces_bucket.main[0].urn] : [],
    var.enable_spaces && var.enable_cdn ? [digitalocean_cdn.main[0].id] : [],
    var.enable_load_balancer ? [digitalocean_loadbalancer.main[0].urn] : []
  )
}
