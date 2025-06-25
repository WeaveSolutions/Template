# HashiCorp Nomad Infrastructure - Development Environment

terraform {
  required_version = ">= 1.5"
  required_providers {
    nomad = {
      source  = "hashicorp/nomad"
      version = "~> 2.0"
    }
  }
  
  backend "s3" {
    # Backend configuration will be provided during terraform init
    # bucket = "your-terraform-state-bucket"
    # key    = "nomad/development/terraform.tfstate"
    # region = "us-east-1"
  }
}

# Configure the Nomad Provider
provider "nomad" {
  address = var.nomad_address
  token   = var.nomad_token
  region  = var.nomad_region
}

# Local values
locals {
  environment = "development"
  project_name = "next-solito-expo"
  job_prefix = "${local.project_name}-${local.environment}"
  
  # Development-specific configuration
  instance_count = 1
  cpu_limit     = 256
  memory_limit  = 512
  
  common_meta = {
    environment = local.environment
    project     = local.project_name
    managed_by  = "terraform"
  }
}

# Next.js Application Job
resource "nomad_job" "nextjs_app" {
  count = var.enable_nextjs_app ? 1 : 0
  
  jobspec = templatefile("${path.module}/../../jobs/nextjs-app.nomad.tpl", {
    job_name       = "${local.job_prefix}-nextjs"
    environment    = local.environment
    instance_count = local.instance_count
    cpu_limit      = local.cpu_limit
    memory_limit   = local.memory_limit
    image_tag      = var.nextjs_image_tag
    port           = var.nextjs_port
    api_url        = var.api_url
    database_url   = var.database_url
    meta           = local.common_meta
  })
  
  # Automatically purge job when destroyed
  purge_on_destroy = true
}

# API Service Job
resource "nomad_job" "api_service" {
  count = var.enable_api_service ? 1 : 0
  
  jobspec = templatefile("${path.module}/../../jobs/api-service.nomad.tpl", {
    job_name       = "${local.job_prefix}-api"
    environment    = local.environment
    instance_count = local.instance_count
    cpu_limit      = local.cpu_limit
    memory_limit   = local.memory_limit
    image_tag      = var.api_image_tag
    port           = var.api_port
    database_url   = var.database_url
    redis_url      = var.redis_url
    meta           = local.common_meta
  })
  
  purge_on_destroy = true
}

# Background Worker Job
resource "nomad_job" "background_worker" {
  count = var.enable_background_worker ? 1 : 0
  
  jobspec = templatefile("${path.module}/../../jobs/background-worker.nomad.tpl", {
    job_name       = "${local.job_prefix}-worker"
    environment    = local.environment
    instance_count = 1
    cpu_limit      = 128
    memory_limit   = 256
    image_tag      = var.worker_image_tag
    database_url   = var.database_url
    redis_url      = var.redis_url
    meta           = local.common_meta
  })
  
  purge_on_destroy = true
}

# Database Backup Job (Periodic)
resource "nomad_job" "database_backup" {
  count = var.enable_database_backup ? 1 : 0
  
  jobspec = templatefile("${path.module}/../../jobs/database-backup.nomad.tpl", {
    job_name      = "${local.job_prefix}-db-backup"
    environment   = local.environment
    cpu_limit     = 128
    memory_limit  = 256
    database_url  = var.database_url
    backup_bucket = var.backup_bucket
    cron_schedule = var.backup_schedule
    meta          = local.common_meta
  })
  
  purge_on_destroy = true
}

# Namespace for development environment
resource "nomad_namespace" "development" {
  count = var.create_namespace ? 1 : 0
  
  name        = local.environment
  description = "Namespace for ${local.project_name} ${local.environment} environment"
  
  meta = local.common_meta
  
  quota = var.enable_quota ? var.namespace_quota : null
}
