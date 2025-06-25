# Development Environment Configuration

terraform {
  backend "s3" {
    # Configure your backend
    # bucket = "your-terraform-state-bucket"
    # key    = "Nexpo/dev/terraform.tfstate"
    # region = "us-east-1"
  }
}

# Common tags for all resources
locals {
  common_tags = {
    Project     = var.project_name
    Environment = var.environment
    ManagedBy   = "Terraform"
    CreatedAt   = timestamp()
  }
}

# Networking Module
module "networking" {
  source = "../../modules/networking"

  project_name     = var.project_name
  environment      = var.environment
  vpc_cidr         = "10.0.0.0/16"
  azs              = ["us-east-1a", "us-east-1b"]
  public_subnets   = ["10.0.1.0/24", "10.0.2.0/24"]
  private_subnets  = ["10.0.11.0/24", "10.0.12.0/24"]
  database_subnets = ["10.0.21.0/24", "10.0.22.0/24"]
  
  enable_nat_gateway = false # Save costs in dev
  single_nat_gateway = true
  
  common_tags = local.common_tags
}

# Storage Module
module "storage" {
  source = "../../modules/storage"

  project_name          = var.project_name
  environment           = var.environment
  cors_allowed_origins  = ["http://localhost:3000", "http://localhost:8081"]
  backup_retention_days = 30
  cloudfront_oai_arn    = module.cdn.cloudfront_oai_arn
  
  common_tags = local.common_tags
}

# Database Module
module "database" {
  source = "../../modules/database"

  project_name        = var.project_name
  environment         = var.environment
  database_subnet_ids = module.networking.database_subnet_ids
  security_group_ids  = [module.networking.database_security_group_id]
  
  # Dev environment settings
  db_instance_class         = "db.t3.micro"
  db_allocated_storage      = 20
  backup_retention_period   = 3
  enable_deletion_protection = false
  enable_performance_insights = false
  
  alarm_sns_topic_arns = [module.monitoring.alarm_topic_arn]
  
  common_tags = local.common_tags
}

# Compute Module
module "compute" {
  source = "../../modules/compute"

  project_name          = var.project_name
  environment           = var.environment
  ecr_repository_name   = "${var.project_name}-nextjs"
  app_port              = 3000
  cpu                   = "0.25 vCPU"
  memory                = "0.5 GB"
  
  subnet_ids            = module.networking.private_subnet_ids
  security_group_ids    = [module.networking.app_security_group_id]
  
  environment_variables = {
    NODE_ENV                    = "development"
    DATABASE_URL                = module.database.database_url
    NEXT_PUBLIC_SUPABASE_URL    = var.supabase_url
    NEXT_PUBLIC_SUPABASE_ANON_KEY = var.supabase_anon_key
    AUTH0_SECRET                = var.auth0_secret
    AUTH0_BASE_URL              = var.auth0_base_url
    AUTH0_ISSUER_BASE_URL       = var.auth0_issuer_base_url
    AUTH0_CLIENT_ID             = var.auth0_client_id
    AUTH0_CLIENT_SECRET         = var.auth0_client_secret
  }
  
  common_tags = local.common_tags
}

# CDN Module
module "cdn" {
  source = "../../modules/cdn"

  project_name                       = var.project_name
  environment                        = var.environment
  assets_bucket_id                   = module.storage.assets_bucket_id
  assets_bucket_regional_domain_name = module.storage.assets_bucket_domain_name
  app_domain                         = module.compute.app_runner_service_url
  logs_bucket_domain_name            = module.storage.logs_bucket_id
  
  price_class = "PriceClass_100" # Use only North America and Europe in dev
  
  alarm_sns_topic_arns = [module.monitoring.alarm_topic_arn]
  
  common_tags = local.common_tags
}

# Monitoring Module
module "monitoring" {
  source = "../../modules/monitoring"

  project_name               = var.project_name
  environment                = var.environment
  aws_region                 = var.aws_region
  alarm_email_endpoints      = var.alarm_email_endpoints
  log_retention_days         = 7 # Shorter retention in dev
  
  app_runner_service_name    = module.compute.app_runner_service_name
  db_instance_id             = module.database.db_instance_id
  cloudfront_distribution_id = module.cdn.cloudfront_distribution_id
  synthetics_bucket_name     = module.storage.logs_bucket_id
  app_url                    = "https://${module.cdn.cloudfront_distribution_domain_name}"
  
  common_tags = local.common_tags
}
