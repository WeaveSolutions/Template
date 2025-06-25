# Staging Environment Configuration

terraform {
  backend "s3" {
    # Configure your backend
    # bucket = "your-terraform-state-bucket"
    # key    = "Nexpo/staging/terraform.tfstate"
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
  vpc_cidr         = "10.1.0.0/16"
  azs              = ["us-east-1a", "us-east-1b", "us-east-1c"]
  public_subnets   = ["10.1.1.0/24", "10.1.2.0/24", "10.1.3.0/24"]
  private_subnets  = ["10.1.11.0/24", "10.1.12.0/24", "10.1.13.0/24"]
  database_subnets = ["10.1.21.0/24", "10.1.22.0/24", "10.1.23.0/24"]
  
  enable_nat_gateway = true
  single_nat_gateway = false # Multiple NAT gateways for HA
  
  common_tags = local.common_tags
}

# Storage Module
module "storage" {
  source = "../../modules/storage"

  project_name          = var.project_name
  environment           = var.environment
  cors_allowed_origins  = var.cors_allowed_origins
  backup_retention_days = 60
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
  
  # Staging environment settings
  db_instance_class         = "db.t3.small"
  db_allocated_storage      = 100
  backup_retention_period   = 7
  enable_deletion_protection = true
  enable_performance_insights = true
  
  # Read replica for testing
  create_read_replica = true
  read_replica_count = 1
  
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
  cpu                   = "0.5 vCPU"
  memory                = "1 GB"
  
  # Auto-scaling configuration
  enable_auto_scaling = true
  min_instances      = 2
  max_instances      = 10
  
  subnet_ids            = module.networking.private_subnet_ids
  security_group_ids    = [module.networking.app_security_group_id]
  
  environment_variables = {
    NODE_ENV                    = "staging"
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
  
  price_class = "PriceClass_200" # Use North America, Europe, Asia, Middle East, and Africa
  
  # WAF protection
  enable_waf = true
  
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
  log_retention_days         = 30
  
  # Enhanced monitoring for staging
  enable_detailed_monitoring = true
  enable_xray_tracing       = true
  
  app_runner_service_name    = module.compute.app_runner_service_name
  db_instance_id             = module.database.db_instance_id
  cloudfront_distribution_id = module.cdn.cloudfront_distribution_id
  synthetics_bucket_name     = module.storage.logs_bucket_id
  app_url                    = var.custom_domain != "" ? "https://${var.custom_domain}" : "https://${module.cdn.cloudfront_distribution_domain_name}"
  
  common_tags = local.common_tags
}

# Security Module (Additional for staging)
module "security" {
  source = "../../modules/security"

  project_name = var.project_name
  environment  = var.environment
  
  # Enable security scanning
  enable_inspector = true
  enable_guardduty = true
  
  common_tags = local.common_tags
}
