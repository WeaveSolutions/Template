# OCI Development Environment

terraform {
  required_version = ">= 1.0"
  
  required_providers {
    oci = {
      source  = "oracle/oci"
      version = "~> 5.0"
    }
  }
  
  # Backend configuration for remote state
  # Uncomment and configure for production use
  # backend "s3" {
  #   bucket   = "terraform-state-bucket"
  #   key      = "oci/dev/terraform.tfstate"
  #   region   = "us-phoenix-1"
  #   endpoint = "https://objectstorage.us-phoenix-1.oraclecloud.com"
  #   
  #   skip_region_validation      = true
  #   skip_credentials_validation = true
  #   skip_metadata_api_check     = true
  #   force_path_style            = true
  # }
}

# Provider configuration
provider "oci" {
  tenancy_ocid     = var.tenancy_ocid
  user_ocid        = var.user_ocid
  fingerprint      = var.fingerprint
  private_key_path = var.private_key_path
  region           = var.region
}

# Local variables
locals {
  environment = "dev"
  tags = merge(
    var.common_tags,
    {
      environment = local.environment
      managed_by  = "terraform"
    }
  )
}

# Networking Module
module "networking" {
  source = "../../modules/networking"
  
  compartment_ocid        = var.compartment_ocid
  project_name           = var.project_name
  environment            = local.environment
  vcn_cidr_block         = var.vcn_cidr_block
  public_subnet_cidr     = var.public_subnet_cidr
  private_subnet_cidr    = var.private_subnet_cidr
  database_subnet_cidr   = var.database_subnet_cidr
  allowed_ssh_cidr_blocks = var.allowed_ssh_cidr_blocks
  tags                   = local.tags
}

# Container Registry Module
module "registry" {
  source = "../../modules/registry"
  
  compartment_ocid = var.compartment_ocid
  project_name    = var.project_name
  environment     = local.environment
  tags            = local.tags
}

# Compute Module (OKE)
module "compute" {
  source = "../../modules/compute"
  
  compartment_ocid     = var.compartment_ocid
  project_name         = var.project_name
  environment          = local.environment
  vcn_id              = module.networking.vcn_id
  public_subnet_id    = module.networking.public_subnet_id
  private_subnet_id   = module.networking.private_subnet_id
  web_nsg_id          = module.networking.web_nsg_id
  app_nsg_id          = module.networking.app_nsg_id
  kubernetes_version  = var.kubernetes_version
  node_pool_size      = var.node_pool_size
  node_shape          = var.node_shape
  node_ocpus          = var.node_ocpus
  node_memory_in_gbs  = var.node_memory_in_gbs
  registry_endpoint   = module.registry.registry_endpoint
  database_url        = module.database.connection_url
  auth0_domain        = var.auth0_domain
  auth0_client_id     = var.auth0_client_id
  auth0_client_secret = var.auth0_client_secret
  auth0_api_identifier = var.auth0_api_identifier
  tags                = local.tags
}

# Storage Module
module "storage" {
  source = "../../modules/storage"
  
  compartment_ocid    = var.compartment_ocid
  region              = var.region
  project_name        = var.project_name
  environment         = local.environment
  availability_domain = data.oci_identity_availability_domains.ads.availability_domains[0].name
  subnet_id           = module.networking.private_subnet_id
  subnet_cidr         = var.private_subnet_cidr
  nsg_ids             = [module.networking.app_nsg_id]
  
  buckets = {
    assets = {
      name          = "assets"
      public_access = "ObjectRead"
      versioning    = true
    }
    backups = {
      name          = "backups"
      public_access = "NoPublicAccess"
      versioning    = true
    }
    uploads = {
      name          = "uploads"
      public_access = "NoPublicAccess"
      versioning    = false
    }
  }
  
  allowed_origins = var.allowed_origins
  
  tags = local.tags
}

# Database Module
module "database" {
  source = "../../modules/database"
  
  compartment_ocid    = var.compartment_ocid
  project_name        = var.project_name
  environment         = local.environment
  subnet_id           = module.networking.database_subnet_id
  nsg_id              = module.networking.database_nsg_id
  db_name             = var.autonomous_db_name
  db_display_name     = var.autonomous_db_display_name
  db_version          = var.autonomous_db_version
  cpu_core_count      = var.autonomous_db_cpu_count
  data_storage_size_gb = var.autonomous_db_storage_gb
  license_model       = var.autonomous_db_license
  admin_password      = var.database_admin_password
  tags                = local.tags
}

# Load Balancer Module
module "load_balancer" {
  source = "../../modules/load-balancer"
  
  compartment_ocid          = var.compartment_ocid
  project_name             = var.project_name
  environment              = local.environment
  subnet_id                = module.networking.public_subnet_id
  nsg_id                   = module.networking.web_nsg_id
  shape                    = var.load_balancer_shape
  min_bandwidth_mbps       = var.load_balancer_min_bandwidth_mbps
  max_bandwidth_mbps       = var.load_balancer_max_bandwidth_mbps
  backend_set_health_check_url = "/health"
  backend_instances = [
    {
      ip_address = module.compute.api_gateway_private_ip
      port       = 3000
    }
  ]
  tags = local.tags
}

# Functions Module
module "functions" {
  source = "../../modules/functions"
  
  compartment_ocid  = var.compartment_ocid
  project_name      = var.project_name
  environment       = local.environment
  subnet_id         = module.networking.private_subnet_id
  nsg_id            = module.networking.app_nsg_id
  enable_functions  = var.enable_functions
  function_shape    = var.function_shape
  registry_endpoint = module.registry.registry_endpoint
  tags              = local.tags
}

# Security Module
module "security" {
  source = "../../modules/security"
  
  compartment_ocid = var.compartment_ocid
  project_name     = var.project_name
  environment      = local.environment
  enable_vault     = var.enable_vault
  enable_bastion   = var.enable_bastion
  subnet_id        = var.enable_bastion ? module.networking.public_subnet_id : null
  target_subnet_id = var.enable_bastion ? module.networking.private_subnet_id : null
  tags             = local.tags
}

# Monitoring Module
module "monitoring" {
  source = "../../modules/monitoring"
  
  compartment_ocid      = var.compartment_ocid
  project_name          = var.project_name
  environment           = local.environment
  enable_monitoring     = var.enable_monitoring
  notification_protocol = var.notification_protocol
  notification_endpoint = var.notification_endpoint
  tags                  = local.tags
}

# Output Auth0 configuration for application
resource "local_file" "auth0_config" {
  content = jsonencode({
    domain         = var.auth0_domain
    clientId       = var.auth0_client_id
    apiIdentifier  = var.auth0_api_identifier
    redirectUri    = "https://${module.load_balancer.load_balancer_ip}/api/auth/callback"
    logoutRedirect = "https://${module.load_balancer.load_balancer_ip}"
  })
  filename = "${path.module}/auth0-config.json"
}

# Create Kubernetes deployment files
resource "local_file" "k8s_deployments" {
  content = templatefile("${path.module}/../../templates/k8s-deployment.yaml.tpl", {
    project_name         = var.project_name
    environment          = local.environment
    registry_endpoint    = module.registry.registry_endpoint
    auth0_domain        = var.auth0_domain
    auth0_client_id     = var.auth0_client_id
    auth0_client_secret = var.auth0_client_secret
    auth0_api_identifier = var.auth0_api_identifier
    database_url        = module.database.connection_url
  })
  filename = "${path.module}/k8s-deployments.yaml"
}
