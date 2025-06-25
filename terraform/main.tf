# Multi-Cloud Infrastructure Configuration
# This is the main entry point for deploying to one or more cloud providers

terraform {
  required_version = ">= 1.5.0"
}

# Provider Selection Variables
variable "enable_aws" {
  description = "Enable AWS provider deployment"
  type        = bool
  default     = false
}

variable "enable_gcp" {
  description = "Enable GCP provider deployment"
  type        = bool
  default     = false
}

variable "enable_azure" {
  description = "Enable Azure provider deployment"
  type        = bool
  default     = false
}

variable "enable_oci" {
  description = "Enable OCI provider deployment"
  type        = bool
  default     = false
}

variable "enable_cloudflare" {
  description = "Enable Cloudflare DNS provider deployment"
  type        = bool
  default     = false
}

variable "enable_ibm" {
  description = "Enable IBM Cloud provider deployment"
  type        = bool
  default     = false
}

# Common Variables
variable "project_name" {
  description = "Name of the project"
  type        = string
  default     = "Nexpo"
}

variable "environment" {
  description = "Environment name (dev, staging, prod)"
  type        = string
  default     = "dev"
}

# AWS Module
module "aws" {
  count  = var.enable_aws ? 1 : 0
  source = "./providers/aws/environments/${var.environment}"
  
  # Pass through common variables
  project_name = var.project_name
  environment  = var.environment
  
  # AWS-specific variables should be defined in aws.tfvars
}

# GCP Module
module "gcp" {
  count  = var.enable_gcp ? 1 : 0
  source = "./providers/gcp/environments/${var.environment}"
  
  # Pass through common variables
  project_name = var.project_name
  environment  = var.environment
  
  # GCP-specific variables should be defined in gcp.tfvars
}

# Azure Module
module "azure" {
  count  = var.enable_azure ? 1 : 0
  source = "./providers/azure/environments/${var.environment}"
  
  # Pass through common variables
  project_name = var.project_name
  environment  = var.environment
  
  # Azure-specific variables should be defined in azure.tfvars
}

# OCI Module
module "oci" {
  count  = var.enable_oci ? 1 : 0
  source = "./providers/oci/environments/${var.environment}"
  
  # Pass through common variables
  project_name = var.project_name
  environment  = var.environment
  
  # OCI-specific variables should be defined in oci.tfvars
}

# IBM Cloud Module
module "ibm" {
  count  = var.enable_ibm ? 1 : 0
  source = "./providers/ibm"
  
  # Pass through common variables
  project_name = var.project_name
  environment  = var.environment
  ibmcloud_api_key = var.ibmcloud_api_key
  region       = var.ibm_region
  zone         = var.ibm_zone
  
  # IBM-specific variables should be defined in ibm.tfvars
}

# Cloudflare Module
module "cloudflare" {
  count  = var.enable_cloudflare ? 1 : 0
  source = "./providers/cloudflare"
  
  # Pass through common variables
  cloudflare_api_token   = var.cloudflare_api_token
  cloudflare_email       = var.cloudflare_email
  cloudflare_account_id  = var.cloudflare_account_id
  
  # Cloudflare-specific variables should be defined in cloudflare.tfvars
  tags = {
    Environment = var.environment
    Project     = var.project_name
    ManagedBy   = "terraform"
  }
}

# Outputs
output "deployment_summary" {
  description = "Summary of deployed infrastructure"
  value = {
    aws        = var.enable_aws ? module.aws[0] : null
    gcp        = var.enable_gcp ? module.gcp[0] : null
    azure      = var.enable_azure ? module.azure[0] : null
    oci        = var.enable_oci ? module.oci[0] : null
    cloudflare = var.enable_cloudflare ? module.cloudflare[0] : null
  }
}
