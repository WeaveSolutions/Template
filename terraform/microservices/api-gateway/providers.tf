# ========================================
# Terraform Provider Configuration
# ========================================

terraform {
  required_version = ">= 1.0.0"

  required_providers {
    # AWS Provider
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.0"
    }

    # GCP Provider
    google = {
      source  = "hashicorp/google"
      version = "~> 4.0"
    }
    # Azure Provider
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.0"
    }
    # OCI Provider
    oci = {
      source  = "oracle/oci"
      version = "~> 4.0"
    }
  }
}

# ========================================
# AWS Provider Configuration
# ========================================
provider "aws" {
  region = var.aws_region
  
  # Only create AWS provider if AWS is enabled
  count = var.enable_aws ? 1 : 0
  
  # Optional: Use AWS profile from environment variables or variables
  profile = var.aws_profile != "default" ? var.aws_profile : null
  
  # Optional: Assume role configuration for cross-account access
  dynamic "assume_role" {
    for_each = var.aws_assume_role_arn != "" ? [1] : []
    content {
      role_arn = var.aws_assume_role_arn
    }
  }
  
  # Default tags for all resources
  default_tags {
    tags = {
      Environment = var.environment
      Project     = var.project_name
      ManagedBy   = "Terraform"
      Component   = "api-gateway"
    }
  }
}

# ========================================
# GCP Provider Configuration
# ========================================
provider "google" {
  project = var.gcp_project_id
  region  = var.gcp_region
  
  # Only create GCP provider if GCP is enabled
  count = var.enable_gcp ? 1 : 0
  
  # Optional: Use service account credentials from environment variables or file
  # credentials = file(var.gcp_credentials_file)
  
  # Optional: Set the billing project for quota checks
  # billing_project = var.gcp_billing_project
  
  # Optional: Add user agent for tracking
  user_project_override = true
}

# ========================================
# Azure Provider Configuration
# ========================================
provider "azurerm" {
  features {}
  
  # Only create Azure provider if Azure is enabled
  count = var.enable_azure ? 1 : 0
  
  # Optional: Use service principal authentication
  subscription_id = var.azure_subscription_id != "" ? var.azure_subscription_id : null
  client_id       = var.azure_client_id != "" ? var.azure_client_id : null
  client_secret   = var.azure_client_secret != "" ? var.azure_client_secret : null
  tenant_id       = var.azure_tenant_id != "" ? var.azure_tenant_id : null
  
  # Optional: Skip provider registration
  skip_provider_registration = true
  
  # Optional: Disable correlation request ID in logging
  disable_correlation_request_id = false
  
  # Optional: Set the Azure environment (public, usgovernment, german, china)
  environment = "public"
}

# ========================================
# OCI Provider Configuration
# ========================================
provider "oci" {
  # Only create OCI provider if OCI is enabled
  count = var.enable_oci ? 1 : 0
  
  # Required authentication parameters
  tenancy_ocid     = var.oci_tenancy_ocid
  user_ocid        = var.oci_user_ocid
  fingerprint      = var.oci_fingerprint
  private_key_path = var.oci_private_key_path
  region           = var.oci_region
  
  # Optional: Use instance principal authentication
  # auth = "InstancePrincipal"
  
  # Optional: Retry settings
  retry_duration_seconds = 60
  
  # Optional: Disable auto-retries for non-idempotent operations
  retry_disabled = false
  
  # Optional: Configure HTTP client
  client_connection_timeout_millis = 30000
  client_read_timeout_millis      = 60000
}

# OCI Provider
provider "oci" {
  version              = "~> 4.0"
  tenancy_ocid         = var.oci_tenancy_ocid
  user_ocid            = var.oci_user_ocid
  fingerprint          = var.oci_fingerprint
  private_key_path     = var.oci_private_key_path
  private_key_password = var.oci_private_key_password
  region               = var.oci_region
  
  # Only create OCI provider if OCI is enabled
  count = var.enable_oci ? 1 : 0
  
  # Optional: Use instance principal authentication
  # auth = "InstancePrincipal"
  
  # Optional: Disable auto-retries for faster failure
  # retry_duration_seconds = 0
}

# Random provider for generating unique names and passwords
provider "random" {
  version = ">= 3.0"
}

# External provider for getting external IP
provider "external" {
  version = "~> 2.0"
}

# Template provider for generating configurations
provider "template" {
  version = "~> 2.0"
}

# Local provider for local file operations
provider "local" {
  version = "~> 2.0"
}

# Null provider for resource dependencies
provider "null" {
  version = "~> 3.0"
}
