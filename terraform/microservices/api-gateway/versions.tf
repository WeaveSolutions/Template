# ========================================
# Terraform Version Configuration
# ========================================
terraform {
  # Require Terraform 1.0.0 or newer
  required_version = ">= 1.0.0"

  # Define required providers and their versions
  required_providers {
    # AWS Provider - Latest stable 4.x
    aws = {
      source  = "hashicorp/aws"
      version = ">= 4.0.0, < 5.0.0"
    }
    
    # Google Cloud Provider - Latest stable 4.x
    google = {
      source  = "hashicorp/google"
      version = ">= 4.0.0, < 5.0.0"
    }
    
    # Azure Provider - Latest stable 3.x
    azurerm = {
      source  = "hashicorp/azurerm"
      version = ">= 3.0.0, < 4.0.0"
    }
    
    # Oracle Cloud Infrastructure (OCI) Provider - Latest stable 4.x
    oci = {
      source  = "oracle/oci"
      version = ">= 4.0.0, < 5.0.0"
    }
    
    # Random Provider - For generating random values
    random = {
      source  = "hashicorp/random"
      version = ">= 3.0.0, < 4.0.0"
    }
    
    # External Provider - For integrating with external tools
    external = {
      source  = "hashicorp/external"
      version = ">= 2.0.0, < 3.0.0"
    }
    
    # Template Provider - For template rendering (deprecated in TF 0.12+, but kept for compatibility)
    template = {
      source  = "hashicorp/template"
      version = ">= 2.0.0, < 3.0.0"
    }
    
    # Local Provider - For local operations
    local = {
      source  = "hashicorp/local"
      version = ">= 2.0.0, < 3.0.0"
    }
    
    # Null Provider - For explicit dependencies
    null = {
      source  = "hashicorp/null"
      version = ">= 3.0.0, < 4.0.0"
    }
    
    # Time Provider - For managing time-based resources
    time = {
      source  = "hashicorp/time"
      version = ">= 0.7.0, < 1.0.0"
    }
    
    # TLS Provider - For TLS certificate management
    tls = {
      source  = "hashicorp/tls"
      version = ">= 3.0.0, < 4.0.0"
    }
    
    # HTTP Provider - For making HTTP requests
    http = {
      source  = "hashicorp/http"
      version = ">= 2.0.0, < 4.0.0"
    }
    }
    
    # Null Provider
    null = {
      source  = "hashicorp/null"
      version = "~> 3.0"
    }
  }
  
  # Backend configuration will be provided by the root module
  # This allows the backend to be configured differently per environment
  # Example:
  # backend "s3" {}
  # backend "gcs" {}
  # backend "azurerm" {}
}
