# Common Variables for Multi-Cloud Infrastructure

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

variable "project_name" {
  description = "Name of the project (lowercase, no spaces)"
  type        = string
  validation {
    condition     = can(regex("^[a-z0-9-]+$", var.project_name))
    error_message = "Project name must be lowercase alphanumeric with hyphens only."
  }
}

variable "environment" {
  description = "Environment name (dev, staging, prod)"
  type        = string
  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be dev, staging, or prod."
  }
}

# Cloudflare Variables
variable "cloudflare_api_token" {
  description = "Cloudflare API token with sufficient permissions to manage resources"
  type        = string
  default     = ""
  sensitive   = true
}

variable "cloudflare_email" {
  description = "Email address associated with the Cloudflare account"
  type        = string
  default     = ""
}

variable "cloudflare_account_id" {
  description = "Cloudflare account ID"
  type        = string
  default     = ""
}

# IBM Cloud Variables
variable "ibmcloud_api_key" {
  description = "IBM Cloud API key for authentication"
  type        = string
  default     = ""
  sensitive   = true
}

variable "ibm_region" {
  description = "IBM Cloud region to deploy resources"
  type        = string
  default     = "us-south"
}

variable "ibm_zone" {
  description = "Availability zone within the IBM Cloud region"
  type        = string
  default     = "us-south-1"
}
