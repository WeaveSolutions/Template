# ========================================
# Terraform Backend Configuration
# ========================================

# This file configures the remote backend for storing Terraform state
# Uncomment and configure the appropriate backend for your use case

# Example: AWS S3 Backend (Recommended for AWS environments)
/*
terraform {
  backend "s3" {
    bucket         = "your-terraform-state-bucket"
    key            = "api-gateway/terraform.tfstate"
    region         = "us-west-2"
    dynamodb_table = "terraform-locks"
    encrypt        = true
  }
}
*/

# Example: GCS Backend (Recommended for GCP environments)
/*
terraform {
  backend "gcs" {
    bucket = "your-terraform-state-bucket"
    prefix = "api-gateway"
  }
}
*/

# Example: Azure Storage Backend (Recommended for Azure environments)
/*
terraform {
  backend "azurerm" {
    resource_group_name  = "terraform-state-rg"
    storage_account_name = "yourstorageaccount"
    container_name       = "tfstate"
    key                  = "api-gateway.terraform.tfstate"
  }
}
*/

# Example: OCI Object Storage Backend (Recommended for OCI environments)
/*
terraform {
  backend "http" {
    address        = "https://objectstorage.{region}.oraclecloud.com/n/{namespace}/b/{bucket}/o/terraform.tfstate"
    update_method  = "PUT"
    lock_address   = "https://objectstorage.{region}.oraclecloud.com/n/{namespace}/b/{bucket}/o/terraform.tfstate.lock"
    unlock_address = "https://objectstorage.{region}.oraclecloud.com/n/{namespace}/b/{bucket}/o/terraform.tfstate.lock"
    lock_method    = "PUT"
    unlock_method  = "DELETE"
    username       = "${var.tenancy_ocid}/${var.user_ocid}/${var.fingerprint}"
    password       = var.private_key_password
  }
}
*/

# Local backend (for development only, not recommended for production)
/*
terraform {
  backend "local" {
    path = "terraform.tfstate"
  }
}
*/

# ========================================
# Remote State Data Sources
# ========================================

# Example: Importing remote state from another Terraform configuration
/*
data "terraform_remote_state" "vpc" {
  backend = "s3"
  config = {
    bucket = "your-terraform-state-bucket"
    key    = "vpc/terraform.tfstate"
    region = "us-west-2"
  }
}
*/

# ========================================
# Backend Configuration Notes
# ========================================
# 1. Always enable state locking to prevent concurrent operations
# 2. Use workspaces for environment separation (dev/staging/prod)
# 3. Enable versioning on your state bucket
# 4. Encrypt state at rest and in transit
# 5. Use IAM roles and policies to restrict access to the state
# 6. Consider using a remote backend for team collaboration
# 7. Backup your state files regularly
# 8. Never commit .tfstate files to version control
# ========================================
