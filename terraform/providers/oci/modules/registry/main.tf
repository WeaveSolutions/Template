# OCI Container Registry Module

# Get tenancy namespace
data "oci_objectstorage_namespace" "ns" {
  compartment_id = var.compartment_ocid
}

# Container repository for Next.js app
resource "oci_artifacts_container_repository" "next_app" {
  compartment_id = var.compartment_ocid
  display_name   = "${var.project_name}/next"
  
  is_public = false
  
  readme {
    content = "Next.js application for ${var.project_name}"
    format  = "text/plain"
  }
  
  freeform_tags = var.tags
}

# Container repository for API Gateway
resource "oci_artifacts_container_repository" "api_gateway" {
  compartment_id = var.compartment_ocid
  display_name   = "${var.project_name}/api-gateway"
  
  is_public = false
  
  readme {
    content = "API Gateway for ${var.project_name}"
    format  = "text/plain"
  }
  
  freeform_tags = var.tags
}

# Container repository for Auth Service
resource "oci_artifacts_container_repository" "auth_service" {
  compartment_id = var.compartment_ocid
  display_name   = "${var.project_name}/auth"
  
  is_public = false
  
  readme {
    content = "Auth Service for ${var.project_name}"
    format  = "text/plain"
  }
  
  freeform_tags = var.tags
}

# Container repository for User Service
resource "oci_artifacts_container_repository" "user_service" {
  compartment_id = var.compartment_ocid
  display_name   = "${var.project_name}/user"
  
  is_public = false
  
  readme {
    content = "User Service for ${var.project_name}"
    format  = "text/plain"
  }
  
  freeform_tags = var.tags
}

# Container repository for Notification Service
resource "oci_artifacts_container_repository" "notification_service" {
  compartment_id = var.compartment_ocid
  display_name   = "${var.project_name}/notifications"
  
  is_public = false
  
  readme {
    content = "Notification Service for ${var.project_name}"
    format  = "text/plain"
  }
  
  freeform_tags = var.tags
}

# Repository policies for retention
resource "oci_artifacts_container_repository" "functions" {
  for_each = var.enable_functions ? toset(["token-exchange", "webhook-handler", "data-processor"]) : []
  
  compartment_id = var.compartment_ocid
  display_name   = "${var.project_name}/functions/${each.key}"
  
  is_public = false
  
  readme {
    content = "Function: ${each.key} for ${var.project_name}"
    format  = "text/plain"
  }
  
  freeform_tags = var.tags
}

# IAM Policy for Registry Access
resource "oci_identity_policy" "registry_policy" {
  compartment_id = var.compartment_ocid
  name           = "${var.project_name}-${var.environment}-registry-policy"
  description    = "Policy for container registry access"
  
  statements = [
    # Allow node pool to pull images
    "Allow dynamic-group ${var.project_name}-${var.environment}-nodes to read repos in compartment id ${var.compartment_ocid}",
    
    # Allow developers to push images
    "Allow group ${var.project_name}-developers to manage repos in compartment id ${var.compartment_ocid}",
    
    # Allow CI/CD to push images
    "Allow group ${var.project_name}-cicd to manage repos in compartment id ${var.compartment_ocid} where any {request.permission='REPOSITORY_CREATE', request.permission='REPOSITORY_UPDATE', request.permission='REPOSITORY_DELETE', request.permission='REPOSITORY_READ'}",
  ]
  
  freeform_tags = var.tags
}

# Create dynamic group for nodes
resource "oci_identity_dynamic_group" "nodes" {
  compartment_id = var.tenancy_ocid  # Dynamic groups are created at tenancy level
  name           = "${var.project_name}-${var.environment}-nodes"
  description    = "Dynamic group for OKE nodes"
  
  matching_rule = "ALL {instance.compartment.id = '${var.compartment_ocid}', tag.oke-cluster.value = '${var.project_name}-${var.environment}'}"
  
  freeform_tags = var.tags
}

# Create groups for developers and CI/CD (if they don't exist)
# Note: In production, these would typically be managed separately
resource "oci_identity_group" "developers" {
  compartment_id = var.tenancy_ocid
  name           = "${var.project_name}-developers"
  description    = "Developers group for ${var.project_name}"
  
  freeform_tags = var.tags
}

resource "oci_identity_group" "cicd" {
  compartment_id = var.tenancy_ocid
  name           = "${var.project_name}-cicd"
  description    = "CI/CD group for ${var.project_name}"
  
  freeform_tags = var.tags
}

# Generate registry endpoint based on region
locals {
  registry_endpoint = "${var.region}.ocir.io"
  registry_namespace = data.oci_objectstorage_namespace.ns.namespace
}
