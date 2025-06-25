# Provider Configuration
provider "oci" {
  region           = var.region
  tenancy_ocid     = var.tenancy_ocid
  config_file_profile = "DEFAULT"
}

# Generate a random string for unique resource naming
resource "random_string" "suffix" {
  length  = 8
  special = false
  upper   = false
}

locals {
  # Standardized naming
  display_name = "${var.project_name}-${var.environment}"
  
  # API Gateway configuration
  api_gateway_name = "${local.display_name}-gw-${random_string.suffix.result}"
  
  # WAF policy name
  waf_policy_name = var.waf_policy_name != "" ? var.waf_policy_name : "${local.display_name}-waf-policy"
  
  # Log group ID
  log_group_id = var.log_group_id != "" ? var.log_group_id : oci_logging_log_group.api_gw_log_group[0].id
}

# API Gateway
resource "oci_apigateway_gateway" "api_gateway" {
  compartment_id = var.compartment_ocid
  endpoint_type  = var.enable_private_endpoint ? "PRIVATE" : "PUBLIC"
  subnet_id     = var.enable_private_endpoint ? var.api_endpoint_subnet_id : null
  display_name   = local.api_gateway_name
  
  response_cache_details {
    type = "NONE"
  }
  
  freeform_tags = {
    "Project"     = var.project_name
    "Environment" = var.environment
    "ManagedBy"   = "Terraform"
  }
}

# API Deployment
resource "oci_apigateway_deployment" "api_deployment" {
  compartment_id = var.compartment_ocid
  gateway_id     = oci_apigateway_gateway.api_gateway.id
  display_name   = "${local.display_name}-deployment"
  path_prefix   = "/${var.project_name}"
  
  specification {
    request_policies {
      authentication {
        type                        = "CUSTOM_AUTHENTICATION"
        function_id                 = oci_functions_function.auth_function.id
        token_header                = "Authorization"
        token_auth_scheme           = "Bearer"
        is_anonymous_access_allowed = false
      }
      
      # Rate limiting
      dynamic "rate_limiting" {
        for_each = var.rate_limiting_policies
        content {
          rate_in_requests_per_second = rate_limiting.value.rate_in_requests_per_second
          rate_key                     = rate_limiting.value.rate_key
        }
      }
      
      # CORS
      cors {
        allowed_origins = ["*"]
        allowed_methods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        allowed_headers = ["*"]
      }
    }
    
    routes {
      path    = "/health"
      methods = ["GET"]
      backend {
        type = "STOCK_RESPONSE_BACKEND"
        body = jsonencode({
          status = "ok"
        })
        status = 200
        headers {
          name  = "Content-Type"
          value = "application/json"
        }
      }
    }
    
    # Add more routes as needed
  }
  
  freeform_tags = {
    "Project"     = var.project_name
    "Environment" = var.environment
    "ManagedBy"   = "Terraform"
  }
}

# Authentication Function
resource "oci_functions_application" "auth_application" {
  compartment_id = var.compartment_ocid
  display_name   = "${local.display_name}-auth-app"
  subnet_ids     = [var.api_endpoint_subnet_id]
  
  freeform_tags = {
    "Project"     = var.project_name
    "Environment" = var.environment
    "ManagedBy"   = "Terraform"
  }
}

resource "oci_functions_function" "auth_function" {
  application_id = oci_functions_application.auth_application.id
  display_name  = "auth-function"
  image         = "iad.ocir.io/your-tenancy/your-repo/auth-function:latest"
  memory_in_mbs = 256
  
  config = {
    "AUTH0_DOMAIN"   = var.auth0_domain
    "AUTH0_AUDIENCE" = var.auth0_audience
  }
  
  freeform_tags = {
    "Project"     = var.project_name
    "Environment" = var.environment
    "ManagedBy"   = "Terraform"
  }
}

# WAF Policy (if enabled)
resource "oci_waf_web_app_firewall_policy" "waf_policy" {
  count = var.enable_waf ? 1 : 0
  
  compartment_id = var.compartment_ocid
  display_name   = local.waf_policy_name
  
  actions {
    name = "action"
    type = "RETURN_HTTP_RESPONSE"
    
    code               = 403
    headers {
      name  = "Content-Type"
      value = "application/json"
    }
    body = jsonencode({
      code    = 403,
      message = "Access Denied by WAF Policy"
    })
  }
  
  # IP-based access control
  dynamic "request_access_control" {
    for_each = length(var.allowed_ips) > 0 ? [1] : []
    
    content {
      type                = "ACCESS_CONTROL"
      name                = "IP Access Control"
      condition_language  = "JMESPATH"
      condition           = "!contains(${jsonencode(var.allowed_ips)}, ipAddr())"
      action_name         = "action"
    }
  }
  
  # OWASP protection rules
  request_protection {
    rules {
      type                = "PROTECTION"
      name                = "OWASP Protection Rules"
      condition_language  = "JMESPATH"
      condition           = "true"
      action_name         = "action"
      
      protection_capabilities {
        key     = "941100"  # SQL Injection
        version = 1
      }
      
      protection_capabilities {
        key     = "941120"  # SQL Injection
        version = 1
      }
      
      protection_capabilities {
        key     = "941130"  # SQL Injection
        version = 1
      }
      
      protection_capabilities {
        key     = "941160"  # NoScript XSS
        version = 1
      }
      
      protection_capabilities {
        key     = "941170"  # XSS
        version = 1
      }
      
      protection_capabilities {
        key     = "941180"  # XSS
        version = 1
      }
      
      protection_capabilities {
        key     = "941190"  # XSS
        version = 1
      }
      
      protection_capabilities {
        key     = "941200"  # XSS
        version = 1
      }
    }
  }
  
  freeform_tags = {
    "Project"     = var.project_name
    "Environment" = var.environment
    "ManagedBy"   = "Terraform"
  }
}

# Log Group (if logging is enabled)
resource "oci_logging_log_group" "api_gw_log_group" {
  count          = var.enable_logging && var.log_group_id == "" ? 1 : 0
  compartment_id = var.compartment_ocid
  display_name   = "${local.display_name}-log-group"
  
  freeform_tags = {
    "Project"     = var.project_name
    "Environment" = var.environment
    "ManagedBy"   = "Terraform"
  }
}

# Log (if logging is enabled)
resource "oci_logging_log" "api_gw_log" {
  count        = var.enable_logging ? 1 : 0
  display_name = "${local.display_name}-log"
  log_group_id = local.log_group_id
  log_type     = "SERVICE"
  
  configuration {
    source {
      category    = "all"
      resource    = oci_apigateway_gateway.api_gateway.id
      service     = "apigateway"
      source_type = "OCISERVICE"
    }
    
    compartment_id = var.compartment_ocid
  }
  
  is_enabled = true
  
  retention_duration = var.log_retention_duration * 24 * 60 * 60  # Convert days to seconds
  
  freeform_tags = {
    "Project"     = var.project_name
    "Environment" = var.environment
    "ManagedBy"   = "Terraform"
  }
}

# Outputs
output "api_gateway_url" {
  description = "The URL of the API Gateway"
  value       = "https://${oci_apigateway_gateway.api_gateway.hostname}"
}

output "api_deployment_url" {
  description = "The URL of the API Deployment"
  value       = "https://${oci_apigateway_gateway.api_gateway.hostname}/${var.project_name}"
}

output "api_gateway_id" {
  description = "The OCID of the API Gateway"
  value       = oci_apigateway_gateway.api_gateway.id
}

output "api_deployment_id" {
  description = "The OCID of the API Deployment"
  value       = oci_apigateway_deployment.api_deployment.id
}

output "waf_policy_id" {
  description = "The OCID of the WAF Policy (if enabled)"
  value       = var.enable_waf ? oci_waf_web_app_firewall_policy.waf_policy[0].id : null
}

output "log_group_id" {
  description = "The OCID of the Log Group"
  value       = local.log_group_id
}
