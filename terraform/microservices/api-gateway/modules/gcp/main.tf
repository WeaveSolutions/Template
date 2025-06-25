# Provider Configuration
provider "google" {
  project = var.gcp_project_id
  region  = var.gcp_region
}

# Enable required APIs
resource "google_project_service" "apigateway" {
  service            = "apigateway.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "service_management" {
  service            = "servicemanagement.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "service_control" {
  service            = "servicecontrol.googleapis.com"
  disable_on_destroy = false
}

# Cloud Armor Security Policy
resource "google_compute_security_policy" "api_policy" {
  count = var.enable_cloud_armor ? 1 : 0
  name  = "${var.project_name}-${var.environment}-api-policy"
  
  # Default rule to deny all traffic
  rule {
    action   = "deny(403)"
    priority = "2147483647"
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = ["*"]
      }
    }
    description = "Default deny rule"
  }
  
  # Allow specific IPs
  dynamic "rule" {
    for_each = length(var.allowed_ips) > 0 ? [1] : []
    content {
      action   = "allow"
      priority = "1000"
      match {
        versioned_expr = "SRC_IPS_V1"
        config {
          src_ip_ranges = var.allowed_ips
        }
      }
      description = "Allow traffic from specific IPs"
    }
  }
  
  # Common web attack protection
  rule {
    action   = "deny(403)"
    priority = "900"
    match {
      expr {
        expression = "evaluatePreconfiguredWaf('sqli-v33-stable', {'sensitivity': 1, 'opt_out_rule_ids': ['owasp-crs-id942220-sqli']})"
      }
    }
    description = "SQL injection filter"
  }
  
  rule {
    action   = "deny(403)"
    priority = "901"
    match {
      expr {
        expression = "evaluatePreconfiguredWaf('xss-v33-stable', {'sensitivity': 1})"
      }
    }
    description = "XSS filter"
  }
}

# API Gateway
resource "google_api_gateway_api" "api_gw" {
  provider     = google-beta
  api_id       = "${var.project_name}-${var.environment}-api"
  display_name = "${var.project_name} API (${var.environment})"
}

# API Config
resource "google_api_gateway_api_config" "api_cfg" {
  provider      = google-beta
  api           = google_api_gateway_api.api_gw.api_id
  api_config_id = "${var.api_config_name}-${substr(md5(timestamp()), 0, 8)}"
  
  openapi_documents {
    document {
      path     = "spec.yaml"
      contents = base64encode(templatefile("${path.module}/openapi_spec.yaml.tftpl", {
        project_name = var.project_name
        environment  = var.environment
        auth0_domain = var.auth0_domain
        auth0_audience = var.auth0_audience
      }))
    }
  }
  
  lifecycle {
    create_before_destroy = true
  }
}

# API Gateway
resource "google_api_gateway_gateway" "api_gw" {
  provider   = google-beta
  region     = var.gcp_region
  project    = var.gcp_project_id
  
  api_config = google_api_gateway_api_config.api_cfg.id
  gateway_id = "${var.project_name}-${var.environment}-gw"
  
  depends_on = [
    google_api_gateway_api_config.api_cfg
  ]
}

# Outputs
output "api_gateway_url" {
  description = "The URL of the API Gateway"
  value       = "https://${google_api_gateway_gateway.api_gw.default_hostname}"
}

output "api_gateway_id" {
  description = "The ID of the API Gateway"
  value       = google_api_gateway_gateway.api_gw.gateway_id
}

output "api_config_id" {
  description = "The ID of the API config"
  value       = google_api_gateway_api_config.api_cfg.api_config_id
}
