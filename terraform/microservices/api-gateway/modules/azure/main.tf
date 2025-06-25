# Provider Configuration
provider "azurerm" {
  features {}
}

# Generate a random string for unique resource naming
resource "random_string" "suffix" {
  length  = 8
  special = false
  upper   = false
}

# Resource Group
resource "azurerm_resource_group" "rg" {
  count    = var.resource_group_name == "" ? 1 : 0
  name     = "${var.project_name}-${var.environment}-rg"
  location = var.location
  tags     = var.tags
}

locals {
  resource_group_name = coalesce(var.resource_group_name, azurerm_resource_group.rg[0].name)
  apim_name          = "${replace(var.project_name, "-", "")}${var.environment}apim"
  apim_id            = "${local.apim_name}-${random_string.suffix.result}"
}

# API Management Service
resource "azurerm_api_management" "apim" {
  name                = local.apim_id
  location            = var.location
  resource_group_name = local.resource_group_name
  publisher_name      = var.publisher_name
  publisher_email     = var.publisher_email
  sku_name            = var.sku_name
  
  identity {
    type = "SystemAssigned"
  }
  
  protocols {
    enable_http2 = true
  }
  
  virtual_network_type = var.enable_virtual_network ? var.virtual_network_type : "None"
  
  tags = merge(var.tags, {
    environment = var.environment
  })
}

# API
resource "azurerm_api_management_api" "api" {
  name                = "${var.project_name}-api"
  resource_group_name = local.resource_group_name
  api_management_name = azurerm_api_management.apim.name
  revision            = "1"
  display_name        = "${var.project_name} API"
  path                = "api"
  protocols           = ["https"]
  
  import {
    content_format = "openapi+json"
    content = jsonencode({
      openapi = "3.0.1"
      info = {
        title   = "${var.project_name} API"
        version = "1.0.0"
      }
      paths = {
        "/health" = {
          get = {
            summary     = "Health check"
            operationId = "healthCheck"
            responses = {
              "200" = {
                description = "OK"
                content = {
                  "application/json" = {
                    schema = {
                      type = "object"
                      properties = {
                        status = { type = "string" }
                      }
                    }
                  }
                }
              }
            }
            security = []
          }
        }
      }
      components = {
        securitySchemes = {
          oauth2 = {
            type = "oauth2"
            flows = {
              implicit = {
                authorizationUrl = "https://${var.auth0_domain}/authorize"
                scopes = {}
              }
            }
          }
        }
      }
    })
  }
}

# Product
resource "azurerm_api_management_product" "product" {
  product_id            = "${var.project_name}-product"
  api_management_name   = azurerm_api_management.apim.name
  resource_group_name   = local.resource_group_name
  display_name          = "${var.project_name} Product"
  subscription_required = true
  approval_required     = false
  published             = true
}

# Product API Association
resource "azurerm_api_management_product_api" "product_api" {
  api_name            = azurerm_api_management_api.api.name
  product_id          = azurerm_api_management_product.product.product_id
  api_management_name = azurerm_api_management.apim.name
  resource_group_name = local.resource_group_name
}

# Policy
resource "azurerm_api_management_api_policy" "policy" {
  api_name            = azurerm_api_management_api.api.name
  api_management_name = azurerm_api_management.apim.name
  resource_group_name = local.resource_group_name

  xml_content = <<XML
<policies>
  <inbound>
    <base />
    <cors>
      <allowed-origins>
        <origin>*</origin>
      </allowed-origins>
      <allowed-methods>
        <method>*</method>
      </allowed-methods>
      <allowed-headers>
        <header>*</header>
      </allowed-headers>
    </cors>
    <validate-jwt header-name="Authorization" failed-validation-httpcode="401" failed-validation-error-message="Unauthorized. Access token is missing or invalid.">
      <openid-config url="https://${var.auth0_domain}/.well-known/openid-configuration" />
      <audiences>
        <audience>${var.auth0_audience}</audience>
      </audiences>
      <issuers>
        <issuer>https://${var.auth0_domain}/</issuer>
      </issuers>
    </validate-jwt>
    <rate-limit calls="100" renewal-period="60" />
  </inbound>
  <backend>
    <base />
  </backend>
  <outbound>
    <base />
  </outbound>
  <on-error>
    <base />
  </on-error>
</policies>
XML
}

# Front Door (if enabled)
resource "azurerm_frontdoor" "afd" {
  count               = var.enable_frontdoor ? 1 : 0
  name                = "${var.project_name}-${var.environment}-fd"
  resource_group_name = local.resource_group_name
  enforce_backend_pools_certificate_name_check = false

  frontend_endpoint {
    name      = "${var.project_name}-endpoint"
    host_name = "${var.project_name}-${var.environment}.azurefd.net"
  }

  backend_pool_load_balancing {
    name = "loadBalancingSettings"
  }

  backend_pool_health_probe {
    name = "healthProbeSettings"
    path = "/"
  }

  backend_pool {
    name = "backendPool"
    backend {
      host_header = azurerm_api_management.apim.gateway_url
      address     = azurerm_api_management.apim.gateway_url
      http_port   = 80
      https_port  = 443
    }

    load_balancing_name = "loadBalancingSettings"
    health_probe_name  = "healthProbeSettings"
  }

  routing_rule {
    name               = "routingRule"
    accepted_protocols = ["Https"]
    patterns_to_match  = ["/*"]
    frontend_endpoints = ["${var.project_name}-endpoint"]
    forwarding_configuration {
      forwarding_protocol = "HttpsOnly"
      backend_pool_name  = "backendPool"
    }
  }

  # WAF Policy (if enabled)
  dynamic "frontend_endpoint" {
    for_each = var.enable_waf ? [1] : []
    content {
      name                                    = "wafEndpoint"
      host_name                              = "${var.project_name}-${var.environment}-waf.azurefd.net"
      web_application_firewall_policy_link_id = azurerm_frontdoor_firewall_policy.waf[0].id
    }
  }
}

# WAF Policy (if enabled)
resource "azurerm_frontdoor_firewall_policy" "waf" {
  count               = var.enable_waf ? 1 : 0
  name                = "${var.project_name}-${var.environment}-waf"
  resource_group_name = local.resource_group_name
  enabled             = true
  mode                = var.waf_mode
  
  managed_rule {
    type    = "Microsoft_DefaultRuleSet"
    version = "2.1"
    action  = "Block"
  }
  
  dynamic "custom_rule" {
    for_each = length(var.allowed_ips) > 0 ? [1] : []
    content {
      name                           = "IPRestriction"
      action                         = "Block"
      enabled                        = true
      priority                       = 100
      type                           = "MatchRule"
      rate_limit_duration_in_minutes = 1
      rate_limit_threshold           = 100
      
      match_condition {
        match_variable     = "RemoteAddr"
        operator           = "IPMatch"
        negation_condition = true
        match_values       = var.allowed_ips
      }
    }
  }
}

# Diagnostic Settings (if Log Analytics Workspace ID is provided)
resource "azurerm_monitor_diagnostic_setting" "apim_diag" {
  count                      = var.enable_diagnostic_setting && var.log_analytics_workspace_id != "" ? 1 : 0
  name                       = "${local.apim_id}-diag"
  target_resource_id         = azurerm_api_management.apim.id
  log_analytics_workspace_id = var.log_analytics_workspace_id

  enabled_log {
    category = "GatewayLogs"
    retention_policy {
      enabled = true
      days    = var.retention_days
    }
  }

  metric {
    category = "AllMetrics"
    enabled  = true
    retention_policy {
      enabled = true
      days    = var.retention_days
    }
  }
}

# Outputs
output "apim_gateway_url" {
  description = "The URL of the API Management Gateway"
  value       = "https://${azurerm_api_management.apim.gateway_url}"
}

output "apim_developer_portal_url" {
  description = "The URL of the API Management Developer Portal"
  value       = "https://${azurerm_api_management.apim.developer_portal_url}"
}

output "apim_management_url" {
  description = "The URL of the API Management Management API"
  value       = azurerm_api_management.apim.management_api_url
}

output "frontdoor_url" {
  description = "The URL of the Front Door (if enabled)"
  value       = var.enable_frontdoor ? "https://${azurerm_frontdoor.afd[0].frontend_endpoint[0].host_name}" : null
}

output "waf_policy_id" {
  description = "The ID of the WAF policy (if enabled)"
  value       = var.enable_waf ? azurerm_frontdoor_firewall_policy.waf[0].id : null
}
