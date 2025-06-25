# Provider Configuration
provider "ibm" {
  region           = var.ibm_region
  ibmcloud_api_key = var.ibmcloud_api_key
}

# Resource Group
resource "ibm_resource_group" "api_gateway_group" {
  name = "${var.project_name}-${var.environment}-rg"
  tags = var.tags
}

# VPC
resource "ibm_is_vpc" "api_gateway_vpc" {
  name           = "${var.project_name}-${var.environment}-vpc"
  resource_group = ibm_resource_group.api_gateway_group.id
  tags           = var.tags
}

# Subnets
resource "ibm_is_subnet" "api_gateway_subnet" {
  count                    = length(var.availability_zones)
  name                     = "${var.project_name}-${var.environment}-subnet-${count.index + 1}"
  vpc                      = ibm_is_vpc.api_gateway_vpc.id
  zone                     = var.availability_zones[count.index]
  total_ipv4_address_count = 256
  resource_group           = ibm_resource_group.api_gateway_group.id
}

# API Gateway Instance
resource "ibm_resource_instance" "api_gateway" {
  name              = "${var.project_name}-${var.environment}-api-gateway"
  service           = "api-gateway"
  plan              = var.api_gateway_plan
  location          = var.ibm_region
  resource_group_id = ibm_resource_group.api_gateway_group.id
  
  tags = var.tags
  
  parameters = {
    default_limit = var.api_rate_limit
  }
}

# API Gateway Definition
resource "ibm_api_gateway_endpoint_subscription" "api_gateway_subscription" {
  artifact_id   = ibm_resource_instance.api_gateway.id
  client_id     = var.api_client_id
  client_secret = var.api_client_secret
  name          = "${var.project_name}-${var.environment}-subscription"
}

# API Gateway Security
resource "ibm_api_gateway_endpoint" "api_gateway_endpoint" {
  name           = "${var.project_name}-${var.environment}-api"
  managed        = true
  provider_id    = ibm_resource_instance.api_gateway.id
  type           = "public"
  open_api_doc   = file(var.openapi_spec_path)
  resource_group = ibm_resource_group.api_gateway_group.id
  
  cors {
    enabled          = true
    allowed_origins  = var.allowed_origins
    allowed_methods  = ["GET", "POST", "PUT", "DELETE", "OPTIONS", "HEAD", "PATCH"]
    allowed_headers  = ["*"]
    exposed_headers  = ["Content-Length", "Content-Type"]
    max_age_seconds  = 86400
    allow_credentials = true
  }
}

# Logging
resource "ibm_resource_instance" "logging" {
  name              = "${var.project_name}-${var.environment}-logging"
  service           = "logdnaat"
  plan              = "lite"
  location          = var.ibm_region
  resource_group_id = ibm_resource_group.api_gateway_group.id
}

# Connect API Gateway logs to logging instance
resource "ibm_resource_binding" "api_gateway_logging_binding" {
  name                 = "${var.project_name}-${var.environment}-logging-binding"
  source_service_name  = ibm_resource_instance.api_gateway.name
  target_service_name  = ibm_resource_instance.logging.name
  resource_group_id    = ibm_resource_group.api_gateway_group.id
}

# Security - Key Protect for secrets management
resource "ibm_resource_instance" "key_protect" {
  name              = "${var.project_name}-${var.environment}-key-protect"
  service           = "kms"
  plan              = "tiered-pricing"
  location          = var.ibm_region
  resource_group_id = ibm_resource_group.api_gateway_group.id
}

# Create a root key for encryption
resource "ibm_kms_key" "api_key" {
  instance_id  = ibm_resource_instance.key_protect.guid
  key_name     = "${var.project_name}-${var.environment}-key"
  standard_key = false
  force_delete = true
}

# Private API Gateway access (when enabled)
resource "ibm_is_security_group" "api_security_group" {
  count          = var.enable_private_endpoints ? 1 : 0
  name           = "${var.project_name}-${var.environment}-sg"
  vpc            = ibm_is_vpc.api_gateway_vpc.id
  resource_group = ibm_resource_group.api_gateway_group.id
}

resource "ibm_is_security_group_rule" "api_security_rule" {
  count     = var.enable_private_endpoints ? 1 : 0
  group     = ibm_is_security_group.api_security_group[0].id
  direction = "inbound"
  remote    = "0.0.0.0/0"
  
  tcp {
    port_min = 443
    port_max = 443
  }
}

# Cloud Internet Services (WAF) - when WAF is enabled
resource "ibm_cis" "api_waf" {
  count             = var.enable_waf ? 1 : 0
  name              = "${var.project_name}-${var.environment}-waf"
  plan              = "standard"
  location          = "global"
  resource_group_id = ibm_resource_group.api_gateway_group.id
}

resource "ibm_cis_domain" "api_domain" {
  count          = var.enable_waf ? 1 : 0
  domain         = var.api_domain
  cis_id         = ibm_cis.api_waf[0].id
  dns_entry_cname = ibm_api_gateway_endpoint.api_gateway_endpoint.url
}

resource "ibm_cis_waf_package" "api_waf_package" {
  count      = var.enable_waf ? 1 : 0
  cis_id     = ibm_cis.api_waf[0].id
  domain_id  = ibm_cis_domain.api_domain[0].id
  sensitivity = "medium"
  action_mode = "simulate"
}
