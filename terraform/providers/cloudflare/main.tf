/**
 * # Cloudflare Provider Configuration
 *
 * This is the main entry point for the Cloudflare provider in the multi-cloud infrastructure.
 * It configures DNS, WAF, Page Rules, and Workers resources for web properties.
 */

terraform {
  required_providers {
    cloudflare = {
      source  = "cloudflare/cloudflare"
      version = "~> 4.0"
    }
  }
}

provider "cloudflare" {
  api_token = var.cloudflare_api_token
  email     = var.cloudflare_email
}

# DNS Management
module "dns" {
  source = "./modules/dns"
  
  cloudflare_account_id = var.cloudflare_account_id
  zones                 = var.dns_zones
  records               = var.dns_records
  tags                  = var.tags
}

# Web Application Firewall
module "waf" {
  source = "./modules/waf"
  
  filters        = var.waf_filters
  firewall_rules = var.waf_firewall_rules
  rate_limits    = var.waf_rate_limits
  access_rules   = var.waf_access_rules
  waf_packages   = var.waf_packages
  waf_groups     = var.waf_groups
  waf_rules      = var.waf_rules
}

# Page Rules for URL redirects, caching, and security
module "page_rules" {
  source = "./modules/page_rules"
  
  page_rules = var.page_rules
}

# Serverless Edge Computing with Workers
module "workers" {
  source = "./modules/workers"
  
  cloudflare_account_id = var.cloudflare_account_id
  scripts               = var.worker_scripts
  kv_namespaces         = var.worker_kv_namespaces
  worker_routes         = var.worker_routes
  cron_triggers         = var.worker_cron_triggers
  kv_entries            = var.worker_kv_entries
}

# SSL/TLS Configuration
resource "cloudflare_zone_settings_override" "zone_settings" {
  for_each = { for zone_name, zone in var.zone_settings : zone_name => zone }
  
  zone_id = module.dns.zone_ids[each.key]
  
  settings {
    ssl                      = lookup(each.value, "ssl_mode", "full")
    always_use_https         = lookup(each.value, "always_use_https", true)
    min_tls_version          = lookup(each.value, "min_tls_version", "1.2")
    automatic_https_rewrites = lookup(each.value, "automatic_https_rewrites", true)
    tls_1_3                  = lookup(each.value, "tls_1_3", "on")
    opportunistic_encryption = lookup(each.value, "opportunistic_encryption", "on")
    universal_ssl            = lookup(each.value, "universal_ssl", true)
  }
}
