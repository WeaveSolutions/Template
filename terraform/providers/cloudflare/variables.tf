variable "cloudflare_api_token" {
  description = "Cloudflare API token with sufficient permissions to manage resources"
  type        = string
  sensitive   = true
}

variable "cloudflare_email" {
  description = "Email address associated with the Cloudflare account"
  type        = string
}

variable "cloudflare_account_id" {
  description = "Cloudflare account ID"
  type        = string
}

variable "tags" {
  description = "A map of tags to add to all resources"
  type        = map(string)
  default     = {}
}

# DNS Variables
variable "dns_zones" {
  description = "Map of DNS zones to manage"
  type        = map(object({
    plan         = optional(string, "free")
    type         = optional(string, "full")
    paused       = optional(bool, false)
    jump_start   = optional(bool, false)
    meta         = optional(map(bool), null)
    tags         = optional(map(string), {})
    apply_settings = optional(bool, false)
    enable_dnssec  = optional(bool, false)
    settings     = optional(map(any), {})
  }))
  default     = {}
}

variable "dns_records" {
  description = "List of DNS records to create"
  type        = list(object({
    zone_name       = optional(string, null)
    zone_id         = optional(string, null)
    name            = string
    value           = string
    type            = string
    ttl             = optional(number, 1)
    priority        = optional(number, null)
    proxied         = optional(bool, false)
    allow_overwrite = optional(bool, false)
    comment         = optional(string, null)
    data            = optional(map(any), null)
  }))
  default     = []
}

# WAF Variables
variable "waf_filters" {
  description = "List of WAF filters to create"
  type        = list(object({
    zone_id     = string
    description = optional(string, null)
    expression  = string
    ref         = optional(string, null)
    paused      = optional(bool, false)
  }))
  default     = []
}

variable "waf_firewall_rules" {
  description = "List of firewall rules to create"
  type        = list(object({
    zone_id     = string
    description = optional(string, null)
    filter_id   = optional(string, null)
    filter_idx  = optional(number, null)
    action      = string
    priority    = optional(number, null)
    paused      = optional(bool, false)
    products    = optional(list(string), null)
  }))
  default     = []
}

variable "waf_rate_limits" {
  description = "List of rate limit rules to create"
  type        = list(object({
    zone_id            = string
    threshold          = number
    period             = number
    description        = optional(string, null)
    disabled           = optional(bool, false)
    bypass_url_patterns = optional(list(string), [])
    match              = object({
      request   = object({
        methods     = optional(list(string), ["GET"])
        schemes     = optional(list(string), ["HTTP", "HTTPS"])
        url_pattern = optional(string, "*")
      })
      response  = optional(object({
        statuses      = optional(list(number), [200])
        origin_traffic = optional(bool, true)
      }), null)
    })
    action            = object({
      mode    = optional(string, "ban")
      timeout = optional(number, 60)
      response = optional(object({
        content_type = optional(string, "text/plain")
        body         = optional(string, "You have been rate limited")
      }), null)
    })
    correlate_by      = optional(string, "ip")
  }))
  default     = []
}

variable "waf_access_rules" {
  description = "List of IP access rules to create"
  type        = list(object({
    zone_id      = optional(string, null)
    account_id   = optional(string, null)
    notes        = optional(string, null)
    mode         = string
    configuration = object({
      target     = string
      value      = string
    })
  }))
  default     = []
}

variable "waf_packages" {
  description = "List of WAF packages to configure"
  type        = list(object({
    zone_id     = string
    package_id  = string
    sensitivity = optional(string, null)
    action_mode = optional(string, null)
  }))
  default     = []
}

variable "waf_groups" {
  description = "List of WAF groups to configure"
  type        = list(object({
    zone_id    = string
    package_id = string
    group_id   = string
    mode       = string
  }))
  default     = []
}

variable "waf_rules" {
  description = "List of WAF rules to configure"
  type        = list(object({
    zone_id    = string
    package_id = string
    rule_id    = string
    mode       = string
  }))
  default     = []
}

# Page Rules Variables
variable "page_rules" {
  description = "List of page rules to create"
  type        = list(object({
    zone_id  = string
    target   = string
    status   = optional(string, "active")
    priority = optional(number, null)
    actions  = map(any)
  }))
  default     = []
}

# Workers Variables
variable "worker_scripts" {
  description = "List of Cloudflare Worker scripts to deploy"
  type        = list(object({
    name                = string
    content             = string
    module              = optional(bool, false)
    kv_namespace_bindings = optional(list(object({
      name         = string
      namespace_id = string
    })), [])
    secret_text_bindings = optional(list(object({
      name = string
      text = string
    })), [])
    plain_text_bindings = optional(list(object({
      name = string
      text = string
    })), [])
    webassembly_bindings = optional(list(object({
      name   = string
      module = string
    })), [])
    service_bindings = optional(list(object({
      name        = string
      service     = string
      environment = optional(string, null)
    })), [])
    r2_bucket_bindings = optional(list(object({
      name        = string
      bucket_name = string
    })), [])
    analytics_engine_bindings = optional(list(object({
      name    = string
      dataset = string
    })), [])
    queue_bindings = optional(list(object({
      name       = string
      queue_name = string
      binding_id = optional(string, null)
    })), [])
    tags                = optional(list(string), [])
    compatibility_date  = optional(string, "2023-01-01")
    compatibility_flags = optional(list(string), [])
    usage_model         = optional(string, "bundled")
  }))
  default     = []
}

variable "worker_kv_namespaces" {
  description = "List of KV namespaces to create"
  type        = list(object({
    title = string
  }))
  default     = []
}

variable "worker_routes" {
  description = "List of worker routes to create"
  type        = list(object({
    zone_id     = string
    pattern     = string
    script_name = string
  }))
  default     = []
}

variable "worker_cron_triggers" {
  description = "List of cron triggers for workers"
  type        = list(object({
    script_name = string
    schedules   = list(string)
  }))
  default     = []
}

variable "worker_kv_entries" {
  description = "List of KV entries to create"
  type        = list(object({
    namespace_id = string
    key          = string
    value        = string
  }))
  default     = []
}

# SSL/TLS Settings
variable "zone_settings" {
  description = "SSL/TLS and other zone settings for domains"
  type        = map(object({
    ssl_mode                = optional(string, "full")
    always_use_https        = optional(bool, true)
    min_tls_version         = optional(string, "1.2")
    automatic_https_rewrites = optional(bool, true)
    tls_1_3                 = optional(string, "on")
    opportunistic_encryption = optional(string, "on")
    universal_ssl           = optional(bool, true)
  }))
  default     = {}
}
