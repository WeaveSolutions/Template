variable "cloudflare_account_id" {
  description = "The Cloudflare account ID to use for Workers resources"
  type        = string
}

variable "scripts" {
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

variable "kv_namespaces" {
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

variable "cron_triggers" {
  description = "List of cron triggers for workers"
  type        = list(object({
    script_name = string
    schedules   = list(string)
  }))
  default     = []
}

variable "kv_entries" {
  description = "List of KV entries to create"
  type        = list(object({
    namespace_id = string
    key          = string
    value        = string
  }))
  default     = []
}
