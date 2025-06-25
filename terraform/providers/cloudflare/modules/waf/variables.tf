variable "filters" {
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

variable "firewall_rules" {
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
  
  validation {
    condition     = length([for rule in var.firewall_rules : true if rule.filter_id == null && rule.filter_idx == null]) == 0
    error_message = "Each firewall rule must specify either filter_id or filter_idx."
  }
}

variable "rate_limits" {
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

variable "access_rules" {
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
  
  validation {
    condition     = length([for rule in var.access_rules : true if rule.zone_id == null && rule.account_id == null]) == 0
    error_message = "Each access rule must specify either zone_id or account_id."
  }
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
