variable "cloudflare_account_id" {
  description = "The Cloudflare account ID to use for creating zones"
  type        = string
}

variable "zones" {
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

variable "records" {
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

  validation {
    condition     = length([for record in var.records : true if record.zone_name == null && record.zone_id == null]) == 0
    error_message = "Each record must specify either zone_name or zone_id."
  }
}

variable "tags" {
  description = "A map of tags to add to all resources"
  type        = map(string)
  default     = {}
}

variable "timeouts" {
  description = "Define timeouts for resource operations"
  type        = map(string)
  default     = {
    create = "5m"
    update = "5m"
    delete = "5m"
  }
}
