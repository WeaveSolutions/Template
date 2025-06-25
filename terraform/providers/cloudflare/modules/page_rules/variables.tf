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
