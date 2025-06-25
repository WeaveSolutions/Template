variable "location" {
  description = "Azure region to deploy resources"
  type        = string
  default     = "eastus"
}

variable "resource_group_name" {
  description = "Name of the resource group"
  type        = string
  default     = ""
}

variable "sku_name" {
  description = "SKU name for API Management service"
  type        = string
  default     = "Consumption"
}

variable "publisher_name" {
  description = "Publisher name for API Management"
  type        = string
  default     = "Contoso"
}

variable "publisher_email" {
  description = "Publisher email for API Management"
  type        = string
  default     = "admin@contoso.com"
}

variable "enable_virtual_network" {
  description = "Enable virtual network integration"
  type        = bool
  default     = false
}

variable "virtual_network_type" {
  description = "Type of virtual network (None, External, Internal)"
  type        = string
  default     = "None"
}

variable "enable_frontdoor" {
  description = "Enable Azure Front Door for global load balancing"
  type        = bool
  default     = true
}

variable "enable_waf" {
  description = "Enable Web Application Firewall"
  type        = bool
  default     = true
}

variable "waf_mode" {
  description = "WAF mode (Prevention or Detection)"
  type        = string
  default     = "Prevention"
}

variable "allowed_ips" {
  description = "List of allowed IP addresses in CIDR notation"
  type        = list(string)
  default     = []
}

variable "enable_diagnostic_setting" {
  description = "Enable diagnostic settings for API Management"
  type        = bool
  default     = true
}

variable "log_analytics_workspace_id" {
  description = "ID of the Log Analytics workspace for diagnostics"
  type        = string
  default     = ""
}

variable "retention_days" {
  description = "Number of days to retain logs"
  type        = number
  default     = 30
}
