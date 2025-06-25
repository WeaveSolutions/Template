variable "compartment_ocid" {
  description = "OCI Compartment OCID"
  type        = string
}

variable "project_name" {
  description = "Project name"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "subnet_id" {
  description = "Subnet ID for load balancer"
  type        = string
}

variable "nsg_id" {
  description = "Network Security Group ID"
  type        = string
}

variable "shape" {
  description = "Load balancer shape"
  type        = string
  default     = "flexible"
}

variable "min_bandwidth_mbps" {
  description = "Minimum bandwidth in Mbps (for flexible shape)"
  type        = number
  default     = 10
}

variable "max_bandwidth_mbps" {
  description = "Maximum bandwidth in Mbps (for flexible shape)"
  type        = number
  default     = 100
}

variable "backend_set_health_check_url" {
  description = "Health check URL path"
  type        = string
  default     = "/health"
}

variable "backend_instances" {
  description = "List of backend instances"
  type = list(object({
    ip_address = string
    port       = number
    weight     = optional(number)
    backup     = optional(bool)
    drain      = optional(bool)
    offline    = optional(bool)
  }))
  default = []
}

variable "certificate_name" {
  description = "Certificate name for HTTPS"
  type        = string
  default     = null
}

variable "public_certificate" {
  description = "Public certificate for HTTPS"
  type        = string
  default     = null
  sensitive   = true
}

variable "private_key" {
  description = "Private key for HTTPS certificate"
  type        = string
  default     = null
  sensitive   = true
}

variable "path_routes" {
  description = "Path-based routing rules"
  type = list(object({
    path             = string
    backend_set_name = string
    match_type       = string
  }))
  default = []
}

variable "enable_waf" {
  description = "Enable Web Application Firewall"
  type        = bool
  default     = false
}

variable "tags" {
  description = "Freeform tags"
  type        = map(string)
  default     = {}
}
