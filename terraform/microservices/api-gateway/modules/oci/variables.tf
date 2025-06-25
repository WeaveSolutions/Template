variable "region" {
  description = "The OCI region to deploy resources"
  type        = string
  default     = "us-ashburn-1"
}

variable "tenancy_ocid" {
  description = "The OCID of the tenancy"
  type        = string
}

variable "compartment_ocid" {
  description = "The OCID of the compartment"
  type        = string
}

variable "api_endpoint_subnet_id" {
  description = "The OCID of the subnet for the API Gateway endpoint"
  type        = string
  default     = ""
}

variable "enable_private_endpoint" {
  description = "Whether to enable private endpoint for the API Gateway"
  type        = bool
  default     = false
}

variable "enable_waf" {
  description = "Whether to enable Web Application Firewall"
  type        = bool
  default     = true
}

variable "waf_policy_name" {
  description = "Name of the WAF policy"
  type        = string
  default     = ""
}

variable "allowed_ips" {
  description = "List of allowed IP addresses in CIDR notation"
  type        = list(string)
  default     = []
}

variable "rate_limiting_policies" {
  description = "Rate limiting policies for the API Gateway"
  type = map(object({
    rate_in_requests_per_second = number
    rate_key                     = string
  }))
  default = {
    default = {
      rate_in_requests_per_second = 100
      rate_key                     = "CLIENT_IP"
    }
  }
}

variable "enable_logging" {
  description = "Whether to enable logging for the API Gateway"
  type        = bool
  default     = true
}

variable "log_group_id" {
  description = "The OCID of the log group for API Gateway logs"
  type        = string
  default     = ""
}

variable "log_retention_duration" {
  description = "The retention period of logs in days"
  type        = number
  default     = 30
}
