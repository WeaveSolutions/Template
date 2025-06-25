variable "ibm_region" {
  description = "IBM Cloud region to deploy resources"
  type        = string
  default     = "us-south"
}

variable "ibmcloud_api_key" {
  description = "IBM Cloud API Key"
  type        = string
  sensitive   = true
}

variable "project_name" {
  description = "Project name for resource naming"
  type        = string
}

variable "environment" {
  description = "Environment name (e.g. dev, prod, staging)"
  type        = string
}

variable "tags" {
  description = "Tags to apply to resources"
  type        = map(string)
  default     = {}
}

variable "availability_zones" {
  description = "List of availability zones"
  type        = list(string)
  default     = ["us-south-1", "us-south-2"]
}

variable "allowed_origins" {
  description = "List of allowed CORS origins"
  type        = list(string)
  default     = ["*"]
}

variable "enable_waf" {
  description = "Enable Web Application Firewall for API Gateway"
  type        = bool
  default     = true
}

variable "enable_private_endpoints" {
  description = "Enable private API Gateway endpoints"
  type        = bool
  default     = false
}

variable "api_gateway_plan" {
  description = "Plan for the API Gateway service"
  type        = string
  default     = "standard"
}

variable "api_rate_limit" {
  description = "Default rate limit for API calls per minute"
  type        = number
  default     = 100
}

variable "api_client_id" {
  description = "Client ID for API Gateway subscription"
  type        = string
}

variable "api_client_secret" {
  description = "Client secret for API Gateway subscription"
  type        = string
  sensitive   = true
}

variable "openapi_spec_path" {
  description = "Path to the OpenAPI specification file"
  type        = string
  default     = "openapi_spec.yaml"
}

variable "api_domain" {
  description = "Domain name for the API Gateway"
  type        = string
  default     = "api.example.com"
}
