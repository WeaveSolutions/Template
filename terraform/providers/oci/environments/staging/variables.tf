variable "project_name" {
  description = "The name of the project"
  type        = string
}

variable "compartment_id" {
  description = "The OCID of the compartment to create resources in"
  type        = string
}

variable "region" {
  description = "The OCI region to deploy resources"
  type        = string
  default     = "us-phoenix-1"
}

variable "db_admin_password" {
  description = "The admin password for the database"
  type        = string
  sensitive   = true
}

variable "kubernetes_version" {
  description = "The version of Kubernetes to use for OKE cluster"
  type        = string
  default     = "v1.26.2"
}

variable "enable_auth0" {
  description = "Enable Auth0 integration"
  type        = bool
  default     = true
}

variable "enable_waf" {
  description = "Enable Web Application Firewall"
  type        = bool
  default     = true
}
