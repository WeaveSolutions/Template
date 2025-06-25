# Variables for Development Environment

variable "project_name" {
  description = "Project name"
  type        = string
  default     = "Nexpo"
}

variable "environment" {
  description = "Environment"
  type        = string
  default     = "development"
}

variable "aws_region" {
  description = "AWS region"
  type        = string
  default     = "us-east-1"
}

# Supabase Configuration
variable "supabase_url" {
  description = "Supabase project URL"
  type        = string
}

variable "supabase_anon_key" {
  description = "Supabase anonymous key"
  type        = string
  sensitive   = true
}

# Auth0 Configuration
variable "auth0_secret" {
  description = "Auth0 secret"
  type        = string
  sensitive   = true
}

variable "auth0_base_url" {
  description = "Auth0 base URL"
  type        = string
}

variable "auth0_issuer_base_url" {
  description = "Auth0 issuer base URL"
  type        = string
}

variable "auth0_client_id" {
  description = "Auth0 client ID"
  type        = string
}

variable "auth0_client_secret" {
  description = "Auth0 client secret"
  type        = string
  sensitive   = true
}

# Monitoring
variable "alarm_email_endpoints" {
  description = "Email addresses for alarm notifications"
  type        = list(string)
  default     = []
}
