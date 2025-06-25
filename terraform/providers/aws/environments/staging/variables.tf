# Variables for Staging Environment

variable "project_name" {
  description = "Project name"
  type        = string
  default     = "Nexpo"
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "staging"
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

# Alarm Configuration
variable "alarm_email_endpoints" {
  description = "Email addresses for CloudWatch alarms"
  type        = list(string)
}

# CORS Configuration
variable "cors_allowed_origins" {
  description = "Allowed origins for CORS"
  type        = list(string)
  default     = ["https://staging.example.com"]
}

# Custom Domain (optional)
variable "custom_domain" {
  description = "Custom domain for the staging environment"
  type        = string
  default     = ""
}

variable "acm_certificate_arn" {
  description = "ACM certificate ARN for custom domain"
  type        = string
  default     = ""
}
