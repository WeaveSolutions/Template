# Variables for Production Environment

variable "project_name" {
  description = "Project name"
  type        = string
  default     = "Nexpo"
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "production"
}

variable "aws_region" {
  description = "AWS region"
  type        = string
  default     = "us-east-1"
}

# Domain Configuration
variable "custom_domain" {
  description = "Custom domain name"
  type        = string
  default     = ""
}

variable "acm_certificate_arn" {
  description = "ACM certificate ARN for SSL"
  type        = string
  default     = ""
}

variable "route53_zone_id" {
  description = "Route 53 hosted zone ID"
  type        = string
  default     = ""
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
}

variable "alarm_sms_endpoints" {
  description = "Phone numbers for SMS alarm notifications"
  type        = list(string)
  default     = []
}

# Budget
variable "monthly_budget_limit" {
  description = "Monthly budget limit in USD"
  type        = string
  default     = "100"
}

variable "budget_alert_emails" {
  description = "Email addresses for budget alerts"
  type        = list(string)
}
