variable "project_name" {
  description = "Name of the project"
  type        = string
  default     = "Nexpo"
}

variable "environment" {
  description = "Environment"
  type        = string
  default     = "development"
}

variable "project_id" {
  description = "GCP Project ID"
  type        = string
}

variable "project_number" {
  description = "GCP Project Number"
  type        = string
}

variable "billing_account_id" {
  description = "GCP Billing Account ID"
  type        = string
}

variable "region" {
  description = "GCP region"
  type        = string
  default     = "us-central1"
}

variable "supabase_url" {
  description = "Supabase URL"
  type        = string
}

variable "supabase_anon_key" {
  description = "Supabase anonymous key"
  type        = string
}

variable "supabase_service_key" {
  description = "Supabase service key"
  type        = string
  sensitive   = true
}

variable "alert_email_endpoints" {
  description = "Email addresses for alerts"
  type        = list(string)
  default     = []
}

# Firebase configuration
variable "firestore_location" {
  description = "The location for Firestore database"
  type        = string
  default     = "nam5"
}

variable "enable_firebase_analytics" {
  description = "Enable Firebase Analytics"
  type        = bool
  default     = true
}

variable "enable_firebase_crashlytics" {
  description = "Enable Firebase Crashlytics"
  type        = bool
  default     = true
}

variable "enable_firebase_performance" {
  description = "Enable Firebase Performance Monitoring"
  type        = bool
  default     = true
}

variable "enable_firebase_messaging" {
  description = "Enable Firebase Cloud Messaging"
  type        = bool
  default     = true
}

variable "enable_firebase_remote_config" {
  description = "Enable Firebase Remote Config"
  type        = bool
  default     = true
}

# Auth0 configuration
variable "auth0_domain" {
  description = "Auth0 domain for token exchange"
  type        = string
}

variable "auth0_api_identifier" {
  description = "Auth0 API identifier for token validation"
  type        = string
}
