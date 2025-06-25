variable "project_name" {
  description = "The name of the project"
  type        = string
}

variable "environment" {
  description = "The deployment environment (development, staging, production)"
  type        = string
}

variable "project_id" {
  description = "The GCP project ID"
  type        = string
}

variable "region" {
  description = "The GCP region"
  type        = string
  default     = "us-central1"
}

variable "firestore_location" {
  description = "The location for Firestore database"
  type        = string
  default     = "nam5" # Multi-region: United States
}

variable "enable_analytics" {
  description = "Enable Firebase Analytics"
  type        = bool
  default     = true
}

variable "enable_crashlytics" {
  description = "Enable Firebase Crashlytics"
  type        = bool
  default     = true
}

variable "enable_performance" {
  description = "Enable Firebase Performance Monitoring"
  type        = bool
  default     = true
}

variable "enable_cloud_messaging" {
  description = "Enable Firebase Cloud Messaging"
  type        = bool
  default     = true
}

variable "enable_remote_config" {
  description = "Enable Firebase Remote Config"
  type        = bool
  default     = true
}

variable "auth0_domain" {
  description = "Auth0 domain for token exchange"
  type        = string
}

variable "auth0_api_identifier" {
  description = "Auth0 API identifier for token validation"
  type        = string
}

variable "labels" {
  description = "A map of labels to assign to resources"
  type        = map(string)
  default     = {}
}
