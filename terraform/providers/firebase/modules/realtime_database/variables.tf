# ==============================================================================
# FIREBASE REALTIME DATABASE MODULE - VARIABLES
# ==============================================================================

variable "project_id" {
  description = "Firebase project ID"
  type        = string
}

variable "database_id" {
  description = "Realtime Database instance ID"
  type        = string
  default     = "default-rtdb"
}

variable "region" {
  description = "Region for the Realtime Database"
  type        = string
  default     = "us-central1"
  
  validation {
    condition = contains([
      "us-central1", "europe-west1", "asia-southeast1"
    ], var.region)
    error_message = "Region must be one of: us-central1, europe-west1, asia-southeast1."
  }
}

variable "database_type" {
  description = "Type of Realtime Database"
  type        = string
  default     = "DEFAULT_DATABASE"
  
  validation {
    condition     = contains(["DEFAULT_DATABASE", "USER_DATABASE"], var.database_type)
    error_message = "Database type must be either DEFAULT_DATABASE or USER_DATABASE."
  }
}

variable "database_url" {
  description = "Custom database URL (optional)"
  type        = string
  default     = ""
}
