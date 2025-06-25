# ==============================================================================
# FIREBASE TERRAFORM PROVIDER - VARIABLES
# ==============================================================================

# ==============================================================================
# PROJECT CONFIGURATION
# ==============================================================================

variable "gcp_project_id" {
  description = "Google Cloud Project ID"
  type        = string
}

variable "project_name" {
  description = "Display name for the project"
  type        = string
  default     = "Nexpo"
}

variable "gcp_region" {
  description = "Default GCP region"
  type        = string
  default     = "us-central1"
}

variable "billing_account_id" {
  description = "Billing account ID for the project"
  type        = string
  default     = ""
}

variable "create_new_project" {
  description = "Whether to create a new Google Cloud project"
  type        = bool
  default     = false
}

# ==============================================================================
# FIREBASE APPLICATIONS
# ==============================================================================

variable "enable_web_app" {
  description = "Enable Firebase Web App"
  type        = bool
  default     = true
}

variable "enable_android_app" {
  description = "Enable Firebase Android App"
  type        = bool
  default     = true
}

variable "enable_ios_app" {
  description = "Enable Firebase iOS App"
  type        = bool
  default     = true
}

variable "android_package_name" {
  description = "Android app package name"
  type        = string
  default     = "com.nexpo.app"
}

variable "ios_bundle_id" {
  description = "iOS app bundle identifier"
  type        = string
  default     = "com.nexpo.app"
}

variable "ios_app_store_id" {
  description = "iOS App Store ID (optional)"
  type        = string
  default     = ""
}

# ==============================================================================
# AUTHENTICATION CONFIGURATION
# ==============================================================================
# Authentication is handled by Auth0, not Firebase Auth.
# Firebase receives custom tokens from CRA service for data access validation.

# No Firebase Auth variables needed - Auth0 integration via CRA service

# ==============================================================================
# FIRESTORE CONFIGURATION
# ==============================================================================

variable "firestore_config" {
  description = "Cloud Firestore configuration"
  type = object({
    location_id         = optional(string, "nam5")
    database_type       = optional(string, "FIRESTORE_NATIVE")
    concurrency_mode    = optional(string, "OPTIMISTIC")
    app_engine_integration_mode = optional(string, "DISABLED")
    
    # Firestore indexes for efficient queries
    indexes = optional(list(object({
      collection = string
      fields = list(object({
        field_path = string
        order      = optional(string, "ASCENDING")
        array_config = optional(string, null)
      }))
      query_scope = optional(string, "COLLECTION")
    })), [])
  })
  default = {}
}

# ==============================================================================
# STORAGE CONFIGURATION
# ==============================================================================

variable "storage_config" {
  description = "Firebase Storage configuration"
  type = object({
    buckets = optional(list(object({
      name     = string
      location = optional(string, "US")
    })), [])
  })
  default = {}
}

# ==============================================================================
# CLOUD FUNCTIONS CONFIGURATION
# ==============================================================================

variable "functions_config" {
  description = "Cloud Functions configuration"
  type = object({
    functions = optional(list(object({
      name         = string
      description  = optional(string, "")
      runtime      = optional(string, "nodejs20")
      entry_point  = optional(string, "main")
      source_bucket = string
      source_object = string
      trigger_type = optional(string, "HTTP")
      environment_variables = optional(map(string), {})
      timeout     = optional(number, 60)
      memory      = optional(number, 256)
    })), [])
  })
  default = {}
}

# ==============================================================================
# REALTIME DATABASE CONFIGURATION
# ==============================================================================

variable "enable_realtime_database" {
  description = "Enable Firebase Realtime Database"
  type        = bool
  default     = false
}

variable "rtdb_config" {
  description = "Firebase Realtime Database configuration"
  type = object({
    database_url = optional(string, "")
    region       = optional(string, "us-central1")
  })
  default = {}
}

# ==============================================================================
# SECURITY RULES CONFIGURATION
# ==============================================================================

variable "security_rules_config" {
  description = "Firebase Security Rules configuration"
  type = object({
    firestore_rules_file = optional(string, "")
    storage_rules_file   = optional(string, "")
  })
  default = {}
}

# ==============================================================================
# APP HOSTING CONFIGURATION
# ==============================================================================

variable "enable_app_hosting" {
  description = "Enable Firebase App Hosting"
  type        = bool
  default     = false
}

variable "app_hosting_config" {
  description = "Firebase App Hosting configuration"
  type = object({
    github_repository_url = optional(string, "")
    github_branch        = optional(string, "main")
    root_directory       = optional(string, "/")
  })
  default = {}
}
