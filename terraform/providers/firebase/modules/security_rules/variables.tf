# ==============================================================================
# FIREBASE SECURITY RULES MODULE - VARIABLES
# ==============================================================================

variable "project_id" {
  description = "Firebase project ID"
  type        = string
}

# ==============================================================================
# FIRESTORE RULES CONFIGURATION
# ==============================================================================

variable "firestore_rules_file" {
  description = "Path to Firestore security rules file"
  type        = string
  default     = ""
}

# ==============================================================================
# STORAGE RULES CONFIGURATION
# ==============================================================================

variable "storage_rules_file" {
  description = "Path to Storage security rules file"
  type        = string
  default     = ""
}

# ==============================================================================
# DEFAULT RULES CONFIGURATION
# ==============================================================================

variable "create_default_rules" {
  description = "Create default security rules if custom rules files are not provided"
  type        = bool
  default     = true
}
