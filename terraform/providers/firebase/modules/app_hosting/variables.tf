# ==============================================================================
# FIREBASE APP HOSTING MODULE - VARIABLES
# ==============================================================================

variable "project_id" {
  description = "Firebase project ID"
  type        = string
}

variable "backend_id" {
  description = "App Hosting backend ID"
  type        = string
  default     = "nexpo-backend"
}

variable "location" {
  description = "Location for App Hosting backend"
  type        = string
  default     = "us-central1"
}

# ==============================================================================
# GITHUB REPOSITORY CONFIGURATION
# ==============================================================================

variable "github_repository_url" {
  description = "GitHub repository URL for the application"
  type        = string
}

variable "github_branch" {
  description = "GitHub branch for continuous deployment"
  type        = string
  default     = "main"
}

variable "root_directory" {
  description = "Root directory in the repository"
  type        = string
  default     = "/"
}

# ==============================================================================
# BUILD CONFIGURATION
# ==============================================================================

variable "create_initial_build" {
  description = "Create an initial build for the backend"
  type        = bool
  default     = true
}
