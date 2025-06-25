# ==============================================================================
# SUPABASE PROVIDER CONFIGURATION
# ==============================================================================

variable "supabase_access_token" {
  description = "Supabase Management API access token"
  type        = string
  sensitive   = true
}

# ==============================================================================
# ORGANIZATION CONFIGURATION
# ==============================================================================

variable "manage_organization" {
  description = "Whether to create and manage a Supabase organization"
  type        = bool
  default     = false
}

variable "organization_name" {
  description = "Name of the Supabase organization"
  type        = string
  default     = ""
}

variable "existing_organization_id" {
  description = "ID of existing Supabase organization (if not creating new one)"
  type        = string
  default     = ""
}

# ==============================================================================
# PROJECT CONFIGURATION
# ==============================================================================

variable "project_name" {
  description = "Base name for Supabase projects"
  type        = string
  default     = "nexpo"
}

variable "supabase_region" {
  description = "Supabase region for projects"
  type        = string
  default     = "us-east-1"
  validation {
    condition = contains([
      "us-east-1", "us-west-1", "ap-southeast-1", "ap-northeast-1", 
      "eu-west-1", "eu-central-1", "ap-south-1"
    ], var.supabase_region)
    error_message = "Supabase region must be a valid region."
  }
}

# Project creation flags
variable "create_development_project" {
  description = "Whether to create a development Supabase project"
  type        = bool
  default     = true
}

variable "create_staging_project" {
  description = "Whether to create a staging Supabase project"
  type        = bool
  default     = true
}

variable "create_production_project" {
  description = "Whether to create a production Supabase project"
  type        = bool
  default     = true
}

# Database passwords
variable "dev_database_password" {
  description = "Database password for development project"
  type        = string
  sensitive   = true
}

variable "staging_database_password" {
  description = "Database password for staging project"
  type        = string
  sensitive   = true
}

variable "prod_database_password" {
  description = "Database password for production project"
  type        = string
  sensitive   = true
}

# Existing project references (if not creating new ones)
variable "existing_dev_project_ref" {
  description = "Reference to existing development project"
  type        = string
  default     = ""
}

variable "existing_staging_project_ref" {
  description = "Reference to existing staging project"
  type        = string
  default     = ""
}

variable "existing_prod_project_ref" {
  description = "Reference to existing production project"
  type        = string
  default     = ""
}

# ==============================================================================
# AUTHENTICATION INTEGRATION
# ==============================================================================
# Authentication is handled by Auth0, not Supabase Auth.
# Supabase provides PostgreSQL database and storage as data services.
# Row Level Security (RLS) policies will validate Auth0 JWT claims via API keys.

# No Supabase Auth variables needed - Auth0 integration via API endpoints

# ==============================================================================
# DATABASE CONFIGURATION
# ==============================================================================

variable "enable_db_config" {
  description = "Whether to configure database settings"
  type        = bool
  default     = true
}

variable "db_settings" {
  description = "Database configuration settings"
  type = object({
    db_schema            = optional(string, "public,storage,graphql_public")
    db_extra_search_path = optional(string, "public,extensions")
    max_rows             = optional(number, 1000)
    db_anon_role         = optional(string, "anon")
    db_service_role      = optional(string, "service_role")
  })
  default = {}
}

# ==============================================================================
# API CONFIGURATION
# ==============================================================================

variable "api_settings" {
  description = "API configuration settings"
  type = object({
    db_schema            = optional(string, "public,storage,graphql_public")
    db_extra_search_path = optional(string, "public,extensions")
    max_rows             = optional(number, 1000)
    jwt_expiry           = optional(number, 3600)
  })
  default = {}
}

# ==============================================================================
# SECRETS CONFIGURATION
# ==============================================================================

variable "project_secrets" {
  description = "Project secrets to be managed via Terraform"
  type = map(object({
    name  = string
    value = string
  }))
  default   = {}
  sensitive = true
}

# ==============================================================================
# STORAGE CONFIGURATION
# ==============================================================================

variable "storage_buckets" {
  description = "Storage bucket configurations"
  type = map(object({
    name         = string
    public       = optional(bool, false)
    file_size_limit = optional(number, 52428800) # 50MB default
    allowed_mime_types = optional(list(string), [])
  }))
  default = {}
}

# ==============================================================================
# EDGE FUNCTIONS CONFIGURATION
# ==============================================================================

variable "edge_functions" {
  description = "Edge function configurations"
  type = map(object({
    name         = string
    source_code  = string
    import_map   = optional(string, "")
    verify_jwt   = optional(bool, true)
  }))
  default = {}
}

# ==============================================================================
# DOMAIN CONFIGURATION
# ==============================================================================

variable "custom_domains" {
  description = "Custom domain configurations"
  type = map(object({
    domain_name = string
    project_ref = string
  }))
  default = {}
}
