# Heroku Development Environment Variables

variable "heroku_email" {
  description = "Email address associated with Heroku account"
  type        = string
}

variable "heroku_api_key" {
  description = "Heroku API key for authentication"
  type        = string
  sensitive   = true
}

variable "heroku_region" {
  description = "Heroku region for application deployment"
  type        = string
  default     = "us"
  
  validation {
    condition     = contains(["us", "eu"], var.heroku_region)
    error_message = "Heroku region must be either 'us' or 'eu'."
  }
}

variable "heroku_stack" {
  description = "Heroku stack for application runtime"
  type        = string
  default     = "heroku-22"
  
  validation {
    condition     = contains(["heroku-22", "heroku-20"], var.heroku_stack)
    error_message = "Heroku stack must be 'heroku-22' or 'heroku-20'."
  }
}

variable "heroku_team" {
  description = "Heroku team/organization name"
  type        = string
  default     = ""
}

variable "enable_api_app" {
  description = "Enable separate API application"
  type        = bool
  default     = true
}

variable "enable_redis" {
  description = "Enable Redis add-on for caching"
  type        = bool
  default     = false
}

variable "enable_logging" {
  description = "Enable Papertrail add-on for logging"
  type        = bool
  default     = false
}

variable "database_plan" {
  description = "Heroku PostgreSQL add-on plan"
  type        = string
  default     = "heroku-postgresql:mini"
  
  validation {
    condition = contains([
      "heroku-postgresql:mini",
      "heroku-postgresql:basic",
      "heroku-postgresql:standard-0"
    ], var.database_plan)
    error_message = "Database plan must be a valid Heroku PostgreSQL plan."
  }
}

variable "redis_plan" {
  description = "Heroku Redis add-on plan"
  type        = string
  default     = "heroku-redis:mini"
  
  validation {
    condition = contains([
      "heroku-redis:mini",
      "heroku-redis:premium-0",
      "heroku-redis:premium-1"
    ], var.redis_plan)
    error_message = "Redis plan must be a valid Heroku Redis plan."
  }
}

variable "logging_plan" {
  description = "Papertrail logging add-on plan"
  type        = string
  default     = "papertrail:choklad"
}

variable "custom_domain" {
  description = "Custom domain for the application (optional)"
  type        = string
  default     = ""
}

variable "jwt_secret" {
  description = "JWT secret for authentication"
  type        = string
  sensitive   = true
  default     = "dev-jwt-secret-change-in-production"
}

variable "api_key" {
  description = "API key for external service authentication"
  type        = string
  sensitive   = true
  default     = "dev-api-key-change-in-production"
}
