variable "enable_db_config" {
  description = "Whether to configure database settings"
  type        = bool
  default     = true
}

variable "dev_project_ref" {
  description = "Development project reference"
  type        = string
}

variable "staging_project_ref" {
  description = "Staging project reference"
  type        = string
}

variable "prod_project_ref" {
  description = "Production project reference"
  type        = string
}

variable "db_settings" {
  description = "Database configuration settings"
  type = object({
    db_schema            = optional(string, "public,storage,graphql_public")
    db_extra_search_path = optional(string, "public,extensions")
    max_rows             = optional(number, 1000)
  })
  default = {}
}
