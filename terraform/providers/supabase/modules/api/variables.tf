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
