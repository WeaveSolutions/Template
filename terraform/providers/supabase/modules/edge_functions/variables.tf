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
