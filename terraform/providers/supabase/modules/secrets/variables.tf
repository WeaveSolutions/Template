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

variable "project_secrets" {
  description = "Project secrets to be managed via Terraform"
  type = map(object({
    name  = string
    value = string
  }))
  default   = {}
  sensitive = true
}
