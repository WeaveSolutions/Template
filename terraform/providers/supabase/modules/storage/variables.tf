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
