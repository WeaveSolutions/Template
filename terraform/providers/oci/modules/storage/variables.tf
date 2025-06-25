variable "compartment_ocid" {
  description = "OCI Compartment OCID"
  type        = string
}

variable "region" {
  description = "OCI region"
  type        = string
}

variable "project_name" {
  description = "Project name"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "buckets" {
  description = "Object storage bucket configurations"
  type = map(object({
    name          = string
    public_access = string
    versioning    = bool
  }))
  default = {}
}

variable "availability_domain" {
  description = "Availability domain for block volumes"
  type        = string
}

variable "subnet_id" {
  description = "Subnet ID for file storage mount target"
  type        = string
}

variable "subnet_cidr" {
  description = "Subnet CIDR for file storage access"
  type        = string
}

variable "nsg_ids" {
  description = "Network security group IDs"
  type        = list(string)
  default     = []
}

variable "block_volume_size" {
  description = "Size of block volume in GB"
  type        = number
  default     = 50
}

variable "block_volume_performance" {
  description = "VPUs per GB for block volume (10=Balanced, 20=Higher Performance)"
  type        = number
  default     = 10
}

variable "allowed_origins" {
  description = "Allowed origins for CORS"
  type        = string
  default     = "*"
}

variable "tags" {
  description = "Freeform tags"
  type        = map(string)
  default     = {}
}
