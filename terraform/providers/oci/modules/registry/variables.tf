variable "compartment_ocid" {
  description = "OCI Compartment OCID"
  type        = string
}

variable "tenancy_ocid" {
  description = "OCI Tenancy OCID"
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

variable "region" {
  description = "OCI region"
  type        = string
}

variable "enable_functions" {
  description = "Enable function repositories"
  type        = bool
  default     = true
}

variable "tags" {
  description = "Freeform tags"
  type        = map(string)
  default     = {}
}
