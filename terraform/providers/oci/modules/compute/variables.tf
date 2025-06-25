variable "compartment_ocid" {
  description = "The OCID of the compartment"
  type        = string
}

variable "project_name" {
  description = "Name of the project"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "vcn_id" {
  description = "VCN OCID"
  type        = string
}

variable "public_subnet_id" {
  description = "Public subnet OCID"
  type        = string
}

variable "private_subnet_id" {
  description = "Private subnet OCID"
  type        = string
}

variable "web_nsg_id" {
  description = "Web NSG OCID"
  type        = string
}

variable "app_nsg_id" {
  description = "App NSG OCID"
  type        = string
}

variable "kubernetes_version" {
  description = "Kubernetes version for OKE"
  type        = string
  default     = "v1.28.2"
}

variable "node_pool_size" {
  description = "Number of nodes in the node pool"
  type        = number
  default     = 3
}

variable "node_shape" {
  description = "Shape of the nodes"
  type        = string
  default     = "VM.Standard.E4.Flex"
}

variable "node_ocpus" {
  description = "Number of OCPUs for nodes"
  type        = number
  default     = 2
}

variable "node_memory_in_gbs" {
  description = "Memory in GBs for nodes"
  type        = number
  default     = 16
}

variable "registry_endpoint" {
  description = "Container registry endpoint"
  type        = string
}

variable "database_url" {
  description = "Database connection URL"
  type        = string
  sensitive   = true
}

# Auth0 Configuration
variable "auth0_domain" {
  description = "Auth0 domain"
  type        = string
}

variable "auth0_client_id" {
  description = "Auth0 client ID"
  type        = string
}

variable "auth0_client_secret" {
  description = "Auth0 client secret"
  type        = string
  sensitive   = true
}

variable "auth0_api_identifier" {
  description = "Auth0 API identifier"
  type        = string
}

variable "tags" {
  description = "Freeform tags"
  type        = map(string)
  default     = {}
}
