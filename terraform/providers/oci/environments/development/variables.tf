# OCI Authentication Variables
variable "tenancy_ocid" {
  description = "OCI Tenancy OCID"
  type        = string
}

variable "user_ocid" {
  description = "OCI User OCID"
  type        = string
}

variable "fingerprint" {
  description = "OCI API Key Fingerprint"
  type        = string
}

variable "private_key_path" {
  description = "Path to OCI API Private Key"
  type        = string
}

variable "region" {
  description = "OCI Region"
  type        = string
}

variable "compartment_ocid" {
  description = "OCI Compartment OCID"
  type        = string
}

# Project Variables
variable "project_name" {
  description = "Name of the project"
  type        = string
  default     = "Nexpo"
}

# Networking Variables
variable "vcn_cidr_block" {
  description = "CIDR block for VCN"
  type        = string
  default     = "10.0.0.0/16"
}

variable "public_subnet_cidr" {
  description = "CIDR block for public subnet"
  type        = string
  default     = "10.0.1.0/24"
}

variable "private_subnet_cidr" {
  description = "CIDR block for private subnet"
  type        = string
  default     = "10.0.2.0/24"
}

variable "database_subnet_cidr" {
  description = "CIDR block for database subnet"
  type        = string
  default     = "10.0.3.0/24"
}

variable "allowed_ssh_cidr_blocks" {
  description = "CIDR blocks allowed for SSH access"
  type        = list(string)
  default     = []
}

# Kubernetes Variables
variable "kubernetes_version" {
  description = "Kubernetes version"
  type        = string
  default     = "v1.28.2"
}

variable "node_pool_size" {
  description = "Initial number of nodes"
  type        = number
  default     = 3
}

variable "node_shape" {
  description = "Shape for nodes"
  type        = string
  default     = "VM.Standard.E4.Flex"
}

variable "node_ocpus" {
  description = "Number of OCPUs per node"
  type        = number
  default     = 2
}

variable "node_memory_in_gbs" {
  description = "Memory per node in GB"
  type        = number
  default     = 16
}

# Database Variables
variable "autonomous_db_name" {
  description = "Autonomous Database name"
  type        = string
  default     = "appdb"
}

variable "autonomous_db_display_name" {
  description = "Autonomous Database display name"
  type        = string
  default     = "Application Database"
}

variable "autonomous_db_version" {
  description = "Autonomous Database version"
  type        = string
  default     = "19c"
}

variable "autonomous_db_cpu_count" {
  description = "Number of CPU cores"
  type        = number
  default     = 1
}

variable "autonomous_db_storage_gb" {
  description = "Storage size in GB"
  type        = number
  default     = 20
}

variable "autonomous_db_license" {
  description = "License model"
  type        = string
  default     = "LICENSE_INCLUDED"
}

variable "database_admin_password" {
  description = "Admin password for database"
  type        = string
  sensitive   = true
}

# Storage Variables
variable "object_storage_buckets" {
  description = "Object storage bucket configurations"
  type = map(object({
    name          = string
    public_access = string
    versioning    = bool
  }))
  default = {
    assets = {
      name          = "assets"
      public_access = "ObjectRead"
      versioning    = true
    }
    backups = {
      name          = "backups"
      public_access = "NoPublicAccess"
      versioning    = false
    }
  }
}

# Load Balancer Variables
variable "load_balancer_shape" {
  description = "Load balancer shape"
  type        = string
  default     = "flexible"
}

variable "load_balancer_min_bandwidth_mbps" {
  description = "Minimum bandwidth in Mbps"
  type        = number
  default     = 10
}

variable "load_balancer_max_bandwidth_mbps" {
  description = "Maximum bandwidth in Mbps"
  type        = number
  default     = 100
}

# Functions Variables
variable "enable_functions" {
  description = "Enable Oracle Functions"
  type        = bool
  default     = true
}

variable "function_shape" {
  description = "Function shape"
  type        = string
  default     = "GENERIC_X86"
}

# Auth0 Variables
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

# Security Variables
variable "enable_vault" {
  description = "Enable OCI Vault"
  type        = bool
  default     = true
}

variable "enable_bastion" {
  description = "Enable Bastion service"
  type        = bool
  default     = false
}

# Monitoring Variables
variable "enable_monitoring" {
  description = "Enable monitoring"
  type        = bool
  default     = true
}

variable "notification_protocol" {
  description = "Notification protocol"
  type        = string
  default     = "EMAIL"
}

variable "notification_endpoint" {
  description = "Notification endpoint"
  type        = string
}

# Tags
variable "common_tags" {
  description = "Common tags for all resources"
  type        = map(string)
  default = {
    project    = "Nexpo"
    managed_by = "terraform"
  }
}
