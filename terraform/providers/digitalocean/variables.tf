# DigitalOcean Provider Variables

variable "digitalocean_access_token" {
  description = "DigitalOcean API access token"
  type        = string
  sensitive   = true
}

variable "project_name" {
  description = "Name of the project (development, staging, production)"
  type        = string
  default     = "next-solito-expo"
}

variable "environment" {
  description = "Environment name (development, staging, production)"
  type        = string
  validation {
    condition     = contains(["development", "staging", "production"], var.environment)
    error_message = "Environment must be one of: development, staging, production."
  }
}

variable "digitalocean_region" {
  description = "DigitalOcean region for resource deployment"
  type        = string
  default     = "nyc3"
}

variable "digitalocean_vpc_cidr" {
  description = "CIDR block for the VPC"
  type        = string
  default     = "10.0.0.0/16"
}

variable "droplet_size" {
  description = "Size of the Droplet instances"
  type        = string
  default     = "s-1vcpu-1gb"
}

variable "droplet_image" {
  description = "Droplet image to use"
  type        = string
  default     = "ubuntu-22-04-x64"
}

variable "enable_app_platform" {
  description = "Enable DigitalOcean App Platform deployment"
  type        = bool
  default     = false
}

variable "enable_managed_database" {
  description = "Enable managed database (PostgreSQL)"
  type        = bool
  default     = false
}

variable "enable_kubernetes" {
  description = "Enable managed Kubernetes cluster"
  type        = bool
  default     = false
}

variable "enable_spaces" {
  description = "Enable Spaces object storage"
  type        = bool
  default     = false
}

variable "enable_load_balancer" {
  description = "Enable load balancer"
  type        = bool
  default     = false
}

variable "enable_monitoring" {
  description = "Enable monitoring and alerting"
  type        = bool
  default     = true
}

variable "database_cluster_size" {
  description = "Size of the database cluster"
  type        = string
  default     = "db-s-1vcpu-1gb"
}

variable "database_engine" {
  description = "Database engine (pg, mysql, redis, mongodb)"
  type        = string
  default     = "pg"
}

variable "database_version" {
  description = "Database engine version"
  type        = string
  default     = "15"
}

variable "kubernetes_node_count" {
  description = "Number of nodes in the Kubernetes cluster"
  type        = number
  default     = 2
}

variable "kubernetes_node_size" {
  description = "Size of Kubernetes worker nodes"
  type        = string
  default     = "s-2vcpu-2gb"
}

variable "app_platform_instance_count" {
  description = "Number of App Platform instances"
  type        = number
  default     = 1
}

variable "app_platform_instance_size" {
  description = "Size of App Platform instances"
  type        = string
  default     = "basic-xxs"
}

variable "spaces_bucket_name" {
  description = "Name of the Spaces bucket"
  type        = string
  default     = ""
}

variable "enable_cdn" {
  description = "Enable CDN for Spaces"
  type        = bool
  default     = false
}

variable "tags" {
  description = "Tags to apply to all resources"
  type        = map(string)
  default = {
    "Environment" = "development"
    "Project"     = "next-solito-expo"
    "ManagedBy"   = "terraform"
  }
}

variable "firewall_rules" {
  description = "Custom firewall rules"
  type = list(object({
    type                     = string
    protocol                 = string
    port_range              = string
    source_addresses        = list(string)
    source_droplet_ids      = list(number)
    source_load_balancer_uids = list(string)
    source_tags             = list(string)
    destination_addresses   = list(string)
    destination_droplet_ids = list(number)
    destination_load_balancer_uids = list(string)
    destination_tags        = list(string)
  }))
  default = []
}

variable "domain_name" {
  description = "Domain name for the application"
  type        = string
  default     = ""
}

variable "subdomain" {
  description = "Subdomain for the environment"
  type        = string
  default     = ""
}
