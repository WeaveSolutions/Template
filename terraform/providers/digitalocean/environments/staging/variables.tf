# DigitalOcean Staging Environment Variables

variable "digitalocean_access_token" {
  description = "DigitalOcean API access token"
  type        = string
  sensitive   = true
}

variable "digitalocean_region" {
  description = "DigitalOcean region for resource deployment"
  type        = string
  default     = "nyc3"
}

variable "digitalocean_vpc_cidr" {
  description = "CIDR block for the VPC"
  type        = string
  default     = "10.20.0.0/16"
}

variable "enable_vpc" {
  description = "Enable VPC creation"
  type        = bool
  default     = true
}

variable "enable_app_platform" {
  description = "Enable DigitalOcean App Platform deployment"
  type        = bool
  default     = true
}

variable "enable_managed_database" {
  description = "Enable managed database (PostgreSQL)"
  type        = bool
  default     = true
}

variable "enable_spaces" {
  description = "Enable Spaces object storage"
  type        = bool
  default     = true
}

variable "enable_cdn" {
  description = "Enable CDN for Spaces"
  type        = bool
  default     = true
}

variable "app_platform_instance_count" {
  description = "Number of App Platform instances"
  type        = number
  default     = 2
}

variable "app_platform_instance_size" {
  description = "Size of App Platform instances"
  type        = string
  default     = "basic-xs"
}

variable "app_platform_repo_url" {
  description = "Git repository URL for App Platform"
  type        = string
  default     = ""
}

variable "app_platform_branch" {
  description = "Git branch for App Platform deployment"
  type        = string
  default     = "staging"
}

variable "database_cluster_size" {
  description = "Size of the database cluster"
  type        = string
  default     = "db-s-1vcpu-2gb"
}

variable "domain_name" {
  description = "Domain name for the application"
  type        = string
  default     = ""
}

variable "subdomain" {
  description = "Subdomain for the staging environment"
  type        = string
  default     = "staging"
}
