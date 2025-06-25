# DigitalOcean Production Environment Variables

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
  default     = "10.30.0.0/16"
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

variable "enable_read_replica" {
  description = "Enable read replica for the database"
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

variable "enable_load_balancer" {
  description = "Enable load balancer"
  type        = bool
  default     = false
}

variable "enable_firewall" {
  description = "Enable firewall"
  type        = bool
  default     = true
}

variable "app_platform_instance_count" {
  description = "Number of App Platform instances"
  type        = number
  default     = 3
}

variable "app_platform_instance_size" {
  description = "Size of App Platform instances"
  type        = string
  default     = "basic-s"
}

variable "app_platform_repo_url" {
  description = "Git repository URL for App Platform"
  type        = string
  default     = ""
}

variable "app_platform_branch" {
  description = "Git branch for App Platform deployment"
  type        = string
  default     = "main"
}

variable "database_cluster_size" {
  description = "Size of the database cluster"
  type        = string
  default     = "db-s-2vcpu-4gb"
}

variable "database_node_count" {
  description = "Number of database nodes"
  type        = number
  default     = 2
}

variable "replica_region" {
  description = "Region for database read replica"
  type        = string
  default     = "sfo3"
}

variable "replica_size" {
  description = "Size of the database read replica"
  type        = string
  default     = "db-s-1vcpu-2gb"
}

variable "load_balancer_size_unit" {
  description = "Size unit for the load balancer"
  type        = number
  default     = 1
}

variable "ssl_certificate_name" {
  description = "Name of the SSL certificate"
  type        = string
  default     = ""
}

variable "cdn_custom_domain" {
  description = "Custom domain for CDN"
  type        = string
  default     = ""
}

variable "ssh_allowed_ips" {
  description = "List of IP addresses allowed for SSH access"
  type        = list(string)
  default     = []
}

variable "domain_name" {
  description = "Domain name for the application"
  type        = string
  default     = ""
}

variable "subdomain" {
  description = "Subdomain for the production environment"
  type        = string
  default     = ""
}
