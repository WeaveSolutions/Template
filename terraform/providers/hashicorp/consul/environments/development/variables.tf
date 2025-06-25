# HashiCorp Consul Development Environment Variables

variable "consul_address" {
  description = "Consul server address"
  type        = string
  default     = "http://localhost:8500"
}

variable "consul_token" {
  description = "Consul authentication token"
  type        = string
  sensitive   = true
  default     = ""
}

variable "consul_datacenter" {
  description = "Consul datacenter name"
  type        = string
  default     = "dc1"
}

variable "consul_node_name" {
  description = "Consul node name for service registration"
  type        = string
  default     = "development-node"
}

variable "enable_nextjs_service" {
  description = "Enable Next.js service registration"
  type        = bool
  default     = true
}

variable "enable_api_service" {
  description = "Enable API service registration"
  type        = bool
  default     = true
}

variable "enable_database_service" {
  description = "Enable database service registration"
  type        = bool
  default     = true
}

variable "enable_redis_service" {
  description = "Enable Redis service registration"
  type        = bool
  default     = false
}

variable "enable_service_mesh" {
  description = "Enable Consul Connect service mesh"
  type        = bool
  default     = false
}

variable "nextjs_host" {
  description = "Next.js application host"
  type        = string
  default     = "localhost"
}

variable "nextjs_port" {
  description = "Next.js application port"
  type        = number
  default     = 3000
}

variable "nextjs_version" {
  description = "Next.js application version"
  type        = string
  default     = "development"
}

variable "api_host" {
  description = "API service host"
  type        = string
  default     = "localhost"
}

variable "api_port" {
  description = "API service port"
  type        = number
  default     = 8000
}

variable "api_version" {
  description = "API service version"
  type        = string
  default     = "development"
}

variable "database_host" {
  description = "Database host"
  type        = string
  default     = "localhost"
}

variable "database_port" {
  description = "Database port"
  type        = number
  default     = 5432
}

variable "database_name" {
  description = "Database name"
  type        = string
  default     = "nextjs_dev"
}

variable "database_version" {
  description = "Database version"
  type        = string
  default     = "15"
}

variable "redis_host" {
  description = "Redis host"
  type        = string
  default     = "localhost"
}

variable "redis_port" {
  description = "Redis port"
  type        = number
  default     = 6379
}

variable "redis_version" {
  description = "Redis version"
  type        = string
  default     = "7"
}

variable "log_level" {
  description = "Application log level"
  type        = string
  default     = "debug"
  
  validation {
    condition     = contains(["debug", "info", "warn", "error"], var.log_level)
    error_message = "Log level must be one of: debug, info, warn, error."
  }
}
