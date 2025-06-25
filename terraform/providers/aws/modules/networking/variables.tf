# Variables for Networking Module

variable "project_name" {
  description = "Project name for resource naming"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "vpc_cidr" {
  description = "CIDR block for VPC"
  type        = string
  default     = "10.0.0.0/16"
}

variable "availability_zones" {
  description = "List of availability zones"
  type        = list(string)
}

variable "enable_nat_gateway" {
  description = "Enable NAT Gateway for private subnets"
  type        = bool
  default     = true
}

variable "app_port" {
  description = "Port for application traffic"
  type        = number
  default     = 3000
}

variable "common_tags" {
  description = "Common tags to apply to resources"
  type        = map(string)
}

variable "enable_flow_logs" {
  description = "Enable VPC Flow Logs"
  type        = bool
  default     = true
}

variable "flow_log_retention_days" {
  description = "Flow log retention in days"
  type        = number
  default     = 30
}

variable "kms_key_id" {
  description = "KMS key ID for encrypting flow logs"
  type        = string
  default     = null
}

variable "enable_network_acls" {
  description = "Enable custom network ACLs"
  type        = bool
  default     = false
}
