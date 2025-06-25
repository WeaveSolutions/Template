variable "resource_group_id" {
  description = "ID of the resource group where resources will be created"
  type        = string
}

variable "region" {
  description = "IBM Cloud region where resources will be created"
  type        = string
  default     = "us-south"
}

variable "tags" {
  description = "Tags to apply to resources created by this module"
  type        = list(string)
  default     = []
}

# VPC Variables
variable "vpc_name" {
  description = "Name of the VPC to create"
  type        = string
}

variable "address_prefix_management" {
  description = "Indicates whether a default address prefix should be created for each zone (auto) or if prefixes will be manually created (manual)"
  type        = string
  default     = "auto"
  
  validation {
    condition     = contains(["auto", "manual"], var.address_prefix_management)
    error_message = "The address_prefix_management value must be either 'auto' or 'manual'."
  }
}

variable "classic_access" {
  description = "Enable VPC access to classic infrastructure network"
  type        = bool
  default     = false
}

variable "default_network_acl_name" {
  description = "Name of the default network ACL"
  type        = string
  default     = null
}

variable "default_security_group_name" {
  description = "Name of the default security group"
  type        = string
  default     = null
}

variable "default_routing_table_name" {
  description = "Name of the default routing table"
  type        = string
  default     = null
}

variable "vpc_zones" {
  description = "List of zones within the region where VPC resources will be created"
  type        = list(string)
  default     = ["us-south-1", "us-south-2", "us-south-3"]
}

variable "public_gateway_zones" {
  description = "List of zones where public gateways should be created"
  type        = list(string)
  default     = []
}

# Subnet Variables
variable "vpc_subnets" {
  description = "List of subnets to create in the VPC"
  type = list(object({
    name           = string
    cidr           = string
    zone           = string
    public_gateway = optional(bool, false)
    network_acl    = optional(string)
  }))
  default = []
}

# Network ACL Variables
variable "vpc_acls" {
  description = "Network ACLs to create in the VPC"
  type = list(object({
    name  = string
    rules = list(object({
      name        = string
      action      = string
      direction   = string
      source      = string
      destination = string
      tcp = optional(object({
        port_min = optional(number)
        port_max = optional(number)
        source_port_min = optional(number)
        source_port_max = optional(number)
      }))
      udp = optional(object({
        port_min = optional(number)
        port_max = optional(number)
        source_port_min = optional(number)
        source_port_max = optional(number)
      }))
      icmp = optional(object({
        type = optional(number)
        code = optional(number)
      }))
    }))
  }))
  default = []
}

# Security Group Variables
variable "vpc_security_groups" {
  description = "Security groups to create in the VPC"
  type = list(object({
    name  = string
    rules = list(object({
      name      = string
      direction = string
      remote    = string
      tcp = optional(object({
        port_min = optional(number)
        port_max = optional(number)
      }))
      udp = optional(object({
        port_min = optional(number)
        port_max = optional(number)
      }))
      icmp = optional(object({
        type = optional(number)
        code = optional(number)
      }))
    }))
  }))
  default = []
}

# Load Balancer Variables
variable "load_balancers" {
  description = "Load balancers to create in the VPC"
  type = list(object({
    name = string
    type = optional(string, "public")  # public or private
    subnet_names = list(string)
    security_group_names = optional(list(string))
    pools = optional(list(object({
      name = string
      algorithm = optional(string, "round_robin")  # round_robin, weighted_round_robin, least_connections
      protocol = string  # http, https, tcp, udp
      health_delay = optional(number, 5)
      health_retries = optional(number, 2)
      health_timeout = optional(number, 2)
      health_type = optional(string, "http")  # http, https, tcp
      health_monitor_url = optional(string, "/")
      health_monitor_port = optional(number)
    })), [])
    listeners = optional(list(object({
      port = number
      protocol = string  # http, https, tcp, udp
      default_pool_name = optional(string)
      certificate_instance = optional(string)
      connection_limit = optional(number)
    })), [])
  }))
  default = []
}

# Transit Gateway Variables
variable "create_transit_gateway" {
  description = "Whether to create a Transit Gateway"
  type        = bool
  default     = false
}

variable "transit_gateway_global" {
  description = "Whether the Transit Gateway should be global (connecting across regions)"
  type        = bool
  default     = false
}
