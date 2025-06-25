variable "ibmcloud_api_key" {
  description = "IBM Cloud API key for authentication"
  type        = string
  sensitive   = true
}

variable "region" {
  description = "IBM Cloud region to deploy resources"
  type        = string
  default     = "us-south"
}

variable "zone" {
  description = "Availability zone within the region"
  type        = string
  default     = "us-south-1"
}

variable "project_name" {
  description = "Name of the project"
  type        = string
  validation {
    condition     = can(regex("^[a-z0-9-]+$", var.project_name))
    error_message = "Project name must be lowercase alphanumeric with hyphens only."
  }
}

variable "environment" {
  description = "Environment name (development, staging, production)"
  type        = string
  validation {
    condition     = contains(["development", "staging", "production"], var.environment)
    error_message = "Environment must be development, staging, or production."
  }
}

variable "tags" {
  description = "Tags to apply to resources created by this module"
  type        = list(string)
  default     = []
}

# VPC Network Variables
variable "vpc_name" {
  description = "Name of the VPC"
  type        = string
  default     = "ibm-vpc"
}

variable "vpc_subnets" {
  description = "List of subnets to create in the VPC"
  type = list(object({
    name           = string
    cidr           = string
    zone           = string
    public_gateway = optional(bool, false)
  }))
  default = []
}

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

# Compute Variables
variable "instances" {
  description = "Virtual server instances to create"
  type = list(object({
    name       = string
    profile    = string
    image      = string
    zone       = string
    subnet_id  = optional(string)
    vpc_id     = optional(string)
    security_groups = optional(list(string), [])
    user_data  = optional(string)
    boot_volume = optional(object({
      name     = optional(string)
      size     = optional(number)
      profile  = optional(string)
      encryption_key = optional(string)
    }))
    data_volumes = optional(list(object({
      name     = string
      size     = number
      profile  = optional(string)
      encryption_key = optional(string)
    })), [])
  }))
  default = []
}

variable "ssh_keys" {
  description = "SSH keys to inject into virtual server instances"
  type = list(object({
    name       = string
    public_key = string
  }))
  default = []
}

# Kubernetes Variables
variable "enable_kubernetes" {
  description = "Enable IBM Kubernetes Service"
  type        = bool
  default     = false
}

variable "kubernetes_cluster_name" {
  description = "Name of the Kubernetes cluster"
  type        = string
  default     = "ibm-k8s-cluster"
}

variable "kubernetes_worker_count" {
  description = "Number of worker nodes per zone"
  type        = number
  default     = 1
}

variable "kubernetes_machine_type" {
  description = "Machine type for worker nodes"
  type        = string
  default     = "bx2.4x16"
}

variable "kubernetes_version" {
  description = "Kubernetes version"
  type        = string
  default     = "1.26"
}

variable "kubernetes_flavor" {
  description = "Cluster flavor (kubernetes or openshift)"
  type        = string
  default     = "kubernetes"
  validation {
    condition     = contains(["kubernetes", "openshift"], var.kubernetes_flavor)
    error_message = "Cluster flavor must be kubernetes or openshift."
  }
}

# Storage Variables
variable "cos_instances" {
  description = "Cloud Object Storage instances to create"
  type = list(object({
    name     = string
    plan     = string
    buckets  = list(object({
      name          = string
      storage_class = optional(string, "standard")
      endpoint_type = optional(string, "public")
      force_delete  = optional(bool, false)
      retention_days = optional(number)
    }))
  }))
  default = []
}

variable "block_volumes" {
  description = "Block storage volumes to create"
  type = list(object({
    name           = string
    profile        = string
    capacity       = number
    iops           = optional(number)
    encryption_key = optional(string)
    zone           = string
  }))
  default = []
}

variable "file_shares" {
  description = "File storage shares to create"
  type = list(object({
    name           = string
    size           = number
    iops           = optional(number)
    profile        = string
    zone           = string
    encryption_key = optional(string)
  }))
  default = []
}

# Database Variables
variable "enable_databases" {
  description = "Enable database services"
  type        = bool
  default     = false
}

variable "db_instances" {
  description = "Database instances to create (PostgreSQL, MySQL, MongoDB, etc.)"
  type = list(object({
    name     = string
    type     = string
    plan     = string
    version  = optional(string)
    adminuser = optional(string)
    adminpassword = optional(string)
    auto_scaling = optional(object({
      disk = optional(object({
        capacity_enabled = optional(bool, false)
        free_space_less_than_percent = optional(number, 10)
        io_above_percent = optional(number, 90)
        io_enabled = optional(bool, false)
        io_over_period = optional(string, "15m")
        rate_increase_percent = optional(number, 10)
        rate_limit_mb_per_member = optional(number, 3670016)
        rate_period_seconds = optional(number, 900)
        rate_units = optional(string, "mb")
      }))
      memory = optional(object({
        io_above_percent = optional(number, 90)
        io_enabled = optional(bool, false)
        io_over_period = optional(string, "15m")
        rate_increase_percent = optional(number, 10)
        rate_limit_mb_per_member = optional(number, 114688)
        rate_period_seconds = optional(number, 900)
        rate_units = optional(string, "mb")
      }))
    }))
  }))
  default = []
}

# Cloudant Variables
variable "enable_cloudant" {
  description = "Enable Cloudant NoSQL database"
  type        = bool
  default     = false
}

variable "cloudant_instances" {
  description = "Cloudant instances to create"
  type = list(object({
    name     = string
    plan     = string
    databases = list(object({
      name         = string
      partitioned  = optional(bool, false)
      capacity     = optional(string, "standard")
      indexes      = optional(list(object({
        name       = string
        index_type = string
        design_doc = string
        fields     = list(map(string))
      })), [])
    }))
  }))
  default = []
}

# Power VS Variables
variable "enable_power_vs" {
  description = "Enable Power Virtual Servers"
  type        = bool
  default     = false
}

variable "power_vs_zone" {
  description = "Power VS zone (different from VPC zones)"
  type        = string
  default     = "dal12"
}

variable "power_vs_instances" {
  description = "Power VS instances to create"
  type = list(object({
    name         = string
    image_name   = string
    processors   = number
    memory       = number
    proc_type    = optional(string, "shared")
    sys_type     = optional(string, "s922")
    network_ids  = list(string)
    ssh_key_name = string
    boot_volume  = optional(object({
      name       = optional(string)
      size       = optional(number)
      type       = optional(string, "tier3")
    }))
    data_volumes = optional(list(object({
      name       = string
      size       = number
      type       = optional(string, "tier3")
      shareable  = optional(bool, false)
    })), [])
  }))
  default = []
}

variable "power_vs_networks" {
  description = "Power VS networks to create"
  type = list(object({
    name       = string
    cidr       = string
    dns_servers = optional(list(string))
    gateway    = optional(string)
    jumbo      = optional(bool, false)
  }))
  default = []
}

variable "power_vs_volumes" {
  description = "Power VS volumes to create"
  type = list(object({
    name      = string
    size      = number
    type      = optional(string, "tier3")
    shareable = optional(bool, false)
  }))
  default = []
}

# Security Variables
variable "kms_instances" {
  description = "Key Management Service instances to create"
  type = list(object({
    name      = string
    plan      = string
    keys      = list(object({
      name    = string
      standard_key = optional(bool, false)
      rotation = optional(number)
      dual_auth_delete = optional(bool, false)
    }))
  }))
  default = []
}

variable "certificates" {
  description = "SSL certificates to manage"
  type = list(object({
    name          = string
    domains       = list(string)
    key_algorithm = optional(string, "rsaEncryption 2048 bit")
    auto_renew    = optional(bool, true)
  }))
  default = []
}

variable "security_groups" {
  description = "Security groups to create for services outside VPC"
  type = list(object({
    name      = string
    rules     = list(object({
      name    = string
      direction = string
      remote  = string
      protocol = string
      port_min = optional(number)
      port_max = optional(number)
    }))
  }))
  default = []
}

# Schematics Variables
variable "enable_schematics" {
  description = "Enable IBM Cloud Schematics"
  type        = bool
  default     = false
}

variable "schematics_workspace_name" {
  description = "Name of the Schematics workspace"
  type        = string
  default     = "terraform-workspace"
}

variable "schematics_template_repo_url" {
  description = "Git repository URL containing Terraform templates"
  type        = string
  default     = ""
}

variable "schematics_template_git_branch" {
  description = "Git branch for Terraform templates"
  type        = string
  default     = "main"
}

variable "schematics_template_git_token" {
  description = "Git access token for private repositories"
  type        = string
  default     = ""
  sensitive   = true
}
