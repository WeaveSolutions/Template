# Variables for Database Module

variable "project_name" {
  description = "Project name for resource naming"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "database_subnet_ids" {
  description = "Subnet IDs for RDS subnet group"
  type        = list(string)
}

variable "security_group_ids" {
  description = "Security group IDs for RDS"
  type        = list(string)
}

variable "db_name" {
  description = "Name of the database"
  type        = string
  default     = "app"
}

variable "db_username" {
  description = "Master username for the database"
  type        = string
  default     = "dbadmin"
}

variable "db_family" {
  description = "DB parameter group family"
  type        = string
  default     = "postgres15"
}

variable "db_engine_version" {
  description = "PostgreSQL engine version"
  type        = string
  default     = "15.4"
}

variable "db_instance_class" {
  description = "RDS instance class"
  type        = string
  default     = "db.t3.micro"
}

variable "db_allocated_storage" {
  description = "Allocated storage in GB"
  type        = number
  default     = 20
}

variable "db_max_allocated_storage" {
  description = "Maximum allocated storage for autoscaling in GB"
  type        = number
  default     = 100
}

variable "backup_retention_period" {
  description = "Backup retention period in days"
  type        = number
  default     = 7
}

variable "backup_window" {
  description = "Preferred backup window"
  type        = string
  default     = "03:00-04:00"
}

variable "maintenance_window" {
  description = "Preferred maintenance window"
  type        = string
  default     = "sun:04:00-sun:05:00"
}

variable "enable_deletion_protection" {
  description = "Enable deletion protection"
  type        = bool
  default     = false
}

variable "enable_performance_insights" {
  description = "Enable Performance Insights"
  type        = bool
  default     = false
}

variable "enable_enhanced_monitoring" {
  description = "Enable enhanced monitoring"
  type        = bool
  default     = false
}

variable "enable_read_replica" {
  description = "Enable read replica (production only)"
  type        = bool
  default     = false
}

variable "db_read_replica_instance_class" {
  description = "Instance class for read replica"
  type        = string
  default     = "db.t3.small"
}

variable "kms_key_id" {
  description = "KMS key ID for encryption"
  type        = string
  default     = null
}

variable "alarm_sns_topic_arns" {
  description = "SNS topic ARNs for alarms"
  type        = list(string)
  default     = []
}

variable "db_cpu_threshold" {
  description = "CPU threshold for alarm (%)"
  type        = number
  default     = 80
}

variable "db_storage_threshold" {
  description = "Free storage threshold for alarm (GB)"
  type        = number
  default     = 5
}

variable "db_connections_threshold" {
  description = "Connection count threshold for alarm"
  type        = number
  default     = 80
}

variable "common_tags" {
  description = "Common tags to apply to resources"
  type        = map(string)
}
