# Variables for Security Module

variable "project_name" {
  description = "Project name for resource naming"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "aws_region" {
  description = "AWS region"
  type        = string
  default     = "us-east-1"
}

variable "blocked_countries" {
  description = "List of country codes to block"
  type        = list(string)
  default     = []
}

variable "threat_intel_list_url" {
  description = "URL of threat intelligence list"
  type        = string
  default     = ""
}

variable "alarm_sns_topic_arn" {
  description = "SNS topic ARN for security alerts"
  type        = string
  default     = ""
}

variable "enable_secret_rotation" {
  description = "Enable automatic secret rotation"
  type        = bool
  default     = true
}

variable "db_credentials_secret_id" {
  description = "Secrets Manager secret ID for database credentials"
  type        = string
  default     = ""
}

variable "db_credentials_secret_arn" {
  description = "Secrets Manager secret ARN for database credentials"
  type        = string
  default     = ""
}

variable "common_tags" {
  description = "Common tags to apply to resources"
  type        = map(string)
}
