# Variables for Monitoring Module

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
}

variable "alarm_email_endpoints" {
  description = "Email addresses for alarm notifications"
  type        = list(string)
  default     = []
}

variable "alarm_sms_endpoints" {
  description = "Phone numbers for SMS alarm notifications"
  type        = list(string)
  default     = []
}

variable "log_retention_days" {
  description = "CloudWatch log retention in days"
  type        = number
  default     = 30
}

variable "kms_key_arn" {
  description = "KMS key ARN for log encryption"
  type        = string
  default     = null
}

variable "app_runner_service_name" {
  description = "Name of the App Runner service"
  type        = string
}

variable "db_instance_id" {
  description = "RDS instance identifier"
  type        = string
}

variable "cloudfront_distribution_id" {
  description = "CloudFront distribution ID"
  type        = string
}

variable "synthetics_bucket_name" {
  description = "S3 bucket for Synthetics artifacts"
  type        = string
}

variable "app_url" {
  description = "Application URL for health checks"
  type        = string
}

variable "canary_subnet_ids" {
  description = "Subnet IDs for Synthetics canary"
  type        = list(string)
  default     = []
}

variable "canary_security_group_ids" {
  description = "Security group IDs for Synthetics canary"
  type        = list(string)
  default     = []
}

variable "enable_ops_center" {
  description = "Enable Systems Manager OpsCenter"
  type        = bool
  default     = false
}

variable "common_tags" {
  description = "Common tags to apply to resources"
  type        = map(string)
}
