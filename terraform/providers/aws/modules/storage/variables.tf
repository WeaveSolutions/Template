# Variables for Storage Module

variable "project_name" {
  description = "Project name for resource naming"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "cors_allowed_origins" {
  description = "CORS allowed origins for uploads bucket"
  type        = list(string)
  default     = ["*"]
}

variable "backup_retention_days" {
  description = "Number of days to retain backups"
  type        = number
  default     = 365
}

variable "kms_deletion_window" {
  description = "KMS key deletion window in days"
  type        = number
  default     = 30
}

variable "cloudfront_oai_arn" {
  description = "CloudFront Origin Access Identity ARN"
  type        = string
  default     = ""
}

variable "upload_processor_lambda_arn" {
  description = "ARN of Lambda function to process uploads"
  type        = string
  default     = ""
}

variable "common_tags" {
  description = "Common tags to apply to resources"
  type        = map(string)
}
