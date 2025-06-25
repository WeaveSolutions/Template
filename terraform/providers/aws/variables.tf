variable "aws_region" {
  description = "AWS region to deploy resources"
  type        = string
  default     = "us-east-1"
}

variable "environment" {
  description = "Environment name (e.g., development, staging, production)"
  type        = string
  default     = "development"
}

variable "project_name" {
  description = "Name of the project"
  type        = string
  default     = "Nexpo"
}

variable "enable_aws" {
  description = "Enable AWS resources"
  type        = bool
  default     = true
}

variable "custom_domains" {
  description = "List of custom domains for the CloudFront distribution"
  type        = list(string)
  default     = []
}

variable "acm_certificate_arn" {
  description = "ARN of the ACM certificate for the custom domain"
  type        = string
  default     = ""
}

variable "route53_zone_id" {
  description = "Route 53 hosted zone ID for DNS records"
  type        = string
  default     = ""
}

variable "tags" {
  description = "A map of tags to add to all resources"
  type        = map(string)
  default     = {}
}
