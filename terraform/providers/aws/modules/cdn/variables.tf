# Variables for CDN Module

variable "project_name" {
  description = "Project name for resource naming"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "assets_bucket_id" {
  description = "ID of the S3 bucket for static assets"
  type        = string
}

variable "assets_bucket_regional_domain_name" {
  description = "Regional domain name of the assets bucket"
  type        = string
}

variable "app_domain" {
  description = "Domain name of the application origin"
  type        = string
}

variable "custom_domain" {
  description = "Custom domain name for the distribution"
  type        = string
  default     = null
}

variable "acm_certificate_arn" {
  description = "ACM certificate ARN for custom domain"
  type        = string
  default     = null
}

variable "price_class" {
  description = "CloudFront price class"
  type        = string
  default     = "PriceClass_100"
}

variable "geo_restriction_type" {
  description = "Geo restriction type (whitelist or blacklist)"
  type        = string
  default     = "none"
}

variable "geo_restriction_locations" {
  description = "List of country codes for geo restriction"
  type        = list(string)
  default     = []
}

variable "waf_acl_id" {
  description = "WAF ACL ID to associate"
  type        = string
  default     = null
}

variable "logs_bucket_domain_name" {
  description = "Domain name of the logs bucket"
  type        = string
}

variable "edge_lambda_arn" {
  description = "ARN of Lambda@Edge function"
  type        = string
  default     = null
}

variable "error_rate_threshold" {
  description = "Error rate threshold for alarms (%)"
  type        = number
  default     = 5
}

variable "alarm_sns_topic_arns" {
  description = "SNS topic ARNs for alarms"
  type        = list(string)
  default     = []
}

variable "common_tags" {
  description = "Common tags to apply to resources"
  type        = map(string)
}
