# Outputs for Staging Environment

output "vpc_id" {
  description = "VPC ID"
  value       = module.networking.vpc_id
}

output "app_url" {
  description = "Application URL"
  value       = var.custom_domain != "" ? "https://${var.custom_domain}" : "https://${module.cdn.cloudfront_distribution_domain_name}"
}

output "app_runner_service_url" {
  description = "App Runner service URL"
  value       = module.compute.app_runner_service_url
}

output "cloudfront_distribution_id" {
  description = "CloudFront distribution ID"
  value       = module.cdn.cloudfront_distribution_id
}

output "cloudfront_distribution_domain_name" {
  description = "CloudFront distribution domain name"
  value       = module.cdn.cloudfront_distribution_domain_name
}

output "database_endpoint" {
  description = "RDS cluster endpoint"
  value       = module.database.database_endpoint
  sensitive   = true
}

output "database_read_endpoint" {
  description = "RDS cluster reader endpoint"
  value       = module.database.database_read_endpoint
  sensitive   = true
}

output "assets_bucket_name" {
  description = "Assets bucket name"
  value       = module.storage.assets_bucket_id
}

output "uploads_bucket_name" {
  description = "Uploads bucket name"
  value       = module.storage.uploads_bucket_id
}

output "ecr_repository_url" {
  description = "ECR repository URL"
  value       = module.compute.ecr_repository_url
}

output "alarm_topic_arn" {
  description = "SNS topic ARN for alarms"
  value       = module.monitoring.alarm_topic_arn
}
