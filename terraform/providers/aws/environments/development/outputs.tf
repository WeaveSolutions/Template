# Outputs for Development Environment

output "vpc_id" {
  description = "ID of the VPC"
  value       = module.networking.vpc_id
}

output "app_url" {
  description = "URL of the application"
  value       = "https://${module.cdn.cloudfront_distribution_domain_name}"
}

output "app_runner_service_url" {
  description = "Direct App Runner service URL"
  value       = module.compute.app_runner_service_url
}

output "database_endpoint" {
  description = "RDS instance endpoint"
  value       = module.database.db_instance_endpoint
  sensitive   = true
}

output "cloudfront_distribution_id" {
  description = "CloudFront distribution ID"
  value       = module.cdn.cloudfront_distribution_id
}

output "ecr_repository_url" {
  description = "ECR repository URL"
  value       = module.compute.ecr_repository_url
}

output "assets_bucket_name" {
  description = "Name of the assets S3 bucket"
  value       = module.storage.assets_bucket_id
}

output "monitoring_dashboard_url" {
  description = "CloudWatch dashboard URL"
  value       = module.monitoring.dashboard_url
}

output "alarm_topic_arn" {
  description = "SNS topic ARN for alarms"
  value       = module.monitoring.alarm_topic_arn
}
