# Outputs for Monitoring Module

output "alarm_topic_arn" {
  description = "ARN of the SNS topic for alarms"
  value       = aws_sns_topic.alarms.arn
}

output "alarm_topic_name" {
  description = "Name of the SNS topic for alarms"
  value       = aws_sns_topic.alarms.name
}

output "app_log_group_name" {
  description = "Name of the application log group"
  value       = aws_cloudwatch_log_group.application.name
}

output "app_log_group_arn" {
  description = "ARN of the application log group"
  value       = aws_cloudwatch_log_group.application.arn
}

output "api_log_group_name" {
  description = "Name of the API Gateway log group"
  value       = aws_cloudwatch_log_group.api_gateway.name
}

output "dashboard_url" {
  description = "URL to the CloudWatch dashboard"
  value       = "https://console.aws.amazon.com/cloudwatch/home?region=${var.aws_region}#dashboards:name=${aws_cloudwatch_dashboard.main.dashboard_name}"
}

output "synthetics_canary_name" {
  description = "Name of the Synthetics canary"
  value       = aws_synthetics_canary.health_check.name
}

output "resource_group_arn" {
  description = "ARN of the resource group"
  value       = aws_resourcegroups_group.main.arn
}
