# Outputs for Security Module

output "waf_acl_id" {
  description = "WAF Web ACL ID"
  value       = aws_wafv2_web_acl.main.id
}

output "waf_acl_arn" {
  description = "WAF Web ACL ARN"
  value       = aws_wafv2_web_acl.main.arn
}

output "guardduty_detector_id" {
  description = "GuardDuty detector ID"
  value       = aws_guardduty_detector.main.id
}

output "security_hub_account_id" {
  description = "Security Hub account ID"
  value       = aws_securityhub_account.main.id
}

output "config_recorder_name" {
  description = "Config recorder name"
  value       = aws_config_configuration_recorder.main.name
}

output "waf_logs_bucket" {
  description = "S3 bucket for WAF logs"
  value       = aws_s3_bucket.waf_logs.id
}

output "config_bucket" {
  description = "S3 bucket for AWS Config"
  value       = aws_s3_bucket.config.id
}
