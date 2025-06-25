# Outputs from Storage Module

output "assets_bucket_id" {
  description = "ID of the assets bucket"
  value       = aws_s3_bucket.assets.id
}

output "assets_bucket_arn" {
  description = "ARN of the assets bucket"
  value       = aws_s3_bucket.assets.arn
}

output "assets_bucket_domain_name" {
  description = "Domain name of the assets bucket"
  value       = aws_s3_bucket.assets.bucket_domain_name
}

output "uploads_bucket_id" {
  description = "ID of the uploads bucket"
  value       = aws_s3_bucket.uploads.id
}

output "uploads_bucket_arn" {
  description = "ARN of the uploads bucket"
  value       = aws_s3_bucket.uploads.arn
}

output "backups_bucket_id" {
  description = "ID of the backups bucket"
  value       = aws_s3_bucket.backups.id
}

output "backups_bucket_arn" {
  description = "ARN of the backups bucket"
  value       = aws_s3_bucket.backups.arn
}

output "logs_bucket_id" {
  description = "ID of the logs bucket"
  value       = aws_s3_bucket.logs.id
}

output "kms_key_id" {
  description = "ID of the KMS key for S3 encryption"
  value       = aws_kms_key.s3.id
}

output "kms_key_arn" {
  description = "ARN of the KMS key for S3 encryption"
  value       = aws_kms_key.s3.arn
}
