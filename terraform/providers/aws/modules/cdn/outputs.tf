# Outputs from CDN Module

output "cloudfront_distribution_id" {
  description = "ID of the CloudFront distribution"
  value       = aws_cloudfront_distribution.main.id
}

output "cloudfront_distribution_arn" {
  description = "ARN of the CloudFront distribution"
  value       = aws_cloudfront_distribution.main.arn
}

output "cloudfront_distribution_domain_name" {
  description = "Domain name of the CloudFront distribution"
  value       = aws_cloudfront_distribution.main.domain_name
}

output "cloudfront_distribution_hosted_zone_id" {
  description = "CloudFront Route 53 zone ID"
  value       = aws_cloudfront_distribution.main.hosted_zone_id
}

output "cloudfront_oai_arn" {
  description = "ARN of the CloudFront Origin Access Identity"
  value       = aws_cloudfront_origin_access_identity.main.iam_arn
}

output "cloudfront_oai_path" {
  description = "Path of the CloudFront Origin Access Identity"
  value       = aws_cloudfront_origin_access_identity.main.cloudfront_access_identity_path
}
