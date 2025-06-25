output "cloudfront_distribution_id" {
  description = "The ID of the CloudFront distribution"
  value       = try(aws_cloudfront_distribution.website[0].id, "")
}

output "cloudfront_domain_name" {
  description = "The domain name of the CloudFront distribution"
  value       = try(aws_cloudfront_distribution.website[0].domain_name, "")
}

output "s3_bucket_name" {
  description = "The name of the S3 bucket"
  value       = try(aws_s3_bucket.website[0].id, "")
}

output "s3_bucket_arn" {
  description = "The ARN of the S3 bucket"
  value       = try(aws_s3_bucket.website[0].arn, "")
}

output "s3_bucket_website_endpoint" {
  description = "The website endpoint of the S3 bucket"
  value       = try(aws_s3_bucket.website[0].website_endpoint, "")
}

output "cloudfront_distribution_status" {
  description = "The current status of the CloudFront distribution"
  value       = try(aws_cloudfront_distribution.website[0].status, "")
}

output "cloudfront_distribution_arn" {
  description = "The ARN of the CloudFront distribution"
  value       = try(aws_cloudfront_distribution.website[0].arn, "")
}

output "cloudfront_hosted_zone_id" {
  description = "The CloudFront Route 53 zone ID"
  value       = try(aws_cloudfront_distribution.website[0].hosted_zone_id, "")
}

output "route53_record_name" {
  description = "The name of the Route 53 record"
  value       = length(var.custom_domains) > 0 ? var.custom_domains[0] : ""
}

output "website_url" {
  description = "The URL of the website"
  value       = "https://${try(aws_cloudfront_distribution.website[0].domain_name, "")}"
}

output "s3_bucket_website_domain" {
  description = "The domain of the S3 website endpoint"
  value       = try(aws_s3_bucket.website[0].website_domain, "")
}

output "cloudfront_distribution_etag" {
  description = "The current version of the CloudFront distribution"
  value       = try(aws_cloudfront_distribution.website[0].etag, "")
}

output "cloudfront_origin_access_identity_iam_arn" {
  description = "A pre-generated ARN for use in S3 bucket policies"
  value       = try(aws_cloudfront_origin_access_identity.website[0].iam_arn, "")
}

output "cloudfront_distribution_hosted_zone_id" {
  description = "The CloudFront Route 53 zone ID"
  value       = try(aws_cloudfront_distribution.website[0].hosted_zone_id, "")
}

output "s3_bucket_region" {
  description = "The AWS region this bucket resides in"
  value       = try(aws_s3_bucket.website[0].region, "")
}

output "cloudfront_distribution_last_modified_time" {
  description = "The date and time the distribution was last modified"
  value       = try(aws_cloudfront_distribution.website[0].last_modified_time, "")
}

output "s3_bucket_website_configuration" {
  description = "The website configuration of the S3 bucket"
  value       = try(aws_s3_bucket.website[0].website, [])
}

output "cloudfront_distribution_aliases" {
  description = "Extra CNAMEs (alternate domain names) for the CloudFront distribution"
  value       = try(aws_cloudfront_distribution.website[0].aliases, [])
}

output "s3_bucket_tags_all" {
  description = "A map of tags assigned to the resource, including those inherited from the provider default_tags configuration block"
  value       = try(aws_s3_bucket.website[0].tags_all, {})
}

output "cloudfront_distribution_tags_all" {
  description = "A map of tags assigned to the resource, including those inherited from the provider default_tags configuration block"
  value       = try(aws_cloudfront_distribution.website[0].tags_all, {})
}

output "cloudfront_distribution_domain_name" {
  description = "The domain name corresponding to the CloudFront distribution"
  value       = try(aws_cloudfront_distribution.website[0].domain_name, "")
}

output "s3_bucket_website_endpoint" {
  description = "The website endpoint of the S3 bucket"
  value       = "http://${try(aws_s3_bucket.website[0].website_endpoint, "")}"
}

output "cloudfront_distribution_enabled" {
  description = "Whether the CloudFront distribution is enabled"
  value       = try(aws_cloudfront_distribution.website[0].enabled, false)
}

output "s3_bucket_versioning_enabled" {
  description = "Whether versioning is enabled on the S3 bucket"
  value       = try(aws_s3_bucket.website[0].versioning[0].enabled, false)
}

output "cloudfront_distribution_http_version" {
  description = "The maximum HTTP version supported by the CloudFront distribution"
  value       = try(aws_cloudfront_distribution.website[0].http_version, "")
}

output "s3_bucket_server_side_encryption_configuration" {
  description = "The server-side encryption configuration of the S3 bucket"
  value       = try(aws_s3_bucket.website[0].server_side_encryption_configuration, [])
}
