# CDN Module - CloudFront Distribution
# Following Well-Architected Framework for performance and security

locals {
  s3_origin_id = "S3-${var.assets_bucket_id}"
  app_origin_id = "App-${var.app_domain}"
}

# CloudFront Origin Access Identity
resource "aws_cloudfront_origin_access_identity" "main" {
  comment = "${var.project_name}-${var.environment} OAI"
}

# CloudFront Distribution
resource "aws_cloudfront_distribution" "main" {
  enabled             = true
  is_ipv6_enabled     = true
  comment             = "${var.project_name}-${var.environment} CDN"
  default_root_object = "index.html"
  aliases             = var.custom_domain != null ? [var.custom_domain, "www.${var.custom_domain}"] : []
  price_class         = var.price_class
  
  # Origins
  origin {
    domain_name = var.assets_bucket_regional_domain_name
    origin_id   = local.s3_origin_id
    
    s3_origin_config {
      origin_access_identity = aws_cloudfront_origin_access_identity.main.cloudfront_access_identity_path
    }
  }
  
  origin {
    domain_name = var.app_domain
    origin_id   = local.app_origin_id
    
    custom_origin_config {
      http_port              = 80
      https_port             = 443
      origin_protocol_policy = "https-only"
      origin_ssl_protocols   = ["TLSv1.2"]
      
      origin_keepalive_timeout = 60
      origin_read_timeout      = 60
    }
    
    custom_header {
      name  = "X-Forwarded-Host"
      value = var.custom_domain != null ? var.custom_domain : var.app_domain
    }
  }
  
  # Default cache behavior (for app)
  default_cache_behavior {
    allowed_methods  = ["DELETE", "GET", "HEAD", "OPTIONS", "PATCH", "POST", "PUT"]
    cached_methods   = ["GET", "HEAD"]
    target_origin_id = local.app_origin_id
    
    forwarded_values {
      query_string = true
      headers      = ["Host", "Accept", "Accept-Language", "Authorization", "CloudFront-Forwarded-Proto"]
      
      cookies {
        forward = "all"
      }
    }
    
    viewer_protocol_policy = "redirect-to-https"
    min_ttl                = 0
    default_ttl            = 0
    max_ttl                = 31536000
    compress               = true
    
    lambda_function_association {
      event_type   = "origin-request"
      lambda_arn   = var.edge_lambda_arn
      include_body = false
    }
  }
  
  # Cache behavior for static assets
  ordered_cache_behavior {
    path_pattern     = "/static/*"
    allowed_methods  = ["GET", "HEAD", "OPTIONS"]
    cached_methods   = ["GET", "HEAD", "OPTIONS"]
    target_origin_id = local.s3_origin_id
    
    forwarded_values {
      query_string = false
      headers      = ["Origin"]
      
      cookies {
        forward = "none"
      }
    }
    
    viewer_protocol_policy = "redirect-to-https"
    min_ttl                = 0
    default_ttl            = 86400
    max_ttl                = 31536000
    compress               = true
  }
  
  # Cache behavior for images
  ordered_cache_behavior {
    path_pattern     = "*.jpg"
    allowed_methods  = ["GET", "HEAD"]
    cached_methods   = ["GET", "HEAD"]
    target_origin_id = local.s3_origin_id
    
    forwarded_values {
      query_string = false
      
      cookies {
        forward = "none"
      }
    }
    
    viewer_protocol_policy = "redirect-to-https"
    min_ttl                = 0
    default_ttl            = 604800  # 7 days
    max_ttl                = 31536000 # 1 year
    compress               = true
  }
  
  ordered_cache_behavior {
    path_pattern     = "*.png"
    allowed_methods  = ["GET", "HEAD"]
    cached_methods   = ["GET", "HEAD"]
    target_origin_id = local.s3_origin_id
    
    forwarded_values {
      query_string = false
      
      cookies {
        forward = "none"
      }
    }
    
    viewer_protocol_policy = "redirect-to-https"
    min_ttl                = 0
    default_ttl            = 604800
    max_ttl                = 31536000
    compress               = true
  }
  
  # API behavior (no caching)
  ordered_cache_behavior {
    path_pattern     = "/api/*"
    allowed_methods  = ["DELETE", "GET", "HEAD", "OPTIONS", "PATCH", "POST", "PUT"]
    cached_methods   = ["GET", "HEAD"]
    target_origin_id = local.app_origin_id
    
    forwarded_values {
      query_string = true
      headers      = ["*"]
      
      cookies {
        forward = "all"
      }
    }
    
    viewer_protocol_policy = "https-only"
    min_ttl                = 0
    default_ttl            = 0
    max_ttl                = 0
    compress               = false
  }
  
  # Error pages
  custom_error_response {
    error_code         = 404
    response_code      = 200
    response_page_path = "/index.html"
  }
  
  custom_error_response {
    error_code         = 403
    response_code      = 200
    response_page_path = "/index.html"
  }
  
  # Geo restrictions
  restrictions {
    geo_restriction {
      restriction_type = var.geo_restriction_type
      locations        = var.geo_restriction_locations
    }
  }
  
  # SSL configuration
  viewer_certificate {
    acm_certificate_arn            = var.acm_certificate_arn
    cloudfront_default_certificate = var.acm_certificate_arn == null ? true : false
    minimum_protocol_version       = "TLSv1.2_2021"
    ssl_support_method             = var.acm_certificate_arn == null ? null : "sni-only"
  }
  
  # WAF
  web_acl_id = var.waf_acl_id
  
  # Logging
  logging_config {
    include_cookies = false
    bucket          = var.logs_bucket_domain_name
    prefix          = "cloudfront/"
  }
  
  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-cdn"
    }
  )
}

# Response Headers Policy
resource "aws_cloudfront_response_headers_policy" "security" {
  name    = "${var.project_name}-${var.environment}-security-headers"
  comment = "Security headers for ${var.project_name}"
  
  security_headers_config {
    content_type_options {
      override = true
    }
    
    frame_options {
      frame_option = "DENY"
      override     = true
    }
    
    referrer_policy {
      referrer_policy = "strict-origin-when-cross-origin"
      override        = true
    }
    
    strict_transport_security {
      access_control_max_age_sec = 63072000
      include_subdomains         = true
      preload                    = true
      override                   = true
    }
    
    xss_protection {
      mode_block = true
      protection = true
      override   = true
    }
  }
  
  custom_headers_config {
    items {
      header   = "X-Frame-Options"
      value    = "SAMEORIGIN"
      override = true
    }
    
    items {
      header   = "X-Content-Type-Options"
      value    = "nosniff"
      override = true
    }
    
    items {
      header   = "Permissions-Policy"
      value    = "camera=(), microphone=(), geolocation=()"
      override = true
    }
  }
}

# Cache Policy
resource "aws_cloudfront_cache_policy" "optimized" {
  name        = "${var.project_name}-${var.environment}-cache-policy"
  comment     = "Cache policy for ${var.project_name}"
  min_ttl     = 0
  default_ttl = 86400
  max_ttl     = 31536000
  
  parameters_in_cache_key_and_forwarded_to_origin {
    enable_accept_encoding_brotli = true
    enable_accept_encoding_gzip   = true
    
    cookies_config {
      cookie_behavior = "none"
    }
    
    headers_config {
      header_behavior = "none"
    }
    
    query_strings_config {
      query_string_behavior = "none"
    }
  }
}

# Origin Request Policy
resource "aws_cloudfront_origin_request_policy" "app" {
  name    = "${var.project_name}-${var.environment}-origin-request-policy"
  comment = "Origin request policy for ${var.project_name}"
  
  cookies_config {
    cookie_behavior = "all"
  }
  
  headers_config {
    header_behavior = "whitelist"
    headers {
      items = [
        "Accept",
        "Accept-Charset",
        "Accept-Encoding",
        "Accept-Language",
        "Authorization",
        "Host",
        "Origin",
        "Referer",
        "User-Agent",
        "CloudFront-Forwarded-Proto",
        "CloudFront-Is-Desktop-Viewer",
        "CloudFront-Is-Mobile-Viewer",
        "CloudFront-Is-SmartTV-Viewer",
        "CloudFront-Is-Tablet-Viewer",
        "CloudFront-Viewer-Country"
      ]
    }
  }
  
  query_strings_config {
    query_string_behavior = "all"
  }
}

# CloudWatch Alarms
resource "aws_cloudwatch_metric_alarm" "origin_4xx" {
  alarm_name          = "${var.project_name}-${var.environment}-cdn-origin-4xx"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = "2"
  metric_name         = "4xxErrorRate"
  namespace           = "AWS/CloudFront"
  period              = "300"
  statistic           = "Average"
  threshold           = var.error_rate_threshold
  alarm_description   = "This metric monitors 4xx errors from origin"
  alarm_actions       = var.alarm_sns_topic_arns
  treat_missing_data  = "notBreaching"
  
  dimensions = {
    DistributionId = aws_cloudfront_distribution.main.id
  }
  
  tags = var.common_tags
}

resource "aws_cloudwatch_metric_alarm" "origin_5xx" {
  alarm_name          = "${var.project_name}-${var.environment}-cdn-origin-5xx"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = "2"
  metric_name         = "5xxErrorRate"
  namespace           = "AWS/CloudFront"
  period              = "300"
  statistic           = "Average"
  threshold           = var.error_rate_threshold
  alarm_description   = "This metric monitors 5xx errors from origin"
  alarm_actions       = var.alarm_sns_topic_arns
  treat_missing_data  = "notBreaching"
  
  dimensions = {
    DistributionId = aws_cloudfront_distribution.main.id
  }
  
  tags = var.common_tags
}
