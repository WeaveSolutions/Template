# GCP CDN Module

# Static IP address
resource "google_compute_global_address" "cdn" {
  name    = "${var.project_name}-${var.environment}-cdn-ip"
  project = var.project_id
}

# SSL Certificate (managed by Google)
resource "google_compute_managed_ssl_certificate" "cdn" {
  count   = var.custom_domain != "" ? 1 : 0
  name    = "${var.project_name}-${var.environment}-ssl-cert"
  project = var.project_id

  managed {
    domains = [var.custom_domain]
  }
}

# Health check for backend
resource "google_compute_health_check" "cdn" {
  name                = "${var.project_name}-${var.environment}-health-check"
  check_interval_sec  = 10
  timeout_sec         = 5
  healthy_threshold   = 2
  unhealthy_threshold = 3
  project             = var.project_id

  http_health_check {
    request_path = var.health_check_path
    port         = "80"
  }
}

# Backend bucket for static assets
resource "google_compute_backend_bucket" "assets" {
  name        = "${var.project_name}-${var.environment}-backend-bucket"
  bucket_name = var.assets_bucket_name
  enable_cdn  = true
  project     = var.project_id

  cdn_policy {
    cache_mode        = "CACHE_ALL_STATIC"
    default_ttl       = 3600
    max_ttl          = 86400
    client_ttl       = 3600
    negative_caching = true

    negative_caching_policy {
      code = 404
      ttl  = 120
    }

    negative_caching_policy {
      code = 410
      ttl  = 120
    }
  }
}

# Network Endpoint Group for Cloud Run
resource "google_compute_region_network_endpoint_group" "cloud_run_neg" {
  name                  = "${var.project_name}-${var.environment}-neg"
  network_endpoint_type = "SERVERLESS"
  region                = var.region
  project               = var.project_id

  cloud_run {
    service = var.cloud_run_service_name
  }
}

# Backend service for Cloud Run
resource "google_compute_backend_service" "app" {
  name                  = "${var.project_name}-${var.environment}-backend-service"
  protocol              = "HTTP"
  port_name             = "http"
  timeout_sec           = 30
  enable_cdn            = true
  project               = var.project_id
  load_balancing_scheme = "EXTERNAL"

  backend {
    group = google_compute_region_network_endpoint_group.cloud_run_neg.id
  }

  cdn_policy {
    cache_mode                   = "USE_ORIGIN_HEADERS"
    default_ttl                  = 300
    max_ttl                     = 3600
    client_ttl                  = 300
    negative_caching            = true
    serve_while_stale           = 86400
    
    cache_key_policy {
      include_host         = true
      include_protocol     = true
      include_query_string = false
    }
  }
}

# URL map
resource "google_compute_url_map" "cdn" {
  name            = "${var.project_name}-${var.environment}-url-map"
  default_service = google_compute_backend_service.app.id
  project         = var.project_id

  host_rule {
    hosts        = [var.custom_domain != "" ? var.custom_domain : "*"]
    path_matcher = "all"
  }

  path_matcher {
    name            = "all"
    default_service = google_compute_backend_service.app.id

    # Static assets from bucket
    path_rule {
      paths   = ["/static/*", "/assets/*", "/_next/static/*"]
      service = google_compute_backend_bucket.assets.id
    }

    # API routes to Cloud Run
    path_rule {
      paths   = ["/api/*", "/auth/*"]
      service = google_compute_backend_service.app.id
    }
  }
}

# HTTP proxy
resource "google_compute_target_http_proxy" "cdn" {
  name    = "${var.project_name}-${var.environment}-http-proxy"
  url_map = google_compute_url_map.cdn.id
  project = var.project_id
}

# HTTPS proxy
resource "google_compute_target_https_proxy" "cdn" {
  count            = var.custom_domain != "" ? 1 : 0
  name             = "${var.project_name}-${var.environment}-https-proxy"
  url_map          = google_compute_url_map.cdn.id
  ssl_certificates = [google_compute_managed_ssl_certificate.cdn[0].id]
  project          = var.project_id
}

# HTTP forwarding rule
resource "google_compute_global_forwarding_rule" "http" {
  name       = "${var.project_name}-${var.environment}-http-rule"
  target     = google_compute_target_http_proxy.cdn.id
  port_range = "80"
  ip_address = google_compute_global_address.cdn.address
  project    = var.project_id
}

# HTTPS forwarding rule
resource "google_compute_global_forwarding_rule" "https" {
  count      = var.custom_domain != "" ? 1 : 0
  name       = "${var.project_name}-${var.environment}-https-rule"
  target     = google_compute_target_https_proxy.cdn[0].id
  port_range = "443"
  ip_address = google_compute_global_address.cdn.address
  project    = var.project_id
}

# Cloud Armor security policy
resource "google_compute_security_policy" "cdn" {
  name    = "${var.project_name}-${var.environment}-security-policy"
  project = var.project_id

  # Default rule
  rule {
    action   = "allow"
    priority = "2147483647"
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = ["*"]
      }
    }
  }

  # Block common attacks
  rule {
    action   = "deny(403)"
    priority = "1000"
    match {
      expr {
        expression = "origin.region_code == 'XX'"
      }
    }
  }

  # Rate limiting
  rule {
    action   = "rate_based_ban"
    priority = "2000"
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = ["*"]
      }
    }
    rate_limit_options {
      conform_action = "allow"
      exceed_action  = "deny(429)"
      rate_limit_threshold {
        count        = 100
        interval_sec = 60
      }
      ban_duration_sec = 600
    }
  }
}
