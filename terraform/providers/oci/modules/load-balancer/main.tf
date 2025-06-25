# OCI Load Balancer Module

# Create a public IP for the load balancer
resource "oci_core_public_ip" "lb_public_ip" {
  compartment_id = var.compartment_ocid
  lifetime       = "RESERVED"
  display_name   = "${var.project_name}-${var.environment}-lb-ip"
  
  freeform_tags = var.tags
}

# Load Balancer
resource "oci_load_balancer_load_balancer" "main" {
  compartment_id = var.compartment_ocid
  display_name   = "${var.project_name}-${var.environment}-lb"
  shape          = var.shape
  
  dynamic "shape_details" {
    for_each = var.shape == "flexible" ? [1] : []
    content {
      minimum_bandwidth_in_mbps = var.min_bandwidth_mbps
      maximum_bandwidth_in_mbps = var.max_bandwidth_mbps
    }
  }
  
  subnet_ids = [var.subnet_id]
  
  network_security_group_ids = [var.nsg_id]
  
  reserved_ips {
    id = oci_core_public_ip.lb_public_ip.id
  }
  
  is_private = false
  
  freeform_tags = var.tags
}

# Backend Set
resource "oci_load_balancer_backend_set" "main" {
  load_balancer_id = oci_load_balancer_load_balancer.main.id
  name             = "${var.project_name}-backend-set"
  policy           = "LEAST_CONNECTIONS"
  
  health_checker {
    protocol            = "HTTP"
    port                = 3000
    url_path            = var.backend_set_health_check_url
    return_code         = 200
    timeout_in_millis   = 3000
    interval_ms         = 10000
    retries             = 3
  }
  
  session_persistence_configuration {
    cookie_name      = "lb-session"
    disable_fallback = false
  }
  
  ssl_configuration {
    verify_peer_certificate = false
    verify_depth           = 1
  }
}

# Backends
resource "oci_load_balancer_backend" "backends" {
  for_each = { for idx, backend in var.backend_instances : idx => backend }
  
  load_balancer_id = oci_load_balancer_load_balancer.main.id
  backendset_name  = oci_load_balancer_backend_set.main.name
  ip_address       = each.value.ip_address
  port             = each.value.port
  weight           = lookup(each.value, "weight", 1)
  
  backup  = lookup(each.value, "backup", false)
  drain   = lookup(each.value, "drain", false)
  offline = lookup(each.value, "offline", false)
}

# Certificate for HTTPS
resource "oci_load_balancer_certificate" "main" {
  count = var.certificate_name != null ? 1 : 0
  
  load_balancer_id   = oci_load_balancer_load_balancer.main.id
  certificate_name   = var.certificate_name
  public_certificate = var.public_certificate
  private_key        = var.private_key
  
  lifecycle {
    create_before_destroy = true
  }
}

# HTTPS Listener
resource "oci_load_balancer_listener" "https" {
  count = var.certificate_name != null ? 1 : 0
  
  load_balancer_id         = oci_load_balancer_load_balancer.main.id
  name                     = "${var.project_name}-https-listener"
  default_backend_set_name = oci_load_balancer_backend_set.main.name
  port                     = 443
  protocol                 = "HTTP"
  
  connection_configuration {
    idle_timeout_in_seconds = 300
  }
  
  ssl_configuration {
    certificate_name        = oci_load_balancer_certificate.main[0].certificate_name
    verify_peer_certificate = false
    verify_depth           = 1
    protocols              = ["TLSv1.2", "TLSv1.3"]
  }
}

# HTTP Listener (redirect to HTTPS)
resource "oci_load_balancer_listener" "http" {
  load_balancer_id         = oci_load_balancer_load_balancer.main.id
  name                     = "${var.project_name}-http-listener"
  default_backend_set_name = oci_load_balancer_backend_set.main.name
  port                     = 80
  protocol                 = "HTTP"
  
  connection_configuration {
    idle_timeout_in_seconds = 300
  }
  
  rule_set_names = var.certificate_name != null ? [oci_load_balancer_rule_set.redirect_http[0].name] : []
}

# Rule set to redirect HTTP to HTTPS
resource "oci_load_balancer_rule_set" "redirect_http" {
  count = var.certificate_name != null ? 1 : 0
  
  load_balancer_id = oci_load_balancer_load_balancer.main.id
  name             = "${var.project_name}-redirect-http"
  
  items {
    action = "REDIRECT"
    
    redirect_uri {
      protocol = "HTTPS"
      port     = 443
    }
    
    conditions {
      attribute_name  = "SOURCE_VCN_ID"
      attribute_value = ""
      operator        = "FORCE_LONGEST_PREFIX_MATCH"
    }
  }
}

# Path route rules for microservices
resource "oci_load_balancer_path_route_set" "main" {
  load_balancer_id = oci_load_balancer_load_balancer.main.id
  name             = "${var.project_name}-path-routes"
  
  dynamic "path_routes" {
    for_each = var.path_routes
    content {
      path                     = path_routes.value.path
      backend_set_name        = path_routes.value.backend_set_name
      path_match_type {
        match_type = path_routes.value.match_type
      }
    }
  }
}

# WAF (Web Application Firewall) Protection
resource "oci_waf_web_app_firewall_policy" "main" {
  count = var.enable_waf ? 1 : 0
  
  compartment_id = var.compartment_ocid
  display_name   = "${var.project_name}-${var.environment}-waf-policy"
  
  actions {
    name = "ALLOW"
    type = "ALLOW"
  }
  
  actions {
    name = "BLOCK"
    type = "RETURN_HTTP_RESPONSE"
    
    body {
      text = "Access Denied"
      type = "STATIC_TEXT"
    }
    
    code = 403
  }
  
  request_protection {
    rules {
      action_name = "BLOCK"
      name        = "BlockSQLi"
      type        = "PROTECTION"
      
      protection_capabilities {
        key     = "941100"
        version = 1
      }
    }
    
    rules {
      action_name = "BLOCK"
      name        = "BlockXSS"
      type        = "PROTECTION"
      
      protection_capabilities {
        key     = "941320"
        version = 1
      }
    }
  }
  
  freeform_tags = var.tags
}

# Attach WAF to Load Balancer
resource "oci_waf_web_app_firewall" "main" {
  count = var.enable_waf ? 1 : 0
  
  compartment_id                   = var.compartment_ocid
  backend_type                     = "LOAD_BALANCER"
  load_balancer_id                 = oci_load_balancer_load_balancer.main.id
  web_app_firewall_policy_id       = oci_waf_web_app_firewall_policy.main[0].id
  display_name                     = "${var.project_name}-${var.environment}-waf"
  
  freeform_tags = var.tags
}
