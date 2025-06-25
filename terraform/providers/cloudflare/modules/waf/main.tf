/**
 * # Cloudflare WAF Module
 *
 * This module manages Web Application Firewall (WAF) settings for Cloudflare-protected domains.
 *
 * ## Features
 * - WAF ruleset management
 * - Custom firewall rules
 * - Rate limiting
 * - IP access rules
 * - Bot management
 */

# Firewall rules
resource "cloudflare_filter" "filter" {
  for_each = { for idx, filter in var.filters : "${filter.zone_id}-${idx}" => filter }

  zone_id     = each.value.zone_id
  description = lookup(each.value, "description", null)
  expression  = each.value.expression
  ref         = lookup(each.value, "ref", null)
  paused      = lookup(each.value, "paused", false)
}

resource "cloudflare_firewall_rule" "rule" {
  for_each = { for idx, rule in var.firewall_rules : "${rule.zone_id}-${idx}" => rule }

  zone_id     = each.value.zone_id
  description = lookup(each.value, "description", null)
  filter_id   = each.value.filter_id != null ? each.value.filter_id : cloudflare_filter.filter["${each.value.zone_id}-${each.value.filter_idx}"].id
  action      = each.value.action
  priority    = lookup(each.value, "priority", null)
  paused      = lookup(each.value, "paused", false)
  products    = lookup(each.value, "products", null)
}

# Rate limiting rules
resource "cloudflare_rate_limit" "rate_limit" {
  for_each = { for idx, limit in var.rate_limits : "${limit.zone_id}-${idx}" => limit }

  zone_id          = each.value.zone_id
  threshold        = each.value.threshold
  period           = each.value.period
  description      = lookup(each.value, "description", null)
  disabled         = lookup(each.value, "disabled", false)
  bypass_url_patterns = lookup(each.value, "bypass_url_patterns", [])

  match {
    request {
      methods   = lookup(each.value.match.request, "methods", ["GET"])
      schemes   = lookup(each.value.match.request, "schemes", ["HTTP", "HTTPS"])
      url_pattern = lookup(each.value.match.request, "url_pattern", "*")
    }

    dynamic "response" {
      for_each = lookup(each.value.match, "response", null) != null ? [each.value.match.response] : []
      content {
        statuses = lookup(response.value, "statuses", [200])
        origin_traffic = lookup(response.value, "origin_traffic", true)
      }
    }
  }

  action {
    mode    = lookup(each.value.action, "mode", "ban")
    timeout = lookup(each.value.action, "timeout", 60)
    
    dynamic "response" {
      for_each = lookup(each.value.action, "response", null) != null ? [each.value.action.response] : []
      content {
        content_type = lookup(response.value, "content_type", "text/plain")
        body         = lookup(response.value, "body", "You have been rate limited")
      }
    }
  }

  correlate {
    by = lookup(each.value, "correlate_by", "ip")
  }
}

# IP Access Rules
resource "cloudflare_access_rule" "rule" {
  for_each = { for idx, rule in var.access_rules : "${rule.mode}-${rule.configuration.target}-${idx}" => rule }

  zone_id = lookup(each.value, "zone_id", null)
  account_id = lookup(each.value, "account_id", null)
  notes      = lookup(each.value, "notes", null)
  mode       = each.value.mode
  
  configuration {
    target = each.value.configuration.target
    value  = each.value.configuration.value
  }
}

# WAF Package settings
resource "cloudflare_waf_package" "package" {
  for_each = { for package in var.waf_packages : "${package.zone_id}-${package.package_id}" => package }
  
  zone_id     = each.value.zone_id
  package_id  = each.value.package_id
  sensitivity = lookup(each.value, "sensitivity", null)
  action_mode = lookup(each.value, "action_mode", null)
}

resource "cloudflare_waf_group" "group" {
  for_each = { for group in var.waf_groups : "${group.zone_id}-${group.group_id}" => group }
  
  zone_id    = each.value.zone_id
  package_id = each.value.package_id
  group_id   = each.value.group_id
  mode       = each.value.mode
}

resource "cloudflare_waf_rule" "rule" {
  for_each = { for rule in var.waf_rules : "${rule.zone_id}-${rule.rule_id}" => rule }
  
  zone_id    = each.value.zone_id
  package_id = each.value.package_id
  rule_id    = each.value.rule_id
  mode       = each.value.mode
}
