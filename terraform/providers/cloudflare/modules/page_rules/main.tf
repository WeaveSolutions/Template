/**
 * # Cloudflare Page Rules Module
 *
 * This module manages page rules for Cloudflare domains.
 *
 * ## Features
 * - Custom page rules for URL patterns
 * - Cache configuration
 * - Security settings
 * - Edge optimization
 * - Redirects and forwarding
 */

resource "cloudflare_page_rule" "page_rule" {
  for_each = { for idx, rule in var.page_rules : idx => rule }

  zone_id  = each.value.zone_id
  target   = each.value.target
  status   = lookup(each.value, "status", "active")
  priority = lookup(each.value, "priority", null)

  dynamic "actions" {
    for_each = [each.value.actions]
    content {
      always_use_https         = lookup(actions.value, "always_use_https", null)
      automatic_https_rewrites = lookup(actions.value, "automatic_https_rewrites", null)
      browser_cache_ttl        = lookup(actions.value, "browser_cache_ttl", null)
      browser_check            = lookup(actions.value, "browser_check", null)
      bypass_cache_on_cookie   = lookup(actions.value, "bypass_cache_on_cookie", null)
      cache_by_device_type     = lookup(actions.value, "cache_by_device_type", null)
      cache_deception_armor    = lookup(actions.value, "cache_deception_armor", null)
      cache_key_fields         = lookup(actions.value, "cache_key_fields", null)
      cache_level              = lookup(actions.value, "cache_level", null)
      cache_on_cookie          = lookup(actions.value, "cache_on_cookie", null)
      cache_ttl_by_status      = lookup(actions.value, "cache_ttl_by_status", null)
      disable_apps             = lookup(actions.value, "disable_apps", null)
      disable_performance      = lookup(actions.value, "disable_performance", null)
      disable_railgun          = lookup(actions.value, "disable_railgun", null)
      disable_security         = lookup(actions.value, "disable_security", null)
      edge_cache_ttl           = lookup(actions.value, "edge_cache_ttl", null)
      email_obfuscation        = lookup(actions.value, "email_obfuscation", null)
      explicit_cache_control   = lookup(actions.value, "explicit_cache_control", null)
      forwarding_url {
        url         = lookup(actions.value, "forwarding_url", null) != null ? actions.value.forwarding_url.url : null
        status_code = lookup(actions.value, "forwarding_url", null) != null ? actions.value.forwarding_url.status_code : null
      }
      host_header_override     = lookup(actions.value, "host_header_override", null)
      ip_geolocation           = lookup(actions.value, "ip_geolocation", null)
      minify {
        html = lookup(actions.value, "minify", null) != null ? lookup(actions.value.minify, "html", false) : null
        css  = lookup(actions.value, "minify", null) != null ? lookup(actions.value.minify, "css", false) : null
        js   = lookup(actions.value, "minify", null) != null ? lookup(actions.value.minify, "js", false) : null
      }
      mirage                   = lookup(actions.value, "mirage", null)
      opportunistic_encryption = lookup(actions.value, "opportunistic_encryption", null)
      origin_error_page_pass_thru = lookup(actions.value, "origin_error_page_pass_thru", null)
      polish                   = lookup(actions.value, "polish", null)
      resolve_override         = lookup(actions.value, "resolve_override", null)
      respect_strong_etag      = lookup(actions.value, "respect_strong_etag", null)
      response_buffering       = lookup(actions.value, "response_buffering", null)
      rocket_loader            = lookup(actions.value, "rocket_loader", null)
      security_level           = lookup(actions.value, "security_level", null)
      server_side_exclude      = lookup(actions.value, "server_side_exclude", null)
      sort_query_string_for_cache = lookup(actions.value, "sort_query_string_for_cache", null)
      ssl                      = lookup(actions.value, "ssl", null)
      true_client_ip_header    = lookup(actions.value, "true_client_ip_header", null)
      waf                      = lookup(actions.value, "waf", null)
    }
  }
}
