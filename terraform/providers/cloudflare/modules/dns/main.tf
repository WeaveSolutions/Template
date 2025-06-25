/**
 * # Cloudflare DNS Module
 *
 * This module manages DNS records and zones in Cloudflare.
 *
 * ## Features
 * - DNS zone management
 * - Record creation (A, AAAA, CNAME, MX, TXT, etc.)
 * - Zone settings customization
 * - DNS-level security policies
 */

resource "cloudflare_zone" "zone" {
  for_each   = var.zones
  zone       = each.key
  account_id = var.cloudflare_account_id
  plan       = lookup(each.value, "plan", "free")
  type       = lookup(each.value, "type", "full")
  paused     = lookup(each.value, "paused", false)
  jump_start = lookup(each.value, "jump_start", false)

  dynamic "meta" {
    for_each = each.value.meta != null ? [each.value.meta] : []
    content {
      wildcard_proxiable = lookup(meta.value, "wildcard_proxiable", false)
      phishing_detected  = lookup(meta.value, "phishing_detected", false)
    }
  }
  
  tags = merge(
    var.tags,
    lookup(each.value, "tags", {})
  )
}

resource "cloudflare_record" "record" {
  for_each = { for idx, record in var.records : "${record.zone_name}-${record.name}-${record.type}-${idx}" => record }
  
  zone_id         = lookup(cloudflare_zone.zone, each.value.zone_name, null) != null ? cloudflare_zone.zone[each.value.zone_name].id : each.value.zone_id
  name            = each.value.name
  value           = each.value.value
  type            = each.value.type
  ttl             = lookup(each.value, "ttl", 1)  # 1 = automatic
  priority        = lookup(each.value, "priority", null)
  proxied         = lookup(each.value, "proxied", false)
  allow_overwrite = lookup(each.value, "allow_overwrite", false)
  comment         = lookup(each.value, "comment", null)
  
  dynamic "data" {
    for_each = each.value.data != null ? [each.value.data] : []
    content {
      algorithm      = lookup(data.value, "algorithm", null)
      key_tag        = lookup(data.value, "key_tag", null)
      flags          = lookup(data.value, "flags", null)
      service        = lookup(data.value, "service", null)
      protocol       = lookup(data.value, "protocol", null)
      digest_type    = lookup(data.value, "digest_type", null)
      digest         = lookup(data.value, "digest", null)
      certificate    = lookup(data.value, "certificate", null)
      type           = lookup(data.value, "type", null)
      usage          = lookup(data.value, "usage", null)
      selector       = lookup(data.value, "selector", null)
      matching_type  = lookup(data.value, "matching_type", null)
      weight         = lookup(data.value, "weight", null)
      port           = lookup(data.value, "port", null)
      target         = lookup(data.value, "target", null)
      content        = lookup(data.value, "content", null)
      value          = lookup(data.value, "value", null)
    }
  }
  
  timeouts {
    create = lookup(var.timeouts, "create", "5m")
    update = lookup(var.timeouts, "update", "5m") 
    delete = lookup(var.timeouts, "delete", "5m")
  }
}

resource "cloudflare_zone_settings_override" "settings" {
  for_each = { for zone_name, zone in var.zones : zone_name => zone if lookup(zone, "apply_settings", false) }
  
  zone_id = cloudflare_zone.zone[each.key].id
  
  settings {
    always_online            = lookup(each.value.settings, "always_online", "on")
    always_use_https         = lookup(each.value.settings, "always_use_https", "on")
    automatic_https_rewrites = lookup(each.value.settings, "automatic_https_rewrites", "on")
    brotli                   = lookup(each.value.settings, "brotli", "on")
    browser_cache_ttl        = lookup(each.value.settings, "browser_cache_ttl", 14400)
    browser_check            = lookup(each.value.settings, "browser_check", "on")
    cache_level              = lookup(each.value.settings, "cache_level", "aggressive")
    challenge_ttl            = lookup(each.value.settings, "challenge_ttl", 1800)
    development_mode         = lookup(each.value.settings, "development_mode", "off")
    email_obfuscation        = lookup(each.value.settings, "email_obfuscation", "on")
    hotlink_protection       = lookup(each.value.settings, "hotlink_protection", "on")
    ip_geolocation           = lookup(each.value.settings, "ip_geolocation", "on")
    ipv6                     = lookup(each.value.settings, "ipv6", "on")
    min_tls_version          = lookup(each.value.settings, "min_tls_version", "1.2")
    mobile_redirect          = lookup(each.value.settings, "mobile_redirect", false) ? {
      status           = "on"
      mobile_subdomain = lookup(each.value.settings, "mobile_subdomain", "m")
      strip_uri        = lookup(each.value.settings, "strip_uri", false)
    } : {
      status           = "off"
      mobile_subdomain = ""
      strip_uri        = false
    }
    opportunistic_encryption = lookup(each.value.settings, "opportunistic_encryption", "on")
    polish                   = lookup(each.value.settings, "polish", "off")
    security_level           = lookup(each.value.settings, "security_level", "medium")
    server_side_exclude      = lookup(each.value.settings, "server_side_exclude", "on")
    ssl                      = lookup(each.value.settings, "ssl", "flexible")
    tls_1_3                  = lookup(each.value.settings, "tls_1_3", "on")
    websockets               = lookup(each.value.settings, "websockets", "on")
    zero_rtt                 = lookup(each.value.settings, "zero_rtt", "on")
  }
}

# DNS Security
resource "cloudflare_zone_dnssec" "dnssec" {
  for_each = { for zone_name, zone in var.zones : zone_name => zone if lookup(zone, "enable_dnssec", false) }
  
  zone_id = cloudflare_zone.zone[each.key].id
}
