output "filters" {
  description = "Map of created WAF filters"
  value       = cloudflare_filter.filter
}

output "firewall_rules" {
  description = "Map of created firewall rules"
  value       = cloudflare_firewall_rule.rule
}

output "rate_limits" {
  description = "Map of created rate limit rules"
  value       = cloudflare_rate_limit.rate_limit
}

output "access_rules" {
  description = "Map of created IP access rules"
  value       = cloudflare_access_rule.rule
}

output "waf_packages" {
  description = "Map of configured WAF packages"
  value       = cloudflare_waf_package.package
}

output "waf_groups" {
  description = "Map of configured WAF groups"
  value       = cloudflare_waf_group.group
}

output "waf_rules" {
  description = "Map of configured WAF rules"
  value       = cloudflare_waf_rule.rule
}
