output "dns_zones" {
  description = "Map of created Cloudflare DNS zones"
  value       = module.dns.zones
}

output "dns_zone_ids" {
  description = "Map of zone names to their IDs"
  value       = module.dns.zone_ids
}

output "dns_zone_name_servers" {
  description = "Map of zone names to their name servers"
  value       = module.dns.zone_name_servers
}

output "dns_records" {
  description = "Map of created DNS records"
  value       = module.dns.records
}

output "dns_dnssec" {
  description = "DNSSEC information for zones with DNSSEC enabled"
  value       = module.dns.dnssec
  sensitive   = true
}

output "waf_filters" {
  description = "Map of created WAF filters"
  value       = module.waf.filters
}

output "waf_firewall_rules" {
  description = "Map of created firewall rules"
  value       = module.waf.firewall_rules
}

output "waf_rate_limits" {
  description = "Map of created rate limit rules"
  value       = module.waf.rate_limits
}

output "waf_access_rules" {
  description = "Map of created IP access rules"
  value       = module.waf.access_rules
}

output "page_rules" {
  description = "Map of created page rules"
  value       = module.page_rules.page_rules
}

output "worker_scripts" {
  description = "Map of deployed worker scripts"
  value       = module.workers.scripts
}

output "worker_kv_namespaces" {
  description = "Map of created KV namespaces"
  value       = module.workers.kv_namespaces
}

output "worker_routes" {
  description = "Map of created worker routes"
  value       = module.workers.routes
}

output "worker_cron_triggers" {
  description = "Map of created cron triggers"
  value       = module.workers.cron_triggers
}
