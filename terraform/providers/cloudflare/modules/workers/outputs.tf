output "scripts" {
  description = "Map of deployed worker scripts"
  value       = cloudflare_worker_script.script
}

output "kv_namespaces" {
  description = "Map of created KV namespaces"
  value       = cloudflare_workers_kv_namespace.namespace
}

output "routes" {
  description = "Map of created worker routes"
  value       = cloudflare_worker_route.route
}

output "cron_triggers" {
  description = "Map of created cron triggers"
  value       = cloudflare_worker_cron_trigger.trigger
}

output "kv_entries" {
  description = "Map of created KV entries"
  value       = cloudflare_workers_kv.kv
  sensitive   = true
}
