output "page_rules" {
  description = "Map of created page rules"
  value       = cloudflare_page_rule.page_rule
}

output "page_rule_ids" {
  description = "List of page rule IDs"
  value       = [for rule in cloudflare_page_rule.page_rule : rule.id]
}
