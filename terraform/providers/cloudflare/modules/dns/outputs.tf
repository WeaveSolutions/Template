output "zones" {
  description = "Map of created Cloudflare zones"
  value       = cloudflare_zone.zone
}

output "zone_ids" {
  description = "Map of zone names to their IDs"
  value       = { for name, zone in cloudflare_zone.zone : name => zone.id }
}

output "zone_name_servers" {
  description = "Map of zone names to their name servers"
  value       = { for name, zone in cloudflare_zone.zone : name => zone.name_servers }
}

output "records" {
  description = "Map of created DNS records"
  value       = cloudflare_record.record
}

output "dnssec" {
  description = "DNSSEC information for zones with DNSSEC enabled"
  value       = { for name, dnssec in cloudflare_zone_dnssec.dnssec : name => {
    algorithm      = dnssec.algorithm
    digest         = dnssec.digest
    digest_type    = dnssec.digest_type
    ds             = dnssec.ds
    flags          = dnssec.flags
    key_tag        = dnssec.key_tag
    key_type       = dnssec.key_type
    public_key     = dnssec.public_key
    status         = dnssec.status
  }}
  sensitive   = true
}
