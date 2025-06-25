output "cdn_ip_address" {
  description = "IP address of the CDN"
  value       = google_compute_global_address.cdn.address
}

output "cdn_url" {
  description = "URL of the CDN"
  value       = var.custom_domain != "" ? "https://${var.custom_domain}" : "http://${google_compute_global_address.cdn.address}"
}

output "load_balancer_ip" {
  description = "IP address of the load balancer"
  value       = google_compute_global_address.cdn.address
}

output "security_policy_id" {
  description = "ID of the Cloud Armor security policy"
  value       = google_compute_security_policy.cdn.id
}
