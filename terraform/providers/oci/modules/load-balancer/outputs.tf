output "load_balancer_id" {
  description = "Load balancer OCID"
  value       = oci_load_balancer_load_balancer.main.id
}

output "load_balancer_ip" {
  description = "Load balancer public IP address"
  value       = oci_core_public_ip.lb_public_ip.ip_address
}

output "load_balancer_url" {
  description = "Load balancer URL"
  value       = "http://${oci_core_public_ip.lb_public_ip.ip_address}"
}

output "load_balancer_https_url" {
  description = "Load balancer HTTPS URL"
  value       = var.certificate_name != null ? "https://${oci_core_public_ip.lb_public_ip.ip_address}" : null
}

output "backend_set_name" {
  description = "Backend set name"
  value       = oci_load_balancer_backend_set.main.name
}

output "http_listener_name" {
  description = "HTTP listener name"
  value       = oci_load_balancer_listener.http.name
}

output "https_listener_name" {
  description = "HTTPS listener name"
  value       = var.certificate_name != null ? oci_load_balancer_listener.https[0].name : null
}

output "waf_id" {
  description = "WAF OCID"
  value       = var.enable_waf ? oci_waf_web_app_firewall.main[0].id : null
}

output "waf_policy_id" {
  description = "WAF policy OCID"
  value       = var.enable_waf ? oci_waf_web_app_firewall_policy.main[0].id : null
}
