# DigitalOcean Provider Outputs

output "vpc_id" {
  description = "ID of the VPC"
  value       = try(digitalocean_vpc.main[0].id, null)
}

output "vpc_cidr" {
  description = "CIDR block of the VPC"
  value       = try(digitalocean_vpc.main[0].ip_range, null)
}

output "droplet_ids" {
  description = "IDs of the created Droplets"
  value       = try(digitalocean_droplet.web[*].id, [])
}

output "droplet_ips" {
  description = "Public IP addresses of the Droplets"
  value       = try(digitalocean_droplet.web[*].ipv4_address, [])
}

output "droplet_private_ips" {
  description = "Private IP addresses of the Droplets"
  value       = try(digitalocean_droplet.web[*].ipv4_address_private, [])
}

output "app_platform_live_url" {
  description = "Live URL of the App Platform application"
  value       = try(digitalocean_app.main[0].live_url, null)
}

output "app_platform_default_ingress" {
  description = "Default ingress URL of the App Platform application"
  value       = try(digitalocean_app.main[0].default_ingress, null)
}

output "app_platform_id" {
  description = "ID of the App Platform application"
  value       = try(digitalocean_app.main[0].id, null)
}

output "database_connection_uri" {
  description = "Connection URI for the managed database"
  value       = try(digitalocean_database_cluster.main[0].uri, null)
  sensitive   = true
}

output "database_host" {
  description = "Host of the managed database"
  value       = try(digitalocean_database_cluster.main[0].host, null)
}

output "database_port" {
  description = "Port of the managed database"
  value       = try(digitalocean_database_cluster.main[0].port, null)
}

output "database_username" {
  description = "Username for the managed database"
  value       = try(digitalocean_database_cluster.main[0].user, null)
}

output "database_password" {
  description = "Password for the managed database"
  value       = try(digitalocean_database_cluster.main[0].password, null)
  sensitive   = true
}

output "database_database" {
  description = "Database name"
  value       = try(digitalocean_database_cluster.main[0].database, null)
}

output "kubernetes_cluster_id" {
  description = "ID of the Kubernetes cluster"
  value       = try(digitalocean_kubernetes_cluster.main[0].id, null)
}

output "kubernetes_cluster_endpoint" {
  description = "Endpoint of the Kubernetes cluster"
  value       = try(digitalocean_kubernetes_cluster.main[0].endpoint, null)
}

output "kubernetes_cluster_token" {
  description = "Token for the Kubernetes cluster"
  value       = try(digitalocean_kubernetes_cluster.main[0].kube_config[0].token, null)
  sensitive   = true
}

output "kubernetes_kubeconfig" {
  description = "Kubeconfig for the Kubernetes cluster"
  value       = try(digitalocean_kubernetes_cluster.main[0].kube_config[0].raw_config, null)
  sensitive   = true
}

output "spaces_bucket_name" {
  description = "Name of the Spaces bucket"
  value       = try(digitalocean_spaces_bucket.main[0].name, null)
}

output "spaces_bucket_endpoint" {
  description = "Endpoint of the Spaces bucket"
  value       = try("https://${digitalocean_spaces_bucket.main[0].name}.${var.digitalocean_region}.digitaloceanspaces.com", null)
}

output "spaces_bucket_domain_name" {
  description = "Domain name of the Spaces bucket"
  value       = try(digitalocean_spaces_bucket.main[0].bucket_domain_name, null)
}

output "cdn_endpoint" {
  description = "CDN endpoint URL"
  value       = try(digitalocean_cdn.main[0].endpoint, null)
}

output "load_balancer_id" {
  description = "ID of the load balancer"
  value       = try(digitalocean_loadbalancer.main[0].id, null)
}

output "load_balancer_ip" {
  description = "IP address of the load balancer"
  value       = try(digitalocean_loadbalancer.main[0].ip, null)
}

output "domain_name" {
  description = "Domain name used for the application"
  value       = var.domain_name
}

output "subdomain" {
  description = "Subdomain used for the environment"
  value       = var.subdomain
}

output "full_domain" {
  description = "Full domain name for the application"
  value       = var.subdomain != "" && var.domain_name != "" ? "${var.subdomain}.${var.domain_name}" : var.domain_name
}

output "project_name" {
  description = "Project name"
  value       = var.project_name
}

output "environment" {
  description = "Environment name"
  value       = var.environment
}

output "region" {
  description = "DigitalOcean region"
  value       = var.digitalocean_region
}

output "firewall_id" {
  description = "ID of the firewall"
  value       = try(digitalocean_firewall.main[0].id, null)
}

output "project_id" {
  description = "ID of the DigitalOcean project"
  value       = try(digitalocean_project.main[0].id, null)
}
