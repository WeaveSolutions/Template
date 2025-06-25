output "vpc_id" {
  description = "ID of the VPC"
  value       = google_compute_network.main.id
}

output "vpc_name" {
  description = "Name of the VPC"
  value       = google_compute_network.main.name
}

output "public_subnet_id" {
  description = "ID of the public subnet"
  value       = google_compute_subnetwork.public.id
}

output "private_subnet_id" {
  description = "ID of the private subnet"
  value       = google_compute_subnetwork.private.id
}

output "vpc_connector_id" {
  description = "ID of the VPC connector"
  value       = google_vpc_access_connector.connector.id
}

output "vpc_connector_name" {
  description = "Name of the VPC connector"
  value       = google_vpc_access_connector.connector.name
}
