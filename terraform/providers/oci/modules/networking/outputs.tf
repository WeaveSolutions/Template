output "vcn_id" {
  description = "OCID of the VCN"
  value       = oci_core_vcn.main.id
}

output "vcn_cidr_block" {
  description = "CIDR block of the VCN"
  value       = var.vcn_cidr_block
}

output "public_subnet_id" {
  description = "OCID of the public subnet"
  value       = oci_core_subnet.public.id
}

output "private_subnet_id" {
  description = "OCID of the private subnet"
  value       = oci_core_subnet.private.id
}

output "database_subnet_id" {
  description = "OCID of the database subnet"
  value       = oci_core_subnet.database.id
}

output "internet_gateway_id" {
  description = "OCID of the Internet Gateway"
  value       = oci_core_internet_gateway.main.id
}

output "nat_gateway_id" {
  description = "OCID of the NAT Gateway"
  value       = oci_core_nat_gateway.main.id
}

output "service_gateway_id" {
  description = "OCID of the Service Gateway"
  value       = oci_core_service_gateway.main.id
}

output "web_nsg_id" {
  description = "OCID of the web network security group"
  value       = oci_core_network_security_group.web.id
}

output "app_nsg_id" {
  description = "OCID of the app network security group"
  value       = oci_core_network_security_group.app.id
}

output "database_nsg_id" {
  description = "OCID of the database network security group"
  value       = oci_core_network_security_group.database.id
}
