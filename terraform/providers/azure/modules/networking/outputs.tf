output "resource_group_name" {
  description = "Name of the resource group"
  value       = azurerm_resource_group.main.name
}

output "resource_group_location" {
  description = "Location of the resource group"
  value       = azurerm_resource_group.main.location
}

output "vnet_id" {
  description = "ID of the virtual network"
  value       = azurerm_virtual_network.main.id
}

output "vnet_name" {
  description = "Name of the virtual network"
  value       = azurerm_virtual_network.main.name
}

output "public_subnet_id" {
  description = "ID of the public subnet"
  value       = azurerm_subnet.public.id
}

output "private_subnet_id" {
  description = "ID of the private subnet"
  value       = azurerm_subnet.private.id
}

output "database_subnet_id" {
  description = "ID of the database subnet"
  value       = azurerm_subnet.database.id
}

output "app_gateway_public_ip" {
  description = "Public IP address for Application Gateway"
  value       = azurerm_public_ip.app_gateway.ip_address
}

output "nat_gateway_public_ip" {
  description = "Public IP address for NAT Gateway"
  value       = azurerm_public_ip.nat.ip_address
}
