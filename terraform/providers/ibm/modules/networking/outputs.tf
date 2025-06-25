output "vpc" {
  description = "The VPC object"
  value       = ibm_is_vpc.vpc
}

output "vpc_id" {
  description = "The ID of the VPC"
  value       = ibm_is_vpc.vpc.id
}

output "vpc_crn" {
  description = "The CRN of the VPC"
  value       = ibm_is_vpc.vpc.crn
}

output "subnets" {
  description = "The subnet objects"
  value       = ibm_is_subnet.subnet
}

output "subnet_ids" {
  description = "Map of subnet names to their IDs"
  value       = { for name, subnet in ibm_is_subnet.subnet : name => subnet.id }
}

output "subnet_zones" {
  description = "Map of subnet names to their zones"
  value       = { for name, subnet in ibm_is_subnet.subnet : name => subnet.zone }
}

output "subnet_cidrs" {
  description = "Map of subnet names to their CIDR blocks"
  value       = { for name, subnet in ibm_is_subnet.subnet : name => subnet.ipv4_cidr_block }
}

output "public_gateways" {
  description = "The public gateway objects"
  value       = ibm_is_public_gateway.gateway
}

output "public_gateway_ids" {
  description = "Map of zones to public gateway IDs"
  value       = { for zone, gateway in ibm_is_public_gateway.gateway : zone => gateway.id }
}

output "acls" {
  description = "The network ACL objects"
  value       = ibm_is_network_acl.acl
}

output "acl_ids" {
  description = "Map of ACL names to their IDs"
  value       = { for name, acl in ibm_is_network_acl.acl : name => acl.id }
}

output "security_groups" {
  description = "The security group objects"
  value       = ibm_is_security_group.security_group
}

output "security_group_ids" {
  description = "Map of security group names to their IDs"
  value       = { for name, sg in ibm_is_security_group.security_group : name => sg.id }
}

output "load_balancers" {
  description = "The load balancer objects"
  value       = ibm_is_lb.load_balancer
}

output "load_balancer_ids" {
  description = "Map of load balancer names to their IDs"
  value       = { for name, lb in ibm_is_lb.load_balancer : name => lb.id }
}

output "load_balancer_hostnames" {
  description = "Map of load balancer names to their hostnames"
  value       = { for name, lb in ibm_is_lb.load_balancer : name => lb.hostname }
}

output "lb_pools" {
  description = "The load balancer pool objects"
  value       = ibm_is_lb_pool.lb_pool
}

output "lb_listeners" {
  description = "The load balancer listener objects"
  value       = ibm_is_lb_listener.lb_listener
}

output "transit_gateway" {
  description = "The transit gateway object, if created"
  value       = var.create_transit_gateway ? ibm_tg_gateway.transit_gateway[0] : null
}

output "transit_gateway_id" {
  description = "The ID of the transit gateway, if created"
  value       = var.create_transit_gateway ? ibm_tg_gateway.transit_gateway[0].id : null
}

output "transit_gateway_connection" {
  description = "The transit gateway connection object, if created"
  value       = var.create_transit_gateway ? ibm_tg_connection.transit_gateway_connection[0] : null
}
