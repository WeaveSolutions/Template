/**
 * # IBM Cloud Networking Module
 *
 * This module creates and manages network infrastructure in IBM Cloud including:
 * - Virtual Private Cloud (VPC)
 * - Subnets
 * - Security Groups
 * - Network ACLs
 * - Public Gateways
 * - Load Balancers
 * - Transit Gateways
 */

# VPC
resource "ibm_is_vpc" "vpc" {
  name                      = var.vpc_name
  resource_group            = var.resource_group_id
  address_prefix_management = var.address_prefix_management
  classic_access            = var.classic_access
  default_network_acl_name  = var.default_network_acl_name
  default_security_group_name = var.default_security_group_name
  default_routing_table_name = var.default_routing_table_name
  tags                      = var.tags
}

# Public Gateways
resource "ibm_is_public_gateway" "gateway" {
  for_each        = { for zone in var.vpc_zones : zone => zone if contains(var.public_gateway_zones, zone) }
  
  name            = "${var.vpc_name}-gateway-${each.key}"
  vpc             = ibm_is_vpc.vpc.id
  zone            = each.key
  resource_group  = var.resource_group_id
  tags            = var.tags
}

# Subnets
resource "ibm_is_subnet" "subnet" {
  for_each        = { for subnet in var.vpc_subnets : subnet.name => subnet }
  
  name            = each.value.name
  vpc             = ibm_is_vpc.vpc.id
  zone            = each.value.zone
  ipv4_cidr_block = each.value.cidr
  resource_group  = var.resource_group_id
  public_gateway  = each.value.public_gateway ? ibm_is_public_gateway.gateway[each.value.zone].id : null
  
  tags            = var.tags
}

# Network ACLs
resource "ibm_is_network_acl" "acl" {
  for_each       = { for acl in var.vpc_acls : acl.name => acl }
  
  name           = each.value.name
  vpc            = ibm_is_vpc.vpc.id
  resource_group = var.resource_group_id
  
  dynamic "rules" {
    for_each = each.value.rules
    content {
      name        = rules.value.name
      action      = rules.value.action
      direction   = rules.value.direction
      source      = rules.value.source
      destination = rules.value.destination
      
      dynamic "tcp" {
        for_each = rules.value.tcp != null ? [rules.value.tcp] : []
        content {
          port_min = lookup(tcp.value, "port_min", null)
          port_max = lookup(tcp.value, "port_max", null)
          source_port_min = lookup(tcp.value, "source_port_min", null)
          source_port_max = lookup(tcp.value, "source_port_max", null)
        }
      }
      
      dynamic "udp" {
        for_each = rules.value.udp != null ? [rules.value.udp] : []
        content {
          port_min = lookup(udp.value, "port_min", null)
          port_max = lookup(udp.value, "port_max", null)
          source_port_min = lookup(udp.value, "source_port_min", null)
          source_port_max = lookup(udp.value, "source_port_max", null)
        }
      }
      
      dynamic "icmp" {
        for_each = rules.value.icmp != null ? [rules.value.icmp] : []
        content {
          type = lookup(icmp.value, "type", null)
          code = lookup(icmp.value, "code", null)
        }
      }
    }
  }
  
  tags = var.tags
}

# Security Groups
resource "ibm_is_security_group" "security_group" {
  for_each       = { for sg in var.vpc_security_groups : sg.name => sg }
  
  name           = each.value.name
  vpc            = ibm_is_vpc.vpc.id
  resource_group = var.resource_group_id
  tags           = var.tags
}

# Security Group Rules
resource "ibm_is_security_group_rule" "security_group_rule" {
  for_each  = { 
    for rule in flatten([
      for sg_key, sg in var.vpc_security_groups : [
        for rule in sg.rules : {
          sg_key = sg_key
          sg_name = sg.name
          rule_name = rule.name
          direction = rule.direction
          remote = rule.remote
          tcp = lookup(rule, "tcp", null)
          udp = lookup(rule, "udp", null)
          icmp = lookup(rule, "icmp", null)
        }
      ]
    ]) : "${rule.sg_name}-${rule.rule_name}" => rule
  }
  
  group     = ibm_is_security_group.security_group[each.value.sg_name].id
  direction = each.value.direction
  remote    = each.value.remote
  
  dynamic "tcp" {
    for_each = each.value.tcp != null ? [each.value.tcp] : []
    content {
      port_min = lookup(tcp.value, "port_min", null)
      port_max = lookup(tcp.value, "port_max", null)
    }
  }
  
  dynamic "udp" {
    for_each = each.value.udp != null ? [each.value.udp] : []
    content {
      port_min = lookup(udp.value, "port_min", null)
      port_max = lookup(udp.value, "port_max", null)
    }
  }
  
  dynamic "icmp" {
    for_each = each.value.icmp != null ? [each.value.icmp] : []
    content {
      type = lookup(icmp.value, "type", null)
      code = lookup(icmp.value, "code", null)
    }
  }
}

# Load Balancers
resource "ibm_is_lb" "load_balancer" {
  for_each        = { for lb in var.load_balancers : lb.name => lb }
  
  name            = each.value.name
  type            = lookup(each.value, "type", "public")
  subnets         = [for subnet_name in each.value.subnet_names : ibm_is_subnet.subnet[subnet_name].id]
  security_groups = lookup(each.value, "security_group_names", null) != null ? [
    for sg_name in each.value.security_group_names : ibm_is_security_group.security_group[sg_name].id
  ] : null
  resource_group  = var.resource_group_id
  tags            = var.tags
}

# Load Balancer Pools
resource "ibm_is_lb_pool" "lb_pool" {
  for_each        = { 
    for pool in flatten([
      for lb_key, lb in var.load_balancers : [
        for pool in lookup(lb, "pools", []) : {
          lb_key = lb_key
          lb_name = lb.name
          name = pool.name
          algorithm = lookup(pool, "algorithm", "round_robin")
          protocol = pool.protocol
          health_delay = lookup(pool, "health_delay", 5)
          health_retries = lookup(pool, "health_retries", 2)
          health_timeout = lookup(pool, "health_timeout", 2)
          health_type = lookup(pool, "health_type", "http")
          health_monitor_url = lookup(pool, "health_monitor_url", "/")
          health_monitor_port = lookup(pool, "health_monitor_port", null)
        }
      ]
    ]) : "${pool.lb_name}-${pool.name}" => pool
  }
  
  name            = each.value.name
  lb              = ibm_is_lb.load_balancer[each.value.lb_name].id
  algorithm       = each.value.algorithm
  protocol        = each.value.protocol
  health_delay    = each.value.health_delay
  health_retries  = each.value.health_retries
  health_timeout  = each.value.health_timeout
  health_type     = each.value.health_type
  
  health_monitor_url  = each.value.protocol == "http" || each.value.protocol == "https" ? each.value.health_monitor_url : null
  health_monitor_port = each.value.health_monitor_port
}

# Load Balancer Listeners
resource "ibm_is_lb_listener" "lb_listener" {
  for_each        = { 
    for listener in flatten([
      for lb_key, lb in var.load_balancers : [
        for listener in lookup(lb, "listeners", []) : {
          lb_key = lb_key
          lb_name = lb.name
          port = listener.port
          protocol = listener.protocol
          default_pool_name = lookup(listener, "default_pool_name", null)
          certificate_instance = lookup(listener, "certificate_instance", null)
          connection_limit = lookup(listener, "connection_limit", null)
        }
      ]
    ]) : "${listener.lb_name}-${listener.port}-${listener.protocol}" => listener
  }
  
  lb              = ibm_is_lb.load_balancer[each.value.lb_name].id
  port            = each.value.port
  protocol        = each.value.protocol
  default_pool    = each.value.default_pool_name != null ? ibm_is_lb_pool.lb_pool["${each.value.lb_name}-${each.value.default_pool_name}"].id : null
  certificate_instance = each.value.certificate_instance
  connection_limit = each.value.connection_limit
}

# Transit Gateway
resource "ibm_tg_gateway" "transit_gateway" {
  count          = var.create_transit_gateway ? 1 : 0
  
  name           = "${var.vpc_name}-transit-gateway"
  location       = var.region
  global         = var.transit_gateway_global
  resource_group = var.resource_group_id
  tags           = var.tags
}

# Transit Gateway VPC Connection
resource "ibm_tg_connection" "transit_gateway_connection" {
  count          = var.create_transit_gateway ? 1 : 0
  
  gateway        = ibm_tg_gateway.transit_gateway[0].id
  network_type   = "vpc"
  name           = "${var.vpc_name}-connection"
  network_id     = ibm_is_vpc.vpc.id
}
