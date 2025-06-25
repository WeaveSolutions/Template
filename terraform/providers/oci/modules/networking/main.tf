# OCI Networking Module

# Virtual Cloud Network (VCN)
resource "oci_core_vcn" "main" {
  compartment_id = var.compartment_ocid
  cidr_blocks    = [var.vcn_cidr_block]
  display_name   = "${var.project_name}-${var.environment}-vcn"
  dns_label      = replace(var.project_name, "-", "")
  
  freeform_tags = var.tags
}

# Internet Gateway
resource "oci_core_internet_gateway" "main" {
  compartment_id = var.compartment_ocid
  vcn_id        = oci_core_vcn.main.id
  display_name  = "${var.project_name}-${var.environment}-igw"
  enabled       = true
  
  freeform_tags = var.tags
}

# NAT Gateway
resource "oci_core_nat_gateway" "main" {
  compartment_id = var.compartment_ocid
  vcn_id        = oci_core_vcn.main.id
  display_name  = "${var.project_name}-${var.environment}-nat"
  
  freeform_tags = var.tags
}

# Service Gateway
data "oci_core_services" "all_services" {
  filter {
    name   = "name"
    values = ["All .* Services In Oracle Services Network"]
    regex  = true
  }
}

resource "oci_core_service_gateway" "main" {
  compartment_id = var.compartment_ocid
  vcn_id        = oci_core_vcn.main.id
  display_name  = "${var.project_name}-${var.environment}-sgw"
  
  services {
    service_id = data.oci_core_services.all_services.services[0].id
  }
  
  freeform_tags = var.tags
}

# Route Tables
resource "oci_core_route_table" "public" {
  compartment_id = var.compartment_ocid
  vcn_id        = oci_core_vcn.main.id
  display_name  = "${var.project_name}-${var.environment}-public-rt"
  
  route_rules {
    network_entity_id = oci_core_internet_gateway.main.id
    destination       = "0.0.0.0/0"
  }
  
  freeform_tags = var.tags
}

resource "oci_core_route_table" "private" {
  compartment_id = var.compartment_ocid
  vcn_id        = oci_core_vcn.main.id
  display_name  = "${var.project_name}-${var.environment}-private-rt"
  
  route_rules {
    network_entity_id = oci_core_nat_gateway.main.id
    destination       = "0.0.0.0/0"
  }
  
  route_rules {
    network_entity_id = oci_core_service_gateway.main.id
    destination       = data.oci_core_services.all_services.services[0].cidr_block
    destination_type  = "SERVICE_CIDR_BLOCK"
  }
  
  freeform_tags = var.tags
}

# Security Lists
resource "oci_core_security_list" "public" {
  compartment_id = var.compartment_ocid
  vcn_id        = oci_core_vcn.main.id
  display_name  = "${var.project_name}-${var.environment}-public-sl"
  
  # Ingress rules
  ingress_security_rules {
    protocol = "6" # TCP
    source   = "0.0.0.0/0"
    
    tcp_options {
      min = 80
      max = 80
    }
  }
  
  ingress_security_rules {
    protocol = "6" # TCP
    source   = "0.0.0.0/0"
    
    tcp_options {
      min = 443
      max = 443
    }
  }
  
  # SSH from allowed IPs
  dynamic "ingress_security_rules" {
    for_each = var.allowed_ssh_cidr_blocks
    content {
      protocol = "6" # TCP
      source   = ingress_security_rules.value
      
      tcp_options {
        min = 22
        max = 22
      }
    }
  }
  
  # Allow all from VCN
  ingress_security_rules {
    protocol = "all"
    source   = var.vcn_cidr_block
  }
  
  # Egress rules
  egress_security_rules {
    protocol    = "all"
    destination = "0.0.0.0/0"
  }
  
  freeform_tags = var.tags
}

resource "oci_core_security_list" "private" {
  compartment_id = var.compartment_ocid
  vcn_id        = oci_core_vcn.main.id
  display_name  = "${var.project_name}-${var.environment}-private-sl"
  
  # Allow all from VCN
  ingress_security_rules {
    protocol = "all"
    source   = var.vcn_cidr_block
  }
  
  # Egress rules
  egress_security_rules {
    protocol    = "all"
    destination = "0.0.0.0/0"
  }
  
  freeform_tags = var.tags
}

# Subnets
resource "oci_core_subnet" "public" {
  compartment_id             = var.compartment_ocid
  vcn_id                    = oci_core_vcn.main.id
  cidr_block                = var.public_subnet_cidr
  display_name              = "${var.project_name}-${var.environment}-public-subnet"
  dns_label                 = "public"
  prohibit_public_ip_on_vnic = false
  route_table_id            = oci_core_route_table.public.id
  security_list_ids         = [oci_core_security_list.public.id]
  
  freeform_tags = var.tags
}

resource "oci_core_subnet" "private" {
  compartment_id             = var.compartment_ocid
  vcn_id                    = oci_core_vcn.main.id
  cidr_block                = var.private_subnet_cidr
  display_name              = "${var.project_name}-${var.environment}-private-subnet"
  dns_label                 = "private"
  prohibit_public_ip_on_vnic = true
  route_table_id            = oci_core_route_table.private.id
  security_list_ids         = [oci_core_security_list.private.id]
  
  freeform_tags = var.tags
}

resource "oci_core_subnet" "database" {
  compartment_id             = var.compartment_ocid
  vcn_id                    = oci_core_vcn.main.id
  cidr_block                = var.database_subnet_cidr
  display_name              = "${var.project_name}-${var.environment}-database-subnet"
  dns_label                 = "database"
  prohibit_public_ip_on_vnic = true
  route_table_id            = oci_core_route_table.private.id
  security_list_ids         = [oci_core_security_list.private.id]
  
  freeform_tags = var.tags
}

# Network Security Groups (NSGs) for additional security
resource "oci_core_network_security_group" "web" {
  compartment_id = var.compartment_ocid
  vcn_id        = oci_core_vcn.main.id
  display_name  = "${var.project_name}-${var.environment}-web-nsg"
  
  freeform_tags = var.tags
}

resource "oci_core_network_security_group_security_rule" "web_ingress_http" {
  network_security_group_id = oci_core_network_security_group.web.id
  direction                 = "INGRESS"
  protocol                 = "6" # TCP
  source                   = "0.0.0.0/0"
  source_type              = "CIDR_BLOCK"
  
  tcp_options {
    destination_port_range {
      min = 80
      max = 80
    }
  }
}

resource "oci_core_network_security_group_security_rule" "web_ingress_https" {
  network_security_group_id = oci_core_network_security_group.web.id
  direction                 = "INGRESS"
  protocol                 = "6" # TCP
  source                   = "0.0.0.0/0"
  source_type              = "CIDR_BLOCK"
  
  tcp_options {
    destination_port_range {
      min = 443
      max = 443
    }
  }
}

resource "oci_core_network_security_group" "app" {
  compartment_id = var.compartment_ocid
  vcn_id        = oci_core_vcn.main.id
  display_name  = "${var.project_name}-${var.environment}-app-nsg"
  
  freeform_tags = var.tags
}

resource "oci_core_network_security_group_security_rule" "app_ingress" {
  network_security_group_id = oci_core_network_security_group.app.id
  direction                 = "INGRESS"
  protocol                 = "6" # TCP
  source                   = oci_core_network_security_group.web.id
  source_type              = "NETWORK_SECURITY_GROUP"
  
  tcp_options {
    destination_port_range {
      min = 3000
      max = 3000
    }
  }
}

resource "oci_core_network_security_group" "database" {
  compartment_id = var.compartment_ocid
  vcn_id        = oci_core_vcn.main.id
  display_name  = "${var.project_name}-${var.environment}-database-nsg"
  
  freeform_tags = var.tags
}

resource "oci_core_network_security_group_security_rule" "database_ingress" {
  network_security_group_id = oci_core_network_security_group.database.id
  direction                 = "INGRESS"
  protocol                 = "6" # TCP
  source                   = oci_core_network_security_group.app.id
  source_type              = "NETWORK_SECURITY_GROUP"
  
  tcp_options {
    destination_port_range {
      min = 1521
      max = 1522
    }
  }
}
