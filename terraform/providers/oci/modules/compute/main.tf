# OCI Compute Module - OKE and Container Instances

# OKE Cluster
resource "oci_containerengine_cluster" "main" {
  compartment_id     = var.compartment_ocid
  kubernetes_version = var.kubernetes_version
  name              = "${var.project_name}-${var.environment}-oke"
  vcn_id            = var.vcn_id
  
  cluster_pod_network_options {
    cni_type = "OCI_VCN_IP_NATIVE"
  }
  
  endpoint_config {
    is_public_ip_enabled = true
    subnet_id           = var.public_subnet_id
    nsg_ids             = [var.web_nsg_id]
  }
  
  options {
    add_ons {
      is_kubernetes_dashboard_enabled = false
      is_tiller_enabled              = false
    }
    
    kubernetes_network_config {
      pods_cidr     = "10.244.0.0/16"
      services_cidr = "10.96.0.0/16"
    }
    
    service_lb_subnet_ids = [var.public_subnet_id]
  }
  
  freeform_tags = var.tags
}

# Node Pool
resource "oci_containerengine_node_pool" "main" {
  cluster_id         = oci_containerengine_cluster.main.id
  compartment_id     = var.compartment_ocid
  kubernetes_version = var.kubernetes_version
  name              = "${var.project_name}-${var.environment}-nodepool"
  
  node_config_details {
    placement_configs {
      availability_domain = data.oci_identity_availability_domains.ads.availability_domains[0].name
      subnet_id          = var.private_subnet_id
    }
    
    size = var.node_pool_size
    
    nsg_ids = [var.app_nsg_id]
  }
  
  node_shape = var.node_shape
  
  node_shape_config {
    memory_in_gbs = var.node_memory_in_gbs
    ocpus        = var.node_ocpus
  }
  
  node_source_details {
    image_id    = data.oci_core_images.node_pool_images.images[0].id
    source_type = "IMAGE"
  }
  
  node_pool_cycling_details {
    is_node_cycling_enabled = true
    maximum_surge          = "33%"
    maximum_unavailable    = "33%"
  }
  
  initial_node_labels {
    key   = "environment"
    value = var.environment
  }
  
  freeform_tags = var.tags
}

# Get latest Oracle Linux image
data "oci_core_images" "node_pool_images" {
  compartment_id           = var.compartment_ocid
  operating_system         = "Oracle Linux"
  operating_system_version = "8"
  shape                    = var.node_shape
  sort_by                  = "TIMECREATED"
  sort_order               = "DESC"
}

# Availability Domains
data "oci_identity_availability_domains" "ads" {
  compartment_id = var.compartment_ocid
}

# Container Instances for lightweight workloads
resource "oci_container_instances_container_instance" "api_gateway" {
  availability_domain = data.oci_identity_availability_domains.ads.availability_domains[0].name
  compartment_id      = var.compartment_ocid
  display_name        = "${var.project_name}-${var.environment}-api-gateway"
  
  container_restart_policy = "ALWAYS"
  
  shape = "CI.Standard.E4.Flex"
  shape_config {
    memory_in_gbs = 4
    ocpus        = 1
  }
  
  vnics {
    subnet_id = var.private_subnet_id
    nsg_ids   = [var.app_nsg_id]
  }
  
  containers {
    display_name = "api-gateway"
    image_url    = "${var.registry_endpoint}/${var.project_name}/api-gateway:latest"
    
    environment_variables = {
      NODE_ENV                = var.environment
      PORT                   = "3000"
      AUTH0_DOMAIN           = var.auth0_domain
      AUTH0_API_IDENTIFIER   = var.auth0_api_identifier
      DATABASE_URL           = var.database_url
    }
    
    resource_config {
      memory_limit_in_gbs = 2
      vcpus_limit        = 0.5
    }
    
    health_checks {
      health_check_type = "HTTP"
      path             = "/health"
      port             = 3000
      
      failure_action   = "RESTART"
      failure_threshold = 3
      success_threshold = 1
      timeout_in_seconds = 5
      interval_in_seconds = 10
    }
  }
  
  freeform_tags = var.tags
}

# Container Instance for Auth Service
resource "oci_container_instances_container_instance" "auth_service" {
  availability_domain = data.oci_identity_availability_domains.ads.availability_domains[0].name
  compartment_id      = var.compartment_ocid
  display_name        = "${var.project_name}-${var.environment}-auth-service"
  
  container_restart_policy = "ALWAYS"
  
  shape = "CI.Standard.E4.Flex"
  shape_config {
    memory_in_gbs = 2
    ocpus        = 0.5
  }
  
  vnics {
    subnet_id = var.private_subnet_id
    nsg_ids   = [var.app_nsg_id]
  }
  
  containers {
    display_name = "auth-service"
    image_url    = "${var.registry_endpoint}/${var.project_name}/auth:latest"
    
    environment_variables = {
      NODE_ENV             = var.environment
      PORT                = "3001"
      AUTH0_DOMAIN        = var.auth0_domain
      AUTH0_CLIENT_ID     = var.auth0_client_id
      AUTH0_CLIENT_SECRET = var.auth0_client_secret
      AUTH0_API_IDENTIFIER = var.auth0_api_identifier
    }
    
    resource_config {
      memory_limit_in_gbs = 1
      vcpus_limit        = 0.25
    }
    
    health_checks {
      health_check_type = "HTTP"
      path             = "/health"
      port             = 8000
      
      failure_action   = "RESTART"
      failure_threshold = 3
      success_threshold = 1
      timeout_in_seconds = 5
      interval_in_seconds = 10
    }
  }
  
  freeform_tags = var.tags
}

# Kubernetes configurations for microservices
resource "local_file" "kubeconfig" {
  content  = oci_containerengine_cluster.main.endpoints[0].kubeconfig
  filename = "${path.module}/kubeconfig_${var.environment}"
}

# Kubernetes namespace
resource "null_resource" "create_namespace" {
  depends_on = [local_file.kubeconfig]
  
  provisioner "local-exec" {
    command = <<-EOT
      export KUBECONFIG=${local_file.kubeconfig.filename}
      kubectl create namespace ${var.project_name}-${var.environment} --dry-run=client -o yaml | kubectl apply -f -
    EOT
  }
}

# ConfigMap for Auth0 configuration
resource "null_resource" "auth0_configmap" {
  depends_on = [null_resource.create_namespace]
  
  provisioner "local-exec" {
    command = <<-EOT
      export KUBECONFIG=${local_file.kubeconfig.filename}
      kubectl create configmap auth0-config \
        --from-literal=AUTH0_DOMAIN=${var.auth0_domain} \
        --from-literal=AUTH0_API_IDENTIFIER=${var.auth0_api_identifier} \
        --namespace=${var.project_name}-${var.environment} \
        --dry-run=client -o yaml | kubectl apply -f -
    EOT
  }
}

# Secret for Auth0 credentials
resource "null_resource" "auth0_secret" {
  depends_on = [null_resource.create_namespace]
  
  provisioner "local-exec" {
    command = <<-EOT
      export KUBECONFIG=${local_file.kubeconfig.filename}
      kubectl create secret generic auth0-credentials \
        --from-literal=AUTH0_CLIENT_ID=${var.auth0_client_id} \
        --from-literal=AUTH0_CLIENT_SECRET=${var.auth0_client_secret} \
        --namespace=${var.project_name}-${var.environment} \
        --dry-run=client -o yaml | kubectl apply -f -
    EOT
  }
}
