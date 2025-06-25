output "cluster_id" {
  description = "OCID of the OKE cluster"
  value       = oci_containerengine_cluster.main.id
}

output "cluster_endpoint" {
  description = "Endpoint for the OKE cluster"
  value       = oci_containerengine_cluster.main.endpoints[0].endpoint
}

output "node_pool_id" {
  description = "OCID of the node pool"
  value       = oci_containerengine_node_pool.main.id
}

output "api_gateway_container_instance_id" {
  description = "OCID of the API Gateway container instance"
  value       = oci_container_instances_container_instance.api_gateway.id
}

output "api_gateway_private_ip" {
  description = "Private IP of the API Gateway container instance"
  value       = oci_container_instances_container_instance.api_gateway.vnics[0].private_ip
}

output "auth_service_container_instance_id" {
  description = "OCID of the Auth Service container instance"
  value       = oci_container_instances_container_instance.auth_service.id
}

output "auth_service_private_ip" {
  description = "Private IP of the Auth Service container instance"
  value       = oci_container_instances_container_instance.auth_service.vnics[0].private_ip
}

output "kubeconfig_path" {
  description = "Path to the kubeconfig file"
  value       = local_file.kubeconfig.filename
}
