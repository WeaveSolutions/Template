output "namespace" {
  description = "Object storage namespace"
  value       = data.oci_objectstorage_namespace.ns.namespace
}

output "bucket_names" {
  description = "Created bucket names"
  value       = {
    for k, v in oci_objectstorage_bucket.buckets : k => v.name
  }
}

output "bucket_urls" {
  description = "Bucket access URLs"
  value = {
    for k, v in oci_objectstorage_bucket.buckets : 
    k => "https://objectstorage.${var.region}.oraclecloud.com/n/${data.oci_objectstorage_namespace.ns.namespace}/b/${v.name}/o/"
  }
}

output "assets_upload_url" {
  description = "Pre-authenticated request URL for asset uploads"
  value       = "https://objectstorage.${var.region}.oraclecloud.com${oci_objectstorage_preauthrequest.upload_assets.access_uri}"
  sensitive   = true
}

output "block_volume_id" {
  description = "Block volume OCID"
  value       = oci_core_volume.app_data.id
}

output "block_volume_attachment_type" {
  description = "Recommended attachment type for block volume"
  value       = "paravirtualized"
}

output "file_system_id" {
  description = "File system OCID"
  value       = oci_file_storage_file_system.shared.id
}

output "mount_target_id" {
  description = "Mount target OCID"
  value       = oci_file_storage_mount_target.shared.id
}

output "mount_target_ip" {
  description = "Mount target IP address"
  value       = oci_file_storage_mount_target.shared.private_ip_ids[0]
}

output "nfs_mount_path" {
  description = "NFS mount path"
  value       = "${oci_file_storage_mount_target.shared.private_ip_ids[0]}:/${var.project_name}-shared"
}

output "storage_policy_id" {
  description = "Storage policy OCID"
  value       = oci_identity_policy.storage_policy.id
}
