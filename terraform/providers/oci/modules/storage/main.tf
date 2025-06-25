# OCI Storage Module - Object Storage and Block Volumes

# Get Object Storage namespace
data "oci_objectstorage_namespace" "ns" {
  compartment_id = var.compartment_ocid
}

# Create Object Storage buckets
resource "oci_objectstorage_bucket" "buckets" {
  for_each = var.buckets
  
  compartment_id = var.compartment_ocid
  namespace      = data.oci_objectstorage_namespace.ns.namespace
  name           = "${var.project_name}-${var.environment}-${each.value.name}"
  
  access_type = each.value.public_access
  
  versioning = each.value.versioning ? "Enabled" : "Disabled"
  
  # Storage tier - Standard for frequently accessed data
  storage_tier = "Standard"
  
  # Enable auto-tiering for cost optimization
  auto_tiering = "InfrequentAccess"
  
  freeform_tags = merge(
    var.tags,
    {
      bucket_type = each.key
    }
  )
}

# Lifecycle policies for buckets
resource "oci_objectstorage_object_lifecycle_policy" "backup_lifecycle" {
  bucket    = oci_objectstorage_bucket.buckets["backups"].name
  namespace = data.oci_objectstorage_namespace.ns.namespace
  
  rules {
    action      = "ARCHIVE"
    is_enabled  = true
    name        = "archive-old-backups"
    time_amount = 30
    time_unit   = "DAYS"
    
    object_name_filter {
      inclusion_patterns = ["*.backup", "*.sql", "*.dump"]
    }
  }
  
  rules {
    action      = "DELETE"
    is_enabled  = true
    name        = "delete-old-backups"
    time_amount = 90
    time_unit   = "DAYS"
    
    object_name_filter {
      inclusion_patterns = ["*.backup", "*.sql", "*.dump"]
    }
  }
}

# Pre-authenticated requests for direct upload/download
resource "oci_objectstorage_preauthrequest" "upload_assets" {
  bucket       = oci_objectstorage_bucket.buckets["assets"].name
  namespace    = data.oci_objectstorage_namespace.ns.namespace
  name         = "${var.project_name}-upload-assets"
  access_type  = "AnyObjectWrite"
  
  time_expires = timeadd(timestamp(), "8760h") # 1 year
  
  bucket_listing_action = "Deny"
}

# CORS configuration for assets bucket
resource "null_resource" "cors_config" {
  provisioner "local-exec" {
    command = <<-EOT
      oci os bucket update \
        --bucket-name ${oci_objectstorage_bucket.buckets["assets"].name} \
        --namespace ${data.oci_objectstorage_namespace.ns.namespace} \
        --public-access-type ${oci_objectstorage_bucket.buckets["assets"].access_type} \
        --metadata '{"cors-enabled": "true", "cors-allowed-origins": ["https://${var.allowed_origins}"], "cors-allowed-methods": ["GET", "HEAD", "OPTIONS"], "cors-allowed-headers": ["*"], "cors-max-age": "3600"}'
    EOT
  }
  
  depends_on = [oci_objectstorage_bucket.buckets]
}

# Create block volumes for persistent storage
resource "oci_core_volume" "app_data" {
  availability_domain = var.availability_domain
  compartment_id      = var.compartment_ocid
  display_name        = "${var.project_name}-${var.environment}-app-data"
  size_in_gbs         = var.block_volume_size
  
  vpus_per_gb = var.block_volume_performance
  
  freeform_tags = var.tags
}

# Volume backup policy
resource "oci_core_volume_backup_policy" "app_data_backup" {
  compartment_id = var.compartment_ocid
  display_name   = "${var.project_name}-${var.environment}-backup-policy"
  
  schedules {
    backup_type       = "INCREMENTAL"
    period            = "ONE_DAY"
    retention_seconds = 604800 # 7 days
    
    time_zone = "UTC"
    hour_of_day = 2
  }
  
  schedules {
    backup_type       = "FULL"
    period            = "ONE_WEEK"
    retention_seconds = 2592000 # 30 days
    
    time_zone = "UTC"
    hour_of_day = 2
    day_of_week = "SUNDAY"
  }
  
  freeform_tags = var.tags
}

# Assign backup policy to volume
resource "oci_core_volume_backup_policy_assignment" "app_data_assignment" {
  asset_id  = oci_core_volume.app_data.id
  policy_id = oci_core_volume_backup_policy.app_data_backup.id
}

# File Storage for shared storage
resource "oci_file_storage_file_system" "shared" {
  availability_domain = var.availability_domain
  compartment_id      = var.compartment_ocid
  display_name        = "${var.project_name}-${var.environment}-shared-fs"
  
  freeform_tags = var.tags
}

# Mount target for file system
resource "oci_file_storage_mount_target" "shared" {
  availability_domain = var.availability_domain
  compartment_id      = var.compartment_ocid
  subnet_id           = var.subnet_id
  display_name        = "${var.project_name}-${var.environment}-mount-target"
  
  nsg_ids = var.nsg_ids
  
  freeform_tags = var.tags
}

# Export for file system
resource "oci_file_storage_export" "shared" {
  export_set_id  = oci_file_storage_mount_target.shared.export_set_id
  file_system_id = oci_file_storage_file_system.shared.id
  path           = "/${var.project_name}-shared"
  
  export_options {
    source                         = var.subnet_cidr
    access                         = "READ_WRITE"
    identity_squash                = "NONE"
    require_privileged_source_port = false
  }
}

# IAM policy for storage access
resource "oci_identity_policy" "storage_policy" {
  compartment_id = var.compartment_ocid
  name           = "${var.project_name}-${var.environment}-storage-policy"
  description    = "Policy for storage access"
  
  statements = [
    # Allow instances to use volumes
    "Allow dynamic-group ${var.project_name}-${var.environment}-instances to use volumes in compartment id ${var.compartment_ocid}",
    "Allow dynamic-group ${var.project_name}-${var.environment}-instances to use volume-attachments in compartment id ${var.compartment_ocid}",
    
    # Allow instances to access object storage
    "Allow dynamic-group ${var.project_name}-${var.environment}-instances to manage objects in compartment id ${var.compartment_ocid}",
    "Allow dynamic-group ${var.project_name}-${var.environment}-instances to read buckets in compartment id ${var.compartment_ocid}",
    
    # Allow functions to access storage
    "Allow dynamic-group ${var.project_name}-${var.environment}-functions to manage objects in compartment id ${var.compartment_ocid} where target.bucket.name='${oci_objectstorage_bucket.buckets["assets"].name}'",
  ]
  
  freeform_tags = var.tags
}
