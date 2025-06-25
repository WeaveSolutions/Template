# OCI Database Module - Autonomous Database

# Random password for wallet (if not provided)
resource "random_password" "wallet_password" {
  count   = var.wallet_password == null ? 1 : 0
  length  = 16
  special = true
}

# Autonomous Database
resource "oci_database_autonomous_database" "main" {
  compartment_id = var.compartment_ocid
  db_name        = var.db_name
  display_name   = var.db_display_name
  
  # Workload type: OLTP, DW, AJD, or APEX
  db_workload = "OLTP"
  
  # Version
  db_version = var.db_version
  
  # Compute
  cpu_core_count = var.cpu_core_count
  
  # Storage
  data_storage_size_in_tbs = ceil(var.data_storage_size_gb / 1024)
  
  # License
  license_model = var.license_model
  
  # Password
  admin_password = var.admin_password
  
  # Network
  subnet_id                = var.subnet_id
  nsg_ids                  = [var.nsg_id]
  is_access_control_enabled = true
  whitelisted_ips          = var.whitelisted_ips
  
  # Auto scaling
  is_auto_scaling_enabled = var.enable_auto_scaling
  
  # Backup
  is_auto_scaling_for_storage_enabled = var.enable_auto_scaling
  
  # Maintenance
  is_preview_version_with_service_terms_accepted = false
  
  # Private endpoint
  private_endpoint_label = "${var.project_name}-${var.environment}"
  
  freeform_tags = var.tags
  
  lifecycle {
    ignore_changes = [admin_password]
  }
}

# Create application schema and user
resource "null_resource" "create_app_user" {
  depends_on = [oci_database_autonomous_database.main]
  
  provisioner "local-exec" {
    command = <<-EOT
      echo "Creating application user..."
      # This would typically be done via SQL*Plus or SQLcl
      # For now, this is a placeholder - actual implementation would use OCI CLI or SDK
    EOT
  }
}

# Autonomous Database Wallet for secure connections
resource "oci_database_autonomous_database_wallet" "main" {
  autonomous_database_id = oci_database_autonomous_database.main.id
  password              = coalesce(var.wallet_password, random_password.wallet_password[0].result)
  base64_encode_content = true
  generate_type         = "SINGLE"
}

# Store wallet in local file (for development)
resource "local_file" "wallet" {
  content_base64 = oci_database_autonomous_database_wallet.main.content
  filename       = "${path.module}/wallet_${var.environment}.zip"
  
  provisioner "local-exec" {
    command = "powershell -Command \"Expand-Archive -Path '${self.filename}' -DestinationPath '${path.module}/wallet_${var.environment}' -Force\""
  }
}

# Create backup configuration
resource "oci_database_autonomous_database_backup" "manual" {
  count = var.create_initial_backup ? 1 : 0
  
  autonomous_database_id = oci_database_autonomous_database.main.id
  display_name          = "${var.project_name}-${var.environment}-initial-backup"
  is_long_term_backup   = true
  retention_in_days     = 60
}

# Enable automated backups
resource "null_resource" "enable_auto_backup" {
  depends_on = [oci_database_autonomous_database.main]
  
  provisioner "local-exec" {
    command = <<-EOT
      oci db autonomous-database update \
        --autonomous-database-id ${oci_database_autonomous_database.main.id} \
        --is-auto-scaling-for-storage-enabled true
    EOT
  }
}

# Access Control List entries
resource "oci_database_autonomous_database_regional_wallet_management" "main" {
  count = var.enable_regional_wallet ? 1 : 0
  
  should_rotate = true
  grace_period  = 30
}

# Database tools connection
locals {
  connection_urls = {
    high   = oci_database_autonomous_database.main.connection_urls[0].apex_url
    medium = oci_database_autonomous_database.main.connection_urls[0].graph_studio_url
    low    = oci_database_autonomous_database.main.connection_urls[0].machine_learning_notebook_url
  }
  
  jdbc_url = "jdbc:oracle:thin:@${var.db_name}_high?TNS_ADMIN=${path.module}/wallet_${var.environment}"
  
  connection_string = var.subnet_id != null ? (
    oci_database_autonomous_database.main.private_endpoint != null ? 
    "tcps://${oci_database_autonomous_database.main.private_endpoint}:1522/${var.db_name}_high.adb.oraclecloud.com" :
    oci_database_autonomous_database.main.connection_strings[0].high
  ) : oci_database_autonomous_database.main.connection_strings[0].high
}

# Output connection information for applications
resource "local_file" "db_config" {
  content = jsonencode({
    db_name           = var.db_name
    connection_string = local.connection_string
    jdbc_url         = local.jdbc_url
    admin_username   = "ADMIN"
    app_username     = var.app_username
    wallet_location  = abspath("${path.module}/wallet_${var.environment}")
  })
  filename = "${path.module}/db-config.json"
  
  depends_on = [local_file.wallet]
}
