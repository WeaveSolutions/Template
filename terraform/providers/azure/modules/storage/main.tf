# Azure Storage Module

# Storage Account
resource "azurerm_storage_account" "main" {
  name                     = lower(replace("${var.project_name}${var.environment}storage", "-", ""))
  resource_group_name      = var.resource_group_name
  location                 = var.location
  account_tier             = var.storage_tier
  account_replication_type = var.replication_type
  
  blob_properties {
    cors_rule {
      allowed_headers    = ["*"]
      allowed_methods    = ["GET", "HEAD", "POST", "PUT", "DELETE"]
      allowed_origins    = var.cors_origins
      exposed_headers    = ["*"]
      max_age_in_seconds = 3600
    }
    
    delete_retention_policy {
      days = var.soft_delete_retention_days
    }
    
    versioning_enabled = var.enable_versioning
  }
  
  network_rules {
    default_action             = "Allow"
    ip_rules                   = var.allowed_ip_rules
    virtual_network_subnet_ids = var.allowed_subnet_ids
  }
  
  tags = {
    Environment = var.environment
    Project     = var.project_name
  }
}

# Assets Container
resource "azurerm_storage_container" "assets" {
  name                  = "assets"
  storage_account_name  = azurerm_storage_account.main.name
  container_access_type = "blob"
}

# Uploads Container
resource "azurerm_storage_container" "uploads" {
  name                  = "uploads"
  storage_account_name  = azurerm_storage_account.main.name
  container_access_type = "private"
}

# Backups Container
resource "azurerm_storage_container" "backups" {
  name                  = "backups"
  storage_account_name  = azurerm_storage_account.main.name
  container_access_type = "private"
}

# Lifecycle Management Policy
resource "azurerm_storage_management_policy" "lifecycle" {
  storage_account_id = azurerm_storage_account.main.id

  rule {
    name    = "backups-lifecycle"
    enabled = true
    
    filters {
      prefix_match = ["backups/"]
      blob_types   = ["blockBlob"]
    }
    
    actions {
      base_blob {
        tier_to_cool_after_days_since_modification_greater_than    = 30
        tier_to_archive_after_days_since_modification_greater_than = 90
        delete_after_days_since_modification_greater_than          = var.backup_retention_days
      }
      
      snapshot {
        delete_after_days_since_creation_greater_than = 7
      }
    }
  }
  
  rule {
    name    = "uploads-lifecycle"
    enabled = true
    
    filters {
      prefix_match = ["uploads/"]
      blob_types   = ["blockBlob"]
    }
    
    actions {
      base_blob {
        tier_to_cool_after_days_since_modification_greater_than = 60
        delete_after_days_since_modification_greater_than       = 365
      }
    }
  }
}

# Storage Account for Static Website (CDN)
resource "azurerm_storage_account" "static_website" {
  count                    = var.enable_static_website ? 1 : 0
  name                     = lower(replace("${var.project_name}${var.environment}web", "-", ""))
  resource_group_name      = var.resource_group_name
  location                 = var.location
  account_tier             = "Standard"
  account_replication_type = "LRS"
  
  static_website {
    index_document     = "index.html"
    error_404_document = "404.html"
  }
  
  tags = {
    Environment = var.environment
    Project     = var.project_name
  }
}

# Storage Queue for async processing
resource "azurerm_storage_queue" "tasks" {
  name                 = "${var.project_name}-${var.environment}-tasks"
  storage_account_name = azurerm_storage_account.main.name
}

# Storage Table for session management
resource "azurerm_storage_table" "sessions" {
  name                 = "sessions"
  storage_account_name = azurerm_storage_account.main.name
}

# Backup storage account (geo-redundant)
resource "azurerm_storage_account" "backup" {
  count                    = var.environment == "prod" ? 1 : 0
  name                     = lower(replace("${var.project_name}${var.environment}backup", "-", ""))
  resource_group_name      = var.resource_group_name
  location                 = var.backup_location
  account_tier             = "Standard"
  account_replication_type = "GRS"
  
  blob_properties {
    delete_retention_policy {
      days = 30
    }
    
    container_delete_retention_policy {
      days = 30
    }
    
    versioning_enabled = true
  }
  
  tags = {
    Environment = var.environment
    Project     = var.project_name
    Purpose     = "Backup"
  }
}
