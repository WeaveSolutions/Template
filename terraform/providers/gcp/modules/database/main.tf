# GCP Database Module

# Random password for database
resource "random_password" "db_password" {
  length  = 32
  special = true
}

# Cloud SQL PostgreSQL instance
resource "google_sql_database_instance" "main" {
  name             = "${var.project_name}-${var.environment}-postgres"
  database_version = "POSTGRES_14"
  region           = var.region
  project          = var.project_id

  settings {
    tier              = var.db_tier
    availability_type = var.high_availability ? "REGIONAL" : "ZONAL"
    disk_size         = var.db_disk_size
    disk_type         = "PD_SSD"
    disk_autoresize   = true

    backup_configuration {
      enabled                        = true
      start_time                     = "03:00"
      point_in_time_recovery_enabled = var.environment == "prod"
      transaction_log_retention_days = var.environment == "prod" ? 7 : 1
      backup_retention_settings {
        retained_backups = var.environment == "prod" ? 30 : 7
        retention_unit   = "COUNT"
      }
    }

    ip_configuration {
      ipv4_enabled    = false
      private_network = var.vpc_id
      require_ssl     = true
    }

    database_flags {
      name  = "max_connections"
      value = var.max_connections
    }

    insights_config {
      query_insights_enabled  = true
      query_string_length     = 1024
      record_application_tags = true
      record_client_address   = true
    }

    maintenance_window {
      day          = 7  # Sunday
      hour         = 4  # 4 AM
      update_track = "stable"
    }
  }

  deletion_protection = var.environment == "prod"
}

# Database
resource "google_sql_database" "main" {
  name     = var.project_name
  instance = google_sql_database_instance.main.name
  project  = var.project_id
}

# Database user
resource "google_sql_user" "main" {
  name     = var.db_username
  instance = google_sql_database_instance.main.name
  password = random_password.db_password.result
  project  = var.project_id
}

# Read replica (production only)
resource "google_sql_database_instance" "read_replica" {
  count                = var.environment == "prod" && var.enable_read_replica ? 1 : 0
  name                 = "${var.project_name}-${var.environment}-postgres-replica"
  master_instance_name = google_sql_database_instance.main.name
  region               = var.replica_region != "" ? var.replica_region : var.region
  database_version     = "POSTGRES_14"
  project              = var.project_id

  replica_configuration {
    failover_target = false
  }

  settings {
    tier              = var.db_tier
    availability_type = "ZONAL"
    disk_size         = var.db_disk_size
    disk_type         = "PD_SSD"

    ip_configuration {
      ipv4_enabled    = false
      private_network = var.vpc_id
      require_ssl     = true
    }

    database_flags {
      name  = "max_connections"
      value = var.max_connections
    }
  }
}

# Firestore database for sessions
resource "google_firestore_database" "sessions" {
  project     = var.project_id
  name        = "${var.project_name}-${var.environment}-sessions"
  location_id = var.firestore_location
  type        = "FIRESTORE_NATIVE"

  concurrency_mode = "OPTIMISTIC"
  app_engine_integration_mode = "DISABLED"

  # Note: Firestore in Datastore mode would use google_datastore_index instead
}

# Secret Manager for database credentials
resource "google_secret_manager_secret" "db_password" {
  secret_id = "${var.project_name}-${var.environment}-db-password"
  project   = var.project_id

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret_version" "db_password" {
  secret      = google_secret_manager_secret.db_password.id
  secret_data = random_password.db_password.result
}

# Service account for database access
resource "google_service_account" "db_user" {
  account_id   = "${var.project_name}-${var.environment}-db-user"
  display_name = "Database User Service Account"
  project      = var.project_id
}

# IAM bindings
resource "google_project_iam_member" "db_user_sql_client" {
  project = var.project_id
  role    = "roles/cloudsql.client"
  member  = "serviceAccount:${google_service_account.db_user.email}"
}

resource "google_secret_manager_secret_iam_member" "db_password_access" {
  secret_id = google_secret_manager_secret.db_password.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.db_user.email}"
  project   = var.project_id
}
