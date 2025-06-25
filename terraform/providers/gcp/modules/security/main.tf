# GCP Security Module

# Enable required APIs
resource "google_project_service" "security_apis" {
  for_each = toset([
    "securitycenter.googleapis.com",
    "cloudkms.googleapis.com",
    "secretmanager.googleapis.com",
    "iap.googleapis.com"
  ])
  
  service = each.value
  project = var.project_id
}

# KMS keyring and key for encryption
resource "google_kms_key_ring" "main" {
  name     = "${var.project_name}-${var.environment}-keyring"
  location = var.region
  project  = var.project_id

  depends_on = [google_project_service.security_apis["cloudkms.googleapis.com"]]
}

resource "google_kms_crypto_key" "main" {
  name            = "${var.project_name}-${var.environment}-key"
  key_ring        = google_kms_key_ring.main.id
  rotation_period = "7776000s" # 90 days

  lifecycle {
    prevent_destroy = true
  }
}

# Service account for security operations
resource "google_service_account" "security" {
  account_id   = "${var.project_name}-${var.environment}-security"
  display_name = "Security Service Account"
  project      = var.project_id
}

# IAM bindings for security service account
resource "google_project_iam_member" "security_viewer" {
  project = var.project_id
  role    = "roles/securitycenter.findingsViewer"
  member  = "serviceAccount:${google_service_account.security.email}"
}

resource "google_kms_crypto_key_iam_member" "security_encrypter" {
  crypto_key_id = google_kms_crypto_key.main.id
  role          = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member        = "serviceAccount:${google_service_account.security.email}"
}

# Organization policies (if organization ID provided)
resource "google_organization_policy" "require_ssl" {
  count      = var.organization_id != "" ? 1 : 0
  org_id     = var.organization_id
  constraint = "compute.requireSsl"

  boolean_policy {
    enforced = true
  }
}

resource "google_organization_policy" "disable_serial_port" {
  count      = var.organization_id != "" ? 1 : 0
  org_id     = var.organization_id
  constraint = "compute.disableSerialPortAccess"

  boolean_policy {
    enforced = true
  }
}

# VPC Service Controls (production only)
resource "google_access_context_manager_service_perimeter" "main" {
  count  = var.environment == "prod" && var.enable_vpc_service_controls ? 1 : 0
  parent = "accessPolicies/${var.access_policy_id}"
  name   = "accessPolicies/${var.access_policy_id}/servicePerimeters/${var.project_name}-${var.environment}"
  title  = "${var.project_name} ${var.environment} Perimeter"

  status {
    resources = ["projects/${var.project_number}"]
    
    restricted_services = [
      "storage.googleapis.com",
      "cloudsql.googleapis.com",
      "run.googleapis.com"
    ]

    vpc_accessible_services {
      enable_restriction = true
      allowed_services   = [
        "storage.googleapis.com",
        "cloudsql.googleapis.com",
        "run.googleapis.com"
      ]
    }
  }
}

# Binary Authorization policy
resource "google_binary_authorization_policy" "main" {
  count   = var.enable_binary_authorization ? 1 : 0
  project = var.project_id

  admission_whitelist_patterns {
    name_pattern = "${var.region}-docker.pkg.dev/${var.project_id}/*"
  }

  default_admission_rule {
    evaluation_mode  = "REQUIRE_ATTESTATION"
    enforcement_mode = "ENFORCED_BLOCK_AND_AUDIT_LOG"
    
    require_attestations_by = [
      google_binary_authorization_attestor.prod[0].name
    ]
  }
}

resource "google_binary_authorization_attestor" "prod" {
  count   = var.enable_binary_authorization ? 1 : 0
  name    = "${var.project_name}-${var.environment}-attestor"
  project = var.project_id

  attestation_authority_note {
    note_reference = google_container_analysis_note.attestor[0].name
  }
}

resource "google_container_analysis_note" "attestor" {
  count   = var.enable_binary_authorization ? 1 : 0
  name    = "${var.project_name}-${var.environment}-attestor-note"
  project = var.project_id

  attestation {
    hint {
      human_readable_name = "Production attestor"
    }
  }
}

# Security Command Center sources and findings (production only)
resource "google_scc_source" "custom" {
  count        = var.environment == "prod" && var.organization_id != "" ? 1 : 0
  display_name = "${var.project_name} Custom Findings"
  organization = var.organization_id
  description  = "Custom security findings for ${var.project_name}"
}

# Web Security Scanner
resource "google_compute_security_scan_config" "main" {
  count        = var.enable_web_security_scanner ? 1 : 0
  display_name = "${var.project_name}-${var.environment}-scan"
  project      = var.project_id

  starting_urls = [var.app_url]

  authentication {
    google_account {
      username = var.scan_auth_user
      password = var.scan_auth_password
    }
  }

  schedule {
    interval_duration_days = 7
  }
}

# Secrets for application
resource "google_secret_manager_secret" "app_secrets" {
  for_each  = var.app_secrets
  secret_id = "${var.project_name}-${var.environment}-${each.key}"
  project   = var.project_id

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret_version" "app_secrets" {
  for_each    = var.app_secrets
  secret      = google_secret_manager_secret.app_secrets[each.key].id
  secret_data = each.value
}

# IAM policy for least privilege access
data "google_iam_policy" "least_privilege" {
  binding {
    role = "roles/run.invoker"
    members = [
      "serviceAccount:${google_service_account.security.email}",
    ]
  }

  binding {
    role = "roles/secretmanager.secretAccessor"
    members = [
      "serviceAccount:${google_service_account.security.email}",
    ]
  }
}
