# GCP Networking Module

# VPC Network
resource "google_compute_network" "main" {
  name                    = "${var.project_name}-${var.environment}-vpc"
  auto_create_subnetworks = false
  project                 = var.project_id
}

# Public Subnet
resource "google_compute_subnetwork" "public" {
  name          = "${var.project_name}-${var.environment}-public"
  ip_cidr_range = var.public_subnet_cidr
  region        = var.region
  network       = google_compute_network.main.id
  project       = var.project_id

  secondary_ip_range {
    range_name    = "pods"
    ip_cidr_range = var.pods_cidr
  }

  secondary_ip_range {
    range_name    = "services"
    ip_cidr_range = var.services_cidr
  }
}

# Private Subnet
resource "google_compute_subnetwork" "private" {
  name                     = "${var.project_name}-${var.environment}-private"
  ip_cidr_range           = var.private_subnet_cidr
  region                  = var.region
  network                 = google_compute_network.main.id
  project                 = var.project_id
  private_ip_google_access = true
}

# Cloud NAT for private subnet
resource "google_compute_router" "nat_router" {
  name    = "${var.project_name}-${var.environment}-router"
  network = google_compute_network.main.id
  region  = var.region
  project = var.project_id
}

resource "google_compute_router_nat" "nat" {
  name                               = "${var.project_name}-${var.environment}-nat"
  router                             = google_compute_router.nat_router.name
  region                             = var.region
  nat_ip_allocate_option            = "AUTO_ONLY"
  source_subnetwork_ip_ranges_to_nat = "LIST_OF_SUBNETWORKS"

  subnetwork {
    name                    = google_compute_subnetwork.private.id
    source_ip_ranges_to_nat = ["ALL_IP_RANGES"]
  }

  log_config {
    enable = true
    filter = "ERRORS_ONLY"
  }
}

# Firewall Rules
resource "google_compute_firewall" "allow_https" {
  name    = "${var.project_name}-${var.environment}-allow-https"
  network = google_compute_network.main.name
  project = var.project_id

  allow {
    protocol = "tcp"
    ports    = ["443"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["https-server"]
}

resource "google_compute_firewall" "allow_http" {
  name    = "${var.project_name}-${var.environment}-allow-http"
  network = google_compute_network.main.name
  project = var.project_id

  allow {
    protocol = "tcp"
    ports    = ["80"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["http-server"]
}

resource "google_compute_firewall" "allow_internal" {
  name    = "${var.project_name}-${var.environment}-allow-internal"
  network = google_compute_network.main.name
  project = var.project_id

  allow {
    protocol = "tcp"
    ports    = ["0-65535"]
  }

  allow {
    protocol = "udp"
    ports    = ["0-65535"]
  }

  allow {
    protocol = "icmp"
  }

  source_ranges = [
    var.public_subnet_cidr,
    var.private_subnet_cidr
  ]
}

# VPC Connector for Cloud Run
resource "google_vpc_access_connector" "connector" {
  name          = "${var.project_name}-${var.environment}-connector"
  ip_cidr_range = var.connector_cidr
  network       = google_compute_network.main.name
  region        = var.region
  project       = var.project_id
}

# Private Service Connection for Cloud SQL
resource "google_compute_global_address" "private_ip_address" {
  name          = "${var.project_name}-${var.environment}-private-ip"
  purpose       = "VPC_PEERING"
  address_type  = "INTERNAL"
  prefix_length = 16
  network       = google_compute_network.main.id
  project       = var.project_id
}

resource "google_service_networking_connection" "private_vpc_connection" {
  network                 = google_compute_network.main.id
  service                 = "servicenetworking.googleapis.com"
  reserved_peering_ranges = [google_compute_global_address.private_ip_address.name]
}
