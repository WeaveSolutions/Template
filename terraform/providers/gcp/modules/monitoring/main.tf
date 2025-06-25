# GCP Monitoring Module

# Enable required APIs
resource "google_project_service" "monitoring_api" {
  service = "monitoring.googleapis.com"
  project = var.project_id
}

resource "google_project_service" "logging_api" {
  service = "logging.googleapis.com"
  project = var.project_id
}

# Log sink for centralized logging
resource "google_logging_project_sink" "main" {
  name                   = "${var.project_name}-${var.environment}-sink"
  destination            = "logging.googleapis.com/projects/${var.project_id}/logs"
  filter                 = "resource.type=\"cloud_run_revision\" OR resource.type=\"cloudsql_database\""
  unique_writer_identity = true
  project                = var.project_id
}

# Notification channel for email
resource "google_monitoring_notification_channel" "email" {
  for_each     = toset(var.alert_email_endpoints)
  display_name = "Email - ${each.value}"
  type         = "email"
  project      = var.project_id

  labels = {
    email_address = each.value
  }
}

# Notification channel for SMS (production)
resource "google_monitoring_notification_channel" "sms" {
  for_each     = var.environment == "prod" ? toset(var.alert_sms_endpoints) : []
  display_name = "SMS - ${each.value}"
  type         = "sms"
  project      = var.project_id

  labels = {
    number = each.value
  }
}

# Uptime check
resource "google_monitoring_uptime_check_config" "app" {
  display_name = "${var.project_name}-${var.environment}-uptime"
  timeout      = "10s"
  period       = "60s"
  project      = var.project_id

  http_check {
    path         = var.health_check_path
    port         = "443"
    use_ssl      = true
    validate_ssl = true
  }

  monitored_resource {
    type = "uptime_url"
    labels = {
      host       = var.app_domain
      project_id = var.project_id
    }
  }

  selected_regions = var.uptime_check_regions
}

# Alert policy for uptime
resource "google_monitoring_alert_policy" "uptime" {
  display_name = "${var.project_name}-${var.environment}-uptime-alert"
  combiner     = "OR"
  project      = var.project_id

  conditions {
    display_name = "Uptime check failure"
    condition_threshold {
      filter          = "metric.type=\"monitoring.googleapis.com/uptime_check/check_passed\" AND resource.type=\"uptime_url\" AND metric.label.\"check_id\"=\"${google_monitoring_uptime_check_config.app.uptime_check_id}\""
      duration        = "300s"
      comparison      = "COMPARISON_LT"
      threshold_value = 1
      
      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_NEXT_OLDER"
        cross_series_reducer = "REDUCE_COUNT_FALSE"
        group_by_fields      = ["resource.label.*"]
      }
    }
  }

  notification_channels = concat(
    [for ch in google_monitoring_notification_channel.email : ch.id],
    [for ch in google_monitoring_notification_channel.sms : ch.id]
  )

  alert_strategy {
    auto_close = "86400s"
  }
}

# Alert policy for Cloud Run errors
resource "google_monitoring_alert_policy" "cloud_run_errors" {
  display_name = "${var.project_name}-${var.environment}-cloud-run-errors"
  combiner     = "OR"
  project      = var.project_id

  conditions {
    display_name = "High error rate"
    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/request_count\" AND metric.label.\"response_code_class\"=\"5xx\""
      duration        = "300s"
      comparison      = "COMPARISON_GT"
      threshold_value = 10
      
      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }
    }
  }

  notification_channels = [for ch in google_monitoring_notification_channel.email : ch.id]
}

# Alert policy for database connections
resource "google_monitoring_alert_policy" "db_connections" {
  display_name = "${var.project_name}-${var.environment}-db-connections"
  combiner     = "OR"
  project      = var.project_id

  conditions {
    display_name = "High database connections"
    condition_threshold {
      filter          = "resource.type=\"cloudsql_database\" AND metric.type=\"cloudsql.googleapis.com/database/postgresql/num_backends\""
      duration        = "300s"
      comparison      = "COMPARISON_GT"
      threshold_value = var.db_connection_threshold
      
      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = [for ch in google_monitoring_notification_channel.email : ch.id]
}

# Custom dashboard
resource "google_monitoring_dashboard" "main" {
  dashboard_json = jsonencode({
    displayName = "${var.project_name}-${var.environment}-dashboard"
    mosaicLayout = {
      columns = 12
      tiles = [
        {
          width  = 6
          height = 4
          widget = {
            title = "Cloud Run Request Rate"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/request_count\""
                    aggregation = {
                      alignmentPeriod  = "60s"
                      perSeriesAligner = "ALIGN_RATE"
                    }
                  }
                }
                plotType = "LINE"
              }]
            }
          }
        },
        {
          width  = 6
          height = 4
          widget = {
            title = "Cloud Run Latency"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/request_latencies\""
                    aggregation = {
                      alignmentPeriod  = "60s"
                      perSeriesAligner = "ALIGN_PERCENTILE_95"
                    }
                  }
                }
                plotType = "LINE"
              }]
            }
          }
        },
        {
          width  = 6
          height = 4
          yPos   = 4
          widget = {
            title = "Database Connections"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type=\"cloudsql_database\" AND metric.type=\"cloudsql.googleapis.com/database/postgresql/num_backends\""
                    aggregation = {
                      alignmentPeriod  = "60s"
                      perSeriesAligner = "ALIGN_MEAN"
                    }
                  }
                }
                plotType = "LINE"
              }]
            }
          }
        },
        {
          width  = 6
          height = 4
          yPos   = 4
          widget = {
            title = "Error Rate"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/request_count\" AND metric.label.\"response_code_class\"=\"5xx\""
                    aggregation = {
                      alignmentPeriod  = "60s"
                      perSeriesAligner = "ALIGN_RATE"
                    }
                  }
                }
                plotType = "LINE"
              }]
            }
          }
        }
      ]
    }
  })
  project = var.project_id
}

# Budget alert
resource "google_billing_budget" "monthly" {
  billing_account = var.billing_account_id
  display_name    = "${var.project_name}-${var.environment}-budget"

  budget_filter {
    projects = ["projects/${var.project_id}"]
  }

  amount {
    specified_amount {
      currency_code = "USD"
      units         = var.monthly_budget_amount
    }
  }

  threshold_rules {
    threshold_percent = 0.5
    spend_basis       = "CURRENT_SPEND"
  }

  threshold_rules {
    threshold_percent = 0.9
    spend_basis       = "CURRENT_SPEND"
  }

  threshold_rules {
    threshold_percent = 1.0
    spend_basis       = "CURRENT_SPEND"
  }

  all_updates_rule {
    monitoring_notification_channels = [for ch in google_monitoring_notification_channel.email : ch.id]
    disable_default_iam_recipients   = false
  }
}
