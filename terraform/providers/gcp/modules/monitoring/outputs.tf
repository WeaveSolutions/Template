output "dashboard_url" {
  description = "URL of the monitoring dashboard"
  value       = "https://console.cloud.google.com/monitoring/dashboards/custom/${google_monitoring_dashboard.main.id}?project=${var.project_id}"
}

output "email_notification_channels" {
  description = "Email notification channel IDs"
  value       = [for ch in google_monitoring_notification_channel.email : ch.id]
}

output "sms_notification_channels" {
  description = "SMS notification channel IDs"
  value       = [for ch in google_monitoring_notification_channel.sms : ch.id]
}

output "uptime_check_id" {
  description = "ID of the uptime check"
  value       = google_monitoring_uptime_check_config.app.uptime_check_id
}
