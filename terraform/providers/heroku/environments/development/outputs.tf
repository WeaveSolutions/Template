# Heroku Development Environment Outputs

output "web_app_name" {
  description = "Name of the web application"
  value       = heroku_app.web_app.name
}

output "web_app_url" {
  description = "URL of the web application"
  value       = "https://${heroku_app.web_app.name}.herokuapp.com"
}

output "web_app_git_url" {
  description = "Git URL for deploying to web application"
  value       = heroku_app.web_app.git_url
}

output "api_app_name" {
  description = "Name of the API application"
  value       = var.enable_api_app ? heroku_app.api_app[0].name : null
}

output "api_app_url" {
  description = "URL of the API application"
  value       = var.enable_api_app ? "https://${heroku_app.api_app[0].name}.herokuapp.com" : null
}

output "api_app_git_url" {
  description = "Git URL for deploying to API application"
  value       = var.enable_api_app ? heroku_app.api_app[0].git_url : null
}

output "database_url" {
  description = "PostgreSQL database connection URL"
  value       = heroku_addon.database.config_vars["DATABASE_URL"]
  sensitive   = true
}

output "redis_url" {
  description = "Redis connection URL"
  value       = var.enable_redis ? heroku_addon.redis[0].config_vars["REDIS_URL"] : null
  sensitive   = true
}

output "web_custom_domain" {
  description = "Custom domain for web application"
  value       = var.custom_domain != "" ? heroku_domain.web_domain[0].hostname : null
}

output "api_custom_domain" {
  description = "Custom domain for API application"
  value       = var.enable_api_app && var.custom_domain != "" ? heroku_domain.api_domain[0].hostname : null
}

output "database_addon_id" {
  description = "Database add-on ID"
  value       = heroku_addon.database.id
}

output "redis_addon_id" {
  description = "Redis add-on ID"
  value       = var.enable_redis ? heroku_addon.redis[0].id : null
}

output "logging_addon_id" {
  description = "Logging add-on ID"
  value       = var.enable_logging ? heroku_addon.logging[0].id : null
}

output "environment_info" {
  description = "Development environment information"
  value = {
    environment = "development"
    region      = var.heroku_region
    stack       = var.heroku_stack
    apps = {
      web = heroku_app.web_app.name
      api = var.enable_api_app ? heroku_app.api_app[0].name : null
    }
    addons = {
      database = heroku_addon.database.plan
      redis    = var.enable_redis ? heroku_addon.redis[0].plan : null
      logging  = var.enable_logging ? heroku_addon.logging[0].plan : null
    }
  }
}
