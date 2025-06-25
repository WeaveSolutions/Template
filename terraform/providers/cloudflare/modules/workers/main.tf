/**
 * # Cloudflare Workers Module
 *
 * This module manages Cloudflare Workers for serverless edge computing.
 *
 * ## Features
 * - Worker script deployment
 * - KV namespace management
 * - Worker routes configuration
 * - Cron triggers
 * - Environment variables
 */

# Worker scripts
resource "cloudflare_worker_script" "script" {
  for_each = { for script in var.scripts : script.name => script }

  name       = each.value.name
  content    = each.value.content
  
  module     = lookup(each.value, "module", false)
  
  dynamic "kv_namespace_binding" {
    for_each = lookup(each.value, "kv_namespace_bindings", [])
    content {
      name         = kv_namespace_binding.value.name
      namespace_id = kv_namespace_binding.value.namespace_id
    }
  }
  
  dynamic "secret_text_binding" {
    for_each = lookup(each.value, "secret_text_bindings", [])
    content {
      name = secret_text_binding.value.name
      text = secret_text_binding.value.text
    }
  }
  
  dynamic "plain_text_binding" {
    for_each = lookup(each.value, "plain_text_bindings", [])
    content {
      name = plain_text_binding.value.name
      text = plain_text_binding.value.text
    }
  }
  
  dynamic "webassembly_binding" {
    for_each = lookup(each.value, "webassembly_bindings", [])
    content {
      name    = webassembly_binding.value.name
      module  = webassembly_binding.value.module
    }
  }
  
  dynamic "service_binding" {
    for_each = lookup(each.value, "service_bindings", [])
    content {
      name        = service_binding.value.name
      service     = service_binding.value.service
      environment = lookup(service_binding.value, "environment", null)
    }
  }
  
  dynamic "r2_bucket_binding" {
    for_each = lookup(each.value, "r2_bucket_bindings", [])
    content {
      name        = r2_bucket_binding.value.name
      bucket_name = r2_bucket_binding.value.bucket_name
    }
  }
  
  dynamic "analytics_engine_binding" {
    for_each = lookup(each.value, "analytics_engine_bindings", [])
    content {
      name    = analytics_engine_binding.value.name
      dataset = analytics_engine_binding.value.dataset
    }
  }
  
  dynamic "queue_binding" {
    for_each = lookup(each.value, "queue_bindings", [])
    content {
      name        = queue_binding.value.name
      queue_name  = queue_binding.value.queue_name
      binding_id  = lookup(queue_binding.value, "binding_id", null)
    }
  }

  # Note: This is a list in some scenarios
  tags = lookup(each.value, "tags", [])
  
  compatibility_date  = lookup(each.value, "compatibility_date", "2023-01-01")
  compatibility_flags = lookup(each.value, "compatibility_flags", [])
  
  # Worker limits
  usage_model = lookup(each.value, "usage_model", "bundled") # Or "unbound"
}

# KV Namespaces
resource "cloudflare_workers_kv_namespace" "namespace" {
  for_each = { for namespace in var.kv_namespaces : namespace.title => namespace }
  
  title = each.value.title
  account_id = var.cloudflare_account_id
}

# Worker routes
resource "cloudflare_worker_route" "route" {
  for_each = { for route in var.worker_routes : "${route.zone_id}-${route.pattern}" => route }
  
  zone_id     = each.value.zone_id
  pattern     = each.value.pattern
  script_name = each.value.script_name
}

# Worker cron triggers
resource "cloudflare_worker_cron_trigger" "trigger" {
  for_each = { for trigger in var.cron_triggers : "${trigger.script_name}-${trigger.cron}" => trigger }
  
  account_id  = var.cloudflare_account_id
  script_name = each.value.script_name
  schedules   = each.value.schedules
}

# Worker KV entries
resource "cloudflare_workers_kv" "kv" {
  for_each = { for kv in var.kv_entries : "${kv.namespace_id}-${kv.key}" => kv }
  
  account_id   = var.cloudflare_account_id
  namespace_id = each.value.namespace_id
  key          = each.value.key
  value        = each.value.value
}
