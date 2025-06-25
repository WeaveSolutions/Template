# ==============================================================================
# FIREBASE SECURITY RULES MODULE - OUTPUTS
# ==============================================================================

output "rules_status" {
  description = "Firebase Security Rules deployment status"
  value = {
    project_id = var.project_id
    
    firestore_rules = {
      custom_rules_deployed = var.firestore_rules_file != ""
      rules_file           = var.firestore_rules_file
      ruleset_name         = var.firestore_rules_file != "" ? google_firebaserules_ruleset.firestore_rules[0].name : (var.create_default_rules ? google_firebaserules_ruleset.default_firestore_rules[0].name : null)
      release_name         = var.firestore_rules_file != "" ? google_firebaserules_release.firestore_release[0].name : (var.create_default_rules ? google_firebaserules_release.default_firestore_release[0].name : null)
    }
    
    storage_rules = {
      custom_rules_deployed = var.storage_rules_file != ""
      rules_file           = var.storage_rules_file
      ruleset_name         = var.storage_rules_file != "" ? google_firebaserules_ruleset.storage_rules[0].name : (var.create_default_rules ? google_firebaserules_ruleset.default_storage_rules[0].name : null)
      release_name         = var.storage_rules_file != "" ? google_firebaserules_release.storage_release[0].name : (var.create_default_rules ? google_firebaserules_release.default_storage_release[0].name : null)
    }
  }
}

output "firestore_ruleset_info" {
  description = "Firestore ruleset information"
  value = var.firestore_rules_file != "" ? {
    name        = google_firebaserules_ruleset.firestore_rules[0].name
    create_time = google_firebaserules_ruleset.firestore_rules[0].create_time
  } : (var.create_default_rules ? {
    name        = google_firebaserules_ruleset.default_firestore_rules[0].name
    create_time = google_firebaserules_ruleset.default_firestore_rules[0].create_time
  } : null)
}

output "storage_ruleset_info" {
  description = "Storage ruleset information"
  value = var.storage_rules_file != "" ? {
    name        = google_firebaserules_ruleset.storage_rules[0].name
    create_time = google_firebaserules_ruleset.storage_rules[0].create_time
  } : (var.create_default_rules ? {
    name        = google_firebaserules_ruleset.default_storage_rules[0].name
    create_time = google_firebaserules_ruleset.default_storage_rules[0].create_time
  } : null)
}

output "firestore_release_info" {
  description = "Firestore rules release information"
  value = var.firestore_rules_file != "" ? {
    name         = google_firebaserules_release.firestore_release[0].name
    ruleset_name = google_firebaserules_release.firestore_release[0].ruleset_name
    create_time  = google_firebaserules_release.firestore_release[0].create_time
    update_time  = google_firebaserules_release.firestore_release[0].update_time
  } : (var.create_default_rules ? {
    name         = google_firebaserules_release.default_firestore_release[0].name
    ruleset_name = google_firebaserules_release.default_firestore_release[0].ruleset_name
    create_time  = google_firebaserules_release.default_firestore_release[0].create_time
    update_time  = google_firebaserules_release.default_firestore_release[0].update_time
  } : null)
}

output "storage_release_info" {
  description = "Storage rules release information"
  value = var.storage_rules_file != "" ? {
    name         = google_firebaserules_release.storage_release[0].name
    ruleset_name = google_firebaserules_release.storage_release[0].ruleset_name
    create_time  = google_firebaserules_release.storage_release[0].create_time
    update_time  = google_firebaserules_release.storage_release[0].update_time
  } : (var.create_default_rules ? {
    name         = google_firebaserules_release.default_storage_release[0].name
    ruleset_name = google_firebaserules_release.default_storage_release[0].ruleset_name
    create_time  = google_firebaserules_release.default_storage_release[0].create_time
    update_time  = google_firebaserules_release.default_storage_release[0].update_time
  } : null)
}

output "rules_testing_urls" {
  description = "URLs for testing security rules"
  value = {
    firestore_simulator = "https://console.firebase.google.com/project/${var.project_id}/firestore/rules"
    storage_simulator   = "https://console.firebase.google.com/project/${var.project_id}/storage/rules"
    rules_playground    = "https://firebase.google.com/docs/rules/simulator"
  }
}

output "default_rules_content" {
  description = "Content of default security rules (if created)"
  value = var.create_default_rules ? {
    firestore_rules_applied = var.firestore_rules_file == ""
    storage_rules_applied   = var.storage_rules_file == ""
    rules_summary = {
      firestore = "Users can read/write their own data, public read for posts"
      storage   = "Users can access their own directory, public read for /public"
    }
  } : null
}
