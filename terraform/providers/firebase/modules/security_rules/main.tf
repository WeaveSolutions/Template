# ==============================================================================
# FIREBASE SECURITY RULES MODULE
# ==============================================================================

# ==============================================================================
# FIRESTORE SECURITY RULES
# ==============================================================================

resource "google_firebaserules_ruleset" "firestore_rules" {
  count   = var.firestore_rules_file != "" ? 1 : 0
  project = var.project_id
  
  source {
    files {
      content = file(var.firestore_rules_file)
      name    = "firestore.rules"
    }
  }
}

resource "google_firebaserules_release" "firestore_release" {
  count        = var.firestore_rules_file != "" ? 1 : 0
  project      = var.project_id
  name         = "cloud.firestore"
  ruleset_name = google_firebaserules_ruleset.firestore_rules[0].name
}

# ==============================================================================
# STORAGE SECURITY RULES
# ==============================================================================

resource "google_firebaserules_ruleset" "storage_rules" {
  count   = var.storage_rules_file != "" ? 1 : 0
  project = var.project_id
  
  source {
    files {
      content = file(var.storage_rules_file)
      name    = "storage.rules"
    }
  }
}

resource "google_firebaserules_release" "storage_release" {
  count        = var.storage_rules_file != "" ? 1 : 0
  project      = var.project_id
  name         = "firebase.storage/${var.project_id}.appspot.com"
  ruleset_name = google_firebaserules_ruleset.storage_rules[0].name
}

# ==============================================================================
# DEFAULT SECURITY RULES (if custom rules not provided)
# ==============================================================================

# Default Firestore rules (restrictive)
resource "google_firebaserules_ruleset" "default_firestore_rules" {
  count   = var.firestore_rules_file == "" && var.create_default_rules ? 1 : 0
  project = var.project_id
  
  source {
    files {
      content = <<-EOT
        rules_version = '2';
        service cloud.firestore {
          match /databases/{database}/documents {
            // Users can read and write their own documents
            match /users/{userId} {
              allow read, write: if request.auth != null && request.auth.uid == userId;
            }
            
            // Public read access to posts, authenticated write access
            match /posts/{document} {
              allow read: if true;
              allow write: if request.auth != null;
            }
            
            // Default deny all other access
            match /{document=**} {
              allow read, write: if false;
            }
          }
        }
      EOT
      name    = "firestore.rules"
    }
  }
}

resource "google_firebaserules_release" "default_firestore_release" {
  count        = var.firestore_rules_file == "" && var.create_default_rules ? 1 : 0
  project      = var.project_id
  name         = "cloud.firestore"
  ruleset_name = google_firebaserules_ruleset.default_firestore_rules[0].name
}

# Default Storage rules (restrictive)
resource "google_firebaserules_ruleset" "default_storage_rules" {
  count   = var.storage_rules_file == "" && var.create_default_rules ? 1 : 0
  project = var.project_id
  
  source {
    files {
      content = <<-EOT
        rules_version = '2';
        service firebase.storage {
          match /b/{bucket}/o {
            // Users can upload to their own directory
            match /users/{userId}/{allPaths=**} {
              allow read, write: if request.auth != null && request.auth.uid == userId;
            }
            
            // Public read access to public directory
            match /public/{allPaths=**} {
              allow read: if true;
              allow write: if request.auth != null;
            }
            
            // Default deny all other access
            match /{allPaths=**} {
              allow read, write: if false;
            }
          }
        }
      EOT
      name    = "storage.rules"
    }
  }
}

resource "google_firebaserules_release" "default_storage_release" {
  count        = var.storage_rules_file == "" && var.create_default_rules ? 1 : 0
  project      = var.project_id
  name         = "firebase.storage/${var.project_id}.appspot.com"
  ruleset_name = google_firebaserules_ruleset.default_storage_rules[0].name
}
