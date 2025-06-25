# Firebase Terraform Provider

A comprehensive and modular Terraform configuration for managing Firebase **data services**, designed to complement the existing Supabase Terraform setup and Auth0 authentication in the Nexpo project.

## Overview

This Firebase Terraform provider enables Infrastructure as Code (IaC) for Firebase **data and backend services**, providing automated provisioning and consistent multi-environment deployments. The modular architecture integrates with **Auth0 as the primary identity provider** through token exchange patterns.

## Architecture

```
firebase/
├── main.tf                    # Root Firebase configuration
├── variables.tf               # Input variables
├── outputs.tf                 # Output values
├── terraform.tfvars.example   # Example configuration
└── modules/
    ├── firestore/             # Cloud Firestore (Data Layer)
    ├── storage/               # Cloud Storage (File Storage)
    ├── functions/             # Cloud Functions (Token Exchange, Webhooks)
    ├── realtime_database/     # Realtime Database (Live Features)
    ├── security_rules/        # Security Rules (Auth0 Integration)
    └── app_hosting/           # Firebase App Hosting (Static Sites)
```

## Auth0 + Firebase Integration

```
┌─────────────┐     ┌─────────────┐     ┌─────────────────┐
│   Client    │────▶│    Auth0    │────▶│ CRA Service    │
│ (Web/Mobile)│     │ (Identity)  │     │ (Token Exchange)│
└─────────────┘     └─────────────┘     └─────────────────┘
       │                                          │
       │                                          ▼
       └──────────────────────────────────▶┌─────────────────┐
                                            │ Firebase        │
                                            │ (Data Services) │
                                            └─────────────────┘
```

## Features

### 🔥 **Firebase as Data Backend**
- **No Native Authentication**: Uses Auth0 custom tokens via CRA service
- **Data Services Only**: Firestore, Storage, Functions, Realtime DB
- **Auth0 Integration**: Security rules validate Auth0 JWT claims

### 🗃️ **Firestore Module**
- **Database Creation**: Firestore database with configurable location and settings
- **Index Management**: Composite indexes for efficient queries
- **Sample Data**: Optional development seed data
- **Backup Schedules**: Automated backup with retention policies

### 📁 **Storage Module**
- **Multi-Bucket Support**: Different buckets for uploads, public assets, etc.
- **CORS Configuration**: Customizable cross-origin resource sharing
- **Lifecycle Rules**: Automated archiving and deletion policies
- **IAM Bindings**: Firebase service account and public access management

### ⚡ **Cloud Functions Module**
- **Multiple Triggers**: HTTP, Pub/Sub, Firestore, Storage, Scheduled triggers
- **Runtime Support**: Node.js, Python, Go, Java runtimes
- **Environment Variables**: Secure configuration management
- **IAM Permissions**: Granular access control

### 📊 **Realtime Database Module**
- **Instance Management**: Realtime Database instance creation
- **Multi-Region Support**: us-central1, europe-west1, asia-southeast1
- **Connection Info**: Client SDK configuration outputs

### 🔒 **Security Rules Module**
- **Firestore Rules**: Database security rule deployment
- **Storage Rules**: File storage security rules
- **Default Rules**: Secure defaults if custom rules not provided
- **Rule Testing**: Console URLs for rule simulation

### 🚀 **App Hosting Module**
- **GitHub Integration**: Continuous deployment from GitHub repositories
- **Build Management**: Automated builds and traffic allocation
- **Multi-Environment**: Backend configuration for different environments

## Quick Start

### 1. Prerequisites

- Terraform >= 1.0
- Google Cloud SDK
- Firebase CLI (optional, for local testing)
- Valid Google Cloud billing account

### 2. Configuration

```bash
# Copy example configuration
cp terraform.tfvars.example terraform.tfvars

# Edit with your project settings
nano terraform.tfvars
```

### 3. Initialize and Deploy

```bash
# Initialize Terraform
terraform init

# Plan deployment
terraform plan

# Apply configuration
terraform apply
```

## Configuration Examples

### Basic Configuration
```hcl
# terraform.tfvars
gcp_project_id = "nexpo-firebase-dev"
project_name   = "Nexpo Development"
gcp_region     = "us-central1"

enable_web_app     = true
enable_android_app = true
enable_ios_app     = true

firestore_config = {
  location_id = "nam5"
  database_type = "FIRESTORE_NATIVE"
}
```

### Multi-Environment Setup
```hcl
# Development
gcp_project_id = "nexpo-dev"
firestore_config = {
  location_id = "nam5"
  database_type = "FIRESTORE_NATIVE"
}

# Production  
gcp_project_id = "nexpo-prod"
firestore_config = {
  location_id = "nam5"
  database_type = "FIRESTORE_NATIVE"
  concurrency_mode = "PESSIMISTIC"
}
```

## Module Documentation

### Firestore Module
```hcl
module "firestore" {
  source = "./modules/firestore"
  
  project_id   = var.gcp_project_id
  location_id  = var.firestore_config.location_id
  database_type = var.firestore_config.database_type
  
  indexes = [
    {
      collection = "users"
      fields = [
        { field_path = "email", order = "ASCENDING" },
        { field_path = "created_at", order = "DESCENDING" }
      ]
    }
  ]
}
```

### Storage Module
```hcl
module "storage" {
  source = "./modules/storage"
  
  project_id = var.gcp_project_id
  
  buckets = [
    {
      name = "uploads"
      location = "US"
      public_read_access = false
      cors_origins = ["https://nexpo.com"]
    }
  ]
}
```

## Security Best Practices

### 1. **Sensitive Variables**
All OAuth secrets and private keys are marked as sensitive:
```hcl
variable "google_client_secret" {
  description = "Google OAuth client secret"
  type        = string
  sensitive   = true
}
```

### 2. **Default Security Rules**
Restrictive security rules are applied by default:
```javascript
// Firestore Rules
service cloud.firestore {
  match /databases/{database}/documents {
    match /users/{userId} {
      allow read, write: if request.auth != null && request.auth.uid == userId;
    }
  }
}
```

### 3. **IAM Least Privilege**
Service accounts are granted minimal necessary permissions:
```hcl
resource "google_project_iam_binding" "functions_sa_bindings" {
  for_each = toset([
    "roles/cloudsql.client",
    "roles/datastore.user", 
    "roles/firebase.admin"
  ])
  # ...
}
```

## Environment-Specific Configurations

### Development
- Public read access for testing
- Sample data seeding enabled
- Relaxed security rules
- Shorter backup retention

### Production
- Strict security rules
- Long backup retention
- Performance optimizations
- Enhanced monitoring

## Integration Examples

### Next.js Integration
```javascript
// firebase.config.js
import { initializeApp } from 'firebase/app';
import { getAuth } from 'firebase/auth';
import { getFirestore } from 'firebase/firestore';

const firebaseConfig = {
  // Values from Terraform outputs
  projectId: process.env.NEXT_PUBLIC_FIREBASE_PROJECT_ID,
  authDomain: process.env.NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN,
  databaseURL: process.env.NEXT_PUBLIC_FIREBASE_DATABASE_URL,
  storageBucket: process.env.NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET,
};

const app = initializeApp(firebaseConfig);
export const auth = getAuth(app);
export const db = getFirestore(app);
```

### React Native Integration
```javascript
// firebase.config.js
import { initializeApp } from '@react-native-firebase/app';
import auth from '@react-native-firebase/auth';
import firestore from '@react-native-firebase/firestore';

const firebaseConfig = {
  // Values from Terraform outputs
  // Same configuration as web
};

export { auth, firestore };
```

## CI/CD Integration

### GitHub Actions Example
```yaml
name: Deploy Firebase Infrastructure

on:
  push:
    branches: [main]
    paths: ['terraform/providers/firebase/**']

jobs:
  terraform:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v2
        
      - name: Authenticate to Google Cloud
        uses: google-github-actions/auth@v1
        with:
          credentials_json: ${{ secrets.GCP_SA_KEY }}
          
      - name: Terraform Init
        run: terraform init
        working-directory: terraform/providers/firebase
        
      - name: Terraform Plan
        run: terraform plan
        working-directory: terraform/providers/firebase
        
      - name: Terraform Apply
        if: github.ref == 'refs/heads/main'
        run: terraform apply -auto-approve
        working-directory: terraform/providers/firebase
```

## Monitoring and Observability

### Available Outputs
- **Database Connection Info**: Client SDK configuration
- **Function URLs**: Cloud Function HTTP endpoints
- **Storage Bucket URLs**: File upload endpoints
- **Console URLs**: Firebase Console links for monitoring

### Logging Integration
```hcl
# Example: Function logs filter
output "function_logs_url" {
  value = "https://console.cloud.google.com/logs/query;query=resource.type%3D%22cloud_function%22?project=${var.project_id}"
}
```

## Troubleshooting

### Common Issues

1. **Project Creation Fails**
   - Verify billing account is active
   - Check quota limits in Google Cloud Console
   - Ensure proper IAM permissions

2. **OAuth Configuration Issues**
   - Verify client IDs and secrets
   - Check OAuth consent screen setup
   - Validate redirect URLs

3. **Firestore Index Creation**
   - Monitor index build progress in console
   - Verify composite index requirements
   - Check query patterns

### Debug Commands
```bash
# Check Terraform state
terraform show

# Validate configuration
terraform validate

# Refresh state
terraform refresh

# Show specific resource
terraform show 'module.auth.google_identity_platform_default_supported_idp_config.google'
```

## Migration from Supabase

If migrating from existing Supabase setup:

1. **Data Export**: Export Supabase data using their CLI
2. **Authentication Migration**: Use Auth0 custom tokens via CRA service
3. **Schema Migration**: Recreate Firestore collections and indexes
4. **Function Migration**: Adapt Supabase Edge Functions to Cloud Functions
5. **Client SDK Updates**: Update application code to use Firebase SDKs

## Contributing

When extending this Firebase Terraform setup:

1. Follow the existing modular architecture
2. Add comprehensive variable descriptions
3. Include example configurations
4. Update documentation
5. Test across multiple environments

## Support

- **Firebase Documentation**: https://firebase.google.com/docs
- **Terraform Google Provider**: https://registry.terraform.io/providers/hashicorp/google
- **Nexpo Project Issues**: Create issue in project repository

---

This Firebase Terraform provider complements the existing Supabase setup, providing you with flexible backend options for the Nexpo project. Choose the backend that best fits your specific use case and requirements.
