# Firebase Module

This module sets up Firebase services with Auth0 integration for authentication.

## Features

- **Firestore**: Real-time NoSQL database with security rules
- **Firebase Storage**: File storage with access control
- **Firebase Authentication**: Custom token generation from Auth0 tokens
- **Cloud Functions**: Token exchange and user synchronization
- **Security Rules**: Role-based access control using Auth0 claims

## Architecture

```
┌─────────────┐     ┌─────────────┐     ┌──────────────┐
│   Client    │────▶│    Auth0    │────▶│ Token Exchange│
│  (Web/App)  │     │   Service   │     │   Function    │
└─────────────┘     └─────────────┘     └──────────────┘
       │                                         │
       │                                         ▼
       │                                 ┌──────────────┐
       └────────────────────────────────▶│   Firebase   │
                                        │   Services   │
                                        └──────────────┘
```

## Usage

```hcl
module "firebase" {
  source = "../../modules/firebase"

  project_name         = var.project_name
  environment          = var.environment
  project_id           = var.project_id
  region               = var.region
  firestore_location   = "nam5"
  
  # Auth0 configuration
  auth0_domain         = var.auth0_domain
  auth0_api_identifier = var.auth0_api_identifier
  
  # Enable features
  enable_analytics       = true
  enable_crashlytics     = true
  enable_performance     = true
  enable_cloud_messaging = true
  enable_remote_config   = true
  
  labels = var.labels
}
```

## Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| project_name | The name of the project | `string` | n/a | yes |
| environment | The deployment environment | `string` | n/a | yes |
| project_id | The GCP project ID | `string` | n/a | yes |
| region | The GCP region | `string` | `"us-central1"` | no |
| firestore_location | The location for Firestore database | `string` | `"nam5"` | no |
| auth0_domain | Auth0 domain for token exchange | `string` | n/a | yes |
| auth0_api_identifier | Auth0 API identifier | `string` | n/a | yes |
| enable_analytics | Enable Firebase Analytics | `bool` | `true` | no |
| enable_crashlytics | Enable Firebase Crashlytics | `bool` | `true` | no |
| enable_performance | Enable Firebase Performance | `bool` | `true` | no |
| enable_cloud_messaging | Enable FCM | `bool` | `true` | no |
| enable_remote_config | Enable Remote Config | `bool` | `true` | no |

## Outputs

| Name | Description |
|------|-------------|
| firebase_web_config | Firebase configuration for web app |
| firebase_android_config | Firebase configuration for Android app |
| firebase_ios_config | Firebase configuration for iOS app |
| token_exchange_url | URL for Auth0 to Firebase token exchange |
| firestore_database | Firestore database name |
| storage_bucket | Firebase Storage bucket name |

## Security Rules

### Firestore Rules

The module includes comprehensive Firestore security rules that:
- Validate Auth0 tokens and custom claims
- Implement role-based access control (RBAC)
- Support user profiles, posts, chat, and notifications
- Enforce data validation and ownership checks

### Storage Rules

Storage security rules provide:
- User-specific private storage
- Shared file access control
- Public asset management
- File type and size validation

## Token Exchange Function

The token exchange function (`exchangeToken`) handles:
1. Auth0 JWT verification
2. User synchronization with Firebase Auth
3. Custom claims mapping (roles, permissions)
4. Firestore user profile updates
5. Firebase custom token generation

## Setup Instructions

1. **Deploy the module**:
   ```bash
   terraform apply
   ```

2. **Configure Auth0**:
   - Add the token exchange URL to your Auth0 application
   - Set up Auth0 Rules/Actions to include custom claims

3. **Update client applications**:
   - Implement Auth0 authentication flow
   - Exchange Auth0 tokens for Firebase tokens
   - Initialize Firebase with custom tokens

4. **Set up webhooks** (optional):
   - Configure Auth0 webhooks to sync user updates
   - Point to the `auth0Webhook` function URL

## Client Integration Example

```javascript
// 1. Authenticate with Auth0
const auth0Token = await auth0.getAccessToken();

// 2. Exchange for Firebase token
const response = await fetch(tokenExchangeUrl, {
  method: 'POST',
  headers: {
    'Authorization': `Bearer ${auth0Token}`,
    'Content-Type': 'application/json'
  }
});

const { token } = await response.json();

// 3. Sign in to Firebase
await firebase.auth().signInWithCustomToken(token);

// 4. Access Firebase services
const user = firebase.auth().currentUser;
const db = firebase.firestore();
```

## Notes

- The module automatically creates a ZIP file for the Cloud Functions
- Ensure the functions directory is properly packaged before deployment
- Service account keys are stored securely in Secret Manager
- Custom claims from Auth0 are preserved in Firebase tokens
