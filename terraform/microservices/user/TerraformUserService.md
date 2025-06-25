# User Microservice Terraform Module

## Overview
This module manages the infrastructure for the User Microservice in the Nexpo platform. The User Microservice handles user profile management, account linking, and user-specific data operations. It integrates with the Central Rank Authority (CRA) for federated identity management and Auth0 for authentication.

## 🔐 Integration with Authentication
- **Auth0**: All requests to the User Microservice are authenticated via Auth0 JWTs, validated through the Kong API Gateway.
- **Central Rank Authority (CRA)**: The CRA manages root identities and account linking across multiple OAuth providers. The User Microservice queries CRA for user identity data.

## 🏗️ Architecture
- **User Profile Management**: Stores and updates user profile information such as display name, primary email, and settings.
- **Account Linking**: Facilitates linking of multiple OAuth provider accounts (Google, GitHub, Discord, etc.) to a single root identity via CRA.
- **Data Backend**: Uses Auth0's built-in user database, Firebase Firestore, Supabase PostgreSQL, or MongoDB for storing user-specific data, with security rules validating Auth0 claims.
- **Kong API Gateway**: Routes requests to the User Microservice, enforcing rate limiting and JWT validation.

## 🚀 Deployment
1. Ensure Auth0 and CRA are configured in the authentication module.
2. Set environment variables for the User Microservice in `terraform.tfvars`.
3. Deploy using Terraform to provision necessary resources (e.g., Firebase/Supabase/MongoDB collections, API endpoints).
4. Refer to the comprehensive documentation at `docs/Authentication/CRA.md` for integration details with Auth0 and CRA.

## 🔒 Security
- **JWT Validation**: All API requests are validated through Kong using Auth0 tokens.
- **Role-Based Access Control (RBAC)**: Permissions managed via Auth0 roles, ensuring users can only access their own data unless authorized.
- **Data Encryption**: Sensitive user data is encrypted at rest in Firebase/Supabase.
- **Vault Integration**: Secrets and API keys are stored securely in HashiCorp Vault.

## 🛠️ Configuration Variables
- `user_service_enabled`: Boolean to enable/disable the User Microservice deployment (default: true).
- `data_backend`: String to specify the backend for user data storage (`auth0`, `firebase`, `supabase`, or `mongodb`, default: `auth0`).
- `api_endpoint`: String for the base URL path in Kong for routing to this service (default: `/api/v1/users`).

## 📤 Outputs
- `user_service_endpoint`: The API endpoint for accessing the User Microservice.
- `user_data_collections`: Details of the Firebase/Supabase collections or tables created for user data.

## 🐛 Troubleshooting
- **Authentication Errors**: Verify Kong JWT plugin configuration and ensure Auth0 tokens include necessary claims for user access.
- **Data Access Issues**: Check security rules in Firebase/Supabase to ensure they validate Auth0 claims correctly.
- **Account Linking Failures**: Ensure CRA is properly configured to manage root identities and provider connections.
- Review the detailed guide at `docs/Authentication/CRA.md` for solutions to common integration issues.

## 🌐 API Endpoints
The User Microservice exposes the following endpoints through Kong API Gateway:
- `GET /api/v1/users/{id}`: Retrieve user profile by ID.
- `PUT /api/v1/users/{id}`: Update user profile information.
- `POST /api/v1/users/link-account`: Initiate linking of an additional OAuth provider to the user's root identity.
- `GET /api/v1/users/{id}/providers`: List connected OAuth providers for a user.

## 💡 Notes
- This module assumes the authentication infrastructure (Auth0 and CRA) is already deployed.
- Ensure rate limiting and caching policies in Kong are configured to handle user data requests efficiently.
- For multi-tenant applications, configure organization-specific data segregation in the backend.
