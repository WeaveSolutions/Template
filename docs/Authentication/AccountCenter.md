  # Account Center

## Table of Contents

- [Overview](#overview)
- [Requirements](#requirements)
  - [Authentication Management](#authentication-management)
  - [Account Operations](#account-operations)
  - [Data & Permissions](#data--permissions)
  - [Security](#security)
- [Authentication Methods](#authentication-methods)
  - [Passkeys/WebAuthn Integration](#passkeyswebauthn-integration)
- [OAuth Provider Setup](#oauth-provider-setup)
  - [Google OAuth 2.0 (Primary)](#google-oauth-20-primary)
  - [Apple](#apple)
  - [Facebook](#facebook)
  - [Microsoft](#microsoft)
  - [X (Twitter)](#x-twitter)
  - [GitHub](#github)
  - [Discord](#discord)
  - [LinkedIn](#linkedin)
  - [Spotify](#spotify)
  - [Amazon](#amazon)
  - [Slack](#slack)
  - [Reddit](#reddit)
  - [CodeRabbit](#coderabbit)
- [Technical Implementation](#technical-implementation)
  - [Conditional Formatting](#conditional-formatting)
  - [Schema to Properties Visualization](#schema-to-properties-visualization)
  - [Schema to Data Object Visualization](#schema-to-data-object-visualization)
- [Testing](#testing)
  - [Manual Testing](#manual-testing)
  - [Automated Testing](#automated-testing)
- [Common Issues](#common-issues)
- [Integration Points](#integration-points)

## Overview

The Account Center is a central hub for managing user authentication, account settings, and connected services within the Nexpo template. It provides a comprehensive interface for users to control their identity and data across all supported OAuth providers and authentication methods.

## Requirements

The Account Center architecture must support the following fundamental capabilities:

### Authentication Management
- **Email or Google Login**: Users can authenticate with any email or Google account
- **Account Linking**: Connect multiple OAuth providers to a single primary account
- **Provider Management**: Link and unlink authentication providers as needed
- **Security Controls**: Manage authentication preferences and security settings

### Account Operations
- **Profile Management**: Update and maintain account information
- **Account Deletion**: Secure account termination with data cleanup
- **Account Recovery**: Password reset and account recovery workflows
- **Audit Trail**: View complete account activity history

### Data & Permissions
- **Information Access**: View all account data and connected services
- **Permission Control**: Manage granular permissions for connected providers
- **Subscription Management**: View and manage active subscriptions
- **Role Administration**: Display user roles and access levels (RBAC)

### Security
- **Authentication Preferences**: Manage authentication methods and security settings
- **Security Controls**: Manage authentication preferences and security settings
- **Scope Minimization**: Only request necessary scopes for functionality
- **Token Storage**: Use HashiCorp Vault for sensitive token storage
- **Consent Management**: Implement clear consent flows for scope requests
- **Regular Audits**: Monitor connected accounts and revoke unused connections
- **Compliance**: Ensure GDPR/CCPA compliance for data handling

### Passkeys/WebAuthn Integration

Passkeys work with both methods:

**Email/Password + Passkeys**
- Users sign up with email/password initially
- After verification, they can register a passkey for future logins
- Subsequent logins use biometric authentication (Face ID, Touch ID, Windows Hello)
- Fallback to email/password remains available

**Google OAuth + Passkeys**
- Users sign up with Google OAuth as primary identity
- Can register passkeys associated with their Google account email
- Passkey authentication bypasses the need for Google OAuth flow on subsequent visits

## OAuth Provider Setup

### Google OAuth 2.0 (Primary)

**Auth0 Setup**:
```
Name: google-oauth2
Client ID: [From Google Cloud Console]
Client Secret: [From Google Cloud Console]
```

**Required Scopes**:
```
openid           # OpenID Connect authentication
profile          # Basic profile information (name, picture)
email            # Email address
```

### Apple

**Auth0 Setup**:
```
Name: apple
Team ID: [From Apple Developer Account]
Key ID: [From Apple Developer Account]
Private Key: [From Apple Developer Account]
```

**Required Scopes**:
```
name     # User's name (optional)
email    # User's email address
```

### Facebook

**Auth0 Setup**:
```
Name: facebook
App ID: [From Facebook App Dashboard]
App Secret: [From Facebook App Dashboard]
```

**Required Scopes**:
```
public_profile   # Basic profile information
email           # Email address
```

### Microsoft

**Auth0 Setup**:
```
Name: windowslive
Client ID: [From Azure App Registration]
Client Secret: [From Azure App Registration]
```

**Required Scopes**:
```
wl.basic         # Basic profile information
wl.emails        # Email access
```

### X (Twitter)

**Auth0 Setup**:
```
Name: twitter
Consumer Key: [From Twitter Developer Portal]
Consumer Secret: [From Twitter Developer Portal]
```

**Required Scopes**:
```
tweet.read       # Read tweets
users.read       # Read user information
```

### GitHub

**Auth0 Setup**:
```
Name: github
Client ID: [From GitHub OAuth App]
Client Secret: [From GitHub OAuth App]
```

**Required Scopes**:
```
user:email       # Email access
read:user        # User information
```

### Discord

**Auth0 Setup**:
```
Name: discord
Client ID: [From Discord Application]
Client Secret: [From Discord Application]
```

**Required Scopes**:
```
identify         # Basic account information
email           # Email address
```

### LinkedIn

**Auth0 Setup**:
```
Name: linkedin
Client ID: [From LinkedIn App]
Client Secret: [From LinkedIn App]
```

**Required Scopes**:
```
r_liteprofile    # Basic profile information
r_emailaddress   # Email address
```

### Spotify

**Auth0 Setup**:
```
Name: spotify
Client ID: [From Spotify App Dashboard]
Client Secret: [From Spotify App Dashboard]
```

**Required Scopes**:
```
user-read-email  # Email address
user-read-private # Profile information
```

### Amazon

**Auth0 Setup**:
```
Name: amazon
Client ID: [From Amazon Developer Console]
Client Secret: [From Amazon Developer Console]
```

**Required Scopes**:
```
profile          # Basic profile information
```

### Slack

**Auth0 Setup**:
```
Name: slack
Client ID: [From Slack App]
Client Secret: [From Slack App]
```

**Required Scopes**:
```
users.profile:read   # User profile information
users:read.email     # Email address
```

### Reddit

**Auth0 Setup**:
```
Name: reddit
Client ID: [From Reddit App Preferences]
Client Secret: [From Reddit App Preferences]
```

**Required Scopes**:
```
identity         # User identity information
```

### CodeRabbit

**Custom OAuth Provider Setup**:
```
Name: coderabbit
Authorization URL: https://api.coderabbit.ai/oauth/authorize
Token URL: https://api.coderabbit.ai/oauth/token
Client ID: [From CodeRabbit Dashboard]
Client Secret: [From CodeRabbit Dashboard]
```

**Required Scopes**:
```
profile:read         # Basic profile information
repositories:read    # Repository access
reviews:read         # Code review data
```

## Technical Implementation

### Conditional Formatting

Use if/then statements to apply the schema's values to the actual program logic for the account center.

For example:
```javascript
if (schema.suspended) {
  // disable account operations
}
```

### Schema to Properties Visualization

We need a system to apply schema values to frontend components. Show a side panel with properties and values from the schema data.

This lets developers understand what's happening in the schema and how it's being used in the application.

### Schema to Data Object Visualization

Can we use MindSpace to visualize the CRA for our own accounts?

This means the JSON format gets converted to a visual interface so users can view data objects in a more user-friendly way.

## Testing

### Manual Testing

1. **Test Google Primary Signup**
   - Navigate to your login page
   - Click "Continue with Google"
   - Complete OAuth flow
   - Verify account creation in Auth0 Dashboard

2. **Test Account Linking**
   - Sign up with email/password
   - Navigate to Account Center
   - Connect additional social provider
   - Verify provider appears in connected accounts

3. **Test JWT Tokens**
   - Inspect JWT tokens for provider information
   - Verify scopes in token claims
   - Test token refresh functionality

### Automated Testing

```javascript
// Example test for Google OAuth flow
describe('Google OAuth Integration', () => {
  test('should create account with Google provider', async () => {
    const user = await authenticateWithGoogle();
    expect(user.identities).toContainEqual({
      provider: 'google-oauth2',
      connection: 'google-oauth2'
    });
  });
  
  test('should populate account center schema', async () => {
    const accountCenter = await getAccountCenter(user.user_id);
    expect(accountCenter.connected_providers.count).toBe(1);
    expect(accountCenter.connected_providers.nodes).toHaveProperty('provider_google_oauth2');
  });
});
```

## Common Issues

1. **Invalid Redirect URI**
   - Verify callback URL matches Auth0 configuration
   - Check for trailing slashes or HTTP vs HTTPS

2. **Scope Permission Denied**
   - Review requested scopes in provider console
   - Ensure app approval for sensitive scopes

3. **Token Refresh Failures**
   - Verify refresh token storage and rotation
   - Check token expiration handling

4. **Account Linking Conflicts**
   - Implement proper duplicate account detection
   - Handle email conflicts between providers

## Integration Points

- **Auth0 Integration**: Seamless integration with Auth0 identity management
- **OAuth Provider Support**: Support for all 13+ configured OAuth providers
- **Kong Gateway**: API gateway integration for secure data access
- **Database Abstraction**: Multi-database support through unified interface
- **CRA System**: Integration with Central Rank Authority system