# Multi-Cloud API Gateway Module

This Terraform module provides a unified interface for deploying API Gateways across multiple cloud providers (AWS, GCP, Azure, OCI, IBM Cloud) in the Nexpo project.

## Features

- **Multi-Cloud Support**: Deploy to AWS, GCP, Azure, OCI, or IBM Cloud with a single configuration
- **Unified Authentication**: Built-in Auth0 JWT validation across all providers with social login integration
- **Social Login Providers**: Support for 12 major social authentication providers with feature flags
- **Security**: Integrated WAF, IP whitelisting, and CORS configuration
- **High Availability**: Multi-region and multi-AZ support where applicable
- **Monitoring**: Built-in logging and monitoring integration
- **Custom Domains**: Support for custom domain names and SSL certificates
- **Feature Flag System**: Granular control over authentication providers and UI components

## Supported Social Login Providers

The API Gateway supports the following social authentication providers, each controlled by feature flags:

| Provider | Feature Flag | Description |
|----------|--------------|-------------|
| 🔍 **Google** | `ENABLE_GOOGLE_LOGIN` | Google OAuth 2.0 authentication |
| 🍎 **Apple** | `ENABLE_APPLE_LOGIN` | Apple Sign-In for iOS/macOS |
| 📘 **Facebook** | `ENABLE_FACEBOOK_LOGIN` | Facebook Login integration |
| 🏢 **Microsoft** | `ENABLE_MICROSOFT_LOGIN` | Microsoft Azure AD authentication |
| ❌ **X (Twitter)** | `ENABLE_X_LOGIN` | X (formerly Twitter) OAuth |
| 🎮 **Discord** | `ENABLE_DISCORD_LOGIN` | Discord OAuth 2.0 |
| 🐙 **GitHub** | `ENABLE_GITHUB_LOGIN` | GitHub OAuth for developers |
| 💼 **LinkedIn** | `ENABLE_LINKEDIN_LOGIN` | LinkedIn professional network |
| 📦 **Amazon** | `ENABLE_AMAZON_LOGIN` | Amazon account authentication |
| 🎵 **Spotify** | `ENABLE_SPOTIFY_LOGIN` | Spotify music platform |
| 💬 **Slack** | `ENABLE_SLACK_LOGIN` | Slack workspace integration |
| 🔴 **Reddit** | `ENABLE_REDDIT_LOGIN` | Reddit community platform |

## Cloud Provider Features

### AWS-Specific Features
- **API Gateway v2 (HTTP API)** with custom domain support
- **VPC with public and private subnets**
- **App Runner VPC Connector** for private service access
- **Security Groups** for network isolation
- **CloudWatch Logs** for API Gateway access logs
- **IAM Roles** with least privilege permissions

### IBM Cloud-Specific Features
- **API Connect** for enterprise API management
- **Code Engine** for serverless container deployment
- **Virtual Private Cloud (VPC)** with subnet isolation
- **Cloud Internet Services** for DDoS protection and CDN
- **Identity and Access Management (IAM)** integration
- **LogDNA** for centralized logging and monitoring

## Basic Usage

### Multi-Cloud Configuration with Social Login

```hcl
module "api_gateway" {
  source = "../../modules/api-gateway"
  
  # General Configuration
  project_name = "myapp"
  environment  = "prod"
  
  # Auth0 Configuration
  auth0_domain   = "your-tenant.auth0.com"
  auth0_audience = "https://api.myapp.com"
  
  # Social Login Feature Flags
  enable_google_login    = true
  enable_apple_login     = true
  enable_facebook_login  = true
  enable_microsoft_login = true
  enable_x_login         = false
  enable_discord_login   = true
  enable_github_login    = true
  enable_linkedin_login  = true
  enable_amazon_login    = false
  enable_spotify_login   = false
  enable_slack_login     = true
  enable_reddit_login    = true
  
  # CORS
  allowed_origins = ["https://app.myapp.com"]
  
  # Provider Configuration (enable as needed)
  enable_aws      = true
  enable_gcp      = false
  enable_azure    = false
  enable_oci      = false
  ENABLE_IBM = true
  
  # AWS Configuration (example)
  aws_region = "us-east-1"
  aws_vpc_cidr = "10.0.0.0/16"
  aws_availability_zones = ["us-east-1a", "us-east-1b"]
  aws_private_subnet_cidrs = ["10.0.1.0/24", "10.0.2.0/24"]
  aws_public_subnet_cidrs  = ["10.0.101.0/24", "10.0.102.0/24"]
  
  # IBM Cloud Configuration (example)
  ibmcloud_region           = "us-south"
  ibmcloud_resource_group   = "default"
  ibmcloud_vpc_name         = "myapp-vpc"
  ibmcloud_subnet_cidrs     = ["10.240.0.0/24", "10.240.1.0/24"]
  ibmcloud_enable_code_engine = true
  
  # GCP Configuration (example)
  gcp_project_id = "your-gcp-project"
  gcp_region    = "us-central1"
  
  # Azure Configuration (example)
  azure_location            = "eastus"
  azure_resource_group_name = "myapp-rg"
  
  # OCI Configuration (example)
  oci_region          = "us-ashburn-1"
  oci_tenancy_ocid    = "ocid1.tenancy.oc1..example"
  oci_compartment_ocid = "ocid1.compartment.oc1..example"
}
```

## Social Login Configuration

### Frontend Integration Example

```typescript
// React/Next.js Social Login Component
import { useAuth0 } from '@auth0/auth0-react';
import { useFeatureFlags } from '../hooks/useFeatureFlags';

export const SocialLoginButtons = () => {
  const { loginWithRedirect } = useAuth0();
  const featureFlags = useFeatureFlags();

  const socialProviders = [
    { 
      name: 'Google', 
      connection: 'google-oauth2', 
      enabled: featureFlags.ENABLE_GOOGLE_LOGIN,
      icon: '🔍',
      className: 'btn-google'
    },
    { 
      name: 'Apple', 
      connection: 'apple', 
      enabled: featureFlags.ENABLE_APPLE_LOGIN,
      icon: '🍎',
      className: 'btn-apple'
    },
    { 
      name: 'Facebook', 
      connection: 'facebook', 
      enabled: featureFlags.ENABLE_FACEBOOK_LOGIN,
      icon: '📘',
      className: 'btn-facebook'
    },
    { 
      name: 'Microsoft', 
      connection: 'windowslive', 
      enabled: featureFlags.ENABLE_MICROSOFT_LOGIN,
      icon: '🏢',
      className: 'btn-microsoft'
    },
    { 
      name: 'X', 
      connection: 'twitter', 
      enabled: featureFlags.ENABLE_X_LOGIN,
      icon: '❌',
      className: 'btn-x'
    },
    { 
      name: 'Discord', 
      connection: 'discord', 
      enabled: featureFlags.ENABLE_DISCORD_LOGIN,
      icon: '🎮',
      className: 'btn-discord'
    },
    { 
      name: 'GitHub', 
      connection: 'github', 
      enabled: featureFlags.ENABLE_GITHUB_LOGIN,
      icon: '🐙',
      className: 'btn-github'
    },
    { 
      name: 'LinkedIn', 
      connection: 'linkedin', 
      enabled: featureFlags.ENABLE_LINKEDIN_LOGIN,
      icon: '💼',
      className: 'btn-linkedin'
    },
    { 
      name: 'Amazon', 
      connection: 'amazon', 
      enabled: featureFlags.ENABLE_AMAZON_LOGIN,
      icon: '📦',
      className: 'btn-amazon'
    },
    { 
      name: 'Spotify', 
      connection: 'spotify', 
      enabled: featureFlags.ENABLE_SPOTIFY_LOGIN,
      icon: '🎵',
      className: 'btn-spotify'
    },
    { 
      name: 'Slack', 
      connection: 'slack', 
      enabled: featureFlags.ENABLE_SLACK_LOGIN,
      icon: '💬',
      className: 'btn-slack'
    },
    { 
      name: 'Reddit', 
      connection: 'reddit', 
      enabled: featureFlags.ENABLE_REDDIT_LOGIN,
      icon: '🔴',
      className: 'btn-reddit'
    }
  ];

  const handleSocialLogin = (connection: string) => {
    loginWithRedirect({
      connection,
      redirectUri: `${window.location.origin}/callback`
    });
  };

  return (
    <div className="social-login-container">
      <h3>Sign in with</h3>
      <div className="social-buttons-grid">
        {socialProviders
          .filter(provider => provider.enabled)
          .map(provider => (
            <button
              key={provider.connection}
              onClick={() => handleSocialLogin(provider.connection)}
              className={`social-btn ${provider.className}`}
              aria-label={`Sign in with ${provider.name}`}
            >
              <span className="social-icon">{provider.icon}</span>
              <span className="social-text">{provider.name}</span>
            </button>
          ))}
      </div>
    </div>
  );
};
```

### React Native Integration Example

```typescript
// React Native Social Login Component
import { useAuth0 } from 'react-native-auth0';
import { useFeatureFlags } from '../hooks/useFeatureFlags';

export const SocialLoginButtons = () => {
  const { authorize } = useAuth0();
  const featureFlags = useFeatureFlags();

  const handleSocialLogin = async (connection: string) => {
    try {
      await authorize({
        connection,
        scope: 'openid profile email'
      });
    } catch (error) {
      console.error('Login failed:', error);
    }
  };

  return (
    <View style={styles.container}>
      <Text style={styles.title}>Sign in with</Text>
      <View style={styles.buttonGrid}>
        {featureFlags.ENABLE_GOOGLE_LOGIN && (
          <TouchableOpacity 
            style={[styles.socialButton, styles.googleButton]}
            onPress={() => handleSocialLogin('google-oauth2')}
          >
            <Text style={styles.buttonText}>🔍 Google</Text>
          </TouchableOpacity>
        )}
        {featureFlags.ENABLE_APPLE_LOGIN && (
          <TouchableOpacity 
            style={[styles.socialButton, styles.appleButton]}
            onPress={() => handleSocialLogin('apple')}
          >
            <Text style={styles.buttonText}>🍎 Apple</Text>
          </TouchableOpacity>
        )}
        {/* Add other providers as needed */}
        {featureFlags.ENABLE_REDDIT_LOGIN && (
          <TouchableOpacity 
            style={[styles.socialButton, styles.redditButton]}
            onPress={() => handleSocialLogin('reddit')}
          >
            <Text style={styles.buttonText}>🔴 Reddit</Text>
          </TouchableOpacity>
        )}
      </View>
    </View>
  );
};
```

## Requirements

| Name | Version |
|------|---------|
| terraform | >= 1.0.0 |
| aws | ~> 4.0 |
| google | ~> 4.0 |
| azurerm | ~> 3.0 |
| oci | ~> 4.0 |
| ibm | ~> 1.45.0 |
| random | >= 3.0 |

## Providers

This module uses the following providers, which should be configured in the root module:

- AWS (when `enable_aws = true`)
- Google Cloud (when `enable_gcp = true`)
- Azure (when `enable_azure = true`)
- OCI (when `enable_oci = true`)
- IBM Cloud (when `ENABLE_IBM = true`)

## Inputs

### General Configuration

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| project_name | Name of the project | `string` | n/a | yes |
| environment | Environment (e.g., dev, staging, prod) | `string` | n/a | yes |
| tags | A map of tags to add to all resources | `map(string)` | `{}` | no |

### Social Login Feature Flags

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| enable_google_login | Enable Google OAuth login | `bool` | `true` | no |
| enable_apple_login | Enable Apple Sign-In | `bool` | `true` | no |
| enable_facebook_login | Enable Facebook Login | `bool` | `true` | no |
| enable_microsoft_login | Enable Microsoft Azure AD login | `bool` | `true` | no |
| enable_x_login | Enable X (Twitter) OAuth login | `bool` | `false` | no |
| enable_discord_login | Enable Discord OAuth login | `bool` | `false` | no |
| enable_github_login | Enable GitHub OAuth login | `bool` | `true` | no |
| enable_linkedin_login | Enable LinkedIn OAuth login | `bool` | `true` | no |
| enable_amazon_login | Enable Amazon account login | `bool` | `false` | no |
| enable_spotify_login | Enable Spotify OAuth login | `bool` | `false` | no |
| enable_slack_login | Enable Slack OAuth login | `bool` | `false` | no |
| enable_reddit_login | Enable Reddit OAuth login | `bool` | `false` | no |

### Authentication

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| auth0_domain | Auth0 domain for JWT validation | `string` | n/a | yes |
| auth0_audience | Auth0 API audience | `string` | n/a | yes |
| allowed_origins | List of allowed CORS origins | `list(string)` | `["*"]` | no |
| enable_waf | Enable Web Application Firewall | `bool` | `true` | no |
| enable_monitoring | Enable CloudWatch/Monitoring | `bool` | `true` | no |
| alert_emails | List of emails for alerts | `list(string)` | `[]` | no |

### Cloud Provider Flags

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| enable_aws | Enable AWS API Gateway | `bool` | `false` | no |
| enable_gcp | Enable GCP API Gateway | `bool` | `false` | no |
| enable_azure | Enable Azure API Management | `bool` | `false` | no |
| enable_oci | Enable OCI API Gateway | `bool` | `false` | no |
| ENABLE_IBM | Enable IBM Cloud API Connect | `bool` | `false` | no |

### IBM Cloud Configuration

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| ibmcloud_region | IBM Cloud region to deploy resources | `string` | `"us-south"` | no |
| ibmcloud_resource_group | IBM Cloud resource group name | `string` | `"default"` | no |
| ibmcloud_vpc_name | Name of the IBM Cloud VPC | `string` | `""` | no |
| ibmcloud_subnet_cidrs | List of subnet CIDR blocks | `list(string)` | `["10.240.0.0/24"]` | no |
| ibmcloud_enable_code_engine | Enable IBM Code Engine for serverless | `bool` | `true` | no |
| ibmcloud_enable_api_connect | Enable IBM API Connect | `bool` | `true` | no |
| ibmcloud_enable_logdna | Enable IBM LogDNA for logging | `bool` | `true` | no |
| ibmcloud_enable_sysdig | Enable IBM Sysdig for monitoring | `bool` | `true` | no |

### AWS Configuration

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| aws_region | AWS region to deploy resources | `string` | `"us-east-1"` | no |
| aws_vpc_cidr | CIDR block for VPC | `string` | `"10.0.0.0/16"` | no |
| aws_availability_zones | List of availability zones | `list(string)` | `[]` | no |
| aws_private_subnet_cidrs | List of private subnet CIDR blocks | `list(string)` | `[]` | no |
| aws_public_subnet_cidrs | List of public subnet CIDR blocks | `list(string)` | `[]` | no |

### GCP Configuration

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| gcp_project_id | GCP project ID | `string` | `""` | no |
| gcp_region | GCP region to deploy resources | `string` | `"us-central1"` | no |

### Azure Configuration

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| azure_location | Azure location to deploy resources | `string` | `"eastus"` | no |
| azure_resource_group_name | Name of the Azure resource group | `string` | `""` | no |
| azure_enable_virtual_network | Enable virtual network integration | `bool` | `false` | no |
| azure_virtual_network_type | Type of virtual network | `string` | `"External"` | no |
| azure_enable_waf | Enable Web Application Firewall | `bool` | `true` | no |

### OCI Configuration

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| oci_region | OCI region to deploy resources | `string` | `"us-ashburn-1"` | no |
| oci_tenancy_ocid | OCID of the OCI tenancy | `string` | `""` | no |
| oci_compartment_ocid | OCID of the OCI compartment | `string` | `""` | no |
| oci_enable_private_endpoint | Enable private endpoint | `bool` | `false` | no |

## Outputs

### General Outputs

| Name | Description |
|------|-------------|
| project_name | The name of the project |
| environment | The environment name |
| enabled_social_providers | List of enabled social login providers |

### AWS-Specific Outputs

| Name | Description |
|------|-------------|
| aws_api_gateway_url | The URL of the AWS API Gateway |
| aws_vpc_link_id | The ID of the VPC Link |
| aws_cloudwatch_log_group_arn | The ARN of the CloudWatch log group |

### IBM Cloud-Specific Outputs

| Name | Description |
|------|-------------|
| ibmcloud_api_connect_url | The URL of the IBM API Connect service |
| ibmcloud_code_engine_url | The URL of the IBM Code Engine application |
| ibmcloud_vpc_id | The ID of the IBM Cloud VPC |
| ibmcloud_logdna_instance_id | The ID of the LogDNA instance |

### GCP-Specific Outputs

| Name | Description |
|------|-------------|
| gcp_api_gateway_url | The URL of the GCP API Gateway |
| gcp_api_gateway_id | The ID of the GCP API Gateway |
| gcp_api_config_id | The ID of the API config |

### Azure-Specific Outputs

| Name | Description |
|------|-------------|
| azure_api_gateway_url | The URL of the Azure API Management |
| azure_apim_id | The ID of the API Management service |
| azure_apim_gateway_url | The Gateway URL of the API Management service |
| azure_apim_developer_portal_url | The Developer Portal URL of the API Management service |

### OCI-Specific Outputs

| Name | Description |
|------|-------------|
| oci_api_gateway_url | The URL of the OCI API Gateway |
| oci_api_gateway_id | The OCID of the API Gateway |
| oci_api_deployment_id | The OCID of the API Deployment |
| oci_waf_policy_id | The OCID of the WAF Policy (if enabled) |

## Deployment

1. Initialize Terraform:
   ```bash
   terraform init
   ```

2. Review the execution plan:
   ```bash
   terraform plan
   ```

3. Apply the configuration:
   ```bash
   terraform apply
   ```

## Clean Up

To destroy all resources:

```bash
terraform destroy
```

## Security Considerations

### General Security

- Restrict `allowed_origins` to specific domains in production
- Enable WAF for additional protection
- Use private endpoints for internal services
- Rotate credentials and certificates regularly
- Enable detailed logging and monitoring
- Implement proper OAuth scopes for social login providers
- Use HTTPS-only redirects for authentication flows

### Social Login Security

- **OAuth 2.0 Best Practices**: Implement PKCE for mobile apps
- **Scope Limitation**: Request minimal required permissions
- **State Parameter**: Use anti-CSRF state parameters
- **Token Validation**: Verify JWT signatures and claims
- **Session Management**: Implement secure session handling

### Provider-Specific Security

- **AWS**: 
  - Enable AWS WAF and Shield for DDoS protection
  - Use VPC endpoints for private connectivity
  - Enable VPC Flow Logs for network monitoring
  
- **IBM Cloud**:
  - Enable IBM Cloud Internet Services for DDoS protection
  - Use Private Service Endpoints for secure connectivity
  - Enable Activity Tracker for audit logging
  - Implement IBM Cloud Security Advisor recommendations
  
- **GCP**:
  - Enable Cloud Armor for DDoS protection
  - Use VPC Service Controls for data isolation
  - Enable Cloud Audit Logging
  
- **Azure**:
  - Enable Azure DDoS Protection Standard
  - Use Private Link for private connectivity
  - Enable Azure Security Center
  
- **OCI**:
  - Enable OCI WAF and DDoS Protection
  - Use Service Gateway for private connectivity
  - Enable OCI Security Monitoring and Analytics

## Feature Flag Management

### Environment Variables

```bash
# Social Login Feature Flags
ENABLE_GOOGLE_LOGIN=true
ENABLE_APPLE_LOGIN=true
ENABLE_FACEBOOK_LOGIN=true
ENABLE_MICROSOFT_LOGIN=true
ENABLE_X_LOGIN=false
ENABLE_DISCORD_LOGIN=false
ENABLE_GITHUB_LOGIN=true
ENABLE_LINKEDIN_LOGIN=true
ENABLE_AMAZON_LOGIN=false
ENABLE_SPOTIFY_LOGIN=false
ENABLE_SLACK_LOGIN=false
ENABLE_REDDIT_LOGIN=false

# Cloud Provider Flags
ENABLE_AWS=true
ENABLE_GCP=false
ENABLE_AZURE=false
ENABLE_OCI=false
ENABLE_IBM=true
```

### Runtime Configuration

```typescript
// Feature flag hook for React/React Native
export const useFeatureFlags = () => {
  return {
    ENABLE_GOOGLE_LOGIN: process.env.NEXT_PUBLIC_ENABLE_GOOGLE_LOGIN === 'true',
    ENABLE_APPLE_LOGIN: process.env.NEXT_PUBLIC_ENABLE_APPLE_LOGIN === 'true',
    ENABLE_FACEBOOK_LOGIN: process.env.NEXT_PUBLIC_ENABLE_FACEBOOK_LOGIN === 'true',
    ENABLE_MICROSOFT_LOGIN: process.env.NEXT_PUBLIC_ENABLE_MICROSOFT_LOGIN === 'true',
    ENABLE_X_LOGIN: process.env.NEXT_PUBLIC_ENABLE_X_LOGIN === 'true',
    ENABLE_DISCORD_LOGIN: process.env.NEXT_PUBLIC_ENABLE_DISCORD_LOGIN === 'true',
    ENABLE_GITHUB_LOGIN: process.env.NEXT_PUBLIC_ENABLE_GITHUB_LOGIN === 'true',
    ENABLE_LINKEDIN_LOGIN: process.env.NEXT_PUBLIC_ENABLE_LINKEDIN_LOGIN === 'true',
    ENABLE_AMAZON_LOGIN: process.env.NEXT_PUBLIC_ENABLE_AMAZON_LOGIN === 'true',
    ENABLE_SPOTIFY_LOGIN: process.env.NEXT_PUBLIC_ENABLE_SPOTIFY_LOGIN === 'true',
    ENABLE_SLACK_LOGIN: process.env.NEXT_PUBLIC_ENABLE_SLACK_LOGIN === 'true',
    ENABLE_REDDIT_LOGIN: process.env.NEXT_PUBLIC_ENABLE_REDDIT_LOGIN === 'true',
  };
};
```

## Related Modules

- [Auth Service](../auth) - Authentication and authorization service with Auth0 integration
- [User Service](../user) - User management service
- [API Service](../api) - Core API service
- [Networking](../networking) - Shared networking resources
- [Monitoring](../monitoring) - Centralized monitoring and alerting

## Migration from Previous Versions

### From v1.x to v2.0

- The module now supports multiple cloud providers including IBM Cloud
- AWS-specific variables have been namespaced with `aws_` prefix
- New providers (GCP, Azure, OCI, IBM Cloud) have their own variable namespaces
- Social login feature flags have been added for granular control
- Review the updated variable names in the documentation above

### From v2.x to v3.0

- Added comprehensive social login provider support
- Introduced feature flag system for authentication providers
- Added IBM Cloud support with API Connect and Code Engine
- Enhanced security configurations for all providers
- Updated frontend integration examples

## Support and Contributing

For issues and feature requests, please open an issue on GitHub. Pull requests are welcome!
