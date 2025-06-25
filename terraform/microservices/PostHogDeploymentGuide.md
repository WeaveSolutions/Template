# PostHog Analytics Deployment Guide

This guide provides step-by-step instructions for deploying PostHog analytics integration across all Nexpo microservices using the Terraform modules.

## Prerequisites

### Required Tools
- Terraform >= 0.14
- PostHog account with organization access
- AWS CLI configured (for SSM Parameter Store)
- kubectl configured (for Kubernetes secrets)
- Nomad CLI configured (for Nomad integration)

### Required Information
- PostHog Organization ID
- PostHog API Key (for provider authentication)
- AWS Account ID and region
- Kubernetes cluster access
- Nomad cluster access

## Deployment Overview

The PostHog integration consists of:

1. **Shared Base Module**: Common PostHog resources and configurations
2. **Service-Specific Modules**: Auth, User, Payments, and API Gateway modules
3. **Cross-Service Integration**: Unified analytics and feature flags
4. **Secret Management**: Secure storage of API keys and webhook secrets

## Step 1: Prepare PostHog Account

### 1.1 Create PostHog Organization
```bash
# Login to PostHog dashboard
# Navigate to Organization Settings
# Note your Organization ID for Terraform configuration
```

### 1.2 Generate API Key
```bash
# In PostHog dashboard:
# Settings → API Keys → Create new key
# Scope: Admin (for Terraform provider)
# Note the API key for Terraform provider configuration
```

## Step 2: Configure Terraform Provider

### 2.1 Create Provider Configuration

Create `terraform/providers.tf`:

```hcl
terraform {
  required_providers {
    posthog = {
      source  = "PostHog/posthog"
      version = "~> 0.1.0"
    }
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.0"
    }
    nomad = {
      source  = "hashicorp/nomad"
      version = "~> 1.4"
    }
  }
}

provider "posthog" {
  api_key         = var.posthog_api_key
  organization_id = var.posthog_organization_id
  host           = "https://app.posthog.com"
}

provider "aws" {
  region = var.aws_region
}

provider "kubernetes" {
  config_path = "~/.kube/config"
}

provider "nomad" {
  address = "http://localhost:4646"
}
```

### 2.2 Create Variables File

Create `terraform/variables.tf`:

```hcl
variable "posthog_api_key" {
  description = "PostHog API key for Terraform provider"
  type        = string
  sensitive   = true
}

variable "posthog_organization_id" {
  description = "PostHog organization ID"
  type        = string
}

variable "aws_region" {
  description = "AWS region for secret storage"
  type        = string
  default     = "us-west-2"
}

variable "environment" {
  description = "Environment name (prod, staging, dev)"
  type        = string
  default     = "prod"
}

variable "project_name" {
  description = "Project name for resource naming"
  type        = string
  default     = "nexpo"
}

variable "common_tags" {
  description = "Common tags for all resources"
  type        = map(string)
  default = {
    Project     = "nexpo"
    ManagedBy   = "terraform"
    Component   = "analytics"
  }
}
```

## Step 3: Deploy Shared PostHog Module

### 3.1 Create Main Configuration

Create `terraform/main.tf`:

```hcl
# Shared PostHog module for common resources
module "shared_posthog" {
  source = "./shared/posthog"
  
  # Base configuration
  enable_posthog          = true
  project_name           = var.project_name
  environment            = var.environment
  posthog_organization_id = var.posthog_organization_id
  
  # Common features
  enable_analytics       = true
  enable_feature_flags   = true
  enable_custom_events   = true
  enable_webhooks        = true
  
  # Privacy settings
  enable_ip_anonymization = true
  data_retention_days     = 365
  
  # Performance settings
  sampling_rate     = 1.0
  batch_size        = 50
  flush_interval_ms = 5000
  
  # AWS integration
  aws_region             = var.aws_region
  use_aws_ssm           = true
  ssm_parameter_prefix  = "/nexpo/posthog"
  
  common_tags = var.common_tags
}
```

### 3.2 Deploy Shared Module

```bash
# Navigate to terraform directory
cd terraform

# Initialize Terraform
terraform init

# Plan deployment
terraform plan -var="posthog_api_key=your-api-key" \
                -var="posthog_organization_id=your-org-id"

# Apply configuration
terraform apply -var="posthog_api_key=your-api-key" \
                 -var="posthog_organization_id=your-org-id"
```

## Step 4: Deploy Service-Specific Modules

### 4.1 API Gateway Module

Add to `terraform/main.tf`:

```hcl
# API Gateway PostHog integration
module "api_gateway_posthog" {
  source = "./microservices/api-gateway/modules/posthog"
  
  # Base configuration
  enable_posthog              = true
  project_name               = var.project_name
  environment                = var.environment
  posthog_organization_id    = var.posthog_organization_id
  
  # API Gateway specific features
  enable_request_analytics   = true
  enable_performance_monitoring = true
  enable_error_tracking      = true
  
  # Rate limiting and monitoring
  track_rate_limits         = true
  track_api_usage          = true
  track_response_times     = true
  
  # Integration
  shared_project_id = module.shared_posthog.posthog_project_id
  
  common_tags = var.common_tags
  
  depends_on = [module.shared_posthog]
}
```

### 4.2 Auth Service Module

Add to `terraform/main.tf`:

```hcl
# Auth service PostHog integration
module "auth_posthog" {
  source = "./microservices/auth/modules/posthog"
  
  # Base configuration
  enable_posthog              = true
  project_name               = var.project_name
  environment                = var.environment
  posthog_organization_id    = var.posthog_organization_id
  
  # Auth-specific features
  enable_social_login_analytics = true
  enable_security_monitoring    = true
  enable_onboarding_analytics   = true
  
  # Social login tracking
  track_google_login    = true
  track_apple_login     = true
  track_facebook_login  = true
  track_microsoft_login = true
  track_github_login    = true
  
  # Security features
  track_failed_logins  = true
  track_mfa_events     = true
  track_session_events = true
  
  # Integration
  parent_project_id = module.api_gateway_posthog.posthog_project_id
  
  common_tags = var.common_tags
  
  depends_on = [module.api_gateway_posthog]
}
```

### 4.3 User Service Module

Add to `terraform/main.tf`:

```hcl
# User service PostHog integration
module "user_posthog" {
  source = "./microservices/user/modules/posthog"
  
  # Base configuration
  enable_posthog              = true
  project_name               = var.project_name
  environment                = var.environment
  posthog_organization_id    = var.posthog_organization_id
  
  # User-specific features
  enable_lifecycle_analytics = true
  enable_profile_analytics   = true
  enable_engagement_tracking = true
  enable_gdpr_compliance     = true
  
  # Lifecycle tracking
  track_profile_creation   = true
  track_profile_updates    = true
  track_profile_completion = true
  track_user_preferences   = true
  
  # Privacy and compliance
  enable_consent_tracking = true
  
  # Integration
  parent_project_id        = module.api_gateway_posthog.posthog_project_id
  integrate_with_auth     = true
  auth_project_id         = module.auth_posthog.posthog_project_id
  
  common_tags = var.common_tags
  
  depends_on = [module.api_gateway_posthog, module.auth_posthog]
}
```

### 4.4 Payments Service Module

Add to `terraform/main.tf`:

```hcl
# Payments service PostHog integration
module "payments_posthog" {
  source = "./microservices/payments/modules/posthog"
  
  # Base configuration
  enable_posthog              = true
  project_name               = var.project_name
  environment                = var.environment
  posthog_organization_id    = var.posthog_organization_id
  
  # Payments-specific features
  enable_revenue_tracking      = true
  enable_subscription_analytics = true
  enable_fraud_detection      = true
  enable_churn_prediction     = true
  
  # Payment provider tracking
  track_stripe_events    = true
  track_paypal_events    = true
  track_apple_pay_events = true
  track_google_pay_events = true
  
  # Business intelligence
  enable_revenue_forecasting = true
  enable_cohort_analysis     = true
  enable_ltv_calculation     = true
  enable_mrr_tracking        = true
  
  # Compliance
  enable_pci_compliance = true
  data_retention_days   = 2555  # 7 years for financial data
  
  # Integration
  parent_project_id     = module.api_gateway_posthog.posthog_project_id
  integrate_with_user   = true
  user_project_id       = module.user_posthog.posthog_project_id
  
  common_tags = var.common_tags
  
  depends_on = [module.api_gateway_posthog, module.user_posthog]
}
```

## Step 5: Deploy All Services

### 5.1 Complete Deployment

```bash
# Plan complete deployment
terraform plan -var="posthog_api_key=your-api-key" \
                -var="posthog_organization_id=your-org-id"

# Apply all modules
terraform apply -var="posthog_api_key=your-api-key" \
                 -var="posthog_organization_id=your-org-id"
```

### 5.2 Verify Deployment

```bash
# Check Terraform state
terraform state list

# Verify PostHog projects created
terraform output

# Check AWS SSM parameters
aws ssm describe-parameters --filters "Key=Name,Values=/nexpo/posthog"
```

## Step 6: Configure Application Integration

### 6.1 Extract Environment Variables

Create `scripts/extract-env-vars.sh`:

```bash
#!/bin/bash

# Extract PostHog configuration for each service
echo "# API Gateway PostHog Configuration"
terraform output -json api_gateway_posthog_env_vars | jq -r 'to_entries[] | "\(.key)=\(.value)"'

echo "# Auth Service PostHog Configuration"
terraform output -json auth_posthog_env_vars | jq -r 'to_entries[] | "\(.key)=\(.value)"'

echo "# User Service PostHog Configuration"
terraform output -json user_posthog_env_vars | jq -r 'to_entries[] | "\(.key)=\(.value)"'

echo "# Payments Service PostHog Configuration"
terraform output -json payments_posthog_env_vars | jq -r 'to_entries[] | "\(.key)=\(.value)"'
```

### 6.2 Create Application Secrets

#### Option A: Kubernetes Secrets

Create `scripts/create-k8s-secrets.sh`:

```bash
#!/bin/bash

# Create secrets for each service
kubectl create secret generic api-gateway-posthog-config \
  --from-env-file=<(terraform output -json api_gateway_posthog_env_vars | jq -r 'to_entries[] | "\(.key)=\(.value)"')

kubectl create secret generic auth-posthog-config \
  --from-env-file=<(terraform output -json auth_posthog_env_vars | jq -r 'to_entries[] | "\(.key)=\(.value)"')

kubectl create secret generic user-posthog-config \
  --from-env-file=<(terraform output -json user_posthog_env_vars | jq -r 'to_entries[] | "\(.key)=\(.value)"')

kubectl create secret generic payments-posthog-config \
  --from-env-file=<(terraform output -json payments_posthog_env_vars | jq -r 'to_entries[] | "\(.key)=\(.value)"')
```

#### Option B: Nomad with Vault Integration

Create `scripts/create-nomad-secrets.sh`:

```bash
#!/bin/bash

# Store PostHog configuration in Vault for Nomad jobs
vault_path="secret/nexpo/posthog"

# API Gateway configuration
terraform output -json api_gateway_posthog_env_vars | jq -r 'to_entries[] | "vault kv put \($vault_path)/api-gateway \(.key)=\(.value)"' | bash

# Auth service configuration
terraform output -json auth_posthog_env_vars | jq -r 'to_entries[] | "vault kv put \($vault_path)/auth \(.key)=\(.value)"' | bash

# User service configuration
terraform output -json user_posthog_env_vars | jq -r 'to_entries[] | "vault kv put \($vault_path)/user \(.key)=\(.value)"' | bash

# Payments service configuration
terraform output -json payments_posthog_env_vars | jq -r 'to_entries[] | "vault kv put \($vault_path)/payments \(.key)=\(.value)"' | bash
```

#### Option C: Nomad Job Files with Environment Variables

Create `nomad-jobs/api-gateway.nomad`:

```hcl
job "api-gateway" {
  region = "global"
  type   = "service"
  
  group "api-gateway" {
    count = 3
    
    # Vault integration for PostHog secrets
    vault {
      policies = ["nexpo-posthog-read"]
    }
    
    task "api-gateway" {
      driver = "docker"
      
      config {
        image = "nexpo/api-gateway:latest"
        ports = ["http"]
      }
      
      # Load PostHog configuration from Vault
      template {
        data = <<EOH
{{ with secret "secret/nexpo/posthog/api-gateway" }}
{{- range $key, $value := .Data.data }}
{{ $key }}="{{ $value }}"
{{- end }}
{{ end }}
EOH
        destination = "secrets/posthog.env"
        env         = true
      }
      
      # Alternative: Direct environment variables
      env {
        POSTHOG_PROJECT_API_KEY = "{{ with secret \"secret/nexpo/posthog/api-gateway\" }}{{ .Data.data.POSTHOG_PROJECT_API_KEY }}{{ end }}"
        POSTHOG_HOST = "{{ with secret \"secret/nexpo/posthog/api-gateway\" }}{{ .Data.data.POSTHOG_HOST }}{{ end }}"
        POSTHOG_PROJECT_ID = "{{ with secret \"secret/nexpo/posthog/api-gateway\" }}{{ .Data.data.POSTHOG_PROJECT_ID }}{{ end }}"
      }
      
      service {
        name = "api-gateway"
        port = "http"
        
        check {
          type     = "http"
          path     = "/health"
          interval = "10s"
          timeout  = "3s"
        }
      }
      
      resources {
        cpu    = 500
        memory = 512
        
        network {
          port "http" {
            static = 8080
          }
        }
      }
    }
  }
}
```

Create `nomad-jobs/auth-service.nomad`:

```hcl
job "auth-service" {
  region = "global"
  type   = "service"
  
  group "auth" {
    count = 2
    
    vault {
      policies = ["nexpo-posthog-read"]
    }
    
    task "auth" {
      driver = "docker"
      
      config {
        image = "nexpo/auth-service:latest"
        ports = ["http"]
      }
      
      template {
        data = <<EOH
{{ with secret "secret/nexpo/posthog/auth" }}
# PostHog Configuration for Auth Service
{{- range $key, $value := .Data.data }}
{{ $key }}="{{ $value }}"
{{- end }}
{{ end }}
EOH
        destination = "secrets/posthog.env"
        env         = true
      }
      
      service {
        name = "auth-service"
        port = "http"
        
        check {
          type     = "http"
          path     = "/health"
          interval = "10s"
          timeout  = "3s"
        }
      }
      
      resources {
        cpu    = 300
        memory = 256
        
        network {
          port "http" {}
        }
      }
    }
  }
}
```

Create `nomad-jobs/user-service.nomad`:

```hcl
job "user-service" {
  region = "global"
  type   = "service"
  
  group "user" {
    count = 2
    
    vault {
      policies = ["nexpo-posthog-read"]
    }
    
    task "user" {
      driver = "docker"
      
      config {
        image = "nexpo/user-service:latest"
        ports = ["http"]
      }
      
      template {
        data = <<EOH
{{ with secret "secret/nexpo/posthog/user" }}
# PostHog Configuration for User Service
{{- range $key, $value := .Data.data }}
{{ $key }}="{{ $value }}"
{{- end }}
{{ end }}
EOH
        destination = "secrets/posthog.env"
        env         = true
      }
      
      service {
        name = "user-service"
        port = "http"
        
        check {
          type     = "http"
          path     = "/health"
          interval = "10s"
          timeout  = "3s"
        }
      }
      
      resources {
        cpu    = 400
        memory = 384
        
        network {
          port "http" {}
        }
      }
    }
  }
}
```

Create `nomad-jobs/payments-service.nomad`:

```hcl
job "payments-service" {
  region = "global"
  type   = "service"
  
  group "payments" {
    count = 2
    
    vault {
      policies = ["nexpo-posthog-read"]
    }
    
    task "payments" {
      driver = "docker"
      
      config {
        image = "nexpo/payments-service:latest"
        ports = ["http"]
      }
      
      template {
        data = <<EOH
{{ with secret "secret/nexpo/posthog/payments" }}
# PostHog Configuration for Payments Service
{{- range $key, $value := .Data.data }}
{{ $key }}="{{ $value }}"
{{- end }}
{{ end }}
EOH
        destination = "secrets/posthog.env"
        env         = true
      }
      
      service {
        name = "payments-service"
        port = "http"
        
        check {
          type     = "http"
          path     = "/health"
          interval = "10s"
          timeout  = "3s"
        }
      }
      
      resources {
        cpu    = 400
        memory = 512
        
        network {
          port "http" {}
        }
      }
    }
  }
}
```

#### Vault Policies for Nomad Integration

Create `vault-policies/nexpo-posthog-read.hcl`:

```hcl
# Policy for reading PostHog configuration from Vault
path "secret/data/nexpo/posthog/*" {
  capabilities = ["read"]
}

path "secret/metadata/nexpo/posthog/*" {
  capabilities = ["read"]
}
```

Apply the Vault policy:

```bash
# Create the policy
vault policy write nexpo-posthog-read vault-policies/nexpo-posthog-read.hcl

# Create a token role for Nomad
vault write auth/token/roles/nexpo-posthog \
    allowed_policies="nexpo-posthog-read" \
    token_explicit_max_ttl=0 \
    name="nexpo-posthog" \
    orphan=true \
    token_period=259200 \
    renewable=true
```

#### Deploy Nomad Jobs

Create `scripts/deploy-nomad-services.sh`:

```bash
#!/bin/bash

# Deploy all Nexpo services with PostHog integration
echo "Deploying Nexpo services to Nomad..."

# Deploy API Gateway
nomad job run nomad-jobs/api-gateway.nomad
echo "API Gateway deployed"

# Deploy Auth Service
nomad job run nomad-jobs/auth-service.nomad
echo "Auth Service deployed"

# Deploy User Service
nomad job run nomad-jobs/user-service.nomad
echo "User Service deployed"

# Deploy Payments Service
nomad job run nomad-jobs/payments-service.nomad
echo "Payments Service deployed"

# Check deployment status
echo "Checking deployment status..."
nomad status api-gateway
nomad status auth-service
nomad status user-service
nomad status payments-service

echo "All services deployed successfully!"
```

## Step 7: Application Code Integration

### 7.1 Node.js Integration Example

Create `src/lib/analytics.js`:

```javascript
const PostHog = require('posthog-node');

class NexpoAnalytics {
  constructor(serviceName) {
    this.serviceName = serviceName;
    this.posthog = new PostHog(process.env.POSTHOG_PROJECT_API_KEY, {
      host: process.env.POSTHOG_HOST,
      flushAt: parseInt(process.env.POSTHOG_BATCH_SIZE),
      flushInterval: parseInt(process.env.POSTHOG_FLUSH_INTERVAL)
    });
  }

  async trackEvent(userId, eventName, properties = {}) {
    const isEnabled = process.env.POSTHOG_FEATURE_FLAGS_ENABLED === 'true';
    
    if (!isEnabled) return;

    this.posthog.capture({
      distinctId: userId,
      event: eventName,
      properties: {
        ...properties,
        service: this.serviceName,
        environment: process.env.NODE_ENV,
        timestamp: new Date().toISOString()
      }
    });
  }

  async isFeatureEnabled(flagName, userId) {
    return await this.posthog.isFeatureEnabled(flagName, userId);
  }

  async identifyUser(userId, properties = {}) {
    this.posthog.identify({
      distinctId: userId,
      properties: {
        ...properties,
        service: this.serviceName
      }
    });
  }

  async shutdown() {
    await this.posthog.shutdown();
  }
}

module.exports = NexpoAnalytics;
```

### 7.2 Service-Specific Usage

For Auth Service (`src/services/auth/analytics.js`):

```javascript
const NexpoAnalytics = require('../../lib/analytics');

class AuthAnalytics extends NexpoAnalytics {
  constructor() {
    super('auth');
  }

  async trackLoginAttempt(userId, provider, success, errorCode = null) {
    await this.trackEvent(userId, 'auth_login_attempt', {
      provider,
      success,
      error_code: errorCode,
      login_method: provider === 'password' ? 'password' : 'social'
    });
  }

  async trackSocialLogin(userId, provider, success, newUser = false) {
    const isEnabled = await this.isFeatureEnabled('social-login-analytics', userId);
    
    if (isEnabled) {
      await this.trackEvent(userId, 'auth_social_login', {
        provider,
        success,
        new_user: newUser
      });
    }
  }

  async trackSecurityEvent(userId, eventType, severity, actionTaken) {
    const isEnabled = await this.isFeatureEnabled('security-monitoring', userId);
    
    if (isEnabled) {
      await this.trackEvent(userId, 'auth_security_event', {
        event_type: eventType,
        severity,
        action_taken: actionTaken
      });
    }
  }
}

module.exports = AuthAnalytics;
```

## Step 8: Configure Webhooks

### 8.1 Create Webhook Endpoints

Create webhook handlers for each service:

```javascript
// src/webhooks/posthog.js
const express = require('express');
const crypto = require('crypto');
const router = express.Router();

function verifyWebhookSignature(payload, signature, secret) {
  const expectedSignature = crypto
    .createHmac('sha256', secret)
    .update(payload)
    .digest('hex');
  
  return signature === `sha256=${expectedSignature}`;
}

// Auth service webhook
router.post('/auth', (req, res) => {
  const signature = req.headers['x-posthog-signature'];
  const payload = JSON.stringify(req.body);
  
  if (!verifyWebhookSignature(payload, signature, process.env.AUTH_WEBHOOK_SECRET)) {
    return res.status(401).send('Unauthorized');
  }
  
  // Process auth-related PostHog events
  const event = req.body;
  console.log('Auth PostHog webhook:', event);
  
  res.status(200).send('OK');
});

// User service webhook
router.post('/user', (req, res) => {
  const signature = req.headers['x-posthog-signature'];
  const payload = JSON.stringify(req.body);
  
  if (!verifyWebhookSignature(payload, signature, process.env.USER_WEBHOOK_SECRET)) {
    return res.status(401).send('Unauthorized');
  }
  
  // Process user-related PostHog events
  const event = req.body;
  console.log('User PostHog webhook:', event);
  
  res.status(200).send('OK');
});

// Payments service webhook
router.post('/payments', (req, res) => {
  const signature = req.headers['x-posthog-signature'];
  const payload = JSON.stringify(req.body);
  
  if (!verifyWebhookSignature(payload, signature, process.env.PAYMENTS_WEBHOOK_SECRET)) {
    return res.status(401).send('Unauthorized');
  }
  
  // Process payments-related PostHog events
  const event = req.body;
  console.log('Payments PostHog webhook:', event);
  
  res.status(200).send('OK');
});

module.exports = router;
```

## Step 9: Monitoring and Validation

### 9.1 Create Monitoring Dashboard

Create `scripts/validate-deployment.sh`:

```bash
#!/bin/bash

echo "=== PostHog Deployment Validation ==="

# Check Terraform state
echo "1. Checking Terraform deployment..."
terraform state list | grep posthog

# Check PostHog projects
echo "2. Validating PostHog projects..."
curl -H "Authorization: Bearer $POSTHOG_API_KEY" \
     "https://app.posthog.com/api/projects/" | jq '.results[] | {id, name}'

# Check AWS SSM parameters
echo "3. Validating AWS SSM parameters..."
aws ssm get-parameters-by-path --path "/nexpo/posthog" --recursive

# Check Kubernetes secrets
echo "4. Validating Kubernetes secrets..."
kubectl get secrets | grep posthog

# Test API endpoints
echo "5. Testing PostHog integration..."
curl -X POST "https://app.posthog.com/capture/" \
     -H "Content-Type: application/json" \
     -d '{
       "api_key": "'$API_GATEWAY_POSTHOG_API_KEY'",
       "event": "deployment_test",
       "properties": {"test": true},
       "distinct_id": "deployment-validation"
     }'

echo "=== Validation Complete ==="
```

### 9.2 Set Up Alerts

Create monitoring alerts for:

```yaml
# PostHog monitoring alerts
alerts:
  - name: "PostHog API Errors"
    condition: "error_rate > 5%"
    notification: "slack://alerts-channel"
  
  - name: "High Event Volume"
    condition: "events_per_minute > 1000"
    notification: "email://platform-team"
  
  - name: "Feature Flag Failures"
    condition: "feature_flag_error_rate > 1%"
    notification: "pagerduty://on-call"
```

## Step 10: Production Checklist

### 10.1 Pre-Production Validation

- [ ] All Terraform modules deployed successfully
- [ ] PostHog projects created and accessible
- [ ] Feature flags configured and tested
- [ ] Webhooks configured and responding
- [ ] Environment variables deployed to all services
- [ ] Analytics events flowing to PostHog
- [ ] Dashboards accessible and populated
- [ ] GDPR compliance features tested
- [ ] PCI compliance validated (for payments)
- [ ] Performance monitoring active

### 10.2 Post-Deployment Tasks

```bash
# 1. Update documentation
echo "Update team documentation with PostHog URLs and access"

# 2. Train team members
echo "Conduct PostHog training for development and product teams"

# 3. Set up regular reviews
echo "Schedule weekly analytics reviews and dashboard monitoring"

# 4. Monitor costs
echo "Set up PostHog usage alerts and cost monitoring"

# 5. Plan feature rollouts
echo "Create feature flag rollout schedule and testing procedures"
```

## Troubleshooting

### Common Issues

1. **Terraform Provider Authentication**
   ```bash
   # Verify PostHog API key
   curl -H "Authorization: Bearer $POSTHOG_API_KEY" \
        "https://app.posthog.com/api/organizations/"
   ```

2. **Missing Environment Variables**
   ```bash
   # Check all required variables are set
   terraform validate
   terraform plan -detailed-exitcode
   ```

3. **PostHog Events Not Appearing**
   ```bash
   # Test event capture manually
   curl -X POST "https://app.posthog.com/capture/" \
        -H "Content-Type: application/json" \
        -d '{"api_key": "your-key", "event": "test", "distinct_id": "test"}'
   ```

4. **Feature Flags Not Working**
   ```bash
   # Test feature flag API
   curl -X POST "https://app.posthog.com/decide/" \
        -H "Content-Type: application/json" \
        -d '{"api_key": "your-key", "distinct_id": "test"}'
   ```

### Support and Resources

- **PostHog Documentation**: https://posthog.com/docs
- **Terraform PostHog Provider**: https://registry.terraform.io/providers/PostHog/posthog
- **Nexpo Platform Team**: platform-team@nexpo.com
- **PostHog Community**: https://posthog.com/slack

## Next Steps

After successful deployment:

1. **Implement Application Instrumentation**: Add PostHog tracking to all microservices
2. **Create Custom Dashboards**: Build service-specific analytics dashboards
3. **Set Up Feature Flag Workflows**: Establish feature rollout procedures
4. **Configure Advanced Analytics**: Implement cohort analysis and funnel tracking
5. **Optimize Performance**: Monitor and tune analytics performance
6. **Expand Coverage**: Add PostHog to additional services and applications

This deployment guide provides a comprehensive approach to implementing PostHog analytics across the Nexpo microservices architecture. Follow each step carefully and validate the deployment at each stage to ensure a successful implementation.
