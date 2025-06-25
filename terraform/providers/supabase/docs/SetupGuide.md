# Supabase Terraform Setup Guide

This guide will walk you through setting up and deploying your Supabase infrastructure using Terraform.

## 🚀 Quick Setup

### Step 1: Get Supabase Access Token

1. Go to [Supabase Dashboard](https://app.supabase.com/account/tokens)
2. Generate a new access token
3. Copy the token (starts with `sbp_`)

### Step 2: Configure Environment

```bash
# Set environment variable (recommended)
export SUPABASE_ACCESS_TOKEN="sbp_your_token_here"

# Or create terraform.tfvars file
cp terraform.tfvars.example terraform.tfvars
```

### Step 3: Update Configuration

Edit `terraform.tfvars`:

```hcl
# Required: Your Supabase access token
supabase_access_token = "sbp_your_token_here"

# Required: Organization (get from Supabase dashboard)
existing_organization_id = "your-org-id-here"

# Required: Database passwords
dev_database_password     = "dev_secure_password_123"
staging_database_password = "staging_secure_password_123"
prod_database_password    = "prod_secure_password_123"
```

### Step 4: Deploy

```bash
# Initialize Terraform
terraform init

# Review what will be created
terraform plan

# Deploy your infrastructure
terraform apply
```

## 🔧 Detailed Configuration

### Authentication Providers

To enable OAuth providers, you need to set up applications with each provider:

#### Google OAuth
1. Go to [Google Cloud Console](https://console.cloud.google.com/)
2. Create OAuth 2.0 credentials
3. Add your redirect URLs
4. Copy Client ID and Secret

```hcl
auth_providers = {
  google_enabled       = true
  google_client_id     = "your-google-client-id"
  google_client_secret = "your-google-client-secret"
}
```

#### GitHub OAuth
1. Go to GitHub Settings > Developer settings > OAuth Apps
2. Create a new OAuth App
3. Set Authorization callback URL
4. Copy Client ID and Secret

```hcl
auth_providers = {
  github_enabled       = true
  github_client_id     = "your-github-client-id"
  github_client_secret = "your-github-client-secret"
}
```

### Storage Buckets

Configure buckets for different file types:

```hcl
storage_buckets = {
  # Public bucket for avatars
  avatars = {
    name         = "avatars"
    public       = true
    file_size_limit = 5242880  # 5MB
    allowed_mime_types = ["image/jpeg", "image/png", "image/webp"]
  }
  
  # Private bucket for documents
  documents = {
    name         = "documents"
    public       = false
    file_size_limit = 52428800  # 50MB
    allowed_mime_types = ["application/pdf", "text/plain"]
  }
}
```

### Edge Functions

Deploy serverless functions:

```hcl
edge_functions = {
  hello_world = {
    name        = "hello-world"
    source_code = file("${path.module}/functions/hello-world/index.ts")
    verify_jwt  = false  # Public function
  }
  
  send_email = {
    name        = "send-email"
    source_code = file("${path.module}/functions/send-email/index.ts")
    verify_jwt  = true   # Requires authentication
  }
}
```

### Project Secrets

Manage sensitive environment variables:

```hcl
project_secrets = {
  openai_api_key = {
    name  = "OPENAI_API_KEY"
    value = "sk-your-openai-key"
  }
  stripe_secret = {
    name  = "STRIPE_SECRET_KEY"
    value = "sk_test_your-stripe-key"
  }
}
```

## 🌍 Environment-Specific Configuration

### Development Environment
- Local development URLs
- Relaxed security settings
- Debug-friendly configuration

```hcl
auth_config = {
  site_url_dev = "http://localhost:3000"
  redirect_urls_dev = [
    "http://localhost:3000/auth/callback",
    "http://localhost:19006/auth/callback"  # React Native/Expo
  ]
}
```

### Staging Environment
- Staging domain URLs
- Production-like settings
- Testing configurations

```hcl
auth_config = {
  site_url_staging = "https://staging.yourapp.com"
  redirect_urls_staging = [
    "https://staging.yourapp.com/auth/callback"
  ]
}
```

### Production Environment
- Production domain URLs
- Maximum security settings
- Performance optimizations

```hcl
auth_config = {
  site_url_prod = "https://yourapp.com"
  redirect_urls_prod = [
    "https://yourapp.com/auth/callback"
  ]
}
```

## 🔐 Security Recommendations

### Access Tokens
- Store in environment variables, not in code
- Use separate tokens for different environments
- Rotate tokens regularly

### Database Passwords
- Use strong, unique passwords for each environment
- Store in secure vaults (AWS Secrets Manager, etc.)
- Never commit to version control

### OAuth Secrets
- Keep client secrets secure
- Use different OAuth apps for different environments
- Regularly rotate credentials

## 📊 Monitoring & Outputs

After deployment, Terraform provides useful outputs:

```bash
# View all outputs
terraform output

# Get specific project URL
terraform output project_urls

# Get API keys
terraform output anon_keys
```

Use these outputs in your application:

```typescript
// React/Next.js example
const supabaseUrl = 'https://xxx.supabase.co'
const supabaseKey = 'eyJ...'

export const supabase = createClient(supabaseUrl, supabaseKey)
```

## 🔄 CI/CD Integration

### GitHub Actions Example

```yaml
name: Deploy Supabase Infrastructure

on:
  push:
    branches: [main]
    paths: ['terraform/providers/supabase/**']

jobs:
  terraform:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v3
        
      - name: Terraform Init
        run: terraform init
        working-directory: ./terraform/providers/supabase
        
      - name: Terraform Plan
        run: terraform plan
        working-directory: ./terraform/providers/supabase
        env:
          SUPABASE_ACCESS_TOKEN: ${{ secrets.SUPABASE_ACCESS_TOKEN }}
          
      - name: Terraform Apply
        if: github.ref == 'refs/heads/main'
        run: terraform apply -auto-approve
        working-directory: ./terraform/providers/supabase
        env:
          SUPABASE_ACCESS_TOKEN: ${{ secrets.SUPABASE_ACCESS_TOKEN }}
```

## 🛠️ Troubleshooting

### Common Issues

1. **"Invalid access token"**
   ```bash
   # Check token is set correctly
   echo $SUPABASE_ACCESS_TOKEN
   
   # Or check terraform.tfvars
   grep supabase_access_token terraform.tfvars
   ```

2. **"Organization not found"**
   ```bash
   # List organizations
   curl -H "Authorization: Bearer $SUPABASE_ACCESS_TOKEN" \
        https://api.supabase.com/v1/organizations
   ```

3. **"Project creation failed"**
   - Check region availability
   - Verify organization permissions
   - Try different project name

4. **"Module not found"**
   ```bash
   # Re-initialize Terraform
   terraform init -upgrade
   ```

### Debug Mode

Enable debug output:

```bash
export TF_LOG=DEBUG
terraform apply
```

## 📚 Next Steps

1. **Set up database migrations** using Supabase CLI
2. **Configure Row Level Security** policies
3. **Set up monitoring** and alerting
4. **Implement backup strategies**
5. **Configure custom domains**

## 🤝 Integration Examples

### With Next.js

```typescript
// lib/supabase.ts
import { createClient } from '@supabase/supabase-js'

const supabaseUrl = process.env.NEXT_PUBLIC_SUPABASE_URL!
const supabaseKey = process.env.NEXT_PUBLIC_SUPABASE_ANON_KEY!

export const supabase = createClient(supabaseUrl, supabaseKey)
```

### With React Native

```typescript
// lib/supabase.ts
import { createClient } from '@supabase/supabase-js'
import AsyncStorage from '@react-native-async-storage/async-storage'

const supabaseUrl = 'your-supabase-url'
const supabaseKey = 'your-supabase-anon-key'

export const supabase = createClient(supabaseUrl, supabaseKey, {
  auth: {
    storage: AsyncStorage,
    autoRefreshToken: true,
    persistSession: true,
    detectSessionInUrl: false,
  },
})
```

### With Node.js/Express

```typescript
// server.ts
import { createClient } from '@supabase/supabase-js'

const supabase = createClient(
  process.env.SUPABASE_URL!,
  process.env.SUPABASE_SERVICE_ROLE_KEY! // Use service role for server-side
)
```

This setup provides a solid foundation for managing your Supabase infrastructure with Terraform! 🎉
