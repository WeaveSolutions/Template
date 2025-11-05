# Supabase Terraform Provider

This module manages Supabase infrastructure using Terraform, providing full Infrastructure as Code for your Supabase **data services**.

> **Authentication Note**: This module focuses on Supabase as a data backend only. Authentication is handled by **Auth0** as the primary identity provider. See [Auth0 Integration Guide](../../../docs/Authentication/Supabase-Auth0.md) for details.

## Features

### ✅ **Supported Resources**

- **Projects**: Create and manage Supabase projects across environments
- **Organizations**: Manage Supabase organizations
- **Database**: Configure PostgreSQL API settings and database schemas
- **Storage**: Manage storage buckets with custom policies
- **Edge Functions**: Deploy and manage serverless functions
- **Secrets**: Securely manage project secrets
- **Domains**: Configure custom domains (future enhancement)

### 🔐 **Auth0 Integration Architecture**

```
Client App → Auth0 → CRA Service → Supabase (Data Services)
```

- **Auth0**: Primary identity provider (OAuth flows, user management)
- **CRA Service**: Token exchange and user profile sync
- **Supabase**: PostgreSQL database, storage, and edge functions
- **RLS Policies**: Validate Auth0 JWT claims via API keys

### 🌍 **Multi-Environment Support**

- **Development**: Local development configuration
- **Staging**: Pre-production testing environment
- **Production**: Production-ready configuration

## Quick Start

### 1. Prerequisites

```bash
# Install Terraform
# Install Supabase CLI (for migrations)
pnpm install -g @supabase/cli

# Get Supabase Management API token
# Go to: https://app.supabase.com/account/tokens
```

### 2. Configuration

```bash
# Copy example configuration
cp terraform.tfvars.example terraform.tfvars

# Edit terraform.tfvars with your settings
nano terraform.tfvars
```

### 3. Deploy

```bash
# Initialize Terraform
terraform init

# Plan the deployment
terraform plan

# Apply the configuration
terraform apply
```

## Configuration Guide

### Required Variables

```hcl
# Supabase Management API Token
supabase_access_token = "sbp_your_token_here"

# Organization setup
existing_organization_id = "your-org-id"

# Database passwords
dev_database_password     = "secure_password"
staging_database_password = "secure_password"
prod_database_password    = "secure_password"
```

### Storage Configuration

```hcl
storage_buckets = {
  avatars = {
    name         = "avatars"
    public       = true
    file_size_limit = 5242880  # 5MB
    allowed_mime_types = ["image/jpeg", "image/png"]
  }
  documents = {
    name         = "documents"
    public       = false
    file_size_limit = 52428800  # 50MB
  }
}
```

### Edge Functions

```hcl
edge_functions = {
  hello_world = {
    name        = "hello-world"
    source_code = file("${path.module}/functions/hello-world/index.ts")
    verify_jwt  = false
  }
}
```

## Integration with Other Providers

### AWS Integration

```hcl
# In your AWS provider configuration
module "aws_app" {
  source = "../aws"
  
  # Use Supabase outputs
  supabase_url_dev     = module.supabase.project_urls.development
  supabase_anon_key_dev = module.supabase.anon_keys.development
  
  supabase_url_prod     = module.supabase.project_urls.production
  supabase_anon_key_prod = module.supabase.anon_keys.production
}
```

### GCP Integration

```hcl
# In your GCP provider configuration
module "gcp_app" {
  source = "../gcp"
  
  # Use Supabase outputs
  supabase_projects = module.supabase.project_urls
  supabase_keys     = module.supabase.anon_keys
}
```

## Outputs

The module provides these outputs for integration:

```hcl
# Project URLs
project_urls = {
  development = "https://xxx.supabase.co"
  staging     = "https://yyy.supabase.co"
  production  = "https://zzz.supabase.co"
}

# Anonymous keys (for client-side)
anon_keys = {
  development = "eyJ..."
  staging     = "eyJ..."
  production  = "eyJ..."
}

# Service role keys (for server-side)
service_role_keys = {
  development = "eyJ..."
  staging     = "eyJ..."
  production  = "eyJ..."
}
```

## Security Best Practices

### 🔐 **Secrets Management**

- Store `supabase_access_token` in environment variables or secret managers
- Use different database passwords for each environment
- Never commit sensitive values to version control

### 🛡️ **Access Control**

- Use service role keys only on server-side
- Configure Row Level Security (RLS) policies
- Limit OAuth redirect URLs to trusted domains

### 🔒 **Production Hardening**

- Enable email confirmation in production
- Use strong database passwords (16+ characters)
- Configure custom domains with SSL

## Troubleshooting

### Common Issues

1. **Authentication Failed**
   ```bash
   # Check your access token
   export SUPABASE_ACCESS_TOKEN="your_token"
   ```

2. **Organization Not Found**
   ```bash
   # List your organizations
   curl -H "Authorization: Bearer $SUPABASE_ACCESS_TOKEN" \
        https://api.supabase.com/v1/organizations
   ```

3. **Project Creation Failed**
   ```bash
   # Check region availability
   terraform plan -var="supabase_region=us-west-1"
   ```

## Advanced Usage

### Importing Existing Projects

```bash
# Import existing project
terraform import supabase_project.production your-project-ref

# Update configuration to match
terraform plan
```

### Custom Modules

Create custom modules for specific use cases:

```hcl
module "blog_supabase" {
  source = "./modules/blog"
  
  project_refs = module.supabase.project_urls
  auth_config  = var.blog_auth_config
}
```

## Migration Guide

If you're migrating from manual Supabase setup:

1. **Backup existing data**
2. **Import existing projects**: `terraform import`
3. **Configure variables** to match current setup
4. **Plan carefully**: `terraform plan`
5. **Apply incrementally**: Start with non-critical resources

## Support

- [Supabase Terraform Provider Docs](https://registry.terraform.io/providers/supabase/supabase/latest/docs)
- [Supabase Management API](https://supabase.com/docs/reference/api)
- [Terraform Documentation](https://www.terraform.io/docs)

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request
