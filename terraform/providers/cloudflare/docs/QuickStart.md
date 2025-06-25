# Cloudflare Quick Start Guide

This guide provides a fast path to deploying your Cloudflare resources using our Terraform modules. Follow these steps to quickly get your environment up and running.

## Prerequisites

Before you begin, ensure you have:

1. **Cloudflare Account**: Sign up at [cloudflare.com](https://dash.cloudflare.com/sign-up)
2. **Domain**: At least one domain added to your Cloudflare account
3. **Terraform**: Install version 1.0.0+ from [terraform.io](https://www.terraform.io/downloads.html)
4. **Git**: For cloning the repository

## Step 1: Set Up Your Environment

### Clone the Repository

```bash
git clone https://github.com/your-organization/Nexpo.git
cd Nexpo
```

### Create Cloudflare API Token

1. Log in to Cloudflare Dashboard
2. Navigate to My Profile → API Tokens
3. Click "Create Token"
4. Select "Create Custom Token"
5. Name your token (e.g., "terraform-deployment")
6. Grant the following permissions:
   - Zone → DNS → Edit
   - Zone → WAF → Edit
   - Zone → Page Rules → Edit
   - Zone → Workers Routes → Edit
   - Account → Workers KV Storage → Edit
   - Account → Workers Scripts → Edit
7. Set appropriate zone resources
8. Create the token and copy it (you won't be able to see it again)

### Configure Environment Variables

Create a `.env` file in the root directory based on the provided `.env.example`:

```bash
# Copy the example file
cp .env.example .env

# Edit the file with your Cloudflare credentials
# Set ENABLE_CLOUDFLARE=true
# Add your Cloudflare API token, email, account ID, and zone ID
```

## Step 2: Configure Your Infrastructure

### Update terraform.tfvars

Create a file named `cloudflare.tfvars` in the `terraform` directory:

```hcl
# Cloudflare Configuration
enable_cloudflare = true
cloudflare_api_token = "your-cloudflare-api-token"
cloudflare_email = "your-email@example.com"
cloudflare_account_id = "your-cloudflare-account-id"
cloudflare_zone_id = "your-cloudflare-zone-id"

# DNS Configuration
dns_records = [
  {
    name  = "www"
    value = "203.0.113.10"
    type  = "A"
    ttl   = 3600
    proxied = true
  },
  {
    name  = "api"
    value = "api.example.com"
    type  = "CNAME"
    ttl   = 3600
    proxied = true
  }
]

# Page Rules Configuration
page_rules = [
  {
    target = "example.com/api/*"
    actions = {
      cache_level = "bypass"
      disable_security = false
      browser_cache_ttl = 0
    }
  }
]

# Optional configurations based on your needs:
# Uncomment and modify as required

# WAF configuration
# enable_waf = true
# waf_rules = [
#   {
#     name = "Block SQL Injection"
#     description = "Block SQL injection attempts"
#     expression = "(http.request.uri.path contains \"sql\")"
#     action = "block"
#   }
# ]

# Workers configuration
# enable_workers = true
# workers = [
#   {
#     name = "redirect-worker"
#     content = "addEventListener('fetch', event => { event.respondWith(handleRequest(event.request)) }); async function handleRequest(request) { return Response.redirect('https://example.com/new-page', 301); }"
#     routes = ["example.com/old-page*"]
#   }
# ]
```

## Step 3: Initialize and Deploy

### Initialize Terraform

```bash
cd terraform
terraform init
```

### Plan Your Deployment

```bash
terraform plan -var-file=cloudflare.tfvars
```

Review the plan to ensure it matches your expectations.

### Deploy Your Infrastructure

```bash
terraform apply -var-file=cloudflare.tfvars
```

Type `yes` when prompted to confirm the deployment.

## Step 4: Access Your Resources

### DNS Records

Verify your DNS records are properly configured:

```bash
# Check DNS propagation
dig @1.1.1.1 www.example.com
dig @1.1.1.1 api.example.com
```

### Page Rules

Check your Page Rules in the Cloudflare Dashboard:
1. Log in to Cloudflare Dashboard
2. Select your domain
3. Go to Rules → Page Rules
4. Verify the rules are configured correctly

### Workers

If you deployed Workers, verify they're working:

```bash
# Test a Worker route
curl -v https://example.com/path-to-worker
```

## Step 5: Multi-Cloud Integration

If you're deploying across multiple cloud providers:

1. Enable the desired providers in your `.env` file:
   ```
   ENABLE_AWS=true
   ENABLE_GCP=false
   ENABLE_AZURE=false
   ENABLE_OCI=false
   ENABLE_IBM=false
   ENABLE_CLOUDFLARE=true
   ```

2. Create provider-specific `.tfvars` files:
   - `aws.tfvars`
   - `cloudflare.tfvars`
   - etc.

3. Deploy using all enabled providers:
   ```bash
   terraform apply \
     -var-file=aws.tfvars \
     -var-file=cloudflare.tfvars
   ```

4. Configure Cloudflare to front your cloud infrastructure:
   - Point DNS records to cloud load balancers or instances
   - Set up proper SSL/TLS mode
   - Configure appropriate Page Rules

## Common Configurations

### DNS Management

```hcl
# Complete DNS zone management
dns_records = [
  # A Records
  {
    name    = "www"
    value   = "203.0.113.10"
    type    = "A"
    ttl     = 1
    proxied = true
  },
  # CNAME Records
  {
    name    = "blog"
    value   = "example.com"
    type    = "CNAME"
    ttl     = 1
    proxied = true
  },
  # MX Records
  {
    name  = "@"
    value = "mx1.example.com"
    type  = "MX"
    ttl   = 3600
    priority = 10
    proxied = false
  },
  # TXT Records
  {
    name  = "@"
    value = "v=spf1 include:_spf.example.com ~all"
    type  = "TXT"
    ttl   = 3600
    proxied = false
  }
]
```

### Zone Settings

```hcl
# Common zone settings
zone_settings = {
  ssl = "strict"
  always_use_https = true
  min_tls_version = "1.2"
  brotli = true
  automatic_https_rewrites = true
  opportunistic_encryption = true
  security_level = "medium"
  challenge_ttl = 2700
}
```

### Page Rules

```hcl
# Common page rules
page_rules = [
  # Cache bypass for API
  {
    target = "example.com/api/*"
    actions = {
      cache_level = "bypass"
      disable_security = false
    }
  },
  # Redirect rule
  {
    target = "example.com/old-blog/*"
    actions = {
      forwarding_url = {
        url = "https://blog.example.com/$1"
        status_code = 301
      }
    }
  },
  # Cache everything rule
  {
    target = "example.com/static/*"
    actions = {
      cache_level = "cache_everything"
      edge_cache_ttl = 86400
    }
  }
]
```

### WAF (Web Application Firewall)

```hcl
# WAF configuration
waf_rules = [
  {
    name = "Block SQL Injection"
    description = "Block SQL injection attempts"
    expression = "(http.request.uri.path contains \"sql\")"
    action = "block"
  },
  {
    name = "Rate Limit Login Page"
    description = "Rate limit login attempts"
    expression = "(http.request.uri.path eq \"/login\")"
    action = "throttle"
    rate_limit = {
      period = 60
      requests_per_period = 10
    }
  }
]
```

### Workers

```hcl
# Workers configuration
workers = [
  {
    name = "redirect-worker"
    content = "addEventListener('fetch', event => { event.respondWith(handleRequest(event.request)) }); async function handleRequest(request) { return Response.redirect('https://example.com/new-page', 301); }"
    routes = ["example.com/old-page*"]
  },
  {
    name = "api-cache"
    content_file = "workers/api-cache.js"
    routes = ["api.example.com/*"]
    bindings = [
      {
        type = "kv_namespace"
        name = "API_CACHE"
        namespace_id = "your-kv-namespace-id"
      }
    ]
  }
]
```

## Troubleshooting

### Common Issues

1. **API Token Permissions**
   - Ensure your API token has the correct permissions
   - Create a new token with all required permissions

2. **DNS Propagation**
   - DNS changes can take time to propagate (up to 24-48 hours)
   - Verify changes are reflected in the Cloudflare dashboard

3. **SSL/TLS Issues**
   - Check the SSL/TLS encryption mode in your zone settings
   - Verify origin certificates if using Full (Strict) mode

4. **Page Rule Conflicts**
   - Page Rules are processed in order; check for conflicts
   - Ensure the most specific rules appear first

### Getting Help

- Cloudflare Support: https://support.cloudflare.com/
- Cloudflare Community: https://community.cloudflare.com/
- Cloudflare Documentation: https://developers.cloudflare.com/
- Cloudflare Status: https://www.cloudflarestatus.com/

## Next Steps

After your initial deployment:

1. **Set Up Workers**: Deploy serverless functions to enhance your site
2. **Configure Argo Tunnel**: Secure connection between Cloudflare and your origin
3. **Implement Access**: Add Zero Trust security with Cloudflare Access
4. **Enable Analytics**: Set up Cloudflare Analytics to monitor traffic
5. **Configure Cache Rules**: Optimize caching for better performance
6. **Set Up Alerts**: Monitor for security threats and downtime

Refer to our [DEPLOYMENT_CHECKLIST.md](./DEPLOYMENT_CHECKLIST.md) for a comprehensive guide to production-ready deployments.
