# Cloudflare DNS Provider for Multi-Cloud Infrastructure

This module provides comprehensive Cloudflare DNS, WAF, Page Rules, and Workers management for your multi-cloud infrastructure. It enables seamless integration of Cloudflare's edge network capabilities with your existing AWS, GCP, Azure, OCI, IBM, Digital Ocean deployments.

## Features

- **DNS Management**: Zones, records, and DNSSEC configuration
- **Web Application Firewall (WAF)**: Custom firewall rules, rate limiting, and security policies
- **Page Rules**: URL redirects, caching policies, and performance settings
- **Cloudflare Workers**: Serverless edge computing for dynamic content and custom logic
- **SSL/TLS Management**: Certificate and security settings for all domains

## Prerequisites

- Cloudflare account with registered domains
- Cloudflare API token with sufficient permissions
- Terraform 1.0.0+

## Quick Start

1. Copy `terraform.tfvars.example` to `terraform.tfvars` and add your Cloudflare credentials:

```bash
cp terraform.tfvars.example terraform.tfvars
```

2. Update the `terraform.tfvars` file with your Cloudflare API token, email, and account ID.

3. Configure your DNS zones and records as needed in the `terraform.tfvars` file.

4. Initialize Terraform:

```bash
terraform init
```

5. Plan and apply your changes:

```bash
terraform plan
terraform apply
```

## Module Structure

```
cloudflare/
├── modules/
│   ├── dns/             # DNS zone and record management
│   ├── waf/             # Web Application Firewall configuration
│   ├── page_rules/      # URL redirects and caching rules
│   └── workers/         # Serverless edge computing
├── main.tf              # Main provider configuration
├── variables.tf         # Input variables definition
├── outputs.tf           # Output values
└── terraform.tfvars.example  # Example variable values
```

## DNS Management

The DNS module allows you to manage zones and records for all your domains:

```hcl
module "dns" {
  source = "./modules/dns"
  
  cloudflare_account_id = var.cloudflare_account_id
  zones = {
    "example.com" = {
      plan = "free"
      enable_dnssec = true
    }
  }
  records = [
    {
      zone_name = "example.com"
      name      = "www"
      value     = "203.0.113.10"
      type      = "A"
      proxied   = true
    }
  ]
}
```

## WAF Configuration

The WAF module provides security features to protect your web applications:

```hcl
module "waf" {
  source = "./modules/waf"
  
  filters = [
    {
      zone_id    = "your-zone-id"
      description = "Block SQL Injection"
      expression  = "http.request.uri.query contains \"SELECT\""
    }
  ]
  
  firewall_rules = [
    {
      zone_id    = "your-zone-id"
      filter_idx = 0
      action     = "block"
    }
  ]
}
```

## Page Rules

Create custom behavior for specific URL patterns:

```hcl
module "page_rules" {
  source = "./modules/page_rules"
  
  page_rules = [
    {
      zone_id = "your-zone-id"
      target  = "example.com/api/*"
      actions = {
        cache_level = "cache_everything"
        edge_cache_ttl = 3600
      }
    }
  ]
}
```

## Workers

Deploy serverless JavaScript at the edge:

```hcl
module "workers" {
  source = "./modules/workers"
  
  cloudflare_account_id = var.cloudflare_account_id
  scripts = [
    {
      name    = "api-gateway"
      content = file("scripts/api-gateway.js")
    }
  ]
  
  worker_routes = [
    {
      zone_id     = "your-zone-id"
      pattern     = "api.example.com/*"
      script_name = "api-gateway"
    }
  ]
}
```

## Integration with Multi-Cloud Setup

This Cloudflare provider is designed to work seamlessly with your existing multi-cloud infrastructure. To enable it in your main Terraform configuration:

1. Add the Cloudflare provider in the main `terraform/main.tf` file:

```hcl
module "cloudflare" {
  source = "./providers/cloudflare"
  count  = var.enable_cloudflare ? 1 : 0

  cloudflare_api_token = var.cloudflare_api_token
  cloudflare_email     = var.cloudflare_email
  cloudflare_account_id = var.cloudflare_account_id
  
  # Pass other variables as needed
}
```

2. Update `terraform/variables.tf` to include Cloudflare variables:

```hcl
variable "enable_cloudflare" {
  description = "Enable Cloudflare provider"
  type        = bool
  default     = false
}

variable "cloudflare_api_token" {
  description = "Cloudflare API token"
  type        = string
  default     = ""
  sensitive   = true
}

variable "cloudflare_email" {
  description = "Cloudflare account email"
  type        = string
  default     = ""
}

variable "cloudflare_account_id" {
  description = "Cloudflare account ID"
  type        = string
  default     = ""
}
```

3. Update `terraform/terraform.tfvars.example` to include Cloudflare options:

```hcl
# Cloudflare Configuration
enable_cloudflare      = false
cloudflare_api_token   = ""
cloudflare_email       = ""
cloudflare_account_id  = ""
```

## Security Best Practices

- Use API tokens with limited scope instead of Global API keys
- Implement DNSSEC for all production domains
- Enable "Full (strict)" SSL mode for enhanced security
- Apply rate limiting to sensitive endpoints
- Regularly review WAF rules and logs

## Troubleshooting

- **API Rate Limiting**: Cloudflare limits API requests to 1,200 per 5 minutes. Use `terraform apply -parallelism=2` to reduce concurrent API calls.
- **DNS Propagation**: DNS changes may take up to 24-48 hours to propagate globally.
- **Workers Limits**: Free accounts have limits on Workers usage. Check Cloudflare's documentation for current limits.

## Resources

- [Cloudflare Terraform Provider Documentation](https://registry.terraform.io/providers/cloudflare/cloudflare/latest/docs)
- [Cloudflare API Documentation](https://developers.cloudflare.com/api/)
- [Cloudflare Workers Documentation](https://developers.cloudflare.com/workers/)
