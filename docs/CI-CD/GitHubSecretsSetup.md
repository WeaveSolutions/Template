# GitHub Repository Secrets Setup Guide

This guide covers setting up GitHub repository secrets for all supported cloud providers and services in the Next-Solito-Expo Terraform infrastructure.

## 🔐 How to Add Secrets

1. Navigate to your GitHub repository
2. Go to **Settings** → **Secrets and variables** → **Actions**
3. Click **New repository secret**
4. Add the secret name and value
5. Click **Add secret**

---

## ☁️ Cloud Provider Secrets

### AWS (Amazon Web Services)
```
AWS_ACCESS_KEY_ID=your-aws-access-key
AWS_SECRET_ACCESS_KEY=your-aws-secret-key
AWS_REGION=us-east-1
```

**How to obtain:**
1. Go to AWS IAM Console
2. Create a new user with programmatic access
3. Attach policies: `AdministratorAccess` (or custom minimal permissions)
4. Save the Access Key ID and Secret Access Key

### GCP (Google Cloud Platform)
```
GCP_PROJECT_ID=your-project-id
GCP_CREDENTIALS={"type":"service_account","project_id":"..."}
GCP_REGION=us-central1
```

**How to obtain:**
1. Go to Google Cloud Console → IAM & Admin → Service Accounts
2. Create a new service account
3. Grant roles: `Editor` or `Owner`
4. Create and download JSON key
5. Use the entire JSON content as `GCP_CREDENTIALS`

### Azure (Microsoft Azure)
```
AZURE_CLIENT_ID=your-app-id
AZURE_CLIENT_SECRET=your-app-secret
AZURE_SUBSCRIPTION_ID=your-subscription-id
AZURE_TENANT_ID=your-tenant-id
```

**How to obtain:**
1. Go to Azure Portal → Azure Active Directory → App registrations
2. Create a new app registration
3. Note the Application (client) ID and Directory (tenant) ID
4. Go to Certificates & secrets → Create new client secret
5. Assign Contributor role to the app in your subscription

### OCI (Oracle Cloud Infrastructure)
```
OCI_TENANCY_OCID=ocid1.tenancy.oc1..
OCI_USER_OCID=ocid1.user.oc1..
OCI_FINGERPRINT=your-fingerprint
OCI_PRIVATE_KEY=-----BEGIN PRIVATE KEY-----...
OCI_REGION=us-ashburn-1
```

**How to obtain:**
1. Generate API key pair in OCI Console → User Settings → API Keys
2. Note the OCID values from your user profile
3. Use the private key content for `OCI_PRIVATE_KEY`

### IBM Cloud
```
IBM_CLOUD_API_KEY=your-api-key
IBM_REGION=us-south
IBM_RESOURCE_GROUP=default
```

**How to obtain:**
1. Go to IBM Cloud Console → Manage → Access (IAM)
2. Go to API keys → Create
3. Select your user account and create the key

### DigitalOcean
```
DIGITALOCEAN_TOKEN=your-personal-access-token
DIGITALOCEAN_REGION=nyc1
```

**How to obtain:**
1. Go to DigitalOcean Control Panel → API
2. Generate New Token with read/write scope
3. Save the token value

### Cloudflare
```
CLOUDFLARE_API_TOKEN=your-api-token
CLOUDFLARE_ZONE_ID=your-zone-id
CLOUDFLARE_ACCOUNT_ID=your-account-id
```

**How to obtain:**
1. Go to Cloudflare Dashboard → My Profile → API Tokens
2. Create Custom token with Zone:Edit permissions
3. Get Zone ID from your domain's Overview page

---

## 🏗️ HashiCorp Stack Secrets

### HashiCorp Nomad
```
NOMAD_ADDR=https://nomad.example.com:4646
NOMAD_TOKEN=your-nomad-token
NOMAD_REGION=global
```

**How to obtain:**
1. Set up Nomad cluster
2. Generate ACL token: `nomad acl token create -policy=admin`
3. Use cluster URL and token

### HashiCorp Vault
```
VAULT_ADDR=https://vault.example.com:8200
VAULT_TOKEN=your-vault-token
VAULT_NAMESPACE=admin
```

**How to obtain:**
1. Set up Vault cluster
2. Use root token or create service token
3. Use cluster URL and token

### HashiCorp Consul
```
CONSUL_HTTP_ADDR=https://consul.example.com:8500
CONSUL_HTTP_TOKEN=your-consul-token
CONSUL_DATACENTER=dc1
```

**How to obtain:**
1. Set up Consul cluster
2. Generate ACL token with appropriate policies
3. Use cluster URL and token

---

## 🚀 Platform as a Service Secrets

### Heroku
```
HEROKU_API_KEY=your-heroku-api-key
HEROKU_EMAIL=your-heroku-email
```

**How to obtain:**
1. Go to Heroku Dashboard → Account Settings
2. Reveal API Key in the API Key section
3. Use your Heroku account email

---

## 🔐 Identity & Authentication Secrets

### Auth0
```
AUTH0_DOMAIN=your-tenant.auth0.com
AUTH0_CLIENT_ID=your-management-api-client-id
AUTH0_CLIENT_SECRET=your-management-api-client-secret
```

**How to obtain:**
1. Go to Auth0 Dashboard → Applications → APIs
2. Authorize Machine to Machine application for Management API
3. Use the authorized application's credentials

---

## 📋 Infrastructure Secrets

### Shared Infrastructure
```
TF_VAR_environment=development
TF_VAR_project_name=nexpo
TF_VAR_domain_name=your-domain.com
```

### Database Secrets
```
DB_PASSWORD=your-secure-database-password
REDIS_PASSWORD=your-secure-redis-password
```

### Monitoring & Observability
```
DATADOG_API_KEY=your-datadog-api-key
SENTRY_DSN=your-sentry-dsn
```

---

## ✅ Required Secrets Checklist

### Core Infrastructure (Always Required)
- [ ] `TF_VAR_environment`
- [ ] `TF_VAR_project_name`
- [ ] `TF_VAR_domain_name`

### Cloud Providers (Based on Enabled Features)
- [ ] AWS secrets (if `ENABLE_AWS=true`)
- [ ] GCP secrets (if `ENABLE_GCP=true`)
- [ ] Azure secrets (if `ENABLE_AZURE=true`)
- [ ] OCI secrets (if `ENABLE_OCI=true`)
- [ ] IBM secrets (if `ENABLE_IBM=true`)
- [ ] DigitalOcean secrets (if `ENABLE_DIGITALOCEAN=true`)
- [ ] Cloudflare secrets (if `ENABLE_CLOUDFLARE=true`)

### HashiCorp Stack (Based on Enabled Features)
- [ ] Nomad secrets (if `ENABLE_HASHICORP_NOMAD=true`)
- [ ] Vault secrets (if `ENABLE_HASHICORP_VAULT=true`)
- [ ] Consul secrets (if `ENABLE_HASHICORP_CONSUL=true`)

### Platform Services (Based on Enabled Features)
- [ ] Heroku secrets (if `ENABLE_HEROKU=true`)
- [ ] Auth0 secrets (if `ENABLE_AUTH0=true`)

---

## 🔒 Security Best Practices

1. **Principle of Least Privilege**: Only grant necessary permissions
2. **Regular Rotation**: Rotate secrets regularly (every 90 days)
3. **Environment Separation**: Use different credentials for dev/staging/prod
4. **Monitoring**: Enable audit logging for all cloud accounts
5. **Backup Access**: Maintain emergency access methods

## 📖 Related Documentation

- [Terraform Workflow Documentation](../.github/workflows/terraform.yml)
- [Environment Variables Configuration](../.env.example)
- [Provider-Specific Documentation](../terraform/README.md)
- [Security Best Practices](../docs/Security-Best-Practices.md)

## 🆘 Troubleshooting

### Common Issues
1. **Invalid credentials**: Double-check secret values and permissions
2. **Missing secrets**: Ensure all required secrets for enabled providers are set
3. **Wrong format**: Verify JSON secrets are properly formatted
4. **Expired tokens**: Check if tokens need renewal

### Getting Help
- Check GitHub Actions logs for specific error messages
- Validate credentials using provider CLI tools
- Review provider documentation for authentication requirements
