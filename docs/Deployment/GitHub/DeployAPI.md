# Nexpo API Backends - Deployment Guide

This document provides an overview of the polyglot API backends and their deployment capabilities across different cloud platforms.

## 🚀 Getting Started

### Prerequisites

Before deploying the API backends, ensure you have:

1. **GitHub Actions Secrets**: Set up the following secrets in your GitHub repository:
   - `DOCKER_USERNAME` - Docker Hub username
   - `DOCKER_PASSWORD` - Docker Hub password/access token

2. **Environment Configuration**: 
   - Configure staging and production environments in GitHub
   - Set up environment-specific variables in GitHub Settings

3. **Review Generated Workflows**: 
   - Check the generated workflow files in `.github/workflows/api/`
   - Customize build/test commands as needed for each backend

### Infrastructure as Code

Terraform configurations are located in:
- **Root Directory**: `terraform/` - Main infrastructure setup
- **Backend-Specific**: `microservices/api-[language]/terraform/` - Backend-specific resources

## 🌐 Cloud Platform Support Matrix

The following table shows deployment support for each API backend across major cloud platforms:

```
┌─────────────────────────────────┬─────┬─────┬───────┬─────┬─────┬──────────────┬────────┬───────────┐
│ Language/Framework              │ AWS │ GCP │ Azure │ OCI │ IBM │ DigitalOcean │ Heroku │ Cloudflare│
├─────────────────────────────────┼─────┼─────┼───────┼─────┼─────┼──────────────┼────────┼───────────┤
│ TypeScript/Node.js/Express      │ ✅  │ ✅ │  ✅   │ ✅ │ ✅ │     ✅       │   ✅   │     ✅   │
│ Python/FastAPI                  │ ✅  │ ✅ │  ✅   │ ✅ │ ✅ │     ✅       │   ✅   │     ✅   │
│ Rust/Actix                      │  ❌ │ ❌ │  ❌   │ ❌ │ ❌ │     ❌       │   ❌   │     ✅   │
│ Go/Beego                        │ ✅  │ ✅ │  ❌   │ ✅ │ ✅ │     ✅       │   ✅   │     ❌   │
│ Java/Scala/Play Framework       │ ✅  │ ✅ │  ✅   │ ✅ │ ✅ │     ❌       │   ✅   │     ❌   │
│ Julia/Genie                     │  ❌ │ ❌ │  ❌   │ ❌ │ ❌ │     ❌       │   ❌   │     ❌   │
│ R/Plumber                       │ ❌  │ ❌ │  ❌   │ ❌ │ ✅ │     ✅       │   ❌   │     ❌   │
│ PHP/Laravel                     │ ✅  │ ✅ │  ✅   │ ✅ │ ✅ │     ✅       │   ✅   │     ✅   │
└─────────────────────────────────┴─────┴─────┴───────┴─────┴─────┴──────────────┴────────┴───────────┘
```

### Platform Notes
- **✅ Full Support**: Complete deployment pipeline with CI/CD
- **❌ Not Supported**: Platform limitations or compatibility issues

#### Specific Platform Considerations:

- **Rust/Actix**: Currently only supports Cloudflare Workers due to WebAssembly compilation requirements
- **Julia/Genie**: Limited cloud platform support due to runtime requirements
- **R/Plumber**: Best suited for IBM Cloud and DigitalOcean with R runtime support
- **PHP/Laravel**: Broad platform support with standard PHP runtime availability

**IMPORTANT**
For all the Not Supported API Backends, all can still can be deployed with AWS ECS as a fallback.

## 📋 Deployment Checklist

### Pre-Deployment
- [ ] Configure GitHub Actions secrets
- [ ] Set up environment variables
- [ ] Review and customize workflow files
- [ ] Validate Terraform configurations

### Post-Deployment
- [ ] Verify health check endpoints
- [ ] Test Auth0 JWT validation
- [ ] Confirm MindsDB connectivity
- [ ] Monitor application logs
- [ ] Validate API documentation (Swagger/OpenAPI)

## 🔧 Customization

Each backend can be customized by:

1. **Environment Variables**: Modify `.env` files in each backend directory
2. **Build Commands**: Update workflow files for specific build requirements
3. **Dependencies**: Customize package files (package.json, requirements.txt, etc.)
4. **Infrastructure**: Adjust Terraform configurations for specific cloud resources

## 📚 Additional Resources
- [Backend Development Guide](../../../docs/development/README.md)
- [Port Configuration](../../../docs/Ports/Ports.md)
- [Infrastructure Setup](../../../terraform/README.md)
- [Feature Flags](../../../docs/configuration/feature-flags.md)