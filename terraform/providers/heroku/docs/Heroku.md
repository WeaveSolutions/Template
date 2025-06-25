# Heroku Infrastructure Provider

This module provides infrastructure provisioning for Heroku platform using Terraform.

## Overview

Heroku is a cloud platform as a service (PaaS) that enables developers to build, run, and operate applications entirely in the cloud.

## Features

- **Dynos**: Lightweight containers that run application code
- **Add-ons**: Managed services like databases, caching, monitoring
- **Pipelines**: Continuous delivery workflow
- **Review Apps**: Temporary apps for pull requests
- **Config Vars**: Environment variable management
- **Releases**: Immutable deployments with rollback capabilities

## Directory Structure

```
heroku/
├── environments/
│   ├── development/
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   ├── outputs.tf
│   │   └── terraform.tfvars.example
│   ├── staging/
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   ├── outputs.tf
│   │   └── terraform.tfvars.example
│   └── production/
│       ├── main.tf
│       ├── variables.tf
│       ├── outputs.tf
│       └── terraform.tfvars.example
├── modules/
│   ├── app/
│   ├── addons/
│   └── pipeline/
├── variables.tf
├── outputs.tf
└── README.md
```

## Environment Configuration

Each environment includes:

- **Web Application**: Next.js frontend with appropriate dyno sizing
- **API Service**: Backend API with database connectivity
- **Database**: PostgreSQL add-on with appropriate plan
- **Redis**: Caching layer for session management
- **Monitoring**: Application performance monitoring

## Quick Start

1. **Configure Environment Variables**:
   ```bash
   export HEROKU_API_KEY="your-heroku-api-key"
   export HEROKU_EMAIL="your-email@example.com"
   ```

2. **Initialize Terraform**:
   ```bash
   cd terraform/providers/heroku/environments/development
   terraform init
   ```

3. **Plan Infrastructure**:
   ```bash
   terraform plan -var-file="terraform.tfvars"
   ```

4. **Apply Configuration**:
   ```bash
   terraform apply
   ```

## Environment Variables

### Required
- `HEROKU_API_KEY`: Heroku API key for authentication
- `HEROKU_EMAIL`: Email address associated with Heroku account

### Optional
- `HEROKU_REGION`: Heroku region (us, eu)
- `HEROKU_STACK`: Application stack (heroku-22, heroku-20)

## Dyno Types

### Development
- **Web**: hobby dynos for cost efficiency
- **Worker**: hobby dynos for background processing

### Staging
- **Web**: standard-1x dynos for testing
- **Worker**: standard-1x dynos for background jobs

### Production
- **Web**: standard-2x or performance dynos
- **Worker**: standard-1x or standard-2x dynos
- **Scheduler**: periodic jobs with Heroku Scheduler

## Add-ons

### Database
- **Development**: `heroku-postgresql:mini` (free tier)
- **Staging**: `heroku-postgresql:basic`
- **Production**: `heroku-postgresql:standard-0` or higher

### Caching
- **Development**: `heroku-redis:mini` (free tier)
- **Staging**: `heroku-redis:premium-0`
- **Production**: `heroku-redis:premium-1` or higher

### Monitoring
- **Development**: Basic logging
- **Staging**: `papertrail:choklad` for log management
- **Production**: `newrelic:wayne` for APM

## Deployment

### Git-based Deployment
```bash
# Connect to Heroku Git remote
git remote add heroku-dev https://git.heroku.com/your-app-dev.git
git remote add heroku-staging https://git.heroku.com/your-app-staging.git
git remote add heroku-prod https://git.heroku.com/your-app-prod.git

# Deploy to environments
git push heroku-dev development:main
git push heroku-staging staging:main
git push heroku-prod main:main
```

### Container Registry
```bash
# Login to Heroku Container Registry
heroku container:login

# Build and push Docker image
heroku container:push web --app your-app-name

# Release the image
heroku container:release web --app your-app-name
```

## Configuration Management

### Config Vars
Heroku uses environment variables for application configuration:

```bash
# Set configuration variables
heroku config:set NODE_ENV=production --app your-app-name
heroku config:set DATABASE_URL=postgresql://... --app your-app-name

# View configuration
heroku config --app your-app-name
```

### buildpacks
Heroku automatically detects buildpacks, but you can specify them:

```bash
# Set Node.js buildpack
heroku buildpacks:set heroku/nodejs --app your-app-name

# Add multiple buildpacks
heroku buildpacks:add --index 1 heroku/nodejs --app your-app-name
heroku buildpacks:add --index 2 heroku/postgresql --app your-app-name
```

## Scaling

### Manual Scaling
```bash
# Scale web dynos
heroku ps:scale web=2 --app your-app-name

# Scale worker dynos
heroku ps:scale worker=1 --app your-app-name
```

### Autoscaling
For production applications, consider using Heroku's autoscaling features or third-party solutions.

## Monitoring and Logging

### Built-in Monitoring
- **Metrics**: CPU, memory, response times
- **Logs**: Application and system logs
- **Alerts**: Threshold-based notifications

### Third-party Add-ons
- **New Relic**: Application performance monitoring
- **Papertrail**: Log management and search
- **LogDNA**: Real-time log analysis

## Security

### SSL/TLS
- **Automatic SSL**: Free SSL certificates for all apps
- **Custom Domains**: SSL certificates for custom domains
- **SSL Endpoint**: Dedicated SSL for custom domains

### Access Control
- **Collaborators**: Team access management
- **OAuth**: Third-party application authorization
- **Private Spaces**: Network isolation for enterprise

## Best Practices

1. **Environment Parity**: Keep development, staging, and production as similar as possible
2. **Config Management**: Use environment variables for configuration
3. **Process Types**: Define clear process types in Procfile
4. **Resource Sizing**: Right-size dynos based on application needs
5. **Add-on Management**: Use appropriate add-on plans for each environment
6. **Monitoring**: Implement comprehensive monitoring and alerting

## Cost Optimization

- **Hobby Dynos**: Free tier for development and testing
- **Dyno Sleeping**: Hobby dynos sleep after 30 minutes of inactivity
- **Add-on Plans**: Choose appropriate plans for each environment
- **Resource Monitoring**: Monitor and optimize resource usage

## Pipeline Management

Heroku Pipelines provide a visual representation of your application's deployment flow:

```
GitHub → Review Apps → Staging → Production
```

### Pipeline Features
- **Review Apps**: Temporary apps for pull requests
- **Promotion**: One-click promotions between stages
- **CI/CD Integration**: Automated testing and deployment

## Support

- [Heroku Documentation](https://devcenter.heroku.com/)
- [Terraform Heroku Provider](https://registry.terraform.io/providers/heroku/heroku/latest/docs)
- [Heroku Support](https://help.heroku.com/)
