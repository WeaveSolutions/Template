# Data Sync Service Infrastructure

This directory contains the Terraform configuration for deploying the Data Sync Service, which handles third-party integrations, data synchronization, and workflow automation using n8n for the Nexpo application.

## 🔄 Service Overview

The Data Sync Service provides:
- **Third-Party Integrations**: Connect with external APIs and services
- **Data Synchronization**: Bi-directional sync between systems
- **Workflow Automation**: n8n-powered automation workflows
- **ETL/ELT Processing**: Extract, Transform, Load operations
- **Real-time Data Streaming**: Live data synchronization
- **Batch Processing**: Scheduled bulk data operations
- **API Orchestration**: Coordinate multiple API calls and data flows

## 🏗️ Architecture

```
┌─────────────────┐     ┌──────────────┐     ┌─────────────┐
│  External APIs  │◄───►│  API Gateway │────▶│   Data Sync │
└─────────────────┘     └──────────────┘     │    Service  │
                                              └────────────┘
                                                       │
                        ┌──────────────────────────────┼──────────────────────────────┐
                        │                              │                              │
                        ▼                              ▼                              ▼
                ┌──────────────┐              ┌──────────────┐              ┌──────────────┐
                │     n8n      │              │ Message Queue│              │   Database   │   
                │ (Workflows)  │              │   (Redis)    │              │  (Multi-DB)  │            
                └──────────────┘              └──────────────┘              └──────────────┘
                        │                              │
                        └──────────────┬───────────────┘
                                       │
                                       ▼
                               ┌──────────────┐
                               │ External     │
                               │ Services     │
                               │ (CRM, ERP)   │
                               └──────────────┘
```

## 📁 Directory Structure

```
data-sync/
├── main.tf                    # Main service configuration
├── variables.tf               # Service variables
├── outputs.tf                # Service outputs
├── provider-specific/        # Cloud-specific configs
│   ├── aws.tf               # ECS, SQS, Lambda, EventBridge
│   ├── gcp.tf               # Cloud Run, Pub/Sub, Cloud Functions
│   ├── azure.tf             # Container Apps, Service Bus, Functions
│   ├── oci.tf               # Container Instances, Streaming
│   ├── ibm.tf               # Code Engine, Event Streams
│   └── cloudflare.tf        # Workers, Queues, Durable Objects
├── n8n/                      # n8n workflow automation
│   ├── deployment.tf        # n8n instance deployment
│   ├── workflows.tf         # Workflow definitions
│   ├── credentials.tf       # External service credentials
│   └── webhooks.tf          # Webhook configurations
├── integrations/             # Third-party service integrations
│   ├── crm.tf               # CRM integrations (Salesforce, HubSpot)
│   ├── erp.tf               # ERP integrations (SAP, Oracle)
│   ├── marketing.tf         # Marketing tools (Mailchimp, Klaviyo)
│   ├── analytics.tf         # Analytics platforms (GA, Mixpanel)
│   └── social.tf            # Social media APIs (Twitter, LinkedIn)
├── data-pipelines/           # Data processing pipelines
│   ├── ingestion.tf         # Data ingestion from external sources
│   ├── transformation.tf    # Data transformation and cleaning
│   ├── validation.tf        # Data quality and validation
│   └── output.tf            # Data output to target systems
├── scheduling/               # Job scheduling and orchestration
│   ├── cron-jobs.tf         # Scheduled data sync jobs
│   ├── event-triggers.tf    # Event-driven synchronization
│   └── retry-logic.tf       # Failure handling and retries
├── monitoring/               # Observability configurations
│   ├── dashboards.tf        # Data sync dashboards
│   ├── alerts.tf            # Sync failure alerts
│   └── logs.tf              # Data sync logging
└── README.md                # This file
```

## 🚀 Deployment

### Prerequisites
- Terraform >= 1.0
- Cloud provider CLI configured
- n8n instance or cloud account
- External service API credentials
- Message queue setup

### Quick Start
```bash
# Navigate to data sync service
cd terraform/microservices/data-sync

# Initialize Terraform
terraform init

# Plan deployment
terraform plan -var-file="../../terraform.tfvars"

# Deploy service
terraform apply -var-file="../../terraform.tfvars"
```

### Environment-Specific Deployment
```bash
# Development (limited integrations)
terraform apply -var="environment=dev" -var="enable_production_apis=false"

# Production (all integrations)
terraform apply -var="environment=prod" -var="enable_all_integrations=true"
```

## 🔧 Configuration

### Required Variables
```hcl
# Service Configuration
service_name     = "data-sync-service"
environment      = "dev"
instance_count   = 2
cpu_units        = 1024
memory_mb        = 2048

# Data Sync Feature Flags
ENABLE_N8N_WORKFLOWS=true
ENABLE_CRM_SYNC=true
ENABLE_ERP_SYNC=false
ENABLE_MARKETING_SYNC=true
ENABLE_ANALYTICS_SYNC=true
ENABLE_SOCIAL_SYNC=false

# n8n Configuration
N8N_BASIC_AUTH_ACTIVE=true
N8N_BASIC_AUTH_USER=admin
N8N_BASIC_AUTH_PASSWORD=your-n8n-password
N8N_ENCRYPTION_KEY=your-encryption-key
N8N_DATABASE_TYPE=postgresdb
N8N_DATABASE_HOST=localhost
N8N_DATABASE_PORT=5432
N8N_DATABASE_NAME=n8n
N8N_DATABASE_USER=n8n_user
N8N_DATABASE_PASSWORD=n8n_password

# External Service Credentials
SALESFORCE_CLIENT_ID=your-salesforce-client-id
SALESFORCE_CLIENT_SECRET=your-salesforce-client-secret
HUBSPOT_API_KEY=your-hubspot-api-key
MAILCHIMP_API_KEY=your-mailchimp-api-key
GOOGLE_ANALYTICS_CLIENT_ID=your-ga-client-id
```

### Integration Configuration
```hcl
# CRM Integrations
crm_integrations = {
  salesforce = {
    enabled = true
    sync_interval = "1h"
    objects = ["Contact", "Lead", "Opportunity", "Account"]
    webhook_url = "https://api.example.com/webhooks/salesforce"
  }
  
  hubspot = {
    enabled = true
    sync_interval = "30m"
    objects = ["contacts", "companies", "deals", "tickets"]
    webhook_url = "https://api.example.com/webhooks/hubspot"
  }
}

# Marketing Integrations
marketing_integrations = {
  mailchimp = {
    enabled = true
    sync_interval = "15m"
    lists = ["newsletter", "product_updates"]
    webhook_url = "https://api.example.com/webhooks/mailchimp"
  }
  
  klaviyo = {
    enabled = false
    sync_interval = "1h"
    metrics = ["email_open", "email_click", "purchase"]
  }
}
```

## 🔌 API Endpoints

### Sync Management APIs
```
GET    /api/sync/status              # Get overall sync status
POST   /api/sync/trigger/:integration # Manually trigger sync
GET    /api/sync/history             # Get sync history
DELETE /api/sync/reset/:integration  # Reset sync state
```

### Integration APIs
```
GET    /api/integrations             # List all integrations
POST   /api/integrations             # Add new integration
PUT    /api/integrations/:id         # Update integration
DELETE /api/integrations/:id         # Remove integration
POST   /api/integrations/:id/test    # Test integration connection
```

### Workflow APIs
```
GET    /api/workflows                # List n8n workflows
POST   /api/workflows/execute        # Execute workflow manually
GET    /api/workflows/:id/status     # Get workflow execution status
POST   /api/workflows/:id/activate   # Activate/deactivate workflow
```

### Data Pipeline APIs
```
POST   /api/pipelines/ingest         # Trigger data ingestion
GET    /api/pipelines/status         # Get pipeline status
POST   /api/pipelines/transform      # Execute data transformation
GET    /api/pipelines/metrics        # Get pipeline metrics
```

### n8n Webhooks Calls
```
Custom
```

## 🔄 n8n Workflow Examples

### CRM Contact Sync Workflow
```json
{
  "name": "Salesforce Contact Sync",
  "nodes": [
    {
      "name": "Webhook Trigger",
      "type": "n8n-nodes-base.webhook",
      "parameters": {
        "path": "salesforce-contact"
      }
    },
    {
      "name": "Salesforce",
      "type": "n8n-nodes-base.salesforce",
      "parameters": {
        "operation": "get",
        "resource": "contact",
        "contactId": "={{$json.contactId}}"
      }
    },
    {
      "name": "Database Update",
      "type": "n8n-nodes-base.postgres",
      "parameters": {
        "operation": "insert",
        "table": "contacts",
        "columns": "name,email,phone,company"
      }
    }
  ]
}
```

### Email Marketing Sync Workflow
```json
{
  "name": "Mailchimp Subscriber Sync",
  "nodes": [
    {
      "name": "Schedule Trigger",
      "type": "n8n-nodes-base.cron",
      "parameters": {
        "rule": {
          "interval": [{"field": "minute", "value": 15}]
        }
      }
    },
    {
      "name": "Get New Users",
      "type": "n8n-nodes-base.postgres",
      "parameters": {
        "operation": "select",
        "query": "SELECT * FROM users WHERE created_at > NOW() - INTERVAL '15 minutes'"
      }
    },
    {
      "name": "Mailchimp Add Subscriber",
      "type": "n8n-nodes-base.mailchimp",
      "parameters": {
        "operation": "create",
        "resource": "listMember",
        "list": "newsletter"
      }
    }
  ]
}
```

## 📊 Data Transformation

### Data Mapping Configuration
```python
# Example data mapping for CRM sync
data_mappings = {
    "salesforce_to_internal": {
        "Contact": {
            "FirstName": "first_name",
            "LastName": "last_name", 
            "Email": "email",
            "Phone": "phone_number",
            "Account.Name": "company_name"
        }
    },
    
    "internal_to_mailchimp": {
        "User": {
            "first_name": "FNAME",
            "last_name": "LNAME",
            "email": "email_address",
            "created_at": "signup_date"
        }
    }
}

# Data validation rules
validation_rules = {
    "email": {
        "required": True,
        "format": "email",
        "unique": True
    },
    "phone_number": {
        "format": "phone",
        "normalize": True
    },
    "company_name": {
        "max_length": 255,
        "trim": True
    }
}
```
### n8n MindsDB HTTP Request
Instead of using ETL MindsDB can be used to power Federal Search Queries with the use of AI and Machine Learning.

```http
POST /api/v1/query HTTP/1.1
Host: mindsdb.com
Content-Type: application/json
Authorization: Bearer YOUR_API_KEY

{
    "query": "Use NLP to make a query in plain English"
}
```

### ETL Pipeline Configuration
```sql
-- Example data transformation query
WITH cleaned_contacts AS (
    SELECT 
        TRIM(LOWER(email)) as email,
        INITCAP(TRIM(first_name)) as first_name,
        INITCAP(TRIM(last_name)) as last_name,
        REGEXP_REPLACE(phone, '[^0-9+]', '', 'g') as phone,
        COALESCE(company_name, 'Unknown') as company_name,
        created_at,
        updated_at
    FROM raw_contacts
    WHERE email IS NOT NULL 
    AND email ~ '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$'
),
deduplicated_contacts AS (
    SELECT DISTINCT ON (email) *
    FROM cleaned_contacts
    ORDER BY email, updated_at DESC
)
INSERT INTO contacts (email, first_name, last_name, phone, company_name, created_at, updated_at)
SELECT * FROM deduplicated_contacts
ON CONFLICT (email) DO UPDATE SET
    first_name = EXCLUDED.first_name,
    last_name = EXCLUDED.last_name,
    phone = EXCLUDED.phone,
    company_name = EXCLUDED.company_name,
    updated_at = EXCLUDED.updated_at;
```

## 🔒 Security Features

### Data Protection
- API key encryption and secure storage
- Data masking for sensitive information
- Audit logging for all sync operations
- Rate limiting for external API calls

### Access Control
- Role-based access to sync configurations
- IP whitelisting for webhook endpoints
- OAuth 2.0 for external service authentication
- Secure credential management

### Compliance
```python
# Data privacy compliance features
compliance_config = {
    "gdpr": {
        "enabled": True,
        "data_retention_days": 365,
        "anonymization_fields": ["email", "phone", "ip_address"],
        "deletion_webhook": "https://api.example.com/gdpr/delete"
    },
    
    "ccpa": {
        "enabled": True,
        "opt_out_webhook": "https://api.example.com/ccpa/opt-out"
    },
    
    "data_classification": {
        "pii_fields": ["email", "phone", "name", "address"],
        "sensitive_fields": ["ssn", "credit_card", "bank_account"],
        "encryption_required": True
    }
}
```

## 📈 Monitoring & Observability

### Health Checks
```
GET /health              # Basic health check
GET /health/integrations # External service connectivity
GET /health/n8n          # n8n workflow engine status
GET /metrics            # Prometheus metrics
```

### Key Metrics
- **Sync Performance**: Success rate, sync duration, data throughput
- **Integration Health**: API response times, error rates, quota usage
- **Data Quality**: Validation failures, duplicate records, missing data
- **System Health**: CPU, memory, queue depth, workflow execution time

### Alerting Rules
```yaml
# High sync failure rate
- alert: HighSyncFailureRate
  expr: (failed_syncs / total_syncs) > 0.1
  for: 5m
  labels:
    severity: warning
  annotations:
    summary: "High data sync failure rate"

# External API rate limit approaching
- alert: APIRateLimitApproaching
  expr: api_requests_remaining < 100
  for: 1m
  labels:
    severity: critical
  annotations:
    summary: "External API rate limit approaching"
```

## 🔄 Integration Points

### Internal Services
- **User Service**: User data synchronization
- **KPI Engine**: Business metrics from external sources
- **Notifications Service**: Sync status notifications
- **AI Advisor Service**: External data for AI insights

### External Integrations
- **CRM Systems**: Salesforce, HubSpot, Pipedrive, Zoho
- **ERP Systems**: SAP, Oracle NetSuite, Microsoft Dynamics
- **Marketing Tools**: Mailchimp, Klaviyo, Constant Contact
- **Analytics Platforms**: Google Analytics, Mixpanel, Amplitude
- **Social Media**: Twitter, LinkedIn, Facebook, Instagram

## 🚦 Deployment Strategies

### Blue-Green Deployment
```bash
# Deploy to green environment
terraform apply -var="deployment_slot=green"

# Test data sync functionality
curl -X POST https://data-sync-green.example.com/api/sync/test

# Switch traffic
terraform apply -var="active_slot=green"
```

### Gradual Integration Rollout
```bash
# Enable integrations gradually
terraform apply -var="enable_crm_sync=true"
terraform apply -var="enable_marketing_sync=true"
terraform apply -var="enable_analytics_sync=true"
```

## 🛠️ Development

### Local Development
```bash
# Start n8n and dependencies
docker-compose up -d n8n postgresql redis

# Set environment variables
export N8N_BASIC_AUTH_USER="admin"
export N8N_BASIC_AUTH_PASSWORD="password"
export SALESFORCE_CLIENT_ID="your-sf-client-id"
export HUBSPOT_API_KEY="your-hubspot-key"

# Run data sync service
pnpm run dev
```

### Testing
```bash
# Unit tests
pnpm run test

# Integration tests with external APIs
pnpm run test:integration

# n8n workflow tests
pnpm run test:workflows

# Data quality tests
pnpm run test:data-quality
```

## 🆘 Troubleshooting

### Common Issues

**Sync Failures**
```bash
# Check external API connectivity
curl -H "Authorization: Bearer $API_KEY" https://api.external-service.com/health

# Review sync logs
kubectl logs -f deployment/data-sync-service | grep "sync-error"

# Check n8n workflow status
curl http://n8n:5678/rest/executions
```

**Data Quality Issues**
```bash
# Run data validation
curl -X POST /api/pipelines/validate

# Check duplicate records
psql -c "SELECT email, COUNT(*) FROM contacts GROUP BY email HAVING COUNT(*) > 1"

# Review transformation logs
kubectl logs -f deployment/data-sync-service | grep "transform"
```

**Performance Issues**
```bash
# Monitor sync performance
curl /api/sync/metrics

# Check queue depth
redis-cli llen sync_queue

# Review n8n performance
curl http://n8n:5678/rest/health
```

## 📚 Related Documentation

- [n8n Workflow Documentation](../../n8n/README.md)
- [External API Integration Guide](../../integrations/README.md)
- [Data Pipeline Configuration](../../data-pipelines/README.md)
- [Monitoring and Alerting](../../monitoring/README.md)
- [Security Best Practices](../../security/README.md)
