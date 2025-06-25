# KPI Engine Service Infrastructure

This directory contains the Terraform configuration for deploying the KPI Engine Service, which calculates and aggregates business metrics in real-time for the Nexpo application.

## 📊 Service Overview

The KPI Engine provides:
- **Real-time Metric Calculation**: Live business KPI computation
- **Historical Data Aggregation**: Time-series data analysis and trends
- **Custom KPI Definitions**: Configurable business metrics
- **Scheduled Reports**: Automated metric reporting
- **Multi-Database Analytics**: Works across PostgreSQL, MongoDB, CosmosDB
- **Time-Series Optimization**: Apache Druid and TimescaleDB integration
- **Performance Monitoring**: Application and business performance metrics

## 🏗️ Architecture

```
┌─────────────────┐     ┌──────────────┐     ┌─────────────┐
│  Dashboard/App  │────▶│  API Gateway │────▶│ KPI Engine  │
└─────────────────┘     └──────────────┘     └─────────────┘
                                                      │
                        ┌─────────────────────────────┼─────────────────────────────┐
                        │                             │                             │
                        ▼                             ▼                             ▼
                ┌──────────────┐              ┌──────────────┐              ┌──────────────┐
                │ Apache Druid │              │ TimescaleDB  │              │    Cache     │
                │   (OLAP)     │              │(Time-Series) │              │   (Redis)    │
                └──────────────┘              └──────────────┘              └──────────────┘
                        │                             │
                        └─────────────┬───────────────┘
                                      │
                                      ▼
                              ┌──────────────┐
                              │ Source DBs   │
                              │(Multi-Provider)│
                              └──────────────┘
```

## 📁 Directory Structure

```
kpi-engine/
├── main.tf                    # Main service configuration
├── variables.tf               # Service variables
├── outputs.tf                # Service outputs
├── provider-specific/        # Cloud-specific configs
│   ├── aws.tf               # ECS, RDS, ElastiCache, Kinesis
│   ├── gcp.tf               # Cloud Run, BigQuery, Dataflow
│   ├── azure.tf             # Container Apps, Synapse, Event Hubs
│   ├── oci.tf               # Container Instances, Analytics Cloud
│   ├── ibm.tf               # Code Engine, Db2 Warehouse
│   └── cloudflare.tf        # Workers, Analytics Engine
├── analytics/                # Analytics engine configurations
│   ├── druid.tf             # Apache Druid cluster setup
│   ├── timescaledb.tf       # TimescaleDB configuration
│   ├── bigquery.tf          # BigQuery datasets and tables
│   └── synapse.tf           # Azure Synapse Analytics
├── data-pipeline/            # Data processing pipelines
│   ├── ingestion.tf         # Data ingestion from source DBs
│   ├── transformation.tf    # ETL/ELT transformations
│   ├── aggregation.tf       # KPI calculation jobs
│   └── streaming.tf         # Real-time data streaming
├── kpi-definitions/          # KPI configuration files
│   ├── business-metrics.tf  # Revenue, growth, conversion KPIs
│   ├── user-metrics.tf      # User engagement and retention
│   ├── performance-metrics.tf # Application performance KPIs
│   └── custom-metrics.tf    # Customer-defined metrics
├── monitoring/               # Observability configurations
│   ├── dashboards.tf        # Grafana/CloudWatch dashboards
│   ├── alerts.tf            # Alert rules and notifications
│   └── logs.tf              # Log aggregation and analysis
└── README.md                # This file
```

## 🚀 Deployment

### Prerequisites
- Terraform >= 1.0
- Cloud provider CLI configured
- Analytics database credentials (Druid/TimescaleDB)
- Source database connections
- Message queue setup (for real-time processing)

### Quick Start
```bash
# Navigate to KPI engine service
cd terraform/microservices/kpi-engine

# Initialize Terraform
terraform init

# Plan deployment
terraform plan -var-file="../../terraform.tfvars"

# Deploy service
terraform apply -var-file="../../terraform.tfvars"
```

### Environment-Specific Deployment
```bash
# Development (single node)
terraform apply -var="environment=dev" -var="druid_cluster_size=1"

# Production (clustered)
terraform apply -var="environment=prod" -var="druid_cluster_size=3"
```

## 🔧 Configuration

### Required Variables
```hcl
# Service Configuration
service_name        = "kpi-engine"
environment         = "dev"
instance_count      = 2
cpu_units          = 1024
memory_mb          = 2048

# Analytics Engine Configuration
enable_druid       = true
enable_timescaledb = true
enable_bigquery    = false
enable_synapse     = false

druid_cluster_size    = 3
druid_storage_size_gb = 100
timescale_storage_gb  = 50

# Data Source Configuration
source_databases = [
  {
    type = "postgresql"
    connection_string = "postgresql://user:pass@host:5432/db"
    tables = ["users", "orders", "events"]
  },
  {
    type = "mongodb"
    connection_string = "mongodb://user:pass@host:27017/db"
    collections = ["user_activity", "transactions"]
  }
]

# Real-time Processing
enable_streaming     = true
stream_buffer_size   = 1000
processing_interval  = "30s"
```

### KPI Definitions
```hcl
# Business KPIs
business_kpis = {
  monthly_recurring_revenue = {
    query = "SELECT SUM(amount) FROM subscriptions WHERE status='active'"
    schedule = "0 0 * * *"  # Daily at midnight
    retention = "365d"
  }
  
  customer_acquisition_cost = {
    query = "SELECT marketing_spend / new_customers FROM monthly_metrics"
    schedule = "0 0 1 * *"  # Monthly
    retention = "1095d"     # 3 years
  }
  
  churn_rate = {
    query = "SELECT cancelled_users / total_users * 100 FROM user_metrics"
    schedule = "0 0 * * 1"  # Weekly on Monday
    retention = "365d"
  }
}

# User Engagement KPIs
user_kpis = {
  daily_active_users = {
    query = "SELECT COUNT(DISTINCT user_id) FROM user_activity WHERE date = CURRENT_DATE"
    schedule = "0 1 * * *"  # Daily at 1 AM
    retention = "90d"
  }
  
  session_duration = {
    query = "SELECT AVG(session_length) FROM user_sessions WHERE date >= CURRENT_DATE - INTERVAL '7 days'"
    schedule = "0 */6 * * *"  # Every 6 hours
    retention = "30d"
  }
}
```

## 📊 Analytics Engines

### Apache Druid Configuration
```hcl
# Druid cluster for OLAP queries
resource "druid_cluster" "main" {
  cluster_name = "${var.service_name}-druid-${var.environment}"
  
  # Coordinator nodes
  coordinator_nodes = {
    count         = 1
    instance_type = "m5.large"
    storage_gb    = 20
  }
  
  # Historical nodes (data storage)
  historical_nodes = {
    count         = var.druid_cluster_size
    instance_type = "r5.xlarge"
    storage_gb    = var.druid_storage_size_gb
  }
  
  # Broker nodes (query processing)
  broker_nodes = {
    count         = 2
    instance_type = "c5.large"
  }
  
  # Real-time ingestion
  middlemanager_nodes = {
    count         = 2
    instance_type = "m5.large"
  }
}
```

### TimescaleDB Configuration
```sql
-- Time-series tables for KPI storage
CREATE TABLE kpi_metrics (
    time TIMESTAMPTZ NOT NULL,
    kpi_name VARCHAR(100) NOT NULL,
    kpi_value DOUBLE PRECISION,
    dimensions JSONB,
    metadata JSONB
);

-- Convert to hypertable for time-series optimization
SELECT create_hypertable('kpi_metrics', 'time');

-- Create indexes for efficient queries
CREATE INDEX idx_kpi_metrics_name_time ON kpi_metrics (kpi_name, time DESC);
CREATE INDEX idx_kpi_metrics_dimensions ON kpi_metrics USING GIN (dimensions);

-- Continuous aggregates for common queries
CREATE MATERIALIZED VIEW kpi_hourly
WITH (timescaledb.continuous) AS
SELECT time_bucket('1 hour', time) AS bucket,
       kpi_name,
       AVG(kpi_value) as avg_value,
       MAX(kpi_value) as max_value,
       MIN(kpi_value) as min_value,
       COUNT(*) as data_points
FROM kpi_metrics
GROUP BY bucket, kpi_name;

-- Retention policy
SELECT add_retention_policy('kpi_metrics', INTERVAL '2 years');
```

## 🔌 API Endpoints

### KPI Query APIs
```
GET    /api/kpis                    # List all available KPIs
GET    /api/kpis/:name              # Get specific KPI current value
GET    /api/kpis/:name/history      # Get KPI historical data
GET    /api/kpis/:name/trend        # Get KPI trend analysis
POST   /api/kpis/query              # Custom KPI query
```

### Real-time APIs
```
GET    /api/kpis/realtime/:name     # Get real-time KPI value
WS     /api/kpis/stream             # WebSocket for live KPI updates
POST   /api/kpis/trigger/:name      # Manually trigger KPI calculation
```

### Configuration APIs
```
GET    /api/kpis/definitions        # List KPI definitions
POST   /api/kpis/definitions        # Create new KPI definition
PUT    /api/kpis/definitions/:name  # Update KPI definition
DELETE /api/kpis/definitions/:name  # Delete KPI definition
```

### Analytics APIs
```
GET    /api/analytics/dashboard     # Dashboard data for multiple KPIs
GET    /api/analytics/compare       # Compare KPIs across time periods
GET    /api/analytics/forecast      # KPI forecasting and predictions
GET    /api/analytics/alerts        # Active KPI alerts and thresholds
```

## 📈 KPI Categories

### Business Metrics
- **Revenue KPIs**: MRR, ARR, Revenue Growth Rate
- **Customer KPIs**: CAC, LTV, Churn Rate, NPS
- **Sales KPIs**: Conversion Rate, Sales Velocity, Pipeline Value
- **Marketing KPIs**: ROAS, Lead Generation, Brand Awareness

### User Engagement Metrics
- **Activity KPIs**: DAU, MAU, Session Duration, Page Views
- **Retention KPIs**: User Retention Rate, Feature Adoption
- **Behavior KPIs**: Click-through Rate, Bounce Rate, Time on Site

### Performance Metrics
- **Application KPIs**: Response Time, Error Rate, Uptime
- **Infrastructure KPIs**: CPU Usage, Memory Usage, Database Performance
- **Security KPIs**: Failed Login Attempts, Security Incidents

### Custom Business KPIs
- Industry-specific metrics
- Company-specific calculations
- Department-level KPIs
- Project-specific metrics

## 🔄 Data Pipeline

### Data Ingestion
```python
# Example data ingestion configuration
ingestion_sources = {
    "user_events": {
        "source": "postgresql://app-db/events",
        "query": "SELECT * FROM user_events WHERE created_at > ?",
        "interval": "5m",
        "batch_size": 1000
    },
    "transaction_data": {
        "source": "mongodb://payments-db/transactions",
        "collection": "completed_transactions",
        "interval": "1m",
        "batch_size": 500
    }
}
```

### ETL Transformations
```sql
-- Example KPI calculation query
WITH monthly_revenue AS (
    SELECT 
        DATE_TRUNC('month', created_at) as month,
        SUM(amount) as revenue,
        COUNT(DISTINCT user_id) as customers
    FROM transactions 
    WHERE status = 'completed'
    GROUP BY DATE_TRUNC('month', created_at)
),
growth_calculation AS (
    SELECT 
        month,
        revenue,
        customers,
        LAG(revenue) OVER (ORDER BY month) as prev_revenue,
        (revenue - LAG(revenue) OVER (ORDER BY month)) / 
        LAG(revenue) OVER (ORDER BY month) * 100 as growth_rate
    FROM monthly_revenue
)
INSERT INTO kpi_metrics (time, kpi_name, kpi_value, dimensions)
SELECT 
    month,
    'monthly_revenue_growth',
    growth_rate,
    jsonb_build_object('revenue', revenue, 'customers', customers)
FROM growth_calculation
WHERE growth_rate IS NOT NULL;
```

## 🔒 Security Features

### Data Access Control
- Role-based access to KPI data
- Row-level security for sensitive metrics
- API key authentication for external access
- Audit logging for all KPI queries

### Data Privacy
- PII anonymization in analytics
- GDPR compliance for user metrics
- Data retention policies
- Secure data transmission (TLS)

## 📈 Monitoring & Observability

### Health Checks
```
GET /health              # Basic health check
GET /health/detailed     # Health with data pipeline status
GET /health/data-sources # Source database connectivity
GET /metrics            # Prometheus metrics
```

### Key Metrics
- **Performance**: Query response time, data processing latency
- **Data Quality**: Data completeness, accuracy, freshness
- **System Health**: CPU, memory, storage usage
- **Business Impact**: KPI calculation success rate

### Alerts
- KPI threshold breaches
- Data pipeline failures
- Performance degradation
- Data quality issues

## 🔄 Integration Points

### Internal Services
- **User Service**: User behavior and engagement metrics
- **Auth Service**: Authentication and session analytics
- **Payments Service**: Revenue and transaction metrics
- **Notifications Service**: Communication effectiveness KPIs

### External Integrations
- **Business Intelligence Tools**: Tableau, Power BI, Looker
- **Data Warehouses**: Snowflake, Redshift, BigQuery
- **Monitoring Tools**: Grafana, DataDog, New Relic
- **Alerting Systems**: PagerDuty, Slack, Email

## 🚦 Deployment Strategies

### Blue-Green Deployment
```bash
# Deploy to green environment
terraform apply -var="deployment_slot=green"

# Validate KPI calculations
curl https://kpi-green.example.com/health/detailed

# Switch traffic
terraform apply -var="active_slot=green"
```

### Canary Deployment
```bash
# Deploy canary with limited KPI processing
terraform apply -var="canary_percentage=10"

# Monitor data quality and performance
# Gradually increase traffic
terraform apply -var="canary_percentage=100"
```

## 🛠️ Development

### Local Development
```bash
# Start analytics stack
docker-compose up -d druid timescaledb redis

# Set environment variables
export DRUID_URL="http://localhost:8082"
export TIMESCALE_URL="postgresql://localhost:5432/timescale"
export REDIS_URL="redis://localhost:6379"

# Run KPI engine
npm run dev
```

### Testing
```bash
# Unit tests
npm run test

# Integration tests (with test databases)
npm run test:integration

# Data quality tests
npm run test:data-quality

# Performance tests
npm run test:performance
```

## 🆘 Troubleshooting

### Common Issues

**Slow KPI Calculations**
```bash
# Check Druid query performance
curl "http://druid-broker:8082/druid/v2/sql" -d "SELECT * FROM kpi_metrics LIMIT 10"

# Optimize TimescaleDB queries
psql -c "EXPLAIN ANALYZE SELECT * FROM kpi_metrics WHERE time > NOW() - INTERVAL '1 day'"
```

**Data Pipeline Failures**
```bash
# Check ingestion status
kubectl logs -f deployment/kpi-ingestion

# Verify source database connectivity
pg_isready -h source-db -p 5432
```

**Memory Issues**
```bash
# Check Druid memory usage
kubectl top pods -l app=druid-historical

# Monitor JVM heap usage
curl http://druid-broker:8082/status/health
```

## 📚 Related Documentation

- [Analytics Database Setup](../../providers/analytics/README.md)
- [Data Pipeline Configuration](../../data-pipeline/README.md)
- [Business Intelligence Integration](../../integrations/bi/README.md)
- [Monitoring and Alerting](../../monitoring/README.md)
- [Performance Optimization](../../performance/README.md)
