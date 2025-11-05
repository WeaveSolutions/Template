# Reports Service Infrastructure

This directory contains the Terraform configuration for deploying the Reports Service, which handles report generation, scheduling, and delivery using PostHog analytics, Looker Studio API and other reporting tools for the Nexpo application.

## 📊 Service Overview

The Reports Service provides:
- **Revenue Analytics**: PostHog-powered funnel statistics and revenue tracking
- **Automated Report Generation**: Scheduled and on-demand reports
- **Multi-Format Export**: PDF, Excel, CSV, JSON formats
- **Interactive Dashboards**: PostHog insights and Looker Studio integration
- **Custom Report Builder**: Drag-and-drop report creation
- **Real-time Analytics**: Live data visualization with PostHog events
- **Report Scheduling**: Automated delivery via email/notifications
- **Data Visualization**: Charts, graphs, and interactive widgets

## 🏗️ Architecture

```
┌─────────────────┐     ┌──────────────┐     ┌─────────────────┐
│   Web/Mobile    │────▶│  API Gateway │────▶│    Reports      │
│   Dashboard     │     └──────────────┘     │    Service      │
└─────────────────┘                          └─────────────────┘
                                                       │
                        ┌──────────────────────────────┼──────────────────────────────┐
                        │                              │                              │
                        ▼                              ▼                              ▼
                ┌──────────────┐              ┌──────────────┐              ┌──────────────┐
                │   PostHog    │              │   Report     │              │   Database   │
                │  Analytics   │              │   Engine     │              │  (Multi-DB)  │
                └──────────────┘              └──────────────┘              └──────────────┘
                        │                              │
                        └──────────────┬───────────────┘
                                       │
                                       ▼
                               ┌──────────────┐
                               │ Looker/Export│
                               │   Service    │
                               │ (PDF/Excel)  │
                               └──────────────┘
                                       │
                                       ▼
                               ┌──────────────┐
                               │ Notification │
                               │   Service    │
                               └──────────────┘
```

## 📁 Directory Structure

```
reports/
├── main.tf                    # Main service configuration
├── variables.tf               # Service variables
├── outputs.tf                # Service outputs
├── provider-specific/        # Cloud-specific configs
│   ├── aws.tf               # ECS, S3, Lambda, QuickSight
│   ├── gcp.tf               # Cloud Run, Cloud Storage, Data Studio
│   ├── azure.tf             # Container Apps, Blob Storage, Power BI
│   ├── oci.tf               # Container Instances, Object Storage
│   ├── ibm.tf               # Code Engine, Cloud Object Storage
│   └── cloudflare.tf        # Workers, R2 Storage
├── analytics/                # Analytics integrations
│   ├── posthog.tf           # PostHog revenue analytics
│   ├── funnel-tracking.tf   # Conversion funnel analysis
│   └── revenue-metrics.tf   # MRR, CLV, churn tracking
├── report-engine/            # Core report generation
│   ├── templates.tf         # Report templates management
│   ├── scheduler.tf         # Report scheduling system
│   ├── generator.tf         # Report generation engine
│   └── cache.tf             # Report caching layer
├── data-sources/             # Data source integrations
│   ├── databases.tf         # Multi-database connections
│   ├── apis.tf              # External API data sources
│   ├── files.tf             # File-based data sources
│   └── real-time.tf         # Real-time data streams
├── visualization/            # Data visualization components
│   ├── looker-studio.tf     # Looker Studio integration
│   ├── charts.tf            # Chart generation (Chart.js, D3)
│   ├── dashboards.tf        # Interactive dashboard builder
│   └── widgets.tf           # Reusable visualization widgets
├── export/                   # Report export functionality
│   ├── pdf-generator.tf     # PDF report generation
│   ├── excel-export.tf      # Excel/CSV export
│   ├── image-export.tf      # Chart/graph image export
│   └── api-export.tf        # JSON/XML API exports
├── delivery/                 # Report delivery mechanisms
│   ├── email-delivery.tf    # Email report delivery
│   ├── webhook-delivery.tf  # Webhook notifications
│   ├── file-storage.tf      # Cloud storage delivery
│   └── portal-access.tf     # Self-service report portal
├── analytics/                # Report usage analytics
│   ├── usage-tracking.tf    # Report access tracking
│   ├── performance.tf       # Report generation performance
│   └── user-behavior.tf     # User interaction analytics
├── security/                 # Report security and access control
│   ├── access-control.tf    # Role-based report access
│   ├── data-privacy.tf      # Data masking and privacy
│   └── audit-logs.tf        # Report access audit logs
├── monitoring/               # Observability configurations
│   ├── dashboards.tf        # Service monitoring dashboards
│   ├── alerts.tf            # Report generation alerts
│   └── performance.tf       # Performance monitoring
└── README.md                # This file
```

## 🚀 Deployment

### Prerequisites
- Terraform >= 1.0
- Cloud provider CLI configured
- PostHog API credentials
- Looker Studio API credentials
- Database connections configured
- Email service for report delivery

### Quick Start
```bash
# Navigate to reports service
cd terraform/microservices/reports

# Initialize Terraform
terraform init

# Plan deployment
terraform plan -var-file="../../terraform.tfvars"

# Deploy service
terraform apply -var-file="../../terraform.tfvars"
```

### Environment-Specific Deployment
```bash
# Development (limited data access)
terraform apply -var="environment=dev" -var="enable_production_data=false"

# Production (full data access)
terraform apply -var="environment=prod" -var="enable_all_data_sources=true"
```

## 🔧 Configuration

### Required Variables
```hcl
# Service Configuration
service_name     = "reports-service"
environment      = "dev"
instance_count   = 2
cpu_units        = 2048
memory_mb        = 4096

# Reports Feature Flags
ENABLE_POSTHOG_ANALYTICS=true
ENABLE_LOOKER_STUDIO=true
ENABLE_PDF_EXPORT=true
ENABLE_EXCEL_EXPORT=true
ENABLE_SCHEDULED_REPORTS=true
ENABLE_REAL_TIME_REPORTS=true
ENABLE_CUSTOM_REPORTS=true
ENABLE_DASHBOARD_BUILDER=true

# PostHog Configuration
POSTHOG_API_KEY=your-posthog-api-key
POSTHOG_PROJECT_ID=your-posthog-project-id
POSTHOG_HOST=https://app.posthog.com

# Looker Studio Configuration
LOOKER_STUDIO_API_KEY=your-looker-studio-api-key
LOOKER_STUDIO_PROJECT_ID=your-looker-project-id
LOOKER_STUDIO_DATASET_ID=your-dataset-id
LOOKER_STUDIO_SERVICE_ACCOUNT=path/to/service-account.json

# Report Generation Configuration
REPORT_CACHE_TTL=3600  # 1 hour
REPORT_MAX_ROWS=100000
REPORT_TIMEOUT_SECONDS=300
REPORT_CONCURRENT_LIMIT=10

# Export Configuration
PDF_GENERATOR_URL=http://pdf-service:3000
EXCEL_EXPORT_ENABLED=true
IMAGE_EXPORT_FORMAT=png  # png, jpg, svg
API_EXPORT_FORMATS=json,xml,csv

# Delivery Configuration
EMAIL_DELIVERY_ENABLED=true
WEBHOOK_DELIVERY_ENABLED=true
FILE_STORAGE_ENABLED=true
REPORT_PORTAL_ENABLED=true
```

### Report Templates Configuration
```hcl
# Pre-built report templates
report_templates = {
  revenue_analytics = {
    name = "Revenue Analytics Report"
    description = "PostHog-powered revenue insights and funnel analysis"
    data_sources = ["posthog", "subscriptions", "payments"]
    default_filters = {
      date_range = "last_30_days"
      revenue_type = "all"
    }
    metrics = ["mrr", "arr", "clv", "churn_rate", "trial_conversion"]
    visualizations = ["funnel_chart", "revenue_trend", "cohort_analysis"]
    export_formats = ["pdf", "excel", "json"]
    schedule_options = ["daily", "weekly", "monthly"]
  }
  
  user_analytics = {
    name = "User Analytics Report"
    description = "User engagement and behavior metrics"
    data_sources = ["users", "events", "sessions", "posthog"]
    default_filters = {
      date_range = "last_30_days"
      user_type = "active"
    }
    visualizations = ["gauge_chart", "trend_lines", "comparison_table"]
    export_formats = ["pdf", "json"]
    schedule_options = ["daily", "weekly"]
  }
  
  kpi_dashboard = {
    name = "KPI Dashboard"
    description = "Key performance indicators overview"
    data_sources = ["kpis", "metrics", "goals"]
    default_filters = {
      date_range = "last_7_days"
      department = "all"
    }
    visualizations = ["gauge_chart", "trend_lines", "comparison_table"]
    export_formats = ["pdf", "json"]
    schedule_options = ["daily", "weekly"]
  }
}
```

### Data Source Configuration
```hcl
# Data source connections
data_sources = {
  posthog = {
    enabled = true
    api_url = "https://app.posthog.com"
    project_id = var.posthog_project_id
    sync_interval = 300 # 5 minutes
    event_types = ["subscription", "payment", "revenue", "funnel"]
  }
  
  postgresql = {
    enabled = true
    connection_string = "postgresql://user:pass@localhost:5432/db"
    query_timeout = 30
    max_connections = 10
  }
  
  mongodb = {
    enabled = true
    connection_string = "mongodb://localhost:27017/db"
    query_timeout = 30
    max_connections = 5
  }
  
  external_apis = {
    google_analytics = {
      enabled = true
      api_key = "your-ga-api-key"
      view_id = "your-ga-view-id"
    }
    
    stripe_api = {
      enabled = true
      secret_key = "sk_your-stripe-secret"
      webhook_endpoint = "/webhooks/stripe"
    }
  }
}
```

## 🔌 API Endpoints

### Report Management APIs
```
GET    /api/reports                  # List available reports
POST   /api/reports                 # Create new report
PUT    /api/reports/:id              # Update report configuration
DELETE /api/reports/:id              # Delete report
GET    /api/reports/:id/preview      # Preview report data
POST   /api/reports/:id/generate     # Generate report on-demand
```

### Report Templates APIs
```
GET    /api/templates               # List report templates
POST   /api/templates               # Create custom template
PUT    /api/templates/:id           # Update template
DELETE /api/templates/:id           # Delete template
POST   /api/templates/:id/clone     # Clone existing template
```

### Scheduling APIs
```
GET    /api/schedules               # List scheduled reports
POST   /api/schedules               # Create report schedule
PUT    /api/schedules/:id           # Update schedule
DELETE /api/schedules/:id           # Delete schedule
POST   /api/schedules/:id/run       # Run scheduled report manually
```

### Export APIs
```
POST   /api/export/pdf              # Export report as PDF
POST   /api/export/excel            # Export report as Excel
POST   /api/export/csv              # Export report as CSV
POST   /api/export/json             # Export report as JSON
GET    /api/export/:id/download     # Download generated export
```

### Dashboard APIs
```
GET    /api/dashboards              # List dashboards
POST   /api/dashboards              # Create new dashboard
PUT    /api/dashboards/:id          # Update dashboard
DELETE /api/dashboards/:id          # Delete dashboard
GET    /api/dashboards/:id/embed    # Get embeddable dashboard URL
```

## 📊 Report Generation Examples

### SQL-Based Report
```sql
-- User engagement report query
WITH user_metrics AS (
    SELECT 
        u.id,
        u.email,
        u.created_at,
        COUNT(DISTINCT s.id) as session_count,
        COUNT(DISTINCT DATE(s.created_at)) as active_days,
        AVG(s.duration_seconds) as avg_session_duration,
        MAX(s.created_at) as last_activity
    FROM users u
    LEFT JOIN sessions s ON u.id = s.user_id
    WHERE s.created_at >= NOW() - INTERVAL '30 days'
    GROUP BY u.id, u.email, u.created_at
),
engagement_segments AS (
    SELECT 
        *,
        CASE 
            WHEN active_days >= 20 THEN 'High'
            WHEN active_days >= 10 THEN 'Medium'
            WHEN active_days >= 1 THEN 'Low'
            ELSE 'Inactive'
        END as engagement_level
    FROM user_metrics
)
SELECT 
    engagement_level,
    COUNT(*) as user_count,
    AVG(session_count) as avg_sessions,
    AVG(avg_session_duration) as avg_duration,
    ROUND(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (), 2) as percentage
FROM engagement_segments
GROUP BY engagement_level
ORDER BY 
    CASE engagement_level 
        WHEN 'High' THEN 1 
        WHEN 'Medium' THEN 2 
        WHEN 'Low' THEN 3 
        WHEN 'Inactive' THEN 4 
    END;
```

### API-Based Report
```javascript
// Revenue analytics report using multiple APIs
async function generateRevenueReport(dateRange) {
    const reportData = {
        period: dateRange,
        metrics: {},
        charts: {}
    };
    
    // Fetch Stripe revenue data
    const stripeRevenue = await fetch('/api/stripe/revenue', {
        method: 'POST',
        body: JSON.stringify({ date_range: dateRange })
    }).then(res => res.json());
    
    // Fetch subscription data
    const subscriptionData = await fetch('/api/subscriptions/analytics', {
        method: 'POST',
        body: JSON.stringify({ date_range: dateRange })
    }).then(res => res.json());
    
    // Calculate key metrics
    reportData.metrics = {
        total_revenue: stripeRevenue.total,
        mrr: subscriptionData.mrr,
        arr: subscriptionData.mrr * 12,
        churn_rate: subscriptionData.churn_rate,
        new_customers: subscriptionData.new_customers,
        growth_rate: ((subscriptionData.mrr - subscriptionData.previous_mrr) / subscriptionData.previous_mrr) * 100
    };
    
    // Generate chart data
    reportData.charts = {
        revenue_trend: stripeRevenue.daily_revenue,
        mrr_growth: subscriptionData.mrr_history,
        customer_acquisition: subscriptionData.acquisition_data
    };
    
    return reportData;
}
```

### PostHog Revenue Analytics
```javascript
// Revenue analytics using PostHog insights
async function generateRevenueReport(dateRange) {
    const posthog = new PostHogClient({
        apiKey: process.env.POSTHOG_API_KEY,
        projectId: process.env.POSTHOG_PROJECT_ID
    });
    
    // Get funnel conversion data
    const funnelInsight = await posthog.insights.get({
        id: 'subscription_funnel',
        date_from: dateRange.start,
        date_to: dateRange.end
    });
    
    // Get revenue metrics
    const revenueMetrics = await posthog.insights.get({
        events: [
            { id: 'subscription_created', math: 'sum', math_property: 'revenue' },
            { id: 'subscription_renewed', math: 'sum', math_property: 'revenue' },
            { id: 'subscription_upgraded', math: 'sum', math_property: 'revenue_delta' }
        ],
        breakdown: 'payment_processor',
        date_from: dateRange.start,
        date_to: dateRange.end
    });
    
    // Get cohort retention
    const cohortData = await posthog.insights.get({
        insight: 'RETENTION',
        target_entity: { type: 'events', id: 'subscription_created' },
        returning_entity: { type: 'events', id: 'subscription_renewed' },
        date_from: '-90d'
    });
    
    return {
        period: dateRange,
        funnel: funnelInsight.results,
        revenue: {
            total: revenueMetrics.results.reduce((sum, r) => sum + r.value, 0),
            by_processor: revenueMetrics.results,
            mrr: calculateMRR(revenueMetrics),
            clv: calculateCLV(cohortData)
        },
        retention: cohortData.results
    };
}
```

## 📈 Visualization Components

### Chart Configuration
```javascript
// Chart.js configuration for revenue trend
const revenueChartConfig = {
    type: 'line',
    data: {
        labels: reportData.dates,
        datasets: [{
            label: 'Daily Revenue',
            data: reportData.revenue,
            borderColor: 'rgb(75, 192, 192)',
            backgroundColor: 'rgba(75, 192, 192, 0.2)',
            tension: 0.1
        }, {
            label: 'Moving Average (7-day)',
            data: reportData.movingAverage,
            borderColor: 'rgb(255, 99, 132)',
            backgroundColor: 'rgba(255, 99, 132, 0.2)',
            borderDash: [5, 5]
        }]
    },
    options: {
        responsive: true,
        plugins: {
            title: {
                display: true,
                text: 'Revenue Trend Analysis'
            },
            legend: {
                position: 'top',
            }
        },
        scales: {
            y: {
                beginAtZero: true,
                ticks: {
                    callback: function(value) {
                        return '$' + value.toLocaleString();
                    }
                }
            }
        }
    }
};
```

### Dashboard Widget
```vue
<!-- Vue.js dashboard widget component -->
<template>
  <div class="report-widget">
    <div class="widget-header">
      <h3>{{ title }}</h3>
      <div class="widget-controls">
        <select v-model="selectedPeriod" @change="refreshData">
          <option value="7d">Last 7 days</option>
          <option value="30d">Last 30 days</option>
          <option value="90d">Last 90 days</option>
        </select>
        <button @click="exportReport" class="export-btn">Export</button>
      </div>
    </div>
    
    <div class="widget-content">
      <div class="metrics-grid">
        <div v-for="metric in metrics" :key="metric.key" class="metric-card">
          <div class="metric-value">{{ formatValue(metric.value) }}</div>
          <div class="metric-label">{{ metric.label }}</div>
          <div class="metric-change" :class="metric.change >= 0 ? 'positive' : 'negative'">
            {{ metric.change >= 0 ? '+' : '' }}{{ metric.change }}%
          </div>
        </div>
      </div>
      
      <div class="chart-container">
        <canvas ref="chartCanvas"></canvas>
      </div>
    </div>
  </div>
</template>

<script>
import Chart from 'chart.js/auto';

export default {
  name: 'ReportWidget',
  props: {
    title: String,
    reportType: String,
    dataSource: String
  },
  data() {
    return {
      selectedPeriod: '30d',
      metrics: [],
      chartData: null,
      chart: null
    };
  },
  methods: {
    async refreshData() {
      const response = await fetch(`/api/reports/widget-data`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          type: this.reportType,
          period: this.selectedPeriod,
          source: this.dataSource
        })
      });
      
      const data = await response.json();
      this.metrics = data.metrics;
      this.updateChart(data.chartData);
    },
    
    updateChart(data) {
      if (this.chart) {
        this.chart.destroy();
      }
      
      const ctx = this.$refs.chartCanvas.getContext('2d');
      this.chart = new Chart(ctx, {
        type: 'line',
        data: data,
        options: {
          responsive: true,
          maintainAspectRatio: false
        }
      });
    },
    
    async exportReport() {
      const response = await fetch(`/api/export/pdf`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          widget_id: this.reportType,
          period: this.selectedPeriod,
          format: 'pdf'
        })
      });
      
      const blob = await response.blob();
      const url = window.URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `${this.title}-${this.selectedPeriod}.pdf`;
      a.click();
    },
    
    formatValue(value) {
      if (typeof value === 'number') {
        return value.toLocaleString();
      }
      return value;
    }
  },
  
  mounted() {
    this.refreshData();
  }
};
</script>
```

## 🔒 Security Features

### Access Control
```python
# Role-based report access control
access_control = {
    "admin": {
        "reports": ["all"],
        "actions": ["create", "read", "update", "delete", "export", "schedule"]
    },
    
    "manager": {
        "reports": ["user_analytics", "revenue_report", "kpi_dashboard"],
        "actions": ["read", "export", "schedule"],
        "filters": {
            "department": "own_department",
            "date_range": "max_1_year"
        }
    },
    
    "analyst": {
        "reports": ["user_analytics", "kpi_dashboard"],
        "actions": ["read", "export"],
        "filters": {
            "date_range": "max_6_months"
        }
    },
    
    "viewer": {
        "reports": ["kpi_dashboard"],
        "actions": ["read"],
        "filters": {
            "date_range": "max_30_days"
        }
    }
}
```

### Data Privacy
```python
# Data masking and privacy controls
privacy_config = {
    "pii_masking": {
        "email": "partial",  # show first 3 chars + domain
        "phone": "partial",  # show last 4 digits
        "name": "initials",  # show initials only
        "address": "city_only"  # show city/state only
    },
    
    "data_retention": {
        "report_cache": "24_hours",
        "export_files": "7_days",
        "audit_logs": "2_years"
    },
    
    "compliance": {
        "gdpr": True,
        "ccpa": True,
        "hipaa": False
    }
}
```

## 📈 Monitoring & Observability

### Health Checks
```
GET /health              # Basic health check
GET /health/datasources  # Data source connectivity
GET /health/posthog      # PostHog API status
GET /metrics            # Prometheus metrics
```

### Key Metrics
- **Report Generation**: Success rate, generation time, queue depth
- **Data Source Performance**: Query response times, connection health
- **Export Performance**: Export success rate, file size, delivery time
- **User Engagement**: Report views, exports, dashboard interactions

### Alerting Rules
```yaml
# Long report generation time
- alert: SlowReportGeneration
  expr: avg_report_generation_time > 300
  for: 5m
  labels:
    severity: warning
  annotations:
    summary: "Reports taking too long to generate"

# Data source connection failure
- alert: DataSourceDown
  expr: data_source_connection_success < 1
  for: 2m
  labels:
    severity: critical
  annotations:
    summary: "Data source connection failed"
```

## 🔄 Integration Points

### Internal Services
- **KPI Engine**: Real-time metrics and KPI data
- **User Service**: User analytics and behavior data
- **Payments Service**: Revenue and subscription analytics
- **Notifications Service**: Report delivery notifications

### External Integrations
- **PostHog**: Revenue analytics and funnel tracking
- **Looker Studio**: Interactive dashboard creation
- **Google Analytics**: Web analytics data
- **Business Intelligence Tools**: Tableau, Power BI, Qlik
- **Email Services**: Report delivery via email
- **Cloud Storage**: Report file storage and archiving

## 🚦 Deployment Strategies

### Blue-Green Deployment
```bash
# Deploy to green environment
terraform apply -var="deployment_slot=green"

# Test report generation
curl -X POST https://reports-green.example.com/api/reports/test

# Switch traffic
terraform apply -var="active_slot=green"
```

### Feature Flag Rollout
```bash
# Enable new visualization features gradually
terraform apply -var="enable_advanced_charts=true" -var="advanced_charts_users=10"
terraform apply -var="advanced_charts_users=50"
terraform apply -var="advanced_charts_users=100"
```

## 🛠️ Development

### Local Development
```bash
# Start dependencies
docker-compose up -d postgres redis

# Set environment variables
export POSTHOG_API_KEY="your-posthog-api-key"
export LOOKER_STUDIO_API_KEY="your-looker-studio-api-key"
export DATABASE_URL="postgresql://localhost:5432/reports"

# Run reports service
pnpm run dev
```

### Testing
```bash
# Unit tests
pnpm run test

# Integration tests with data sources
pnpm run test:integration

# Report generation tests
pnpm run test:reports

# Performance tests
pnpm run test:performance
```

## 🆘 Troubleshooting

### Common Issues

**Report Generation Failures**
```bash
# Check data source connectivity
curl -X GET /api/health/datasources

# Review generation logs
kubectl logs -f deployment/reports-service | grep "generation-error"

# Check query performance
curl -X GET /api/reports/performance/:reportId
```

**Export Issues**
```bash
# Test PDF generation
curl -X POST /api/export/test -d '{"format": "pdf", "data": "sample"}'

# Check export queue
redis-cli llen export_queue

# Review export logs
kubectl logs -f deployment/reports-service | grep "export"
```

**PostHog Integration**
```bash
# Test API connectivity
curl -H "Authorization: Bearer $POSTHOG_API_KEY" https://app.posthog.com/api/v1/reports

# Check service account permissions
gcloud auth list

# Review integration logs
kubectl logs -f deployment/reports-service | grep "posthog"
```

## 📚 Related Documentation

- [PostHog Integration Guide](../../integrations/posthog/README.md)
- [Data Visualization Best Practices](../../visualization/README.md)
- [Report Template Development](../../templates/README.md)
- [Export Service Configuration](../../export/README.md)
- [Dashboard Builder Guide](../../dashboards/README.md)
