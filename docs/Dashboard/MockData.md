# Dashboard Mock Data Documentation

## Overview
This document describes the data structure and schema for the Weave Business Intelligence Dashboard. The data is currently stored in PostgreSQL and consumed by Looker Studio for visualization.

## Database Connection Details

**Database Server**
- **Hostname/IP**: `localhost` (or `127.0.0.1` for local development)
- **Port**: `5432` (default PostgreSQL port)
- **Database**: `weave_bi`
- **Username**: `weave_admin`
- **Password**: `weave_secure_pass_2024` *(Change in production!)*

**Connection String Format**:
```
postgresql://weave_admin:weave_secure_pass_2024@localhost:5432/weave_bi
```

**For Production**: Update these values in your environment variables:
- `DB_HOST`
- `DB_PORT`
- `DB_NAME`
- `DB_USER`
- `DB_PASSWORD`

---

## Database Schema

### 1. KPI Metrics Table
**Table**: `kpi_metrics`

Stores key performance indicators updated in real-time or batch.

| Column | Type | Description |
|--------|------|-------------|
| `id` | SERIAL PRIMARY KEY | Auto-incrementing ID |
| `metric_date` | DATE NOT NULL | Date of the metric snapshot |
| `mrr` | DECIMAL(12,2) | Monthly Recurring Revenue |
| `arr` | DECIMAL(12,2) | Annual Recurring Revenue |
| `arpu` | DECIMAL(10,2) | Average Revenue Per User |
| `total_users` | INTEGER | Total registered users |
| `active_users` | INTEGER | Active users in period |
| `dau` | INTEGER | Daily Active Users |
| `mau` | INTEGER | Monthly Active Users |
| `active_subscriptions` | INTEGER | Number of active subscriptions |
| `churn_rate` | DECIMAL(5,2) | Churn rate percentage |
| `ltv` | DECIMAL(10,2) | Customer Lifetime Value |
| `cac` | DECIMAL(10,2) | Customer Acquisition Cost |
| `ltv_cac_ratio` | DECIMAL(5,2) | LTV to CAC ratio |
| `nps` | INTEGER | Net Promoter Score |
| `uptime` | DECIMAL(5,2) | System uptime percentage |
| `avg_load_time` | DECIMAL(5,2) | Average page load time (seconds) |
| `error_rate` | DECIMAL(5,2) | Error rate percentage |
| `created_at` | TIMESTAMP | Record creation timestamp |
| `updated_at` | TIMESTAMP | Last update timestamp |

**Sample Data** (Latest values as of Nov 2024):
```sql
INSERT INTO kpi_metrics (metric_date, mrr, arr, arpu, total_users, active_users, dau, mau, 
                         active_subscriptions, churn_rate, ltv, cac, ltv_cac_ratio, nps, 
                         uptime, avg_load_time, error_rate) 
VALUES (
    '2024-11-09', 98750.00, 1185000.00, 11.05, 15420, 8930, 3245, 8930,
    1250, 3.2, 450.00, 120.00, 3.75, 42,
    99.94, 1.2, 0.03
);
```

---

### 2. Revenue Trends Table
**Table**: `revenue_trends`

Tracks MRR and ARR over time for trend analysis.

| Column | Type | Description |
|--------|------|-------------|
| `id` | SERIAL PRIMARY KEY | Auto-incrementing ID |
| `period` | VARCHAR(20) NOT NULL | Time period label (e.g., 'Jan', '2024') |
| `period_date` | DATE NOT NULL | Actual date for the period |
| `metric_type` | VARCHAR(10) NOT NULL | 'MRR' or 'ARR' |
| `value` | DECIMAL(12,2) NOT NULL | Revenue value |
| `created_at` | TIMESTAMP | Record creation timestamp |

**Indexes**:
```sql
CREATE INDEX idx_revenue_trends_period_date ON revenue_trends(period_date);
CREATE INDEX idx_revenue_trends_metric_type ON revenue_trends(metric_type);
```

**Sample Data - MRR (6 months)**:
```sql
INSERT INTO revenue_trends (period, period_date, metric_type, value) VALUES
    ('Jan', '2024-01-01', 'MRR', 85000.00),
    ('Feb', '2024-02-01', 'MRR', 87500.00),
    ('Mar', '2024-03-01', 'MRR', 91000.00),
    ('Apr', '2024-04-01', 'MRR', 93500.00),
    ('May', '2024-05-01', 'MRR', 96000.00),
    ('Jun', '2024-06-01', 'MRR', 98750.00);
```

**Sample Data - ARR (10 years)**:
```sql
INSERT INTO revenue_trends (period, period_date, metric_type, value) VALUES
    ('2015', '2015-01-01', 'ARR', 420000.00),
    ('2016', '2016-01-01', 'ARR', 485000.00),
    ('2017', '2017-01-01', 'ARR', 562000.00),
    ('2018', '2018-01-01', 'ARR', 648000.00),
    ('2019', '2019-01-01', 'ARR', 745000.00),
    ('2020', '2020-01-01', 'ARR', 856000.00),
    ('2021', '2021-01-01', 'ARR', 925000.00),
    ('2022', '2022-01-01', 'ARR', 1015000.00),
    ('2023', '2023-01-01', 'ARR', 1095000.00),
    ('2024', '2024-01-01', 'ARR', 1185000.00);
```

---

### 3. User Growth Table
**Table**: `user_growth`

Tracks user metrics with platform breakdown over time.

| Column | Type | Description |
|--------|------|-------------|
| `id` | SERIAL PRIMARY KEY | Auto-incrementing ID |
| `period` | VARCHAR(20) NOT NULL | Time period label |
| `period_date` | DATE NOT NULL | Actual date for the period |
| `metric_type` | VARCHAR(10) NOT NULL | 'MAU' or 'DAU' |
| `dau` | INTEGER | Daily Active Users count |
| `mau` | INTEGER | Monthly Active Users count |
| `mobile` | INTEGER NOT NULL | Users on mobile platforms |
| `desktop` | INTEGER NOT NULL | Users on desktop platforms |
| `website` | INTEGER NOT NULL | Users on website |
| `created_at` | TIMESTAMP | Record creation timestamp |

**Indexes**:
```sql
CREATE INDEX idx_user_growth_period_date ON user_growth(period_date);
CREATE INDEX idx_user_growth_metric_type ON user_growth(metric_type);
```

**Sample Data - MAU (6 months)**:
```sql
INSERT INTO user_growth (period, period_date, metric_type, dau, mau, mobile, desktop, website) VALUES
    ('Jan', '2024-01-01', 'MAU', 2850, 7200, 3600, 2400, 1200),
    ('Feb', '2024-02-01', 'MAU', 2920, 7550, 3775, 2513, 1262),
    ('Mar', '2024-03-01', 'MAU', 3050, 8100, 4050, 2700, 1350),
    ('Apr', '2024-04-01', 'MAU', 3120, 8450, 4225, 2817, 1408),
    ('May', '2024-05-01', 'MAU', 3180, 8720, 4360, 2907, 1453),
    ('Jun', '2024-06-01', 'MAU', 3245, 8930, 4465, 2977, 1488);
```

**Sample Data - DAU (30 days, Nov 2024)**:
```sql
INSERT INTO user_growth (period, period_date, metric_type, dau, mau, mobile, desktop, website) VALUES
    ('Nov 1', '2024-11-01', 'DAU', 2980, 0, 1490, 994, 496),
    ('Nov 2', '2024-11-02', 'DAU', 3020, 0, 1510, 1007, 503),
    ('Nov 3', '2024-11-03', 'DAU', 2850, 0, 1425, 950, 475),
    -- ... (additional 27 days)
    ('Nov 30', '2024-11-30', 'DAU', 3245, 0, 1622, 1082, 541);
```

---

### 4. Subscription Breakdown Table
**Table**: `subscription_breakdown`

Current distribution of subscription plans.

| Column | Type | Description |
|--------|------|-------------|
| `id` | SERIAL PRIMARY KEY | Auto-incrementing ID |
| `snapshot_date` | DATE NOT NULL | Date of this snapshot |
| `plan_name` | VARCHAR(50) NOT NULL | Plan name (Free/Pro/Enterprise) |
| `subscriber_count` | INTEGER NOT NULL | Number of subscribers |
| `percentage` | DECIMAL(5,2) | Percentage of total |
| `created_at` | TIMESTAMP | Record creation timestamp |

**Sample Data**:
```sql
INSERT INTO subscription_breakdown (snapshot_date, plan_name, subscriber_count, percentage) VALUES
    ('2024-11-09', 'Free', 12500, 81.00),
    ('2024-11-09', 'Pro', 2100, 14.00),
    ('2024-11-09', 'Enterprise', 820, 5.00);
```

---

## Looker Studio Integration

### Embed URLs Structure

The dashboard uses toggle switches to load different Looker Studio reports. Each metric type (MRR/ARR, MAU/DAU) corresponds to a unique Looker Studio embed URL.

**Configuration Format**:
```typescript
{
  revenue: {
    mrr: 'https://lookerstudio.google.com/embed/reporting/REPORT_ID_MRR/page/PAGE_ID',
    arr: 'https://lookerstudio.google.com/embed/reporting/REPORT_ID_ARR/page/PAGE_ID'
  },
  users: {
    mau: 'https://lookerstudio.google.com/embed/reporting/REPORT_ID_MAU/page/PAGE_ID',
    dau: 'https://lookerstudio.google.com/embed/reporting/REPORT_ID_DAU/page/PAGE_ID'
  }
}
```

### Data Source Connection in Looker Studio

1. **Connect PostgreSQL to Looker Studio**:
   - In Looker Studio, create a new Data Source
   - Select "PostgreSQL" as the connector
   - Enter connection details:
     - Host: `localhost` (or your server IP)
     - Port: `5432`
     - Database: `weave_bi`
     - Username: `weave_admin`
     - Password: `weave_secure_pass_2024`

2. **Create Separate Reports for Each Metric**:
   - **MRR Report**: Query `revenue_trends` WHERE `metric_type = 'MRR'`
   - **ARR Report**: Query `revenue_trends` WHERE `metric_type = 'ARR'`
   - **MAU Report**: Query `user_growth` WHERE `metric_type = 'MAU'`
   - **DAU Report**: Query `user_growth` WHERE `metric_type = 'DAU'`

3. **Share Reports**:
   - Enable embedding for each report
   - Copy the embed URL
   - Add URLs to your environment config

---

## Environment Variables

Create a `.env` file with the following:

```bash
# PostgreSQL Database
DB_HOST=localhost
DB_PORT=5432
DB_NAME=weave_bi
DB_USER=weave_admin
DB_PASSWORD=weave_secure_pass_2024

# Looker Studio Embeds
LOOKER_MRR_URL=https://lookerstudio.google.com/embed/reporting/YOUR_MRR_REPORT_ID/page/YOUR_PAGE_ID
LOOKER_ARR_URL=https://lookerstudio.google.com/embed/reporting/YOUR_ARR_REPORT_ID/page/YOUR_PAGE_ID
LOOKER_MAU_URL=https://lookerstudio.google.com/embed/reporting/YOUR_MAU_REPORT_ID/page/YOUR_PAGE_ID
LOOKER_DAU_URL=https://lookerstudio.google.com/embed/reporting/YOUR_DAU_REPORT_ID/page/YOUR_PAGE_ID

# Looker Studio Generic URL (for the "Looker Studio" tab)
LOOKER_STUDIO_URL=https://lookerstudio.google.com
```

---

## Data Refresh Strategy

### Real-time Updates
- KPI metrics: Updated every 5 minutes via scheduled job
- User growth: Updated every hour
- Revenue trends: Updated daily at midnight
- Subscription breakdown: Updated daily at midnight

### Batch Updates
Use cron jobs or scheduled tasks to populate data:
```bash
# Daily data refresh at midnight
0 0 * * * /path/to/refresh_daily_metrics.sh

# Hourly user metrics
0 * * * * /path/to/refresh_user_metrics.sh
```

---

## Migration from Mock Data

The mock data previously in `packages/shared-bi-core/src/mock-data/index.ts` has been migrated to PostgreSQL. 

**Migration Script**: See `database/migrations/001_initial_schema.sql`

**Seed Script**: See `database/seeds/seed_mock_data.sql`

---

## API Endpoints (Future)

When implementing API endpoints to fetch this data:

```
GET /api/v1/kpi/latest          - Latest KPI metrics
GET /api/v1/revenue/trend       - Revenue trend (MRR or ARR)
GET /api/v1/users/growth        - User growth (MAU or DAU)
GET /api/v1/subscriptions       - Subscription breakdown
```

Query parameters:
- `metric_type`: 'MRR' | 'ARR' | 'MAU' | 'DAU'
- `start_date`: ISO date string
- `end_date`: ISO date string
- `platform`: 'mobile' | 'desktop' | 'website' (for user growth)

---

## Notes

- **Security**: Change default passwords before deploying to production
- **Backup**: Schedule regular database backups (daily recommended)
- **Scaling**: Consider read replicas for Looker Studio queries if load increases
- **Monitoring**: Set up alerts for database connection failures
- **Indexes**: Additional indexes may be needed based on query patterns

**Last Updated**: November 9, 2024
