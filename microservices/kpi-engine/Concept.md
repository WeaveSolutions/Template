# KPI Engine Microservice Implementation Guide

## Overview

The KPI Engine microservice provides real-time business metric calculation, historical data aggregation, and performance monitoring for the Nexpo application. It supports multi-database analytics with time-series optimization for efficient metric computation and reporting.

## Architecture

### Core Technologies
- **Language**: TypeScript/Node.js
- **Framework**: Express.js or Fastify
- **Analytics DB**: Apache Druid (OLAP) or TimescaleDB (Time-series)
- **Cache**: Redis for computed metrics
- **Stream Processing**: Apache Kafka or AWS Kinesis
- **Query Engine**: Presto/Trino for cross-database queries

### Service Structure
```
kpi-engine-service/
├── src/
│   ├── api/
│   │   ├── routes/
│   │   │   ├── metrics.ts
│   │   │   ├── reports.ts
│   │   │   ├── definitions.ts
│   │   │   └── dashboard.ts
│   │   └── middleware/
│   │       ├── auth.ts
│   │       └── cache.ts
│   ├── engines/
│   │   ├── calculation/
│   │   │   ├── realtime.ts
│   │   │   ├── batch.ts
│   │   │   └── aggregation.ts
│   │   ├── storage/
│   │   │   ├── druid.ts
│   │   │   ├── timescale.ts
│   │   │   └── cache.ts
│   │   └── query/
│   │       ├── builder.ts
│   │       ├── optimizer.ts
│   │       └── executor.ts
│   ├── definitions/
│   │   ├── kpi-registry.ts
│   │   ├── formulas.ts
│   │   └── validators.ts
│   ├── pipeline/
│   │   ├── ingestion.ts
│   │   ├── transformation.ts
│   │   └── streaming.ts
│   ├── scheduler/
│   │   ├── jobs.ts
│   │   └── cron.ts
│   └── utils/
│       ├── logger.ts
│       └── metrics.ts
├── tests/
├── docs/
└── package.json
```

## KPI Definitions

### KPI Configuration Schema
```typescript
// src/definitions/types.ts
export interface KPIDefinition {
  id: string;
  name: string;
  category: 'business' | 'user' | 'performance' | 'custom';
  description: string;
  formula: KPIFormula;
  aggregations: AggregationType[];
  dimensions: string[];
  filters?: FilterCondition[];
  schedule?: {
    frequency: 'realtime' | 'minute' | 'hour' | 'day' | 'week' | 'month';
    timezone?: string;
  };
  thresholds?: {
    warning?: number;
    critical?: number;
  };
  metadata?: Record<string, any>;
}

export interface KPIFormula {
  type: 'simple' | 'composite' | 'custom';
  expression: string; // e.g., "revenue / users"
  variables: Variable[];
}

export interface Variable {
  name: string;
  source: {
    database: string;
    table: string;
    column: string;
    aggregation?: 'sum' | 'count' | 'avg' | 'min' | 'max';
  };
  timeRange?: {
    field: string;
    period: string; // e.g., "last_30_days"
  };
}
```

### Business Metrics Examples
```typescript
// src/definitions/business-metrics.ts
export const businessKPIs: KPIDefinition[] = [
  {
    id: 'mrr',
    name: 'Monthly Recurring Revenue',
    category: 'business',
    description: 'Total monthly recurring revenue from subscriptions',
    formula: {
      type: 'simple',
      expression: 'SUM(subscription_amount)',
      variables: [{
        name: 'subscription_amount',
        source: {
          database: 'payments',
          table: 'subscriptions',
          column: 'amount',
          aggregation: 'sum'
        }
      }]
    },
    aggregations: ['day', 'week', 'month'],
    dimensions: ['plan_type', 'region']
  },
  {
    id: 'cac',
    name: 'Customer Acquisition Cost',
    category: 'business',
    description: 'Average cost to acquire a new customer',
    formula: {
      type: 'composite',
      expression: 'marketing_spend / new_customers',
      variables: [
        {
          name: 'marketing_spend',
          source: {
            database: 'finance',
            table: 'expenses',
            column: 'amount',
            aggregation: 'sum'
          }
        },
        {
          name: 'new_customers',
          source: {
            database: 'users',
            table: 'accounts',
            column: 'id',
            aggregation: 'count'
          }
        }
      ]
    },
    aggregations: ['month', 'quarter'],
    dimensions: ['channel', 'campaign']
  }
];
```

## Calculation Engine

### Real-time Calculation
```typescript
// src/engines/calculation/realtime.ts
import { EventEmitter } from 'events';
import { KPIDefinition, CalculationResult } from '../types';

export class RealtimeCalculationEngine extends EventEmitter {
  private activeCalculations: Map<string, NodeJS.Timer>;
  private cache: RedisCache;

  constructor(cache: RedisCache) {
    super();
    this.activeCalculations = new Map();
    this.cache = cache;
  }

  async startCalculation(kpi: KPIDefinition): Promise<void> {
    if (kpi.schedule?.frequency !== 'realtime') {
      throw new Error('KPI not configured for realtime calculation');
    }

    // Set up event listeners for data changes
    const calculator = setInterval(async () => {
      try {
        const result = await this.calculate(kpi);
        await this.cache.set(`kpi:${kpi.id}:latest`, result);
        this.emit('calculated', { kpiId: kpi.id, result });
      } catch (error) {
        this.emit('error', { kpiId: kpi.id, error });
      }
    }, 5000); // Calculate every 5 seconds

    this.activeCalculations.set(kpi.id, calculator);
  }

  private async calculate(kpi: KPIDefinition): Promise<CalculationResult> {
    const values = await this.fetchVariableValues(kpi.formula.variables);
    const result = this.evaluateFormula(kpi.formula.expression, values);
    
    return {
      kpiId: kpi.id,
      value: result,
      timestamp: new Date(),
      dimensions: await this.calculateDimensions(kpi, values)
    };
  }

  private async fetchVariableValues(variables: Variable[]): Promise<Record<string, number>> {
    const values: Record<string, number> = {};
    
    for (const variable of variables) {
      const query = this.buildQuery(variable);
      const result = await this.executeQuery(query);
      values[variable.name] = result.value;
    }
    
    return values;
  }

  private evaluateFormula(expression: string, values: Record<string, number>): number {
    // Safe expression evaluation using math.js or similar
    const math = require('mathjs');
    const scope = values;
    return math.evaluate(expression, scope);
  }
}
```

### Batch Aggregation
```typescript
// src/engines/calculation/batch.ts
import { CronJob } from 'cron';
import { KPIDefinition, AggregationResult } from '../types';

export class BatchAggregationEngine {
  private jobs: Map<string, CronJob>;
  private storage: StorageEngine;

  constructor(storage: StorageEngine) {
    this.jobs = new Map();
    this.storage = storage;
  }

  scheduleAggregation(kpi: KPIDefinition): void {
    const cronPattern = this.getCronPattern(kpi.schedule?.frequency || 'hour');
    
    const job = new CronJob(cronPattern, async () => {
      try {
        await this.runAggregation(kpi);
      } catch (error) {
        console.error(`Aggregation failed for KPI ${kpi.id}:`, error);
      }
    });

    job.start();
    this.jobs.set(kpi.id, job);
  }

  private async runAggregation(kpi: KPIDefinition): Promise<void> {
    const timeRanges = this.getTimeRanges(kpi.aggregations);
    
    for (const range of timeRanges) {
      const result = await this.aggregateForTimeRange(kpi, range);
      await this.storage.saveAggregation(kpi.id, range, result);
    }
  }

  private async aggregateForTimeRange(
    kpi: KPIDefinition, 
    range: TimeRange
  ): Promise<AggregationResult> {
    // Build and execute aggregation query
    const query = this.buildAggregationQuery(kpi, range);
    const data = await this.executeQuery(query);
    
    return {
      kpiId: kpi.id,
      timeRange: range,
      aggregations: data,
      calculatedAt: new Date()
    };
  }
}
```

## Storage Engines

### Apache Druid Integration
```typescript
// src/engines/storage/druid.ts
import axios from 'axios';
import { StorageEngine, Query, QueryResult } from '../types';

export class DruidStorageEngine implements StorageEngine {
  private baseUrl: string;
  private datasource: string;

  constructor(baseUrl: string, datasource: string) {
    this.baseUrl = baseUrl;
    this.datasource = datasource;
  }

  async query(query: Query): Promise<QueryResult> {
    const druidQuery = this.translateQuery(query);
    
    const response = await axios.post(
      `${this.baseUrl}/druid/v2/sql`,
      { query: druidQuery }
    );

    return this.transformResult(response.data);
  }

  private translateQuery(query: Query): string {
    // Translate generic query to Druid SQL
    return `
      SELECT 
        ${query.metrics.join(', ')},
        ${query.dimensions.join(', ')}
      FROM ${this.datasource}
      WHERE __time >= '${query.timeRange.start}'
        AND __time < '${query.timeRange.end}'
        ${query.filters ? this.buildFilters(query.filters) : ''}
      GROUP BY ${query.dimensions.join(', ')}
    `;
  }

  async ingest(data: any[]): Promise<void> {
    // Ingest data into Druid
    await axios.post(
      `${this.baseUrl}/druid/indexer/v1/task`,
      {
        type: 'index_parallel',
        spec: {
          dataSchema: {
            dataSource: this.datasource,
            timestampSpec: { column: 'timestamp', format: 'iso' },
            dimensionsSpec: { dimensions: this.extractDimensions(data) },
            metricsSpec: this.extractMetrics(data)
          }
        }
      }
    );
  }
}
```

### TimescaleDB Integration
```typescript
// src/engines/storage/timescale.ts
import { Pool } from 'pg';
import { StorageEngine, Query, QueryResult } from '../types';

export class TimescaleStorageEngine implements StorageEngine {
  private pool: Pool;

  constructor(connectionString: string) {
    this.pool = new Pool({ connectionString });
    this.initializeHypertables();
  }

  private async initializeHypertables(): Promise<void> {
    // Create hypertables for time-series data
    await this.pool.query(`
      CREATE TABLE IF NOT EXISTS kpi_metrics (
        time TIMESTAMPTZ NOT NULL,
        kpi_id TEXT NOT NULL,
        value DOUBLE PRECISION,
        dimensions JSONB,
        PRIMARY KEY (time, kpi_id)
      );
    `);

    await this.pool.query(`
      SELECT create_hypertable('kpi_metrics', 'time', 
        chunk_time_interval => INTERVAL '1 day',
        if_not_exists => TRUE
      );
    `);

    // Create continuous aggregates
    await this.pool.query(`
      CREATE MATERIALIZED VIEW kpi_hourly
      WITH (timescaledb.continuous) AS
      SELECT 
        time_bucket('1 hour', time) AS bucket,
        kpi_id,
        AVG(value) as avg_value,
        MAX(value) as max_value,
        MIN(value) as min_value,
        COUNT(*) as count
      FROM kpi_metrics
      GROUP BY bucket, kpi_id
      WITH NO DATA;
    `);
  }

  async query(query: Query): Promise<QueryResult> {
    const sql = this.buildSQL(query);
    const result = await this.pool.query(sql);
    
    return {
      rows: result.rows,
      metadata: {
        rowCount: result.rowCount,
        executionTime: result.command
      }
    };
  }

  async saveMetric(
    kpiId: string, 
    value: number, 
    dimensions?: Record<string, any>
  ): Promise<void> {
    await this.pool.query(
      `INSERT INTO kpi_metrics (time, kpi_id, value, dimensions) 
       VALUES (NOW(), $1, $2, $3)`,
      [kpiId, value, JSON.stringify(dimensions || {})]
    );
  }
}
```

## API Endpoints

### Query Metrics
```typescript
// GET /api/metrics/:kpiId
// GET /api/metrics/:kpiId/history
// GET /api/metrics/:kpiId/dimensions

interface MetricQueryParams {
  timeRange?: {
    start: string;
    end: string;
  };
  aggregation?: 'hour' | 'day' | 'week' | 'month';
  dimensions?: string[];
  filters?: Record<string, any>;
}

interface MetricResponse {
  kpiId: string;
  name: string;
  value: number;
  trend: {
    direction: 'up' | 'down' | 'stable';
    percentage: number;
  };
  history?: TimeSeriesData[];
  dimensions?: DimensionalData[];
}
```

### Reports
```typescript
// GET /api/reports
// POST /api/reports
// GET /api/reports/:reportId

interface Report {
  id: string;
  name: string;
  description?: string;
  kpis: string[];
  schedule?: {
    frequency: 'daily' | 'weekly' | 'monthly';
    time: string;
    timezone: string;
    recipients: string[];
  };
  format: 'dashboard' | 'pdf' | 'csv' | 'json';
  filters?: Record<string, any>;
}
```

### KPI Definitions
```typescript
// GET /api/definitions
// POST /api/definitions
// PUT /api/definitions/:kpiId
// DELETE /api/definitions/:kpiId

interface KPIDefinitionAPI {
  createKPI(definition: KPIDefinition): Promise<KPIDefinition>;
  updateKPI(id: string, updates: Partial<KPIDefinition>): Promise<KPIDefinition>;
  deleteKPI(id: string): Promise<void>;
  validateFormula(formula: KPIFormula): Promise<ValidationResult>;
}
```

## Environment Variables

```bash
# Service Configuration
NODE_ENV=production
PORT=3004
SERVICE_NAME=kpi-engine-service

# Auth0 M2M
AUTH0_DOMAIN=your-tenant.auth0.com
AUTH0_CLIENT_ID=your_m2m_client_id
AUTH0_CLIENT_SECRET=your_m2m_client_secret
AUTH0_AUDIENCE=https://api.nexpo.app

# Storage Engines
DRUID_URL=http://localhost:8082
DRUID_DATASOURCE=nexpo_metrics
TIMESCALE_URL=postgresql://user:password@localhost:5432/metrics

# Redis Cache
REDIS_URL=redis://localhost:6379

# Source Databases
PAYMENTS_DB_URL=postgresql://user:password@localhost:5432/payments
USERS_DB_URL=postgresql://user:password@localhost:5432/users
ANALYTICS_DB_URL=mongodb://localhost:27017/analytics

# Stream Processing
KAFKA_BROKERS=localhost:9092
KAFKA_TOPIC_PREFIX=kpi-engine

# Scheduling
ENABLE_SCHEDULER=true
SCHEDULER_TIMEZONE=UTC

# Performance
MAX_CONCURRENT_CALCULATIONS=10
CACHE_TTL_SECONDS=300
QUERY_TIMEOUT_MS=30000
```

## Deployment

### Docker Configuration
```dockerfile
FROM node:18-alpine

WORKDIR /app

COPY package*.json ./
RUN pnpm ci --only=production

COPY . .
RUN pnpm run build

EXPOSE 3004
CMD ["node", "dist/index.js"]
```

### Health Checks
```typescript
// GET /health
app.get('/health', async (req, res) => {
  const checks = {
    service: 'ok',
    storage: {
      druid: await checkDruid(),
      timescale: await checkTimescale(),
      cache: await checkRedis()
    },
    scheduler: scheduler.isRunning() ? 'ok' : 'stopped',
    activeCalculations: calculationEngine.getActiveCount()
  };

  const healthy = Object.values(checks).every(v => 
    typeof v === 'string' ? v === 'ok' : 
    typeof v === 'object' ? Object.values(v).every(s => s === 'ok') : true
  );

  res.status(healthy ? 200 : 503).json(checks);
});
```

## Performance Optimization

### Caching Strategy
- Cache computed KPIs in Redis with TTL
- Use materialized views for common aggregations
- Implement query result caching
- Pre-compute frequently accessed metrics

### Query Optimization
- Partition tables by time
- Create appropriate indexes
- Use continuous aggregates in TimescaleDB
- Implement query plan caching

## Next Steps

1. **Set Up Storage Layer**
   - Deploy TimescaleDB or Druid
   - Create schema and hypertables
   - Configure continuous aggregates

2. **Implement Core Engine**
   - Build calculation engine
   - Create KPI registry
   - Implement formula parser

3. **Build Data Pipeline**
   - Set up data ingestion
   - Implement streaming processing
   - Create transformation jobs

4. **Create API Layer**
   - Implement REST endpoints
   - Add GraphQL support
   - Build WebSocket for real-time updates

5. **Add Monitoring**
   - Set up Prometheus metrics
   - Create Grafana dashboards
   - Implement alerting rules

6. **Deploy Service**
   - Create Docker image
   - Set up Kubernetes manifests
   - Configure auto-scaling