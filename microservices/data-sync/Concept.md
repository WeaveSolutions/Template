# Data Sync Microservice Implementation Guide

## Overview

The Data Sync microservice handles third-party integrations, data synchronization, and workflow automation for the Nexpo application. It leverages n8n for visual workflow automation while providing programmatic APIs for real-time and batch data synchronization across multiple systems.

## Architecture

### Core Technologies
- **Language**: TypeScript/Node.js
- **Framework**: Express.js or Fastify
- **Workflow Engine**: n8n (self-hosted or cloud)
- **Message Queue**: Redis, RabbitMQ, or AWS SQS
- **Database**: PostgreSQL for sync state and metadata
- **Cache**: Redis for temporary data and deduplication

### Service Structure
```
data-sync-service/
├── src/
│   ├── api/
│   │   ├── routes/
│   │   │   ├── sync.ts
│   │   │   ├── integrations.ts
│   │   │   ├── workflows.ts
│   │   │   └── mappings.ts
│   │   └── middleware/
│   │       ├── auth.ts
│   │       └── validation.ts
│   ├── integrations/
│   │   ├── crm/
│   │   │   ├── salesforce.ts
│   │   │   ├── hubspot.ts
│   │   │   └── types.ts
│   │   ├── erp/
│   │   │   ├── sap.ts
│   │   │   ├── oracle.ts
│   │   │   └── types.ts
│   │   ├── marketing/
│   │   │   ├── mailchimp.ts
│   │   │   ├── klaviyo.ts
│   │   │   └── types.ts
│   │   └── base/
│   │       ├── connector.ts
│   │       └── transformer.ts
│   ├── sync-engine/
│   │   ├── realtime.ts
│   │   ├── batch.ts
│   │   ├── conflict-resolver.ts
│   │   └── state-manager.ts
│   ├── n8n/
│   │   ├── client.ts
│   │   ├── workflow-manager.ts
│   │   └── webhook-handler.ts
│   ├── transformers/
│   │   ├── mapper.ts
│   │   ├── validator.ts
│   │   └── enricher.ts
│   ├── database/
│   │   ├── models/
│   │   ├── migrations/
│   │   └── connection.ts
│   └── utils/
│       ├── logger.ts
│       └── metrics.ts
├── tests/
├── docs/
└── package.json
```

## Integration Framework

### Base Connector Interface
```typescript
// src/integrations/base/connector.ts
export interface IntegrationConnector {
  name: string;
  version: string;
  authenticate(): Promise<void>;
  testConnection(): Promise<boolean>;
  getSchema(): Promise<Schema>;
  getData(query: DataQuery): Promise<DataResult>;
  pushData(data: DataPayload): Promise<PushResult>;
  subscribe(events: string[]): Promise<Subscription>;
  unsubscribe(subscriptionId: string): Promise<void>;
}

export abstract class BaseConnector implements IntegrationConnector {
  protected config: ConnectorConfig;
  protected rateLimiter: RateLimiter;
  protected retryPolicy: RetryPolicy;

  constructor(config: ConnectorConfig) {
    this.config = config;
    this.rateLimiter = new RateLimiter(config.rateLimit);
    this.retryPolicy = new RetryPolicy(config.retry);
  }

  abstract authenticate(): Promise<void>;
  abstract testConnection(): Promise<boolean>;
  abstract getSchema(): Promise<Schema>;

  protected async executeWithRetry<T>(
    operation: () => Promise<T>
  ): Promise<T> {
    return this.retryPolicy.execute(operation);
  }

  protected async checkRateLimit(): Promise<void> {
    await this.rateLimiter.checkLimit();
  }
}
```

### Salesforce Integration Example
```typescript
// src/integrations/crm/salesforce.ts
import jsforce from 'jsforce';
import { BaseConnector } from '../base/connector';

export class SalesforceConnector extends BaseConnector {
  private connection: jsforce.Connection;

  async authenticate(): Promise<void> {
    this.connection = new jsforce.Connection({
      oauth2: {
        clientId: this.config.clientId,
        clientSecret: this.config.clientSecret,
        redirectUri: this.config.redirectUri
      }
    });

    if (this.config.refreshToken) {
      await this.connection.oauth2.refreshToken(this.config.refreshToken);
    } else {
      await this.connection.login(
        this.config.username,
        this.config.password + this.config.securityToken
      );
    }
  }

  async getSchema(): Promise<Schema> {
    const metadata = await this.connection.describeGlobal();
    
    return {
      objects: metadata.sobjects.map(obj => ({
        name: obj.name,
        label: obj.label,
        fields: []  // Fetch fields on demand
      }))
    };
  }

  async getData(query: DataQuery): Promise<DataResult> {
    await this.checkRateLimit();

    const soql = this.buildSOQL(query);
    const result = await this.executeWithRetry(() => 
      this.connection.query(soql)
    );

    return {
      records: result.records,
      totalSize: result.totalSize,
      nextPageUrl: result.nextRecordsUrl
    };
  }

  async pushData(data: DataPayload): Promise<PushResult> {
    await this.checkRateLimit();

    const operation = data.operation || 'upsert';
    const result = await this.executeWithRetry(() => {
      switch (operation) {
        case 'create':
          return this.connection.sobject(data.object).create(data.records);
        case 'update':
          return this.connection.sobject(data.object).update(data.records);
        case 'upsert':
          return this.connection.sobject(data.object).upsert(
            data.records,
            data.externalIdField || 'Id'
          );
        case 'delete':
          return this.connection.sobject(data.object).delete(
            data.records.map(r => r.Id)
          );
      }
    });

    return {
      success: Array.isArray(result) 
        ? result.every(r => r.success) 
        : result.success,
      errors: this.extractErrors(result)
    };
  }

  async subscribe(events: string[]): Promise<Subscription> {
    // Implement Salesforce Streaming API subscription
    const channel = `/topic/${events[0]}`;
    const subscription = this.connection.streaming.topic(channel)
      .subscribe((message) => {
        this.emit('data', message);
      });

    return {
      id: channel,
      unsubscribe: () => subscription.cancel()
    };
  }
}
```

## Sync Engine

### Real-time Synchronization
```typescript
// src/sync-engine/realtime.ts
import { EventEmitter } from 'events';
import { IntegrationConnector } from '../integrations/base/connector';

export class RealtimeSyncEngine extends EventEmitter {
  private connectors: Map<string, IntegrationConnector>;
  private subscriptions: Map<string, Subscription>;
  private transformers: Map<string, DataTransformer>;

  constructor() {
    super();
    this.connectors = new Map();
    this.subscriptions = new Map();
    this.transformers = new Map();
  }

  async startSync(config: SyncConfig): Promise<void> {
    const source = this.connectors.get(config.sourceId);
    const target = this.connectors.get(config.targetId);
    const transformer = this.transformers.get(config.transformerId);

    if (!source || !target) {
      throw new Error('Source or target connector not found');
    }

    // Subscribe to source events
    const subscription = await source.subscribe(config.events);
    this.subscriptions.set(config.id, subscription);

    // Handle incoming data
    source.on('data', async (data) => {
      try {
        // Transform data
        const transformed = transformer 
          ? await transformer.transform(data)
          : data;

        // Apply conflict resolution
        const resolved = await this.resolveConflicts(
          transformed,
          config.conflictResolution
        );

        // Push to target
        const result = await target.pushData({
          object: config.targetObject,
          records: [resolved],
          operation: config.operation
        });

        this.emit('synced', {
          configId: config.id,
          sourceData: data,
          targetData: resolved,
          result
        });

      } catch (error) {
        this.emit('error', {
          configId: config.id,
          error,
          data
        });
      }
    });
  }

  private async resolveConflicts(
    data: any,
    strategy: ConflictResolutionStrategy
  ): Promise<any> {
    switch (strategy) {
      case 'source_wins':
        return data;
      
      case 'target_wins':
        // Fetch current target data and merge
        return this.mergeWithTarget(data);
      
      case 'merge':
        // Custom merge logic
        return this.customMerge(data);
      
      case 'manual':
        // Queue for manual review
        await this.queueForReview(data);
        throw new Error('Manual conflict resolution required');
    }
  }
}
```

### Batch Synchronization
```typescript
// src/sync-engine/batch.ts
import { CronJob } from 'cron';
import { Queue } from 'bullmq';

export class BatchSyncEngine {
  private jobs: Map<string, CronJob>;
  private queue: Queue;
  private stateManager: SyncStateManager;

  constructor(queueConfig: QueueConfig) {
    this.jobs = new Map();
    this.queue = new Queue('batch-sync', queueConfig);
    this.stateManager = new SyncStateManager();
  }

  scheduleSync(config: BatchSyncConfig): void {
    const job = new CronJob(config.schedule, async () => {
      await this.runBatchSync(config);
    });

    job.start();
    this.jobs.set(config.id, job);
  }

  private async runBatchSync(config: BatchSyncConfig): Promise<void> {
    const state = await this.stateManager.getState(config.id);
    const source = this.getConnector(config.sourceId);
    const target = this.getConnector(config.targetId);

    try {
      // Fetch data from source
      const query: DataQuery = {
        object: config.sourceObject,
        fields: config.fields,
        filters: this.buildFilters(config, state),
        limit: config.batchSize || 1000
      };

      let hasMore = true;
      let processed = 0;

      while (hasMore) {
        const result = await source.getData(query);
        
        if (result.records.length === 0) {
          hasMore = false;
          break;
        }

        // Queue records for processing
        const jobs = result.records.map(record => ({
          name: 'process-record',
          data: {
            configId: config.id,
            record,
            targetConfig: config.target
          }
        }));

        await this.queue.addBulk(jobs);
        processed += result.records.length;

        // Update state
        await this.stateManager.updateState(config.id, {
          lastSyncTime: new Date(),
          recordsProcessed: processed,
          lastRecordId: result.records[result.records.length - 1].Id
        });

        // Check for more records
        hasMore = result.nextPageUrl !== null;
        if (hasMore) {
          query.offset = (query.offset || 0) + result.records.length;
        }
      }

    } catch (error) {
      await this.stateManager.markError(config.id, error);
      throw error;
    }
  }
}
```

## n8n Integration

### Workflow Manager
```typescript
// src/n8n/workflow-manager.ts
import axios from 'axios';
import { N8nWorkflow, WorkflowExecution } from './types';

export class N8nWorkflowManager {
  private baseUrl: string;
  private apiKey: string;

  constructor(baseUrl: string, apiKey: string) {
    this.baseUrl = baseUrl;
    this.apiKey = apiKey;
  }

  async createWorkflow(workflow: N8nWorkflow): Promise<string> {
    const response = await axios.post(
      `${this.baseUrl}/api/v1/workflows`,
      workflow,
      {
        headers: {
          'X-N8N-API-KEY': this.apiKey
        }
      }
    );

    return response.data.id;
  }

  async executeWorkflow(
    workflowId: string,
    data: any
  ): Promise<WorkflowExecution> {
    const response = await axios.post(
      `${this.baseUrl}/api/v1/workflows/${workflowId}/execute`,
      { data },
      {
        headers: {
          'X-N8N-API-KEY': this.apiKey
        }
      }
    );

    return response.data;
  }

  async getExecutionStatus(executionId: string): Promise<ExecutionStatus> {
    const response = await axios.get(
      `${this.baseUrl}/api/v1/executions/${executionId}`,
      {
        headers: {
          'X-N8N-API-KEY': this.apiKey
        }
      }
    );

    return response.data;
  }

  async registerWebhook(
    workflowId: string,
    path: string
  ): Promise<WebhookRegistration> {
    // Register webhook endpoint for n8n workflow
    return {
      url: `${this.baseUrl}/webhook/${path}`,
      workflowId,
      path
    };
  }
}
```

## Data Transformation

### Mapping Engine
```typescript
// src/transformers/mapper.ts
export class DataMapper {
  private mappings: Map<string, FieldMapping[]>;

  constructor() {
    this.mappings = new Map();
  }

  registerMapping(
    mappingId: string,
    fieldMappings: FieldMapping[]
  ): void {
    this.mappings.set(mappingId, fieldMappings);
  }

  async transform(
    data: any,
    mappingId: string
  ): Promise<any> {
    const fieldMappings = this.mappings.get(mappingId);
    if (!fieldMappings) {
      throw new Error(`Mapping ${mappingId} not found`);
    }

    const transformed: any = {};

    for (const mapping of fieldMappings) {
      const sourceValue = this.getNestedValue(data, mapping.source);
      const transformedValue = await this.applyTransformation(
        sourceValue,
        mapping.transformation
      );

      this.setNestedValue(
        transformed,
        mapping.target,
        transformedValue
      );
    }

    return transformed;
  }

  private async applyTransformation(
    value: any,
    transformation?: Transformation
  ): Promise<any> {
    if (!transformation) return value;

    switch (transformation.type) {
      case 'format':
        return this.formatValue(value, transformation.params);
      
      case 'lookup':
        return await this.lookupValue(value, transformation.params);
      
      case 'calculate':
        return this.calculateValue(value, transformation.params);
      
      case 'custom':
        return await this.customTransform(value, transformation.params);
      
      default:
        return value;
    }
  }
}

interface FieldMapping {
  source: string;
  target: string;
  transformation?: Transformation;
  required?: boolean;
  defaultValue?: any;
}

interface Transformation {
  type: 'format' | 'lookup' | 'calculate' | 'custom';
  params: any;
}
```

## API Endpoints

### Sync Management
```typescript
// POST /api/sync/start
// POST /api/sync/stop
// GET /api/sync/status/:syncId

interface SyncRequest {
  sourceId: string;
  targetId: string;
  mode: 'realtime' | 'batch';
  config: {
    sourceObject: string;
    targetObject: string;
    fieldMappings: FieldMapping[];
    schedule?: string; // Cron expression for batch
    events?: string[]; // Events for realtime
    conflictResolution?: ConflictResolutionStrategy;
  };
}

interface SyncStatus {
  id: string;
  status: 'running' | 'stopped' | 'error';
  mode: 'realtime' | 'batch';
  statistics: {
    recordsSynced: number;
    errors: number;
    lastSyncTime: Date;
    averageProcessingTime: number;
  };
  errors?: SyncError[];
}
```

### Integration Management
```typescript
// GET /api/integrations
// POST /api/integrations
// PUT /api/integrations/:id
// DELETE /api/integrations/:id
// POST /api/integrations/:id/test

interface Integration {
  id: string;
  name: string;
  type: 'salesforce' | 'hubspot' | 'sap' | 'custom';
  config: {
    authentication: any;
    rateLimit?: RateLimitConfig;
    retry?: RetryConfig;
  };
  status: 'active' | 'inactive' | 'error';
  lastTestTime?: Date;
  lastTestResult?: boolean;
}
```

### Workflow Management
```typescript
// GET /api/workflows
// POST /api/workflows
// POST /api/workflows/:id/execute
// GET /api/workflows/:id/executions

interface WorkflowRequest {
  name: string;
  description?: string;
  trigger: {
    type: 'webhook' | 'schedule' | 'manual';
    config: any;
  };
  nodes: WorkflowNode[];
  connections: WorkflowConnection[];
}

interface WorkflowExecution {
  id: string;
  workflowId: string;
  status: 'running' | 'success' | 'error';
  startTime: Date;
  endTime?: Date;
  input: any;
  output?: any;
  error?: any;
}
```

## Environment Variables

```bash
# Service Configuration
NODE_ENV=production
PORT=3005
SERVICE_NAME=data-sync-service

# Auth0 M2M
AUTH0_DOMAIN=your-tenant.auth0.com
AUTH0_CLIENT_ID=your_m2m_client_id
AUTH0_CLIENT_SECRET=your_m2m_client_secret
AUTH0_AUDIENCE=https://api.nexpo.app

# Database
DATABASE_URL=postgresql://user:password@localhost:5432/datasync

# Redis
REDIS_URL=redis://localhost:6379

# n8n Configuration
N8N_BASE_URL=http://localhost:5678
N8N_API_KEY=your_n8n_api_key

# Integration Credentials (encrypted in DB)
ENCRYPTION_KEY=your_encryption_key

# Queue Configuration
QUEUE_CONCURRENCY=10
BATCH_SIZE_DEFAULT=1000

# Rate Limiting
DEFAULT_RATE_LIMIT_PER_MINUTE=60
DEFAULT_RATE_LIMIT_PER_HOUR=1000

# Monitoring
ENABLE_METRICS=true
METRICS_PORT=9090
```

## Security Considerations

### Credential Management
- Store all integration credentials encrypted in database
- Use Auth0 M2M tokens for service authentication
- Implement credential rotation policies
- Audit all credential access

### Data Security
- Encrypt sensitive data in transit and at rest
- Implement field-level encryption for PII
- Use secure webhooks with signature verification
- Implement data retention policies

## Deployment

### Docker Configuration
```dockerfile
FROM node:18-alpine

WORKDIR /app

COPY package*.json ./
RUN pnpm ci --only=production

COPY . .
RUN pnpm run build

EXPOSE 3005
CMD ["node", "dist/index.js"]
```

### Health Checks
```typescript
// GET /health
app.get('/health', async (req, res) => {
  const checks = {
    service: 'ok',
    database: await checkDatabase(),
    redis: await checkRedis(),
    n8n: await checkN8n(),
    integrations: await checkIntegrations()
  };

  const healthy = Object.values(checks).every(v => 
    typeof v === 'string' ? v === 'ok' : 
    Object.values(v).every(s => s === 'ok')
  );

  res.status(healthy ? 200 : 503).json(checks);
});
```

## Next Steps

1. **Set Up n8n**
   - Deploy n8n instance
   - Configure API access
   - Create base workflows

2. **Implement Core Connectors**
   - Build Salesforce connector
   - Add HubSpot integration
   - Create generic REST connector

3. **Build Sync Engine**
   - Implement real-time sync
   - Create batch processor
   - Add conflict resolution

4. **Create Transformation Layer**
   - Build mapping engine
   - Add validation rules
   - Implement enrichment

5. **Add Monitoring**
   - Set up sync metrics
   - Create error tracking
   - Build sync dashboards

6. **Deploy Service**
   - Create Docker image
   - Configure auto-scaling
   - Set up monitoring alerts