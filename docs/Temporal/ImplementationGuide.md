# Temporal.io Implementation Guide

Quick start guide for implementing Temporal.io workflows in the Nexpo template.

## Quick Start

### 1. Environment Variables

Add to your `.env.example`:

```bash
# Temporal Configuration
ENABLE_TEMPORAL=false
TEMPORAL_SERVER_URL=localhost:7233
TEMPORAL_NAMESPACE=default
TEMPORAL_TASK_QUEUE=main-task-queue

# Temporal Cloud (optional)
TEMPORAL_CLOUD_NAMESPACE=
TEMPORAL_CLOUD_API_KEY=
```

### 2. Package Setup

```bash
# Create the temporal workflows package
mkdir -p packages/temporal-workflows
cd packages/temporal-workflows

# Initialize package
pnpm init -y

# Install dependencies
pnpm install @temporalio/worker @temporalio/client @temporalio/workflow @temporalio/activity @temporalio/common nanoid
pnpm install -D typescript @types/node
```

### 3. Basic Project Structure

```
packages/temporal-workflows/
├── src/
│   ├── activities/
│   │   ├── database-activities.ts    # Multi-DB operations
│   │   ├── cloud-activities.ts       # Multi-cloud operations
│   │   └── ai-activities.ts          # Claude 4 Sonnet integration
│   ├── workflows/
│   │   ├── user-onboarding.ts        # User creation saga
│   │   ├── data-sync.ts              # Cross-DB synchronization
│   │   └── ai-content-pipeline.ts    # AI-powered workflows
│   ├── workers/
│   │   └── main-worker.ts            # Worker configuration
│   └── client/
│       └── temporal-client.ts        # Client setup
├── package.json
└── tsconfig.json
```

## Priority Implementation Order

### Phase 1: Basic Setup (Week 1)
1. ✅ Environment configuration
2. ✅ Package structure
3. ⏳ Basic worker and client setup
4. ⏳ Health check workflow

### Phase 2: Database Integration (Week 2)
1. ⏳ Database activities for each provider
2. ⏳ User onboarding saga
3. ⏳ Basic error handling and compensation

### Phase 3: AI Integration (Week 3)
1. ⏳ Claude 4 Sonnet activity wrapper
2. ⏳ Content generation workflow
3. ⏳ AI decision-making workflows

### Phase 4: Cloud Operations (Week 4)
1. ⏳ Multi-cloud activities
2. ⏳ Failover workflows
3. ⏳ Resource provisioning workflows

## Key Workflows to Implement First

### 1. User Onboarding Saga
**Purpose**: Create users across all enabled database providers with rollback capability.

**Activities**:
- `createPostgresUser`
- `createMongoUser`
- `createCosmosUser`
- `createSupabaseUser`
- `createSqlServerUser`

**Compensation**: Delete user from all databases if any step fails.

### 2. AI Content Pipeline
**Purpose**: Generate, review, and publish content using Claude 4 Sonnet.

**Activities**:
- `generateContent` (Claude 4 Sonnet)
- `reviewContent` (AI or human)
- `publishContent` (to enabled databases)
- `notifyUsers`

### 3. Cross-Database Sync
**Purpose**: Keep related data synchronized across database providers.

**Activities**:
- `detectChanges`
- `validateData`
- `syncToProviders`
- `verifySync`

## Sample Implementation

### Basic Worker Setup

```typescript
// packages/temporal-workflows/src/workers/main-worker.ts
import { Worker } from '@temporalio/worker';
import * as activities from '../activities';

async function run() {
  const worker = await Worker.create({
    workflowsPath: require.resolve('../workflows'),
    activities,
    taskQueue: process.env.TEMPORAL_TASK_QUEUE || 'main-task-queue',
  });

  await worker.run();
}

run().catch((err) => {
  console.error(err);
  process.exit(1);
});
```

### Sample Database Activity

```typescript
// packages/temporal-workflows/src/activities/database-activities.ts
import { PostgresUser, MongodbUser, CosmosUser } from '@/shared-db';

export async function createUserInAllDatabases(userData: {
  email: string;
  name: string;
  password: string;
}): Promise<{ success: boolean; userIds: Record<string, string> }> {
  const userIds: Record<string, string> = {};
  
  try {
    // Create in PostgreSQL if enabled
    if (process.env.ENABLE_POSTGRES === 'true') {
      const postgresUser = await PostgresUser.create(userData);
      userIds.postgres = postgresUser.id;
    }

    // Create in MongoDB if enabled
    if (process.env.ENABLE_MONGODB === 'true') {
      const mongoUser = await MongodbUser.create(userData);
      userIds.mongodb = mongoUser.id;
    }

    // Create in CosmosDB if enabled
    if (process.env.ENABLE_COSMOSDB === 'true') {
      const cosmosUser = await CosmosUser.create(userData);
      userIds.cosmosdb = cosmosUser.id;
    }

    return { success: true, userIds };
  } catch (error) {
    // Compensation logic would go here
    throw error;
  }
}
```

## Integration with Existing Architecture

### Feature Flag Integration
```typescript
// Check feature flags before executing workflows
if (process.env.ENABLE_TEMPORAL === 'true') {
  // Execute Temporal workflows
} else {
  // Fall back to direct database operations
}
```

### API Gateway Integration
```typescript
// Add workflow triggers to your existing API routes
app.post('/api/users', async (req, res) => {
  if (process.env.ENABLE_TEMPORAL === 'true') {
    // Trigger Temporal workflow
    const workflowId = await client.workflow.start(userOnboardingWorkflow, {
      args: [req.body],
      taskQueue: 'main-task-queue',
      workflowId: `user-onboarding-${Date.now()}`,
    });
    res.json({ workflowId });
  } else {
    // Direct database operation
    const user = await createUser(req.body);
    res.json(user);
  }
});
```

## Monitoring and Debugging

### Temporal Web UI
Access at `http://localhost:8080` when running Temporal server locally.

### Custom Metrics
```typescript
// Add custom metrics to track workflow performance
import { metrics } from '@temporalio/client';

// Track workflow completion rates
metrics.counter('workflow_completions', {
  workflow_type: 'user_onboarding',
  status: 'success'
}).inc();
```

## Next Steps

1. **Start with Phase 1**: Set up basic infrastructure
2. **Implement one workflow**: Begin with user onboarding saga
3. **Add monitoring**: Set up Temporal Web UI and basic metrics
4. **Iterate**: Add more workflows based on business needs
5. **Scale**: Add more workers and optimize performance

## Resources

- [Temporal Documentation](https://docs.temporal.io/)
- [TypeScript SDK Guide](https://docs.temporal.io/typescript)
- [Workflow Patterns](https://docs.temporal.io/encyclopedia)
- [Best Practices](https://docs.temporal.io/typescript/best-practices)
