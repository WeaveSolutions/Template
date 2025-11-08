# MongoDB Atlas Setup for BI Dashboard - Nexpo & Taurte

**Database:** MongoDB Atlas (Cloud) + MongoDB Charts  
**Alternative Visualization:** Looker Studio via ETL or Data API  
**Platforms:** Next.js, Expo, Svelte, Tauri

---

## Checklist Overview

- [ ] MongoDB Atlas project setup
- [ ] Database and collection design
- [ ] Index optimization
- [ ] Auth0 + MongoDB integration
- [ ] RBAC implementation
- [ ] Aggregation pipelines for KPIs
- [ ] MongoDB Charts setup (native BI)
- [ ] Looker Studio integration (via ETL)
- [ ] Nexpo implementation
- [ ] Taurte implementation
- [ ] Real-time change streams
- [ ] Performance optimization
- [ ] Testing and deployment

---

## 1. MongoDB Atlas Setup

### Create Atlas Account & Cluster
- [ ] Sign up at mongodb.com/cloud/atlas
- [ ] Create new project: "BI-Dashboard"
- [ ] Create cluster (M0 Free or M10+ for production)
- [ ] Choose cloud provider and region
- [ ] Configure cluster name: "bi-production"

### Security Configuration
- [ ] Create database user with strong password
- [ ] Set user role: `readWrite` on `bi_dashboard` database
- [ ] Add IP whitelist (0.0.0.0/0 for development, specific IPs for production)
- [ ] Enable network peering (optional, for VPC)
- [ ] Configure private endpoint (optional)

### Get Connection Details
- [ ] Get connection string: `mongodb+srv://username:password@cluster.mongodb.net/`
- [ ] Note database name: `bi_dashboard`
- [ ] Save credentials securely

### Environment Variables

**Nexpo (.env.local):**
```bash
MONGODB_URI=mongodb+srv://username:password@cluster.mongodb.net/bi_dashboard?retryWrites=true&w=majority
MONGODB_DB=bi_dashboard

# Auth0
AUTH0_SECRET=...
AUTH0_BASE_URL=...
AUTH0_ISSUER_BASE_URL=...
AUTH0_CLIENT_ID=...
AUTH0_CLIENT_SECRET=...
```

**Taurte (.env):**
```bash
VITE_MONGODB_DATA_API_URL=https://data.mongodb-api.com/app/YOUR_APP_ID/endpoint/data/v1
VITE_MONGODB_API_KEY=your_api_key_here

# Auth0
VITE_AUTH0_DOMAIN=...
VITE_AUTH0_CLIENT_ID=...
```

---

## 2. Database Schema Design

### Collections Structure

#### Users Collection
```javascript
{
  _id: ObjectId("..."),
  auth0_id: "auth0|123456789",
  email: "admin@example.com",
  display_name: "Admin User",
  role: "admin", // viewer | analyst | admin
  is_active: true,
  created_at: ISODate("2024-01-15T10:00:00Z"),
  last_login: ISODate("2024-01-20T15:30:00Z"),
  metadata: {
    department: "Engineering",
    timezone: "America/Los_Angeles"
  }
}
```

#### Subscriptions Collection
```javascript
{
  _id: ObjectId("..."),
  user_id: ObjectId("..."), // Reference to users._id
  plan: "pro", // free | pro | enterprise
  status: "active", // active | cancelled | past_due | trialing
  mrr: 99.00,
  arr: 1188.00, // mrr * 12
  start_date: ISODate("2024-01-01T00:00:00Z"),
  end_date: null,
  billing_cycle: "monthly", // monthly | yearly
  payment_provider: "stripe",
  payment_provider_id: "sub_1234567890",
  created_at: ISODate("2024-01-01T00:00:00Z"),
  updated_at: ISODate("2024-01-01T00:00:00Z")
}
```

#### Analytics Events Collection
```javascript
{
  _id: ObjectId("..."),
  event_type: "page_view", // page_view | feature_used | subscription_created | etc.
  user_id: ObjectId("..."),
  timestamp: ISODate("2024-01-20T16:45:00Z"),
  platform: "web", // web | mobile_ios | mobile_android | desktop
  properties: {
    page: "/dashboard",
    referrer: "/login",
    session_id: "sess_abc123",
    user_agent: "Mozilla/5.0..."
  }
}
```

#### KPI Snapshots Collection (Time Series)
```javascript
{
  _id: ObjectId("..."),
  snapshot_date: ISODate("2024-01-20T00:00:00Z"),
  metrics: {
    total_users: 15420,
    active_users: 8930,
    dau: 3245,
    mau: 8930,
    active_subscriptions: 1250,
    mrr: 98750.00,
    arr: 1185000.00,
    arpu: 11.05,
    churn_rate: 3.2,
    ltv: 450.00,
    cac: 120.00,
    nps: 42
  },
  created_at: ISODate("2024-01-20T00:05:00Z")
}
```

### Create Collections with Validation
- [ ] Go to Atlas Dashboard > Browse Collections
- [ ] Create database: `bi_dashboard`
- [ ] Create collections: `users`, `subscriptions`, `analytics_events`, `kpi_snapshots`

**Optional: Add Schema Validation**
```javascript
// In MongoDB Shell or Compass
db.createCollection("users", {
  validator: {
    $jsonSchema: {
      bsonType: "object",
      required: ["auth0_id", "email", "role"],
      properties: {
        auth0_id: { bsonType: "string" },
        email: { bsonType: "string" },
        role: { enum: ["viewer", "analyst", "admin"] },
        is_active: { bsonType: "bool" }
      }
    }
  }
});
```

---

## 3. Indexes for Performance

### Create Indexes
- [ ] Go to Atlas > Collections > Indexes
- [ ] Create the following indexes:

**Users Collection:**
```javascript
db.users.createIndex({ auth0_id: 1 }, { unique: true });
db.users.createIndex({ email: 1 });
db.users.createIndex({ role: 1 });
db.users.createIndex({ last_login: -1 });
```

**Subscriptions Collection:**
```javascript
db.subscriptions.createIndex({ user_id: 1 });
db.subscriptions.createIndex({ status: 1 });
db.subscriptions.createIndex({ plan: 1 });
db.subscriptions.createIndex({ created_at: -1 });
db.subscriptions.createIndex({ user_id: 1, status: 1 }); // Compound index
```

**Analytics Events Collection:**
```javascript
db.analytics_events.createIndex({ user_id: 1 });
db.analytics_events.createIndex({ event_type: 1 });
db.analytics_events.createIndex({ timestamp: -1 });
db.analytics_events.createIndex({ platform: 1 });
db.analytics_events.createIndex({ timestamp: -1, event_type: 1 }); // Compound
```

**KPI Snapshots Collection:**
```javascript
db.kpi_snapshots.createIndex({ snapshot_date: -1 }, { unique: true });
```

---

## 4. Auth0 + MongoDB Integration

### Create Auth0 Action for MongoDB Sync

- [ ] Go to Auth0 Dashboard > Actions > Flows > Login
- [ ] Create new Action: "Sync MongoDB User"
- [ ] Add dependencies: `mongodb@6.3`

**Action Code:**
```javascript
const { MongoClient } = require('mongodb');

exports.onExecutePostLogin = async (event, api) => {
  const uri = event.secrets.MONGODB_URI;
  const client = new MongoClient(uri);
  
  try {
    await client.connect();
    const db = client.db('bi_dashboard');
    const usersCollection = db.collection('users');
    
    // Determine user role
    let role = 'viewer';
    if (event.user.email.endsWith('@yourdomain.com')) {
      role = 'admin';
    }
    
    // Check if user exists
    const existingUser = await usersCollection.findOne({
      auth0_id: event.user.user_id
    });
    
    if (!existingUser) {
      // Create new user
      await usersCollection.insertOne({
        auth0_id: event.user.user_id,
        email: event.user.email,
        display_name: event.user.name,
        role: role,
        is_active: true,
        created_at: new Date(),
        last_login: new Date(),
        metadata: {}
      });
    } else {
      // Update last login and role
      await usersCollection.updateOne(
        { auth0_id: event.user.user_id },
        { 
          $set: { 
            last_login: new Date(),
            role: existingUser.role || role // Keep existing role if set
          } 
        }
      );
      role = existingUser.role;
    }
    
    // Add role to token
    api.idToken.setCustomClaim('role', role);
    api.accessToken.setCustomClaim('role', role);
    
  } catch (error) {
    console.error('MongoDB sync error:', error);
  } finally {
    await client.close();
  }
};
```

### Add Auth0 Secrets
- [ ] `MONGODB_URI` = your MongoDB connection string

---

## 5. Aggregation Pipelines for KPIs

### Create Server-Side Aggregation Functions

**Calculate Current Metrics:**
```javascript
// Aggregation pipeline for real-time metrics
db.users.aggregate([
  {
    $facet: {
      totalUsers: [{ $count: "count" }],
      mau: [
        {
          $match: {
            last_login: { $gte: new Date(Date.now() - 30 * 24 * 60 * 60 * 1000) }
          }
        },
        { $count: "count" }
      ],
      dau: [
        {
          $match: {
            last_login: { $gte: new Date(Date.now() - 24 * 60 * 60 * 1000) }
          }
        },
        { $count: "count" }
      ]
    }
  },
  {
    $project: {
      total_users: { $arrayElemAt: ["$totalUsers.count", 0] },
      mau: { $arrayElemAt: ["$mau.count", 0] },
      dau: { $arrayElemAt: ["$dau.count", 0] }
    }
  }
]);

// Subscription metrics
db.subscriptions.aggregate([
  {
    $match: { status: "active" }
  },
  {
    $group: {
      _id: null,
      active_subscriptions: { $sum: 1 },
      total_mrr: { $sum: "$mrr" },
      total_arr: { $sum: "$arr" }
    }
  }
]);

// Calculate ARPU
db.subscriptions.aggregate([
  {
    $match: { status: "active" }
  },
  {
    $group: {
      _id: null,
      total_mrr: { $sum: "$mrr" }
    }
  },
  {
    $lookup: {
      from: "users",
      pipeline: [
        {
          $match: {
            last_login: { $gte: new Date(Date.now() - 30 * 24 * 60 * 60 * 1000) }
          }
        },
        { $count: "active_users" }
      ],
      as: "user_counts"
    }
  },
  {
    $project: {
      arpu: {
        $divide: [
          "$total_mrr",
          { $arrayElemAt: ["$user_counts.active_users", 0] }
        ]
      }
    }
  }
]);
```

### Create MongoDB Atlas Functions (Serverless)

- [ ] Go to Atlas > App Services > Create New App
- [ ] Enable MongoDB Atlas Data API
- [ ] Create function: `getCurrentMetrics`

```javascript
exports = async function getCurrentMetrics() {
  const mongodb = context.services.get("mongodb-atlas");
  const db = mongodb.db("bi_dashboard");
  
  // Get user metrics
  const userMetrics = await db.collection("users").aggregate([
    {
      $facet: {
        total: [{ $count: "count" }],
        mau: [
          { $match: { last_login: { $gte: new Date(Date.now() - 30*24*60*60*1000) } } },
          { $count: "count" }
        ],
        dau: [
          { $match: { last_login: { $gte: new Date(Date.now() - 24*60*60*1000) } } },
          { $count: "count" }
        ]
      }
    }
  ]).toArray();
  
  // Get subscription metrics
  const subMetrics = await db.collection("subscriptions").aggregate([
    { $match: { status: "active" } },
    {
      $group: {
        _id: null,
        active_subscriptions: { $sum: 1 },
        mrr: { $sum: "$mrr" },
        arr: { $sum: "$arr" }
      }
    }
  ]).toArray();
  
  return {
    total_users: userMetrics[0].total[0]?.count || 0,
    mau: userMetrics[0].mau[0]?.count || 0,
    dau: userMetrics[0].dau[0]?.count || 0,
    active_subscriptions: subMetrics[0]?.active_subscriptions || 0,
    mrr: subMetrics[0]?.mrr || 0,
    arr: subMetrics[0]?.arr || 0
  };
};
```

---

## 6. MongoDB Charts Setup (Native BI)

### Enable MongoDB Charts
- [ ] Go to Atlas Dashboard
- [ ] Click "Charts" in left sidebar
- [ ] Activate Charts (free with Atlas)
- [ ] Create new dashboard: "Executive BI Dashboard"

### Create Data Sources
- [ ] Add data source: `bi_dashboard` database
- [ ] Select collections: `users`, `subscriptions`, `kpi_snapshots`
- [ ] Configure permissions (read-only)

### Create Charts

**MRR Trend Chart:**
- [ ] Chart Type: Line Chart
- [ ] Data Source: `kpi_snapshots`
- [ ] X-Axis: `snapshot_date`
- [ ] Y-Axis: `metrics.mrr`
- [ ] Aggregation: None (use raw values)

**Active Users Chart:**
- [ ] Chart Type: Combo Chart
- [ ] Data Source: `kpi_snapshots`
- [ ] X-Axis: `snapshot_date`
- [ ] Y-Axis 1: `metrics.dau` (Line)
- [ ] Y-Axis 2: `metrics.mau` (Line)

**Subscription Plan Distribution:**
- [ ] Chart Type: Donut Chart
- [ ] Data Source: `subscriptions`
- [ ] Label: `plan`
- [ ] Value: Count of documents
- [ ] Filter: `status = "active"`

**Top KPIs (Number Charts):**
- [ ] Create separate Number charts for:
  - Total MRR (`kpi_snapshots.metrics.mrr` - latest)
  - Total ARR (`kpi_snapshots.metrics.arr` - latest)
  - Active Subscriptions (`kpi_snapshots.metrics.active_subscriptions`)
  - Churn Rate (`kpi_snapshots.metrics.churn_rate`)

### Embed MongoDB Charts

**Get Embed Code:**
- [ ] Enable embedding on dashboard
- [ ] Set authentication: Verified Signature (for RBAC)
- [ ] Copy embed SDK snippet

**Embed in App:**
```html
<!-- MongoDB Charts Embed -->
<iframe
  src="https://charts.mongodb.com/charts-project-id/embed/dashboards?id=dashboard-id&theme=light"
  width="100%"
  height="800px"
  style="border: 0;"
></iframe>
```

---

## 7. Looker Studio Integration (Alternative)

### Option 1: ETL to BigQuery (Recommended)

**Using MongoDB Atlas Data Federation + BigQuery:**
- [ ] Set up Atlas Data Federation
- [ ] Create federated database instance
- [ ] Configure SQL queries on MongoDB data
- [ ] Export to BigQuery using scheduled queries
- [ ] Connect Looker Studio to BigQuery

**ETL Pipeline with Fivetran/Airbyte:**
- [ ] Sign up for Fivetran or Airbyte
- [ ] Create MongoDB source connector
- [ ] Create BigQuery destination
- [ ] Configure sync schedule (hourly/daily)
- [ ] Map MongoDB collections to BigQuery tables
- [ ] Connect Looker Studio to BigQuery

### Option 2: MongoDB Data API + Google Apps Script

**Enable MongoDB Data API:**
- [ ] Go to Atlas > App Services
- [ ] Enable Data API
- [ ] Create API key
- [ ] Configure IP access list

**Create Looker Studio Community Connector:**
- [ ] Use Google Apps Script
- [ ] Call MongoDB Data API endpoints
- [ ] Transform data for Looker Studio
- [ ] Deploy as community connector

**Example Apps Script:**
```javascript
function getData(request) {
  const dataApiUrl = 'https://data.mongodb-api.com/app/YOUR_APP_ID/endpoint/data/v1';
  const apiKey = 'YOUR_API_KEY';
  
  const response = UrlFetchApp.fetch(`${dataApiUrl}/action/find`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'api-key': apiKey
    },
    payload: JSON.stringify({
      dataSource: 'Cluster0',
      database: 'bi_dashboard',
      collection: 'kpi_snapshots',
      filter: {},
      sort: { snapshot_date: -1 },
      limit: 90
    })
  });
  
  const data = JSON.parse(response.getContentText());
  // Transform and return data for Looker Studio
}
```

### Option 3: MongoDB Connector for BI (SQL Interface)

- [ ] Download MongoDB Connector for BI
- [ ] Install and configure
- [ ] Create DSN (Data Source Name)
- [ ] Use ODBC/JDBC to connect Looker Studio
- [ ] Query MongoDB using SQL syntax

---

## 8. Nexpo Implementation

### Next.js Setup

**Install MongoDB Driver:**
```bash
npm install mongodb
```

**Create MongoDB Client (`lib/mongodb.ts`):**
```typescript
import { MongoClient } from 'mongodb';

const uri = process.env.MONGODB_URI!;
const options = {};

let client: MongoClient;
let clientPromise: Promise<MongoClient>;

if (process.env.NODE_ENV === 'development') {
  let globalWithMongo = global as typeof globalThis & {
    _mongoClientPromise?: Promise<MongoClient>
  };
  
  if (!globalWithMongo._mongoClientPromise) {
    client = new MongoClient(uri, options);
    globalWithMongo._mongoClientPromise = client.connect();
  }
  clientPromise = globalWithMongo._mongoClientPromise;
} else {
  client = new MongoClient(uri, options);
  clientPromise = client.connect();
}

export default clientPromise;
```

**API Route (`app/api/bi/metrics/route.ts`):**
```typescript
import { NextResponse } from 'next/server';
import { getSession } from '@auth0/nextjs-auth0';
import clientPromise from '@/lib/mongodb';

export async function GET() {
  const session = await getSession();
  
  if (!session || session.user.role !== 'admin') {
    return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
  }

  try {
    const client = await clientPromise;
    const db = client.db('bi_dashboard');
    
    // Get latest KPI snapshot
    const latestSnapshot = await db.collection('kpi_snapshots')
      .findOne({}, { sort: { snapshot_date: -1 } });
    
    // Get active subscriptions count
    const activeSubsCount = await db.collection('subscriptions')
      .countDocuments({ status: 'active' });
    
    // Calculate total MRR
    const mrrResult = await db.collection('subscriptions')
      .aggregate([
        { $match: { status: 'active' } },
        { $group: { _id: null, total_mrr: { $sum: '$mrr' } } }
      ])
      .toArray();
    
    return NextResponse.json({
      snapshot: latestSnapshot?.metrics || {},
      active_subscriptions: activeSubsCount,
      total_mrr: mrrResult[0]?.total_mrr || 0
    });
  } catch (error) {
    console.error('MongoDB query error:', error);
    return NextResponse.json({ error: 'Database error' }, { status: 500 });
  }
}
```

### Expo Mobile Setup

**Using MongoDB Data API (Recommended for Mobile):**
```bash
npm install axios
```

**API Client (`utils/mongodbApi.ts`):**
```typescript
import axios from 'axios';

const DATA_API_URL = process.env.EXPO_PUBLIC_MONGODB_DATA_API_URL;
const API_KEY = process.env.EXPO_PUBLIC_MONGODB_API_KEY;

export async function fetchMetrics() {
  const response = await axios.post(
    `${DATA_API_URL}/action/findOne`,
    {
      dataSource: 'Cluster0',
      database: 'bi_dashboard',
      collection: 'kpi_snapshots',
      sort: { snapshot_date: -1 }
    },
    {
      headers: {
        'Content-Type': 'application/json',
        'api-key': API_KEY
      }
    }
  );
  
  return response.data.document;
}
```

---

## 9. Taurte Implementation

### Svelte Web Setup

**Using MongoDB Data API:**
```bash
npm install axios
```

**Svelte Store (`src/stores/biMetrics.ts`):**
```typescript
import { writable } from 'svelte/store';
import axios from 'axios';

export const metrics = writable(null);
export const loading = writable(false);

const DATA_API_URL = import.meta.env.VITE_MONGODB_DATA_API_URL;
const API_KEY = import.meta.env.VITE_MONGODB_API_KEY;

export async function fetchMetrics() {
  loading.set(true);
  
  try {
    const response = await axios.post(
      `${DATA_API_URL}/action/findOne`,
      {
        dataSource: 'Cluster0',
        database: 'bi_dashboard',
        collection: 'kpi_snapshots',
        sort: { snapshot_date: -1 }
      },
      {
        headers: {
          'Content-Type': 'application/json',
          'api-key': API_KEY
        }
      }
    );
    
    metrics.set(response.data.document);
  } catch (error) {
    console.error('Failed to fetch metrics:', error);
  } finally {
    loading.set(false);
  }
}
```

### Tauri Desktop/Mobile (Rust Backend)

**Add Dependencies to Cargo.toml:**
```toml
[dependencies]
mongodb = "2.8"
serde = { version = "1.0", features = ["derive"] }
tokio = { version = "1", features = ["full"] }
```

**Tauri Command (`src-tauri/src/main.rs`):**
```rust
use mongodb::{Client, options::ClientOptions};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct KpiMetrics {
    total_users: i32,
    mau: i32,
    dau: i32,
    mrr: f64,
    arr: f64,
}

#[tauri::command]
async fn fetch_mongodb_metrics(mongo_uri: String) -> Result<KpiMetrics, String> {
    let client_options = ClientOptions::parse(&mongo_uri)
        .await
        .map_err(|e| e.to_string())?;
    
    let client = Client::with_options(client_options)
        .map_err(|e| e.to_string())?;
    
    let db = client.database("bi_dashboard");
    let collection = db.collection::<bson::Document>("kpi_snapshots");
    
    let options = mongodb::options::FindOneOptions::builder()
        .sort(bson::doc! { "snapshot_date": -1 })
        .build();
    
    let result = collection.find_one(None, options)
        .await
        .map_err(|e| e.to_string())?;
    
    // Parse and return metrics
    // Implementation details...
    
    Ok(KpiMetrics {
        total_users: 0,
        mau: 0,
        dau: 0,
        mrr: 0.0,
        arr: 0.0,
    })
}
```

---

## 10. Real-Time Updates (Change Streams)

### Enable Change Streams
- [ ] Requires MongoDB Atlas M10+ tier
- [ ] Go to Atlas > Database Access > Enable Change Streams

**Next.js Server-Side Events:**
```typescript
// app/api/bi/stream/route.ts
import { NextRequest } from 'next/server';
import clientPromise from '@/lib/mongodb';

export async function GET(request: NextRequest) {
  const client = await clientPromise;
  const db = client.db('bi_dashboard');
  const collection = db.collection('kpi_snapshots');
  
  const changeStream = collection.watch([
    { $match: { operationType: 'insert' } }
  ]);
  
  const encoder = new TextEncoder();
  const stream = new ReadableStream({
    async start(controller) {
      for await (const change of changeStream) {
        const data = `data: ${JSON.stringify(change.fullDocument)}\n\n`;
        controller.enqueue(encoder.encode(data));
      }
    }
  });
  
  return new Response(stream, {
    headers: {
      'Content-Type': 'text/event-stream',
      'Cache-Control': 'no-cache',
      'Connection': 'keep-alive'
    }
  });
}
```

---

## 11. Performance Optimization

### Indexing Strategy
- [ ] Create compound indexes for common queries
- [ ] Use covered queries (query only indexed fields)
- [ ] Monitor slow queries in Atlas Performance Advisor

### Aggregation Optimization
- [ ] Use `$match` early in pipeline
- [ ] Limit documents with `$limit`
- [ ] Use `$project` to reduce data transfer
- [ ] Create aggregation pipeline indexes

### Caching Layer
- [ ] Use Redis for frequently accessed metrics
- [ ] Cache aggregation results (TTL: 5-15 minutes)
- [ ] Invalidate cache on data updates

### Atlas Features
- [ ] Enable Performance Advisor
- [ ] Set up cluster auto-scaling
- [ ] Configure read preference (secondary reads for analytics)
- [ ] Use Atlas Search for full-text search

---

## 12. Testing & Deployment

### Testing Checklist
- [ ] Test Auth0 user sync
- [ ] Verify role-based access in MongoDB
- [ ] Test aggregation pipelines
- [ ] Validate KPI calculations
- [ ] Test change streams (if enabled)
- [ ] Test MongoDB Charts embedding
- [ ] Load test aggregations
- [ ] Test failover scenarios

### Production Checklist
- [ ] Upgrade to M10+ cluster (production tier)
- [ ] Enable automatic backups (continuous or snapshots)
- [ ] Configure alerts (CPU, memory, disk, connections)
- [ ] Set up monitoring with Atlas Charts or external tools
- [ ] Enable database auditing
- [ ] Configure VPC peering (if needed)
- [ ] Set appropriate connection pool size
- [ ] Enable encryption at rest
- [ ] Document connection strings and credentials

---

## Cost Considerations

### MongoDB Atlas Pricing
- **M0 (Free)**: 512MB storage, shared CPU, good for development
- **M10 (Production)**: $0.08/hr (~$57/month), 10GB storage, dedicated CPU
- **M30 (Scale)**: $0.54/hr (~$389/month), 40GB storage, better performance
- **Charts**: Free with Atlas
- **Data API**: Free tier available

### Cost Optimization
- [ ] Use time-based auto-scaling
- [ ] Archive old analytics events to cheaper storage
- [ ] Use aggregation pipelines instead of fetching all docs
- [ ] Implement TTL indexes for auto-deletion
- [ ] Monitor data transfer costs

---

## Success Criteria

- [ ] MongoDB Atlas cluster running
- [ ] All collections created with proper indexes
- [ ] Auth0 syncs users to MongoDB
- [ ] Aggregation pipelines calculate KPIs correctly
- [ ] MongoDB Charts dashboard functional
- [ ] OR Looker Studio connected via ETL
- [ ] Real-time updates work (if using change streams)
- [ ] All Nexpo platforms can query MongoDB
- [ ] All Taurte platforms can query MongoDB
- [ ] Query response time <500ms for dashboards
- [ ] 99.9% uptime (Atlas SLA)
