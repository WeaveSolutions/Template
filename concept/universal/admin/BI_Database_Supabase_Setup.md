# Supabase Database Setup for BI Dashboard - Nexpo & Taurte

**Database:** Supabase (PostgreSQL + Auth)  
**Visualization:** Looker Studio  
**Platforms:** Next.js, Expo, Svelte, Tauri

---

## Checklist Overview

- [ ] Supabase project setup
- [ ] PostgreSQL schema design
- [ ] Row-Level Security (RLS) policies
- [ ] Auth0 + Supabase integration
- [ ] Looker Studio connection
- [ ] Nexpo implementation
- [ ] Taurte implementation
- [ ] Real-time subscriptions
- [ ] Performance optimization
- [ ] Testing and deployment

---

## 1. Supabase Project Setup

### Initial Configuration
- [ ] Create Supabase project at supabase.com
- [ ] Choose region (closest to users)
- [ ] Generate database password (save securely)
- [ ] Note project URL: `https://xxxxx.supabase.co`
- [ ] Note anon/public key
- [ ] Note service_role key (keep secret!)
- [ ] Upgrade to Pro plan ($25/month) for production

### Environment Variables

**Nexpo (.env.local):**
```bash
NEXT_PUBLIC_SUPABASE_URL=https://xxxxx.supabase.co
NEXT_PUBLIC_SUPABASE_ANON_KEY=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
SUPABASE_SERVICE_ROLE_KEY=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9... # Server-only!

# Auth0
AUTH0_SECRET=...
AUTH0_BASE_URL=...
AUTH0_ISSUER_BASE_URL=...
AUTH0_CLIENT_ID=...
AUTH0_CLIENT_SECRET=...
```

**Taurte (.env):**
```bash
VITE_SUPABASE_URL=https://xxxxx.supabase.co
VITE_SUPABASE_ANON_KEY=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...

# Auth0
VITE_AUTH0_DOMAIN=...
VITE_AUTH0_CLIENT_ID=...
```

---

## 2. PostgreSQL Database Schema

### SQL Migration Script

#### Create Tables
- [ ] Go to Supabase Dashboard > SQL Editor
- [ ] Create new query
- [ ] Run the following migration:

```sql
-- Enable UUID extension
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Users table
CREATE TABLE public.users (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  auth0_id TEXT UNIQUE NOT NULL,
  email TEXT NOT NULL,
  display_name TEXT,
  role TEXT NOT NULL DEFAULT 'viewer' CHECK (role IN ('viewer', 'analyst', 'admin')),
  is_active BOOLEAN DEFAULT true,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  last_login TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  metadata JSONB DEFAULT '{}'::jsonb
);

-- Subscriptions table
CREATE TABLE public.subscriptions (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id UUID REFERENCES public.users(id) ON DELETE CASCADE,
  plan TEXT NOT NULL CHECK (plan IN ('free', 'pro', 'enterprise')),
  status TEXT NOT NULL CHECK (status IN ('active', 'cancelled', 'past_due', 'trialing')),
  mrr DECIMAL(10, 2) NOT NULL DEFAULT 0,
  arr DECIMAL(10, 2) GENERATED ALWAYS AS (mrr * 12) STORED,
  start_date TIMESTAMP WITH TIME ZONE NOT NULL,
  end_date TIMESTAMP WITH TIME ZONE,
  billing_cycle TEXT CHECK (billing_cycle IN ('monthly', 'yearly')),
  payment_provider TEXT,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Analytics events table
CREATE TABLE public.analytics_events (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  event_type TEXT NOT NULL,
  user_id UUID REFERENCES public.users(id) ON DELETE SET NULL,
  timestamp TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  platform TEXT CHECK (platform IN ('web', 'mobile_ios', 'mobile_android', 'desktop')),
  properties JSONB DEFAULT '{}'::jsonb
);

-- KPI snapshots table (daily aggregations)
CREATE TABLE public.kpi_snapshots (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  snapshot_date DATE UNIQUE NOT NULL DEFAULT CURRENT_DATE,
  total_users INTEGER NOT NULL DEFAULT 0,
  active_users INTEGER NOT NULL DEFAULT 0,
  dau INTEGER NOT NULL DEFAULT 0,
  mau INTEGER NOT NULL DEFAULT 0,
  active_subscriptions INTEGER NOT NULL DEFAULT 0,
  mrr DECIMAL(12, 2) NOT NULL DEFAULT 0,
  arr DECIMAL(12, 2) NOT NULL DEFAULT 0,
  arpu DECIMAL(10, 2) NOT NULL DEFAULT 0,
  churn_rate DECIMAL(5, 2) NOT NULL DEFAULT 0,
  ltv DECIMAL(10, 2),
  cac DECIMAL(10, 2),
  nps INTEGER,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Create indexes for performance
CREATE INDEX idx_users_auth0_id ON public.users(auth0_id);
CREATE INDEX idx_users_role ON public.users(role);
CREATE INDEX idx_subscriptions_user_id ON public.subscriptions(user_id);
CREATE INDEX idx_subscriptions_status ON public.subscriptions(status);
CREATE INDEX idx_analytics_events_user_id ON public.analytics_events(user_id);
CREATE INDEX idx_analytics_events_timestamp ON public.analytics_events(timestamp);
CREATE INDEX idx_analytics_events_event_type ON public.analytics_events(event_type);
CREATE INDEX idx_kpi_snapshots_date ON public.kpi_snapshots(snapshot_date);

-- Enable Row Level Security
ALTER TABLE public.users ENABLE ROW LEVEL SECURITY;
ALTER TABLE public.subscriptions ENABLE ROW LEVEL SECURITY;
ALTER TABLE public.analytics_events ENABLE ROW LEVEL SECURITY;
ALTER TABLE public.kpi_snapshots ENABLE ROW LEVEL SECURITY;
```

#### Create Updated Timestamp Trigger
```sql
-- Function to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Apply trigger to subscriptions table
CREATE TRIGGER update_subscriptions_updated_at
    BEFORE UPDATE ON public.subscriptions
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();
```

---

## 3. Row-Level Security (RLS) Policies

### Implement RBAC at Database Level

```sql
-- Helper function to get user role from JWT
CREATE OR REPLACE FUNCTION auth.user_role() 
RETURNS TEXT AS $$
  SELECT COALESCE(
    auth.jwt() -> 'user_metadata' ->> 'role',
    auth.jwt() -> 'app_metadata' ->> 'role',
    'viewer'
  )::TEXT;
$$ LANGUAGE SQL STABLE;

-- Users table policies
CREATE POLICY "Users can view own profile"
  ON public.users FOR SELECT
  USING (auth.uid()::text = auth0_id OR auth.user_role() = 'admin');

CREATE POLICY "Admins can manage all users"
  ON public.users FOR ALL
  USING (auth.user_role() = 'admin');

-- Subscriptions table policies
CREATE POLICY "Users can view own subscriptions"
  ON public.subscriptions FOR SELECT
  USING (
    user_id IN (SELECT id FROM public.users WHERE auth0_id = auth.uid()::text)
    OR auth.user_role() IN ('admin', 'analyst')
  );

CREATE POLICY "Admins can manage subscriptions"
  ON public.subscriptions FOR ALL
  USING (auth.user_role() = 'admin');

-- Analytics events policies
CREATE POLICY "Users can create own events"
  ON public.analytics_events FOR INSERT
  WITH CHECK (
    user_id IN (SELECT id FROM public.users WHERE auth0_id = auth.uid()::text)
  );

CREATE POLICY "Admins and analysts can view events"
  ON public.analytics_events FOR SELECT
  USING (auth.user_role() IN ('admin', 'analyst'));

-- KPI snapshots policies
CREATE POLICY "Admins and analysts can view KPIs"
  ON public.kpi_snapshots FOR SELECT
  USING (auth.user_role() IN ('admin', 'analyst'));

CREATE POLICY "Admins can manage KPIs"
  ON public.kpi_snapshots FOR ALL
  USING (auth.user_role() = 'admin');
```

---

## 4. Auth0 + Supabase Integration

### Auth0 Configuration

#### Create Auth0 Action for Supabase
- [ ] Go to Auth0 Dashboard > Actions > Flows > Login
- [ ] Create new Action: "Sync Supabase User"

```javascript
const { createClient } = require('@supabase/supabase-js');

exports.onExecutePostLogin = async (event, api) => {
  const supabase = createClient(
    event.secrets.SUPABASE_URL,
    event.secrets.SUPABASE_SERVICE_KEY
  );

  try {
    // Check if user exists in Supabase
    const { data: existingUser } = await supabase
      .from('users')
      .select('*')
      .eq('auth0_id', event.user.user_id)
      .single();

    // Determine role
    let role = 'viewer';
    if (event.user.email.endsWith('@yourdomain.com')) {
      role = 'admin';
    }

    if (!existingUser) {
      // Create new user
      await supabase.from('users').insert({
        auth0_id: event.user.user_id,
        email: event.user.email,
        display_name: event.user.name,
        role: role,
        last_login: new Date().toISOString()
      });
    } else {
      // Update last login
      await supabase
        .from('users')
        .update({ last_login: new Date().toISOString() })
        .eq('auth0_id', event.user.user_id);
      
      role = existingUser.role;
    }

    // Add role to token
    api.idToken.setCustomClaim('role', role);
    api.accessToken.setCustomClaim('role', role);

  } catch (error) {
    console.error('Supabase sync error:', error);
  }
};
```

#### Add Auth0 Secrets
- [ ] `SUPABASE_URL` = your Supabase project URL
- [ ] `SUPABASE_SERVICE_KEY` = your service_role key

#### Install Action Dependencies
- [ ] Add `@supabase/supabase-js` to Action dependencies

---

## 5. Database Functions & Views

### Create Materialized Views for Performance

```sql
-- Materialized view for current metrics
CREATE MATERIALIZED VIEW public.current_metrics AS
SELECT
  COUNT(DISTINCT u.id) as total_users,
  COUNT(DISTINCT CASE WHEN u.last_login >= NOW() - INTERVAL '30 days' THEN u.id END) as mau,
  COUNT(DISTINCT CASE WHEN u.last_login >= NOW() - INTERVAL '1 day' THEN u.id END) as dau,
  COUNT(DISTINCT CASE WHEN s.status = 'active' THEN s.id END) as active_subscriptions,
  COALESCE(SUM(CASE WHEN s.status = 'active' THEN s.mrr END), 0) as total_mrr,
  COALESCE(SUM(CASE WHEN s.status = 'active' THEN s.arr END), 0) as total_arr
FROM public.users u
LEFT JOIN public.subscriptions s ON u.id = s.user_id;

-- Create index on materialized view
CREATE UNIQUE INDEX ON public.current_metrics ((true));

-- Refresh function (call this hourly or daily)
CREATE OR REPLACE FUNCTION refresh_current_metrics()
RETURNS void AS $$
BEGIN
  REFRESH MATERIALIZED VIEW CONCURRENTLY public.current_metrics;
END;
$$ LANGUAGE plpgsql;
```

### Create Database Functions for KPI Calculations

```sql
-- Function to calculate ARPU
CREATE OR REPLACE FUNCTION calculate_arpu()
RETURNS DECIMAL AS $$
DECLARE
  total_mrr DECIMAL;
  active_users_count INTEGER;
BEGIN
  SELECT total_mrr, mau INTO total_mrr, active_users_count
  FROM public.current_metrics;
  
  IF active_users_count > 0 THEN
    RETURN total_mrr / active_users_count;
  ELSE
    RETURN 0;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Function to calculate churn rate
CREATE OR REPLACE FUNCTION calculate_churn_rate(period_days INTEGER DEFAULT 30)
RETURNS DECIMAL AS $$
DECLARE
  churned_count INTEGER;
  starting_count INTEGER;
BEGIN
  -- Count subscriptions that were active at start of period but cancelled
  SELECT COUNT(*) INTO churned_count
  FROM public.subscriptions
  WHERE status = 'cancelled'
    AND updated_at >= NOW() - INTERVAL '1 day' * period_days;
  
  -- Count active subscriptions at start of period
  SELECT COUNT(*) INTO starting_count
  FROM public.subscriptions
  WHERE created_at < NOW() - INTERVAL '1 day' * period_days;
  
  IF starting_count > 0 THEN
    RETURN (churned_count::DECIMAL / starting_count::DECIMAL) * 100;
  ELSE
    RETURN 0;
  END IF;
END;
$$ LANGUAGE plpgsql;
```

---

## 6. Looker Studio Connection

### Direct PostgreSQL Connection

#### Get Connection Details
- [ ] Go to Supabase Dashboard > Settings > Database
- [ ] Note connection details:
  - Host: `db.xxxxx.supabase.co`
  - Port: `5432`
  - Database: `postgres`
  - User: `postgres`
  - Password: (your database password)

#### Connect Looker Studio
- [ ] Open Looker Studio
- [ ] Create new report
- [ ] Add data source > PostgreSQL
- [ ] Enter connection details:
  - Host: `db.xxxxx.supabase.co`
  - Port: `5432`
  - Database: `postgres`
  - User: `postgres`
  - Password: (your password)
- [ ] Enable SSL: Yes
- [ ] Test connection

#### Create Custom SQL Query Data Source

```sql
SELECT
  snapshot_date as date,
  total_users,
  active_users,
  dau,
  mau,
  active_subscriptions,
  mrr,
  arr,
  arpu,
  churn_rate,
  ltv,
  cac,
  nps
FROM public.kpi_snapshots
WHERE snapshot_date >= CURRENT_DATE - INTERVAL '90 days'
ORDER BY snapshot_date DESC
```

### Alternative: Use Supabase REST API

- [ ] Create Looker Studio Community Connector
- [ ] Use Google Apps Script
- [ ] Call Supabase REST API endpoints
- [ ] Parse JSON responses

---

## 7. Nexpo Implementation

### Next.js Web Setup

#### Install Dependencies
```bash
npm install @supabase/supabase-js @supabase/auth-helpers-nextjs @auth0/nextjs-auth0
```

#### Create Supabase Client (`lib/supabase.ts`)
```typescript
import { createClient } from '@supabase/supabase-js';

const supabaseUrl = process.env.NEXT_PUBLIC_SUPABASE_URL!;
const supabaseAnonKey = process.env.NEXT_PUBLIC_SUPABASE_ANON_KEY!;

export const supabase = createClient(supabaseUrl, supabaseAnonKey);
```

#### Create Server-Side Client (`lib/supabase-server.ts`)
```typescript
import { createClient } from '@supabase/supabase-js';

const supabaseUrl = process.env.NEXT_PUBLIC_SUPABASE_URL!;
const supabaseServiceKey = process.env.SUPABASE_SERVICE_ROLE_KEY!;

export const supabaseAdmin = createClient(supabaseUrl, supabaseServiceKey, {
  auth: {
    autoRefreshToken: false,
    persistSession: false
  }
});
```

#### API Route Example (`app/api/bi/metrics/route.ts`)
```typescript
import { NextResponse } from 'next/server';
import { getSession } from '@auth0/nextjs-auth0';
import { supabaseAdmin } from '@/lib/supabase-server';

export async function GET() {
  const session = await getSession();
  
  if (!session || session.user.role !== 'admin') {
    return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
  }

  try {
    const { data, error } = await supabaseAdmin
      .from('current_metrics')
      .select('*')
      .single();

    if (error) throw error;

    return NextResponse.json(data);
  } catch (error) {
    return NextResponse.json({ error: 'Failed to fetch metrics' }, { status: 500 });
  }
}
```

### Expo Mobile Setup

#### Install Dependencies
```bash
npx expo install @supabase/supabase-js @react-native-async-storage/async-storage react-native-url-polyfill
```

#### Initialize Supabase (`utils/supabase.ts`)
```typescript
import 'react-native-url-polyfill/auto';
import { createClient } from '@supabase/supabase-js';
import AsyncStorage from '@react-native-async-storage/async-storage';

const supabaseUrl = process.env.EXPO_PUBLIC_SUPABASE_URL!;
const supabaseAnonKey = process.env.EXPO_PUBLIC_SUPABASE_ANON_KEY!;

export const supabase = createClient(supabaseUrl, supabaseAnonKey, {
  auth: {
    storage: AsyncStorage,
    autoRefreshToken: true,
    persistSession: true,
    detectSessionInUrl: false,
  },
});
```

---

## 8. Taurte Implementation

### Svelte Web Setup

#### Install Dependencies
```bash
npm install @supabase/supabase-js @auth0/auth0-spa-js
```

#### Create Supabase Client (`src/lib/supabase.ts`)
```typescript
import { createClient } from '@supabase/supabase-js';

const supabaseUrl = import.meta.env.VITE_SUPABASE_URL;
const supabaseAnonKey = import.meta.env.VITE_SUPABASE_ANON_KEY;

export const supabase = createClient(supabaseUrl, supabaseAnonKey);
```

#### Svelte Store for Real-Time Data (`src/stores/biMetrics.ts`)
```typescript
import { writable } from 'svelte/store';
import { supabase } from '$lib/supabase';

export const metrics = writable(null);
export const loading = writable(false);

export async function subscribeToMetrics() {
  loading.set(true);
  
  // Fetch initial data
  const { data } = await supabase
    .from('current_metrics')
    .select('*')
    .single();
  
  metrics.set(data);
  loading.set(false);
  
  // Subscribe to real-time updates
  const subscription = supabase
    .channel('metrics_changes')
    .on('postgres_changes', {
      event: '*',
      schema: 'public',
      table: 'kpi_snapshots'
    }, (payload) => {
      console.log('Metrics updated:', payload);
      // Refetch metrics
      fetchMetrics();
    })
    .subscribe();
  
  return () => {
    subscription.unsubscribe();
  };
}

async function fetchMetrics() {
  const { data } = await supabase
    .from('current_metrics')
    .select('*')
    .single();
  metrics.set(data);
}
```

### Tauri Rust Backend

#### Add Dependencies to Cargo.toml
```toml
[dependencies]
reqwest = { version = "0.11", features = ["json"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
tokio = { version = "1", features = ["full"] }
```

#### Tauri Command (`src-tauri/src/main.rs`)
```rust
use serde::{Deserialize, Serialize};
use reqwest;

#[derive(Serialize, Deserialize)]
struct SupabaseMetrics {
    total_users: i32,
    mau: i32,
    dau: i32,
    active_subscriptions: i32,
    total_mrr: f64,
    total_arr: f64,
}

#[tauri::command]
async fn fetch_supabase_metrics(
    supabase_url: String,
    anon_key: String,
    auth_token: String
) -> Result<SupabaseMetrics, String> {
    let client = reqwest::Client::new();
    let url = format!("{}/rest/v1/current_metrics?select=*", supabase_url);
    
    let response = client
        .get(&url)
        .header("apikey", anon_key)
        .header("Authorization", format!("Bearer {}", auth_token))
        .send()
        .await
        .map_err(|e| e.to_string())?;
    
    let metrics: Vec<SupabaseMetrics> = response
        .json()
        .await
        .map_err(|e| e.to_string())?;
    
    metrics.into_iter().next()
        .ok_or_else(|| "No metrics found".to_string())
}
```

---

## 9. Looker Studio Embedding

### Generate Embed URL
- [ ] Open Looker Studio dashboard
- [ ] Click Share > Embed
- [ ] Copy iframe code
- [ ] URL format: `https://lookerstudio.google.com/embed/reporting/YOUR_REPORT_ID`

### Nexpo Web Component
```typescript
// components/LookerEmbed.tsx
export default function LookerEmbed({ reportUrl }: { reportUrl: string }) {
  return (
    <iframe
      src={reportUrl}
      width="100%"
      height="800px"
      style={{ border: 0 }}
      allowFullScreen
    />
  );
}
```

### Taurte Svelte Component
```svelte
<!-- components/LookerEmbed.svelte -->
<iframe
  src={reportUrl}
  width="100%"
  height="800px"
  style="border: 0;"
  allowfullscreen
  title="BI Dashboard"
/>
```

---

## 10. Testing & Deployment

### Testing Checklist
- [ ] Test Auth0 login flow
- [ ] Verify user role syncs to Supabase
- [ ] Test RLS policies (try accessing as different roles)
- [ ] Verify KPI calculations accuracy
- [ ] Test real-time subscriptions
- [ ] Test Looker Studio connection
- [ ] Test dashboard embeds on all platforms
- [ ] Load test API endpoints
- [ ] Test mobile responsiveness

### Production Checklist
- [ ] Enable connection pooling (PgBouncer)
- [ ] Set up database backups (daily)
- [ ] Configure point-in-time recovery
- [ ] Enable database logs
- [ ] Set up monitoring alerts
- [ ] Optimize slow queries
- [ ] Add CDN for static assets
- [ ] Configure rate limiting
- [ ] Enable HTTPS everywhere
- [ ] Document all API endpoints

---

## Success Criteria
- [ ] Supabase database created and configured
- [ ] RLS policies enforce role-based access
- [ ] Auth0 syncs user roles to Supabase
- [ ] Looker Studio connects to PostgreSQL
- [ ] All KPIs calculate correctly
- [ ] Real-time updates work
- [ ] Dashboards embed in Nexpo and Taurte
- [ ] Sub-2 second query times
- [ ] 99.9% uptime
