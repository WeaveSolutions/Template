# @weave/bi-core

**Shared BI Dashboard Core** - Platform-agnostic business intelligence infrastructure.

Works with both **Nexpo** (React/Next.js/Expo) and **Taurte** (Svelte/SvelteKit/Tauri) across Web, Mobile, and Desktop platforms.

## Features

- ✅ **Type-safe** TypeScript interfaces for all BI data
- ✅ **Mock data** for development and testing
- ✅ **Multiple database adapters** (Firebase, Supabase, MongoDB)
- ✅ **Calculation utilities** (ARPU, LTV, CAC, churn, etc.)
- ✅ **Formatters** (currency, percentage, dates)
- ✅ **Validators** for data quality
- ✅ **Platform-agnostic** - works everywhere

## Installation

```bash
# In your Nexpo or Taurte app
pnpm add @weave/bi-core
```

## Usage

### Basic Example

```typescript
import { createBIClient, mockKPIData, formatCurrency } from '@weave/bi-core';

// Create a mock API client
const client = createBIClient({ type: 'mock' });

// Fetch KPI data
const kpis = await client.fetchKPIData();
console.log(formatCurrency(kpis.mrr)); // "$98,750.00"
```

### With React (Nexpo)

```tsx
import { useEffect, useState } from 'react';
import { createBIClient, type KPIData } from '@weave/bi-core';

export function useBIData() {
  const [data, setData] = useState<KPIData | null>(null);
  
  useEffect(() => {
    const client = createBIClient({ type: 'mock' });
    client.fetchKPIData().then(setData);
  }, []);
  
  return data;
}
```

### With Svelte (Taurte)

```typescript
import { writable } from 'svelte/store';
import { createBIClient, type KPIData } from '@weave/bi-core';

const client = createBIClient({ type: 'mock' });
export const biData = writable<KPIData | null>(null);

export async function loadBIData() {
  const data = await client.fetchKPIData();
  biData.set(data);
}
```

## API Clients

### Mock (Development)

```typescript
const client = createBIClient({
  type: 'mock',
  mock: {
    simulateDelay: true,
    delayMs: 500
  }
});
```

### Firebase (Coming Soon)

```typescript
const client = createBIClient({
  type: 'firebase',
  firebase: {
    projectId: 'your-project',
    apiKey: 'your-api-key'
  }
});
```

### Supabase (Coming Soon)

```typescript
const client = createBIClient({
  type: 'supabase',
  supabase: {
    supabaseUrl: 'https://xxx.supabase.co',
    supabaseKey: 'your-key'
  }
});
```

### MongoDB (Coming Soon)

```typescript
const client = createBIClient({
  type: 'mongodb',
  mongodb: {
    connectionString: 'mongodb+srv://...',
    database: 'bi_dashboard'
  }
});
```

## Utilities

### Calculations

```typescript
import { calculateARPU, calculateLTVCACRatio } from '@weave/bi-core/utils';

const arpu = calculateARPU(98750, 8930); // 11.05
const ratio = calculateLTVCACRatio(450, 120); // 3.75
```

### Formatters

```typescript
import { formatCurrency, formatPercentage, formatNumber } from '@weave/bi-core/utils';

formatCurrency(98750); // "$98,750.00"
formatPercentage(3.2); // "3.2%"
formatNumber(1500000); // "1.5M"
```

### Validators

```typescript
import { validateKPIData, isHealthyKPI } from '@weave/bi-core/utils';

const isValid = validateKPIData(kpis); // true/false
const isHealthy = isHealthyKPI('churnRate', 3.2); // true (< 5%)
```

## Configuration

```typescript
import { CHART_COLORS, KPI_THRESHOLDS, LOOKER_STUDIO_URL } from '@weave/bi-core/config';

// Use in your charts
const chartColors = CHART_COLORS.primary; // '#3B82F6'

// Check thresholds
const maxChurn = KPI_THRESHOLDS.churnRate.danger; // 10

// Embed Looker Studio
<iframe src={LOOKER_STUDIO_URL} />
```

## Package Structure

```
@weave/bi-core/
├── types/           # TypeScript interfaces
├── api/             # API client interface + implementations
├── utils/           # Calculations, formatters, validators
├── mock-data/       # Sample data for testing
└── config/          # Constants and configuration
```

## License

Apache-2.0
