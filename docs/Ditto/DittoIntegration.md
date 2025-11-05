# Ditto Integration with Multi-Cloud Databases

This document provides instructions on how to use Ditto with your Next.js, Expo, and Tauri applications, enabling a local-first architecture with synchronization capabilities to multiple cloud databases simultaneously.

## Table of Contents

- [Overview](#overview)
- [Getting Started](#getting-started)
  - [Environment Variables](#environment-variables)
  - [Next.js Setup](#nextjs-setup)
  - [Expo Setup](#expo-setup)
  - [Tauri Setup](#tauri-setup)
- [Multi-Cloud Synchronization](#multi-cloud-synchronization)
  - [How It Works](#how-it-works)
  - [Sync Flow](#sync-flow)
- [Providers](#providers)
  - [PostgreSQL](#postgresql)
  - [MongoDB](#mongodb)
  - [CosmosDB](#cosmosdb)
  - [SQL Server](#sql-server)
  - [IBM Cloud DB2](#ibm-cloud-db2)
- [Usage Examples](#usage-examples)
- [Troubleshooting](#troubleshooting)

## Overview

Ditto is a local-first database that enables real-time synchronization between devices and cloud databases. This integration provides:

- Cross-platform support for Next.js, Expo, and Tauri
- Offline-first capabilities with seamless syncing
- **True multi-cloud synchronization** with multiple database providers simultaneously
- Bidirectional data flow between Ditto and all cloud databases
- Configurable synchronization options and intervals
- Automatic conflict resolution

## Getting Started

### Environment Variables

First, set up the required environment variables in your `.env` file. You can use the `.env.example` file as a template.

```env
# Enable/disable Ditto
ENABLE_DITTO=true

# Ditto credentials (get these from your Ditto dashboard)
DITTO_APP_ID=your-ditto-app-id
DITTO_TOKEN=your-ditto-token

# Cloud sync configuration
DITTO_CLOUD_SYNC_ENABLED=true
DITTO_CLOUD_SYNC_URL=your-sync-service-url
DITTO_SYNC_WITH_PEERS=true
DITTO_SYNC_WITH_CLOUD=true
DITTO_CLOUD_SYNC_INTERVAL=60000
DITTO_CLOUD_SYNC_BATCH_SIZE=50

# Enable/disable sync with specific providers
DITTO_SYNC_POSTGRES=true
DITTO_SYNC_MONGODB=true
DITTO_SYNC_COSMOSDB=true
DITTO_SYNC_SQLSERVER=true
DITTO_SYNC_IBMCLOUD=true
```

For Expo applications, the environment variables should be prefixed with `EXPO_PUBLIC_`:

```env
EXPO_PUBLIC_ENABLE_DITTO=true
EXPO_PUBLIC_DITTO_APP_ID=your-ditto-app-id
EXPO_PUBLIC_DITTO_TOKEN=your-ditto-token
# And so on...
```

### Next.js Setup

1. Add the Ditto provider to your application's entry point (`_app.tsx` or `_app.jsx`):

```tsx
import { DittoProvider } from '@your-app/shared-provider';
import { getDittoConfig, getCloudSyncConfig } from '@your-app/shared-utils/ditto-config';

function MyApp({ Component, pageProps }) {
  // Get configuration from environment variables
  const dittoConfig = getDittoConfig();
  const cloudSyncConfig = getCloudSyncConfig();

  return (
    <DittoProvider dittoConfig={dittoConfig} cloudSyncConfig={cloudSyncConfig}>
      <Component {...pageProps} />
    </DittoProvider>
  );
}

export default MyApp;
```

2. Install the necessary dependencies:

```bash
pnpm install @dittolive/ditto
```

### Expo Setup

1. Add the Ditto provider to your Expo application's entry point:

```tsx
import { DittoProvider } from '@your-app/shared-provider';
import { getExpoDittoConfig, getExpoCloudSyncConfig } from '@your-app/shared-utils/ditto-config';

export default function App() {
  // Get configuration from environment variables
  const dittoConfig = getExpoDittoConfig();
  const cloudSyncConfig = getExpoCloudSyncConfig();

  return (
    <DittoProvider dittoConfig={dittoConfig} cloudSyncConfig={cloudSyncConfig}>
      {/* Your app components */}
    </DittoProvider>
  );
}
```

2. Install the necessary dependencies:

```bash
pnpm install @dittolive/ditto react-native-ditto
```

3. Add the required permissions for Ditto in your `app.json`:

```json
{
  "expo": {
    "plugins": [
      [
        "react-native-ditto",
        {
          "iosInfoPlist": {
            "NSBluetoothAlwaysUsageDescription": "This app uses Bluetooth to discover and connect to nearby devices.",
            "NSLocalNetworkUsageDescription": "This app uses the local network to discover and connect to nearby devices."
          }
        }
      ]
    ]
  }
}
```

## Multi-Cloud Synchronization

### How It Works

Our implementation enables true multi-cloud synchronization, allowing your data to flow seamlessly between:

1. **Ditto local database** on user devices (both web and mobile)
2. **Multiple cloud databases** from different providers simultaneously

This approach provides several key benefits:

- **Data redundancy** across multiple cloud providers
- **Vendor flexibility** without being locked into a single database provider
- **Hybrid cloud architecture** supporting private and public cloud deployments
- **Geographic distribution** of data for improved performance and compliance
- **Graceful degradation** if one cloud provider experiences issues

### Sync Flow

The synchronization process follows these steps:

1. **Local-first operations**: All read/write operations happen first in the local Ditto database, ensuring fast response times and offline capabilities

2. **Peer-to-peer sync**: If enabled, Ditto automatically syncs between peer devices over local networks

3. **Multi-cloud sync**: At configurable intervals or on-demand, data is synchronized with all enabled cloud providers

4. **Conflict resolution**: Any conflicts are resolved automatically using Ditto's conflict resolution mechanisms

5. **Bidirectional flow**: Changes from any cloud database are pulled back to Ditto and then propagated to other cloud databases in the next sync cycle

## Providers

You can enable multiple database providers simultaneously. Data will be synchronized across all enabled providers.

### PostgreSQL

The PostgreSQL provider enables synchronization between Ditto and PostgreSQL databases. 

```env
# Enable PostgreSQL synchronization
DITTO_SYNC_POSTGRES=true
POSTGRES_CONNECTION_STRING=postgres://username:password@hostname:port/database
```

### MongoDB

The MongoDB provider enables synchronization between Ditto and MongoDB databases.

```env
# Enable MongoDB synchronization
DITTO_SYNC_MONGODB=true
MONGODB_CONNECTION_STRING=mongodb://username:password@hostname:port/database
```

### CosmosDB

The CosmosDB provider enables synchronization between Ditto and Azure Cosmos DB.

```env
# Enable CosmosDB synchronization
DITTO_SYNC_COSMOSDB=true
COSMOSDB_CONNECTION_STRING=your-cosmosdb-connection-string
COSMOSDB_DATABASE_NAME=dittoSync
```

### SQL Server

The SQL Server provider enables synchronization between Ditto and Microsoft SQL Server.

```env
# Enable SQL Server synchronization
DITTO_SYNC_SQLSERVER=true
SQLSERVER_CONNECTION_STRING=Server=myServerAddress;Database=myDataBase;User Id=myUsername;Password=myPassword;
SQLSERVER_SCHEMA=dbo
```

### IBM Cloud DB2

The IBM Cloud DB2 provider enables synchronization between Ditto and IBM Cloud DB2 databases.

```env
# Enable IBM Cloud DB2 synchronization
DITTO_SYNC_IBMCLOUD=true
IBMCLOUD_CONNECTION_STRING=postgres://username:password@hostname:port/database
```

The integration uses the PostgreSQL connector for IBM Cloud DB2 since Prisma doesn't have native support for DB2.

## Usage Examples

### Using the Ditto Hook

You can use the `useDittoContext` hook to access Ditto functionality in your components:

```tsx
import { useDittoContext } from '@your-app/shared-provider';

function MyComponent() {
  const { ditto, isInitialized, isOnline, syncNow, isSyncing, lastSyncTime } = useDittoContext();
  
  // Use Ditto in your component
  const addItem = async () => {
    if (ditto && isInitialized) {
      await ditto.collection('items').upsert({
        name: 'New Item',
        createdAt: Date.now()
      });
    }
  };
  
  return (
    <div>
      <button onClick={addItem}>Add Item</button>
      <button onClick={syncNow} disabled={isSyncing}>
        {isSyncing ? 'Syncing...' : 'Sync Now'}
      </button>
      {lastSyncTime && (
        <p>Last synced: {lastSyncTime.toLocaleString()}</p>
      )}
    </div>
  );
}
```

### Using the DittoExample Component

We've provided a `DittoExample` component that demonstrates basic Ditto functionality:

```tsx
import { DittoExample } from '@your-app/shared-components/ditto/DittoExample';

function MyPage() {
  return (
    <div>
      <h1>Ditto Example</h1>
      <DittoExample />
    </div>
  );
}
```

## Troubleshooting

### Common Issues

1. **Ditto not initializing**: Make sure you've set up all the required environment variables and installed the necessary dependencies.

2. **Sync not working**: Check your network connection and ensure the cloud sync URL is correct.

3. **Database connection issues**: Verify your connection strings for each database provider and ensure your firewalls allow connections.

4. **Data not syncing between clouds**: Ensure you've enabled multiple providers in your environment variables and that all connection strings are correct.

### Debugging

You can enable debug logging by setting:

```env
DEBUG_DITTO=true
DEBUG_DITTO_SYNC=true
```

This will log detailed information about Ditto operations and sync activities to the console.

### Monitoring Sync Status

You can monitor the sync status for each provider using the `getSyncStatus` method from the `useDittoContext` hook:

```tsx
const { getSyncStatus } = useDittoContext();

const syncStatus = getSyncStatus();
console.log('Sync status for all providers:', syncStatus);
```

This returns an array of status objects with information about each provider, including the last sync time and any errors.

### Support

For more information on Ditto, refer to the [official Ditto documentation](https://docs.ditto.live/).

For issues specific to this integration, please file an issue in the project repository.
