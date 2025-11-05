/**
 * Ditto Supabase Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and Supabase databases.
 */
import { DittoInstance } from './ditto-service';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';
/**
 * Configuration for Supabase sync adapter
 */
export interface SupabaseSyncConfig {
    url: string;
    apiKey: string;
    collections: string[];
    defaultDirection: SyncDirection;
    schema?: string;
}
/**
 * Supabase sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export declare class SupabaseSyncAdapter implements CloudSyncAdapter {
    private ditto;
    private provider;
    private config;
    private isInitialized;
    private lastSyncTime;
    /**
     * Create a new Supabase sync adapter
     */
    constructor(config: SupabaseSyncConfig);
    /**
     * Initialize the adapter
     * @param ditto The Ditto instance to sync with
     */
    initialize(ditto: DittoInstance): Promise<void>;
    /**
     * Get the provider type
     */
    getProviderType(): string;
    /**
     * Sync data between Ditto and Supabase
     * @param collection The collection to sync
     * @param direction The direction to sync
     */
    syncCollection(collection: string, direction?: SyncDirection): Promise<void>;
    /**
     * Sync all configured collections
     */
    syncAll(): Promise<void>;
    /**
     * Get the sync status
     */
    getSyncStatus(): SyncStatus;
    /**
     * Dispose the adapter
     */
    dispose(): Promise<void>;
}
//# sourceMappingURL=ditto-supabase-sync.d.ts.map
