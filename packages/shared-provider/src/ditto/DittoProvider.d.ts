/**
 * Ditto Provider Component
 *
 * This provider wraps the application and initializes Ditto,
 * providing local-first database capabilities with multi-cloud sync.
 */
import React from 'react';
import { DittoConfig, DittoInstance } from '../../../shared-utils/src/ditto-service';
import { DittoCloudSync, CloudSyncConfig } from '../../../shared-utils/src/ditto-cloud-sync';
import '../../../shared-utils/src/ditto-web';
interface DittoContextType {
    ditto: DittoInstance | null;
    cloudSync: DittoCloudSync | null;
    isInitialized: boolean;
    isOnline: boolean;
    isSyncing: boolean;
    lastSyncTime: Date | null;
    error: Error | null;
    syncNow: () => Promise<void>;
}
interface DittoProviderProps {
    children: React.ReactNode;
    dittoConfig: DittoConfig;
    cloudSyncConfig?: CloudSyncConfig;
}
/**
 * Ditto Provider Component
 * Initializes and provides Ditto context throughout the app
 */
export declare function DittoProvider({ children, dittoConfig, cloudSyncConfig }: DittoProviderProps): React.JSX.Element;
/**
 * Hook to use Ditto in components
 */
export declare function useDittoContext(): DittoContextType;
export default DittoProvider;
//# sourceMappingURL=DittoProvider.d.ts.map
