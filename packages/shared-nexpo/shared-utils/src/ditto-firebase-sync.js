/**
 * Ditto Firebase Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and Firebase Firestore databases.
 */
import { FirebaseProvider } from '../../shared-db/src/providers/firebase-provider';
import { SyncDirection } from './ditto-cloud-sync';
/**
 * Firebase sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export class FirebaseSyncAdapter {
    ditto = null;
    provider = null;
    config;
    isInitialized = false;
    lastSyncTime = null;
    /**
     * Create a new Firebase sync adapter
     */
    constructor(config) {
        this.config = config;
    }
    /**
     * Initialize the adapter
     * @param ditto The Ditto instance to sync with
     */
    async initialize(ditto) {
        if (this.isInitialized) {
            return;
        }
        // Store the Ditto instance
        this.ditto = ditto;
        // Initialize the Firebase provider
        try {
            this.provider = FirebaseProvider.getInstance({
                projectId: this.config.projectId,
                apiKey: this.config.apiKey,
                authDomain: this.config.authDomain || `${this.config.projectId}.firebaseapp.com`
            });
            await this.provider.connect();
            this.isInitialized = true;
        }
        catch (error) {
            console.error('Failed to initialize Firebase sync adapter:', error);
            throw error;
        }
    }
    /**
     * Get the provider type
     */
    getProviderType() {
        return 'firebase';
    }
    /**
     * Sync data between Ditto and Firebase
     * @param collection The collection to sync
     * @param direction The direction to sync
     */
    async syncCollection(collection, direction) {
        if (!this.isInitialized || !this.ditto || !this.provider) {
            throw new Error('Firebase sync adapter not initialized');
        }
        // Use default direction if not specified
        const syncDirection = direction || this.config.defaultDirection;
        try {
            const dittoCollection = this.ditto.collection(collection);
            // For Firebase, we'll use the same collection name but prefix with Firebase
            const firestoreCollection = `Firebase${collection}`;
            if (syncDirection === SyncDirection.TO_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
                // Sync from Ditto to Firebase
                const dittoDocuments = await dittoCollection.find().exec();
                for (const doc of dittoDocuments) {
                    // Upsert the document to Firebase
                    await this.provider.setDocument(firestoreCollection, doc.id, doc.value);
                }
            }
            if (syncDirection === SyncDirection.FROM_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
                // Sync from Firebase to Ditto
                const firestoreDocs = await this.provider.getDocuments(firestoreCollection);
                for (const doc of firestoreDocs) {
                    // Upsert to Ditto
                    await dittoCollection.upsert({ _id: doc.id, ...doc.data });
                }
            }
            // Update last sync time
            this.lastSyncTime = new Date();
        }
        catch (error) {
            console.error(`Failed to sync collection ${collection}:`, error);
            throw error;
        }
    }
    /**
     * Sync all configured collections
     */
    async syncAll() {
        if (!this.isInitialized) {
            throw new Error('Firebase sync adapter not initialized');
        }
        for (const collection of this.config.collections) {
            await this.syncCollection(collection);
        }
    }
    /**
     * Get the sync status
     */
    getSyncStatus() {
        return {
            provider: this.getProviderType(),
            lastSync: this.lastSyncTime,
            collections: this.config.collections,
        };
    }
    /**
     * Dispose the adapter
     */
    async dispose() {
        if (this.provider) {
            await this.provider.disconnect();
        }
        this.ditto = null;
        this.provider = null;
        this.isInitialized = false;
    }
}
//# sourceMappingURL=ditto-firebase-sync.js.map
