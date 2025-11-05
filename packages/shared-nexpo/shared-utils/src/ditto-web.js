/**
 * Ditto Web Implementation for Next.js
 */
import { registerDittoPlatform } from './ditto-service';
// Import Ditto for web - dynamic import to avoid SSR issues
let Ditto = null;
// Web implementation of the Ditto instance
class WebDittoInstance {
    store = null;
    isInitialized = false;
    isOnline = false;
    collections = new Map();
    subscriptions = new Map();
    async initialize(config) {
        if (typeof window === 'undefined') {
            console.warn('Ditto initialization skipped in SSR context');
            return;
        }
        if (!Ditto) {
            try {
                // Dynamic import to avoid SSR issues
                const ditto = await import('@dittolive/ditto');
                Ditto = ditto;
            }
            catch (error) {
                console.error('Failed to import Ditto:', error);
                throw error;
            }
        }
        try {
            // Initialize Ditto for web
            const identity = {
                appID: config.appID,
                token: config.token,
            };
            // Create Ditto instance
            this.store = new Ditto.Ditto(identity, {
                type: 'onlinePlayground',
                enableSyncWithPeers: config.syncWithPeers,
                webAssemblyDirectoryURL: '/ditto_wasm/',
            });
            // Initialize collections
            for (const name of config.collectionNames) {
                this.collections.set(name, this.store.collection(name));
            }
            // Set up cloud sync if enabled
            if (config.enableCloudSync && config.syncWithCloud && config.cloudSyncURL) {
                await this.store.sync.configure({
                    syncURL: config.cloudSyncURL,
                    authenticator: new Ditto.OnlinePlaygroundAuthenticator(identity),
                });
            }
            // Start Ditto
            await this.store.startSync();
            this.isInitialized = true;
            this.isOnline = navigator.onLine;
            // Listen for online/offline events
            window.addEventListener('online', () => {
                this.isOnline = true;
            });
            window.addEventListener('offline', () => {
                this.isOnline = false;
            });
            console.log('Ditto initialized successfully (web)');
        }
        catch (error) {
            console.error('Failed to initialize Ditto:', error);
            throw error;
        }
    }
    async sync() {
        if (!this.isInitialized) {
            throw new Error('Ditto is not initialized');
        }
        try {
            // For web, we don't need to do anything special - Ditto automatically syncs
            // We could implement custom sync logic here if needed
            console.log('Ditto sync triggered');
        }
        catch (error) {
            console.error('Failed to sync Ditto:', error);
            throw error;
        }
    }
    async disconnect() {
        if (!this.isInitialized) {
            return;
        }
        try {
            // Clear all subscriptions
            for (const [_, subscription] of this.subscriptions.entries()) {
                subscription.cancel();
            }
            this.subscriptions.clear();
            // Stop Ditto
            await this.store.stopSync();
            this.isInitialized = false;
            console.log('Ditto disconnected successfully');
        }
        catch (error) {
            console.error('Failed to disconnect Ditto:', error);
            throw error;
        }
    }
    collection(name) {
        if (!this.isInitialized) {
            throw new Error('Ditto is not initialized');
        }
        if (!this.collections.has(name)) {
            this.collections.set(name, this.store.collection(name));
        }
        return this.collections.get(name);
    }
    async query(collectionName, query) {
        if (!this.isInitialized) {
            throw new Error('Ditto is not initialized');
        }
        try {
            const collection = this.collection(collectionName);
            const result = await collection.find(query).exec();
            return result.items.map((item) => ({
                id: item.id,
                ...item.value,
            }));
        }
        catch (error) {
            console.error(`Failed to query collection ${collectionName}:`, error);
            throw error;
        }
    }
    async insert(collectionName, document) {
        if (!this.isInitialized) {
            throw new Error('Ditto is not initialized');
        }
        try {
            const collection = this.collection(collectionName);
            const result = await collection.insert(document);
            return result;
        }
        catch (error) {
            console.error(`Failed to insert document into ${collectionName}:`, error);
            throw error;
        }
    }
    async update(collectionName, id, updateObject) {
        if (!this.isInitialized) {
            throw new Error('Ditto is not initialized');
        }
        try {
            const collection = this.collection(collectionName);
            await collection.findByID(id).update((doc) => {
                Object.keys(updateObject).forEach((key) => {
                    doc[key] = updateObject[key];
                });
            });
            return true;
        }
        catch (error) {
            console.error(`Failed to update document in ${collectionName}:`, error);
            throw error;
        }
    }
    async remove(collectionName, id) {
        if (!this.isInitialized) {
            throw new Error('Ditto is not initialized');
        }
        try {
            const collection = this.collection(collectionName);
            await collection.findByID(id).remove();
            return true;
        }
        catch (error) {
            console.error(`Failed to remove document from ${collectionName}:`, error);
            throw error;
        }
    }
    subscribe(collectionName, callback) {
        if (!this.isInitialized) {
            throw new Error('Ditto is not initialized');
        }
        try {
            const collection = this.collection(collectionName);
            const subscription = collection.find().subscribe((result) => {
                const docs = result.items.map((item) => ({
                    id: item.id,
                    ...item.value,
                }));
                callback(docs);
            });
            const subscriptionKey = `${collectionName}-${Date.now()}`;
            this.subscriptions.set(subscriptionKey, subscription);
            return () => {
                if (this.subscriptions.has(subscriptionKey)) {
                    const sub = this.subscriptions.get(subscriptionKey);
                    sub.cancel();
                    this.subscriptions.delete(subscriptionKey);
                }
            };
        }
        catch (error) {
            console.error(`Failed to subscribe to collection ${collectionName}:`, error);
            throw error;
        }
    }
    async exportData() {
        if (!this.isInitialized) {
            throw new Error('Ditto is not initialized');
        }
        try {
            // Export all collections as JSON
            const exportData = {};
            for (const [name, _] of this.collections.entries()) {
                const docs = await this.query(name, {});
                exportData[name] = docs;
            }
            return JSON.stringify(exportData);
        }
        catch (error) {
            console.error('Failed to export Ditto data:', error);
            throw error;
        }
    }
    async importData(data) {
        if (!this.isInitialized) {
            throw new Error('Ditto is not initialized');
        }
        try {
            const importData = JSON.parse(data);
            // Import data into collections
            for (const [name, docs] of Object.entries(importData)) {
                const collection = this.collection(name);
                // Clear existing data
                const existingDocs = await this.query(name, {});
                for (const doc of existingDocs) {
                    await collection.findByID(doc.id).remove();
                }
                // Insert new data
                for (const doc of docs) {
                    const { id, ...rest } = doc;
                    await collection.insert(rest);
                }
            }
        }
        catch (error) {
            console.error('Failed to import Ditto data:', error);
            throw error;
        }
    }
}
// Create the web platform implementation
export const WebDitto = {
    createInstance: () => {
        return new WebDittoInstance();
    }
};
// Auto-register will be done lazily when DittoService is initialized
// to avoid SSR issues with Next.js
export default WebDitto;
//# sourceMappingURL=ditto-web.js.map
