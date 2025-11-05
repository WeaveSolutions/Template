/**
 * Client-side stub for Ditto Sync Factory
 * Database sync is server-side only
 */

export class DittoSyncFactory {
  constructor(config) {
    console.warn('DittoSyncFactory is not available on the client side');
  }

  async initialize(ditto) {
    // No-op on client
  }

  getAdapters() {
    return [];
  }

  getAdapter(providerType) {
    return null;
  }

  async syncCollection(collection, direction) {
    // No-op on client
  }

  async syncAll() {
    // No-op on client
  }

  getSyncStatus() {
    return [];
  }

  async dispose() {
    // No-op on client
  }
}
