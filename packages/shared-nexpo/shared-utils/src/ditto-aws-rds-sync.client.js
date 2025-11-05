/**
 * Client-side stub for AWS RDS Sync
 * Database sync is server-side only
 */

export class AwsRdsSyncAdapter {
  constructor(config) {
    console.warn('AwsRdsSyncAdapter is not available on the client side');
  }

  async initialize(ditto) {}
  async syncCollection(collection, direction) {}
  async syncAll() {}
  getSyncStatus() { return { connected: false }; }
  async dispose() {}
}
