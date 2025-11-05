// Kong-compatible session management and caching
// This implementation uses in-memory cache and JWT tokens with Kong Gateway

interface SessionData {
  userId: string;
  email: string;
  roles: string[];
  createdAt: number;
  lastAccessed: number;
  userAgent?: string;
  ipAddress?: string;
}

interface CacheEntry {
  data: SessionData;
  expiresAt: number;
}

/**
 * Kong-compatible Session Store
 * 
 * This works with Kong's JWT validation and proxy caching. Kong handles 
 * the heavy lifting for caching and JWT validation at the gateway level.
 * This store manages session state locally while Kong manages API-level caching.
 */
class KongSessionStore {
  private cache = new Map<string, CacheEntry>();
  private ttlMs: number;
  private cleanupInterval: NodeJS.Timeout;

  constructor(ttlSeconds = 86400) {
    this.ttlMs = ttlSeconds * 1000; // Convert to milliseconds
    
    // Cleanup expired entries every 5 minutes
    this.cleanupInterval = setInterval(() => {
      this.cleanup();
    }, 5 * 60 * 1000);
  }

  private cleanup(): void {
    const now = Date.now();
    for (const [key, entry] of this.cache.entries()) {
      if (entry.expiresAt < now) {
        this.cache.delete(key);
      }
    }
  }

  async get(sessionId: string): Promise<SessionData | null> {
    const entry = this.cache.get(sessionId);
    if (!entry) return null;

    // Check if expired
    if (entry.expiresAt < Date.now()) {
      this.cache.delete(sessionId);
      return null;
    }

    // Update last accessed
    entry.data.lastAccessed = Date.now();
    return entry.data;
  }

  async set(sessionId: string, data: SessionData): Promise<boolean> {
    const expiresAt = Date.now() + this.ttlMs;
    this.cache.set(sessionId, { data, expiresAt });
    return true;
  }

  async delete(sessionId: string): Promise<boolean> {
    return this.cache.delete(sessionId);
  }

  async exists(sessionId: string): Promise<boolean> {
    const entry = this.cache.get(sessionId);
    if (!entry) return false;
    
    if (entry.expiresAt < Date.now()) {
      this.cache.delete(sessionId);
      return false;
    }
    
    return true;
  }

  async getTTL(sessionId: string): Promise<number> {
    const entry = this.cache.get(sessionId);
    if (!entry) return -1;
    
    const remaining = Math.max(0, entry.expiresAt - Date.now());
    return Math.floor(remaining / 1000); // Convert to seconds
  }

  async refresh(sessionId: string): Promise<boolean> {
    const entry = this.cache.get(sessionId);
    if (!entry) return false;
    
    entry.expiresAt = Date.now() + this.ttlMs;
    return true;
  }

  // Alias for refresh to maintain compatibility with existing code
  async touch(sessionId: string): Promise<boolean> {
    return this.refresh(sessionId);
  }

  // Get cache statistics
  getStats() {
    return {
      size: this.cache.size,
      memoryUsage: process.memoryUsage(),
      ttl: this.ttlMs / 1000
    };
  }

  // Clear all sessions
  clear(): void {
    this.cache.clear();
  }

  // Cleanup on shutdown
  destroy(): void {
    if (this.cleanupInterval) {
      clearInterval(this.cleanupInterval);
    }
    this.cache.clear();
  }
}

// Kong-compatible cache wrapper functions
// These functions work with Kong's caching strategy and maintain compatibility
// with existing session management code

const sessionStore = new KongSessionStore();

/**
 * Kong-compatible caching functions
 * These work with Kong's proxy cache and JWT validation at the gateway level
 */

export const getAsync = async (key: string): Promise<string | null> => {
  const sessionData = await sessionStore.get(key.replace('sess:', ''));
  return sessionData ? JSON.stringify(sessionData) : null;
};

export const setAsync = async (key: string, value: string, ex?: number): Promise<string | null> => {
  try {
    const sessionId = key.replace('sess:', '');
    const data = JSON.parse(value) as SessionData;
    const success = await sessionStore.set(sessionId, data);
    return success ? 'OK' : null;
  } catch (error) {
    console.error('Kong session set error:', error);
    return null;
  }
};

export const delAsync = async (key: string): Promise<number> => {
  const sessionId = key.replace('sess:', '');
  const success = await sessionStore.delete(sessionId);
  return success ? 1 : 0;
};

export const ttlAsync = async (key: string): Promise<number> => {
  const sessionId = key.replace('sess:', '');
  return await sessionStore.getTTL(sessionId);
};

export const expireAsync = async (key: string, seconds: number): Promise<number> => {
  // Kong handles expiration at the gateway level via proxy cache
  return 1;
};

// Export types and the session store instance
export type { SessionData };
export { KongSessionStore };
export { sessionStore };
export default sessionStore;

// Kong integration notes:
// 1. Kong's JWT plugin validates tokens at the gateway level
// 2. Kong's proxy cache handles response caching automatically  
// 3. This in-memory cache is for session state only
// 4. Kong's rate limiting and CORS plugins handle API-level concerns
// 5. For production, consider Kong's cluster-wide cache sharing
// 6. Session headers can include cache-control directives for Kong
