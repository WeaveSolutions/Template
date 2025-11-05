/**
 * Auth0 Provider Service
 * Integrates with Auth0 Management API to manage user data and metadata
 * Implements CRA Account Center schema with linked list operations
 */

import type { ManagementClient } from 'auth0';
import { createHash, createHmac } from 'node:crypto';
import { z } from 'zod';

// Configuration schema
const Auth0ConfigSchema = z.object({
  domain: z.string(),
  clientId: z.string(),
  clientSecret: z.string(),
  audience: z.string(),
  scope: z.string().default('read:users update:users read:user_metadata update:user_metadata read:app_metadata update:app_metadata'),
  vaultUrl: z.string().optional(),
  vaultToken: z.string().optional(),
  hmacSecret: z.string(),
});

export type Auth0Config = z.infer<typeof Auth0ConfigSchema>;

// Linked list node interfaces
interface LinkedListNode {
  id: string;
  next: string | null;
}

interface ConnectedProviderNode extends LinkedListNode {
  provider: string;
  provider_user_id: string;
  display_name: string;
  email?: string;
  scopes: string[];
  token_data: {
    access_token_hash?: string;
    refresh_token_hash?: string;
    vault_reference: string;
    expires_at: string;
    scope_granted: string[];
  };
  status: 'active' | 'disconnected' | 'suspended' | 'error';
  connected_at: string;
  last_sync?: string;
  data_retention_choice: 'keep' | 'delete';
  error_count: number;
  last_error?: any;
  origin_signature: string;
}

interface DataObjectNode extends LinkedListNode {
  object_id: string;
  object_type: string;
  data_reference: {
    storage_type: 'inline' | 'external' | 'encrypted';
    storage_key: string;
    size_bytes: number;
    encryption_method: 'aes-256-gcm' | 'none';
  };
  origin: {
    provider: string;
    provider_user_id: string;
    source_type: string;
    imported_at: string;
    last_updated: string;
    retention_policy: 'keep' | 'delete';
    sync_enabled: boolean;
    quality_score: number;
    trust_level: 'verified' | 'unverified' | 'suspicious';
    integrity_hash: string;
  };
  access_log: {
    head: string | null;
    count: number;
    recent_actions: Array<{
      action: string;
      timestamp: string;
      source: string;
      ip_address: string;
      user_agent: string;
    }>;
  };
}

interface AuditTrailNode extends LinkedListNode {
  action: string;
  resource_type: string;
  resource_id: string;
  changes?: any;
  ip_address: string;
  user_agent: string;
  timestamp: string;
  previous: string | null;
}

// CRA Account Center structure
interface CRAAccountCenter {
  root_id: string;
  cra_version: string;
  primary_email: string;
  display_name?: string;
  connected_providers: {
    head: string | null;
    count: number;
    nodes: Record<string, ConnectedProviderNode>;
  };
  data_objects: {
    head: string | null;
    count: number;
    size_bytes: number;
    nodes: Record<string, DataObjectNode>;
  };
  user_preferences: {
    default_retention_policy: 'keep' | 'delete';
    mfa_enabled: boolean;
    preferred_mfa_method: 'sms' | 'totp' | 'webauthn' | 'none';
    notifications: {
      email_alerts: boolean;
      security_notifications: boolean;
      provider_sync_notifications: boolean;
    };
    privacy: {
      data_sharing_consent: boolean;
      analytics_enabled: boolean;
      gdpr_consent_given?: boolean;
      gdpr_consent_date?: string;
    };
  };
  system_metadata: {
    roles: string[];
    permissions: string[];
    subscription: {
      plan: 'free' | 'premium' | 'enterprise';
      status: 'active' | 'cancelled' | 'past_due';
      features_enabled: string[];
    };
    security: {
      risk_score: number;
      failed_login_attempts: number;
      account_locked: boolean;
      last_security_audit?: string;
    };
  };
  audit_trail: {
    head: string | null;
    tail: string | null;
    count: number;
    retention_days: number;
    last_compaction?: string;
  };
  created_at: string;
  updated_at: string;
  last_login?: string;
  schema_integrity: {
    checksum: string;
    last_verified: string;
    signature: string;
  };
}

export class Auth0Provider {
  private management: ManagementClient;
  private config: Auth0Config;

  constructor(config: Auth0Config) {
    this.config = Auth0ConfigSchema.parse(config);
    this.management = new ManagementClient({
      domain: this.config.domain,
      clientId: this.config.clientId,
      clientSecret: this.config.clientSecret,
      audience: this.config.audience,
      scope: this.config.scope,
    });
  }

  // === LINKED LIST OPERATIONS ===

  /**
   * Traverses a linked list and returns all nodes
   */
  private traverseLinkedList<T extends LinkedListNode>(
    nodes: Record<string, T>,
    head: string | null
  ): T[] {
    const result: T[] = [];
    let current = head;

    while (current) {
      const node = nodes[current];
      if (!node) break;
      result.push(node);
      current = node.next;
    }

    return result;
  }

  /**
   * Inserts a node at the beginning of a linked list
   */
  private insertNodeAtHead<T extends LinkedListNode>(
    nodes: Record<string, T>,
    head: string | null,
    newNode: T
  ): { newHead: string; newCount: number } {
    newNode.next = head;
    nodes[newNode.id] = newNode;

    // Count existing nodes
    const existingNodes = this.traverseLinkedList(nodes, head);
    const newCount = existingNodes.length + 1;

    return { newHead: newNode.id, newCount };
  }

  /**
   * Removes a node from a linked list
   */
  private removeNodeFromList<T extends LinkedListNode>(
    nodes: Record<string, T>,
    head: string | null,
    nodeId: string
  ): { newHead: string | null; newCount: number } {
    if (!head) return { newHead: null, newCount: 0 };

    // If removing the head node
    if (head === nodeId) {
      const headNode = nodes[head];
      delete nodes[nodeId];
      return {
        newHead: headNode?.next || null,
        newCount: this.traverseLinkedList(nodes, headNode?.next || null).length,
      };
    }

    // Find the node before the one to remove
    let current = head;
    while (current) {
      const currentNode = nodes[current];
      if (!currentNode) break;

      if (currentNode.next === nodeId) {
        const nodeToRemove = nodes[nodeId];
        currentNode.next = nodeToRemove?.next || null;
        delete nodes[nodeId];
        break;
      }
      current = currentNode.next;
    }

    const newCount = this.traverseLinkedList(nodes, head).length;
    return { newHead: head, newCount };
  }

  // === SECURITY OPERATIONS ===

  /**
   * Generates HMAC-SHA256 signature for tamper detection
   */
  private generateSignature(data: any): string {
    const dataString = JSON.stringify(data, Object.keys(data).sort());
    return createHmac('sha256', this.config.hmacSecret)
      .update(dataString)
      .digest('hex');
  }

  /**
   * Verifies HMAC-SHA256 signature
   */
  private verifySignature(data: any, signature: string): boolean {
    const expectedSignature = this.generateSignature(data);
    return expectedSignature === signature;
  }

  /**
   * Generates SHA-256 checksum for integrity verification
   */
  private generateChecksum(data: any): string {
    const dataString = JSON.stringify(data, Object.keys(data).sort());
    return createHash('sha256').update(dataString).digest('hex');
  }

  /**
   * Stores sensitive token data in Vault
   */
  private async storeInVault(data: any): Promise<string> {
    if (!this.config.vaultUrl || !this.config.vaultToken) {
      throw new Error('Vault configuration required for sensitive data storage');
    }

    const vaultReference = `auth0-token-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    
    // In a real implementation, this would make an API call to Vault
    // For now, we'll return a reference
    console.log(`[VAULT] Storing data with reference: ${vaultReference}`);
    
    return vaultReference;
  }

  /**
   * Retrieves sensitive token data from Vault
   */
  private async retrieveFromVault(reference: string): Promise<any> {
    if (!this.config.vaultUrl || !this.config.vaultToken) {
      throw new Error('Vault configuration required for sensitive data retrieval');
    }

    // In a real implementation, this would make an API call to Vault
    console.log(`[VAULT] Retrieving data with reference: ${reference}`);
    
    return null; // Placeholder
  }

  // === AUDIT LOGGING ===

  /**
   * Logs an audit event
   */
  private async logAuditEvent(
    userId: string,
    action: string,
    resourceType: string,
    resourceId: string,
    changes?: any,
    ipAddress?: string,
    userAgent?: string
  ): Promise<void> {
    const auditEvent: AuditTrailNode = {
      id: `audit_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`,
      action,
      resource_type: resourceType,
      resource_id: resourceId,
      changes,
      ip_address: ipAddress || 'unknown',
      user_agent: userAgent || 'unknown',
      timestamp: new Date().toISOString(),
      next: null,
      previous: null,
    };

    // Get current audit trail from user metadata
    const user = await this.management.getUser({ id: userId });
    const currentMetadata = user.app_metadata || {};
    const auditTrail = currentMetadata.audit_trail || {
      head: null,
      tail: null,
      count: 0,
      retention_days: 3653, // 10 years
      nodes: {},
    };

    // Insert at tail (append-only)
    if (auditTrail.tail) {
      const tailNode = auditTrail.nodes[auditTrail.tail];
      if (tailNode) {
        tailNode.next = auditEvent.id;
        auditEvent.previous = auditTrail.tail;
      }
    }

    auditTrail.nodes[auditEvent.id] = auditEvent;
    auditTrail.tail = auditEvent.id;
    auditTrail.count += 1;

    if (!auditTrail.head) {
      auditTrail.head = auditEvent.id;
    }

    // Update user metadata
    await this.management.updateAppMetadata({ id: userId }, { audit_trail: auditTrail });
  }

  // === CRUD OPERATIONS ===

  /**
   * Gets a user's complete CRA account center data
   */
  async getUserAccountCenter(userId: string): Promise<CRAAccountCenter | null> {
    try {
      const user = await this.management.getUser({ id: userId });
      if (!user) return null;

      const appMetadata = user.app_metadata || {};
      const userMetadata = user.user_metadata || {};

      // Construct CRA account center structure
      const accountCenter: CRAAccountCenter = {
        root_id: user.user_id!,
        cra_version: userMetadata.cra_version || '2.1.0',
        primary_email: user.email!,
        display_name: userMetadata.display_name || user.name || undefined,
        connected_providers: userMetadata.connected_providers || {
          head: null,
          count: 0,
          nodes: {},
        },
        data_objects: userMetadata.data_objects || {
          head: null,
          count: 0,
          size_bytes: 0,
          nodes: {},
        },
        user_preferences: userMetadata.user_preferences || {
          default_retention_policy: 'keep',
          mfa_enabled: false,
          preferred_mfa_method: 'none',
          notifications: {
            email_alerts: true,
            security_notifications: true,
            provider_sync_notifications: false,
          },
          privacy: {
            data_sharing_consent: false,
            analytics_enabled: true,
          },
        },
        system_metadata: {
          roles: appMetadata.roles || [],
          permissions: appMetadata.permissions || [],
          subscription: appMetadata.subscription || {
            plan: 'free',
            status: 'active',
            features_enabled: [],
          },
          security: appMetadata.security || {
            risk_score: 0.0,
            failed_login_attempts: 0,
            account_locked: false,
          },
        },
        audit_trail: appMetadata.audit_trail || {
          head: null,
          tail: null,
          count: 0,
          retention_days: 3653,
        },
        created_at: user.created_at!,
        updated_at: user.updated_at!,
        last_login: user.last_login || undefined,
        schema_integrity: {
          checksum: '',
          last_verified: new Date().toISOString(),
          signature: '',
        },
      };

      // Generate integrity checksum and signature
      accountCenter.schema_integrity.checksum = this.generateChecksum(accountCenter);
      accountCenter.schema_integrity.signature = this.generateSignature(accountCenter);

      await this.logAuditEvent(
        userId,
        'read',
        'account_center',
        userId,
        undefined,
        'system',
        'auth0-provider'
      );

      return accountCenter;
    } catch (error) {
      console.error('[Auth0Provider] Error getting user account center:', error);
      return null;
    }
  }

  /**
   * Adds a connected provider to a user's account
   */
  async addConnectedProvider(
    userId: string,
    provider: string,
    providerUserId: string,
    displayName: string,
    tokenData: any,
    options: {
      email?: string;
      scopes?: string[];
      ipAddress?: string;
      userAgent?: string;
    } = {}
  ): Promise<void> {
    try {
      const user = await this.management.getUser({ id: userId });
      const userMetadata = user.user_metadata || {};
      const connectedProviders = userMetadata.connected_providers || {
        head: null,
        count: 0,
        nodes: {},
      };

      // Store token data in Vault
      const vaultReference = await this.storeInVault(tokenData);

      // Create new provider node
      const providerNode: ConnectedProviderNode = {
        id: `provider_${provider}_${Date.now()}`,
        provider,
        provider_user_id: providerUserId,
        display_name: displayName,
        email: options.email,
        scopes: options.scopes || [],
        token_data: {
          access_token_hash: tokenData.access_token ? createHash('sha256').update(tokenData.access_token).digest('hex') : undefined,
          refresh_token_hash: tokenData.refresh_token ? createHash('sha256').update(tokenData.refresh_token).digest('hex') : undefined,
          vault_reference: vaultReference,
          expires_at: tokenData.expires_at || new Date(Date.now() + 3600000).toISOString(),
          scope_granted: tokenData.scope?.split(' ') || [],
        },
        status: 'active',
        connected_at: new Date().toISOString(),
        data_retention_choice: 'keep',
        error_count: 0,
        next: null,
        origin_signature: '',
      };

      // Generate signature for the provider node
      providerNode.origin_signature = this.generateSignature(providerNode);

      // Insert at head of linked list
      const { newHead, newCount } = this.insertNodeAtHead(
        connectedProviders.nodes,
        connectedProviders.head,
        providerNode
      );

      connectedProviders.head = newHead;
      connectedProviders.count = newCount;

      // Update user metadata
      await this.management.updateUserMetadata(
        { id: userId },
        { connected_providers: connectedProviders }
      );

      await this.logAuditEvent(
        userId,
        'add_connected_provider',
        'connected_provider',
        providerNode.id,
        { provider, provider_user_id: providerUserId },
        options.ipAddress,
        options.userAgent
      );

      console.log(`[Auth0Provider] Added connected provider ${provider} for user ${userId}`);
    } catch (error) {
      console.error('[Auth0Provider] Error adding connected provider:', error);
      throw error;
    }
  }

  /**
   * Removes a connected provider from a user's account
   */
  async removeConnectedProvider(
    userId: string,
    providerId: string,
    options: {
      ipAddress?: string;
      userAgent?: string;
    } = {}
  ): Promise<void> {
    try {
      const user = await this.management.getUser({ id: userId });
      const userMetadata = user.user_metadata || {};
      const connectedProviders = userMetadata.connected_providers || {
        head: null,
        count: 0,
        nodes: {},
      };

      // Remove from linked list
      const { newHead, newCount } = this.removeNodeFromList(
        connectedProviders.nodes,
        connectedProviders.head,
        providerId
      );

      connectedProviders.head = newHead;
      connectedProviders.count = newCount;

      // Update user metadata
      await this.management.updateUserMetadata(
        { id: userId },
        { connected_providers: connectedProviders }
      );

      await this.logAuditEvent(
        userId,
        'remove_connected_provider',
        'connected_provider',
        providerId,
        undefined,
        options.ipAddress,
        options.userAgent
      );

      console.log(`[Auth0Provider] Removed connected provider ${providerId} for user ${userId}`);
    } catch (error) {
      console.error('[Auth0Provider] Error removing connected provider:', error);
      throw error;
    }
  }

  /**
   * Updates user roles and permissions in app metadata
   */
  async updateUserRoles(
    userId: string,
    roles: string[],
    permissions: string[],
    options: {
      ipAddress?: string;
      userAgent?: string;
    } = {}
  ): Promise<void> {
    try {
      const user = await this.management.getUser({ id: userId });
      const appMetadata = user.app_metadata || {};

      const oldRoles = appMetadata.roles || [];
      const oldPermissions = appMetadata.permissions || [];

      // Update roles and permissions
      appMetadata.roles = roles;
      appMetadata.permissions = permissions;

      await this.management.updateAppMetadata({ id: userId }, appMetadata);

      await this.logAuditEvent(
        userId,
        'update_roles_permissions',
        'user_authorization',
        userId,
        {
          old_roles: oldRoles,
          new_roles: roles,
          old_permissions: oldPermissions,
          new_permissions: permissions,
        },
        options.ipAddress,
        options.userAgent
      );

      console.log(`[Auth0Provider] Updated roles and permissions for user ${userId}`);
    } catch (error) {
      console.error('[Auth0Provider] Error updating user roles:', error);
      throw error;
    }
  }

  /**
   * Verifies schema integrity for a user
   */
  async verifySchemaIntegrity(userId: string): Promise<boolean> {
    try {
      const accountCenter = await this.getUserAccountCenter(userId);
      if (!accountCenter) return false;

      // Verify checksum
      const currentChecksum = this.generateChecksum({
        ...accountCenter,
        schema_integrity: undefined, // Exclude integrity fields from checksum
      });

      // Verify signature
      const currentSignature = this.generateSignature({
        ...accountCenter,
        schema_integrity: undefined,
      });

      const checksumValid = currentChecksum === accountCenter.schema_integrity.checksum;
      const signatureValid = currentSignature === accountCenter.schema_integrity.signature;

      if (!checksumValid || !signatureValid) {
        await this.logAuditEvent(
          userId,
          'integrity_failure',
          'schema_integrity',
          userId,
          {
            checksum_valid: checksumValid,
            signature_valid: signatureValid,
          }
        );
      }

      return checksumValid && signatureValid;
    } catch (error) {
      console.error('[Auth0Provider] Error verifying schema integrity:', error);
      return false;
    }
  }

  /**
   * Compacts audit trail (removes old entries based on retention policy)
   */
  async compactAuditTrail(userId: string): Promise<void> {
    try {
      const user = await this.management.getUser({ id: userId });
      const appMetadata = user.app_metadata || {};
      const auditTrail = appMetadata.audit_trail;

      if (!auditTrail || !auditTrail.nodes) return;

      const retentionMs = auditTrail.retention_days * 24 * 60 * 60 * 1000;
      const cutoffDate = new Date(Date.now() - retentionMs);

      // Remove old entries
      const allNodes = this.traverseLinkedList(auditTrail.nodes, auditTrail.head);
      let compactedCount = 0;

      for (const node of allNodes) {
        const nodeDate = new Date(node.timestamp);
        if (nodeDate < cutoffDate) {
          delete auditTrail.nodes[node.id];
          compactedCount++;
        }
      }

      // Rebuild linked list structure
      const remainingNodes = Object.values(auditTrail.nodes).sort(
        (a, b) => new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime()
      );

      // Reset linked list
      auditTrail.head = remainingNodes.length > 0 ? remainingNodes[0].id : null;
      auditTrail.tail = remainingNodes.length > 0 ? remainingNodes[remainingNodes.length - 1].id : null;
      auditTrail.count = remainingNodes.length;
      auditTrail.last_compaction = new Date().toISOString();

      // Rebuild next/previous pointers
      for (let i = 0; i < remainingNodes.length; i++) {
        remainingNodes[i].next = i < remainingNodes.length - 1 ? remainingNodes[i + 1].id : null;
        remainingNodes[i].previous = i > 0 ? remainingNodes[i - 1].id : null;
      }

      await this.management.updateAppMetadata({ id: userId }, { audit_trail: auditTrail });

      console.log(`[Auth0Provider] Compacted ${compactedCount} audit entries for user ${userId}`);
    } catch (error) {
      console.error('[Auth0Provider] Error compacting audit trail:', error);
      throw error;
    }
  }
}

export default Auth0Provider;
