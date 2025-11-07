/**
 * WebTransport Store
 * 
 * Manages WebTransport connection state and provides reactive updates
 */

import { writable, derived } from 'svelte/store';
import { createWebTransport, type WebTransportClient, type StreamMessage } from '../lib/webtransport';

export interface WebTransportState {
  connected: boolean;
  connecting: boolean;
  error: Error | null;
  reconnectAttempts: number;
  lastMessage: StreamMessage | null;
}

const initialState: WebTransportState = {
  connected: false,
  connecting: false,
  error: null,
  reconnectAttempts: 0,
  lastMessage: null,
};

// Create writable store
const webTransportState = writable<WebTransportState>(initialState);

// WebTransport client instance
let client: WebTransportClient | null = null;

/**
 * Initialize WebTransport connection
 */
export async function initWebTransport(url: string): Promise<void> {
  if (client) {
    console.warn('[WebTransportStore] Already initialized');
    return;
  }

  webTransportState.update(state => ({ ...state, connecting: true, error: null }));

  try {
    client = createWebTransport({
      url,
      reconnect: true,
      reconnectDelay: 1000,
      maxReconnectAttempts: 5,
      onConnect: () => {
        console.log('[WebTransportStore] Connected');
        webTransportState.update(state => ({
          ...state,
          connected: true,
          connecting: false,
          error: null,
          reconnectAttempts: 0,
        }));
      },
      onDisconnect: () => {
        console.log('[WebTransportStore] Disconnected');
        webTransportState.update(state => ({
          ...state,
          connected: false,
          connecting: false,
        }));
      },
      onError: (error: Error) => {
        console.error('[WebTransportStore] Error:', error);
        webTransportState.update(state => ({
          ...state,
          error,
          connecting: false,
        }));
      },
    });

    await client.connect();
  } catch (error) {
    console.error('[WebTransportStore] Initialization failed:', error);
    webTransportState.update(state => ({
      ...state,
      error: error as Error,
      connecting: false,
    }));
  }
}

/**
 * Send message via WebTransport
 */
export async function sendMessage(type: string, payload: any): Promise<void> {
  if (!client) {
    throw new Error('WebTransport not initialized');
  }

  if (!client.isConnected()) {
    throw new Error('WebTransport not connected');
  }

  const message: StreamMessage = {
    type,
    payload,
    timestamp: Date.now(),
  };

  await client.send(message);
}

/**
 * Send binary data via unidirectional stream
 */
export async function sendBinary(data: Uint8Array): Promise<void> {
  if (!client) {
    throw new Error('WebTransport not initialized');
  }

  await client.sendUnidirectional(data);
}

/**
 * Send datagram (unreliable, for real-time updates)
 */
export async function sendDatagram(data: Uint8Array): Promise<void> {
  if (!client) {
    throw new Error('WebTransport not initialized');
  }

  await client.sendDatagram(data);
}

/**
 * Subscribe to messages of a specific type
 */
export function onMessage(messageType: string, handler: (data: any) => void): () => void {
  if (!client) {
    console.warn('[WebTransportStore] Cannot subscribe - not initialized');
    return () => {};
  }

  // Update last message in store
  const unsubscribe = client.on(messageType, (data: any) => {
    webTransportState.update(state => ({
      ...state,
      lastMessage: { type: messageType, payload: data, timestamp: Date.now() },
    }));
    handler(data);
  });

  return unsubscribe;
}

/**
 * Disconnect from WebTransport
 */
export async function disconnectWebTransport(): Promise<void> {
  if (!client) {
    return;
  }

  await client.disconnect();
  client = null;
  webTransportState.set(initialState);
}

/**
 * Get WebTransport client instance (for advanced usage)
 */
export function getWebTransportClient(): WebTransportClient | null {
  return client;
}

// Export store as default
export default webTransportState;

// Derived stores for specific state properties
export const isConnected = derived(webTransportState, $state => $state.connected);
export const isConnecting = derived(webTransportState, $state => $state.connecting);
export const connectionError = derived(webTransportState, $state => $state.error);
export const lastMessage = derived(webTransportState, $state => $state.lastMessage);
