/**
 * WebTransport Service for Taurte Desktop
 * 
 * Provides low-latency, bidirectional communication using HTTP/3 and QUIC protocol.
 * Replaces traditional WebSocket with modern WebTransport API.
 * 
 * Features:
 * - Ultra-low latency (~10ms)
 * - Multiplexed streams (no head-of-line blocking)
 * - Connection migration support
 * - 0-RTT reconnection
 * - Built-in flow control
 */

export interface WebTransportConfig {
  url: string;
  reconnect?: boolean;
  reconnectDelay?: number;
  maxReconnectAttempts?: number;
  onConnect?: () => void;
  onDisconnect?: () => void;
  onError?: (error: Error) => void;
}

export interface StreamMessage {
  type: string;
  payload: any;
  timestamp?: number;
}

export class WebTransportClient {
  private transport: WebTransport | null = null;
  private config: Required<WebTransportConfig>;
  private reconnectAttempts = 0;
  private reconnectTimer: number | null = null;
  private messageHandlers = new Map<string, Set<(data: any) => void>>();
  private streams = new Map<string, ReadableStreamDefaultReader<Uint8Array>>();
  
  constructor(config: WebTransportConfig) {
    this.config = {
      reconnect: true,
      reconnectDelay: 1000,
      maxReconnectAttempts: 5,
      onConnect: () => {},
      onDisconnect: () => {},
      onError: () => {},
      ...config,
    };
  }

  /**
   * Connect to WebTransport server
   */
  async connect(): Promise<void> {
    try {
      // Check if WebTransport is supported
      if (!('WebTransport' in window)) {
        throw new Error('WebTransport is not supported in this browser');
      }

      console.log(`[WebTransport] Connecting to ${this.config.url}`);
      
      this.transport = new WebTransport(this.config.url);
      
      // Wait for connection to be ready
      await this.transport.ready;
      
      console.log('[WebTransport] Connected successfully');
      this.reconnectAttempts = 0;
      this.config.onConnect();

      // Start listening for incoming streams
      this.listenForIncomingStreams();
      
      // Handle connection closure
      this.transport.closed
        .then(() => {
          console.log('[WebTransport] Connection closed gracefully');
          this.handleDisconnect();
        })
        .catch((error) => {
          console.error('[WebTransport] Connection closed with error:', error);
          this.handleError(error);
          this.handleDisconnect();
        });

    } catch (error) {
      console.error('[WebTransport] Connection failed:', error);
      this.handleError(error as Error);
      
      if (this.config.reconnect && this.reconnectAttempts < this.config.maxReconnectAttempts) {
        this.scheduleReconnect();
      }
    }
  }

  /**
   * Send data via bidirectional stream
   */
  async send(message: StreamMessage): Promise<void> {
    if (!this.transport) {
      throw new Error('WebTransport not connected');
    }

    try {
      const stream = await this.transport.createBidirectionalStream();
      const writer = stream.writable.getWriter();
      
      const encoder = new TextEncoder();
      const data = encoder.encode(JSON.stringify(message));
      
      await writer.write(data);
      await writer.close();
      
      console.log('[WebTransport] Message sent:', message.type);
    } catch (error) {
      console.error('[WebTransport] Failed to send message:', error);
      throw error;
    }
  }

  /**
   * Send data via unidirectional stream (client to server)
   */
  async sendUnidirectional(data: Uint8Array): Promise<void> {
    if (!this.transport) {
      throw new Error('WebTransport not connected');
    }

    try {
      const stream = await this.transport.createUnidirectionalStream();
      const writer = stream.getWriter();
      
      await writer.write(data);
      await writer.close();
      
      console.log('[WebTransport] Unidirectional stream sent');
    } catch (error) {
      console.error('[WebTransport] Failed to send unidirectional stream:', error);
      throw error;
    }
  }

  /**
   * Send datagram (unreliable, unordered)
   * Perfect for real-time updates where latest data matters most
   */
  async sendDatagram(data: Uint8Array): Promise<void> {
    if (!this.transport) {
      throw new Error('WebTransport not connected');
    }

    try {
      const writer = (this.transport.datagrams as any).writable.getWriter();
      await writer.write(data);
      writer.releaseLock();
      
      console.log('[WebTransport] Datagram sent');
    } catch (error) {
      console.error('[WebTransport] Failed to send datagram:', error);
      throw error;
    }
  }

  /**
   * Subscribe to message type
   */
  on(messageType: string, handler: (data: any) => void): () => void {
    if (!this.messageHandlers.has(messageType)) {
      this.messageHandlers.set(messageType, new Set());
    }
    
    this.messageHandlers.get(messageType)!.add(handler);
    
    // Return unsubscribe function
    return () => {
      const handlers = this.messageHandlers.get(messageType);
      if (handlers) {
        handlers.delete(handler);
      }
    };
  }

  /**
   * Listen for incoming bidirectional and unidirectional streams
   */
  private async listenForIncomingStreams(): Promise<void> {
    if (!this.transport) return;

    // Listen for bidirectional streams
    this.listenBidirectionalStreams();
    
    // Listen for unidirectional streams (server to client)
    this.listenUnidirectionalStreams();
    
    // Listen for datagrams
    this.listenDatagrams();
  }

  /**
   * Listen for bidirectional streams
   */
  private async listenBidirectionalStreams(): Promise<void> {
    if (!this.transport) return;

    try {
      const reader = this.transport.incomingBidirectionalStreams.getReader();
      
      while (true) {
        const { value: stream, done } = await reader.read();
        if (done) break;
        
        this.handleIncomingStream(stream.readable);
      }
    } catch (error) {
      console.error('[WebTransport] Error reading bidirectional streams:', error);
    }
  }

  /**
   * Listen for unidirectional streams (server to client)
   */
  private async listenUnidirectionalStreams(): Promise<void> {
    if (!this.transport) return;

    try {
      const reader = this.transport.incomingUnidirectionalStreams.getReader();
      
      while (true) {
        const { value: stream, done } = await reader.read();
        if (done) break;
        
        this.handleIncomingStream(stream);
      }
    } catch (error) {
      console.error('[WebTransport] Error reading unidirectional streams:', error);
    }
  }

  /**
   * Listen for datagrams
   */
  private async listenDatagrams(): Promise<void> {
    if (!this.transport) return;

    try {
      const reader = (this.transport.datagrams as any).readable.getReader();
      
      while (true) {
        const { value, done } = await reader.read();
        if (done) break;
        
        this.handleDatagram(value);
      }
    } catch (error) {
      console.error('[WebTransport] Error reading datagrams:', error);
    }
  }

  /**
   * Handle incoming stream data
   */
  private async handleIncomingStream(readable: ReadableStream<Uint8Array>): Promise<void> {
    try {
      const reader = readable.getReader();
      const decoder = new TextDecoder();
      let buffer = '';
      
      while (true) {
        const { value, done } = await reader.read();
        if (done) break;
        
        buffer += decoder.decode(value, { stream: true });
      }
      
      // Parse message
      const message: StreamMessage = JSON.parse(buffer);
      console.log('[WebTransport] Received message:', message.type);
      
      // Dispatch to handlers
      const handlers = this.messageHandlers.get(message.type);
      if (handlers) {
        handlers.forEach(handler => handler(message.payload));
      }
      
    } catch (error) {
      console.error('[WebTransport] Error handling stream:', error);
    }
  }

  /**
   * Handle incoming datagram
   */
  private handleDatagram(data: Uint8Array): void {
    try {
      const decoder = new TextDecoder();
      const text = decoder.decode(data);
      const message: StreamMessage = JSON.parse(text);
      
      console.log('[WebTransport] Received datagram:', message.type);
      
      // Dispatch to handlers
      const handlers = this.messageHandlers.get(message.type);
      if (handlers) {
        handlers.forEach(handler => handler(message.payload));
      }
      
    } catch (error) {
      console.error('[WebTransport] Error handling datagram:', error);
    }
  }

  /**
   * Handle disconnection
   */
  private handleDisconnect(): void {
    this.transport = null;
    this.config.onDisconnect();
    
    if (this.config.reconnect && this.reconnectAttempts < this.config.maxReconnectAttempts) {
      this.scheduleReconnect();
    }
  }

  /**
   * Handle errors
   */
  private handleError(error: Error): void {
    this.config.onError(error);
  }

  /**
   * Schedule reconnection attempt
   */
  private scheduleReconnect(): void {
    if (this.reconnectTimer) return;
    
    this.reconnectAttempts++;
    const delay = this.config.reconnectDelay * this.reconnectAttempts;
    
    console.log(`[WebTransport] Reconnecting in ${delay}ms (attempt ${this.reconnectAttempts}/${this.config.maxReconnectAttempts})`);
    
    this.reconnectTimer = window.setTimeout(() => {
      this.reconnectTimer = null;
      this.connect();
    }, delay);
  }

  /**
   * Disconnect and cleanup
   */
  async disconnect(): Promise<void> {
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }
    
    if (this.transport) {
      try {
        this.transport.close();
        await this.transport.closed;
      } catch (error) {
        console.error('[WebTransport] Error during disconnect:', error);
      }
      
      this.transport = null;
    }
    
    this.messageHandlers.clear();
    this.streams.clear();
    
    console.log('[WebTransport] Disconnected');
  }

  /**
   * Check if connected
   */
  isConnected(): boolean {
    return this.transport !== null;
  }
}

/**
 * Create a WebTransport client instance
 */
export function createWebTransport(config: WebTransportConfig): WebTransportClient {
  return new WebTransportClient(config);
}
