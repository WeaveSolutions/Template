<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import {
    initWebTransport,
    sendMessage,
    onMessage,
    disconnectWebTransport,
    isConnected,
    isConnecting,
    connectionError,
  } from '../stores/webtransportStore';

  // Demo state
  let messageText = '';
  let messageType = 'chat';
  let receivedMessages: Array<{ type: string; data: any; time: string }> = [];
  let serverUrl = ''; // Leave empty - user must provide their own WebTransport server
  
  // Subscriptions
  let unsubscribers: Array<() => void> = [];

  onMount(() => {
    // Note: Subscriptions are set up after successful connection in setupSubscriptions()
  });

  onDestroy(() => {
    // Cleanup subscriptions
    unsubscribers.forEach(unsub => unsub());
    unsubscribers = [];
  });

  function setupSubscriptions() {
    // Clear any existing subscriptions
    unsubscribers.forEach(unsub => unsub());
    unsubscribers = [];

    // Subscribe to various message types
    unsubscribers.push(
      onMessage('chat', (data) => {
        addReceivedMessage('chat', data);
      })
    );
    
    unsubscribers.push(
      onMessage('notification', (data) => {
        addReceivedMessage('notification', data);
      })
    );
    
    unsubscribers.push(
      onMessage('update', (data) => {
        addReceivedMessage('update', data);
      })
    );
  }

  function addReceivedMessage(type: string, data: any) {
    const time = new Date().toLocaleTimeString();
    receivedMessages = [{ type, data, time }, ...receivedMessages].slice(0, 50); // Keep last 50
  }

  async function connect() {
    if (!serverUrl.trim()) {
      alert('Please enter a valid WebTransport server URL');
      return;
    }

    try {
      await initWebTransport(serverUrl);
      // Set up subscriptions after successful connection
      setupSubscriptions();
    } catch (error) {
      console.error('Failed to connect:', error);
    }
  }

  async function disconnect() {
    await disconnectWebTransport();
    receivedMessages = [];
  }

  async function handleSendMessage() {
    if (!messageText.trim()) return;
    
    try {
      await sendMessage(messageType, {
        text: messageText,
        user: 'desktop-user',
        timestamp: Date.now(),
      });
      
      messageText = '';
    } catch (error) {
      console.error('Failed to send message:', error);
    }
  }

  function clearMessages() {
    receivedMessages = [];
  }
</script>

<div class="webtransport-demo">
  <div class="demo-header">
    <h3>🔄 WebTransport Connection</h3>
    <div class="connection-status">
      {#if $isConnecting}
        <span class="status connecting">🔄 Connecting...</span>
      {:else if $isConnected}
        <span class="status connected">✅ Connected</span>
      {:else}
        <span class="status disconnected">⚪ Disconnected</span>
      {/if}
    </div>
  </div>

  <div class="info-banner">
    ℹ️ <strong>WebTransport Server Required:</strong> This demo requires a running WebTransport server. 
    <a href="../WEBTRANSPORT.md" target="_blank" class="info-link">See setup guide →</a>
  </div>

  {#if $connectionError}
    <div class="error-banner">
      ⚠️ Connection Error: {$connectionError.message}
      {#if $connectionError.message.includes('ERR_NAME_NOT_RESOLVED')}
        <br/><small>Server not found. Please check the URL or set up a WebTransport server.</small>
      {/if}
    </div>
  {/if}

  <div class="demo-controls">
    <div class="control-group">
      <label for="server-url">WebTransport Server URL:</label>
      <input
        id="server-url"
        type="text"
        bind:value={serverUrl}
        placeholder="https://your-server.com/wt (HTTP/3 + QUIC required)"
        disabled={$isConnected}
        class="url-input"
      />
      <small class="help-text">Enter your WebTransport server URL (must support HTTP/3 and have valid TLS 1.3+ certificate)</small>
    </div>

    <div class="action-buttons">
      {#if !$isConnected}
        <button on:click={connect} disabled={$isConnecting} class="btn btn-primary">
          {$isConnecting ? 'Connecting...' : 'Connect'}
        </button>
      {:else}
        <button on:click={disconnect} class="btn btn-danger">
          Disconnect
        </button>
      {/if}
    </div>
  </div>

  {#if $isConnected}
    <div class="message-section">
      <h4>Send Message</h4>
      <div class="message-form">
        <select bind:value={messageType} class="message-type-select">
          <option value="chat">Chat</option>
          <option value="notification">Notification</option>
          <option value="update">Update</option>
        </select>
        
        <input
          type="text"
          bind:value={messageText}
          on:keypress={(e) => e.key === 'Enter' && handleSendMessage()}
          placeholder="Type a message..."
          class="message-input"
        />
        
        <button on:click={handleSendMessage} class="btn btn-primary">
          Send
        </button>
      </div>
    </div>

    <div class="messages-section">
      <div class="messages-header">
        <h4>Received Messages ({receivedMessages.length})</h4>
        <button on:click={clearMessages} class="btn btn-small">Clear</button>
      </div>
      
      <div class="messages-list">
        {#if receivedMessages.length === 0}
          <div class="no-messages">No messages yet. Try sending one!</div>
        {:else}
          {#each receivedMessages as message}
            <div class="message-item">
              <span class="message-time">{message.time}</span>
              <span class="message-type {message.type}">{message.type}</span>
              <pre class="message-data">{JSON.stringify(message.data, null, 2)}</pre>
            </div>
          {/each}
        {/if}
      </div>
    </div>
  {:else}
    <div class="connect-prompt">
      <div class="prompt-icon">🚀</div>
      <h3>Ready to Test WebTransport</h3>
      <p>
        Enter your WebTransport server URL above and click Connect to start.
      </p>
      <div class="requirements">
        <h4>Server Requirements:</h4>
        <ul>
          <li>✅ HTTP/3 and QUIC protocol support</li>
          <li>✅ Valid TLS 1.3+ certificate (Let's Encrypt works)</li>
          <li>✅ WebTransport endpoint configured</li>
          <li>✅ CORS enabled (if needed)</li>
        </ul>
      </div>
      <p class="tech-note">
        <strong>Why WebTransport?</strong> 5x lower latency (~10ms vs ~50ms), unlimited multiplexing, 
        0-RTT reconnection, and better congestion control than WebSocket.
      </p>
    </div>
  {/if}
</div>

<style>
  .webtransport-demo {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.08) 0%, rgba(118, 75, 162, 0.04) 100%);
    border-radius: 1rem;
    padding: 2rem;
    border: 1px solid rgba(102, 126, 234, 0.2);
  }

  .demo-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1.5rem;
  }

  .demo-header h3 {
    margin: 0;
    color: #ffffff;
  }

  .connection-status {
    font-weight: 600;
  }

  .status {
    padding: 0.5rem 1rem;
    border-radius: 2rem;
    font-size: 0.875rem;
  }

  .status.connecting {
    background: rgba(255, 193, 7, 0.2);
    color: #ffc107;
    border: 1px solid rgba(255, 193, 7, 0.3);
  }

  .status.connected {
    background: rgba(76, 175, 80, 0.2);
    color: #4caf50;
    border: 1px solid rgba(76, 175, 80, 0.3);
  }

  .status.disconnected {
    background: rgba(158, 158, 158, 0.2);
    color: #9e9e9e;
    border: 1px solid rgba(158, 158, 158, 0.3);
  }

  .info-banner {
    background: rgba(33, 150, 243, 0.2);
    color: #2196f3;
    padding: 1rem;
    border-radius: 0.5rem;
    margin-bottom: 1rem;
    border: 1px solid rgba(33, 150, 243, 0.3);
  }

  .info-link {
    color: #64b5f6;
    text-decoration: underline;
    margin-left: 0.5rem;
  }

  .info-link:hover {
    color: #90caf9;
  }

  .error-banner {
    background: rgba(244, 67, 54, 0.2);
    color: #f44336;
    padding: 1rem;
    border-radius: 0.5rem;
    margin-bottom: 1rem;
    border: 1px solid rgba(244, 67, 54, 0.3);
  }

  .error-banner small {
    opacity: 0.8;
    font-size: 0.875rem;
  }

  .demo-controls {
    margin-bottom: 2rem;
  }

  .control-group {
    margin-bottom: 1rem;
  }

  .control-group label {
    display: block;
    margin-bottom: 0.5rem;
    color: #d1d5db;
    font-weight: 500;
  }

  .url-input {
    width: 100%;
    padding: 0.75rem;
    border-radius: 0.5rem;
    border: 1px solid rgba(255, 255, 255, 0.2);
    background: rgba(255, 255, 255, 0.05);
    color: #ffffff;
    font-size: 1rem;
    margin-bottom: 0.5rem;
  }

  .url-input:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .help-text {
    display: block;
    color: #9ca3af;
    font-size: 0.8125rem;
    line-height: 1.4;
  }

  .action-buttons {
    display: flex;
    gap: 1rem;
  }

  .btn {
    padding: 0.75rem 1.5rem;
    border: none;
    border-radius: 0.5rem;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.2s;
  }

  .btn-primary {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: #ffffff;
  }

  .btn-primary:hover:not(:disabled) {
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
  }

  .btn-primary:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .btn-danger {
    background: rgba(244, 67, 54, 0.8);
    color: #ffffff;
  }

  .btn-danger:hover {
    background: rgba(244, 67, 54, 1);
  }

  .btn-small {
    padding: 0.5rem 1rem;
    font-size: 0.875rem;
    background: rgba(255, 255, 255, 0.1);
    color: #ffffff;
  }

  .message-section {
    margin-bottom: 2rem;
  }

  .message-section h4 {
    margin-bottom: 1rem;
    color: #ffffff;
  }

  .message-form {
    display: flex;
    gap: 0.5rem;
  }

  .message-type-select {
    padding: 0.75rem;
    border-radius: 0.5rem;
    border: 1px solid rgba(255, 255, 255, 0.2);
    background: rgba(255, 255, 255, 0.05);
    color: #ffffff;
  }

  .message-input {
    flex: 1;
    padding: 0.75rem;
    border-radius: 0.5rem;
    border: 1px solid rgba(255, 255, 255, 0.2);
    background: rgba(255, 255, 255, 0.05);
    color: #ffffff;
  }

  .messages-section {
    background: rgba(0, 0, 0, 0.2);
    border-radius: 0.75rem;
    padding: 1.5rem;
  }

  .messages-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1rem;
  }

  .messages-header h4 {
    margin: 0;
    color: #ffffff;
  }

  .messages-list {
    max-height: 400px;
    overflow-y: auto;
  }

  .no-messages {
    text-align: center;
    color: #9ca3af;
    padding: 2rem;
  }

  .message-item {
    background: rgba(255, 255, 255, 0.05);
    padding: 1rem;
    border-radius: 0.5rem;
    margin-bottom: 0.75rem;
    border: 1px solid rgba(255, 255, 255, 0.1);
  }

  .message-time {
    font-size: 0.75rem;
    color: #9ca3af;
    margin-right: 0.5rem;
  }

  .message-type {
    display: inline-block;
    padding: 0.25rem 0.75rem;
    border-radius: 1rem;
    font-size: 0.75rem;
    font-weight: 600;
    text-transform: uppercase;
    margin-right: 0.5rem;
  }

  .message-type.chat {
    background: rgba(33, 150, 243, 0.2);
    color: #2196f3;
  }

  .message-type.notification {
    background: rgba(255, 193, 7, 0.2);
    color: #ffc107;
  }

  .message-type.update {
    background: rgba(76, 175, 80, 0.2);
    color: #4caf50;
  }

  .message-data {
    margin-top: 0.5rem;
    padding: 0.75rem;
    background: rgba(0, 0, 0, 0.3);
    border-radius: 0.375rem;
    font-size: 0.875rem;
    color: #e0e0e0;
    overflow-x: auto;
  }

  .connect-prompt {
    text-align: center;
    padding: 3rem 2rem;
    color: #d1d5db;
  }

  .prompt-icon {
    font-size: 4rem;
    margin-bottom: 1rem;
  }

  .connect-prompt h3 {
    font-size: 1.75rem;
    margin-bottom: 1rem;
    color: #ffffff;
  }

  .connect-prompt p {
    margin-bottom: 1.5rem;
    font-size: 1.125rem;
    line-height: 1.6;
  }

  .requirements {
    background: rgba(33, 150, 243, 0.1);
    border: 1px solid rgba(33, 150, 243, 0.2);
    border-radius: 0.75rem;
    padding: 1.5rem;
    margin: 2rem auto;
    max-width: 500px;
    text-align: left;
  }

  .requirements h4 {
    margin: 0 0 1rem 0;
    color: #2196f3;
    font-size: 1rem;
  }

  .requirements ul {
    list-style: none;
    padding: 0;
    margin: 0;
  }

  .requirements li {
    padding: 0.5rem 0;
    font-size: 0.9375rem;
  }

  .tech-note {
    font-size: 0.9375rem;
    color: #9ca3af;
    max-width: 600px;
    margin: 0 auto;
    line-height: 1.6;
  }

  .tech-note strong {
    color: #d1d5db;
  }
</style>
