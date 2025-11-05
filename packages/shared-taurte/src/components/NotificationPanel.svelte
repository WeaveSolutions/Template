<script lang="ts">
  import { invoke } from "@tauri-apps/api/core";
  
  let notificationTitle = "";
  let notificationBody = "";
  let isSending = false;
  let message = "";

  const sendNotification = async () => {
    if (!notificationTitle.trim() || !notificationBody.trim()) {
      showMessage("Please enter both title and message", "error");
      return;
    }

    isSending = true;
    
    try {
      await invoke("send_notification", {
        title: notificationTitle.trim(),
        body: notificationBody.trim()
      });
      
      showMessage("Notification sent successfully!", "success");
      notificationTitle = "";
      notificationBody = "";
    } catch (error) {
      console.error("Failed to send notification:", error);
      showMessage("Failed to send notification", "error");
    } finally {
      isSending = false;
    }
  };

  const sendTestNotification = async () => {
    isSending = true;
    
    try {
      await invoke("send_notification", {
        title: "Test Notification",
        body: `This is a test notification sent at ${new Date().toLocaleTimeString()}`
      });
      
      showMessage("Test notification sent!", "success");
    } catch (error) {
      console.error("Failed to send test notification:", error);
      showMessage("Failed to send test notification", "error");
    } finally {
      isSending = false;
    }
  };

  const showMessage = (text: string, type: "success" | "error") => {
    message = text;
    setTimeout(() => {
      message = "";
    }, 3000);
  };
</script>

<div class="notification-panel">
  <h2>Push Notifications</h2>
  <p class="subtitle">Test and send push notifications</p>

  {#if message}
    <div class="message success">
      <span class="icon">✓</span>
      {message}
    </div>
  {/if}

  <!-- Test Notification Section -->
  <div class="section">
    <h3>Quick Test</h3>
    <p class="section-description">Send a test notification instantly</p>
    
    <button 
      class="test-btn" 
      on:click={sendTestNotification}
      disabled={isSending}
    >
      {#if isSending}
        <span class="spinner"></span>
        Sending...
      {:else}
        Send Test Notification
      {/if}
    </button>
  </div>

  <!-- Custom Notification Section -->
  <div class="section">
    <h3>Custom Notification</h3>
    <p class="section-description">Create and send a custom notification</p>
    
    <div class="form">
      <div class="form-group">
        <label for="title">Title</label>
        <input
          type="text"
          id="title"
          bind:value={notificationTitle}
          placeholder="Notification title"
          disabled={isSending}
          maxlength="50"
        />
        <small class="char-count">{notificationTitle.length}/50</small>
      </div>

      <div class="form-group">
        <label for="body">Message</label>
        <textarea
          id="body"
          bind:value={notificationBody}
          placeholder="Notification message"
          rows="3"
          disabled={isSending}
          maxlength="200"
        ></textarea>
        <small class="char-count">{notificationBody.length}/200</small>
      </div>

      <button 
        class="send-btn" 
        on:click={sendNotification}
        disabled={isSending || !notificationTitle.trim() || !notificationBody.trim()}
      >
        {#if isSending}
          <span class="spinner"></span>
          Sending...
        {:else}
          Send Custom Notification
        {/if}
      </button>
    </div>
  </div>

  <!-- Info Section -->
  <div class="info-section">
    <div class="info-item">
      <span class="info-icon">ℹ️</span>
      <div class="info-content">
        <strong>Notification Permissions</strong>
        <p>Make sure notifications are enabled in your system settings to receive push notifications.</p>
      </div>
    </div>
    
    <div class="info-item">
      <span class="info-icon">🔔</span>
      <div class="info-content">
        <strong>Cross-Platform</strong>
        <p>Notifications work on desktop, iOS, and Android platforms with native system integration.</p>
      </div>
    </div>
  </div>
</div>

<style>
  .notification-panel h2 {
    color: #333;
    margin-bottom: 8px;
    font-size: 1.8rem;
    font-weight: 600;
  }

  .subtitle {
    color: #666;
    margin-bottom: 2rem;
    font-size: 0.9rem;
  }

  .message {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.75rem;
    margin-bottom: 1.5rem;
    border-radius: 6px;
    font-size: 0.875rem;
  }

  .message.success {
    background-color: #d1e7dd;
    color: #0f5132;
    border: 1px solid #badbcc;
  }

  .message .icon {
    font-weight: bold;
  }

  .section {
    background: #f8f9fa;
    border-radius: 8px;
    padding: 1.5rem;
    margin-bottom: 1.5rem;
  }

  .section h3 {
    color: #495057;
    font-size: 1.1rem;
    font-weight: 600;
    margin-bottom: 0.5rem;
  }

  .section-description {
    color: #6c757d;
    font-size: 0.85rem;
    margin-bottom: 1rem;
  }

  .test-btn {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.75rem 1.5rem;
    background-color: #28a745;
    color: white;
    border: none;
    border-radius: 6px;
    font-size: 0.9rem;
    font-weight: 500;
    cursor: pointer;
    transition: background-color 0.2s ease;
    width: 100%;
    justify-content: center;
  }

  .test-btn:hover:not(:disabled) {
    background-color: #218838;
  }

  .test-btn:disabled {
    opacity: 0.6;
    cursor: not-allowed;
  }

  .form {
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }

  .form-group {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }

  .form-group label {
    color: #495057;
    font-weight: 500;
    font-size: 0.9rem;
  }

  .form-group input,
  .form-group textarea {
    padding: 0.75rem;
    border: 1px solid #ced4da;
    border-radius: 4px;
    font-size: 0.9rem;
    transition: border-color 0.2s ease;
    resize: vertical;
  }

  .form-group input:focus,
  .form-group textarea:focus {
    outline: none;
    border-color: #86b7fe;
    box-shadow: 0 0 0 0.25rem rgba(13, 110, 253, 0.25);
  }

  .form-group input:disabled,
  .form-group textarea:disabled {
    background-color: #e9ecef;
    cursor: not-allowed;
  }

  .char-count {
    color: #6c757d;
    font-size: 0.75rem;
    text-align: right;
  }

  .send-btn {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.75rem 1.5rem;
    background-color: #007bff;
    color: white;
    border: none;
    border-radius: 6px;
    font-size: 0.9rem;
    font-weight: 500;
    cursor: pointer;
    transition: background-color 0.2s ease;
    justify-content: center;
  }

  .send-btn:hover:not(:disabled) {
    background-color: #0056b3;
  }

  .send-btn:disabled {
    opacity: 0.6;
    cursor: not-allowed;
  }

  .spinner {
    width: 14px;
    height: 14px;
    border: 2px solid transparent;
    border-top: 2px solid currentColor;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

  .info-section {
    background: #e7f3ff;
    border-radius: 8px;
    padding: 1.5rem;
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }

  .info-item {
    display: flex;
    gap: 0.75rem;
    align-items: flex-start;
  }

  .info-icon {
    font-size: 1.2rem;
    flex-shrink: 0;
  }

  .info-content {
    flex: 1;
  }

  .info-content strong {
    color: #0056b3;
    font-size: 0.9rem;
    display: block;
    margin-bottom: 0.25rem;
  }

  .info-content p {
    color: #495057;
    font-size: 0.8rem;
    margin: 0;
    line-height: 1.4;
  }

  @keyframes spin {
    from {
      transform: rotate(0deg);
    }
    to {
      transform: rotate(360deg);
    }
  }

  /* Dark mode support */
  :global(.dark-mode) .notification-panel {
    color: white;
  }

  :global(.dark-mode) .notification-panel h2 {
    color: white;
  }

  :global(.dark-mode) .section {
    background-color: #4a5568;
  }

  :global(.dark-mode) .section h3 {
    color: #cbd5e0;
  }

  :global(.dark-mode) .section-description {
    color: #a0aec0;
  }

  :global(.dark-mode) .form-group label {
    color: #cbd5e0;
  }

  :global(.dark-mode) .form-group input,
  :global(.dark-mode) .form-group textarea {
    background-color: #2d3748;
    border-color: #4a5568;
    color: white;
  }

  :global(.dark-mode) .char-count {
    color: #a0aec0;
  }

  :global(.dark-mode) .info-section {
    background-color: #2d3748;
  }

  :global(.dark-mode) .info-content strong {
    color: #63b3ed;
  }

  :global(.dark-mode) .info-content p {
    color: #cbd5e0;
  }
</style>
