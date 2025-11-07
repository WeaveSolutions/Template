<script lang="ts">
  import { onMount } from 'svelte';
  import { authStore } from '../stores/auth.svelte';
  import { getAccessToken } from '../lib/auth';

  let accessToken = $state('');
  let isLoadingToken = $state(false);

  // Redirect if not authenticated
  onMount(() => {
    if (!authStore.isAuthenticated) {
      window.location.href = '/login';
    }
  });

  async function handleGetToken() {
    try {
      isLoadingToken = true;
      const token = await getAccessToken();
      if (token) {
        accessToken = token;
      }
    } catch (error) {
      console.error('Failed to get access token:', error);
    } finally {
      isLoadingToken = false;
    }
  }

  function copyToken() {
    if (accessToken && navigator.clipboard) {
      navigator.clipboard.writeText(accessToken);
      alert('Token copied to clipboard!');
    }
  }
</script>

{#if authStore.isAuthenticated}
<div class="container">
  <div class="dashboard">
    <div class="dashboard-header">
      <h1 class="dashboard-title">Dashboard</h1>
      <p class="dashboard-subtitle">Welcome to your Nexpo Svelte dashboard</p>
    </div>

    <div class="dashboard-content">
      <!-- User Info Card -->
      <div class="card user-card">
        <h2 class="card-title">User Information</h2>
        
        {#if authStore.user}
          <div class="user-details">
            <div class="user-avatar">
              {#if authStore.user.picture}
                <img src={authStore.user.picture} alt="User avatar" class="avatar-img" />
              {:else}
                <div class="avatar-placeholder">
                  {authStore.user.name?.charAt(0) || authStore.user.email?.charAt(0) || 'U'}
                </div>
              {/if}
            </div>
            
            <div class="user-info">
              <div class="info-row">
                <strong>Name:</strong> 
                <span>{authStore.user.name || 'Not provided'}</span>
              </div>
              <div class="info-row">
                <strong>Email:</strong> 
                <span>{authStore.user.email || 'Not provided'}</span>
              </div>
              <div class="info-row">
                <strong>Email Verified:</strong> 
                <span class="verification-status" class:verified={authStore.user.email_verified}>
                  {authStore.user.email_verified ? '✓ Verified' : '✗ Not Verified'}
                </span>
              </div>
              <div class="info-row">
                <strong>Last Updated:</strong> 
                <span>{authStore.user.updated_at ? new Date(authStore.user.updated_at).toLocaleDateString() : 'Unknown'}</span>
              </div>
            </div>
          </div>
        {/if}
      </div>

      <!-- Access Token Card -->
      <div class="card token-card">
        <h2 class="card-title">Access Token</h2>
        <p class="card-description">
          Get your current Auth0 access token for API calls
        </p>
        
        <div class="token-actions">
          <button 
            class="btn btn-primary"
            onclick={handleGetToken}
            disabled={isLoadingToken}
          >
            {#if isLoadingToken}
              <span class="spinner-sm"></span>
              Getting Token...
            {:else}
              Get Access Token
            {/if}
          </button>
          
          {#if accessToken}
            <button class="btn btn-outline" onclick={copyToken}>
              Copy Token
            </button>
          {/if}
        </div>

        {#if accessToken}
          <div class="token-display">
            <label class="form-label">Access Token:</label>
            <textarea 
              class="form-input token-textarea"
              readonly
              value={accessToken}
              placeholder="Your access token will appear here..."
            ></textarea>
            <p class="token-info">
              This token can be used to authenticate API requests to your backend services.
            </p>
          </div>
        {/if}
      </div>

      <!-- Quick Actions Card -->
      <div class="card actions-card">
        <h2 class="card-title">Quick Actions</h2>
        
        <div class="action-grid">
          <div class="action-item">
            <div class="action-icon">🏠</div>
            <h3 class="action-title">Home</h3>
            <p class="action-description">Return to the homepage</p>
            <a href="/" class="btn btn-outline btn-sm">Go Home</a>
          </div>
          
          <div class="action-item">
            <div class="action-icon">📱</div>
            <h3 class="action-title">Mobile App</h3>
            <p class="action-description">Access the mobile version</p>
            <a href="http://localhost:19000" class="btn btn-outline btn-sm" target="_blank" rel="noopener noreferrer">
              Open Mobile
            </a>
          </div>
          
          <div class="action-item">
            <div class="action-icon">🌐</div>
            <h3 class="action-title">Web App</h3>
            <p class="action-description">Visit the Next.js web app</p>
            <a href="http://localhost:3000" class="btn btn-outline btn-sm" target="_blank" rel="noopener noreferrer">
              Open Web
            </a>
          </div>
          
          <div class="action-item">
            <div class="action-icon">🖥️</div>
            <h3 class="action-title">Desktop App</h3>
            <p class="action-description">Launch the Tauri desktop app</p>
            <a href="http://localhost:1420" class="btn btn-outline btn-sm" target="_blank" rel="noopener noreferrer">
              Open Desktop
            </a>
          </div>
        </div>
      </div>
    </div>
  </div>
</div>
{/if}

<style>
  .dashboard {
    padding: 2rem 0;
  }

  .dashboard-header {
    text-align: center;
    margin-bottom: 3rem;
  }

  .dashboard-title {
    font-size: 2.5rem;
    font-weight: bold;
    color: var(--color-text);
    margin-bottom: 0.5rem;
  }

  .dashboard-subtitle {
    color: var(--color-text-muted);
    font-size: 1.125rem;
  }

  .dashboard-content {
    display: flex;
    flex-direction: column;
    gap: 2rem;
  }

  .card-title {
    font-size: 1.5rem;
    font-weight: 600;
    color: var(--color-text);
    margin-bottom: 1rem;
  }

  .card-description {
    color: var(--color-text-muted);
    margin-bottom: 1.5rem;
  }

  /* User Card */
  .user-details {
    display: flex;
    gap: 1.5rem;
    align-items: flex-start;
  }

  .user-avatar {
    flex-shrink: 0;
  }

  .avatar-img {
    width: 80px;
    height: 80px;
    border-radius: 50%;
    object-fit: cover;
  }

  .avatar-placeholder {
    width: 80px;
    height: 80px;
    border-radius: 50%;
    background-color: var(--color-primary);
    color: white;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 2rem;
    font-weight: bold;
  }

  .user-info {
    flex: 1;
  }

  .info-row {
    display: flex;
    margin-bottom: 0.75rem;
    gap: 0.5rem;
  }

  .info-row strong {
    min-width: 120px;
    color: var(--color-text);
  }

  .info-row span {
    color: var(--color-text-muted);
  }

  .verification-status.verified {
    color: var(--color-success);
  }

  /* Token Card */
  .token-actions {
    display: flex;
    gap: 1rem;
    margin-bottom: 1.5rem;
  }

  .token-display {
    margin-top: 1.5rem;
  }

  .token-textarea {
    resize: vertical;
    min-height: 120px;
    font-family: monospace;
    font-size: 0.875rem;
  }

  .token-info {
    color: var(--color-text-muted);
    font-size: 0.875rem;
    margin-top: 0.5rem;
  }

  /* Actions Card */
  .action-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 1.5rem;
  }

  .action-item {
    background-color: var(--color-background);
    border: 1px solid var(--color-border);
    border-radius: var(--border-radius);
    padding: 1.5rem;
    text-align: center;
    transition: all 0.2s ease-in-out;
  }

  .action-item:hover {
    box-shadow: var(--shadow);
    transform: translateY(-2px);
  }

  .action-icon {
    font-size: 2rem;
    margin-bottom: 1rem;
  }

  .action-title {
    font-size: 1.125rem;
    font-weight: 600;
    color: var(--color-text);
    margin-bottom: 0.5rem;
  }

  .action-description {
    color: var(--color-text-muted);
    font-size: 0.875rem;
    margin-bottom: 1rem;
    line-height: 1.4;
  }

  .spinner-sm {
    width: 16px;
    height: 16px;
    border: 2px solid transparent;
    border-top: 2px solid currentColor;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }

  @media (max-width: 768px) {
    .dashboard {
      padding: 1rem 0;
    }

    .dashboard-title {
      font-size: 2rem;
    }

    .dashboard-header {
      margin-bottom: 2rem;
    }

    .dashboard-content {
      gap: 1.5rem;
    }

    .user-details {
      flex-direction: column;
      text-align: center;
    }

    .token-actions {
      flex-direction: column;
    }

    .action-grid {
      grid-template-columns: 1fr;
      gap: 1rem;
    }

    .action-item {
      padding: 1rem;
    }
  }
</style>
