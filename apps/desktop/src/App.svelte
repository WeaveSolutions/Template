<script lang="ts">
  import { onMount } from 'svelte';
  import { invoke } from '@tauri-apps/api/core';
  import { listen } from '@tauri-apps/api/event';
  
  // Shared stores from Tauri Mobile
  import { authStore, settingsStore } from "@taurte/shared";
  import { AuthPanel, UserProfile, SettingsPanel, NotificationPanel } from "@taurte/shared";
  
  let activeTab = 'auth';
  let greetMsg = '';
  let name = '';
  let systemInfo = '';

  async function greet() {
    greetMsg = await invoke('greet', { name });
  }

  async function getSystemInfo() {
    try {
      systemInfo = await invoke('get_system_info');
    } catch (error) {
      console.error('Failed to get system info:', error);
      systemInfo = 'Failed to get system info';
    }
  }

  // Listen for events from backend
  onMount(async () => {
    await getSystemInfo();
    
    // Listen for auth events
    const unlistenAuth = await listen('auth-status-changed', (event: any) => {
      console.log('Auth status changed:', event.payload);
    });

    // Listen for settings changes
    const unlistenSettings = await listen('settings-updated', (event: any) => {
      console.log('Settings updated:', event.payload);
    });

    return () => {
      unlistenAuth();
      unlistenSettings();
    };
  });

  // Reactive statement to update tab based on auth state
  $: if ($authStore.isAuthenticated && activeTab === 'auth') {
    activeTab = 'profile';
  }
</script>

<main class="container">
  <header class="header">
    <h1>Nexpo Tauri Desktop</h1>
    <p class="subtitle">Svelte Standalone Mode - Cross-Platform Unified Experience</p>
    
    <div class="system-info">
      <small>{systemInfo}</small>
    </div>
  </header>

  <nav class="nav-tabs">
    <button 
      class="tab-button {activeTab === 'auth' ? 'active' : ''}"
      class:disabled={$authStore.isAuthenticated}
      on:click={() => !$authStore.isAuthenticated && (activeTab = 'auth')}
    >
      Authentication
    </button>
    
    <button 
      class="tab-button {activeTab === 'profile' ? 'active' : ''}"
      class:disabled={!$authStore.isAuthenticated}
      on:click={() => $authStore.isAuthenticated && (activeTab = 'profile')}
    >
      Profile
    </button>
    
    <button 
      class="tab-button {activeTab === 'settings' ? 'active' : ''}"
      on:click={() => (activeTab = 'settings')}
    >
      Settings
    </button>
    
    <button 
      class="tab-button {activeTab === 'notifications' ? 'active' : ''}"
      on:click={() => (activeTab = 'notifications')}
    >
      Notifications
    </button>
  </nav>

  <section class="content">
    {#if activeTab === 'auth'}
      <AuthPanel />
    {:else if activeTab === 'profile'}
      <UserProfile />
    {:else if activeTab === 'settings'}
      <SettingsPanel />
    {:else if activeTab === 'notifications'}
      <NotificationPanel />
    {/if}
  </section>

  <!-- Demo section for desktop-specific features -->
  <section class="desktop-demo">
    <h3>Desktop Features Demo</h3>
    <div class="demo-controls">
      <div class="greet-section">
        <input
          id="greet-input"
          bind:value={name}
          placeholder="Enter a name..."
        />
        <button type="button" on:click={greet}>Greet</button>
      </div>
      {#if greetMsg}
        <p class="greet-msg">{greetMsg}</p>
      {/if}
      
      <button type="button" on:click={getSystemInfo}>Refresh System Info</button>
    </div>
  </section>

  <footer class="footer">
    <p>Tauri Desktop v2.0+ with Svelte 5+ | Shared resources with Tauri Mobile</p>
    <div class="status-indicators">
      <span class="status-item" class:active={$authStore.isAuthenticated}>
        🔐 Auth: {$authStore.isAuthenticated ? 'Connected' : 'Disconnected'}
      </span>
      <span class="status-item" class:active={$settingsStore.darkMode}>
        🌙 Theme: {$settingsStore.darkMode ? 'Dark' : 'Light'}
      </span>
    </div>
  </footer>
</main>

<style>
  .container {
    padding: 2rem;
    max-width: 800px;
    margin: 0 auto;
    font-family: Inter, system-ui, Avenir, Helvetica, Arial, sans-serif;
    min-height: 100vh;
    display: flex;
    flex-direction: column;
  }

  .header {
    text-align: center;
    margin-bottom: 2rem;
    border-bottom: 1px solid #e1e5e9;
    padding-bottom: 1rem;
  }

  .header h1 {
    color: #213547;
    font-size: 2.5rem;
    margin-bottom: 0.5rem;
    font-weight: 700;
  }

  .subtitle {
    color: #6c757d;
    font-size: 1.1rem;
    margin-bottom: 1rem;
  }

  .system-info {
    background: #f8f9fa;
    padding: 0.5rem 1rem;
    border-radius: 0.5rem;
    margin-top: 1rem;
  }

  .nav-tabs {
    display: flex;
    gap: 0.5rem;
    margin-bottom: 2rem;
    border-bottom: 2px solid #e1e5e9;
  }

  .tab-button {
    background: none;
    border: none;
    padding: 0.75rem 1.5rem;
    font-size: 1rem;
    font-weight: 500;
    color: #6c757d;
    cursor: pointer;
    border-bottom: 2px solid transparent;
    transition: all 0.2s ease;
  }

  .tab-button:hover:not(.disabled) {
    color: #495057;
    background-color: #f8f9fa;
  }

  .tab-button.active {
    color: #0d6efd;
    border-bottom-color: #0d6efd;
    background-color: #e7f3ff;
  }

  .tab-button.disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .content {
    flex: 1;
    background: #ffffff;
    border-radius: 0.75rem;
    padding: 1.5rem;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.07);
    margin-bottom: 2rem;
  }

  .desktop-demo {
    background: #f8f9fa;
    padding: 1.5rem;
    border-radius: 0.75rem;
    margin-bottom: 2rem;
  }

  .desktop-demo h3 {
    color: #495057;
    margin-bottom: 1rem;
    font-size: 1.25rem;
  }

  .demo-controls {
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }

  .greet-section {
    display: flex;
    gap: 0.5rem;
    align-items: center;
  }

  .greet-section input {
    flex: 1;
    padding: 0.5rem;
    border: 1px solid #dee2e6;
    border-radius: 0.375rem;
    font-size: 1rem;
  }

  .greet-section button,
  .demo-controls > button {
    background-color: #0d6efd;
    color: white;
    border: none;
    padding: 0.5rem 1rem;
    border-radius: 0.375rem;
    font-size: 1rem;
    cursor: pointer;
    transition: background-color 0.2s ease;
  }

  .greet-section button:hover,
  .demo-controls > button:hover {
    background-color: #0b5ed7;
  }

  .greet-msg {
    padding: 1rem;
    background-color: #d1e7dd;
    color: #0f5132;
    border-radius: 0.375rem;
    margin: 0;
  }

  .footer {
    text-align: center;
    padding-top: 1rem;
    border-top: 1px solid #e1e5e9;
    color: #6c757d;
  }

  .status-indicators {
    display: flex;
    justify-content: center;
    gap: 2rem;
    margin-top: 0.5rem;
  }

  .status-item {
    font-size: 0.875rem;
    padding: 0.25rem 0.75rem;
    border-radius: 1rem;
    background-color: #f8f9fa;
    transition: all 0.2s ease;
  }

  .status-item.active {
    background-color: #d1e7dd;
    color: #0f5132;
  }

  /* Dark mode support */
  :global(.dark-mode) .container {
    background-color: #1a1a1a;
    color: #ffffff;
  }

  :global(.dark-mode) .header h1 {
    color: #ffffff;
  }

  :global(.dark-mode) .content {
    background-color: #2d3748;
    color: #ffffff;
  }

  :global(.dark-mode) .desktop-demo {
    background-color: #4a5568;
  }

  :global(.dark-mode) .tab-button {
    color: #a0aec0;
  }

  :global(.dark-mode) .tab-button:hover:not(.disabled) {
    color: #ffffff;
    background-color: #4a5568;
  }

  :global(.dark-mode) .tab-button.active {
    color: #63b3ed;
    background-color: #2d3748;
  }

  /* Responsive design */
  @media (max-width: 768px) {
    .container {
      padding: 1rem;
    }

    .nav-tabs {
      flex-wrap: wrap;
    }

    .tab-button {
      padding: 0.5rem 1rem;
      font-size: 0.875rem;
    }

    .status-indicators {
      flex-direction: column;
      gap: 0.5rem;
    }

    .greet-section {
      flex-direction: column;
    }
  }
</style>
