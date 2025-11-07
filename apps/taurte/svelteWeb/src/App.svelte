<script lang="ts">
  import { onMount } from 'svelte';
  import Home from './routes/Home.svelte';
  import Dashboard from './routes/Dashboard.svelte';
  import Login from './routes/Login.svelte';
  import Navigation from './components/Navigation.svelte';
  import { initializeAuth } from './lib/auth';

  let loading = $state(true);
  let currentPath = $state(window.location.pathname);

  // Simple router for Svelte 5
  function navigate(path: string) {
    window.history.pushState({}, '', path);
    currentPath = path;
  }

  // Handle browser back/forward
  function handlePopState() {
    currentPath = window.location.pathname;
  }

  onMount(() => {
    window.addEventListener('popstate', handlePopState);
    
    // Initialize auth asynchronously
    initializeAuth()
      .catch(error => console.error('Auth initialization failed:', error))
      .finally(() => loading = false);

    return () => {
      window.removeEventListener('popstate', handlePopState);
    };
  });
</script>

<svelte:window on:click={(e) => {
  const target = e.target as HTMLElement;
  const anchor = target.closest('a');
  if (anchor && anchor.href && anchor.href.startsWith(window.location.origin)) {
    e.preventDefault();
    const path = new URL(anchor.href).pathname;
    navigate(path);
  }
}} />

<div class="app">
  {#if loading}
    <div class="loading">
      <div class="spinner"></div>
      <p>Loading...</p>
    </div>
  {:else}
    <Navigation />
    
    <main class="main-content">
      {#if currentPath === '/login'}
        <Login />
      {:else if currentPath === '/dashboard'}
        <Dashboard />
      {:else}
        <Home />
      {/if}
    </main>
  {/if}
</div>

<style>
  .app {
    min-height: 100vh;
    display: flex;
    flex-direction: column;
  }

  .loading {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 100vh;
    gap: 1rem;
  }

  .spinner {
    width: 40px;
    height: 40px;
    border: 4px solid #f3f3f3;
    border-top: 4px solid #646cff;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }

  .main-content {
    flex: 1;
    padding: 2rem;
  }

  /* Global styles */
  :global(body) {
    margin: 0;
    padding: 0;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  }
</style>
