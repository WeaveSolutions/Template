<script lang="ts">
  import { Router, Route } from 'svelte-routing';
  import { onMount } from 'svelte';
  import Home from './routes/Home.svelte';
  import Dashboard from './routes/Dashboard.svelte';
  import Login from './routes/Login.svelte';
  import Navigation from './components/Navigation.svelte';
  import { authStore } from './stores/auth';
  import { initializeAuth } from './lib/auth';

  let loading = true;

  onMount(async () => {
    try {
      await initializeAuth();
    } catch (error) {
      console.error('Auth initialization failed:', error);
    } finally {
      loading = false;
    }
  });
</script>

<div class="app">
  {#if loading}
    <div class="loading">
      <div class="spinner"></div>
      <p>Loading...</p>
    </div>
  {:else}
    <Router>
      <Navigation />
      
      <main class="main-content">
        <Route path="/login" component={Login} />
        <Route path="/dashboard" component={Dashboard} />
        <Route path="/" component={Home} />
      </main>
    </Router>
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
