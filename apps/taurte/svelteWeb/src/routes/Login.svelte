<script lang="ts">
  import { onMount } from 'svelte';
  import { navigate } from 'svelte-routing';
  import { authStore } from '../stores/auth';
  import { login } from '../lib/auth';

  let isLogging = false;

  async function handleLogin() {
    try {
      isLogging = true;
      await login();
    } catch (error) {
      console.error('Login failed:', error);
      isLogging = false;
    }
  }

  // Redirect if already authenticated
  onMount(() => {
    if (authStore.isAuthenticated) {
      navigate('/dashboard', { replace: true });
    }
  });

  // Watch for authentication changes
  $effect(() => {
    if (authStore.isAuthenticated) {
      navigate('/dashboard', { replace: true });
    }
  });
</script>

<div class="container">
  <div class="login-container">
    <div class="login-card">
      <div class="login-header">
        <h1 class="login-title">Welcome to Nexpo</h1>
        <p class="login-subtitle">Sign in to access your dashboard</p>
      </div>

      {#if authStore.error}
        <div class="alert alert-danger">
          {authStore.error}
        </div>
      {/if}

      <div class="login-content">
        <div class="login-form">
          <button 
            class="btn btn-primary login-btn"
            on:click={handleLogin}
            disabled={isLogging || authStore.isLoading}
          >
            {#if isLogging || authStore.isLoading}
              <span class="spinner-sm"></span>
              Signing in...
            {:else}
              Sign In with Auth0
            {/if}
          </button>

          <div class="login-info">
            <p class="info-text">
              This will redirect you to Auth0 for secure authentication.
              After signing in, you'll be redirected back to your dashboard.
            </p>
          </div>
        </div>

        <div class="login-features">
          <h3 class="features-title">What you get:</h3>
          <ul class="features-list">
            <li class="feature-item">
              <span class="feature-icon">🔐</span>
              Secure authentication with Auth0
            </li>
            <li class="feature-item">
              <span class="feature-icon">👤</span>
              Personalized dashboard experience
            </li>
            <li class="feature-item">
              <span class="feature-icon">🔄</span>
              Seamless integration with all Nexpo apps
            </li>
            <li class="feature-item">
              <span class="feature-icon">📱</span>
              Cross-platform synchronization
            </li>
          </ul>
        </div>
      </div>
    </div>
  </div>
</div>

<style>
  .login-container {
    display: flex;
    justify-content: center;
    align-items: center;
    min-height: calc(100vh - 200px);
    padding: 2rem 0;
  }

  .login-card {
    background-color: var(--color-surface);
    border: 1px solid var(--color-border);
    border-radius: var(--border-radius);
    box-shadow: var(--shadow-lg);
    padding: 2rem;
    max-width: 500px;
    width: 100%;
  }

  .login-header {
    text-align: center;
    margin-bottom: 2rem;
  }

  .login-title {
    font-size: 2rem;
    font-weight: bold;
    color: var(--color-text);
    margin-bottom: 0.5rem;
  }

  .login-subtitle {
    color: var(--color-text-muted);
    font-size: 1rem;
  }

  .login-content {
    display: flex;
    flex-direction: column;
    gap: 2rem;
  }

  .login-form {
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }

  .login-btn {
    width: 100%;
    padding: 0.75rem 1.5rem;
    font-size: 1rem;
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 0.5rem;
  }

  .spinner-sm {
    width: 16px;
    height: 16px;
    border: 2px solid transparent;
    border-top: 2px solid currentColor;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

  .login-info {
    background-color: var(--color-background);
    padding: 1rem;
    border-radius: var(--border-radius);
    border: 1px solid var(--color-border);
  }

  .info-text {
    color: var(--color-text-muted);
    font-size: 0.875rem;
    line-height: 1.5;
    margin: 0;
  }

  .login-features {
    border-top: 1px solid var(--color-border);
    padding-top: 2rem;
  }

  .features-title {
    font-size: 1.1rem;
    font-weight: 600;
    color: var(--color-text);
    margin-bottom: 1rem;
  }

  .features-list {
    list-style: none;
    padding: 0;
    margin: 0;
    display: flex;
    flex-direction: column;
    gap: 0.75rem;
  }

  .feature-item {
    display: flex;
    align-items: center;
    gap: 0.75rem;
    color: var(--color-text);
    font-size: 0.875rem;
  }

  .feature-icon {
    font-size: 1.25rem;
  }

  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }

  @media (max-width: 768px) {
    .login-container {
      padding: 1rem;
      min-height: calc(100vh - 150px);
    }

    .login-card {
      padding: 1.5rem;
    }

    .login-title {
      font-size: 1.5rem;
    }

    .login-content {
      gap: 1.5rem;
    }

    .login-features {
      padding-top: 1.5rem;
    }
  }
</style>
