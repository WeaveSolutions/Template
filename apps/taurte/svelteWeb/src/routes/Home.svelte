<script lang="ts">
  import { link } from 'svelte-routing';
  import { authStore } from '../stores/auth';
</script>

<div class="container">
  <div class="hero">
    <h1 class="hero-title">Welcome to Nexpo Svelte</h1>
    <p class="hero-subtitle">
      A modern Svelte 5 application with Auth0 integration and shared components
    </p>
    
    <div class="hero-actions">
      {#if authStore.isAuthenticated}
        <a href="/dashboard" use:link class="btn btn-primary">
          Go to Dashboard
        </a>
        <div class="welcome-message">
          <span>Welcome back, {authStore.user?.name || authStore.user?.email}!</span>
        </div>
      {:else}
        <a href="/login" use:link class="btn btn-primary">
          Get Started
        </a>
        <span class="login-prompt">Login to access your dashboard</span>
      {/if}
    </div>
  </div>

  <div class="features">
    <div class="feature-grid">
      <div class="feature-card">
        <div class="feature-icon">⚡</div>
        <h3 class="feature-title">Svelte 5</h3>
        <p class="feature-description">
          Built with the latest Svelte 5 and its new runes system for reactive state management
        </p>
      </div>

      <div class="feature-card">
        <div class="feature-icon">🔐</div>
        <h3 class="feature-title">Auth0 Integration</h3>
        <p class="feature-description">
          Secure authentication with Auth0's industry-leading identity platform
        </p>
      </div>

      <div class="feature-card">
        <div class="feature-icon">🧩</div>
        <h3 class="feature-title">Shared Components</h3>
        <p class="feature-description">
          Leverages shared components from the Nexpo monorepo for consistency
        </p>
      </div>

      <div class="feature-card">
        <div class="feature-icon">⚡</div>
        <h3 class="feature-title">Vite Powered</h3>
        <p class="feature-description">
          Lightning-fast development with Vite's hot module replacement
        </p>
      </div>

      <div class="feature-card">
        <div class="feature-icon">📱</div>
        <h3 class="feature-title">Responsive Design</h3>
        <p class="feature-description">
          Mobile-first design that works perfectly on all device sizes
        </p>
      </div>

      <div class="feature-card">
        <div class="feature-icon">🎨</div>
        <h3 class="feature-title">Modern UI</h3>
        <p class="feature-description">
          Clean, modern interface with dark mode support and smooth animations
        </p>
      </div>
    </div>
  </div>
</div>

<style>
  .hero {
    text-align: center;
    padding: 4rem 0;
    max-width: 800px;
    margin: 0 auto;
  }

  .hero-title {
    font-size: 3rem;
    font-weight: bold;
    color: var(--color-text);
    margin-bottom: 1rem;
    line-height: 1.2;
  }

  .hero-subtitle {
    font-size: 1.25rem;
    color: var(--color-text-muted);
    margin-bottom: 2rem;
    line-height: 1.5;
  }

  .hero-actions {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 1rem;
  }

  .welcome-message {
    padding: 1rem;
    background-color: var(--color-surface);
    border: 1px solid var(--color-border);
    border-radius: var(--border-radius);
    color: var(--color-text);
  }

  .login-prompt {
    color: var(--color-text-muted);
    font-size: 0.875rem;
  }

  .features {
    padding: 4rem 0;
  }

  .feature-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: 2rem;
  }

  .feature-card {
    background-color: var(--color-surface);
    border: 1px solid var(--color-border);
    border-radius: var(--border-radius);
    padding: 2rem;
    text-align: center;
    box-shadow: var(--shadow-sm);
    transition: all 0.2s ease-in-out;
  }

  .feature-card:hover {
    box-shadow: var(--shadow);
    transform: translateY(-2px);
  }

  .feature-icon {
    font-size: 3rem;
    margin-bottom: 1rem;
  }

  .feature-title {
    font-size: 1.25rem;
    font-weight: bold;
    color: var(--color-text);
    margin-bottom: 1rem;
  }

  .feature-description {
    color: var(--color-text-muted);
    line-height: 1.5;
  }

  @media (max-width: 768px) {
    .hero-title {
      font-size: 2rem;
    }

    .hero-subtitle {
      font-size: 1rem;
    }

    .hero {
      padding: 2rem 0;
    }

    .features {
      padding: 2rem 0;
    }

    .feature-grid {
      grid-template-columns: 1fr;
      gap: 1rem;
    }

    .feature-card {
      padding: 1.5rem;
    }
  }
</style>
