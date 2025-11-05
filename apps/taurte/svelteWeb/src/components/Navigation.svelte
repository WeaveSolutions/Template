<script lang="ts">
  import { link } from 'svelte-routing';
  import { authStore } from '../stores/auth';
  import { logout } from '../lib/auth';

  async function handleLogout() {
    try {
      await logout();
    } catch (error) {
      console.error('Logout failed:', error);
    }
  }
</script>

<nav class="navbar">
  <div class="navbar-brand">
    <a href="/" use:link class="brand-link">
      <span class="brand-text">Nexpo</span>
      <span class="brand-subtitle">Svelte</span>
    </a>
  </div>

  <ul class="navbar-nav">
    <li class="nav-item">
      <a href="/" use:link class="nav-link">Home</a>
    </li>
    
    {#if authStore.isAuthenticated}
      <li class="nav-item">
        <a href="/dashboard" use:link class="nav-link">Dashboard</a>
      </li>
      <li class="nav-item">
        <div class="user-info">
          <span class="user-name">
            {authStore.user?.name || authStore.user?.email || 'User'}
          </span>
          <button class="btn btn-outline btn-sm" on:click={handleLogout}>
            Logout
          </button>
        </div>
      </li>
    {:else}
      <li class="nav-item">
        <a href="/login" use:link class="nav-link">Login</a>
      </li>
    {/if}
  </ul>
</nav>

<style>
  .navbar {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 1rem 2rem;
    background-color: var(--color-surface);
    border-bottom: 1px solid var(--color-border);
    box-shadow: var(--shadow-sm);
  }

  .navbar-brand {
    display: flex;
    align-items: center;
  }

  .brand-link {
    text-decoration: none;
    display: flex;
    flex-direction: column;
    align-items: flex-start;
  }

  .brand-text {
    font-size: 1.5rem;
    font-weight: bold;
    color: var(--color-primary);
    line-height: 1;
  }

  .brand-subtitle {
    font-size: 0.75rem;
    color: var(--color-text-muted);
    letter-spacing: 0.1em;
    text-transform: uppercase;
  }

  .navbar-nav {
    display: flex;
    list-style: none;
    margin: 0;
    padding: 0;
    gap: 1.5rem;
    align-items: center;
  }

  .nav-item {
    display: flex;
    align-items: center;
  }

  .nav-link {
    text-decoration: none;
    color: var(--color-text);
    padding: 0.5rem 1rem;
    border-radius: var(--border-radius);
    transition: all 0.2s ease-in-out;
  }

  .nav-link:hover {
    background-color: var(--color-primary);
    color: white;
  }

  .user-info {
    display: flex;
    align-items: center;
    gap: 0.75rem;
  }

  .user-name {
    font-size: 0.875rem;
    color: var(--color-text);
    font-weight: 500;
  }

  .btn-sm {
    padding: 0.25rem 0.75rem;
    font-size: 0.75rem;
  }

  @media (max-width: 768px) {
    .navbar {
      padding: 1rem;
      flex-direction: column;
      gap: 1rem;
    }

    .navbar-nav {
      gap: 1rem;
    }

    .user-info {
      flex-direction: column;
      gap: 0.5rem;
    }
  }
</style>
