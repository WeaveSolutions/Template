<script lang="ts">
  import { authStore } from '../stores/auth.svelte';
  import { logout } from '../lib/auth';

  let mobileMenuOpen = $state(false);
  let currentPath = $state(window.location.pathname);

  // Update current path on navigation
  $effect(() => {
    const handleLocationChange = () => {
      currentPath = window.location.pathname;
    };
    window.addEventListener('popstate', handleLocationChange);
    return () => window.removeEventListener('popstate', handleLocationChange);
  });

  async function handleLogout() {
    try {
      await logout();
    } catch (error) {
      console.error('Logout failed:', error);
    }
  }

  function toggleMobileMenu() {
    mobileMenuOpen = !mobileMenuOpen;
  }

  function isActive(path: string): boolean {
    return currentPath === path;
  }
</script>

<nav class="navbar">
  <div class="navbar-container">
    <div class="navbar-brand">
      <a href="/" class="brand-link">
        <div class="brand-icon">⚡</div>
        <div class="brand-content">
          <span class="brand-text">Taurte</span>
          <span class="brand-subtitle">Web Platform</span>
        </div>
      </a>
      <div class="tech-badge">Svelte 5</div>
    </div>

    <!-- Desktop Navigation -->
    <ul class="navbar-nav desktop-nav">
      <li class="nav-item">
        <a href="/" class="nav-link" class:active={isActive('/')}>
          <span class="nav-icon">🏠</span>
          <span>Home</span>
        </a>
      </li>
      
      <li class="nav-item">
        <a href="/dashboard" class="nav-link" class:active={isActive('/dashboard')}>
          <span class="nav-icon">📊</span>
          <span>Dashboard</span>
        </a>
      </li>

      <div class="nav-divider"></div>

      {#if authStore.isAuthenticated}
        <li class="nav-item user-item">
          <div class="user-profile">
            <div class="user-avatar">
              {(authStore.user?.name || authStore.user?.email || 'U').charAt(0).toUpperCase()}
            </div>
            <div class="user-details">
              <span class="user-name">
                {authStore.user?.name || authStore.user?.email || 'User'}
              </span>
              <span class="user-badge">Auth0</span>
            </div>
          </div>
          <button class="btn-logout" onclick={handleLogout}>
            <span>🚪</span>
            <span>Logout</span>
          </button>
        </li>
      {:else}
        <li class="nav-item">
          <a href="/login" class="btn-login" class:active={isActive('/login')}>
            <span>🔐</span>
            <span>Sign In</span>
          </a>
        </li>
      {/if}
    </ul>

    <!-- Mobile Menu Toggle -->
    <button class="mobile-menu-toggle" onclick={toggleMobileMenu} aria-label="Toggle menu">
      <span class="hamburger" class:open={mobileMenuOpen}></span>
    </button>
  </div>

  <!-- Mobile Navigation -->
  {#if mobileMenuOpen}
    <div class="mobile-nav" class:open={mobileMenuOpen}>
      <a href="/" class="mobile-nav-link" class:active={isActive('/')} onclick={() => mobileMenuOpen = false}>
        <span class="nav-icon">🏠</span>
        <span>Home</span>
      </a>
      <a href="/dashboard" class="mobile-nav-link" class:active={isActive('/dashboard')} onclick={() => mobileMenuOpen = false}>
        <span class="nav-icon">📊</span>
        <span>Dashboard</span>
      </a>
      {#if authStore.isAuthenticated}
        <div class="mobile-user-info">
          <div class="user-avatar">
            {(authStore.user?.name || authStore.user?.email || 'U').charAt(0).toUpperCase()}
          </div>
          <span class="user-name">
            {authStore.user?.name || authStore.user?.email || 'User'}
          </span>
        </div>
        <button class="mobile-logout-btn" onclick={handleLogout}>
          <span>🚪</span>
          <span>Logout</span>
        </button>
      {:else}
        <a href="/login" class="mobile-login-btn" onclick={() => mobileMenuOpen = false}>
          <span>🔐</span>
          <span>Sign In with Auth0</span>
        </a>
      {/if}
    </div>
  {/if}
</nav>

<style>
  /* Navbar Container */
  .navbar {
    position: sticky;
    top: 0;
    left: 0;
    right: 0;
    z-index: 1000;
    background: rgba(11, 14, 23, 0.85);
    backdrop-filter: blur(20px);
    -webkit-backdrop-filter: blur(20px);
    border-bottom: 1px solid rgba(102, 126, 234, 0.2);
    box-shadow: 0 4px 30px rgba(0, 0, 0, 0.3);
  }

  .navbar-container {
    max-width: 1400px;
    margin: 0 auto;
    padding: 0.75rem 2rem;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }

  /* Brand */
  .navbar-brand {
    display: flex;
    align-items: center;
    gap: 1rem;
  }

  .brand-link {
    text-decoration: none;
    display: flex;
    align-items: center;
    gap: 0.75rem;
    transition: transform 0.2s ease;
  }

  .brand-link:hover {
    transform: translateY(-1px);
  }

  .brand-icon {
    font-size: 2rem;
    line-height: 1;
    animation: pulse 2s infinite;
  }

  @keyframes pulse {
    0%, 100% { transform: scale(1); }
    50% { transform: scale(1.05); }
  }

  .brand-content {
    display: flex;
    flex-direction: column;
    gap: 0.125rem;
  }

  .brand-text {
    font-size: 1.5rem;
    font-weight: 800;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
    line-height: 1;
  }

  .brand-subtitle {
    font-size: 0.625rem;
    color: #9ca3af;
    letter-spacing: 0.1em;
    text-transform: uppercase;
    font-weight: 600;
  }

  .tech-badge {
    padding: 0.25rem 0.75rem;
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.2) 0%, rgba(118, 75, 162, 0.2) 100%);
    border: 1px solid rgba(102, 126, 234, 0.3);
    border-radius: 1rem;
    font-size: 0.6875rem;
    font-weight: 700;
    color: #667eea;
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }

  /* Desktop Navigation */
  .navbar-nav {
    display: flex;
    list-style: none;
    margin: 0;
    padding: 0;
    gap: 0.5rem;
    align-items: center;
  }

  .desktop-nav {
    display: none;
  }

  @media (min-width: 769px) {
    .desktop-nav {
      display: flex;
    }
  }

  .nav-item {
    display: flex;
    align-items: center;
  }

  .nav-link {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    text-decoration: none;
    color: #d1d5db;
    padding: 0.625rem 1.25rem;
    border-radius: 0.5rem;
    font-weight: 600;
    font-size: 0.9375rem;
    transition: all 0.3s ease;
    position: relative;
  }

  .nav-link .nav-icon {
    font-size: 1.125rem;
    transition: transform 0.3s ease;
  }

  .nav-link:hover {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.15) 0%, rgba(118, 75, 162, 0.15) 100%);
    color: #ffffff;
    transform: translateY(-1px);
  }

  .nav-link:hover .nav-icon {
    transform: scale(1.1);
  }

  .nav-link.active {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.25) 0%, rgba(118, 75, 162, 0.25) 100%);
    color: #667eea;
    border: 1px solid rgba(102, 126, 234, 0.3);
  }

  .nav-link.active::before {
    content: '';
    position: absolute;
    bottom: 0;
    left: 50%;
    transform: translateX(-50%);
    width: 60%;
    height: 2px;
    background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
  }

  .nav-divider {
    width: 1px;
    height: 24px;
    background: rgba(255, 255, 255, 0.1);
    margin: 0 0.5rem;
  }

  /* User Section */
  .user-item {
    display: flex;
    gap: 1rem;
    align-items: center;
  }

  .user-profile {
    display: flex;
    align-items: center;
    gap: 0.75rem;
    padding: 0.5rem 1rem;
    background: rgba(255, 255, 255, 0.05);
    border-radius: 2rem;
    border: 1px solid rgba(255, 255, 255, 0.1);
  }

  .user-avatar {
    width: 36px;
    height: 36px;
    border-radius: 50%;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    display: flex;
    align-items: center;
    justify-content: center;
    font-weight: 700;
    font-size: 0.875rem;
    color: white;
    flex-shrink: 0;
  }

  .user-details {
    display: flex;
    flex-direction: column;
    gap: 0.125rem;
  }

  .user-name {
    font-size: 0.875rem;
    color: #ffffff;
    font-weight: 600;
    line-height: 1;
  }

  .user-badge {
    font-size: 0.625rem;
    color: #667eea;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    font-weight: 600;
  }

  /* Buttons */
  .btn-login {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.625rem 1.5rem;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    text-decoration: none;
    border-radius: 0.5rem;
    font-weight: 600;
    font-size: 0.9375rem;
    transition: all 0.3s ease;
    box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3);
  }

  .btn-login:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 20px rgba(102, 126, 234, 0.4);
  }

  .btn-logout {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.5rem 1rem;
    background: rgba(239, 68, 68, 0.1);
    border: 1px solid rgba(239, 68, 68, 0.3);
    color: #ef4444;
    border-radius: 0.5rem;
    font-weight: 600;
    font-size: 0.875rem;
    cursor: pointer;
    transition: all 0.3s ease;
  }

  .btn-logout:hover {
    background: rgba(239, 68, 68, 0.2);
    border-color: rgba(239, 68, 68, 0.5);
    transform: translateY(-1px);
  }

  /* Mobile Menu Toggle */
  .mobile-menu-toggle {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 40px;
    height: 40px;
    background: rgba(255, 255, 255, 0.05);
    border: 1px solid rgba(255, 255, 255, 0.1);
    border-radius: 0.5rem;
    cursor: pointer;
    transition: all 0.3s ease;
  }

  .mobile-menu-toggle:hover {
    background: rgba(255, 255, 255, 0.1);
  }

  @media (min-width: 769px) {
    .mobile-menu-toggle {
      display: none;
    }
  }

  .hamburger {
    position: relative;
    width: 20px;
    height: 14px;
    display: flex;
    flex-direction: column;
    justify-content: space-between;
  }

  .hamburger::before,
  .hamburger::after {
    content: '';
    width: 100%;
    height: 2px;
    background: #ffffff;
    border-radius: 2px;
    transition: all 0.3s ease;
  }

  .hamburger::before {
    transform-origin: top left;
  }

  .hamburger::after {
    transform-origin: bottom left;
  }

  .hamburger.open::before {
    transform: rotate(45deg) translate(2px, -2px);
  }

  .hamburger.open::after {
    transform: rotate(-45deg) translate(2px, 2px);
  }

  /* Mobile Navigation */
  .mobile-nav {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
    padding: 1rem 2rem 1.5rem;
    background: rgba(11, 14, 23, 0.95);
    border-top: 1px solid rgba(102, 126, 234, 0.2);
    animation: slideDown 0.3s ease;
  }

  @keyframes slideDown {
    from {
      opacity: 0;
      transform: translateY(-10px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }

  @media (min-width: 769px) {
    .mobile-nav {
      display: none;
    }
  }

  .mobile-nav-link {
    display: flex;
    align-items: center;
    gap: 0.75rem;
    padding: 0.875rem 1rem;
    background: rgba(255, 255, 255, 0.03);
    border: 1px solid rgba(255, 255, 255, 0.1);
    border-radius: 0.5rem;
    color: #d1d5db;
    text-decoration: none;
    font-weight: 600;
    font-size: 1rem;
    transition: all 0.3s ease;
  }

  .mobile-nav-link:hover {
    background: rgba(102, 126, 234, 0.15);
    border-color: rgba(102, 126, 234, 0.3);
    color: #ffffff;
  }

  .mobile-nav-link.active {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.25) 0%, rgba(118, 75, 162, 0.25) 100%);
    border-color: rgba(102, 126, 234, 0.4);
    color: #667eea;
  }

  .mobile-user-info {
    display: flex;
    align-items: center;
    gap: 1rem;
    padding: 1rem;
    background: rgba(255, 255, 255, 0.05);
    border-radius: 0.75rem;
    margin-top: 0.5rem;
  }

  .mobile-logout-btn,
  .mobile-login-btn {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 0.5rem;
    padding: 0.875rem;
    border-radius: 0.5rem;
    font-weight: 600;
    font-size: 1rem;
    cursor: pointer;
    transition: all 0.3s ease;
    text-decoration: none;
    margin-top: 0.5rem;
  }

  .mobile-logout-btn {
    background: rgba(239, 68, 68, 0.1);
    border: 1px solid rgba(239, 68, 68, 0.3);
    color: #ef4444;
  }

  .mobile-logout-btn:hover {
    background: rgba(239, 68, 68, 0.2);
  }

  .mobile-login-btn {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    border: none;
  }

  .mobile-login-btn:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 20px rgba(102, 126, 234, 0.4);
  }
</style>
