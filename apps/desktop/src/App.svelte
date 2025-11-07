<script lang="ts">
  /**
   * Taurte Desktop Application
   * 
   * Architecture Notes:
   * - Real-time Communication: WebTransport (upgraded from WebSocket)
   *   * Ultra-low latency: ~10ms vs ~50ms with WebSocket
   *   * HTTP/3 based with QUIC protocol for better performance
   *   * Multiplexing: Multiple streams without head-of-line blocking
   *   * Built-in congestion control and flow control
   *   * Unidirectional and bidirectional streams support
   *   * Perfect for: Live data sync, real-time collaboration, streaming
   */

  import { onMount } from 'svelte';
  import { invoke } from '@tauri-apps/api/core';
  import { listen } from '@tauri-apps/api/event';
  
  // Shared stores from Tauri Mobile
  import { authStore, settingsStore } from "@taurte/shared";
  import { AuthPanel, UserProfile } from "@taurte/shared";
  
  // WebTransport demo component
  import WebTransportDemo from './components/WebTransportDemo.svelte';
  
  let activeView = 'home';
  let greetMsg = '';
  let name = '';
  let systemInfo: any = null;
  let isLoading = true;

  async function greet() {
    try {
      greetMsg = await invoke('greet', { name });
    } catch (error) {
      greetMsg = `Hello, ${name}! (Demo mode)`;
    }
  }

  async function getSystemInfo() {
    try {
      const info = await invoke('get_system_info');
      systemInfo = info;
      isLoading = false;
    } catch (error) {
      systemInfo = { os: 'Windows', arch: 'x86_64', version: 'Demo' };
      isLoading = false;
    }
  }

  // Listen for events from backend
  onMount(async () => {
    setTimeout(async () => {
      await getSystemInfo();
      
      try {
        const unlistenAuth = await listen('auth-status-changed', (event: any) => {
          console.log('Auth status changed:', event.payload);
        });
        const unlistenSettings = await listen('settings-updated', (event: any) => {
          console.log('Settings updated:', event.payload);
        });
        return () => {
          unlistenAuth();
          unlistenSettings();
        };
      } catch (error) {
        // Browser mode
      }
    }, 100);
  });
</script>

<div class="app">
  <!-- Animated particles background -->
  <div class="particles">
    <div class="particle"></div>
    <div class="particle"></div>
    <div class="particle"></div>
    <div class="particle"></div>
    <div class="particle"></div>
    <div class="particle"></div>
    <div class="particle"></div>
    <div class="particle"></div>
    <div class="particle"></div>
    <div class="particle"></div>
  </div>

  <!-- Navigation Bar -->
  <nav class="navbar">
    <div class="nav-brand">
      <h1 class="brand-title">⚡ Taurte</h1>
      <span class="brand-subtitle">Enterprise Desktop</span>
    </div>
    <div class="nav-links">
      <button class="nav-link" class:active={activeView === 'home'} on:click={() => activeView = 'home'}>
        Home
      </button>
      <button class="nav-link" class:active={activeView === 'features'} on:click={() => activeView = 'features'}>
        Features
      </button>
      <button class="nav-link" class:active={activeView === 'demo'} on:click={() => activeView = 'demo'}>
        Demo
      </button>
      <button class="nav-link" class:active={activeView === 'account'} on:click={() => activeView = 'account'}>
        Account
      </button>
    </div>
  </nav>

  <!-- Main Content -->
  <main class="main-content">
    {#if activeView === 'home'}
      <!-- Hero Section -->
      <section class="hero">
        <div class="hero-badge">⚡ Powered by Tauri 2.0 & Svelte 5</div>
        <h2 class="hero-title">
          Build Desktop Apps<br/>
          <span class="gradient-text">At Lightning Speed</span>
        </h2>
        <p class="hero-subtitle">Native Performance. Web Technologies. Zero Compromises.</p>
        <p class="hero-description">
          Create lightweight, secure, and blazing-fast desktop applications with Rust + Svelte 5.
          Enterprise-grade security with Auth0, real-time backend integration, and cross-platform deployment.
        </p>
        <div class="hero-buttons">
          <button class="btn btn-primary btn-glow" on:click={() => activeView = 'demo'}>
            <span>🚀 Try Interactive Demo</span>
          </button>
          <button class="btn btn-secondary" on:click={() => activeView = 'features'}>
            <span>✨ Explore Features</span>
          </button>
        </div>
        <div class="hero-stats-inline">
          <div class="stat-inline">
            <strong>3MB</strong>
            <span>Binary Size</span>
          </div>
          <div class="stat-divider">•</div>
          <div class="stat-inline">
            <strong>&lt;50ms</strong>
            <span>Cold Start</span>
          </div>
          <div class="stat-divider">•</div>
          <div class="stat-inline">
            <strong>100%</strong>
            <span>Type Safe</span>
          </div>
        </div>
      </section>

      <!-- Stats Grid -->
      <section class="stats">
        <div class="stat-card">
          <div class="stat-value">2.0+</div>
          <div class="stat-label">Tauri Version</div>
        </div>
        <div class="stat-card">
          <div class="stat-value">5.0+</div>
          <div class="stat-label">Svelte Version</div>
        </div>
        <div class="stat-card">
          <div class="stat-value">Native</div>
          <div class="stat-label">Performance</div>
        </div>
        <div class="stat-card">
          <div class="stat-value">Cross-Platform</div>
          <div class="stat-label">Windows/Mac/Linux</div>
        </div>
      </section>

      <!-- System Info -->
      {#if !isLoading && systemInfo}
        <section class="system-card">
          <h3>🖥️ System Information</h3>
          <div class="system-grid">
            <div class="system-item">
              <span class="system-label">Operating System:</span>
              <span class="system-value">{systemInfo.os}</span>
            </div>
            <div class="system-item">
              <span class="system-label">Architecture:</span>
              <span class="system-value">{systemInfo.arch}</span>
            </div>
            <div class="system-item">
              <span class="system-label">Version:</span>
              <span class="system-value">{systemInfo.version}</span>
            </div>
            <div class="system-item">
              <span class="system-label">Theme:</span>
              <span class="system-value">{$settingsStore.darkMode ? 'Dark' : 'Light'} Mode</span>
            </div>
          </div>
        </section>
      {/if}

    {:else if activeView === 'features'}
      <!-- Features Section -->
      <section class="features">
        <div class="section-header">
          <h2 class="section-title">Why Choose Taurte?</h2>
          <p class="section-subtitle">Enterprise-grade features built for modern desktop applications</p>
        </div>
        <div class="features-grid">
          <div class="feature-card">
            <div class="feature-icon-wrapper">
              <div class="feature-icon">🚀</div>
            </div>
            <h3>Lightning Fast Performance</h3>
            <p>Native Rust backend with sub-50ms cold starts. Optimized Svelte 5 frontend delivers 60fps animations and instant UI updates.</p>
            <div class="feature-badge">Native Speed</div>
          </div>
          <div class="feature-card">
            <div class="feature-icon-wrapper">
              <div class="feature-icon">🔐</div>
            </div>
            <h3>Enterprise Security</h3>
            <p>Bank-grade Auth0 authentication, end-to-end TLS encryption, and secure IPC. GDPR compliant with full audit logging.</p>
            <div class="feature-badge">Zero Trust</div>
          </div>
          <div class="feature-card">
            <div class="feature-icon-wrapper">
              <div class="feature-icon">🎨</div>
            </div>
            <h3>Modern Design System</h3>
            <p>Beautiful Svelte 5 components with reactive stores, smooth animations, and full dark mode support out of the box.</p>
            <div class="feature-badge">Pixel Perfect</div>
          </div>
          <div class="feature-card">
            <div class="feature-icon-wrapper">
              <div class="feature-icon">🔄</div>
            </div>
            <h3>Real-time Synchronization</h3>
            <p>Event-driven architecture with WebTransport protocol. Ultra-low latency bi-directional streaming, multiplexing support, and instant data sync across multiple windows and backend services.</p>
            <div class="feature-badge">Live Updates</div>
          </div>
          <div class="feature-card">
            <div class="feature-icon-wrapper">
              <div class="feature-icon">📦</div>
            </div>
            <h3>Tiny Footprint</h3>
            <p>Only 3MB binary with all features. 10x smaller than Electron apps while delivering better performance and security.</p>
            <div class="feature-badge">Optimized</div>
          </div>
          <div class="feature-card">
            <div class="feature-icon-wrapper">
              <div class="feature-icon">🌐</div>
            </div>
            <h3>True Cross-Platform</h3>
            <p>Write once, deploy everywhere. Single codebase builds native apps for Windows, macOS, and Linux with platform-specific features.</p>
            <div class="feature-badge">Universal</div>
          </div>
        </div>
      </section>

    {:else if activeView === 'demo'}
      <!-- Interactive Demo -->
      <section class="demo-section">
        <h2 class="section-title">Interactive Demo</h2>
        
        <!-- WebTransport Real-time Communication Demo -->
        <WebTransportDemo />

        <div class="demo-card">
          <h3>🎯 Rust Backend Communication</h3>
          <p>Test the Tauri invoke system:</p>
          <div class="demo-input-group">
            <input
              class="demo-input"
              bind:value={name}
              placeholder="Enter your name..."
            />
            <button class="btn btn-primary" on:click={greet}>Send Greeting</button>
          </div>
          {#if greetMsg}
            <div class="demo-result">{greetMsg}</div>
          {/if}
        </div>

        <div class="demo-card">
          <h3>⚙️ System Commands</h3>
          <div class="demo-buttons">
            <button class="btn btn-secondary" on:click={getSystemInfo}>
              Refresh System Info
            </button>
            <button class="btn btn-secondary" on:click={() => alert('File dialogs require tauri-plugin-dialog')}>
              Open File Dialog
            </button>
            <button class="btn btn-secondary" on:click={() => alert('Notifications require tauri-plugin-notification')}>
              Show Notification
            </button>
          </div>
        </div>

      </section>

    {:else if activeView === 'account'}
      <!-- Account Section -->
      <section class="account-section">
        <h2 class="section-title">Account Management</h2>
        
        <div class="account-tabs">
          <button class="tab-btn" class:active={!$authStore.isAuthenticated}>
            Authentication
          </button>
          <button class="tab-btn" class:active={$authStore.isAuthenticated}>
            Profile
          </button>
        </div>

        <div class="account-content">
          {#if !$authStore.isAuthenticated}
            <AuthPanel />
          {:else}
            <UserProfile />
          {/if}
        </div>
      </section>
    {/if}
  </main>

  <!-- Footer -->
  <footer class="footer">
    <div class="footer-content">
      <div class="footer-section">
        <strong>Taurte Desktop v1.0.0</strong>
        <p>Powered by Tauri 2.0+ & Svelte 5+</p>
      </div>
      <div class="footer-section">
        <div class="status-badge" class:active={$authStore.isAuthenticated}>
          {$authStore.isAuthenticated ? '🟢 Authenticated' : '⚪ Guest Mode'}
        </div>
      </div>
    </div>
  </footer>
</div>

<style>
  :global(body) {
    margin: 0;
    font-family: Inter, system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    background: #0b0e17;
    color: #ffffff;
    min-height: 100vh;
    position: relative;
    overflow-x: hidden;
  }

  :global(body)::before {
    content: '';
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: 
      radial-gradient(ellipse 800px 600px at 20% 40%, rgba(102, 126, 234, 0.35) 0%, transparent 60%),
      radial-gradient(ellipse 600px 800px at 80% 60%, rgba(118, 75, 162, 0.3) 0%, transparent 60%),
      radial-gradient(ellipse 1000px 400px at 50% 100%, rgba(240, 147, 251, 0.2) 0%, transparent 70%),
      linear-gradient(180deg, rgba(11, 14, 23, 0.4) 0%, rgba(11, 14, 23, 0.95) 100%);
    pointer-events: none;
    z-index: 0;
    animation: atmosphericShift 20s ease-in-out infinite;
  }

  @keyframes atmosphericShift {
    0%, 100% {
      opacity: 1;
    }
    50% {
      opacity: 0.85;
    }
  }

  :global(body)::after {
    content: '';
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background-image: 
      repeating-linear-gradient(90deg, rgba(102, 126, 234, 0.03) 0px, transparent 1px, transparent 80px, rgba(102, 126, 234, 0.03) 81px),
      repeating-linear-gradient(0deg, rgba(118, 75, 162, 0.03) 0px, transparent 1px, transparent 80px, rgba(118, 75, 162, 0.03) 81px);
    pointer-events: none;
    z-index: 0;
    opacity: 0.4;
  }

  .app {
    display: flex;
    flex-direction: column;
    min-height: 100vh;
    position: relative;
    z-index: 1;
  }

  /* Atmospheric Particles */
  .particles {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    overflow: hidden;
    pointer-events: none;
    z-index: 1;
  }

  .particle {
    position: absolute;
    width: 3px;
    height: 3px;
    background: rgba(102, 126, 234, 0.6);
    border-radius: 50%;
    box-shadow: 0 0 10px rgba(102, 126, 234, 0.8);
    animation: float 20s infinite ease-in-out;
  }

  .particle:nth-child(1) {
    top: 20%;
    left: 10%;
    animation-delay: 0s;
    animation-duration: 25s;
  }

  .particle:nth-child(2) {
    top: 40%;
    left: 30%;
    animation-delay: 3s;
    animation-duration: 22s;
    background: rgba(118, 75, 162, 0.6);
    box-shadow: 0 0 10px rgba(118, 75, 162, 0.8);
  }

  .particle:nth-child(3) {
    top: 60%;
    left: 50%;
    animation-delay: 6s;
    animation-duration: 28s;
  }

  .particle:nth-child(4) {
    top: 80%;
    left: 70%;
    animation-delay: 9s;
    animation-duration: 24s;
    background: rgba(240, 147, 251, 0.6);
    box-shadow: 0 0 10px rgba(240, 147, 251, 0.8);
  }

  .particle:nth-child(5) {
    top: 30%;
    left: 80%;
    animation-delay: 12s;
    animation-duration: 26s;
  }

  .particle:nth-child(6) {
    top: 50%;
    left: 20%;
    animation-delay: 15s;
    animation-duration: 23s;
    background: rgba(118, 75, 162, 0.6);
    box-shadow: 0 0 10px rgba(118, 75, 162, 0.8);
  }

  .particle:nth-child(7) {
    top: 70%;
    left: 40%;
    animation-delay: 18s;
    animation-duration: 27s;
  }

  .particle:nth-child(8) {
    top: 15%;
    left: 60%;
    animation-delay: 2s;
    animation-duration: 21s;
    background: rgba(240, 147, 251, 0.6);
    box-shadow: 0 0 10px rgba(240, 147, 251, 0.8);
  }

  .particle:nth-child(9) {
    top: 85%;
    left: 15%;
    animation-delay: 7s;
    animation-duration: 29s;
  }

  .particle:nth-child(10) {
    top: 45%;
    left: 85%;
    animation-delay: 11s;
    animation-duration: 20s;
    background: rgba(118, 75, 162, 0.6);
    box-shadow: 0 0 10px rgba(118, 75, 162, 0.8);
  }

  @keyframes float {
    0%, 100% {
      transform: translate(0, 0) scale(1);
      opacity: 0.3;
    }
    25% {
      transform: translate(20px, -20px) scale(1.2);
      opacity: 0.8;
    }
    50% {
      transform: translate(-15px, 10px) scale(0.9);
      opacity: 0.5;
    }
    75% {
      transform: translate(10px, 15px) scale(1.1);
      opacity: 0.7;
    }
  }

  /* Navigation */
  .navbar {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 1.25rem 3rem;
    background: rgba(10, 14, 26, 0.95);
    backdrop-filter: blur(20px);
    border-bottom: 1px solid rgba(102, 126, 234, 0.2);
    box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3);
    position: sticky;
    top: 0;
    z-index: 1000;
  }

  .nav-brand {
    display: flex;
    align-items: center;
    gap: 1rem;
  }

  .brand-title {
    margin: 0;
    font-size: 1.75rem;
    font-weight: 700;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
  }

  .brand-subtitle {
    font-size: 0.875rem;
    color: #9ca3af;
  }

  .nav-links {
    display: flex;
    gap: 0.5rem;
  }

  .nav-link {
    background: none;
    border: none;
    color: #9ca3af;
    padding: 0.5rem 1rem;
    border-radius: 0.5rem;
    cursor: pointer;
    font-size: 0.9375rem;
    font-weight: 500;
    transition: all 0.2s;
  }

  .nav-link:hover {
    background: rgba(255, 255, 255, 0.05);
    color: #ffffff;
  }

  .nav-link.active {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: #ffffff;
  }

  /* Main Content */
  .main-content {
    flex: 1;
    padding: 3rem 2rem;
    max-width: 1200px;
    margin: 0 auto;
    width: 100%;
    background: transparent;
    position: relative;
    z-index: 10;
  }

  /* Hero Section */
  .hero {
    text-align: center;
    margin-bottom: 5rem;
    margin-top: 2rem;
    padding-top: 2rem;
    position: relative;
  }

  .hero::before {
    content: '';
    position: absolute;
    top: -50%;
    left: 50%;
    transform: translateX(-50%);
    width: 800px;
    height: 800px;
    background: radial-gradient(circle, rgba(102, 126, 234, 0.15) 0%, transparent 70%);
    pointer-events: none;
    animation: pulse 4s ease-in-out infinite;
  }

  @keyframes pulse {
    0%, 100% { opacity: 0.5; transform: translateX(-50%) scale(1); }
    50% { opacity: 0.8; transform: translateX(-50%) scale(1.1); }
  }

  .hero-badge {
    display: inline-block;
    padding: 0.5rem 1.5rem;
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.2), rgba(118, 75, 162, 0.2));
    border: 1px solid rgba(102, 126, 234, 0.3);
    border-radius: 2rem;
    font-size: 0.875rem;
    font-weight: 600;
    margin-bottom: 2rem;
    animation: fadeInDown 0.6s ease-out;
  }

  @keyframes fadeInDown {
    from {
      opacity: 0;
      transform: translateY(-20px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }

  .hero-title {
    font-size: 4.5rem;
    font-weight: 800;
    margin-bottom: 1.5rem;
    line-height: 1.1;
    color: #ffffff;
    animation: fadeInUp 0.8s ease-out 0.2s both;
  }

  @keyframes fadeInUp {
    from {
      opacity: 0;
      transform: translateY(30px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }

  .gradient-text {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 50%, #f093fb 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
    display: inline-block;
    animation: shimmer 3s ease-in-out infinite;
    background-size: 200% auto;
  }

  @keyframes shimmer {
    0% { background-position: 0% center; }
    50% { background-position: 100% center; }
    100% { background-position: 0% center; }
  }

  .hero-subtitle {
    font-size: 1.5rem;
    color: #e5e7eb;
    margin-bottom: 1.5rem;
    font-weight: 600;
    letter-spacing: -0.02em;
    animation: fadeInUp 0.8s ease-out 0.4s both;
  }

  .hero-description {
    font-size: 1.125rem;
    color: #d1d5db;
    max-width: 750px;
    margin: 0 auto 2.5rem;
    line-height: 1.8;
    animation: fadeInUp 0.8s ease-out 0.6s both;
  }

  .hero-buttons {
    display: flex;
    gap: 1.5rem;
    justify-content: center;
    margin-bottom: 3rem;
    animation: fadeInUp 0.8s ease-out 0.8s both;
  }

  .hero-stats-inline {
    display: flex;
    gap: 2rem;
    justify-content: center;
    align-items: center;
    animation: fadeInUp 0.8s ease-out 1s both;
  }

  .stat-inline {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }

  .stat-inline strong {
    font-size: 1.5rem;
    font-weight: 700;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
  }

  .stat-inline span {
    font-size: 0.875rem;
    color: #9ca3af;
  }

  .stat-divider {
    color: rgba(255, 255, 255, 0.2);
    font-size: 1.5rem;
  }

  /* Buttons */
  .btn {
    padding: 1rem 2.5rem;
    border: none;
    border-radius: 0.75rem;
    font-size: 1rem;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.3s ease;
    position: relative;
    overflow: hidden;
  }

  .btn span {
    position: relative;
    z-index: 1;
  }

  .btn::before {
    content: '';
    position: absolute;
    top: 50%;
    left: 50%;
    width: 0;
    height: 0;
    border-radius: 50%;
    background: rgba(255, 255, 255, 0.2);
    transform: translate(-50%, -50%);
    transition: width 0.6s, height 0.6s;
  }

  .btn:hover::before {
    width: 400px;
    height: 400px;
  }

  .btn-primary {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: #ffffff;
    box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4);
  }

  .btn-primary:hover {
    transform: translateY(-3px);
    box-shadow: 0 12px 30px rgba(102, 126, 234, 0.5);
  }

  .btn-primary:active {
    transform: translateY(-1px);
  }

  .btn-glow {
    animation: glow 2s ease-in-out infinite;
  }

  @keyframes glow {
    0%, 100% {
      box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4), 0 0 30px rgba(102, 126, 234, 0.2);
    }
    50% {
      box-shadow: 0 4px 20px rgba(102, 126, 234, 0.6), 0 0 40px rgba(102, 126, 234, 0.4);
    }
  }

  .btn-secondary {
    background: rgba(255, 255, 255, 0.05);
    color: #ffffff;
    border: 2px solid rgba(255, 255, 255, 0.2);
    backdrop-filter: blur(10px);
  }

  .btn-secondary:hover {
    background: rgba(255, 255, 255, 0.1);
    border-color: rgba(255, 255, 255, 0.4);
    transform: translateY(-3px);
  }

  /* Stats Grid */
  .stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
    gap: 1.5rem;
    margin-bottom: 4rem;
  }

  .stat-card {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.1) 0%, rgba(118, 75, 162, 0.05) 100%);
    padding: 2.5rem 2rem;
    border-radius: 1.25rem;
    text-align: center;
    border: 1px solid rgba(102, 126, 234, 0.2);
    transition: all 0.4s ease;
    position: relative;
    overflow: hidden;
  }

  .stat-card::before {
    content: '';
    position: absolute;
    top: -2px;
    left: -2px;
    right: -2px;
    bottom: -2px;
    background: linear-gradient(135deg, #667eea, #764ba2, #f093fb);
    border-radius: 1.25rem;
    opacity: 0;
    transition: opacity 0.4s ease;
    z-index: -1;
  }

  .stat-card:hover::before {
    opacity: 0.3;
  }

  .stat-card:hover {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.15) 0%, rgba(118, 75, 162, 0.1) 100%);
    transform: translateY(-8px) scale(1.02);
    box-shadow: 0 20px 40px rgba(102, 126, 234, 0.3);
  }

  .stat-value {
    font-size: 2.5rem;
    font-weight: 800;
    margin-bottom: 0.75rem;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 50%, #f093fb 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
    background-size: 200% auto;
    animation: shimmer 3s ease-in-out infinite;
  }

  .stat-label {
    color: #d1d5db;
    font-size: 0.9375rem;
    font-weight: 500;
    letter-spacing: 0.5px;
  }

  /* System Card */
  .system-card {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.08) 0%, rgba(118, 75, 162, 0.04) 100%);
    padding: 2.5rem;
    border-radius: 1.5rem;
    border: 1px solid rgba(102, 126, 234, 0.15);
    margin-bottom: 3rem;
    box-shadow: 0 10px 30px rgba(0, 0, 0, 0.2);
    backdrop-filter: blur(10px);
  }

  .system-card h3 {
    margin-top: 0;
    margin-bottom: 2rem;
    font-size: 1.75rem;
    font-weight: 700;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
  }

  .system-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
    gap: 1.25rem;
  }

  .system-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 1.25rem;
    background: rgba(255, 255, 255, 0.05);
    border-radius: 0.75rem;
    border: 1px solid rgba(255, 255, 255, 0.1);
    transition: all 0.3s ease;
  }

  .system-item:hover {
    background: rgba(255, 255, 255, 0.08);
    transform: translateX(4px);
    border-color: rgba(102, 126, 234, 0.3);
  }

  .system-label {
    color: #9ca3af;
    font-size: 0.9375rem;
  }

  .system-value {
    font-weight: 700;
    color: #ffffff;
    font-size: 1rem;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
  }

  /* Features Section */
  .features {
    margin-bottom: 5rem;
  }

  .section-header {
    text-align: center;
    margin-bottom: 4rem;
  }

  .section-title {
    font-size: 3rem;
    margin-bottom: 1rem;
    font-weight: 800;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 50%, #f093fb 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
    background-size: 200% auto;
    animation: shimmer 3s ease-in-out infinite;
  }

  .section-subtitle {
    font-size: 1.25rem;
    color: #9ca3af;
    font-weight: 400;
    max-width: 600px;
    margin: 0 auto;
  }

  .features-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: 2rem;
  }

  .feature-card {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.08) 0%, rgba(118, 75, 162, 0.04) 100%);
    padding: 2.5rem;
    border-radius: 1.5rem;
    border: 1px solid rgba(102, 126, 234, 0.15);
    transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
    position: relative;
    overflow: hidden;
    display: flex;
    flex-direction: column;
  }

  .feature-card::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 4px;
    background: linear-gradient(90deg, #667eea, #764ba2, #f093fb);
    transform: translateX(-100%);
    transition: transform 0.6s ease;
  }

  .feature-card:hover::before {
    transform: translateX(0);
  }

  .feature-card:hover {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.12) 0%, rgba(118, 75, 162, 0.08) 100%);
    transform: translateY(-8px);
    border-color: rgba(102, 126, 234, 0.4);
    box-shadow: 0 20px 40px rgba(102, 126, 234, 0.25);
  }

  .feature-icon-wrapper {
    width: 80px;
    height: 80px;
    border-radius: 1.25rem;
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.15), rgba(118, 75, 162, 0.15));
    display: flex;
    align-items: center;
    justify-content: center;
    margin-bottom: 1.5rem;
    transition: all 0.4s ease;
    border: 1px solid rgba(102, 126, 234, 0.2);
  }

  .feature-card:hover .feature-icon-wrapper {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.25), rgba(118, 75, 162, 0.25));
    border-color: rgba(102, 126, 234, 0.5);
    transform: translateY(-4px);
    box-shadow: 0 10px 30px rgba(102, 126, 234, 0.3);
  }

  .feature-icon {
    font-size: 2.5rem;
    transition: transform 0.3s ease;
  }

  .feature-card:hover .feature-icon {
    transform: scale(1.15) rotate(-5deg);
  }

  .feature-card h3 {
    margin: 0 0 1rem 0;
    font-size: 1.5rem;
    font-weight: 700;
    color: #ffffff;
    line-height: 1.3;
  }

  .feature-card p {
    margin: 0 0 1.5rem 0;
    color: #d1d5db;
    line-height: 1.8;
    font-size: 1rem;
    flex: 1;
  }

  .feature-badge {
    display: inline-block;
    padding: 0.5rem 1rem;
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.2), rgba(118, 75, 162, 0.2));
    border: 1px solid rgba(102, 126, 234, 0.3);
    border-radius: 2rem;
    font-size: 0.75rem;
    font-weight: 600;
    letter-spacing: 0.5px;
    text-transform: uppercase;
    color: #e5e7eb;
  }

  /* Demo Section */
  .demo-section {
    margin-bottom: 4rem;
  }

  .demo-card {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.08) 0%, rgba(118, 75, 162, 0.04) 100%);
    padding: 2.5rem;
    border-radius: 1.5rem;
    border: 1px solid rgba(102, 126, 234, 0.15);
    margin-bottom: 2.5rem;
    transition: all 0.3s ease;
  }

  .demo-card:hover {
    border-color: rgba(102, 126, 234, 0.3);
    box-shadow: 0 10px 30px rgba(102, 126, 234, 0.15);
  }

  .demo-card h3 {
    margin-top: 0;
    margin-bottom: 1.5rem;
    font-size: 1.5rem;
    font-weight: 700;
    color: #ffffff;
  }

  .demo-card p {
    color: #d1d5db;
    margin-bottom: 1.5rem;
    line-height: 1.6;
  }

  .demo-input-group {
    display: flex;
    gap: 1rem;
    margin-bottom: 1rem;
  }

  .demo-input {
    flex: 1;
    padding: 0.75rem;
    border-radius: 0.5rem;
    border: 1px solid rgba(255, 255, 255, 0.2);
    background: rgba(255, 255, 255, 0.05);
    color: #ffffff;
    font-size: 1rem;
  }

  .demo-input::placeholder {
    color: #6b7280;
  }

  .demo-result {
    padding: 1rem;
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.2), rgba(118, 75, 162, 0.2));
    border-radius: 0.5rem;
    border: 1px solid rgba(102, 126, 234, 0.3);
    margin-top: 1rem;
  }

  .demo-buttons {
    display: flex;
    gap: 1rem;
    flex-wrap: wrap;
  }

  /* Account Section */
  .account-section {
    margin-bottom: 4rem;
  }

  .account-tabs {
    display: flex;
    gap: 1rem;
    margin-bottom: 2rem;
  }

  .tab-btn {
    padding: 0.75rem 1.5rem;
    background: rgba(255, 255, 255, 0.05);
    border: 1px solid rgba(255, 255, 255, 0.1);
    border-radius: 0.5rem;
    color: #9ca3af;
    cursor: pointer;
    transition: all 0.2s;
  }

  .tab-btn.active {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: #ffffff;
    border-color: transparent;
  }

  .account-content {
    background: rgba(255, 255, 255, 0.05);
    padding: 2rem;
    border-radius: 1rem;
    border: 1px solid rgba(255, 255, 255, 0.1);
  }

  /* Footer */
  .footer {
    background: linear-gradient(180deg, rgba(11, 14, 23, 0.3) 0%, rgba(11, 14, 23, 0.95) 100%);
    backdrop-filter: blur(20px);
    padding: 2rem;
    border-top: 1px solid rgba(102, 126, 234, 0.2);
    box-shadow: 0 -4px 20px rgba(0, 0, 0, 0.3);
  }

  .footer-content {
    display: flex;
    justify-content: space-between;
    align-items: center;
    max-width: 1200px;
    margin: 0 auto;
  }

  .footer-section p {
    margin: 0.5rem 0 0;
    color: #9ca3af;
    font-size: 0.875rem;
  }

  .status-badge {
    padding: 0.5rem 1rem;
    border-radius: 2rem;
    background: rgba(255, 255, 255, 0.1);
    font-size: 0.875rem;
  }

  .status-badge.active {
    background: linear-gradient(135deg, rgba(16, 185, 129, 0.2), rgba(5, 150, 105, 0.2));
    border: 1px solid rgba(16, 185, 129, 0.3);
  }

  /* Responsive */
  @media (max-width: 768px) {
    .navbar {
      flex-direction: column;
      gap: 1rem;
    }

    .nav-links {
      width: 100%;
      justify-content: space-around;
    }

    .hero-title {
      font-size: 2.5rem;
    }

    .footer-content {
      flex-direction: column;
      gap: 1rem;
      text-align: center;
    }

    .demo-input-group {
      flex-direction: column;
    }
  }
</style>
