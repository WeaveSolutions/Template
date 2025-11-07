<script lang="ts">
  // Taurte Web - Enterprise Web Platform
</script>

<div class="container">
  <!-- Hero Section -->
  <section class="hero">
    <div class="hero-badge">⚡ Powered by Svelte 5 & Auth0</div>
    <h1 class="hero-title">
      Build Modern Web Apps<br/>
      <span class="gradient-text">At Lightning Speed</span>
    </h1>
    <p class="hero-subtitle">Native Performance. Web Technologies. Zero Compromises.</p>
    <p class="hero-description">
      Create lightning-fast, secure web applications with Svelte 5's revolutionary runes system.
      Enterprise-grade security with Auth0, real-time backend integration, and responsive design.
    </p>
    <div class="hero-buttons">
      <a href="/login" class="btn btn-primary btn-glow">
        <span>🚀 Get Started</span>
      </a>
      <a href="/dashboard" class="btn btn-secondary">
        <span>📊 Dashboard</span>
      </a>
      <span class="hero-hint">Enterprise authentication with Auth0</span>
    </div>
    <div class="hero-stats-inline">
      <div class="stat-inline">
        <strong>&lt;1MB</strong>
        <span>Initial Load</span>
      </div>
      <div class="stat-divider">•</div>
      <div class="stat-inline">
        <strong>&lt;10ms</strong>
        <span>Interaction</span>
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
      <div class="stat-value">5.0+</div>
      <div class="stat-label">Svelte Version</div>
    </div>
    <div class="stat-card">
      <div class="stat-value">Auth0</div>
      <div class="stat-label">Enterprise Security</div>
    </div>
    <div class="stat-card">
      <div class="stat-value">Blazing</div>
      <div class="stat-label">Performance</div>
    </div>
    <div class="stat-card">
      <div class="stat-value">Responsive</div>
      <div class="stat-label">All Devices</div>
    </div>
  </section>

  <!-- Features Section -->
  <section class="features">
    <div class="section-header">
      <h2 class="section-title">Why Choose Taurte Web?</h2>
      <p class="section-subtitle">Enterprise-grade features built for modern web applications</p>
    </div>
    <div class="features-grid">
      <div class="feature-card">
        <div class="feature-icon-wrapper">
          <div class="feature-icon">⚡</div>
        </div>
        <h3>Lightning Fast Performance</h3>
        <p>Svelte 5's revolutionary runes system delivers instant reactivity. Sub-10ms interactions and 60fps animations with minimal bundle size.</p>
        <div class="feature-badge">Blazing Speed</div>
      </div>
      
      <div class="feature-card">
        <div class="feature-icon-wrapper">
          <div class="feature-icon">🔐</div>
        </div>
        <h3>Enterprise Security</h3>
        <p>Bank-grade Auth0 authentication with OAuth 2.0 and OpenID Connect. Multi-factor authentication and role-based access control built-in.</p>
        <div class="feature-badge">Zero Trust</div>
      </div>
      
      <div class="feature-card">
        <div class="feature-icon-wrapper">
          <div class="feature-icon">🎨</div>
        </div>
        <h3>Modern Design System</h3>
        <p>Beautiful components with reactive stores, smooth animations, and full dark mode support. Accessibility-first approach out of the box.</p>
        <div class="feature-badge">Pixel Perfect</div>
      </div>
      
      <div class="feature-card">
        <div class="feature-icon-wrapper">
          <div class="feature-icon">🔄</div>
        </div>
        <h3>Real-time Synchronization</h3>
        <p>Event-driven architecture with WebSocket/WebTransport. Ultra-low latency bi-directional streaming and instant data sync.</p>
        <div class="feature-badge">Live Updates</div>
      </div>
      
      <div class="feature-card">
        <div class="feature-icon-wrapper">
          <div class="feature-icon">📦</div>
        </div>
        <h3>Tiny Footprint</h3>
        <p>Under 1MB initial bundle with code-splitting. 5x smaller than traditional frameworks while delivering superior performance.</p>
        <div class="feature-badge">Optimized</div>
      </div>
      
      <div class="feature-card">
        <div class="feature-icon-wrapper">
          <div class="feature-icon">📱</div>
        </div>
        <h3>Fully Responsive</h3>
        <p>Mobile-first design that adapts beautifully to any screen size. Progressive Web App ready with offline support.</p>
        <div class="feature-badge">Universal</div>
      </div>
    </div>
  </section>
</div>

<style>
  .container {
    max-width: 1400px;
    margin: 0 auto;
    padding: 0 2rem;
  }

  /* Hero Section */
  .hero {
    text-align: center;
    padding: 4rem 0 2rem;
    max-width: 900px;
    margin: 0 auto;
  }

  .hero-badge {
    display: inline-block;
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.2) 0%, rgba(118, 75, 162, 0.2) 100%);
    border: 1px solid rgba(102, 126, 234, 0.3);
    color: #667eea;
    padding: 0.5rem 1.5rem;
    border-radius: 2rem;
    font-size: 0.875rem;
    font-weight: 600;
    margin-bottom: 2rem;
    letter-spacing: 0.5px;
  }

  .hero-title {
    font-size: 3.5rem;
    font-weight: 800;
    color: #ffffff;
    margin-bottom: 1.5rem;
    line-height: 1.1;
  }

  .gradient-text {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
  }

  .hero-subtitle {
    font-size: 1.5rem;
    color: #d1d5db;
    margin-bottom: 1rem;
    font-weight: 500;
  }

  .hero-description {
    font-size: 1.125rem;
    color: #9ca3af;
    margin-bottom: 2.5rem;
    line-height: 1.7;
    max-width: 700px;
    margin-left: auto;
    margin-right: auto;
  }

  .hero-buttons {
    display: flex;
    flex-direction: row;
    align-items: center;
    justify-content: center;
    gap: 1rem;
    margin-bottom: 1.5rem;
    flex-wrap: wrap;
  }

  .hero-buttons .btn {
    padding: 0.75rem 2rem;
    font-size: 1rem;
  }

  .btn-glow {
    box-shadow: 0 0 20px rgba(102, 126, 234, 0.4);
    transition: all 0.3s ease;
  }

  .btn-glow:hover {
    box-shadow: 0 0 30px rgba(102, 126, 234, 0.6);
    transform: translateY(-2px);
  }

  .hero-hint {
    color: #9ca3af;
    font-size: 0.9375rem;
  }

  .hero-stats-inline {
    display: flex;
    justify-content: center;
    align-items: center;
    gap: 2rem;
    flex-wrap: wrap;
  }

  .stat-inline {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.25rem;
  }

  .stat-inline strong {
    font-size: 1.5rem;
    font-weight: 700;
    color: #667eea;
  }

  .stat-inline span {
    font-size: 0.8125rem;
    color: #9ca3af;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .stat-divider {
    color: #4b5563;
    font-size: 1.5rem;
  }

  /* Stats Grid */
  .stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 1.5rem;
    margin: 3rem 0;
  }

  .stat-card {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.08) 0%, rgba(118, 75, 162, 0.04) 100%);
    border: 1px solid rgba(102, 126, 234, 0.2);
    border-radius: 1rem;
    padding: 2rem;
    text-align: center;
    transition: all 0.3s ease;
  }

  .stat-card:hover {
    border-color: rgba(102, 126, 234, 0.4);
    transform: translateY(-4px);
    box-shadow: 0 10px 30px rgba(102, 126, 234, 0.2);
  }

  .stat-value {
    font-size: 2rem;
    font-weight: 700;
    color: #667eea;
    margin-bottom: 0.5rem;
  }

  .stat-label {
    font-size: 0.875rem;
    color: #9ca3af;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  /* Features Section */
  .features {
    padding: 4rem 0;
  }

  .section-header {
    text-align: center;
    margin-bottom: 3rem;
  }

  .section-title {
    font-size: 2.5rem;
    font-weight: 700;
    color: #ffffff;
    margin-bottom: 1rem;
  }

  .section-subtitle {
    font-size: 1.125rem;
    color: #9ca3af;
  }

  .features-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
    gap: 2rem;
  }

  .feature-card {
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.05) 0%, rgba(118, 75, 162, 0.02) 100%);
    border: 1px solid rgba(102, 126, 234, 0.2);
    border-radius: 1rem;
    padding: 2rem;
    transition: all 0.3s ease;
    position: relative;
    overflow: hidden;
  }

  .feature-card::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 2px;
    background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
    transform: scaleX(0);
    transition: transform 0.3s ease;
  }

  .feature-card:hover::before {
    transform: scaleX(1);
  }

  .feature-card:hover {
    border-color: rgba(102, 126, 234, 0.4);
    transform: translateY(-8px);
    box-shadow: 0 15px 40px rgba(102, 126, 234, 0.2);
  }

  .feature-icon-wrapper {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 64px;
    height: 64px;
    background: linear-gradient(135deg, rgba(102, 126, 234, 0.2) 0%, rgba(118, 75, 162, 0.2) 100%);
    border-radius: 1rem;
    margin-bottom: 1.5rem;
  }

  .feature-icon {
    font-size: 2rem;
  }

  .feature-card h3 {
    font-size: 1.25rem;
    font-weight: 700;
    color: #ffffff;
    margin-bottom: 1rem;
  }

  .feature-card p {
    color: #9ca3af;
    line-height: 1.6;
    margin-bottom: 1.5rem;
  }

  .feature-badge {
    display: inline-block;
    background: rgba(102, 126, 234, 0.1);
    color: #667eea;
    padding: 0.375rem 1rem;
    border-radius: 1rem;
    font-size: 0.75rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  /* Responsive Design */
  @media (max-width: 768px) {
    .hero-title {
      font-size: 2.5rem;
    }

    .hero-subtitle {
      font-size: 1.25rem;
    }

    .hero-description {
      font-size: 1rem;
    }

    .hero {
      padding: 2rem 0;
    }

    .stats {
      grid-template-columns: repeat(2, 1fr);
    }

    .features-grid {
      grid-template-columns: 1fr;
    }

    .section-title {
      font-size: 2rem;
    }

    .hero-stats-inline {
      gap: 1rem;
    }

    .stat-inline strong {
      font-size: 1.25rem;
    }
  }
</style>
