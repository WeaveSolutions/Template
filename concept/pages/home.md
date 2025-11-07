# Home Page Template

## Overview
The landing/home page is the first impression. Focus on clear value proposition and conversion.

## Structure

```tsx
export default function HomePage() {
  return (
    <div className="home-page">
      {/* Hero Section */}
      <Hero
        title="Welcome to Your App"
        description="Build amazing things"
        primaryCTA={{ label: 'Get Started', onClick: handleSignup }}
        secondaryCTA={{ label: 'Learn More', onClick: scrollToFeatures }}
      />

      {/* Features Section */}
      <section className="features">
        <h2>Powerful Features</h2>
        <div className="feature-grid">
          <FeatureCard
            icon="⚡"
            title="Lightning Fast"
            description="Built for speed and performance"
          />
          <FeatureCard
            icon="🔒"
            title="Secure"
            description="Enterprise-grade security"
          />
          <FeatureCard
            icon="📱"
            title="Cross-Platform"
            description="Web, mobile, and desktop"
          />
        </div>
      </section>

      {/* Social Proof */}
      <section className="social-proof">
        <h3>Trusted by thousands</h3>
        <div className="logo-strip">
          {/* Customer logos */}
        </div>
      </section>

      {/* CTA Section */}
      <section className="cta">
        <h2>Ready to get started?</h2>
        <button>Start Free Trial</button>
      </section>
    </div>
  );
}
```

## Key Sections

1. **Hero**: Grab attention immediately
2. **Features**: Showcase 3-6 key benefits
3. **Social Proof**: Logos, testimonials, stats
4. **How It Works**: 3-step process
5. **Pricing**: Clear plans (optional)
6. **FAQ**: Address objections
7. **Final CTA**: Convert visitors

## SEO Optimization

```tsx
import Head from 'next/head';

export default function HomePage() {
  return (
    <>
      <Head>
        <title>App Name - Tagline</title>
        <meta name="description" content="Brief description" />
        <meta property="og:title" content="App Name" />
        <meta property="og:description" content="Description" />
        <meta property="og:image" content="/og-image.png" />
      </Head>
      {/* Page content */}
    </>
  );
}
```

## Performance

- Lazy load below-the-fold content
- Optimize images (WebP, srcset)
- Minimize JavaScript on initial load
- Use CDN for static assets
- Implement critical CSS

## Conversion Best Practices

1. Clear value proposition in 5 seconds
2. Single primary CTA above the fold
3. Remove friction from signup
4. Mobile-first design
5. Fast load times (<3s)
6. A/B test everything
