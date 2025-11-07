# Hero Section Template

## Overview
A prominent landing section with headline, description, and call-to-action.

## Features
- Large headline
- Supporting description
- Primary/secondary CTAs
- Hero image or video background
- Social proof (logos, stats)
- Visual elements (gradients, shapes)

## Structure

```tsx
// React/Next.js Example
export default function Hero() {
  return (
    <section className="hero">
      <div className="hero-content">
        {/* Text Content */}
        <div className="hero-text">
          <div className="hero-badge">⚡ New Release v2.0</div>
          
          <h1 className="hero-title">
            Build Amazing Applications
            <span className="gradient-text">At Lightning Speed</span>
          </h1>
          
          <p className="hero-description">
            The complete full-stack template for building modern web, mobile, and desktop applications.
            Start shipping faster with production-ready code.
          </p>

          {/* CTAs */}
          <div className="hero-actions">
            <button className="btn-primary">
              Get Started Free
            </button>
            <button className="btn-secondary">
              View Demo →
            </button>
          </div>

          {/* Social Proof */}
          <div className="hero-stats">
            <div className="stat">
              <strong>10k+</strong>
              <span>Developers</span>
            </div>
            <div className="stat">
              <strong>50k+</strong>
              <span>Apps Built</span>
            </div>
            <div className="stat">
              <strong>99.9%</strong>
              <span>Uptime</span>
            </div>
          </div>
        </div>

        {/* Visual Content */}
        <div className="hero-visual">
          <img src="/hero-image.png" alt="Product Screenshot" />
          {/* Or animated graphic, video, etc. */}
        </div>
      </div>
    </section>
  );
}
```

## Styling Guidelines

```css
.hero {
  min-height: 100vh;
  display: flex;
  align-items: center;
  padding: 4rem 2rem;
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  position: relative;
  overflow: hidden;
}

.hero-content {
  max-width: 1400px;
  margin: 0 auto;
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 4rem;
  align-items: center;
}

.hero-badge {
  display: inline-block;
  padding: 0.5rem 1rem;
  background: rgba(255, 255, 255, 0.2);
  border-radius: 2rem;
  font-size: 0.875rem;
  margin-bottom: 1.5rem;
}

.hero-title {
  font-size: 3.5rem;
  font-weight: 800;
  line-height: 1.1;
  margin-bottom: 1.5rem;
}

.gradient-text {
  background: linear-gradient(135deg, #ffd700, #ff6b6b);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  display: block;
}

.hero-description {
  font-size: 1.25rem;
  color: rgba(255, 255, 255, 0.9);
  margin-bottom: 2rem;
  line-height: 1.6;
}

.hero-actions {
  display: flex;
  gap: 1rem;
  margin-bottom: 3rem;
}

.hero-stats {
  display: flex;
  gap: 3rem;
}

.stat {
  display: flex;
  flex-direction: column;
}

.stat strong {
  font-size: 2rem;
  font-weight: 700;
}

@media (max-width: 768px) {
  .hero-content {
    grid-template-columns: 1fr;
    text-align: center;
  }
  
  .hero-title {
    font-size: 2.5rem;
  }
  
  .hero-actions {
    flex-direction: column;
  }
}
```

## Props/Configuration

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `title` | `string` | - | Main headline |
| `description` | `string` | - | Supporting text |
| `primaryCTA` | `CTA` | - | Primary call-to-action |
| `secondaryCTA` | `CTA` | `null` | Secondary action |
| `badge` | `string` | `null` | Optional badge text |
| `stats` | `Stat[]` | `[]` | Social proof statistics |
| `imageSrc` | `string` | - | Hero visual path |

## Variants

### 1. Centered Hero
```tsx
<Hero layout="centered" />
```

### 2. Split Hero (Text + Image)
```tsx
<Hero layout="split" imageSrc="/hero.png" />
```

### 3. Video Background
```tsx
<Hero backgroundType="video" videoSrc="/hero.mp4" />
```

### 4. Minimal Hero
```tsx
<Hero variant="minimal" showStats={false} />
```

## Accessibility

- Semantic HTML structure
- Proper heading hierarchy
- Alt text for images
- Keyboard accessible CTAs
- Sufficient color contrast
- Reduced motion support

## Best Practices

1. Keep headline under 10 words
2. Description should be 1-2 sentences
3. Primary CTA should stand out
4. Use high-quality visuals
5. Optimize images (WebP, lazy loading)
6. A/B test different variations

## Platform Adaptations

### Web
- Full viewport height
- Parallax scrolling effects
- Video backgrounds

### Mobile
- Simplified layout
- Touch-friendly buttons
- Optimized images

### Desktop App
- Compact hero
- Focus on getting started
- Less marketing copy
