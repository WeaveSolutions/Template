# Footer Component Template

## Overview
A site-wide footer with links, social media, copyright, and optional newsletter signup.

## Features
- Company/Product info
- Link columns (Products, Company, Support, Legal)
- Social media icons
- Newsletter subscription
- Copyright notice
- Privacy/Terms links

## Structure

```tsx
// React/Next.js Example
export default function Footer() {
  return (
    <footer className="footer">
      <div className="footer-content">
        {/* Brand Section */}
        <div className="footer-brand">
          <img src="/logo.png" alt="Logo" />
          <p>Your tagline or mission statement here.</p>
          <div className="social-links">
            <a href="#" aria-label="Twitter">𝕏</a>
            <a href="#" aria-label="LinkedIn">in</a>
            <a href="#" aria-label="GitHub">⚡</a>
          </div>
        </div>

        {/* Link Columns */}
        <div className="footer-links">
          <div className="link-column">
            <h4>Product</h4>
            <ul>
              <li><a href="/features">Features</a></li>
              <li><a href="/pricing">Pricing</a></li>
              <li><a href="/docs">Documentation</a></li>
              <li><a href="/changelog">Changelog</a></li>
            </ul>
          </div>

          <div className="link-column">
            <h4>Company</h4>
            <ul>
              <li><a href="/about">About</a></li>
              <li><a href="/blog">Blog</a></li>
              <li><a href="/careers">Careers</a></li>
              <li><a href="/contact">Contact</a></li>
            </ul>
          </div>

          <div className="link-column">
            <h4>Support</h4>
            <ul>
              <li><a href="/help">Help Center</a></li>
              <li><a href="/community">Community</a></li>
              <li><a href="/status">Status</a></li>
            </ul>
          </div>

          <div className="link-column">
            <h4>Legal</h4>
            <ul>
              <li><a href="/privacy">Privacy</a></li>
              <li><a href="/terms">Terms</a></li>
              <li><a href="/cookies">Cookies</a></li>
            </ul>
          </div>
        </div>

        {/* Newsletter */}
        <div className="footer-newsletter">
          <h4>Stay Updated</h4>
          <form>
            <input type="email" placeholder="Enter your email" />
            <button type="submit">Subscribe</button>
          </form>
        </div>
      </div>

      {/* Bottom Bar */}
      <div className="footer-bottom">
        <p>&copy; 2025 Company Name. All rights reserved.</p>
        <div className="footer-bottom-links">
          <a href="/privacy">Privacy</a>
          <a href="/terms">Terms</a>
        </div>
      </div>
    </footer>
  );
}
```

## Styling Guidelines

```css
.footer {
  background: var(--bg-secondary);
  border-top: 1px solid var(--border-color);
  padding: 4rem 2rem 2rem;
}

.footer-content {
  max-width: 1400px;
  margin: 0 auto;
  display: grid;
  grid-template-columns: 2fr 3fr 2fr;
  gap: 3rem;
  margin-bottom: 3rem;
}

.footer-brand {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.footer-links {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 2rem;
}

.link-column ul {
  list-style: none;
  padding: 0;
}

.footer-bottom {
  max-width: 1400px;
  margin: 0 auto;
  padding-top: 2rem;
  border-top: 1px solid var(--border-color);
  display: flex;
  justify-content: space-between;
  align-items: center;
}

@media (max-width: 768px) {
  .footer-content {
    grid-template-columns: 1fr;
  }
  
  .footer-links {
    grid-template-columns: repeat(2, 1fr);
  }
}
```

## Props/Configuration

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `companyName` | `string` | - | Company name for copyright |
| `logoSrc` | `string` | - | Logo image path |
| `socialLinks` | `SocialLink[]` | `[]` | Social media URLs |
| `linkColumns` | `LinkColumn[]` | `[]` | Footer link sections |
| `showNewsletter` | `boolean` | `true` | Show/hide newsletter signup |

## Accessibility

- Use semantic `<footer>` element
- Add `aria-label` to social links
- Ensure sufficient color contrast
- Make all links keyboard accessible
- Add descriptive link text

## Platform Adaptations

### Web
- Standard footer at bottom of page
- Sticky footer if content is short

### Mobile
- Simplified, stacked layout
- Larger touch targets
- Collapsible sections

### Desktop
- May include extra navigation
- Possibly integrated with titlebar

## Best Practices

1. Keep links organized and categorized
2. Include essential legal pages
3. Make contact information easily findable
4. Optimize for SEO (sitemap links)
5. Ensure fast loading (defer non-critical content)
