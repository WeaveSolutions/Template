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

The footer component should be organized into:
- **Brand Section**: Logo, tagline, and social media links
- **Link Columns**: Organized by category (Product, Company, Support, Legal)
- **Newsletter Section**: Email signup form (optional)
- **Bottom Bar**: Copyright notice and legal links

## Styling Guidelines

- Use secondary background color to differentiate from main content
- Add top border to separate from page content
- Apply generous padding (4rem top, 2rem sides)
- Use CSS Grid for responsive column layout
- On desktop: 3-column layout (brand, links, newsletter)
- Link columns: 4-column grid for organized navigation
- On mobile: Stack to single column, links in 2-column grid
- Bottom bar: Flexbox with space-between for copyright and links
- Ensure proper contrast for accessibility

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
