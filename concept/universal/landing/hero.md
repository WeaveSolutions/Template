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

The hero section should include:
- **Text Content**:
  - Optional badge ("New Release", "Beta", etc.)
  - Large headline with key value proposition
  - Supporting description (1-2 sentences)
  - Primary and secondary CTA buttons
  - Social proof statistics or logos
- **Visual Content**:
  - Hero image, product screenshot, or video
  - Can be illustration, animation, or graphic

## Styling Guidelines

- Full viewport height (min-height: 100vh)
- Centered content with flexbox/grid
- Eye-catching gradient or solid background
- Large, bold typography for headline (3.5rem on desktop)
- Gradient text effect for emphasis
- Two-column layout (text + visual) on desktop
- Generous spacing between elements
- Stats displayed horizontally with gaps
- Mobile: Stack to single column, center text, smaller fonts

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

1. **Centered Hero**: All content centered, no side image
2. **Split Hero**: Text on one side, image/visual on the other
3. **Video Background**: Full-width video with overlaid text
4. **Minimal Hero**: Simple headline and CTA, no extra elements
5. **Animated Hero**: Moving graphics or illustrations

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
