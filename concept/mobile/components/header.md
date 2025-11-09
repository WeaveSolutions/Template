# Header Component Template

## Overview
A page-level header component for displaying titles, breadcrumbs, and actions.

## Features
- Page title
- Breadcrumb navigation
- Action buttons
- Subtitle/description
- Back button
- Search integration

## Structure

The page header should contain:
- **Breadcrumb Navigation**: Optional hierarchical navigation trail
- **Title Section**: Main page heading (h1) with optional subtitle
- **Back Button**: Optional navigation to previous page
- **Action Buttons**: Primary actions for the page (Export, Add, Edit, etc.)

## Styling Guidelines

- Apply consistent padding (2rem)
- Add bottom border to separate from content
- Use flexbox for horizontal layout between title and actions
- Breadcrumbs: Small font size with separator between items
- Title: Large, bold font (2rem)
- Subtitle: Secondary text color
- Action buttons: Horizontal flex with gap
- Mobile: Stack title and actions vertically
- Maintain proper spacing and alignment

## Props/Configuration

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `title` | `string` | - | Page title (required) |
| `subtitle` | `string` | `null` | Optional description |
| `breadcrumbs` | `Breadcrumb[]` | `[]` | Navigation breadcrumbs |
| `actions` | `Action[]` | `[]` | Header action buttons |
| `showBack` | `boolean` | `false` | Show back navigation button |

## Configuration Types

**Breadcrumb**: Object with label and href properties
**Action**: Object with label, onClick handler, optional variant (primary/secondary/danger), and optional icon

## Usage Examples

1. **Simple Header**: Just a title
2. **With Breadcrumbs**: Include navigation trail from home to current page
3. **With Actions**: Add Export, Add, or Edit buttons
4. **Detail Page**: Include back button for navigation
5. **With Subtitle**: Add descriptive text below title

## Accessibility

- Use semantic `<header>` element
- Proper heading hierarchy (h1 for page title)
- Aria labels for breadcrumb navigation
- Keyboard accessible buttons
- Focus management for back button

## Platform Adaptations

### Web
- Sticky header option
- Responsive layout
- SEO-friendly heading structure

### Mobile
- Simplified layout
- Touch-friendly buttons
- Native back gesture support

### Desktop
- May integrate with window titlebar
- Keyboard shortcuts
- Native menu integration
