# Navigation Component Template

## Overview
A responsive navigation bar that adapts to desktop, tablet, and mobile viewports.

## Features
- Logo/Brand
- Menu items (desktop/mobile)
- User authentication state
- Dropdown menus
- Mobile hamburger menu
- Search bar (optional)
- Call-to-action buttons

## Structure

The navigation component should include:
- Container with branding (logo and app name)
- Desktop menu with primary navigation links
- User action buttons (login/logout based on auth state)
- Mobile hamburger toggle button
- Collapsible mobile menu overlay

## Styling Guidelines

- Use sticky positioning at top of viewport
- Implement backdrop blur for glassmorphism effect
- Ensure proper z-index layering
- Center content with max-width container
- Use flexbox for horizontal layout
- Hide desktop menu on mobile breakpoint (<768px)
- Add smooth transitions for mobile menu
- Maintain consistent spacing and alignment

## Props/Configuration

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `items` | `NavItem[]` | `[]` | Navigation menu items |
| `user` | `User \| null` | `null` | Current user object |
| `onLogin` | `() => void` | - | Login handler |
| `onLogout` | `() => void` | - | Logout handler |
| `logoSrc` | `string` | - | Logo image path |

## Accessibility

- Use semantic `<nav>` element
- Add `aria-label` to navigation
- Ensure keyboard navigation works
- Add `aria-expanded` to mobile toggle
- Use proper heading hierarchy

## Platform Adaptations

### Web (React/Next.js)
- Use Next.js Link for routing
- Implement client-side navigation
- Add loading states

### Mobile (React Native/Expo)
- Use drawer navigation
- Implement native gestures
- Add haptic feedback

### Desktop (Tauri/Electron)
- Consider window controls
- Add keyboard shortcuts
- Implement native menus

## Examples

See implementations:
- `/apps/nexpo/nextWeb/components/Navigation.tsx`
- `/apps/taurte/svelteWeb/src/components/Navigation.svelte`
- `/apps/desktop/src/App.svelte`
