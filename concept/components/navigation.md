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

```tsx
// React/Next.js Example
export default function Navigation() {
  return (
    <nav className="navbar">
      <div className="container">
        {/* Logo */}
        <div className="brand">
          <img src="/logo.png" alt="Logo" />
          <span>App Name</span>
        </div>

        {/* Desktop Menu */}
        <ul className="nav-menu desktop">
          <li><a href="/">Home</a></li>
          <li><a href="/about">About</a></li>
          <li><a href="/features">Features</a></li>
          <li><a href="/pricing">Pricing</a></li>
        </ul>

        {/* User Actions */}
        <div className="nav-actions">
          {isAuthenticated ? (
            <>
              <button>Dashboard</button>
              <button onClick={logout}>Logout</button>
            </>
          ) : (
            <>
              <button>Login</button>
              <button className="cta">Sign Up</button>
            </>
          )}
        </div>

        {/* Mobile Toggle */}
        <button className="mobile-toggle" onClick={toggleMenu}>
          ☰
        </button>
      </div>

      {/* Mobile Menu */}
      {mobileMenuOpen && (
        <div className="mobile-menu">
          {/* Menu items... */}
        </div>
      )}
    </nav>
  );
}
```

## Styling Guidelines

```css
.navbar {
  position: sticky;
  top: 0;
  z-index: 1000;
  background: var(--bg-primary);
  border-bottom: 1px solid var(--border-color);
  backdrop-filter: blur(10px);
}

.container {
  max-width: 1400px;
  margin: 0 auto;
  padding: 1rem 2rem;
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.brand {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-weight: 700;
}

.nav-menu {
  display: flex;
  gap: 2rem;
  list-style: none;
}

@media (max-width: 768px) {
  .nav-menu.desktop {
    display: none;
  }
}
```

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
