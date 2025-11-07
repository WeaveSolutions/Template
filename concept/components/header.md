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

```tsx
// React/Next.js Example
export default function Header({
  title,
  subtitle,
  breadcrumbs = [],
  actions = [],
  showBack = false
}) {
  return (
    <header className="page-header">
      {/* Breadcrumbs */}
      {breadcrumbs.length > 0 && (
        <nav aria-label="Breadcrumb">
          <ol className="breadcrumbs">
            {breadcrumbs.map((crumb, index) => (
              <li key={index}>
                {index < breadcrumbs.length - 1 ? (
                  <a href={crumb.href}>{crumb.label}</a>
                ) : (
                  <span>{crumb.label}</span>
                )}
              </li>
            ))}
          </ol>
        </nav>
      )}

      {/* Title Section */}
      <div className="header-content">
        <div className="header-text">
          {showBack && (
            <button onClick={() => window.history.back()} className="back-btn">
              ← Back
            </button>
          )}
          <h1>{title}</h1>
          {subtitle && <p className="subtitle">{subtitle}</p>}
        </div>

        {/* Actions */}
        {actions.length > 0 && (
          <div className="header-actions">
            {actions.map((action, index) => (
              <button
                key={index}
                onClick={action.onClick}
                className={action.variant || 'default'}
              >
                {action.label}
              </button>
            ))}
          </div>
        )}
      </div>
    </header>
  );
}
```

## Styling Guidelines

```css
.page-header {
  padding: 2rem;
  background: var(--bg-primary);
  border-bottom: 1px solid var(--border-color);
}

.breadcrumbs {
  display: flex;
  gap: 0.5rem;
  list-style: none;
  padding: 0;
  margin-bottom: 1rem;
  font-size: 0.875rem;
}

.breadcrumbs li:not(:last-child)::after {
  content: "/";
  margin-left: 0.5rem;
  color: var(--text-tertiary);
}

.header-content {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.header-text h1 {
  font-size: 2rem;
  font-weight: 700;
  margin: 0;
}

.subtitle {
  color: var(--text-secondary);
  margin-top: 0.5rem;
}

.header-actions {
  display: flex;
  gap: 1rem;
}

@media (max-width: 768px) {
  .header-content {
    flex-direction: column;
    align-items: flex-start;
    gap: 1rem;
  }
}
```

## Props/Configuration

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `title` | `string` | - | Page title (required) |
| `subtitle` | `string` | `null` | Optional description |
| `breadcrumbs` | `Breadcrumb[]` | `[]` | Navigation breadcrumbs |
| `actions` | `Action[]` | `[]` | Header action buttons |
| `showBack` | `boolean` | `false` | Show back navigation button |

## Types

```typescript
interface Breadcrumb {
  label: string;
  href: string;
}

interface Action {
  label: string;
  onClick: () => void;
  variant?: 'primary' | 'secondary' | 'danger';
  icon?: string;
}
```

## Usage Examples

```tsx
// Simple page header
<Header title="Dashboard" />

// With breadcrumbs
<Header
  title="User Profile"
  breadcrumbs={[
    { label: 'Home', href: '/' },
    { label: 'Users', href: '/users' },
    { label: 'John Doe', href: '/users/123' }
  ]}
/>

// With actions
<Header
  title="Products"
  subtitle="Manage your product inventory"
  actions={[
    { label: 'Export', onClick: handleExport, variant: 'secondary' },
    { label: 'Add Product', onClick: handleAdd, variant: 'primary' }
  ]}
/>

// Detail page with back button
<Header
  title="Product Details"
  showBack={true}
/>
```

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
