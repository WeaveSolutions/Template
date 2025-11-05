# Nexpo Svelte App

A modern Svelte 5 application with Auth0 integration, TypeScript support, and seamless integration with the Nexpo monorepo shared packages.

## Features

- **Svelte 5** - Latest Svelte with new runes system for reactive state management
- **TypeScript** - Full TypeScript support with strict type checking
- **Auth0 Integration** - Secure authentication with Auth0's industry-leading platform
- **Vite** - Lightning-fast development with Hot Module Replacement
- **Shared Components** - Leverages shared packages from the Nexpo monorepo
- **Responsive Design** - Mobile-first design with dark mode support
- **Modern UI** - Clean, contemporary interface with smooth animations

## Development

### Prerequisites

- Node.js 18+ 
- pnpm (recommended package manager)

### Getting Started

1. **Install dependencies:**
   ```bash
   pnpm install
   ```

2. **Set up environment variables:**
   ```bash
   cp .env.example .env
   ```

3. **Configure Auth0:**
   Update `.env` with your Auth0 configuration:
   ```env
   VITE_AUTH0_DOMAIN=your-tenant.auth0.com
   VITE_AUTH0_CLIENT_ID=your-auth0-client-id
   VITE_AUTH0_AUDIENCE=https://your-api-identifier
   ```

4. **Start development server:**
   ```bash
   pnpm dev
   ```

   The app will be available at `http://localhost:1420`

### Available Scripts

- `pnpm dev` - Start development server
- `pnpm build` - Build for production
- `pnpm preview` - Preview production build locally
- `pnpm start` - Start production server on port 1420
- `pnpm check` - Run Svelte type checking
- `pnpm check:watch` - Run type checking in watch mode
- `pnpm lint` - Run ESLint
- `pnpm format` - Format code with Prettier

## Architecture

### State Management

The app uses Svelte 5's new runes system for state management:

```typescript
// stores/auth.ts - Auth store using $state rune
class AuthStore {
  private state = $state<AuthState>({
    isLoading: true,
    isAuthenticated: false,
    user: null,
    error: null
  });

  get isAuthenticated() {
    return this.state.isAuthenticated;
  }
  
  // ... other methods
}
```

### Authentication Flow

1. **Initialization** - App initializes Auth0 client on startup
2. **Redirect Login** - Uses Auth0's Universal Login for secure authentication
3. **Callback Handling** - Handles Auth0 redirect callback and extracts tokens
4. **State Management** - Updates global auth state using Svelte stores
5. **Protected Routes** - Guards dashboard and other protected pages

### Shared Package Integration

The app integrates with Nexpo monorepo shared packages:

```typescript
// Import shared components
import { Button } from '@shared/components';
import { useAuth } from '@shared/hooks';
import { apiClient } from '@shared/utils';
```

Path aliases are configured in `vite.config.ts` and `tsconfig.json` for seamless imports.

### Styling

The app uses CSS custom properties (CSS variables) for theming with automatic dark mode support:

```css
:root {
  --color-primary: #646cff;
  --color-background: #ffffff;
  --color-text: #213547;
}

@media (prefers-color-scheme: dark) {
  :root {
    --color-background: #1a1a1a;
    --color-text: #ffffff;
  }
}
```

## Project Structure

```
src/
├── components/          # Svelte components
│   └── Navigation.svelte
├── routes/             # Page components
│   ├── Home.svelte
│   ├── Login.svelte
│   └── Dashboard.svelte
├── stores/             # Svelte stores (state management)
│   └── auth.ts
├── lib/                # Utility functions
│   └── auth.ts
├── App.svelte          # Root component
├── main.ts             # App entry point
├── app.css             # Global styles
└── vite-env.d.ts       # TypeScript declarations
```

## Integration with Nexpo Ecosystem

This Svelte app is part of the larger Nexpo ecosystem:

- **Web App** (Next.js) - `http://localhost:3000`
- **Mobile App** (Expo) - `http://localhost:19000`
- **Desktop App** (Tauri) - `http://localhost:1420`
- **Svelte App** (This) - `http://localhost:1420`

All apps share:
- **Authentication** - Unified Auth0 identity system
- **Components** - Shared UI components and utilities
- **API** - Common backend microservices
- **State** - Cross-platform state synchronization

## Auth0 Setup

1. **Create Auth0 Application:**
   - Application Type: Single Page Application (SPA)
   - Allowed Callback URLs: `http://localhost:1420`
   - Allowed Logout URLs: `http://localhost:1420`
   - Allowed Web Origins: `http://localhost:1420`

2. **Configure Environment:**
   ```env
   VITE_AUTH0_DOMAIN=your-tenant.auth0.com
   VITE_AUTH0_CLIENT_ID=your-spa-client-id
   VITE_AUTH0_AUDIENCE=https://your-api-identifier
   ```

3. **Enable Social Connections** (Optional):
   - Google, Facebook, Twitter, etc.
   - Configure in Auth0 Dashboard > Authentication > Social

## Deployment

### Production Build

```bash
pnpm build
```

The build output will be in the `dist/` directory.

### Environment Variables

For production, ensure these environment variables are set:

- `VITE_AUTH0_DOMAIN` - Your Auth0 domain
- `VITE_AUTH0_CLIENT_ID` - Your Auth0 client ID
- `VITE_AUTH0_AUDIENCE` - Your Auth0 API audience
- `VITE_API_URL` - Your backend API URL

### Deployment Platforms

The built static assets can be deployed to:

- **Netlify** - Zero-config deployment
- **Vercel** - Automatic builds from Git
- **AWS S3 + CloudFront** - Static hosting with CDN
- **GitHub Pages** - Free hosting for open source projects

## Troubleshooting

### Common Issues

1. **Auth0 Configuration**
   - Ensure callback URLs match exactly
   - Check domain and client ID are correct
   - Verify CORS settings in Auth0 dashboard

2. **Module Resolution**
   - Run `pnpm install` to ensure dependencies are installed
   - Check path aliases in `vite.config.ts`

3. **Type Errors**
   - Run `pnpm check` for Svelte-specific type checking
   - Ensure shared packages are built

### Debug Mode

Enable debug mode by setting:
```env
VITE_ENABLE_DEBUG=true
```

This will show additional logging in the browser console.

## Contributing

1. **Follow Code Style** - Use `pnpm format` and `pnpm lint`
2. **Type Safety** - Ensure all code passes `pnpm check`
3. **Test Locally** - Verify the app works with Auth0 integration
4. **Update Documentation** - Keep README.md updated with changes

## License

This project is part of the Nexpo template and follows the same licensing terms.
