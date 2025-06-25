# Vercel Deployment Guide

This guide covers deploying the Next.js web application to Vercel, including environment setup, deployment configuration, and best practices.

**Note**: The Next.js server is now part of the terraform microservices architecture under `terraform/microservices/auth/server`.

## Table of Contents

- [Prerequisites](#prerequisites)
- [Initial Setup](#initial-setup)
- [Deployment Methods](#deployment-methods)
- [Environment Variables](#environment-variables)
- [Domain Configuration](#domain-configuration)
- [Build Configuration](#build-configuration)
- [Performance Optimization](#performance-optimization)
- [Monitoring & Analytics](#monitoring--analytics)
- [Troubleshooting](#troubleshooting)

## Prerequisites

Before deploying to Vercel, ensure you have:

1. **Vercel Account**: Sign up at [vercel.com](https://vercel.com)
2. **Git Repository**: Your code should be in GitHub, GitLab, or Bitbucket
3. **Node.js**: Version 18.x or higher
4. **Build Success**: Ensure `pnpm build` runs successfully locally

## Initial Setup

### 1. Install Vercel CLI (Optional)

```bash
npm i -g vercel
```

### 2. Project Structure Verification

Ensure your project structure is correct:

```
Nexpo/
├── terraform/
│   └── microservices/
│       └── auth/
│           └── server/      # This is what we'll deploy
│               ├── package.json
│               ├── server.ts
│               ├── api/
│               └── middleware/
├── packages/
│   ├── shared-components/
│   └── shared-pages/
└── package.json            # Root package.json
```

## Deployment Methods

### Method 1: Vercel Dashboard (Recommended)

1. **Import Project**
   - Go to [vercel.com/new](https://vercel.com/new)
   - Import your Git repository
   - Select the repository containing your code

2. **Configure Project**
   - **Framework Preset**: Next.js
   - **Root Directory**: `terraform/microservices/auth/server`
   - **Build Command**: `cd ../../../.. && pnpm build --filter=auth-server`
   - **Output Directory**: `terraform/microservices/auth/server/.next`
   - **Install Command**: `pnpm install`

3. **Deploy**
   - Click "Deploy"
   - Wait for the build to complete

### Method 2: Vercel CLI

1. **Login to Vercel**
   ```bash
   vercel login
   ```

2. **Navigate to Auth Server**
   ```bash
   cd terraform/microservices/auth/server
   ```

3. **Deploy**
   ```bash
   vercel
   ```

4. **Follow Prompts**
   - Set up and deploy: Yes
   - Which scope: Select your account
   - Link to existing project: No (for first time)
   - Project name: your-project-name
   - Directory: ./
   - Override settings: Yes
   - **Build Command**: `cd ../../../.. && pnpm build --filter=auth-server`
   - **Output Directory**: `.next`
   - **Development Command**: `cd ../../../.. && pnpm dev --filter=auth-server`

### Method 3: GitHub Integration

1. **Connect GitHub**
   - In Vercel Dashboard, go to Settings
   - Connect your GitHub account
   - Import the repository

2. **Automatic Deployments**
   - Every push to main branch triggers deployment
   - Pull requests get preview deployments

## Environment Variables

### Setting Environment Variables

1. **In Vercel Dashboard**
   - Go to Project Settings → Environment Variables
   - Add your variables:

   ```
   # Supabase
   NEXT_PUBLIC_SUPABASE_URL=your_supabase_url
   NEXT_PUBLIC_SUPABASE_ANON_KEY=your_supabase_anon_key
   
   # Analytics (Optional)
   NEXT_PUBLIC_GA_ID=your_google_analytics_id
   NEXT_PUBLIC_VERCEL_ANALYTICS_ID=your_vercel_analytics_id
   
   # API Keys (Server-side only)
   API_SECRET_KEY=your_secret_key
   ```

2. **Environment Variable Scopes**
   - **Production**: Main branch deployments
   - **Preview**: PR and branch deployments
   - **Development**: Local development (via Vercel CLI)

### Best Practices

- Use `NEXT_PUBLIC_` prefix for client-side variables
- Never commit `.env` files to Git
- Use different values for development/staging/production
- Rotate sensitive keys regularly

## Domain Configuration

### Adding a Custom Domain

1. **In Project Settings → Domains**
   - Click "Add Domain"
   - Enter your domain: `example.com`

2. **DNS Configuration**
   
   **For Apex Domain (example.com)**:
   ```
   Type: A
   Name: @
   Value: 76.76.21.21
   ```

   **For Subdomain (www.example.com)**:
   ```
   Type: CNAME
   Name: www
   Value: cname.vercel-dns.com
   ```

3. **SSL Certificate**
   - Automatically provisioned by Vercel
   - Let's Encrypt certificates
   - Auto-renewal

### Redirects

Add to `terraform/microservices/auth/server/next.config.js`:

```javascript
module.exports = {
  async redirects() {
    return [
      {
        source: '/old-path',
        destination: '/new-path',
        permanent: true,
      },
    ]
  },
}
```

## Build Configuration

### vercel.json Configuration

Create `vercel.json` in your auth server directory:

```json
{
  "buildCommand": "cd ../../../.. && pnpm build --filter=auth-server",
  "outputDirectory": ".next",
  "installCommand": "pnpm install",
  "framework": "nextjs",
  "regions": ["iad1"],
  "functions": {
    "pages/api/*": {
      "maxDuration": 10
    }
  }
}
```

### Build Optimizations

1. **Enable SWC**
   ```javascript
   // next.config.js
   module.exports = {
     swcMinify: true,
   }
   ```

2. **Image Optimization**
   ```javascript
   module.exports = {
     images: {
       domains: ['your-image-domain.com'],
       formats: ['image/avif', 'image/webp'],
     },
   }
   ```

3. **Bundle Analysis**
   ```bash
   # Install analyzer
   pnpm add -D @next/bundle-analyzer

   # Add to next.config.js
   const withBundleAnalyzer = require('@next/bundle-analyzer')({
     enabled: process.env.ANALYZE === 'true',
   })
   module.exports = withBundleAnalyzer({
     // your config
   })
   ```

## Performance Optimization

### 1. Edge Functions

Use Edge Runtime for better performance:

```typescript
// pages/api/hello.ts
export const config = {
  runtime: 'edge',
}

export default function handler(req: Request) {
  return new Response('Hello from the edge!')
}
```

### 2. ISR (Incremental Static Regeneration)

```typescript
export async function getStaticProps() {
  const data = await fetchData()
  
  return {
    props: { data },
    revalidate: 60, // Revalidate every 60 seconds
  }
}
```

### 3. Middleware

Create `middleware.ts` in your auth server root:

```typescript
import { NextResponse } from 'next/server'
import type { NextRequest } from 'next/server'

export function middleware(request: NextRequest) {
  // Add security headers
  const response = NextResponse.next()
  response.headers.set('X-Frame-Options', 'DENY')
  response.headers.set('X-Content-Type-Options', 'nosniff')
  return response
}

export const config = {
  matcher: '/:path*',
}
```

## Monitoring & Analytics

### 1. Vercel Analytics

```bash
# Install
pnpm add @vercel/analytics

# Add to _app.tsx
import { Analytics } from '@vercel/analytics/react'

function MyApp({ Component, pageProps }) {
  return (
    <>
      <Component {...pageProps} />
      <Analytics />
    </>
  )
}
```

### 2. Speed Insights

```bash
# Install
pnpm add @vercel/speed-insights

# Add to _app.tsx
import { SpeedInsights } from '@vercel/speed-insights/next'

function MyApp({ Component, pageProps }) {
  return (
    <>
      <Component {...pageProps} />
      <SpeedInsights />
    </>
  )
}
```

### 3. Web Vitals

```typescript
// pages/_app.tsx
export function reportWebVitals(metric) {
  console.log(metric)
  // Send to analytics
}
```

## Troubleshooting

### Common Issues

1. **Build Failures**
   - Check build logs in Vercel dashboard
   - Ensure all dependencies are in package.json
   - Verify environment variables are set

2. **Module Not Found**
   ```json
   // vercel.json
   {
     "functions": {
       "pages/api/*": {
         "includeFiles": "../../../../**"
       }
     }
   }
   ```

3. **Monorepo Issues**
   - Ensure correct root directory
   - Use proper build commands with filters
   - Check pnpm workspace configuration

4. **Large Bundle Size**
   - Use dynamic imports
   - Analyze bundle with @next/bundle-analyzer
   - Optimize images and fonts

### Debug Mode

Enable detailed logs:

```bash
VERCEL_DEBUG=1 vercel
```

### Function Logs

View function logs in Vercel dashboard:
- Functions tab → View logs
- Real-time log streaming
- Error tracking

## Production Checklist

Before going live:

- [ ] Environment variables configured
- [ ] Custom domain setup
- [ ] SSL certificate active
- [ ] Analytics configured
- [ ] Error tracking setup
- [ ] Performance monitoring enabled
- [ ] Security headers configured
- [ ] Redirects tested
- [ ] API rate limiting implemented
- [ ] Database connection pooling configured

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Deploy to Vercel

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Deploy to Vercel
        uses: amondnet/vercel-action@v25
        with:
          vercel-token: ${{ secrets.VERCEL_TOKEN }}
          vercel-org-id: ${{ secrets.VERCEL_ORG_ID }}
          vercel-project-id: ${{ secrets.VERCEL_PROJECT_ID }}
          working-directory: ./terraform/microservices/auth/server
```

## Additional Resources

- [Vercel Documentation](https://vercel.com/docs)
- [Next.js on Vercel](https://vercel.com/docs/frameworks/nextjs)
- [Vercel CLI Reference](https://vercel.com/docs/cli)
- [Deployment Best Practices](https://vercel.com/docs/concepts/deployments/overview)

---

For more deployment options, see:
- [AWS Deployment Guide](./AWS-Deployment.md)
- [Google Cloud Deployment Guide](./GCP-Deployment.md)
- [Self-Hosted Deployment Guide](./Self-Hosted-Deployment.md)
