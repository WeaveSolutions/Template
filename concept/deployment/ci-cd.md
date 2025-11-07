# CI/CD Pipeline Template

## GitHub Actions Workflow

```yaml
name: CI/CD Pipeline

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Node
        uses: actions/setup-node@v4
        with:
          node-version: '20'
          cache: 'pnpm'
      
      - name: Install dependencies
        run: pnpm install
      
      - name: Lint
        run: pnpm lint
      
      - name: Type check
        run: pnpm type-check
      
      - name: Run tests
        run: pnpm test
      
      - name: Build
        run: pnpm build

  deploy-web:
    needs: test
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    steps:
      - uses: actions/checkout@v4
      
      - name: Deploy to Vercel
        uses: amondnet/vercel-action@v25
        with:
          vercel-token: ${{ secrets.VERCEL_TOKEN }}
          vercel-org-id: ${{ secrets.VERCEL_ORG_ID }}
          vercel-project-id: ${{ secrets.VERCEL_PROJECT_ID }}
          vercel-args: '--prod'

  deploy-mobile:
    needs: test
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Expo
        uses: expo/expo-github-action@v8
        with:
          expo-version: latest
          token: ${{ secrets.EXPO_TOKEN }}
      
      - name: Publish to Expo
        run: |
          cd apps/nexpo/expoMobile
          expo publish

  deploy-desktop:
    needs: test
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
    if: github.ref == 'refs/heads/main'
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Rust
        uses: dtolnay/rust-toolchain@stable
      
      - name: Build Desktop App
        run: |
          cd apps/desktop
          pnpm tauri build
      
      - name: Upload Artifacts
        uses: actions/upload-artifact@v4
        with:
          name: desktop-app-${{ matrix.os }}
          path: apps/desktop/src-tauri/target/release/
```

## Environment Variables

```bash
# .env.production
DATABASE_URL=postgresql://...
REDIS_URL=redis://...
AUTH0_DOMAIN=...
AUTH0_CLIENT_ID=...
API_URL=https://api.example.com
```

## Deployment Checklist

- [ ] All tests passing
- [ ] Environment variables configured
- [ ] Database migrations run
- [ ] Secrets rotated
- [ ] Monitoring enabled
- [ ] Backup configured
- [ ] Rollback plan ready
- [ ] Documentation updated

## Monitoring

```yaml
# monitoring.yml
services:
  - name: web
    url: https://example.com
    interval: 60s
    alerts:
      - type: email
      - type: slack
  
  - name: api
    url: https://api.example.com/health
    interval: 30s
```

## Rollback Strategy

```bash
# Rollback to previous version
vercel rollback
# or
git revert HEAD
git push origin main
```
