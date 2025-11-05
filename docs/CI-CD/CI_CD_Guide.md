# CI/CD Configuration Guide for Nexpo

This guide provides instructions for setting up Continuous Integration and Continuous Deployment (CI/CD) for the Nexpo template using GitHub Actions. CI/CD automates testing, building, and deployment processes to ensure code quality and streamline releases.

## Overview

Continuous Integration (CI) runs automated tests and builds on every code change, while Continuous Deployment (CD) automates the deployment of code to staging or production environments. This template uses GitHub Actions for CI/CD, covering testing with Jest, building Next.js and Expo apps, and deploying to Vercel (for Next.js) and Expo EAS (for mobile).

## Prerequisites

- GitHub repository for your project ([https://github.com/](https://github.com/))
- Node.js and pnpm installed ([https://nodejs.org/](https://nodejs.org/))
- Auth0 account for authentication and authorization ([https://auth0.com/](https://auth0.com/))
- Supabase project for backend services ([https://supabase.com/](https://supabase.com/))
- Firebase account for web app builds and deployment ([https://firebase.google.com/](https://firebase.google.com/))

# Application Development

- Vercel account for Next.js deployment ([https://vercel.com/](https://vercel.com/))
- Expo account for mobile app builds and deployment ([https://expo.dev/](https://expo.dev/))
- Tauri account for desktop app builds and deployment ([https://tauri.app/](https://tauri.app/))\
- Cloudflare account for domain management ([https://cloudflare.com/](https://cloudflare.com/))

## Setting Up CI/CD with GitHub Actions

### 1. Directory Structure for Workflows

GitHub Actions workflows are defined as YAML files in the `.github/workflows/` directory. We'll create separate workflows for testing, Next.js deployment, and Expo mobile app builds.

Create the following directory structure if not already present:

```
Nexpo-main/
├── .github/
│   └── workflows/
│       ├── ci.yml
│       ├── deploy-nextjs.yml
│       └── deploy-expo.yml
```

### 2. CI Workflow for Testing

Create a workflow file at `.github/workflows/ci.yml` to run tests on every push or pull request:

```yaml
name: CI - Tests

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3

    - name: Setup Node.js
      uses: actions/setup-node@v3
      with:
        node-version: '18'
        cache: 'pnpm'

    - name: Install Dependencies
      run: pnpm ci

    - name: Run Tests
      run: pnpm test
      env:
        NODE_ENV: test

    - name: Cache node modules
      uses: actions/cache@v3
      with:
        path: ~/.pnpm
        key: ${{ runner.os }}-node-${{ hashFiles('**/pnpm-lock.yaml') }}
        restore-keys: |
          ${{ runner.os }}-node-
```

- **Triggers**: Runs on pushes or pull requests to `main` or `develop` branches.
- **Steps**: Checks out code, sets up Node.js, installs dependencies, and runs Jest tests.
- **Environment**: Sets `NODE_ENV` to `test` for testing configurations.

### 3. CD Workflow for Next.js Deployment to Vercel

Create a workflow file at `.github/workflows/deploy-nextjs.yml` for deploying the Next.js app to Vercel:

```yaml
name: CD - Deploy Next.js to Vercel

on:
  push:
    branches: [ main ]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3

    - name: Setup Node.js
      uses: actions/setup-node@v3
      with:
        node-version: '18'
        cache: 'pnpm'

    - name: Install Dependencies
      run: pnpm ci

    - name: Install Vercel CLI
      run: pnpm install --global vercel

    - name: Pull Vercel Environment Information
      run: vercel pull --yes --environment=production --token=${{ secrets.VERCEL_TOKEN }}
      working-directory: ./apps/next

    - name: Build Next.js App
      run: vercel build --prod --token=${{ secrets.VERCEL_TOKEN }}
      working-directory: ./apps/next

    - name: Deploy to Vercel
      run: vercel deploy --prebuilt --prod --token=${{ secrets.VERCEL_TOKEN }}
      working-directory: ./apps/next
      env:
        VERCEL_ORG_ID: ${{ secrets.VERCEL_ORG_ID }}
        VERCEL_PROJECT_ID: ${{ secrets.VERCEL_PROJECT_ID }}
```

- **Triggers**: Runs on push to `main` branch.
- **Steps**: Installs Vercel CLI, pulls environment info, builds, and deploys the Next.js app.
- **Secrets**: Requires `VERCEL_TOKEN`, `VERCEL_ORG_ID`, and `VERCEL_PROJECT_ID` stored in GitHub Secrets (Settings > Secrets and variables > Actions).
  - `VERCEL_TOKEN`: Generate from your Vercel account under Tokens.
  - `VERCEL_ORG_ID` and `VERCEL_PROJECT_ID`: Find these in your Vercel project settings.

### 4. CD Workflow for Expo Mobile App Builds with EAS

Create a workflow file at `.github/workflows/deploy-expo.yml` for building and submitting the Expo app to app stores:

```yaml
name: CD - Build Expo App with EAS

on:
  push:
    branches: [ main ]
  workflow_dispatch: # Allows manual triggering

jobs:
  build-ios:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3

    - name: Setup Node.js
      uses: actions/setup-node@v3
      with:
        node-version: '18'
        cache: 'pnpm'

    - name: Install Dependencies
      run: pnpm ci

    - name: Setup EAS CLI
      run: pnpm install -g eas-cli

    - name: Login to Expo
      run: eas login --non-interactive
      env:
        EXPO_USERNAME: ${{ secrets.EXPO_USERNAME }}
        EXPO_PASSWORD: ${{ secrets.EXPO_PASSWORD }}

    - name: Build iOS App
      run: eas build --platform ios --profile production --non-interactive
      working-directory: ./apps/expo

    - name: Submit iOS App to App Store (Optional)
      run: eas submit --platform ios --profile production --non-interactive
      working-directory: ./apps/expo
      if: github.event_name == 'workflow_dispatch' # Only on manual trigger

  build-android:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3

    - name: Setup Node.js
      uses: actions/setup-node@v3
      with:
        node-version: '18'
        cache: 'pnpm'

    - name: Install Dependencies
      run: pnpm ci

    - name: Setup EAS CLI
      run: pnpm install -g eas-cli

    - name: Login to Expo
      run: eas login --non-interactive
      env:
        EXPO_USERNAME: ${{ secrets.EXPO_USERNAME }}
        EXPO_PASSWORD: ${{ secrets.EXPO_PASSWORD }}

    - name: Build Android App
      run: eas build --platform android --profile production --non-interactive
      working-directory: ./apps/expo

    - name: Submit Android App to Google Play (Optional)
      run: eas submit --platform android --profile production --non-interactive
      working-directory: ./apps/expo
      if: github.event_name == 'workflow_dispatch' # Only on manual trigger
```

- **Triggers**: Runs on push to `main` or manually via `workflow_dispatch`.
- **Steps**: Builds iOS and Android apps separately using EAS CLI. Submission to app stores is optional and only triggered manually.
- **Secrets**: Requires `EXPO_USERNAME` and `EXPO_PASSWORD` in GitHub Secrets.
  - Note: For security, consider using Expo tokens instead of username/password in a production setup.
- Ensure `eas.json` in `apps/expo` is configured with a `production` profile as shown in the deployment guide.

### 5. Setting Up GitHub Secrets

Add the following secrets in your GitHub repository under Settings > Secrets and variables > Actions:

- `VERCEL_TOKEN`: Your Vercel deployment token.
- `VERCEL_ORG_ID`: Your Vercel organization ID.
- `VERCEL_PROJECT_ID`: Your Vercel project ID for the Next.js app.
- `EXPO_USERNAME`: Your Expo account username.
- `EXPO_PASSWORD`: Your Expo account password (or use a token for better security).

Additionally, for Supabase integration or other environment variables, add:

- `SUPABASE_URL`: Production Supabase project URL.
- `SUPABASE_ANON_KEY`: Production Supabase anonymous key.

### 6. Environment Variables in Workflows

For workflows needing environment variables (like Supabase credentials for tests), add them directly in the YAML under `env` or use GitHub Secrets. Avoid hardcoding sensitive data in workflows.

Example for adding Supabase vars in `ci.yml`:

```yaml
- name: Run Tests
  run: pnpm test
  env:
    NODE_ENV: test
    NEXT_PUBLIC_SUPABASE_URL: ${{ secrets.SUPABASE_URL }}
    NEXT_PUBLIC_SUPABASE_ANON_KEY: ${{ secrets.SUPABASE_ANON_KEY }}
    EXPO_PUBLIC_SUPABASE_URL: ${{ secrets.SUPABASE_URL }}
    EXPO_PUBLIC_SUPABASE_ANON_KEY: ${{ secrets.SUPABASE_ANON_KEY }}
```

## Best Practices

- **Branching Strategy**: Use a branching model like GitFlow with `main` for production and `develop` for staging. Deploy from `main` only after thorough testing.
- **Testing**: Ensure CI runs comprehensive tests (unit, integration) before deployment. Add linting or code quality checks if needed.
- **Artifacts**: Store build artifacts (e.g., Expo builds) in GitHub Actions for debugging or manual deployment if automated submission fails.
- **Notifications**: Add Slack or email notifications to workflows for build/deployment status using GitHub Actions integrations.
- **Security**: Never commit sensitive keys or tokens to version control. Use GitHub Secrets or a secrets management service.
- **Versioning**: Automate version bumps in `package.json` or `app.json` as part of the deployment workflow to track releases.
- **Rollbacks**: For Vercel, rollbacks are handled via the dashboard. For Expo, ensure OTA updates are configured for quick fixes without app store resubmission.

## Troubleshooting

- **Workflow Failures**: Check GitHub Actions logs for errors. Common issues include missing secrets, incorrect Node.js versions, or dependency conflicts.
- **Vercel Deployment Errors**: Ensure `VERCEL_PROJECT_ID` matches your project. Check Vercel logs for build errors (e.g., missing environment variables).
- **Expo Build Failures**: Verify EAS CLI setup and credentials. Check build logs in Expo dashboard for SDK or configuration issues.
- **Test Failures**: If CI tests fail, replicate locally with `NODE_ENV=test pnpm test`. Ensure test environment variables are set in CI.
- **Permissions**: If workflows can't access secrets, ensure the repository has correct permissions for Actions.

For more detailed information, refer to:
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Vercel CI/CD Integration](https://vercel.com/docs/concepts/git)
- [Expo EAS Build](https://docs.expo.dev/build/introduction/)
