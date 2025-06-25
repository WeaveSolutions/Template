# Contributing to Nexpo Multi-Cloud Template

We welcome contributions from the community! This project is designed to be a comprehensive solution for cross-platform development with multi-cloud infrastructure support. Here's how you can help improve this project.

## Getting Started

1. **Fork** the repository on GitHub
2. **Clone** the project to your own machine
3. **Commit** changes to your own branch
4. **Push** your work back up to your fork
5. Submit a **Pull Request** so that we can review your changes

## Development Setup

### Prerequisites

#### Core Technologies
- Node.js >= 16.x
- pnpm >= 8.x or yarn >= 1.22.x
- Expo CLI (for mobile development)
- React Native development environment

#### Infrastructure & Cloud
- Terraform >= 1.5.0
- AWS CLI (if working with AWS)
- gcloud CLI (if working with GCP)
- Azure CLI (if working with Azure)
- OCI CLI (if working with Oracle Cloud)
- IBM Cloud CLI (if working with IBM Cloud)
- Cloudflare CLI (if working with Cloudflare)

#### Database Development
- PostgreSQL (local instance or Docker)
- MongoDB (local instance or Docker)
- SQL Server (local instance or Docker)

### Local Development

1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/Nexpo-main.git
   cd Nexpo-main
   ```

2. Install dependencies:
   ```bash
   pnpm install
   ```

3. Create a local `.env` file:
   ```bash
   cp .env.example .env
   ```
   Edit the `.env` file to configure your local development environment and enable feature flags.

4. Run the development servers:
   ```bash
   # For web (Next.js)
   pnpm run dev:web
   
   # For mobile (Expo)
   pnpm run dev:mobile
   ```

## Project Structure

This is a monorepo with the following structure:

```
./
├── apps/
│   ├── expo/        # Mobile app (iOS & Android)
│   └── next/        # Web app
├── packages/
│   ├── shared-api/           # Shared API layer
│   ├── shared-components/    # UI components
│   ├── shared-db/            # Database clients
│   ├── shared-feature-flags/ # Feature flag system
│   ├── shared-pages/         # Full page components
│   └── shared-utils/         # Utility functions
├── terraform/      # Multi-cloud infrastructure
└── .github/        # GitHub Actions workflows
```

## Feature Development

### Adding New Features

When adding new features, consider the following guidelines:

1. **Cross-Platform Compatibility**: Ensure your feature works on both web and mobile platforms.
2. **Feature Flags**: Implement feature flags for any new functionality to allow easy enabling/disabling.
3. **Database Integration**: If your feature requires database access, support all database providers using the Prisma client abstractions.
4. **Offline Support**: Consider how your feature will work offline with Ditto synchronization.
5. **Multi-Cloud Support**: For infrastructure changes, ensure compatibility with all supported cloud providers.

### Feature Flag Implementation

Add new feature flags to:

1. The `.env.example` file
2. The feature flag provider in `packages/shared-feature-flags`
3. Documentation in relevant README files

## Infrastructure Development

### Terraform Changes

When modifying Terraform infrastructure:

1. Initialize Terraform:
   ```bash
   cd terraform
   terraform init
   ```

2. Test your changes with specific providers enabled:
   ```bash
   # Run Terraform plan for AWS
   ./run_terraform.ps1 plan -var="enable_aws=true"
   
   # Or for Linux/Mac
   ./run_terraform.sh plan -var="enable_aws=true"
   ```

3. Ensure changes are compatible with all cloud providers and maintain modularity.

### Environment Variables

All infrastructure configuration should use environment variables loaded from the root `.env` file using the Terraform run scripts.

## Database Integration

### Working with Prisma

When modifying database schemas:

1. Update the provider-specific schema in `packages/shared-db/prisma/providers/`
2. Generate the Prisma client:
   ```bash
   cd packages/shared-db
   pnpm run generate
   ```

3. Ensure your changes support all database providers by testing each one.

### Provider Isolation

Maintain proper isolation between database providers by using provider-specific model names, unique datasource names, and dedicated client generators.

## Testing

### Running Application Tests

1. Run unit tests:
   ```bash
   pnpm run test
   ```

2. Run integration tests:
   ```bash
   pnpm run test:integration
   ```

3. Test infrastructure changes for each provider:
   ```bash
   cd terraform
   ./run_terraform.ps1 validate
   ```

### Pre-commit Hooks

We use pre-commit hooks to ensure code quality. Install them with:

```bash
pre-commit install
```

### Running Terraform Tests

```bash
make test
```

## CI/CD Integration

This project uses GitHub Actions for CI/CD. When making changes:

1. Test your changes against the workflow files in `.github/workflows/`
2. For infrastructure changes, update the deployment workflows
3. For mobile changes, ensure the Expo EAS workflows are updated

## Documentation

Always update documentation when adding or modifying features:

1. Update the main README.md
2. Update provider-specific documentation
3. Add code comments for complex logic
4. Create or update example files where appropriate

## Code Style

- Follow the established coding style in the project
- Use TypeScript for type safety
- Follow React best practices
- Use feature flags for new functionality
- Maintain cross-platform compatibility
- For Terraform files:
  - Follow the [Terraform Style Conventions](https://www.terraform.io/docs/language/syntax/style.html)
  - Use `terraform fmt` to format your code
  - Document all variables and outputs
  - Include examples in the `examples/` directory

## Pull Request Process

1. Create a branch for your changes:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. Make your changes and commit them with descriptive messages

3. Update the CHANGELOG.md with details of changes

4. Ensure all tests pass and coverage is adequate

5. Push your branch and create a pull request against the `main` branch

6. Ensure all CI checks pass before requesting review

## Reporting Issues

When reporting issues, please include:

- Relevant software versions (Node.js, Terraform, etc.)
- Module or component affected
- Configuration details (sanitized if sensitive)
- Steps to reproduce
- Expected behavior
- Actual behavior

## License

By contributing to this project, you agree that your contributions will be licensed under the project's Apache 2.0 License.
