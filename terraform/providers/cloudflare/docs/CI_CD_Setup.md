# CI/CD Setup for Cloudflare Infrastructure

This document provides detailed instructions for setting up Continuous Integration and Continuous Deployment (CI/CD) for your Cloudflare infrastructure managed with Terraform.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [CI/CD Options](#cicd-options)
3. [GitHub Actions Setup](#github-actions-setup)
4. [GitLab CI Setup](#gitlab-ci-setup)
5. [Jenkins Setup](#jenkins-setup)
6. [CircleCI Setup](#circleci-setup)
7. [Terraform Backend Configuration](#terraform-backend-configuration)
8. [Security Best Practices](#security-best-practices)
9. [Troubleshooting](#troubleshooting)

## Prerequisites

Before setting up CI/CD for Cloudflare with Terraform, ensure you have:

- Cloudflare API token with appropriate permissions
- Cloudflare account with zones configured
- Terraform 1.0.0+ installed
- Git repository for your infrastructure code

## CI/CD Options

You can use several CI/CD platforms with Cloudflare:

1. **GitHub Actions**: Great for GitHub repositories, with simple YAML configuration
2. **GitLab CI/CD**: Integrated solution for GitLab repositories
3. **Jenkins**: Self-hosted option with extensive customization
4. **CircleCI**: Cloud-based CI/CD with a free tier
5. **Other Options**: Travis CI, Azure DevOps, AWS CodePipeline, etc.

## GitHub Actions Setup

### Sample GitHub Actions Workflow

Create `.github/workflows/terraform-cloudflare.yml` in your repository:

```yaml
name: 'Terraform Cloudflare'

on:
  push:
    branches: [ main ]
    paths: [ 'terraform/providers/cloudflare/**' ]
  pull_request:
    branches: [ main ]
    paths: [ 'terraform/providers/cloudflare/**' ]

jobs:
  terraform:
    name: 'Terraform'
    runs-on: ubuntu-latest
    defaults:
      run:
        working-directory: ./terraform

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Setup Terraform
      uses: hashicorp/setup-terraform@v2
      with:
        terraform_version: 1.5.0

    - name: Terraform Format
      run: terraform fmt -check -recursive

    - name: Terraform Init
      run: terraform init
      env:
        TF_VAR_cloudflare_api_token: ${{ secrets.CLOUDFLARE_API_TOKEN }}

    - name: Terraform Validate
      run: terraform validate

    - name: Terraform Plan
      if: github.event_name == 'pull_request'
      run: terraform plan -var-file=cloudflare.tfvars -var="enable_cloudflare=true"
      env:
        TF_VAR_cloudflare_api_token: ${{ secrets.CLOUDFLARE_API_TOKEN }}
        TF_VAR_cloudflare_email: ${{ secrets.CLOUDFLARE_EMAIL }}

    - name: Terraform Apply
      if: github.ref == 'refs/heads/main' && github.event_name == 'push'
      run: terraform apply -auto-approve -var-file=cloudflare.tfvars -var="enable_cloudflare=true"
      env:
        TF_VAR_cloudflare_api_token: ${{ secrets.CLOUDFLARE_API_TOKEN }}
        TF_VAR_cloudflare_email: ${{ secrets.CLOUDFLARE_EMAIL }}
```

### Setting Up GitHub Secrets

1. Go to your GitHub repository → Settings → Secrets and variables → Actions
2. Add these secrets:
   - `CLOUDFLARE_API_TOKEN`: Your Cloudflare API token
   - `CLOUDFLARE_EMAIL`: Your Cloudflare account email
   - `CLOUDFLARE_ACCOUNT_ID`: Your Cloudflare account ID (if needed)

## GitLab CI Setup

### Sample GitLab CI Configuration

Create `.gitlab-ci.yml` in your repository:

```yaml
image: hashicorp/terraform:1.5.0

variables:
  TF_ROOT: ${CI_PROJECT_DIR}/terraform

cache:
  paths:
    - ${TF_ROOT}/.terraform

stages:
  - validate
  - plan
  - apply

before_script:
  - cd ${TF_ROOT}

terraform:validate:
  stage: validate
  script:
    - terraform init
    - terraform validate
    - terraform fmt -check -recursive

terraform:plan:
  stage: plan
  script:
    - terraform init
    - terraform plan -var-file=cloudflare.tfvars -var="enable_cloudflare=true" -out=tfplan
  artifacts:
    paths:
      - ${TF_ROOT}/tfplan

terraform:apply:
  stage: apply
  script:
    - terraform init
    - terraform apply -auto-approve tfplan
  dependencies:
    - terraform:plan
  only:
    - main
  when: manual
```

### Setting Up GitLab CI/CD Variables

1. Go to your GitLab project → Settings → CI/CD → Variables
2. Add these variables:
   - `TF_VAR_cloudflare_api_token`: Your Cloudflare API token (Masked: Yes, Protected: Yes)
   - `TF_VAR_cloudflare_email`: Your Cloudflare account email
   - `TF_VAR_cloudflare_account_id`: Your Cloudflare account ID (if needed)

## Jenkins Setup

### Sample Jenkinsfile

Create a `Jenkinsfile` in your repository:

```groovy
pipeline {
    agent {
        docker {
            image 'hashicorp/terraform:1.5.0'
        }
    }
    
    environment {
        TF_ROOT = "${WORKSPACE}/terraform"
        CLOUDFLARE_API_TOKEN = credentials('cloudflare-api-token')
        CLOUDFLARE_EMAIL = credentials('cloudflare-email')
    }
    
    stages {
        stage('Init') {
            steps {
                dir("${env.TF_ROOT}") {
                    sh 'terraform init'
                }
            }
        }
        
        stage('Validate') {
            steps {
                dir("${env.TF_ROOT}") {
                    sh 'terraform validate'
                    sh 'terraform fmt -check -recursive'
                }
            }
        }
        
        stage('Plan') {
            steps {
                dir("${env.TF_ROOT}") {
                    sh 'terraform plan -var-file=cloudflare.tfvars -var="enable_cloudflare=true" -out=tfplan'
                }
            }
        }
        
        stage('Apply') {
            when {
                branch 'main'
            }
            steps {
                dir("${env.TF_ROOT}") {
                    sh 'terraform apply -auto-approve tfplan'
                }
            }
        }
    }
}
```

### Setting Up Jenkins Credentials

1. In Jenkins, go to Manage Jenkins → Manage Credentials
2. Add these Secret Text credentials:
   - `cloudflare-api-token`: Your Cloudflare API token
   - `cloudflare-email`: Your Cloudflare account email

## CircleCI Setup

### Sample CircleCI Config

Create `.circleci/config.yml` in your repository:

```yaml
version: 2.1

jobs:
  validate:
    docker:
      - image: hashicorp/terraform:1.5.0
    steps:
      - checkout
      - run:
          name: Terraform Init
          command: cd terraform && terraform init
      - run:
          name: Terraform Validate
          command: cd terraform && terraform validate
      - run:
          name: Terraform Format
          command: cd terraform && terraform fmt -check -recursive

  plan:
    docker:
      - image: hashicorp/terraform:1.5.0
    steps:
      - checkout
      - run:
          name: Terraform Init
          command: cd terraform && terraform init
      - run:
          name: Terraform Plan
          command: cd terraform && terraform plan -var-file=cloudflare.tfvars -var="enable_cloudflare=true" -out=tfplan
      - persist_to_workspace:
          root: terraform
          paths:
            - tfplan

  apply:
    docker:
      - image: hashicorp/terraform:1.5.0
    steps:
      - checkout
      - attach_workspace:
          at: terraform
      - run:
          name: Terraform Init
          command: cd terraform && terraform init
      - run:
          name: Terraform Apply
          command: cd terraform && terraform apply -auto-approve tfplan

workflows:
  version: 2
  terraform:
    jobs:
      - validate
      - plan:
          requires:
            - validate
      - approve-apply:
          type: approval
          requires:
            - plan
          filters:
            branches:
              only: main
      - apply:
          requires:
            - approve-apply
          filters:
            branches:
              only: main
```

### Setting Up CircleCI Environment Variables

1. Go to your CircleCI project → Project Settings → Environment Variables
2. Add these variables:
   - `TF_VAR_cloudflare_api_token`: Your Cloudflare API token
   - `TF_VAR_cloudflare_email`: Your Cloudflare account email
   - `TF_VAR_cloudflare_account_id`: Your Cloudflare account ID (if needed)

## Terraform Backend Configuration

For any CI/CD setup, it's recommended to use a remote backend for Terraform state:

### AWS S3 Backend

Create a `backend.tf` file in your Terraform directory:

```hcl
terraform {
  backend "s3" {
    bucket         = "terraform-state-bucket"
    key            = "terraform/cloudflare/terraform.tfstate"
    region         = "us-west-2"
    encrypt        = true
    dynamodb_table = "terraform-lock"
  }
}
```

### Other Backend Options

You can also use:
- **Terraform Cloud**
- **Azure Storage Account**
- **Google Cloud Storage**
- **HashiCorp Consul**

## Security Best Practices

1. **Never** commit API tokens or credentials to your repository
2. Use environment variables or secrets management in your CI/CD platform
3. Create scoped API tokens with the minimum required permissions
4. Regularly rotate API tokens
5. Use rate limiting for API calls
6. Enable two-factor authentication for Cloudflare accounts
7. Implement approval gates for production deployments
8. Scan IaC for security misconfigurations using tools like Checkov or tfsec

## Terraform Workspaces for Environments

Use Terraform workspaces to manage multiple environments (dev, staging, prod):

```bash
# Create workspaces
terraform workspace new dev
terraform workspace new staging
terraform workspace new prod

# Select workspace
terraform workspace select dev

# Deploy to the selected workspace
terraform apply -var-file=cloudflare-dev.tfvars
```

## Troubleshooting

### Common Issues

1. **Authentication Failures**
   - Check if your Cloudflare API token has the correct permissions
   - Verify the API token has not expired or been revoked
   - Ensure you're using the correct account email

2. **Terraform Initialization Failures**
   - Check network connectivity to the Terraform registry
   - Verify backend access and credentials

3. **Rate Limiting Issues**
   - Add delays between API calls or use the Cloudflare provider's rate limiting features
   - Implement exponential backoff in your CI/CD pipeline

4. **Zone Access Problems**
   - Verify your API token has access to the required zones
   - Check zone name spelling and configuration

### Getting Help

- Cloudflare Community: https://community.cloudflare.com/
- Cloudflare Support: https://support.cloudflare.com/
- Cloudflare Terraform Provider: https://registry.terraform.io/providers/cloudflare/cloudflare/latest/docs
- Terraform Community: https://discuss.hashicorp.com/c/terraform-core/
