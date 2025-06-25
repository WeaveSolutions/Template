# CI/CD Setup for IBM Cloud Infrastructure

This document provides detailed instructions for setting up Continuous Integration and Continuous Deployment (CI/CD) for your IBM Cloud infrastructure managed with Terraform.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [CI/CD Options](#cicd-options)
3. [GitHub Actions Setup](#github-actions-setup)
4. [GitLab CI Setup](#gitlab-ci-setup)
5. [Jenkins Setup](#jenkins-setup)
6. [IBM Cloud DevOps Setup](#ibm-cloud-devops-setup)
7. [Terraform Backend Configuration](#terraform-backend-configuration)
8. [Security Best Practices](#security-best-practices)
9. [Troubleshooting](#troubleshooting)

## Prerequisites

Before setting up CI/CD for IBM Cloud with Terraform, ensure you have:

- IBM Cloud API key with appropriate permissions
- IBM Cloud CLI installed
- Terraform 1.0.0+ installed
- Git repository for your infrastructure code

## CI/CD Options

You can use several CI/CD platforms with IBM Cloud:

1. **GitHub Actions**: Great for GitHub repositories, with simple YAML configuration
2. **GitLab CI/CD**: Integrated solution for GitLab repositories
3. **Jenkins**: Self-hosted option with extensive customization
4. **IBM Cloud DevOps**: Native IBM Cloud solution with tight integration
5. **Other Options**: CircleCI, Travis CI, Azure DevOps, etc.

## GitHub Actions Setup

### Sample GitHub Actions Workflow

Create `.github/workflows/terraform-ibm.yml` in your repository:

```yaml
name: 'Terraform IBM Cloud'

on:
  push:
    branches: [ main ]
    paths: [ 'terraform/providers/ibm/**' ]
  pull_request:
    branches: [ main ]
    paths: [ 'terraform/providers/ibm/**' ]

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
        TF_VAR_ibmcloud_api_key: ${{ secrets.IBM_CLOUD_API_KEY }}

    - name: Terraform Validate
      run: terraform validate

    - name: Terraform Plan
      if: github.event_name == 'pull_request'
      run: terraform plan -var-file=ibm.tfvars -var="enable_ibm=true"
      env:
        TF_VAR_ibmcloud_api_key: ${{ secrets.IBM_CLOUD_API_KEY }}

    - name: Terraform Apply
      if: github.ref == 'refs/heads/main' && github.event_name == 'push'
      run: terraform apply -auto-approve -var-file=ibm.tfvars -var="enable_ibm=true"
      env:
        TF_VAR_ibmcloud_api_key: ${{ secrets.IBM_CLOUD_API_KEY }}
```

### Setting Up GitHub Secrets

1. Go to your GitHub repository → Settings → Secrets and variables → Actions
2. Add these secrets:
   - `IBM_CLOUD_API_KEY`: Your IBM Cloud API key

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
    - terraform plan -var-file=ibm.tfvars -var="enable_ibm=true" -out=tfplan
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
   - `TF_VAR_ibmcloud_api_key`: Your IBM Cloud API key (Masked: Yes, Protected: Yes)

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
        IBM_CLOUD_API_KEY = credentials('ibm-cloud-api-key')
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
                    sh 'terraform plan -var-file=ibm.tfvars -var="enable_ibm=true" -out=tfplan'
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
2. Add a Secret Text credential with ID `ibm-cloud-api-key` containing your IBM Cloud API key

## IBM Cloud DevOps Setup

IBM Cloud provides its own CI/CD solution via IBM Cloud Continuous Delivery service.

### Setting Up IBM Cloud DevOps Toolchain

1. Log in to IBM Cloud Console
2. Navigate to DevOps → Toolchains → Create Toolchain
3. Choose "Develop a Terraform template" or "Simple Cloud Foundry toolchain"
4. Connect your Git repository (GitHub, GitLab, or IBM Cloud Git)
5. Configure the Delivery Pipeline:
   - Add a stage for validation
   - Add a stage for planning
   - Add a stage for deployment

### Sample Delivery Pipeline Configuration

**Validation Stage:**
```bash
# Install terraform-docs if needed
if [ ! -f /usr/local/bin/terraform-docs ]; then
  curl -Lo ./terraform-docs.tar.gz https://github.com/terraform-docs/terraform-docs/releases/download/v0.16.0/terraform-docs-v0.16.0-$(uname)-amd64.tar.gz
  tar -xzf terraform-docs.tar.gz
  chmod +x terraform-docs
  mv terraform-docs /usr/local/bin/
fi

cd ${WORKSPACE}/terraform
terraform init
terraform validate
terraform fmt -check -recursive
```

**Plan Stage:**
```bash
cd ${WORKSPACE}/terraform
terraform init
terraform plan -var-file=ibm.tfvars -var="enable_ibm=true" -out=tfplan
```

**Deploy Stage:**
```bash
cd ${WORKSPACE}/terraform
terraform init
terraform apply -auto-approve tfplan
```

## Terraform Backend Configuration

For any CI/CD setup, it's recommended to use a remote backend for Terraform state:

### IBM Cloud Object Storage Backend

Create a `backend.tf` file in your Terraform directory:

```hcl
terraform {
  backend "cos" {
    bucket                   = "terraform-state-bucket"
    key                      = "terraform/ibm-cloud/terraform.tfstate"
    region                   = "us-south"
    endpoint                 = "s3.us-south.cloud-object-storage.appdomain.cloud"
    skip_region_validation   = true
    skip_credentials_validation = true
  }
}
```

### Other Backend Options

You can also use:
- **Terraform Cloud**
- **AWS S3** (with the proper configuration)
- **Azure Storage Account**
- **Google Cloud Storage**

## Security Best Practices

1. **Never** commit API keys or credentials to your repository
2. Use environment variables or secrets management in your CI/CD platform
3. Implement IP restriction for CI/CD runners when possible
4. Use IAM roles with the principle of least privilege
5. Regularly rotate API keys
6. Enable audit logging for all deployments
7. Implement approval gates for production deployments
8. Scan IaC for security misconfigurations using tools like Checkov or tfsec

## Troubleshooting

### Common Issues

1. **Authentication Failures**
   - Check if your IBM Cloud API key has the correct permissions
   - Verify the API key has not expired or been revoked

2. **Terraform Initialization Failures**
   - Check network connectivity to the Terraform registry
   - Verify backend access and credentials

3. **Terraform Plan/Apply Failures**
   - Review the error message for resource-specific issues
   - Check for version compatibility issues
   - Verify that required provider plugins are available

### Getting Help

- IBM Cloud Support: https://cloud.ibm.com/unifiedsupport/supportcenter
- IBM Cloud Terraform Provider: https://registry.terraform.io/providers/IBM-Cloud/ibm/latest/docs
- IBM Developer Community: https://developer.ibm.com/answers/
