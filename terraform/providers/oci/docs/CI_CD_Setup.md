# OCI CI/CD Setup Guide

This guide covers setting up CI/CD pipelines for Oracle Cloud Infrastructure (OCI) deployments using GitHub Actions, GitLab CI, or Oracle DevOps.

## Prerequisites

- OCI tenancy with appropriate permissions
- OCI CLI configured locally
- Container Registry access
- API key for service account

## GitHub Actions Setup

### 1. Create Service Principal

```bash
# Create a group for CI/CD
oci iam group create \
  --name "cicd-group" \
  --description "Group for CI/CD operations"

# Create a user for CI/CD
oci iam user create \
  --name "cicd-user" \
  --description "User for CI/CD operations"

# Add user to group
oci iam group add-user \
  --group-id <group-ocid> \
  --user-id <user-ocid>

# Generate API key
oci iam user api-key upload \
  --user-id <user-ocid> \
  --key-file ~/.oci/cicd_api_key_public.pem
```

### 2. Create Required Policies

```hcl
# policies.tf
resource "oci_identity_policy" "cicd_policy" {
  compartment_id = var.tenancy_ocid
  name           = "cicd-policy"
  description    = "Policy for CI/CD operations"
  
  statements = [
    # Container Registry access
    "Allow group cicd-group to manage repos in tenancy",
    "Allow group cicd-group to use container-images in tenancy",
    
    # OKE access
    "Allow group cicd-group to use clusters in compartment ${var.compartment_name}",
    "Allow group cicd-group to manage cluster-node-pools in compartment ${var.compartment_name}",
    
    # Object Storage access for Terraform state
    "Allow group cicd-group to manage objects in compartment ${var.compartment_name} where target.bucket.name='terraform-state'",
    
    # Resource Manager (optional for OCI Resource Manager)
    "Allow group cicd-group to manage orm-stacks in compartment ${var.compartment_name}",
    "Allow group cicd-group to manage orm-jobs in compartment ${var.compartment_name}"
  ]
}
```

### 3. Store Secrets in GitHub

Add the following secrets to your GitHub repository:

- `OCI_USER_OCID`: User OCID for CI/CD user
- `OCI_TENANCY_OCID`: Tenancy OCID
- `OCI_FINGERPRINT`: API key fingerprint
- `OCI_PRIVATE_KEY`: Base64 encoded private key
- `OCI_REGION`: OCI region (e.g., us-phoenix-1)
- `OCI_COMPARTMENT_OCID`: Compartment OCID

### 4. GitHub Actions Workflow

```yaml
# .github/workflows/oci-deploy.yml
name: Deploy to OCI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

env:
  OCI_CLI_USER: ${{ secrets.OCI_USER_OCID }}
  OCI_CLI_TENANCY: ${{ secrets.OCI_TENANCY_OCID }}
  OCI_CLI_FINGERPRINT: ${{ secrets.OCI_FINGERPRINT }}
  OCI_CLI_KEY_CONTENT: ${{ secrets.OCI_PRIVATE_KEY }}
  OCI_CLI_REGION: ${{ secrets.OCI_REGION }}

jobs:
  terraform:
    runs-on: ubuntu-latest
    
    steps:
    - name: Checkout code
      uses: actions/checkout@v3
    
    - name: Setup OCI CLI
      run: |
        # Install OCI CLI
        curl -L https://raw.githubusercontent.com/oracle/oci-cli/master/scripts/install/install.sh | bash
        
        # Configure OCI CLI
        mkdir -p ~/.oci
        echo "${{ secrets.OCI_PRIVATE_KEY }}" | base64 -d > ~/.oci/key.pem
        chmod 600 ~/.oci/key.pem
        
        # Create config file
        cat > ~/.oci/config << EOF
        [DEFAULT]
        user=${{ secrets.OCI_USER_OCID }}
        fingerprint=${{ secrets.OCI_FINGERPRINT }}
        tenancy=${{ secrets.OCI_TENANCY_OCID }}
        region=${{ secrets.OCI_REGION }}
        key_file=~/.oci/key.pem
        EOF
        
        chmod 600 ~/.oci/config
    
    - name: Setup Terraform
      uses: hashicorp/setup-terraform@v2
      with:
        terraform_version: 1.5.0
    
    - name: Terraform Init
      working-directory: ./terraform
      run: |
        terraform init \
          -backend-config="address=https://objectstorage.${{ secrets.OCI_REGION }}.oraclecloud.com/p/<pre-auth-request>/n/<namespace>/b/terraform-state/o/terraform.tfstate" \
          -backend-config="update_method=PUT"
    
    - name: Terraform Plan
      working-directory: ./terraform
      run: |
        terraform plan \
          -var="enable_oci=true" \
          -var="compartment_ocid=${{ secrets.OCI_COMPARTMENT_OCID }}" \
          -var="environment=${{ github.ref == 'refs/heads/main' && 'prod' || 'dev' }}" \
          -out=tfplan
    
    - name: Terraform Apply
      if: github.ref == 'refs/heads/main' && github.event_name == 'push'
      working-directory: ./terraform
      run: terraform apply -auto-approve tfplan
  
  build-and-push:
    runs-on: ubuntu-latest
    needs: terraform
    
    steps:
    - name: Checkout code
      uses: actions/checkout@v3
    
    - name: Setup Docker Buildx
      uses: docker/setup-buildx-action@v2
    
    - name: Login to OCI Registry
      run: |
        echo "${{ secrets.OCI_PRIVATE_KEY }}" | base64 -d > key.pem
        chmod 600 key.pem
        
        docker login -u "${{ secrets.OCI_TENANCY_OCID }}/${{ secrets.OCI_USER_OCID }}" \
          --password-stdin ${{ secrets.OCI_REGION }}.ocir.io < key.pem
    
    - name: Build and Push Images
      run: |
        # Build Next.js app
        docker build -t ${{ secrets.OCI_REGION }}.ocir.io/${{ secrets.OCI_TENANCY_OCID }}/next-app:${{ github.sha }} \
          -f apps/next/Dockerfile .
        
        docker push ${{ secrets.OCI_REGION }}.ocir.io/${{ secrets.OCI_TENANCY_OCID }}/next-app:${{ github.sha }}
        
        # Build microservices
        for service in auth user notifications; do
          docker build -t ${{ secrets.OCI_REGION }}.ocir.io/${{ secrets.OCI_TENANCY_OCID }}/${service}:${{ github.sha }} \
            -f services/${service}/Dockerfile services/${service}
          
          docker push ${{ secrets.OCI_REGION }}.ocir.io/${{ secrets.OCI_TENANCY_OCID }}/${service}:${{ github.sha }}
        done
  
  deploy:
    runs-on: ubuntu-latest
    needs: build-and-push
    if: github.ref == 'refs/heads/main' && github.event_name == 'push'
    
    steps:
    - name: Setup kubectl
      uses: oracle-actions/configure-kubectl-oke@v1
      with:
        cluster: ${{ secrets.OKE_CLUSTER_OCID }}
    
    - name: Deploy to OKE
      run: |
        # Update image tags in Kubernetes manifests
        kubectl set image deployment/next-app \
          next-app=${{ secrets.OCI_REGION }}.ocir.io/${{ secrets.OCI_TENANCY_OCID }}/next-app:${{ github.sha }} \
          -n production
        
        # Wait for rollout
        kubectl rollout status deployment/next-app -n production
```

## GitLab CI Setup

### 1. GitLab CI Configuration

```yaml
# .gitlab-ci.yml
stages:
  - validate
  - plan
  - deploy
  - build
  - release

variables:
  TF_ROOT: ${CI_PROJECT_DIR}/terraform
  OCI_CLI_USER: ${OCI_USER_OCID}
  OCI_CLI_TENANCY: ${OCI_TENANCY_OCID}
  OCI_CLI_FINGERPRINT: ${OCI_FINGERPRINT}
  OCI_CLI_KEY_CONTENT: ${OCI_PRIVATE_KEY}
  OCI_CLI_REGION: ${OCI_REGION}

before_script:
  - |
    # Install OCI CLI
    pip install oci-cli
    
    # Configure OCI
    mkdir -p ~/.oci
    echo "${OCI_PRIVATE_KEY}" | base64 -d > ~/.oci/key.pem
    chmod 600 ~/.oci/key.pem
    
    cat > ~/.oci/config << EOF
    [DEFAULT]
    user=${OCI_USER_OCID}
    fingerprint=${OCI_FINGERPRINT}
    tenancy=${OCI_TENANCY_OCID}
    region=${OCI_REGION}
    key_file=~/.oci/key.pem
    EOF

terraform:validate:
  stage: validate
  image: hashicorp/terraform:1.5.0
  script:
    - cd ${TF_ROOT}
    - terraform init
    - terraform validate
    - terraform fmt -check

terraform:plan:
  stage: plan
  image: hashicorp/terraform:1.5.0
  script:
    - cd ${TF_ROOT}
    - terraform init
    - terraform plan -out=plan.tfplan
  artifacts:
    paths:
      - ${TF_ROOT}/plan.tfplan

terraform:apply:
  stage: deploy
  image: hashicorp/terraform:1.5.0
  script:
    - cd ${TF_ROOT}
    - terraform init
    - terraform apply -auto-approve plan.tfplan
  dependencies:
    - terraform:plan
  only:
    - main

build:images:
  stage: build
  image: docker:latest
  services:
    - docker:dind
  script:
    - |
      # Login to OCIR
      echo "${OCI_PRIVATE_KEY}" | base64 -d | \
        docker login -u "${OCI_TENANCY_OCID}/${OCI_USER_OCID}" \
        --password-stdin ${OCI_REGION}.ocir.io
      
      # Build and push images
      docker build -t ${OCI_REGION}.ocir.io/${OCI_TENANCY_OCID}/next-app:${CI_COMMIT_SHA} .
      docker push ${OCI_REGION}.ocir.io/${OCI_TENANCY_OCID}/next-app:${CI_COMMIT_SHA}
  only:
    - main

deploy:oke:
  stage: release
  image: bitnami/kubectl:latest
  script:
    - |
      # Configure kubectl for OKE
      oci ce cluster create-kubeconfig \
        --cluster-id ${OKE_CLUSTER_OCID} \
        --file $HOME/.kube/config \
        --region ${OCI_REGION}
      
      # Deploy to OKE
      kubectl set image deployment/next-app \
        next-app=${OCI_REGION}.ocir.io/${OCI_TENANCY_OCID}/next-app:${CI_COMMIT_SHA} \
        -n production
      
      kubectl rollout status deployment/next-app -n production
  only:
    - main
```

## Oracle DevOps Setup

### 1. Create DevOps Project

```bash
# Create DevOps project
oci devops project create \
  --name "Nexpo" \
  --description "Nexpo deployment project" \
  --compartment-id ${COMPARTMENT_OCID}
```

### 2. Build Pipeline

```yaml
# build_spec.yaml
version: 0.1
component: build
timeoutInSeconds: 10000
shell: bash
env:
  exportedVariables:
    - BUILDRUN_HASH
    - IMAGE_TAG

steps:
  - type: Command
    name: "Build Docker Image"
    command: |
      export BUILDRUN_HASH=`echo ${OCI_BUILD_RUN_ID} | rev | cut -c 1-7`
      export IMAGE_TAG=${BUILDRUN_HASH}
      
      docker build -t next-app:${IMAGE_TAG} -f apps/next/Dockerfile .
      
  - type: Command
    name: "Push to OCIR"
    command: |
      docker tag next-app:${IMAGE_TAG} ${OCI_RESOURCE_PRINCIPAL_REGION}.ocir.io/${OCI_RESOURCE_PRINCIPAL_TENANCY}/next-app:${IMAGE_TAG}
      docker push ${OCI_RESOURCE_PRINCIPAL_REGION}.ocir.io/${OCI_RESOURCE_PRINCIPAL_TENANCY}/next-app:${IMAGE_TAG}

outputArtifacts:
  - name: deployment_manifest
    type: BINARY
    location: k8s/deployment.yaml
```

### 3. Deployment Pipeline

```yaml
# deployment_spec.yaml
version: 0.1
component: deployment
env:
  variables:
    namespace: "production"

steps:
  - type: Command
    name: "Deploy to OKE"
    command: |
      kubectl apply -f deployment.yaml -n ${namespace}
      kubectl set image deployment/next-app \
        next-app=${OCI_RESOURCE_PRINCIPAL_REGION}.ocir.io/${OCI_RESOURCE_PRINCIPAL_TENANCY}/next-app:${IMAGE_TAG} \
        -n ${namespace}
      kubectl rollout status deployment/next-app -n ${namespace}
```

## Environment Variables

### Required CI/CD Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `OCI_USER_OCID` | CI/CD user OCID | ocid1.user.oc1..aaa... |
| `OCI_TENANCY_OCID` | Tenancy OCID | ocid1.tenancy.oc1..aaa... |
| `OCI_FINGERPRINT` | API key fingerprint | aa:bb:cc:dd:ee:ff |
| `OCI_PRIVATE_KEY` | Base64 encoded private key | LS0tLS1CRU... |
| `OCI_REGION` | OCI region | us-phoenix-1 |
| `OCI_COMPARTMENT_OCID` | Compartment OCID | ocid1.compartment.oc1..aaa... |
| `OKE_CLUSTER_OCID` | OKE cluster OCID | ocid1.cluster.oc1..aaa... |

### Auth0 Variables

| Variable | Description |
|----------|-------------|
| `AUTH0_DOMAIN` | Auth0 domain |
| `AUTH0_CLIENT_ID` | Auth0 client ID |
| `AUTH0_CLIENT_SECRET` | Auth0 client secret |
| `AUTH0_AUDIENCE` | Auth0 API audience |

## Best Practices

1. **Use Dynamic Groups**: For OKE nodes and functions
2. **Implement Resource Tagging**: For cost tracking
3. **Enable Audit Logging**: For compliance
4. **Use Private Endpoints**: For enhanced security
5. **Implement Blue-Green Deployments**: For zero-downtime updates

## Monitoring CI/CD

### OCI Logging

```bash
# Create log group for CI/CD
oci logging log-group create \
  --compartment-id ${COMPARTMENT_OCID} \
  --display-name "cicd-logs"

# Create custom logs
oci logging log create \
  --log-group-id ${LOG_GROUP_ID} \
  --display-name "deployment-logs" \
  --log-type CUSTOM
```

### Metrics and Alarms

```bash
# Create alarm for failed deployments
oci monitoring alarm create \
  --compartment-id ${COMPARTMENT_OCID} \
  --display-name "deployment-failures" \
  --metric-compartment-id ${COMPARTMENT_OCID} \
  --namespace "oci_devops" \
  --query "BuildPipelineRunFailed[1m].count() > 0"
```

## Troubleshooting

### Common Issues

1. **OCIR Login Failures**
   - Verify user has correct policies
   - Check API key is properly configured
   - Ensure correct region endpoint

2. **OKE Deployment Issues**
   - Verify kubeconfig is correct
   - Check node pool is running
   - Ensure image pull secrets are configured

3. **Terraform State Issues**
   - Verify Object Storage bucket exists
   - Check pre-authenticated request is valid
   - Ensure state locking is configured

### Debug Commands

```bash
# Test OCI CLI configuration
oci iam user get --user-id ${OCI_USER_OCID}

# Test OCIR access
docker login ${OCI_REGION}.ocir.io

# Test OKE access
kubectl get nodes

# Check DevOps build logs
oci devops build-run get --build-run-id ${BUILD_RUN_ID}
```
