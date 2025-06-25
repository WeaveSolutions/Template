#!/bin/bash
# Setup development environment for OCI

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

echo -e "${YELLOW}Setting up OCI development environment...${NC}"

# Check OCI CLI
if ! command -v oci &> /dev/null; then
    echo -e "${RED}OCI CLI not found. Installing...${NC}"
    if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" ]]; then
        echo "Please install OCI CLI from: https://docs.oracle.com/en-us/iaas/Content/API/SDKDocs/cliinstall.htm"
    else
        bash -c "$(curl -L https://raw.githubusercontent.com/oracle/oci-cli/master/scripts/install/install.sh)"
    fi
fi

# Check kubectl
if ! command -v kubectl &> /dev/null; then
    echo -e "${YELLOW}kubectl not found. Please install kubectl${NC}"
    echo "Visit: https://kubernetes.io/docs/tasks/tools/"
fi

# Check Docker
if ! command -v docker &> /dev/null; then
    echo -e "${YELLOW}Docker not found. Please install Docker${NC}"
    echo "Visit: https://docs.docker.com/get-docker/"
fi

# Check Terraform
if ! command -v terraform &> /dev/null; then
    echo -e "${YELLOW}Terraform not found. Please install Terraform 1.5.0+${NC}"
    echo "Visit: https://www.terraform.io/downloads"
else
    TERRAFORM_VERSION=$(terraform version -json | jq -r '.terraform_version')
    echo -e "${GREEN}Terraform version: $TERRAFORM_VERSION${NC}"
fi

# Configure OCI CLI if not already configured
if [ ! -f ~/.oci/config ]; then
    echo -e "${YELLOW}OCI CLI not configured. Running setup...${NC}"
    oci setup config
fi

# Test OCI connectivity
echo -e "${YELLOW}Testing OCI connectivity...${NC}"
if oci iam user get --user-id="$OCI_CLI_USER" &> /dev/null; then
    echo -e "${GREEN}OCI CLI configured successfully${NC}"
else
    echo -e "${RED}OCI CLI configuration failed. Please check your credentials${NC}"
    exit 1
fi

# Create .env file template
if [ ! -f .env ]; then
    echo -e "${YELLOW}Creating .env template...${NC}"
    cat > .env << 'EOF'
# OCI Configuration
export OCI_USER_OCID=ocid1.user.oc1..aaaaaaa...
export OCI_TENANCY_OCID=ocid1.tenancy.oc1..aaaaaaa...
export OCI_FINGERPRINT=00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00
export OCI_REGION=us-phoenix-1
export OCI_COMPARTMENT_ID=ocid1.compartment.oc1..aaaaaaa...

# Auth0 Configuration
export AUTH0_DOMAIN=your-domain.auth0.com
export AUTH0_CLIENT_ID=your-client-id
export AUTH0_CLIENT_SECRET=your-client-secret
export AUTH0_AUDIENCE=your-api-audience
export AUTH0_ISSUER_BASE_URL=https://your-domain.auth0.com

# Application Configuration
export PROJECT_NAME=Nexpo
export ENVIRONMENT=dev
EOF
    echo -e "${GREEN}.env template created. Please update with your values${NC}"
fi

# Create terraform.tfvars if not exists
if [ ! -f environments/dev/terraform.tfvars ]; then
    echo -e "${YELLOW}Creating terraform.tfvars from template...${NC}"
    cp environments/dev/terraform.tfvars.example environments/dev/terraform.tfvars
    echo -e "${GREEN}terraform.tfvars created. Please update with your values${NC}"
fi

# Install pre-commit hooks
if command -v pre-commit &> /dev/null; then
    echo -e "${YELLOW}Installing pre-commit hooks...${NC}"
    cat > .pre-commit-config.yaml << 'EOF'
repos:
  - repo: https://github.com/antonbabenko/pre-commit-terraform
    rev: v1.83.0
    hooks:
      - id: terraform_fmt
      - id: terraform_validate
      - id: terraform_docs
      - id: terraform_tflint
EOF
    pre-commit install
    echo -e "${GREEN}Pre-commit hooks installed${NC}"
fi

echo -e "${GREEN}Development environment setup complete!${NC}"
echo ""
echo -e "${YELLOW}Next steps:${NC}"
echo "1. Update .env with your OCI credentials"
echo "2. Update environments/dev/terraform.tfvars with your configuration"
echo "3. Run 'source .env' to load environment variables"
echo "4. Run 'make init ENV=dev' to initialize Terraform"
