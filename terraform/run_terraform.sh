#!/bin/bash
# Terraform execution script for Mac/Linux users
# Equivalent to run_terraform.ps1 for Windows

# Default values
OUTPUT_PATH="./terraform-output.json"
TERRAFORM_DIR="$(dirname "$(realpath "$0")")"
ROOT_DIR="$(dirname "$TERRAFORM_DIR")"
TFVARS_FILE="${TERRAFORM_DIR}/terraform.tfvars"
ENV_FILE="${ROOT_DIR}/.env"

# Load environment variables from .env file if it exists
if [[ -f "$ENV_FILE" ]]; then
  echo "Loading environment variables from .env file..."
  while IFS='=' read -r key value || [[ -n "$key" ]]; do
    # Skip comments and empty lines
    [[ $key =~ ^\s*# ]] && continue
    [[ -z "$key" ]] && continue
    
    # Remove quotes if present and any comments after the value
    value=$(echo "$value" | sed -E 's/^["\''](.*)["\''']$/\1/' | sed -E 's/#.*$//' | xargs)
    
    # Handle PROJECT_NAME specially
    if [[ "$key" == "PROJECT_NAME" ]]; then
      export TF_VAR_project_name="${value}"
      echo "  Set TF_VAR_project_name from PROJECT_NAME environment variable"
    else
      # Export as TF_VAR_* environment variable for other variables
      export TF_VAR_${key}="${value}"
      echo "  Set TF_VAR_${key} environment variable"
    fi
  done < "$ENV_FILE"
  echo "Environment variables loaded"
fi

# Parse command line arguments
INIT=false
PLAN=false
APPLY=false
DESTROY=false

function show_usage {
  echo "Usage: ./run_terraform.sh [options]"
  echo "Options:"
  echo "  --init        Initialize Terraform"
  echo "  --plan        Run Terraform plan"
  echo "  --apply       Apply Terraform configuration"
  echo "  --destroy     Destroy Terraform resources"
  echo "  --output PATH Save output to specified path (default: ./terraform-output.json)"
  echo "  --help        Show this help message"
  echo ""
  echo "Examples:"
  echo "  ./run_terraform.sh --init"
  echo "  ./run_terraform.sh --plan"
  echo "  ./run_terraform.sh --apply"
  exit 1
}

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --init)
      INIT=true
      shift
      ;;
    --plan)
      PLAN=true
      shift
      ;;
    --apply)
      APPLY=true
      shift
      ;;
    --destroy)
      DESTROY=true
      shift
      ;;
    --output)
      OUTPUT_PATH="$2"
      shift 2
      ;;
    --help)
      show_usage
      ;;
    *)
      echo "Unknown option: $1"
      show_usage
      ;;
  esac
done

# Validate at least one action is specified
if [[ "$INIT" != "true" && "$PLAN" != "true" && "$APPLY" != "true" && "$DESTROY" != "true" ]]; then
  echo "Error: You must specify at least one action: --init, --plan, --apply, or --destroy"
  show_usage
fi

# Check if terraform is installed
if ! command -v terraform &> /dev/null; then
  echo "Error: Terraform is not installed or not in PATH."
  echo "Please install Terraform first: https://developer.hashicorp.com/terraform/downloads"
  exit 1
fi

# Check if terraform.tfvars exists
if [[ ! -f "$TFVARS_FILE" ]]; then
  echo "Warning: terraform.tfvars file not found. Creating from example file..."
  
  EXAMPLE_FILE="${TERRAFORM_DIR}/terraform.tfvars.example"
  if [[ -f "$EXAMPLE_FILE" ]]; then
    cp "$EXAMPLE_FILE" "$TFVARS_FILE"
    echo "Created terraform.tfvars from example. Please review and modify if needed."
  else
    echo "Error: terraform.tfvars.example not found. Please create terraform.tfvars manually."
    exit 1
  fi
fi

# Change to terraform directory
pushd "$TERRAFORM_DIR" > /dev/null

# Initialize if requested
if [[ "$INIT" == "true" ]]; then
  echo "Initializing Terraform..."
  terraform init
  
  if [[ $? -ne 0 ]]; then
    echo "Terraform init failed"
    popd > /dev/null
    exit 1
  fi
  
  echo "Terraform initialization complete"
fi

# Plan if requested
if [[ "$PLAN" == "true" ]]; then
  echo "Running Terraform plan..."
  terraform plan -var-file="$TFVARS_FILE" -out="terraform.tfplan"
  
  if [[ $? -ne 0 ]]; then
    echo "Terraform plan failed"
    popd > /dev/null
    exit 1
  fi
  
  echo "Terraform plan complete"
fi

# Apply if requested
if [[ "$APPLY" == "true" ]]; then
  echo "Running Terraform apply..."
  
  # Check if we have a plan file
  if [[ -f "terraform.tfplan" ]]; then
    terraform apply "terraform.tfplan"
  else
    # Ask for confirmation when applying without a plan
    read -p "No plan file found. Do you want to apply without a plan? (y/n): " confirmation
    if [[ "$confirmation" == "y" ]]; then
      terraform apply -var-file="$TFVARS_FILE" -auto-approve
    else
      echo "Apply canceled"
      popd > /dev/null
      exit 0
    fi
  fi
  
  if [[ $? -ne 0 ]]; then
    echo "Terraform apply failed"
    popd > /dev/null
    exit 1
  fi
  
  # Save output to file if apply was successful
  terraform output -json > "$OUTPUT_PATH"
  echo "Terraform output saved to $OUTPUT_PATH"
  echo "Terraform apply complete"
fi

# Destroy if requested
if [[ "$DESTROY" == "true" ]]; then
  echo "WARNING: This will destroy all resources managed by Terraform"
  read -p "Are you sure you want to destroy? Type 'yes' to confirm: " confirmation
  
  if [[ "$confirmation" == "yes" ]]; then
    echo "Running Terraform destroy..."
    terraform destroy -var-file="$TFVARS_FILE" -auto-approve
    
    if [[ $? -ne 0 ]]; then
      echo "Terraform destroy failed"
      popd > /dev/null
      exit 1
    fi
    
    echo "Terraform destroy complete"
  else
    echo "Destroy canceled"
  fi
fi

# Return to original directory
popd > /dev/null
