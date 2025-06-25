# OCI Terraform Makefile

.PHONY: help init plan apply destroy fmt validate clean

# Default environment
ENV ?= dev

# Color output
RED=\033[0;31m
GREEN=\033[0;32m
YELLOW=\033[0;33m
NC=\033[0m # No Color

help: ## Show this help message
	@echo 'Usage: make [target] ENV=dev|staging|prod'
	@echo ''
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  ${GREEN}%-15s${NC} %s\n", $$1, $$2}' $(MAKEFILE_LIST)
	@echo ''
	@echo 'Examples:'
	@echo '  make init ENV=dev        Initialize Terraform for dev environment'
	@echo '  make plan ENV=prod       Plan changes for production'
	@echo '  make apply ENV=staging   Apply changes to staging'

check-env: ## Check if environment is set
	@if [ -z "$(ENV)" ]; then \
		echo "${RED}Error: ENV is not set${NC}"; \
		echo "Usage: make [target] ENV=dev|staging|prod"; \
		exit 1; \
	fi
	@if [ ! -d "environments/$(ENV)" ]; then \
		echo "${RED}Error: Environment '$(ENV)' does not exist${NC}"; \
		echo "Available environments:"; \
		ls -d environments/*/ | xargs -n 1 basename; \
		exit 1; \
	fi

check-auth: ## Verify OCI authentication
	@echo "${YELLOW}Checking OCI authentication...${NC}"
	@oci iam user get --user-id=$${OCI_CLI_USER} > /dev/null 2>&1 || \
		(echo "${RED}Error: OCI CLI not configured properly${NC}" && exit 1)
	@echo "${GREEN}OCI authentication verified${NC}"

init: check-env check-auth ## Initialize Terraform
	@echo "${YELLOW}Initializing Terraform for $(ENV) environment...${NC}"
	@cd environments/$(ENV) && \
		terraform init -upgrade
	@echo "${GREEN}Initialization complete${NC}"

validate: check-env ## Validate Terraform configuration
	@echo "${YELLOW}Validating Terraform configuration...${NC}"
	@cd environments/$(ENV) && \
		terraform validate
	@echo "${GREEN}Validation successful${NC}"

fmt: ## Format Terraform files
	@echo "${YELLOW}Formatting Terraform files...${NC}"
	@terraform fmt -recursive .
	@echo "${GREEN}Formatting complete${NC}"

plan: check-env validate ## Plan Terraform changes
	@echo "${YELLOW}Planning Terraform changes for $(ENV)...${NC}"
	@cd environments/$(ENV) && \
		terraform plan -var-file="terraform.tfvars" -out=tfplan
	@echo "${GREEN}Plan complete. Review the plan above.${NC}"

apply: check-env ## Apply Terraform changes
	@echo "${YELLOW}Applying Terraform changes for $(ENV)...${NC}"
	@cd environments/$(ENV) && \
		if [ -f tfplan ]; then \
			terraform apply tfplan; \
		else \
			echo "${RED}Error: No plan file found. Run 'make plan' first.${NC}"; \
			exit 1; \
		fi
	@echo "${GREEN}Apply complete${NC}"

destroy: check-env ## Destroy Terraform resources
	@echo "${RED}WARNING: This will destroy all resources in $(ENV)${NC}"
	@echo "Press Ctrl+C to cancel, or Enter to continue..."
	@read confirm
	@cd environments/$(ENV) && \
		terraform destroy -var-file="terraform.tfvars"

clean: ## Clean up Terraform files
	@echo "${YELLOW}Cleaning up...${NC}"
	@find . -type f -name "*.tfplan" -delete
	@find . -type f -name "tfplan" -delete
	@find . -type f -name "*.tfstate*" -delete
	@find . -type d -name ".terraform" -exec rm -rf {} + 2>/dev/null || true
	@echo "${GREEN}Cleanup complete${NC}"

output: check-env ## Show Terraform outputs
	@cd environments/$(ENV) && \
		terraform output -json | jq '.'

refresh: check-env ## Refresh Terraform state
	@echo "${YELLOW}Refreshing Terraform state for $(ENV)...${NC}"
	@cd environments/$(ENV) && \
		terraform refresh -var-file="terraform.tfvars"
	@echo "${GREEN}Refresh complete${NC}"

# OCI-specific targets

create-backend: ## Create Object Storage backend
	@echo "${YELLOW}Creating Object Storage backend...${NC}"
	@./scripts/create-backend.sh
	@echo "${GREEN}Backend created${NC}"

test-connectivity: check-auth ## Test OCI connectivity
	@echo "${YELLOW}Testing OCI connectivity...${NC}"
	@echo "Tenancy: $$(oci iam tenancy get --tenancy-id=$${OCI_CLI_TENANCY} --query 'data.name' --raw-output)"
	@echo "Region: $${OCI_CLI_REGION}"
	@echo "User: $$(oci iam user get --user-id=$${OCI_CLI_USER} --query 'data.name' --raw-output)"
	@echo "${GREEN}Connectivity test passed${NC}"

list-resources: check-env check-auth ## List deployed resources
	@echo "${YELLOW}Listing resources in $(ENV)...${NC}"
	@echo "\n=== Compute Instances ==="
	@oci compute instance list --compartment-id=$${OCI_COMPARTMENT_ID} --query 'data[].{Name:"display-name",State:"lifecycle-state"}' --output table
	@echo "\n=== Load Balancers ==="
	@oci lb load-balancer list --compartment-id=$${OCI_COMPARTMENT_ID} --query 'data[].{Name:"display-name",State:"lifecycle-state"}' --output table
	@echo "\n=== Autonomous Databases ==="
	@oci db autonomous-database list --compartment-id=$${OCI_COMPARTMENT_ID} --query 'data[].{Name:"display-name",State:"lifecycle-state"}' --output table

cost-estimate: check-env ## Estimate monthly costs
	@echo "${YELLOW}Estimating costs for $(ENV)...${NC}"
	@cd environments/$(ENV) && \
		terraform plan -var-file="terraform.tfvars" -out=tfplan > /dev/null && \
		terraform show -json tfplan | \
		jq -r '.resource_changes[] | select(.change.actions[] == "create") | {type:.type, name:.name}'
	@echo "${YELLOW}Note: Use OCI Cost Estimator for accurate pricing${NC}"

# Container operations

build-images: ## Build Docker images
	@echo "${YELLOW}Building Docker images...${NC}"
	@./scripts/build-images.sh
	@echo "${GREEN}Images built successfully${NC}"

push-images: check-auth ## Push images to OCIR
	@echo "${YELLOW}Pushing images to OCIR...${NC}"
	@./scripts/push-images.sh
	@echo "${GREEN}Images pushed successfully${NC}"

deploy-k8s: check-env ## Deploy to OKE
	@echo "${YELLOW}Deploying to OKE in $(ENV)...${NC}"
	@./scripts/deploy-k8s.sh $(ENV)
	@echo "${GREEN}Deployment complete${NC}"

# Monitoring

logs: check-env ## View application logs
	@echo "${YELLOW}Fetching logs from $(ENV)...${NC}"
	@oci logging-search search-logs \
		--compartment-id=$${OCI_COMPARTMENT_ID} \
		--search-query="search \"$${OCI_COMPARTMENT_ID}\" | where type='custom' | sort by datetime desc" \
		--time-start=$$(date -u -d '1 hour ago' +%Y-%m-%dT%H:%M:%S.000Z) \
		--time-end=$$(date -u +%Y-%m-%dT%H:%M:%S.000Z)

metrics: check-env ## View metrics dashboard
	@echo "${YELLOW}Opening metrics dashboard for $(ENV)...${NC}"
	@echo "Dashboard URL: https://console.$${OCI_CLI_REGION}.oraclecloud.com/monitoring/dashboards"
	@echo "Compartment: $${OCI_COMPARTMENT_ID}"

# Development helpers

setup-dev: ## Setup development environment
	@echo "${YELLOW}Setting up development environment...${NC}"
	@./scripts/setup-dev.sh
	@echo "${GREEN}Development environment ready${NC}"

test-modules: ## Test individual modules
	@echo "${YELLOW}Testing Terraform modules...${NC}"
	@cd modules && \
		for module in */; do \
			echo "Testing $$module..."; \
			cd $$module && terraform init && terraform validate && cd ..; \
		done
	@echo "${GREEN}All modules validated${NC}"
