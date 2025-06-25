.PHONY: help init-dev init-prod plan-dev plan-prod apply-dev apply-prod destroy-dev destroy-prod fmt validate clean docs cost-dev cost-prod test lint

# Default environment
ENV ?= dev

# Colors for output
RED := \033[0;31m
GREEN := \033[0;32m
YELLOW := \033[0;33m
NC := \033[0m # No Color

help: ## Show this help message
	@echo 'Usage: make [target] [ENV=dev|prod]'
	@echo ''
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  ${GREEN}%-15s${NC} %s\n", $$1, $$2}' $(MAKEFILE_LIST)

# Setup and Installation
setup: ## Install required tools
	@echo "${YELLOW}Installing required tools...${NC}"
	@command -v terraform >/dev/null 2>&1 || { echo "${RED}Terraform not installed. Please install terraform.${NC}" >&2; exit 1; }
	@command -v tflint >/dev/null 2>&1 || { echo "${YELLOW}Installing tflint...${NC}"; curl -s https://raw.githubusercontent.com/terraform-linters/tflint/master/install_linux.sh | bash; }
	@command -v pre-commit >/dev/null 2>&1 || { echo "${YELLOW}Installing pre-commit...${NC}"; pip install pre-commit; }
	@pre-commit install
	@echo "${GREEN}Setup complete!${NC}"

# Terraform Commands
init-dev: ## Initialize Terraform for development
	@echo "${YELLOW}Initializing Terraform for development...${NC}"
	@cd environments/dev && terraform init

init-prod: ## Initialize Terraform for production
	@echo "${YELLOW}Initializing Terraform for production...${NC}"
	@cd environments/prod && terraform init

plan-dev: ## Plan Terraform changes for development
	@echo "${YELLOW}Planning Terraform changes for development...${NC}"
	@cd environments/dev && terraform plan -var-file=terraform.tfvars

plan-prod: ## Plan Terraform changes for production
	@echo "${YELLOW}Planning Terraform changes for production...${NC}"
	@cd environments/prod && terraform plan -var-file=terraform.tfvars

apply-dev: ## Apply Terraform changes for development
	@echo "${YELLOW}Applying Terraform changes for development...${NC}"
	@cd environments/dev && terraform apply -var-file=terraform.tfvars

apply-prod: ## Apply Terraform changes for production
	@echo "${RED}WARNING: This will apply changes to PRODUCTION!${NC}"
	@echo "Press Ctrl+C to cancel, or Enter to continue..."
	@read confirm
	@cd environments/prod && terraform apply -var-file=terraform.tfvars

destroy-dev: ## Destroy Terraform resources for development
	@echo "${RED}WARNING: This will destroy all development resources!${NC}"
	@echo "Press Ctrl+C to cancel, or Enter to continue..."
	@read confirm
	@cd environments/dev && terraform destroy -var-file=terraform.tfvars

destroy-prod: ## Destroy Terraform resources for production
	@echo "${RED}DANGER: This will destroy all PRODUCTION resources!${NC}"
	@echo "${RED}Are you absolutely sure? Type 'destroy-production' to confirm:${NC}"
	@read confirm && [ "$$confirm" = "destroy-production" ] || { echo "${YELLOW}Cancelled.${NC}"; exit 1; }
	@cd environments/prod && terraform destroy -var-file=terraform.tfvars

# Formatting and Validation
fmt: ## Format Terraform files
	@echo "${YELLOW}Formatting Terraform files...${NC}"
	@terraform fmt -recursive .
	@echo "${GREEN}Formatting complete!${NC}"

validate: ## Validate Terraform configuration
	@echo "${YELLOW}Validating Terraform configuration...${NC}"
	@for dir in modules/*/; do \
		echo "Validating $$dir..."; \
		cd $$dir && terraform init -backend=false && terraform validate || exit 1; \
		cd ../..; \
	done
	@echo "${GREEN}Validation complete!${NC}"

lint: ## Run TFLint on all Terraform files
	@echo "${YELLOW}Running TFLint...${NC}"
	@tflint --init
	@tflint --recursive
	@echo "${GREEN}Linting complete!${NC}"

# Documentation
docs: ## Generate documentation for Terraform modules
	@echo "${YELLOW}Generating documentation...${NC}"
	@for dir in modules/*/; do \
		echo "Generating docs for $$dir..."; \
		terraform-docs markdown $$dir > $$dir/README.md; \
	done
	@echo "${GREEN}Documentation generated!${NC}"

# Cost Estimation
cost-dev: ## Estimate costs for development environment
	@echo "${YELLOW}Estimating costs for development...${NC}"
	@cd environments/dev && \
		terraform plan -out=tfplan.binary && \
		terraform show -json tfplan.binary > tfplan.json && \
		infracost breakdown --path tfplan.json

cost-prod: ## Estimate costs for production environment
	@echo "${YELLOW}Estimating costs for production...${NC}"
	@cd environments/prod && \
		terraform plan -out=tfplan.binary && \
		terraform show -json tfplan.binary > tfplan.json && \
		infracost breakdown --path tfplan.json

# Testing
test: ## Run Terraform tests
	@echo "${YELLOW}Running tests...${NC}"
	@pre-commit run --all-files
	@echo "${GREEN}Tests complete!${NC}"

# Utility Commands
clean: ## Clean up Terraform files
	@echo "${YELLOW}Cleaning up...${NC}"
	@find . -type f -name "*.tfplan" -delete
	@find . -type f -name "*.tfstate.*" -delete
	@find . -type f -name ".terraform.lock.hcl" -delete
	@find . -type d -name ".terraform" -exec rm -rf {} + 2>/dev/null || true
	@echo "${GREEN}Cleanup complete!${NC}"

graph-dev: ## Generate dependency graph for development
	@echo "${YELLOW}Generating dependency graph for development...${NC}"
	@cd environments/dev && terraform graph | dot -Tpng > ../../terraform-graph-dev.png
	@echo "${GREEN}Graph saved to terraform-graph-dev.png${NC}"

graph-prod: ## Generate dependency graph for production
	@echo "${YELLOW}Generating dependency graph for production...${NC}"
	@cd environments/prod && terraform graph | dot -Tpng > ../../terraform-graph-prod.png
	@echo "${GREEN}Graph saved to terraform-graph-prod.png${NC}"

# Quick commands
quick-dev: init-dev plan-dev ## Quick setup for development (init + plan)

quick-prod: init-prod plan-prod ## Quick setup for production (init + plan)

# Environment-specific targets
init: init-$(ENV) ## Initialize for specified environment (default: dev)

plan: plan-$(ENV) ## Plan for specified environment (default: dev)

apply: apply-$(ENV) ## Apply for specified environment (default: dev)

destroy: destroy-$(ENV) ## Destroy for specified environment (default: dev)

cost: cost-$(ENV) ## Estimate costs for specified environment (default: dev)
