# MindsDB Multi-Cloud Makefile

.PHONY: help up down start stop restart logs init clean status backup restore

# Color output
RED=\033[0;31m
GREEN=\033[0;32m
YELLOW=\033[0;33m
NC=\033[0m

help: ## Show this help message
	@echo 'MindsDB Multi-Cloud Management'
	@echo ''
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  ${GREEN}%-15s${NC} %s\n", $$1, $$2}' $(MAKEFILE_LIST)

check-docker: ## Check if Docker is running
	@docker info > /dev/null 2>&1 || (echo "${RED}Docker is not running${NC}" && exit 1)

up: check-docker ## Start MindsDB and all services
	@echo "${YELLOW}Starting MindsDB with multi-cloud support...${NC}"
	@docker-compose up -d
	@echo "${GREEN}MindsDB is starting...${NC}"
	@echo "Web UI will be available at: http://localhost:47334"
	@echo "Waiting for services to be ready..."
	@sleep 10
	@$(MAKE) init

down: ## Stop and remove all containers
	@echo "${YELLOW}Stopping MindsDB...${NC}"
	@docker-compose down
	@echo "${GREEN}MindsDB stopped${NC}"

start: check-docker ## Start existing containers
	@echo "${YELLOW}Starting MindsDB containers...${NC}"
	@docker-compose start
	@echo "${GREEN}MindsDB started${NC}"

stop: ## Stop containers without removing
	@echo "${YELLOW}Stopping MindsDB containers...${NC}"
	@docker-compose stop
	@echo "${GREEN}MindsDB stopped${NC}"

restart: ## Restart all containers
	@echo "${YELLOW}Restarting MindsDB...${NC}"
	@docker-compose restart
	@echo "${GREEN}MindsDB restarted${NC}"

logs: ## View MindsDB logs
	@docker-compose logs -f mindsdb

logs-all: ## View all service logs
	@docker-compose logs -f

init: ## Initialize MindsDB with cloud datasources
	@echo "${YELLOW}Initializing MindsDB with cloud providers...${NC}"
	@python init_mindsdb.py
	@echo "${GREEN}Initialization complete${NC}"

status: ## Check service status
	@echo "${YELLOW}Service Status:${NC}"
	@docker-compose ps

clean: down ## Clean up all data volumes
	@echo "${RED}WARNING: This will delete all MindsDB data!${NC}"
	@echo "Press Ctrl+C to cancel, or Enter to continue..."
	@read confirm
	@docker-compose down -v
	@echo "${GREEN}Cleanup complete${NC}"

backup: ## Backup MindsDB data
	@echo "${YELLOW}Creating backup...${NC}"
	@mkdir -p backups
	@docker run --rm -v mindsdb_mindsdb_data:/data -v $(PWD)/backups:/backup alpine tar czf /backup/mindsdb-backup-$(shell date +%Y%m%d-%H%M%S).tar.gz -C /data .
	@echo "${GREEN}Backup created in ./backups/${NC}"

restore: ## Restore MindsDB data from latest backup
	@echo "${YELLOW}Restoring from latest backup...${NC}"
	@if [ -z "$$(ls -A backups 2>/dev/null)" ]; then \
		echo "${RED}No backups found${NC}"; \
		exit 1; \
	fi
	@LATEST_BACKUP=$$(ls -t backups/*.tar.gz | head -1); \
	echo "Restoring from: $$LATEST_BACKUP"; \
	docker run --rm -v mindsdb_mindsdb_data:/data -v $(PWD)/backups:/backup alpine tar xzf /backup/$$(basename $$LATEST_BACKUP) -C /data
	@echo "${GREEN}Restore complete${NC}"

shell: ## Open MindsDB container shell
	@docker-compose exec mindsdb /bin/bash

mysql-client: ## Connect to MindsDB via MySQL client
	@echo "${YELLOW}Connecting to MindsDB MySQL API...${NC}"
	@docker run -it --rm --network mindsdb_mindsdb_network mysql:8 mysql -h mindsdb -P 47335 -u mindsdb

test-connection: ## Test MindsDB API connection
	@echo "${YELLOW}Testing MindsDB connection...${NC}"
	@curl -s http://localhost:47334/api/util/ping && echo "${GREEN}✓ MindsDB API is responding${NC}" || echo "${RED}✗ MindsDB API is not responding${NC}"

# MCP (Model Control Plane) Management
mcp-status: ## Check MCP service status
	@echo "${YELLOW}MCP Service Status:${NC}"
	@curl -s http://localhost:${MCP_PORT:-5500}/health | jq .

mcp-logs: ## View MCP service logs
	@docker-compose logs -f mindsdb | grep -i mcp

# Cloud Provider Management
enable-provider: ## Enable a cloud provider (e.g., make enable-provider PROVIDER=aws)
	@if [ -z "$(PROVIDER)" ]; then \
		echo "${RED}Error: PROVIDER not specified. Usage: make enable-provider PROVIDER=aws${NC}"; \
		exit 1; \
	fi
	@if [ ! -f .env ]; then \
		cp .env.example .env; \
		echo "${YELLOW}Created .env file from .env.example${NC}"; \
	fi
	@if grep -q "^ENABLE_$(shell echo $(PROVIDER) | tr '[:lower:]' '[:upper:]')=false" .env; then \
		sed -i.bak "s/^ENABLE_$(shell echo $(PROVIDER) | tr '[:lower:]' '[:upper:]')=false/ENABLE_$(shell echo $(PROVIDER) | tr '[:lower:]' '[:upper:]')=true/" .env; \
		echo "${GREEN}Enabled $(PROVIDER) provider${NC}"; \
	else \
		echo "${YELLOW}Provider $(PROVIDER) is already enabled or not found in .env${NC}"; \
	fi

list-providers: ## List all available cloud providers and their status
	@echo "${YELLOW}Cloud Provider Status:${NC}"
	@echo "AWS:   $$(grep -E '^ENABLE_AWS=' .env 2>/dev/null || echo 'ENABLE_AWS=false' | cut -d= -f2)"
	@echo "GCP:   $$(grep -E '^ENABLE_GCP=' .env 2>/dev/null || echo 'ENABLE_GCP=false' | cut -d= -f2)"
	@echo "Azure: $$(grep -E '^ENABLE_AZURE=' .env 2>/dev/null || echo 'ENABLE_AZURE=false' | cut -d= -f2)"
	@echo "OCI:   $$(grep -E '^ENABLE_OCI=' .env 2>/dev/null || echo 'ENABLE_OCI=false' | cut -d= -f2)"

# Development helpers

dev-setup: ## Setup development environment
	@echo "${YELLOW}Setting up development environment...${NC}"
	@cp -n .env.example .env || true
	@echo "${GREEN}Please update .env with your cloud credentials${NC}"

update: ## Update MindsDB to latest version
	@echo "${YELLOW}Updating MindsDB...${NC}"
	@docker-compose pull
	@$(MAKE) restart
	@echo "${GREEN}MindsDB updated${NC}"

# Cloud-specific targets

check-aws: ## Check AWS connectivity
	@echo "${YELLOW}Checking AWS connectivity...${NC}"
	@docker-compose exec mindsdb python -c "import boto3; print('✓ AWS SDK available')" || echo "${RED}✗ AWS SDK not available${NC}"

check-gcp: ## Check GCP connectivity
	@echo "${YELLOW}Checking GCP connectivity...${NC}"
	@docker-compose exec mindsdb python -c "from google.cloud import storage; print('✓ GCP SDK available')" || echo "${RED}✗ GCP SDK not available${NC}"

check-azure: ## Check Azure connectivity
	@echo "${YELLOW}Checking Azure connectivity...${NC}"
	@docker-compose exec mindsdb python -c "from azure.storage.blob import BlobServiceClient; print('✓ Azure SDK available')" || echo "${RED}✗ Azure SDK not available${NC}"

check-oci: ## Check OCI connectivity
	@echo "${YELLOW}Checking OCI connectivity...${NC}"
	@docker-compose exec mindsdb python -c "import oci; print('✓ OCI SDK available')" || echo "${RED}✗ OCI SDK not available${NC}"
