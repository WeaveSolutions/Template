#!/bin/bash
# Official MindsDB installation script for the Nexpo template

# Default values
DATA_DIR="./mdb_data"
ENABLE_AUTH=false
USERNAME="admin"
PASSWORD="password"
ENABLE_APIS="http,mysql"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --data-dir)
      DATA_DIR="$2"
      shift 2
      ;;
    --enable-auth)
      ENABLE_AUTH=true
      shift
      ;;
    --username)
      USERNAME="$2"
      shift 2
      ;;
    --password)
      PASSWORD="$2"
      shift 2
      ;;
    --enable-apis)
      ENABLE_APIS="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

# Verify Docker is installed
if ! command -v docker &> /dev/null; then
    echo "Error: Docker is not installed or not in PATH."
    echo "Please install Docker first: https://www.docker.com/products/docker-desktop/"
    exit 1
fi

# Create data directory if it doesn't exist
if [ ! -d "$DATA_DIR" ]; then
    echo "Creating MindsDB data directory at $DATA_DIR..."
    mkdir -p "$DATA_DIR"
fi

# Build the Docker command
DOCKER_CMD="docker run --name mindsdb_container"

# Add environment variables
if [ "$ENABLE_AUTH" = true ]; then
    DOCKER_CMD="$DOCKER_CMD -e MINDSDB_USERNAME='$USERNAME' -e MINDSDB_PASSWORD='$PASSWORD'"
fi

# Add APIs
DOCKER_CMD="$DOCKER_CMD -e MINDSDB_APIS='$ENABLE_APIS'"

# Add MKL optimization to avoid training issues
DOCKER_CMD="$DOCKER_CMD -e MKL_SERVICE_FORCE_INTEL=1"

# Add port mappings based on enabled APIs
DOCKER_CMD="$DOCKER_CMD -p 47334:47334" # HTTP API and GUI (always needed)

if [[ $ENABLE_APIS == *"mysql"* ]]; then
    DOCKER_CMD="$DOCKER_CMD -p 47335:47335" # MySQL API
fi

if [[ $ENABLE_APIS == *"mongodb"* ]]; then
    DOCKER_CMD="$DOCKER_CMD -p 47336:47336" # MongoDB API
fi

if [[ $ENABLE_APIS == *"postgres"* ]]; then
    DOCKER_CMD="$DOCKER_CMD -p 55432:55432" # PostgreSQL API
fi

# Add volume mapping for data persistence
ABS_DATA_PATH=$(realpath "$DATA_DIR")
DOCKER_CMD="$DOCKER_CMD -v '$ABS_DATA_PATH:/root/mdb_storage'"

# Add detach flag to run in background
DOCKER_CMD="$DOCKER_CMD -d"

# Add image name
DOCKER_CMD="$DOCKER_CMD mindsdb/mindsdb:v25.6.2.0"

# Check if container already exists
if docker ps -a --filter "name=mindsdb_container" --format "{{.Names}}" | grep -q "mindsdb_container"; then
    echo "MindsDB container already exists. Removing it..."
    docker rm -f mindsdb_container > /dev/null
fi

# Run the Docker command
echo "Installing MindsDB using official Docker image..."
echo "Command: $DOCKER_CMD"
eval $DOCKER_CMD

# Wait for MindsDB to start
echo "Waiting for MindsDB to start..."
MAX_RETRIES=30
RETRY_COUNT=0
MINDSDB_READY=false

while [ "$MINDSDB_READY" = false ] && [ $RETRY_COUNT -lt $MAX_RETRIES ]; do
    sleep 5
    RETRY_COUNT=$((RETRY_COUNT + 1))
    
    if docker logs mindsdb_container 2>&1 | grep -q "MindsDB API Server listening on"; then
        MINDSDB_READY=true
    else
        echo "Waiting for MindsDB to initialize... (Attempt $RETRY_COUNT/$MAX_RETRIES)"
    fi
done

if [ "$MINDSDB_READY" = true ]; then
    echo "MindsDB has been successfully installed and is running!"
    echo "You can access MindsDB at http://localhost:47334"
    
    if [ "$ENABLE_AUTH" = true ]; then
        echo "Authentication is enabled with username: $USERNAME"
    fi
    
    # Display available APIs
    echo "Enabled APIs:"
    if [[ $ENABLE_APIS == *"http"* ]]; then
        echo "  - HTTP API and GUI: http://localhost:47334"
    fi
    if [[ $ENABLE_APIS == *"mysql"* ]]; then
        echo "  - MySQL API: localhost:47335"
    fi
    if [[ $ENABLE_APIS == *"mongodb"* ]]; then
        echo "  - MongoDB API: localhost:47336"
    fi
    if [[ $ENABLE_APIS == *"postgres"* ]]; then
        echo "  - PostgreSQL API: localhost:55432"
    fi
    
    # Initialize MindsDB with cloud providers if init_mindsdb.py exists
    SCRIPT_DIR=$(dirname "$(realpath "$0")")
    INIT_SCRIPT="$SCRIPT_DIR/init_mindsdb.py"
    if [ -f "$INIT_SCRIPT" ]; then
        echo ""
        echo "To initialize MindsDB with your cloud providers, run:"
        echo "python $INIT_SCRIPT"
    fi
else
    echo "MindsDB installation timed out. Check docker logs for details:"
    echo "docker logs mindsdb_container"
fi
