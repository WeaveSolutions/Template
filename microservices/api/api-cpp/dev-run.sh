#!/bin/bash

# C++ Drogon API Development Runner
# This script builds and runs the C++ API locally for development

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
GRAY='\033[0;37m'
NC='\033[0m' # No Color

echo -e "${CYAN}🔧 Building C++ Drogon API...${NC}"

# Validate 64-bit architecture
ARCH=$(uname -m)
if [[ "$ARCH" == "x86_64" || "$ARCH" == "aarch64" ]]; then
    echo -e "${GREEN}✅ 64-bit architecture confirmed ($ARCH)${NC}"
else
    echo -e "${YELLOW}⚠️ Warning: Non-64-bit architecture detected ($ARCH). This may cause issues.${NC}"
fi

# Check for required tools
if ! command -v cmake &> /dev/null; then
    echo -e "${RED}❌ CMake not found. Please install CMake first.${NC}"
    exit 1
fi

if ! command -v make &> /dev/null; then
    echo -e "${RED}❌ Make not found. Please install build-essential or equivalent.${NC}"
    exit 1
fi

# Function to handle cleanup
cleanup() {
    if [ -d ".." ]; then
        cd ..
    fi
}

# Set trap for cleanup
trap cleanup EXIT

# Create build directory
if [ ! -d "build" ]; then
    echo -e "${YELLOW}📁 Creating build directory...${NC}"
    mkdir -p build
fi

cd build

# Configure with CMake
echo -e "${YELLOW}⚙️ Configuring with CMake...${NC}"
cmake ..

# Build the project
CORES=$(nproc)
echo -e "${YELLOW}🔨 Building with $CORES cores...${NC}"
make -j$CORES

echo -e "${GREEN}✅ Build completed successfully!${NC}"
echo -e "${GREEN}🚀 Starting C++ API server on port 8110...${NC}"
echo -e "${CYAN}📡 Health check: http://localhost:8110/health${NC}"
echo -e "${CYAN}📚 API docs: http://localhost:8110/api/v1/cpp${NC}"
echo -e "${GRAY}Press Ctrl+C to stop the server${NC}"
echo ""

# Run the server
./nexpo-api-cpp
