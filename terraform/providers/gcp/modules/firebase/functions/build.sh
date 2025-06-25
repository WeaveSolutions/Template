#!/bin/bash

# Build script for Firebase Cloud Functions

# Navigate to functions directory
cd "$(dirname "$0")"

# Install dependencies
pnpm install

# Create deployment directory
mkdir -p dist

# Copy source files
cp index.js dist/
cp package.json dist/
# Copy node_modules (pnpm creates hoisted structure at root)
cp -r ../../../node_modules dist/

# Create zip file
cd dist
zip -r ../token-exchange.zip .
cd ..

# Clean up
rm -rf dist

echo "Cloud Functions package created: token-exchange.zip"
