#!/bin/bash

# HyperBEAM Documentation Build and Serve Script
# This script generates literate documentation and serves it locally

set -e

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}🚀 Building and serving HyperBEAM documentation...${NC}"

# Generate literate documentation
echo -e "${GREEN}📚 Generating literate documentation...${NC}"
./build-literate-erlang-js.sh

# Build and serve mdBook
echo -e "${GREEN}📖 Building mdBook...${NC}"
cd book
mdbook build

echo -e "${GREEN}🌐 Starting development server...${NC}"
echo -e "${BLUE}📖 Documentation will be available at: http://localhost:3033${NC}"
echo -e "${BLUE}🔄 Auto-reload enabled - changes will be reflected automatically${NC}"
echo -e "${BLUE}⏹️  Press Ctrl+C to stop the server${NC}"

mdbook serve --port 3033