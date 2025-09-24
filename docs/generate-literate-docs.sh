#!/bin/bash

# Comprehensive Erlang Literate Documentation Builder (JavaScript Implementation)
# This script wraps the JavaScript parser for seamless integration

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Get script directory and root
SCRIPT_DIR="$(dirname "$(realpath "$0")")"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
cd "$ROOT_DIR"

# Configuration
SRC_DIR="${SRC_DIR:-$ROOT_DIR/src}"
OUTPUT_DIR="${OUTPUT_DIR:-$ROOT_DIR/docs/book/src}"
PARSER_SCRIPT="$SCRIPT_DIR/erlang-literate-parser.js"

# Parse arguments
VERBOSE=false
DRY_RUN=false
SHOW_HELP=false

if [[ "$@" == *"-h"* ]] || [[ "$@" == *"--help"* ]]; then
    SHOW_HELP=true
fi
if [[ "$@" == *"-v"* ]] || [[ "$@" == *"--verbose"* ]]; then
    VERBOSE=true
fi
if [[ "$@" == *"--dry-run"* ]] || [[ "$@" == *"--dryrun"* ]]; then
    DRY_RUN=true
fi

if [ "$SHOW_HELP" = true ]; then
    echo -e "${GREEN}HyperBEAM Literate Erlang Documentation Generator (JavaScript)${NC}"
    echo "========================================================"
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -v, --verbose     Enable verbose output"
    echo "  --dry-run        Simulate deployment without actually deploying"
    echo "  -h, --help       Show this help message"
    echo ""
    echo "Environment Variables:"
    echo "  SRC_DIR          Source directory (default: ./src)"
    echo "  OUTPUT_DIR       Output directory (default: ./docs/book/src)"
    echo "  DEPLOY_KEY       Arweave wallet key for deployment"
    echo "  ANT_PROCESS      ANT process ID for ArNS deployment"
    echo ""
    echo "Examples:"
    echo "  $0                    # Generate documentation normally"
    echo "  $0 -v                 # Generate with verbose output"
    echo "  $0 --dry-run          # Test deployment without deploying"
    echo "  $0 --dry-run -v       # Dry run with verbose output"
    echo ""
    exit 0
fi

echo -e "${GREEN}HyperBEAM Literate Erlang Documentation Generator (JavaScript)${NC}"
if [ "$DRY_RUN" = true ]; then
    echo -e "${YELLOW}[DRY RUN MODE] - No actual deployment will occur${NC}"
fi
echo "========================================================"

# Check for Node.js
if ! command -v node &> /dev/null; then
    echo -e "${RED}Error: Node.js is required but not installed.${NC}"
    echo "Please install Node.js (version 14 or later) to use this parser."
    echo ""
    echo "On macOS with Homebrew: brew install node"
    echo "On Ubuntu: curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash - && sudo apt-get install -y nodejs"
    exit 1
fi

# Check Node.js version (need ES modules support)
NODE_VERSION=$(node --version | cut -d'v' -f2 | cut -d'.' -f1)
if [ "$NODE_VERSION" -lt 14 ]; then
    echo -e "${RED}Error: Node.js version 14 or later is required for ES modules support.${NC}"
    echo "Current version: $(node --version)"
    exit 1
fi

# Verify source directory exists
if [ ! -d "$SRC_DIR" ]; then
    echo -e "${RED}Error: Source directory not found: $SRC_DIR${NC}"
    exit 1
fi

# Verify parser script exists
if [ ! -f "$PARSER_SCRIPT" ]; then
    echo -e "${RED}Error: Parser script not found: $PARSER_SCRIPT${NC}"
    exit 1
fi

# Make parser executable
chmod +x "$PARSER_SCRIPT"

# Count Erlang files
ERL_COUNT=$(find "$SRC_DIR" -name "*.erl" -type f | wc -l)
if [ "$ERL_COUNT" -eq 0 ]; then
    echo -e "${YELLOW}Warning: No .erl files found in $SRC_DIR${NC}"
    exit 0
fi

echo "Source directory: $SRC_DIR"
echo "Output directory: $OUTPUT_DIR"
echo "Found $ERL_COUNT Erlang files"
echo ""

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

# Run the JavaScript parser
echo -e "${GREEN}Generating literate documentation...${NC}"

# Set environment variables and run parser
export SRC_DIR="$SRC_DIR"
export OUTPUT_DIR="$OUTPUT_DIR"

if [ "$VERBOSE" = true ]; then
    node "$PARSER_SCRIPT" --verbose
else
    node "$PARSER_SCRIPT"
fi

PARSER_EXIT_CODE=$?

if [ $PARSER_EXIT_CODE -eq 0 ]; then
    echo ""
    echo -e "${GREEN}✓ Literate documentation generated successfully${NC}"

    # List generated files
    if [ "$VERBOSE" = true ]; then
        echo ""
        echo "Generated files:"
        ls -la "$OUTPUT_DIR"/*.md 2>/dev/null | while read -r line; do
            echo "  $line"
        done
    fi

    # Copy to mdBook if it exists
    if [ -d "$ROOT_DIR/docs/book/src" ]; then
        if [ "$DRY_RUN" = true ]; then
            echo -e "${YELLOW}[DRY RUN] Would copy documentation to mdBook...${NC}"
            echo -e "${YELLOW}[DRY RUN] ✓ Documentation would be copied to mdBook${NC}"
        else
            echo -e "${GREEN}Copying documentation to mdBook...${NC}"
            cp "$OUTPUT_DIR"/*.md "$ROOT_DIR/docs/book/src/" 2>/dev/null
            if [ $? -eq 0 ]; then
                echo -e "${GREEN}✓ Documentation copied to mdBook${NC}"
            else
                echo -e "${YELLOW}Warning: Could not copy to mdBook (no files generated?)${NC}"
            fi
        fi
    fi

    # Build mdBook if available
    if command -v mdbook &> /dev/null && [ -f "$ROOT_DIR/docs/book/book.toml" ]; then
        if [ "$DRY_RUN" = true ]; then
            echo -e "${YELLOW}[DRY RUN] Would build mdBook...${NC}"
            echo -e "${YELLOW}[DRY RUN] ✓ mdBook would be built successfully${NC}"
            echo -e "${YELLOW}[DRY RUN] Would be viewable at: file://$ROOT_DIR/docs/book/dist/index.html${NC}"
        else
            echo -e "${GREEN}Building mdBook...${NC}"
            cd "$ROOT_DIR/docs/book"
            mdbook build
            if [ $? -eq 0 ]; then
                echo -e "${GREEN}✓ mdBook built successfully${NC}"
                echo "View at: file://$ROOT_DIR/docs/book/dist/index.html"
            else
                echo -e "${YELLOW}Warning: mdBook build failed${NC}"
            fi
            cd "$ROOT_DIR"
        fi
    fi

    # Run deployment dry run if requested
    if [ "$DRY_RUN" = true ]; then
        echo ""
        echo -e "${YELLOW}Running deployment dry run...${NC}"
        "$SCRIPT_DIR/deploy-dry-run.sh"
    fi

    echo ""
    if [ "$DRY_RUN" = true ]; then
        echo -e "${YELLOW}Documentation generation and deployment dry run complete!${NC}"
    else
        echo -e "${GREEN}Documentation generation complete!${NC}"
    fi
    echo "Output directory: $OUTPUT_DIR"

else
    echo ""
    echo -e "${RED}✗ Documentation generation failed (exit code: $PARSER_EXIT_CODE)${NC}"
    exit $PARSER_EXIT_CODE
fi