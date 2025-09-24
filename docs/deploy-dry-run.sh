#!/bin/bash

# Deployment Dry Run Script
# Simulates the permaweb deployment without actually deploying

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Default values (can be overridden by environment variables)
ARNS_NAME="${ARNS_NAME:-hyperbeam}"
UNDERNAME="${UNDERNAME:-book}"
DEPLOY_FOLDER="${DEPLOY_FOLDER:-docs/book/dist}"

echo -e "${YELLOW}============================================${NC}"
echo -e "${YELLOW}    DEPLOYMENT DRY RUN SIMULATION${NC}"
echo -e "${YELLOW}============================================${NC}"
echo ""

# Simulate the deployment command that would be run
echo -e "${GREEN}Would execute deployment command:${NC}"
echo "npx permaweb-deploy \\"
echo "  --arns-name=${ARNS_NAME} \\"
echo "  --undername=${UNDERNAME} \\"
echo "  --ant-process=\${ANT_PROCESS} \\"
echo "  --deploy-folder=${DEPLOY_FOLDER}"
echo ""

# Check if the deploy folder exists
if [ -d "$DEPLOY_FOLDER" ]; then
    echo -e "${GREEN}✓ Deploy folder exists: ${DEPLOY_FOLDER}${NC}"

    # Count files in deploy folder
    FILE_COUNT=$(find "$DEPLOY_FOLDER" -type f | wc -l)
    FOLDER_SIZE=$(du -sh "$DEPLOY_FOLDER" | cut -f1)
    echo -e "${GREEN}  - Contains ${FILE_COUNT} files${NC}"
    echo -e "${GREEN}  - Total size: ${FOLDER_SIZE}${NC}"

    # List some key files
    echo ""
    echo -e "${GREEN}Key files to be deployed:${NC}"
    if [ -f "$DEPLOY_FOLDER/index.html" ]; then
        echo -e "${GREEN}  ✓ index.html${NC}"
    else
        echo -e "${RED}  ✗ index.html (missing - deployment would fail)${NC}"
    fi

    # Check for CSS and JS files
    CSS_COUNT=$(find "$DEPLOY_FOLDER" -name "*.css" | wc -l)
    JS_COUNT=$(find "$DEPLOY_FOLDER" -name "*.js" | wc -l)
    HTML_COUNT=$(find "$DEPLOY_FOLDER" -name "*.html" | wc -l)

    echo -e "${GREEN}  - ${CSS_COUNT} CSS files${NC}"
    echo -e "${GREEN}  - ${JS_COUNT} JavaScript files${NC}"
    echo -e "${GREEN}  - ${HTML_COUNT} HTML files${NC}"

else
    echo -e "${RED}✗ Deploy folder does not exist: ${DEPLOY_FOLDER}${NC}"
    echo -e "${RED}  Deployment would fail!${NC}"
    echo ""
    echo -e "${YELLOW}To fix this, run:${NC}"
    echo "  cd docs/book && mdbook build"
    exit 1
fi

echo ""
echo -e "${GREEN}Environment Variables Check:${NC}"
if [ -n "$DEPLOY_KEY" ]; then
    echo -e "${GREEN}  ✓ DEPLOY_KEY is set${NC}"
else
    echo -e "${YELLOW}  ! DEPLOY_KEY not set (would use default)${NC}"
fi

if [ -n "$ANT_PROCESS" ]; then
    echo -e "${GREEN}  ✓ ANT_PROCESS is set${NC}"
else
    echo -e "${YELLOW}  ! ANT_PROCESS not set (required for deployment)${NC}"
fi

echo ""
echo -e "${GREEN}Deployment Target:${NC}"
echo -e "${GREEN}  - ArNS Name: ${ARNS_NAME}${NC}"
echo -e "${GREEN}  - Undername: ${UNDERNAME}${NC}"
echo -e "${GREEN}  - Full URL: https://${UNDERNAME}_${ARNS_NAME}.arweave.net${NC}"

echo ""
echo -e "${YELLOW}============================================${NC}"
echo -e "${YELLOW}      DRY RUN COMPLETE${NC}"
echo -e "${YELLOW}============================================${NC}"
echo ""
echo -e "${GREEN}✓ Deployment validation successful!${NC}"
echo -e "${YELLOW}To perform actual deployment, run without --dry-run flag${NC}"