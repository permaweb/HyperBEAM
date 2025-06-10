#!/bin/bash

# Script to build HyperBEAM documentation in one seamless command
#
# Usage: ./docs/build-all.sh [-v | --verbose]
#   -v, --verbose: Show detailed output from rebar3 and mkdocs commands.

# --- Color Definitions ---
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# HyperBEAM Logo Colors
NEON_GREEN='\033[38;5;46m'
CYAN='\033[38;5;51m'
BRIGHT_YELLOW='\033[38;5;226m'
MAGENTA='\033[38;5;201m'
BRIGHT_RED='\033[38;5;196m'
BLACK='\033[38;5;0m'
GRAY='\033[38;5;245m'

# --- Helper Functions ---
log_success() {
  echo -e "${GREEN}✓ $1${NC}"
}

log_info() {
  echo -e "${BLUE}→ $1${NC}"
}

log_step() {
  echo -e "\n${YELLOW}${BOLD}$1${NC}"
}

log_error() {
  echo -e "${RED}✗ $1${NC}"
}

# --- Variable Defaults ---
VERBOSE=false

# --- Parse Command Line Arguments ---
while [[ $# -gt 0 ]]; do
  key="$1"

  case $key in
    -v|--verbose)
      VERBOSE=true
      log_info "Verbose mode enabled"
      shift # past argument
      ;;
    *)
      # unknown option
      log_error "Unknown option: $1"
      # Optionally, show usage here and exit
      exit 1
      ;;
  esac
done

# --- Display HyperBEAM ASCII Logo ---
display_logo() {
  echo -e "
${NEON_GREEN}                ++         ${BLACK}${BOLD}                                 ${NC}
${NEON_GREEN}               +++        ${BLACK}${BOLD} _                              ${NC}
${NEON_GREEN}             ++++*        ${BLACK}${BOLD}| |__  _   _ _ __   ___ _ __  ${NC}
${NEON_GREEN}           :+++*${BRIGHT_YELLOW}##       ${BLACK}${BOLD} | '_ \\| | | | '_ \\ / _ \\ '__| ${NC}
${NEON_GREEN}          ++**${BRIGHT_YELLOW}####       ${BLACK}${BOLD} | | | | |_| | |_) |  __/ |    ${NC}
${NEON_GREEN}        +++${BRIGHT_YELLOW}####${NEON_GREEN}***       ${BLACK}${BOLD} |_| |_|\\__, | .__/ \\___|_|    ${NC}
${NEON_GREEN}        +*${BRIGHT_YELLOW}##${NEON_GREEN}****${MAGENTA}+--      ${BLACK}${BOLD}        |___/|_|              ${NC}
${MAGENTA}    -**${BRIGHT_YELLOW}##${NEON_GREEN}**${MAGENTA}+------       ${BLACK}${BOLD}                	BEAM.${NC}
${MAGENTA}   -##${NEON_GREEN}*+${BRIGHT_RED}---:::::::
${GRAY}  =${GRAY}%%${NEON_GREEN}*+${BRIGHT_RED}=-:::::::::${GRAY}        DECENTRALIZED OPERATING SYSTEM${NC}
"
}

# --- Script Start ---
display_logo
log_step "DOCUMENTATION BUILD"

# Ensure we're in the root directory of the project
ROOT_DIR="$(dirname "$(realpath "$0")")/.."
cd "$ROOT_DIR" || { log_error "Failed to change to root directory"; exit 1; }

# --- Step 1: Compile the project with rebar3 ---
log_step "Compiling project"
if [ "$VERBOSE" = true ]; then
  rebar3 compile || { log_error "rebar3 compile failed"; exit 1; }
else
  rebar3 compile > /dev/null 2>&1 || { log_error "rebar3 compile failed"; exit 1; }
fi
log_success "Compilation completed"

# --- Step 2: Generate edoc documentation ---
log_step "Generating edoc documentation"
if [ "$VERBOSE" = true ]; then
  rebar3 edoc || { log_error "rebar3 edoc failed"; exit 1; }
else
  rebar3 edoc > /dev/null 2>&1 || { log_error "rebar3 edoc failed"; exit 1; }
fi
log_success "Edoc generation completed"

# --- Step 3: Process source code documentation ---
log_step "Processing source code documentation"
DOCS_DIR="$ROOT_DIR/docs/resources/source-code"
INDEX_FILE="$DOCS_DIR/index.md"

# Check if the directory and index file exist
if [ ! -d "$DOCS_DIR" ]; then
  log_error "Source code docs directory not found at $DOCS_DIR"
  exit 1
fi

if [ ! -f "$INDEX_FILE" ]; then
  log_error "Source code index file not found at $INDEX_FILE"
  exit 1
fi

# --- Step 3.1: Remove ToC entries for Function Index and Function Details ---
log_info "Cleaning module files"

find "$DOCS_DIR" -maxdepth 1 -type f -name "*.md" -not -name "index.md" -not -name "README.md" | while read -r file; do
  TEMP_MODULE_FILE=$(mktemp)

  awk '
    /^\* \[Description\]\(#description\)$/ { next; }
    /^\* \[Function Index\]\(#index\)$/ { next; }
    /^\* \[Function Details\]\(#functions\)$/ { next; }
	/^\* \[Data Types\]\(#types\)$/ { next; }
    { print; }
  ' "$file" > "$TEMP_MODULE_FILE"

  mv "$TEMP_MODULE_FILE" "$file"
done

# --- Step 3.2: Add GitHub links to source code files ---
log_info "Adding GitHub repository links to source code files"

# Base GitHub repository URL
GITHUB_BASE_URL="https://github.com/permaweb/HyperBEAM/blob/main/src"

# Process only files in the resources/source-code directory
find "$DOCS_DIR" -maxdepth 1 -type f -name "*.md" -not -name "index.md" -not -name "README.md" | while read -r file; do
  TEMP_MODULE_FILE_CLEANED=$(mktemp)
  TEMP_MODULE_FILE_FINAL=$(mktemp)

  # Get the module name from the filename
  module_name=$(basename "$file" .md)

  # Define the exact header pattern to remove
  # Note: Assumes module names are simple enough not to need complex regex escaping.
  header_pattern="^# Module ${module_name} #$"

  # Remove the old header line using sed
  # Use -i option for in-place editing on a temporary copy first to avoid issues with read/write on same file descriptor
  cp "$file" "$TEMP_MODULE_FILE_CLEANED"
  sed -i'' -e "/${header_pattern}/d" "$TEMP_MODULE_FILE_CLEANED"

  # Add the new GitHub link header at the top of the final temp file
  # Using the user's updated format with "Module" text
  echo "# [Module $module_name.erl]($GITHUB_BASE_URL/$module_name.erl)" > "$TEMP_MODULE_FILE_FINAL"
  echo "" >> "$TEMP_MODULE_FILE_FINAL"

  # Append the cleaned content (without the old header)
  cat "$TEMP_MODULE_FILE_CLEANED" >> "$TEMP_MODULE_FILE_FINAL"

  # Replace the original file
  mv "$TEMP_MODULE_FILE_FINAL" "$file"

  # Clean up the intermediate temp file
  rm "$TEMP_MODULE_FILE_CLEANED"
done

log_success "GitHub links added and old headers removed"

# --- Step 3.3: Update mkdocs.yml navigation with current module list ---
# log_info "Updating mkdocs.yml navigation"

# # Create temporary file for the new mkdocs.yml
# MKDOCS_TEMP=$(mktemp)
# MKDOCS_FILE="$ROOT_DIR/mkdocs.yml"

# # Process mkdocs.yml file to remove old modules
# awk '
# BEGIN { in_modules = 0; skip_modules = 0; }
# /^ *- Modules:/ {
#   print $0;
#   in_modules = 1;
#   skip_modules = 1;
#   next;
# }
# {
#   if (skip_modules == 0) {
#     print $0;
#   }
#   if (in_modules == 1 && $0 ~ /^ *-/) {
#     if ($0 !~ /^ *- Modules:/) {
#       in_modules = 0;
#       skip_modules = 0;
#       print $0;
#     }
#   }
# }
# ' "$MKDOCS_FILE" > "$MKDOCS_TEMP"

# # Find the position to insert module entries
# INSERT_LINE=$(grep -n "^ *- Modules:" "$MKDOCS_TEMP" | cut -d: -f1)

# if [ -z "$INSERT_LINE" ]; then
#   log_error "Could not find '- Modules:' section in mkdocs.yml"
#   # Clean up temp file before exiting
#   rm -f "$MKDOCS_TEMP"
#   exit 1
# fi

# # Prepare head and tail parts
# head -n "$INSERT_LINE" "$MKDOCS_TEMP" > "${MKDOCS_TEMP}.head"
# tail -n +$((INSERT_LINE + 1)) "$MKDOCS_TEMP" > "${MKDOCS_TEMP}.tail"

# # Use an associative array to track added modules
# declare -A added_modules
# MODULE_LINES="" # Accumulate module lines here

# # Use process substitution to read modules without a subshell per iteration
# while IFS= read -r module_file; do
#     # Check if module_file is empty or not a file (safety check)
#     if [[ -z "$module_file" || ! -f "$module_file" ]]; then
#         continue
#     fi

#     module_name=$(basename "$module_file" .md)

#     # Only add the module if its basename hasn't been added yet
#     if [[ -z "${added_modules[$module_name]}" ]]; then
#       # Append the line to a variable instead of echoing directly
#       MODULE_LINES+="        - $module_name: 'resources/source-code/$module_name.md'\n"
#       added_modules[$module_name]=1
#     fi
# # Feed the loop using process substitution <(...)
# done < <(find "$DOCS_DIR" -maxdepth 1 -type f -name "*.md" -not -name "index.md" -not -name "README.md" | sort -u)

# # Assemble the final mkdocs.yml
# {
#   cat "${MKDOCS_TEMP}.head"
#   # Echo the accumulated module lines (use printf for robustness)
#   printf "%b" "$MODULE_LINES"
#   cat "${MKDOCS_TEMP}.tail"
# } > "$MKDOCS_FILE"

# # Clean up temporary files
# rm -f "$MKDOCS_TEMP" "${MKDOCS_TEMP}.head" "${MKDOCS_TEMP}.tail"

# log_success "mkdocs.yml navigation updated"

# --- Step 4: Build and serve mkdocs ---
log_step "Building mkdocs documentation"
if [ "$VERBOSE" = true ]; then
  mkdocs build || { log_error "mkdocs build failed"; exit 1; }
else
  mkdocs build > /dev/null 2>&1 || { log_error "mkdocs build failed"; exit 1; }
fi

# Find the latest CSS files with their hashes
MAIN_CSS=$(find ./mkdocs-site/assets/stylesheets -name "main.*.min.css" | sort | tail -n 1)
PALETTE_CSS=$(find ./mkdocs-site/assets/stylesheets -name "palette.*.min.css" | sort | tail -n 1)

# Extract just the filenames from the paths
MAIN_CSS_FILE=$(basename "$MAIN_CSS")
PALETTE_CSS_FILE=$(basename "$PALETTE_CSS")

# Find all HTML files and replace the CSS references in each one
log_info "Updating CSS references in HTML files"
find ./mkdocs-site -type f -name "*.html" | while read -r html_file; do
    sed -i'' -e "s|MAIN\.CSS|assets/stylesheets/$MAIN_CSS_FILE|g" "$html_file"
    sed -i'' -e "s|MAIN_PALETTE\.CSS|assets/stylesheets/$PALETTE_CSS_FILE|g" "$html_file"
done

# Remove .html-e files
find ./mkdocs-site -type f -name "*.html-e" -delete

log_success "MkDocs build completed"

# --- Step 5: Generate LLM context files ---
log_step "Generating LLM context files"

LLM_SUMMARY_FILE="$ROOT_DIR/docs/llms.txt"
LLM_FULL_FILE="$ROOT_DIR/docs/llms-full.txt"
DOC_DIRS=(
    "$ROOT_DIR/docs/introduction"
    "$ROOT_DIR/docs/run"
    "$ROOT_DIR/docs/build"
    "$ROOT_DIR/docs/devices"
    "$ROOT_DIR/docs/resources"
)

# Get current timestamp
GENERATION_TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Generate llms.txt (routes and summary)
log_info "Creating summary and routes file"
cat > "$LLM_SUMMARY_FILE" <<EOL
Generated: $GENERATION_TIMESTAMP

## HyperBEAM Documentation Summary

This document provides an overview and routes for the HyperBEAM documentation, intended for LLM consumption.
Key sections include: Getting Started (begin), Running HyperBEAM (run), Developer Guides (guides), Device Integration (devices), and Resources (resources).

## Documentation Pages by Section
EOL

# Loop through each documentation directory
for DOC_DIR in "${DOC_DIRS[@]}"; do
    SECTION_NAME=$(basename "$DOC_DIR")
    SECTION_HEADING=$(echo "$SECTION_NAME" | sed 's/\b\(.\)/\u\1/g')

    echo "" >> "$LLM_SUMMARY_FILE"
    echo "### $SECTION_HEADING" >> "$LLM_SUMMARY_FILE"
    echo "" >> "$LLM_SUMMARY_FILE"

    find "$DOC_DIR" -type f -name "*.md" -print |
      sed "s|^$ROOT_DIR/||" |
      sed 's/^docs\///' |
      sed 's/\.md$//' |
      sort |
      while IFS= read -r base_path; do
        html_path="${base_path}.html"
        md_path_relative="docs/${base_path}.md"
        md_file_path="$ROOT_DIR/$md_path_relative"

        if [ -f "$md_file_path" ]; then
            title=$(grep -m 1 '^# ' "$md_file_path" 2>/dev/null | sed 's/^# //')
        else
            title=""
        fi

        if [ -z "$title" ]; then
            title=$(basename "$base_path" | sed -e 's/-/ /g' -e 's/\b\(.\)/\u\1/g')
        fi

        echo "* [$title](./$html_path)" >> "$LLM_SUMMARY_FILE"
      done
done

# Generate llms-full.txt (concatenated content)
log_info "Creating full documentation file"
echo "Generated: $GENERATION_TIMESTAMP" > "$LLM_FULL_FILE"
echo "" >> "$LLM_FULL_FILE"

find "${DOC_DIRS[@]}" -type f -name "*.md" | sort | while read -r doc_file; do
    relative_path="${doc_file#$ROOT_DIR/}"
    echo "--- START OF FILE: $relative_path ---" >> "$LLM_FULL_FILE"
    cat "$doc_file" >> "$LLM_FULL_FILE"
    echo "" >> "$LLM_FULL_FILE"
    echo "--- END OF FILE: $relative_path ---" >> "$LLM_FULL_FILE"
    echo "" >> "$LLM_FULL_FILE"
done

log_success "LLM context files generated"

# --- Final success message ---
echo -e "\n${GREEN}${BOLD}✓ Documentation build completed successfully${NC}\n"
