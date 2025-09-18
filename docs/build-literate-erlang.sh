#!/bin/bash

# Script to generate literate Erlang documentation from HyperBEAM source files
#
# This creates .erl.md files that combine source code with documentation
# in a format optimized for GitHub rendering with cleaner appearance
#
# Usage: ./docs/build-literate-erlang.sh [-v | --verbose]
#   -v, --verbose: Show detailed processing output

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

log_verbose() {
  if [ "$VERBOSE" = true ]; then
    echo -e "${GRAY}  $1${NC}"
  fi
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
      shift
      ;;
    *)
      log_error "Unknown option: $1"
      echo "Usage: $0 [-v | --verbose]"
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
${GRAY}  =${GRAY}%%${NEON_GREEN}*+${BRIGHT_RED}=-:::::::::${GRAY}        LITERATE ERLANG DOCUMENTATION${NC}
"
}

# --- Script Start ---
display_logo
log_step "LITERATE ERLANG DOCUMENTATION GENERATION"

# Ensure we're in the root directory
ROOT_DIR="$(dirname "$(realpath "$0")")/.."
cd "$ROOT_DIR" || { log_error "Failed to change to root directory"; exit 1; }

# GitHub repository base URL
GITHUB_BASE_URL="https://github.com/permaweb/HyperBEAM/blob/edge/src"

# Output directory for literate Erlang files
OUTPUT_DIR="$ROOT_DIR/docs/literate-erlang"
mkdir -p "$OUTPUT_DIR"

# --- Function to extract module documentation ---
extract_module_doc() {
  local file="$1"
  local in_doc=false
  local doc_content=""

  while IFS= read -r line; do
    if [[ "$line" =~ ^%%%[[:space:]]?(.*)$ ]]; then
      in_doc=true
      doc_content+="${BASH_REMATCH[1]}"$'\n'
    elif [[ "$line" =~ ^%%[[:space:]]?(@doc[[:space:]])?(.*)$ ]] && [ "$in_doc" = true ]; then
      # Extract content after @doc if present
      doc_content+="${BASH_REMATCH[2]}"$'\n'
    elif [[ ! "$line" =~ ^%% ]] && [ "$in_doc" = true ]; then
      break
    fi
  done < "$file"

  # Clean up @doc prefixes, empty lines, and convert edocs syntax to markdown
  echo "$doc_content" | \
    sed 's/^@doc$//' | \
    sed 's/^@doc //' | \
    sed 's/^@end$//' | \
    sed 's/^@author /**Author:** /' | \
    sed 's/^@copyright /**Copyright:** /' | \
    sed 's/^---*$//' | \
    sed '/^[[:space:]]*$/d' | \
    sed "s/\`\([^']*\)'/\`\1\`/g"
}

# --- Function to extract function documentation ---
extract_function_doc() {
  local content="$1"

  # Remove leading %% or % and @doc tags, then convert edocs syntax to markdown
  echo "$content" | \
    sed 's/^%% *//' | \
    sed 's/^% *//' | \
    sed 's/^@doc$//' | \
    sed 's/^@doc //' | \
    sed 's/^@end$//' | \
    sed 's/^@author /**Author:** /' | \
    sed 's/^@copyright /**Copyright:** /' | \
    sed 's/^---*$//' | \
    sed '/^$/d' | \
    sed "s/\`\([^']*\)'/\`\1\`/g"
}

# --- Function to process a single Erlang file ---
process_erlang_file() {
  local src_file="$1"
  local module_name=$(basename "$src_file" .erl)
  local output_file="$OUTPUT_DIR/${module_name}.erl.md"

  log_verbose "Processing $module_name"

  # Start the literate Erlang document with cleaner format
  cat > "$output_file" <<EOF
# $module_name

[View source on GitHub]($GITHUB_BASE_URL/${module_name}.erl)

EOF

  # Extract and add module documentation if it exists
  local module_doc=$(extract_module_doc "$src_file")
  if [ -n "$module_doc" ]; then
    echo "$module_doc" >> "$output_file"
    echo "" >> "$output_file"
    echo "---" >> "$output_file"
    echo "" >> "$output_file"
  fi

  # Add module exports in a clean format
  local exports=$(grep -E "^-export\(" "$src_file" | sed 's/-export(\[//' | sed 's/\]).*//' | tr ',' '\n' | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//' | sort -u)

  if [ -n "$exports" ]; then
    echo "## Exported Functions" >> "$output_file"
    echo "" >> "$output_file"

    # Create a proper bulleted list for exports
    while IFS= read -r export; do
      if [[ "$export" =~ ^[a-z] ]]; then
        echo "- \`$export\`" >> "$output_file"
      fi
    done <<< "$exports"

    echo "" >> "$output_file"
    echo "---" >> "$output_file"
    echo "" >> "$output_file"
  fi

  # Process functions
  local in_function=false
  local in_spec=false
  local in_doc_comment=false
  local current_function=""
  local function_content=""
  local spec_content=""
  local doc_content=""
  local functions_written=0

  while IFS= read -r line; do
    # Check for doc comments (before functions)
    if [[ "$line" =~ ^%+[[:space:]]?@doc[[:space:]](.*)$ ]] ||
       ([[ "$line" =~ ^%+[[:space:]](.*)$ ]] && [ "$in_doc_comment" = true ]); then
      in_doc_comment=true
      if [[ "$line" =~ @doc[[:space:]](.*)$ ]]; then
        doc_content+="${BASH_REMATCH[1]}"$'\n'
      else
        doc_content+="${BASH_REMATCH[1]}"$'\n'
      fi
      continue
    fi

    # Check for -spec
    if [[ "$line" =~ ^-spec[[:space:]] ]]; then
      in_spec=true
      spec_content="$line"$'\n'
      in_doc_comment=false
      continue
    fi

    # Continue collecting spec if in multi-line spec
    if [ "$in_spec" = true ]; then
      spec_content+="$line"$'\n'
      if [[ "$line" =~ \.[[:space:]]*$ ]]; then
        in_spec=false
      fi
      continue
    fi

    # Check for function definition
    if [[ "$line" =~ ^([a-z][a-z0-9_]*)[[:space:]]*\( ]]; then
      # If we were already in a function, write it out
      if [ -n "$current_function" ] && [ -n "$function_content" ]; then
        write_clean_function "$output_file" "$current_function" "$spec_content" "$doc_content" "$function_content" "$functions_written"
        ((functions_written++))
      fi

      # Start new function
      current_function="${BASH_REMATCH[1]}"
      function_content="$line"$'\n'
      in_function=true
      in_doc_comment=false
      continue
    fi

    # Continue collecting function content
    if [ "$in_function" = true ]; then
      function_content+="$line"$'\n'
      # Check for function end (period at end of line not in string)
      if [[ "$line" =~ \.[[:space:]]*$ ]] && ! [[ "$line" =~ \" ]]; then
        in_function=false
        write_clean_function "$output_file" "$current_function" "$spec_content" "$doc_content" "$function_content" "$functions_written"
        ((functions_written++))
        current_function=""
        function_content=""
        spec_content=""
        doc_content=""
      fi
    elif [ "$in_doc_comment" = false ]; then
      # Reset doc content if we hit a non-comment, non-function line
      doc_content=""
    fi
  done < "$src_file"

  # Write any remaining function
  if [ -n "$current_function" ] && [ -n "$function_content" ]; then
    write_clean_function "$output_file" "$current_function" "$spec_content" "$doc_content" "$function_content" "$functions_written"
  fi

  # Add footer
  echo "" >> "$output_file"
  echo "---" >> "$output_file"
  echo "" >> "$output_file"
  echo "*Generated from [$module_name.erl]($GITHUB_BASE_URL/${module_name}.erl)*" >> "$output_file"
}

# --- Function to write a function section with cleaner format ---
write_clean_function() {
  local output_file="$1"
  local func_name="$2"
  local spec="$3"
  local doc="$4"
  local code="$5"
  local func_num="$6"

  # Add section separator for better readability (except for first function)
  if [ "$func_num" -gt 0 ]; then
    echo "" >> "$output_file"
  fi

  echo "### $func_name" >> "$output_file"
  echo "" >> "$output_file"

  # Add documentation if present
  if [ -n "$doc" ]; then
    local cleaned_doc=$(extract_function_doc "$doc")
    if [ -n "$cleaned_doc" ]; then
      echo "$cleaned_doc" >> "$output_file"
      echo "" >> "$output_file"
    fi
  fi

  # Add spec if present (in a more compact format)
  if [ -n "$spec" ] && [ "$spec" != $'\n' ]; then
    echo '```erlang' >> "$output_file"
    echo -n "$spec" | sed '/^[[:space:]]*$/d' >> "$output_file"
    echo '```' >> "$output_file"
    echo "" >> "$output_file"
  fi

  # Add implementation
  echo '```erlang' >> "$output_file"
  echo -n "$code" | sed '/^[[:space:]]*$/d' >> "$output_file"
  echo '```' >> "$output_file"
}

# --- Main processing loop ---
log_step "Processing Erlang source files"

# Count total files
total_files=$(find "$ROOT_DIR/src" -name "*.erl" -type f | wc -l)
processed=0

# Process each .erl file in src directory
find "$ROOT_DIR/src" -name "*.erl" -type f | sort | while read -r erl_file; do
  ((processed++))
  module_name=$(basename "$erl_file" .erl)
  log_info "[$processed/$total_files] Processing $module_name.erl"
  process_erlang_file "$erl_file"
done

log_success "Processed $total_files Erlang files"

# --- Generate index file ---
log_step "Generating index file"

cat > "$OUTPUT_DIR/README.md" <<EOF
# HyperBEAM Literate Erlang Documentation

This directory contains literate Erlang documentation generated from the HyperBEAM source code.
Each file combines the source code with embedded documentation in a format optimized for GitHub.

## Modules

EOF

# Add links to all generated files in a cleaner multi-column format
echo "| Module | Description |" >> "$OUTPUT_DIR/README.md"
echo "|--------|-------------|" >> "$OUTPUT_DIR/README.md"

find "$OUTPUT_DIR" -name "*.erl.md" -type f | sort | while read -r md_file; do
  module_name=$(basename "$md_file" .erl.md)
  # Try to extract first line of module doc as description
  first_line=$(grep -m 1 -A 1 "^# $module_name" "$md_file" | tail -1 | head -c 100)
  if [ "$first_line" = "[View source on GitHub]"* ] || [ -z "$first_line" ]; then
    first_line="Erlang module"
  fi
  echo "| [$module_name](./${module_name}.erl.md) | $first_line... |" >> "$OUTPUT_DIR/README.md"
done

cat >> "$OUTPUT_DIR/README.md" <<EOF

## About Literate Programming

Literate programming is a methodology that combines a programming language with a documentation language,
making programs more robust, more portable, and more easily maintained than programs written only in a
high-level language.

These files present the HyperBEAM source code in a narrative format, with documentation and code
interwoven to provide better understanding of the implementation.

---

*Generated on $(date -u +"%Y-%m-%d %H:%M:%S UTC")*
EOF

log_success "Index file created"

# --- Copy files to mdBook structure ---
log_step "Copying files to mdBook structure"

BOOK_SRC_DIR="$ROOT_DIR/docs/book/src"

if [ -d "$BOOK_SRC_DIR" ]; then
    # Copy all generated .erl.md files to the book source directory
    find "$OUTPUT_DIR" -name "*.erl.md" -type f | while read -r md_file; do
        module_name=$(basename "$md_file")
        cp "$md_file" "$BOOK_SRC_DIR/"
        log_verbose "Copied $module_name to book source"
    done

    log_success "Copied $(find "$OUTPUT_DIR" -name "*.erl.md" -type f | wc -l) files to mdBook source directory"
else
    log_info "mdBook structure not found at $BOOK_SRC_DIR - skipping copy"
fi

# --- Final success message ---
echo -e "\n${GREEN}${BOLD}✓ Literate Erlang documentation generated successfully${NC}"
echo -e "${GREEN}   Output directory: $OUTPUT_DIR${NC}"
echo -e "${GREEN}   mdBook source: $BOOK_SRC_DIR${NC}"
echo -e "${GREEN}   Total modules processed: $total_files${NC}\n"