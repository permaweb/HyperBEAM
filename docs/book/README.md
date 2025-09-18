# HyperBEAM Literate Documentation

This repository contains the mdBook-based documentation for HyperBEAM, generated from Erlang source files using a literate programming approach.

## Overview

The documentation combines Erlang source code with comprehensive documentation in a format optimized for both reading and LLM consumption. Each `.erl.md` file represents a module from the HyperBEAM codebase with embedded documentation, function signatures, and implementation details.

## Generation Process

### 1. Source Documentation Generation

Documentation is generated from the HyperBEAM repository using the literate Erlang script:

```bash
# From the HyperBEAM repository
./docs/build-literate-erlang.sh
```

This script:
- Extracts module documentation from `%%%` comments
- Converts edoc tags (`@author`, `@copyright`, `@doc`, `@end`) to markdown format
- Processes function documentation and specifications
- Converts quote patterns (`'text'` to `text`) for proper backtick formatting
- Generates individual `.erl.md` files for each module

### 2. Copy Generated Files

Copy the generated documentation to this book's source directory:

```bash
# Copy from HyperBEAM docs/literate-erlang/ to src/
cp /path/to/HyperBEAM/docs/literate-erlang/*.erl.md src/
```

### 3. Build the mdBook

Generate the final documentation:

```bash
mdbook build
```

This creates the static HTML documentation in the `book/` directory.

## Features

### Enhanced Copy Functionality

The documentation includes a custom copy button (📋 icon) in the top-right corner that:
- Fetches the original markdown content from the `src/` directory
- Copies the raw markdown to clipboard for LLM use
- Preserves all formatting, code blocks, and structure exactly as written

### Theme Support

Supports all mdBook themes with HyperBEAM brand colors:
- **Neon Green**: `#00ff94`
- **Cyan**: `#00d4ff`
- **Yellow**: `#fff700`
- **Magenta**: `#ff006a`

### Clean Documentation Structure

Each module page includes:
- GitHub source link pointing to the `edge` branch
- **Author** and **Copyright** information (when available)
- Exported functions list
- Function documentation with signatures
- Implementation code blocks
- Test functions (when present)

## Configuration

### book.toml

Key configuration options:

```toml
[book]
title = "HyperBEAM Literate Documentation"
src = "src"

[build]
build-dir = "book"

[output.html]
additional-css = ["custom.css"]
additional-js = ["custom.js"]
edit-url-template = "https://github.com/permaweb/HyperBEAM/edit/edge/src/{path}"
git-repository-url = "https://github.com/permaweb/HyperBEAM"
```

### Custom Styling

- `custom.css`: HyperBEAM brand colors for all themes
- `custom.js`: Copy functionality and theme detection

## Development Workflow

1. **Update source documentation**: Run `./docs/build-literate-erlang.sh` in HyperBEAM repo
2. **Copy to book**: Transfer generated `.erl.md` files to `src/`
3. **Build book**: Run `mdbook build`
4. **Serve locally**: Use `mdbook serve` for development

## Repository Structure

```
HB-DevicesBook/
├── src/                    # Markdown source files
│   ├── *.erl.md           # Generated module documentation
│   └── SUMMARY.md         # Book structure
├── book/                  # Generated HTML output
├── custom.css             # HyperBEAM theme styling
├── custom.js              # Copy functionality
├── book.toml              # mdBook configuration
└── README.md              # This file
```

## Notes

- The documentation is generated from the HyperBEAM `edge` branch
- All GitHub links point to the source files in the HyperBEAM repository
- The copy functionality fetches original markdown for accurate LLM consumption
- Search is enabled with fuzzy matching and result limiting for performance