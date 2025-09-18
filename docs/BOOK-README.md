
# HyperBEAM Documentation

This directory contains the documentation build system for HyperBEAM, implementing a literate programming approach using mdBook to generate browsable documentation directly from Erlang source code.

## Overview

The documentation system consists of two main components:

1. **Literate Erlang Generator**: Converts HyperBEAM Erlang source code into markdown files with embedded documentation
2. **mdBook Documentation Site**: Compiles the generated markdown into a browsable documentation website

## Quick Start

```bash
# Generate literate docs and build the book
./docs/build-literate-erlang.sh    # Generate .erl.md files from source
cd docs/book && mdbook build       # Build the documentation site
cd docs/book && mdbook serve       # Serve locally on http://localhost:3471
```

## Build Process

### 1. Literate Erlang Generation

The `build-literate-erlang.sh` script processes all `.erl` files in `src/` and generates corresponding `.erl.md` files with:

- Module documentation extracted from `%%%` and `%% @doc` comments
- Function documentation from preceding comment blocks
- Type specifications (`-spec`)
- Source code formatted in markdown code blocks
- Links to GitHub source files

**Input**: `src/*.erl` (HyperBEAM Erlang source files)
**Output**:
- `docs/literate-erlang/*.erl.md` (intermediate generated files)
- `docs/book/src/*.erl.md` (copied for mdBook processing)

### 2. mdBook Compilation

The mdBook system takes the generated `.erl.md` files and compiles them into a static documentation website.

**Configuration**: `docs/book/book.toml`
**Source**: `docs/book/src/`
**Output**: `docs/book/dist/` (static website)

## File Structure

```
docs/
├── README.md                    # This file
├── build-literate-erlang.sh    # Literate Erlang generator
├── literate-erlang/            # Generated .erl.md files (gitignored)
└── book/
    ├── book.toml               # mdBook configuration
    ├── README.md               # mdBook setup instructions
    ├── custom.css              # Custom styling
    ├── custom.js               # Custom JavaScript
    ├── src/
    │   ├── SUMMARY.md          # Navigation structure
    │   ├── introduction.md     # Introduction page
    │   └── *.erl.md            # Generated docs (gitignored)
    └── dist/                   # Built documentation site (gitignored)
```

## Generated Files

**Important**: All `.erl.md` files are generated artifacts and should not be committed to version control. They are automatically excluded via `.gitignore`.

- `docs/literate-erlang/*.erl.md` - Intermediate generated files
- `docs/book/src/*.erl.md` - Files copied for mdBook processing
- `docs/book/dist/` - Final compiled documentation website

## Dependencies

- **mdBook**: Install via `cargo install mdbook` or download from [GitHub](https://github.com/rust-lang/mdBook)
- **Bash**: For running build scripts (available on Unix-like systems)

## Development Workflow

1. **Modify Erlang source code** in `src/` with proper documentation comments
2. **Run build script** to regenerate documentation
3. **Preview locally** using `mdbook serve`
4. **Deploy** the `docs/book/dist/` directory to hosting platform

## Documentation Standards

### Module Documentation

Use `%%%` or `%% @doc` at the beginning of files:

```erlang
%%%-------------------------------------------------------------------
%%% @doc Module for handling HyperBEAM caching operations.
%%%
%%% This module provides...
%%% @end
%%%-------------------------------------------------------------------
```

### Function Documentation

Use comment blocks before function definitions:

```erlang
%% @doc Retrieves a value from the cache.
%%
%% Returns the cached value for the given key, or `undefined` if not found.
-spec get(Key :: term()) -> term() | undefined.
get(Key) ->
    % Implementation...
```

## Deployment

The built documentation in `docs/book/dist/` can be deployed to any static hosting service:

- **Vercel**: Automatic deployment from git repository
- **GitHub Pages**: Use GitHub Actions to build and deploy
- **Netlify**: Connect repository and set build command to `./docs/build-literate-erlang.sh && cd docs/book && mdbook build`

## Troubleshooting

### Common Issues

1. **mdBook not found**: Install mdBook using `cargo install mdbook`
2. **Permission denied**: Make scripts executable with `chmod +x docs/*.sh`
3. **Empty output**: Ensure Erlang files have proper documentation comments

### Cleaning Up

```bash
# Remove all generated files
rm -rf docs/literate-erlang/
rm -f docs/book/src/*.erl.md
rm -rf docs/book/dist/

# Regenerate everything
./docs/build-literate-erlang.sh
cd docs/book && mdbook build
```

---

For more information about the HyperBEAM project, see the main [README](../README.md).
