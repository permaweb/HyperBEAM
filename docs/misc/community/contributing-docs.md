# Contributing Documentation

This guide explains how to contribute documentation to the HyperBEAM project. Following these steps will help ensure your documentation is properly integrated into the official documentation.

## Overview

The HyperBEAM documentation is built using [MkDocs](https://www.mkdocs.org/) with the [Material for MkDocs](https://squidfunk.github.io/mkdocs-material/) theme. All documentation is written in Markdown and organized into logical sections within the `docs/` directory.

## Contribution Process

### 1. Fork the Repository

First, fork the [HyperBEAM repository](https://github.com/permaweb/HyperBEAM) to your GitHub account.

### 2. Choose the Right Location

Review the existing documentation structure in `./docs/` to determine the appropriate location for your content. The documentation is organized into several main sections:

- `overview/`: High-level concepts and architecture
- `installation-core/`: Setup and configuration guides
- `components/`: Detailed component documentation
- `usage/`: Tutorials and usage guides
- `resources/`: Reference materials and source code documentation
- `community/`: Contribution guidelines and community resources

### 3. Create Your Documentation

Create a new Markdown file (`.md`) in the appropriate directory. Follow these guidelines:

- Use proper Markdown syntax
- Include clear headings and subheadings
- Add code blocks with appropriate language specification
- Link to related documentation
- For images:
	- Upload images to Arweave using [ArDrive](https://ardrive.io/) or your preferred Arweave upload method
  	- Reference images using their Arweave transaction ID (txid) in the format: `https://arweave.net/<txid>`
  	- Example: `![Image Description](https://arweave.net/abc123...)`
- Follow the existing documentation style and format

### 4. Update the Navigation

Edit `mkdocs.yml` to add your documentation to the navigation:

1. Open `mkdocs.yml`
2. Find the appropriate section under the `nav:` configuration
3. Add your entry following the existing indentation and format
4. Ensure the path to your documentation is correct

### 5. Test Your Changes

Set up a local development environment to test your changes:

```bash
# Create and activate a virtual environment
python3 -m venv venv
source venv/bin/activate  # (macOS/Linux) On Windows use `venv\Scripts\activate`

# Install required packages
pip3 install mkdocs mkdocs-material

# Run the build script
./docs/build-all.sh

# Start a local server
cd mkdocs-site
python3 -m http.server 8000
```

View your documentation at `http://127.0.0.1:8000/` to ensure everything renders correctly.

### 6. Submit a Pull Request

When your documentation is ready:

1. Create a new branch for your changes
2. Commit your changes with a descriptive message
3. Submit a PR with:
	- A clear title describing the documentation addition
	- A detailed description explaining:
		- The purpose of the new documentation
		- Why it should be added to the official docs
		- Any related issues or discussions
	- Screenshots of the rendered documentation (if applicable)

### 7. Review Process

The HyperBEAM team will review your PR and may request changes. Be prepared to:

- Address any feedback
- Make necessary adjustments
- Respond to questions about your contribution

Once approved, your documentation will be merged into the main repository.

## Additional Resources

- [Community Guidelines](./guidelines.md)
- [Development Setup](./setup.md)
- [MkDocs Documentation](https://www.mkdocs.org/)
- [Material for MkDocs Documentation](https://squidfunk.github.io/mkdocs-material/) 