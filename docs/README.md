
## Documentation

HyperBEAM uses [MkDocs](https://www.mkdocs.org/) with the [Material for MkDocs](https://squidfunk.github.io/mkdocs-material/) theme to build its documentation site.

Building the documentation requires Python 3 and pip. It's recommended to use a virtual environment:

```bash
# Create and activate a virtual environment (optional but recommended)
python3 -m venv venv
source venv/bin/activate  # (macOS/Linux) On Windows use `venv\Scripts\activate`

# Install required packages
pip3 install mkdocs mkdocs-material mkdocs-git-revision-date-localized-plugin

# Deactivate the virtual environment when done
# deactivate
```

- **Source Files:** All documentation source files (Markdown `.md`, images, CSS) are located in the `docs/` directory.
- **Source Code Docs:** Erlang source code documentation is generated using `rebar3 edoc` (with the `edown_doclet` plugin) into the `docs/source-code-docs/` directory as Markdown files. These are then incorporated into the main MkDocs site.
- **Build Script:** The entire process (compiling, generating edoc, processing source docs, building the site) is handled by the `./docs/build-all.sh` script.

To build the documentation locally:

1.  Ensure you are in the project root directory.
2.  If using a virtual environment, make sure it's activated.
3.  Run the build script:
    ```bash
    ./docs/build-all.sh
    ```

This script performs the following steps:
- Compiles the Erlang project (`rebar3 compile`).
- Generates Markdown documentation from source code comments (`rebar3 edoc`) into `docs/source-code-docs/`.
- Processes the generated source code Markdown files (updates index, cleans up TOCs).
- Builds the MkDocs site into the `mkdocs-site` directory (`mkdocs build`).

To view the built documentation locally:

1.  Navigate to the site directory:
    ```bash
    cd mkdocs-site
    ```
2.  Start a simple Python HTTP server:
    ```bash
    python3 -m http.server 8000
    ```
3.  Open your web browser and go to `http://127.0.0.1:8000/`.

Press `Ctrl+C` in the terminal where the server is running to stop it.

The final static site is generated in the `mkdocs-site` directory, as configured in `mkdocs.yml` (`site_dir: mkdocs-site`).

### Contributing to the Documentation

To contribute documentation to HyperBEAM, follow these steps:

1. **Fork the Repository**
   - Fork the [HyperBEAM repository](https://github.com/permaweb/HyperBEAM) to your GitHub account

2. **Choose the Right Location**
   - Review the existing documentation structure in `./docs/` to determine the appropriate location for your content
   - Documentation is organized into several main sections:
     - `overview/`: High-level concepts and architecture
     - `installation-core/`: Setup and configuration guides
     - `components/`: Detailed component documentation
     - `usage/`: Tutorials and usage guides
     - `resources/`: Reference materials and source code documentation
     - `community/`: Contribution guidelines and community resources

3. **Create Your Documentation**
   - Create a new Markdown file (`.md`) in the appropriate directory
   - Follow the existing documentation style and format
   - Use proper Markdown syntax and include:
     - Clear headings and subheadings
     - Code blocks with appropriate language specification
     - Links to related documentation
     - Images (if needed) in the `docs/assets/` directory

4. **Update the Navigation**
   - Edit `mkdocs.yml` to add your documentation to the navigation
   - Place your entry in the appropriate section under the `nav:` configuration
   - Follow the existing indentation and format

5. **Test Your Changes**
   - Set up a local development environment:
     ```bash
     python3 -m venv venv
     source venv/bin/activate
     pip3 install mkdocs mkdocs-material mkdocs-git-revision-date-localized-plugin
     ```
   - Run the build script to verify your changes:
     ```bash
     ./docs/build-all.sh
     ```
   - View the documentation locally at `http://127.0.0.1:8000/`

6. **Submit a Pull Request**
   - Create a new branch for your documentation changes
   - Commit your changes with a descriptive message
   - Submit a PR with:
     - A clear title describing the documentation addition
     - A detailed description explaining:
       - The purpose of the new documentation
       - Why it should be added to the official docs
       - Any related issues or discussions
     - Screenshots of the rendered documentation (if applicable)

7. **Review Process**
   - The HyperBEAM team will review your PR
   - Be prepared to make adjustments based on feedback
   - Once approved, your documentation will be merged into the main repository

For more detailed contribution guidelines, see the [Community Guidelines](./docs/misc/community/guidelines.md) and [Development Setup](./docs/misc/community/setup.md) documentation.
