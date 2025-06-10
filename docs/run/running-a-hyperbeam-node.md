# Running a HyperBEAM Node

This guide provides the basics for running your own HyperBEAM node, installing dependencies, and connecting to the AO network.

## System Dependencies

To successfully build and run a HyperBEAM node, your system needs several software dependencies installed.

=== "macOS"
    Install core dependencies using [Homebrew](https://brew.sh/):

    ```bash
    brew install cmake git pkg-config openssl ncurses
    ```

=== "Linux (Debian/Ubuntu)"
    Install core dependencies using `apt`:
    ```bash
    sudo apt-get update && sudo apt-get install -y --no-install-recommends \
        build-essential \
        cmake \
        git \
        pkg-config \
        ncurses-dev \
        libssl-dev \
        sudo \
        curl
        ca-certificates
    ```

=== "Windows (WSL)"
    Using the Windows Subsystem for Linux (WSL) with a distribution like Ubuntu is recommended. Follow the Linux (Debian/Ubuntu) instructions within your WSL environment.

<!-- **Core Dependency Breakdown:**

*   `build-essential` (Linux) / Xcode Command Line Tools (macOS): Basic C/C++ compilers and build tools (gcc, g++, make).
*   `cmake`: Build system generator.
*   `git`: Version control for fetching the source code.
*   `pkg-config`: Helps find installed libraries during compilation.
*   `ncurses-dev` / `ncurses`: Required for some terminal interface elements used by Erlang/OTP.
*   `libssl-dev` / `openssl`: Necessary for cryptographic operations and secure connections (HTTPS). You might need to set specific environment variables for `openssl` during Erlang compilation if building from source on macOS.
*   `sudo` (Linux/macOS): Needed for system-level installations.
*   `curl`: Used for downloading dependencies or interacting with web services.
*   `ca-certificates` (Linux): Required for validating SSL certificates (often handled by the OS on macOS/Windows).

*(Note: Package names may vary slightly on other Linux distributions. Use your system's package manager accordingly, e.g., `yum`, `dnf`, `pacman`.)* -->

### Erlang/OTP

HyperBEAM is built on Erlang/OTP. You need a compatible version installed (check the `rebar.config` or project documentation for specific version requirements, **typically OTP 27**).

Installation methods:

=== "macOS (brew)"
    ```bash
    brew install erlang
    ```

=== "Linux (apt)"
    ```bash
    sudo apt install erlang
    ```
<!-- 
=== "asdf (Recommended)"
    Tools like `asdf-vm` with the `asdf-erlang` plugin are highly recommended for managing multiple Erlang versions across platforms.
    ```bash
    asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git
    asdf install erlang <version> # e.g., 27.0
    asdf global erlang <version>
    ``` -->

=== "Source Build"
    Download from [erlang.org](https://www.erlang.org/downloads) and follow the build instructions for your platform.

### Rebar3

Rebar3 is the build tool for Erlang projects.

Installation methods:

=== "macOS (brew)"
    ```bash
    brew install rebar3
    ```

=== "Linux / macOS (Direct Download)"
    Get the `rebar3` binary from the [official website](https://rebar3.org/). Place the downloaded `rebar3` file in your system's `PATH` (e.g., `/usr/local/bin`) and make it executable (`chmod +x rebar3`).

<!-- === "asdf (Recommended)"
    If using `asdf`, you can install it via the `rebar` plugin:
    ```bash
    asdf plugin add rebar https://github.com/asdf-vm/asdf-rebar.git
    asdf install rebar <version> # e.g., 3.23.0
    asdf global rebar <version>
    ``` -->

### Node.js

Node.js might be required for certain JavaScript-related tools or dependencies.

Installation methods:

=== "macOS (brew)"
    ```bash
    brew install node
    ```

=== "Linux (apt)"
    ```bash
    # Check your distribution's recommended method, might need nodesource repo
    sudo apt install nodejs npm 
    ```

=== "asdf (Recommended)"
    `asdf-vm` with the `asdf-nodejs` plugin is recommended.
    ```bash
    asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
    asdf install nodejs <version> # e.g., lts
    asdf global nodejs <version>
    ```

### Rust

Rust is needed if you intend to work with or build components involving WebAssembly (WASM) or certain Native Implemented Functions (NIFs) used by some devices (like `~snp@1.0`).

The recommended way to install Rust on **all platforms** is via `rustup`:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source "$HOME/.cargo/env" # Or follow the instructions provided by rustup
```

## Prerequisites for Running

Before starting a node, ensure you have:

*   Installed the [system dependencies](#system-dependencies) mentioned above.
*   Cloned the [HyperBEAM repository](https://github.com/permaweb/HyperBEAM) (`git clone ...`).
*   Compiled the source code (`rebar3 compile` in the repo directory).
*   An Arweave **wallet keyfile** (e.g., generated via [Wander](https://www.wander.app)). The path to this file is typically set via the `hb_key` configuration option (see [Configuring Your HyperBEAM Node](./configuring-your-machine.md)).

## Starting a Basic Node

The simplest way to start a HyperBEAM node for development or testing is using `rebar3` from the repository's root directory:

```bash
rebar3 shell
```

This command:

1.  Starts the Erlang Virtual Machine (BEAM) with all HyperBEAM modules loaded.
2.  Initializes the node with default settings (from `hb_opts.erl`).
3.  Starts the default HTTP server (typically on **port 10000**), making the node accessible via HyperPATHs.
4.  Drops you into an interactive Erlang shell where you can interact with the running node.

This basic setup is suitable for local development and exploring HyperBEAM's functionalities.

## Optional Build Profiles

HyperBEAM uses build profiles to enable optional features, often requiring extra dependencies. To run a node with specific profiles enabled, use `rebar3 as ... shell`:

**Available Profiles (Examples):**

*   `genesis_wasm`: Enables Genesis WebAssembly support.
*   `rocksdb`: Enables the RocksDB storage backend.
*   `http3`: Enables HTTP/3 support.

**Example Usage:**

```bash
# Start with RocksDB profile
rebar3 as rocksdb shell

# Start with RocksDB and Genesis WASM profiles
rebar3 as rocksdb, genesis_wasm shell
```

*Note: Choose profiles **before** starting the shell, as they affect compile-time options.*

## Node Configuration

HyperBEAM offers various configuration options (port, key file, data storage, logging, etc.). These are primarily set using a `config.flat` file and can be overridden by environment variables or command-line arguments.

See the dedicated **[Configuring Your HyperBEAM Node](./configuring-your-machine.md)** guide for detailed information on all configuration methods and options.

## Verify Installation

To quickly check if your node is running and accessible, you can send a request to its `~meta@1.0` device (assuming default port 10000):

```bash
curl http://localhost:10000/~meta@1.0/info
```

A JSON response containing node information indicates success.

## Running for Production (Mainnet)

While you can connect to the main AO network using the `rebar3 shell` for testing purposes (potentially using specific configurations or helper functions like `hb:start_mainnet/1` if available and applicable), the standard and recommended method for a stable production deployment (like running on the mainnet) is to build and run a **release**.

**1. Build the Release:**

From the root of the HyperBEAM repository, build the release package. You might include specific profiles needed for your mainnet setup (e.g., `rocksdb` if you intend to use it):

```bash
# Build release with default profile
rebar3 release

# Or, build with specific profiles (example)
# rebar3 as rocksdb release
```

This command compiles the project and packages it along with the Erlang Runtime System (ERTS) and all dependencies into a directory, typically `_build/default/rel/hb`.

**2. Configure the Release:**

Navigate into the release directory (e.g., `cd _build/default/rel/hb`). Ensure you have a correctly configured `config.flat` file here. See the [configuration guide](./configuring-your-machine.md) for details on setting mainnet parameters (port, key file location, store path, specific peers, etc.). Environment variables can also be used to override settings in the release's `config.flat` when starting the node.

**3. Start the Node:**

Use the generated start script (`bin/hb`) to run the node:

```bash
# Start the node in the foreground (logs to console)
./bin/hb console

# Start the node as a background daemon
./bin/hb start

# Check the status
./bin/hb ping
./bin/hb status

# Stop the node
./bin/hb stop
```

Consult the generated `bin/hb` script or Erlang/OTP documentation for more advanced start-up options (e.g., attaching a remote shell).

Running as a release provides a more robust, isolated, and manageable way to operate a node compared to running directly from the `rebar3 shell`.

## Stopping the Node (rebar3 shell)

To stop the node running *within the `rebar3 shell`*, press `Ctrl+C` twice or use the Erlang command `q().`.

## Next Steps

*   **Configure Your Node:** Deep dive into [configuration options](./configuring-your-machine.md).
*   **TEE Nodes:** Learn about running nodes in [Trusted Execution Environments](./tee-nodes.md) for enhanced security.
*   **Routers:** Understand how to configure and run a [router node](./joining-running-a-router.md).
