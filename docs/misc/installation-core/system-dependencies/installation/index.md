# Installation Overview

HyperBEAM requires several dependencies to be installed on your system. This guide will walk you through the installation process for each component.

## Installation Order

For the best experience, we recommend installing prerequisites in this order:

1. System dependencies (build tools, libraries)
2. Erlang/OTP (programming language for HyperBEAM)
3. Rebar3 (Erlang build tool)
4. Node.js (required for the Compute Unit)
5. Rust (required for the dev_snp_nif)

## Component Guides

Follow these guides in sequence to set up your environment:

1. [System Dependencies](dependencies.md) - Basic system packages
2. [Erlang Installation](erlang.md) - Programming language for HyperBEAM
3. [Rebar3 Installation](rebar3.md) - Build tool for Erlang
4. [Node.js Installation](nodejs.md) - Required for the Compute Unit
5. [Rust Installation](rust.md) - Required for the dev_snp_nif

## Next Steps

After installing all the dependencies, you can proceed to:

- [HyperBEAM Setup](../../hyperbeam/setup.md)
- [Compute Unit Setup](../../compute-unit/setup.md) 