# Development Setup

This guide will help you set up your development environment for contributing to HyperBEAM.

## Prerequisites

Before starting, ensure you have the following installed:

- **Ubuntu 22.04** (primary development platform)
- **Git**
- **Erlang/OTP 27**
- **Rebar3**
- **Rust** (for certain components)
- **A code editor** (Cursor, VSCode, Emacs, Vim, etc.)

## Setting Up Your Development Environment

### 1. Fork and Clone the Repository

First, fork the HyperBEAM repository on GitHub, then clone your fork:

```bash
git clone https://github.com/YOUR-USERNAME/HyperBEAM.git
cd HyperBEAM
```

### 2. Add the Upstream Remote

Add the original repository as an upstream remote to keep your fork in sync:

```bash
git remote add upstream https://github.com/permaweb/HyperBEAM.git
```

### 3. Install Dependencies

#### HyperBEAM Core (Erlang) Dependencies

```bash
# Install Erlang dependencies
rebar3 compile
```

### 4. Run Tests

Verify that your setup is working correctly by running the tests:

```bash
# Run Erlang tests
rebar3 eunit
```

## Development Workflow

### 1. Keep Your Fork Updated

Regularly sync your fork with the upstream repository:

```bash
git fetch upstream
git checkout main
git merge upstream/main
```

### 2. Create a Feature Branch

Create a branch for each feature or bugfix:

```bash
git checkout -b feature/your-feature-name
```

or

```bash
git checkout -b fix/bug-you-are-fixing
```

### 3. Development Cycle

The typical development cycle is:

1. Make changes to the code
2. Write tests for your changes
3. Run the tests to verify your changes
4. Commit your changes
5. Push to your fork
6. Create a pull request

### 4. Running HyperBEAM Locally

To run HyperBEAM for development:

#### Start HyperBEAM

```bash
rebar3 shell
```

In the Erlang shell:

```erlang
application:ensure_all_started(hyperbeam).
```


### 5. Debugging

#### Erlang Debugging

You can use the Erlang debugger or add logging:

```erlang
?event({debug, Variable}).
% or for more context:
?event(module_name, {debug_label, Variable}).
% for more detailed output with explicit information:
?event(module_name, {debug_label, {explicit, Variable}}).
```

To filter logs for specific modules or files, you can prefix the rebar3 shell command with the `HB_PRINT` environment variable, providing a comma-separated list of module or file names:

```bash
HB_PRINT=module_name1,module_name2,filename1 rebar3 shell
```

This will show debug logs only from the specified modules or files, making it easier to focus on relevant information during development and troubleshooting.


## Code Editor Setup

### VS Code

We recommend the following extensions for VS Code:

- Erlang LS
- erlang
- Erlang/OTP

## Common Issues and Solutions

### Permission Issues

If you encounter permission issues:

```bash
sudo chown -R $(whoami) .
```

## Getting Help

If you need help with your development setup:

- Check existing issues on GitHub
- Ask for help in the Discord channel
- Create a new issue with the "question" label

## Next Steps

Once your development environment is set up, check out the [Contribution Guidelines](guidelines.md) for information on how to submit your changes. 