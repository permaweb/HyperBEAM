# Troubleshooting Guide

This guide addresses common issues you might encounter when working with HyperBEAM and the Compute Unit.

## Installation Issues

### Erlang Installation Fails

**Symptoms**: Errors during Erlang compilation or installation

**Solutions**:

- Ensure all required dependencies are installed: `sudo apt-get install -y libssl-dev ncurses-dev make cmake gcc g++`
- Try configuring with fewer options: `./configure --without-wx --without-debugger --without-observer --without-et`
- Check disk space, as compilation requires several GB of free space

### Rebar3 Bootstrap Fails

**Symptoms**: Errors when running `./bootstrap` for Rebar3

**Solutions**:

- Verify Erlang is correctly installed: `erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'`
- Ensure you have the latest version of the repository: `git fetch && git reset --hard origin/master`
- Try manually downloading a precompiled Rebar3 binary

## HyperBEAM Issues

### HyperBEAM Won't Start

**Symptoms**: Errors when running `rebar3 shell` or the HyperBEAM startup command

**Solutions**:

- Check for port conflicts: Another service might be using the configured port
- Verify the wallet key file exists and is accessible
- Examine Erlang crash dumps for detailed error information
- Ensure all required dependencies are installed

### HyperBEAM Crashes During Operation

**Symptoms**: Unexpected termination of the HyperBEAM process

**Solutions**:

- Check system resources (memory, disk space)
- Examine Erlang crash dumps for details
- Reduce memory limits if the system is resource-constrained
- Check for network connectivity issues if connecting to external services

## Compute Unit Issues

### Compute Unit Won't Start

**Symptoms**: Errors when running `npm start` in the CU directory

**Solutions**:

- Verify Node.js is installed correctly: `node -v`
- Ensure all dependencies are installed: `npm i`
- Check that the wallet file exists and is correctly formatted
- Verify the `.env` file has all required settings

### Memory Errors in Compute Unit

**Symptoms**: Out of memory errors or excessive memory usage

**Solutions**:

- Adjust the `PROCESS_WASM_MEMORY_MAX_LIMIT` environment variable
- Enable garbage collection by setting an appropriate `GC_INTERVAL_MS`
- Monitor memory usage and adjust limits as needed
- If on a low-memory system, reduce concurrent process execution

## Integration Issues

### HyperBEAM Can't Connect to Compute Unit

**Symptoms**: Connection errors in HyperBEAM logs when trying to reach the CU

**Solutions**:

- Verify the CU is running: `curl http://localhost:6363`
- Ensure there are no firewall rules blocking the connection
- Verify network configuration if components are on different machines

### Process Execution Fails

**Symptoms**: Errors when deploying or executing processes

**Solutions**:

- Check both HyperBEAM and CU logs for specific error messages
- Verify that the WASM module is correctly compiled and valid
- Test with a simple example process to isolate the issue
- Adjust memory limits if the process requires more resources

## Getting Help

If you're still experiencing issues after trying these troubleshooting steps:

1. Check the [GitHub repository](https://github.com/permaweb/HyperBEAM) for known issues
2. Join the [Discord community](https://discord.gg/V3yjzrBxPM) for support
3. Open an issue on GitHub with detailed information about your problem 