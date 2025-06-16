# Developer Troubleshooting Guide

This guide addresses common issues you might encounter when developing processes for HyperBEAM.

## Process Execution Fails

**Symptoms**: Errors when deploying or executing processes

**Solutions**:

- Check both HyperBEAM and CU logs for specific error messages
- Verify that the WASM module is correctly compiled and valid
- Test with a simple example process to isolate the issue
- Adjust memory limits if the process requires more resources

## Memory Errors in Compute Unit

**Symptoms**: Out of memory errors or excessive memory usage during process execution

**Solutions**:

- Adjust the `PROCESS_WASM_MEMORY_MAX_LIMIT` environment variable
- Enable garbage collection by setting an appropriate `GC_INTERVAL_MS`
- Monitor memory usage and adjust limits as needed
- If on a low-memory system, reduce concurrent process execution

## Getting Help

If you're still experiencing issues after trying these troubleshooting steps:

1. Check the [GitHub repository](https://github.com/permaweb/HyperBEAM) for known issues
2. Join the [Discord community](https://discord.gg/V3yjzrBxPM) for support
3. Open an issue on GitHub with detailed information about your problem 