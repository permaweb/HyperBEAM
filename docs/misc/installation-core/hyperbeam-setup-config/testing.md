# Testing HyperBEAM

This guide covers how to test your HyperBEAM installation to ensure it's working correctly.

## Unit Tests

HyperBEAM comes with a suite of unit tests that can be run to verify the installation and functionality.

### Running All Tests

To run all unit tests for HyperBEAM, use the following Rebar3 command:

```bash
rebar3 eunit
```

This will execute the EUnit tests and provide the results in your terminal.

### Running Tests for a Specific Module

To run tests for a specific module, use the following command:

```bash
rebar3 eunit --module dev_meta
```

This will run the tests for the `dev_meta` module.

### Running a Specific Test in a Module

To run a specific test within a module, use the `--test` flag with the module name and test function. 
For example, to run the `config_test` in the `dev_meta` module:

```bash
rebar3 eunit --test dev_meta:config_test
```

## Troubleshooting Failed Tests

If tests fail, check the following:

1. Ensure all dependencies are installed correctly
2. Verify that HyperBEAM is properly configured
3. Look for error messages in the test output
4. Examine the HyperBEAM logs for more details

### Common Issues

- **Connection refused**: Ensure HyperBEAM is running on the expected port
- **Authentication errors**: Check your wallet configuration
- **Device not found**: Verify the device is included in your HyperBEAM configuration

For specific error messages, refer to the [Troubleshooting Guide](../reference/troubleshooting.md). 