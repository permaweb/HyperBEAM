# **HyperBEAM Repository Setup**

This guide provides step-by-step instructions for setting up and testing HyperBEAM.

!!! info "TEE-based Computation"
    If you intend to offer TEE-based computation of AO-Core devices, please see the [HyperBEAM OS repository](https://github.com/permaweb/hb-os) for details on configuration and deployment. Additional documentation on TEE setup and configuration will be added here in future updates.

## **Prerequisites**

Before you begin, ensure you have the following installed:

- The Erlang runtime (OTP 27)
- Rebar3
- Git

## **1. Clone the HyperBEAM Repository**

First, clone the `permaweb/HyperBEAM` repository from GitHub:

```bash
git clone https://github.com/permaweb/HyperBEAM
```

Navigate to the project directory:

```bash
cd HyperBEAM
```

## **2. Compile the Code with Rebar3**

To compile the HyperBEAM code, you'll need to use Rebar3. Run the following command to compile the project:

```bash
rebar3 compile
```

This will compile the necessary code to get HyperBEAM up and running.

## **3. Run HyperBEAM with Shell**

Once the code is compiled, you can start the HyperBEAM shell with Rebar3:

```bash
rebar3 shell
```

This will start HyperBEAM using the default configuration inside the hb_opts.erl,
which preloads all devices and sets up default stores. All of these can be configured using
the config.flat file with any overrides you specify.

To verify that your HyperBEAM node is running correctly, you can check:

```bash
curl http://localhost:10000/~meta@1.0/info
```
If you receive a response with node information, your HyperBEAM installation is working properly.

## **4. Create and Run a HyperBEAM Release**

For a more stable setup, especially when connecting to networks like mainnet or using specific features, it's recommended to create a release.

### **a. Configure Your Node**

HyperBEAM uses a `config.flat` file for configuration when running as a release. A sample file is included in the repository.

1.  Locate the `config.flat` file in the root of the HyperBEAM project directory.
2.  Edit the file to specify your desired settings. For example, to set the port and specify your wallet key file:

    ```
    port: 10001
    priv_key_location: /path/to/your/wallet.json
    # Add other configurations as needed
    ```
    Ensure the `priv_key_location` points to the correct path of your Arweave wallet key file.

### **b. Build the Release (with Optional Profiles)**

You can build a standard release or include specific profiles for additional features (like `genesis_wasm`, `rocksdb`, `http3`).

To build a standard release:
```bash
rebar3 release
```

To build a release with specific profiles (e.g., `rocksdb`):
```bash
rebar3 as rocksdb release
```

This command creates a self-contained release package in the `_build/default/rel/hb` directory.

### **c. Run the Release**

Navigate to the release directory and start the HyperBEAM node:

```bash
cd _build/default/rel/hb
./bin/hb console
```
Replace `console` with `start` to run it in the background.

!!! note "Stopping the Node"
    To stop a HyperBEAM node started with `./bin/hb start`, run `./bin/hb stop` from the release directory (`_build/default/rel/hb`). If started with `./bin/hb console`, press `Ctrl+C` in the terminal to stop it.

### **d. Verify the Release Node**

Once the node is running, verify it by checking the meta device info endpoint. Use the port you specified in your `config.flat` (e.g., 10001):

```bash
curl http://localhost:10001/~meta@1.0/info
```

If you receive a response with node information, your HyperBEAM release is configured and running correctly.

## **Next Steps**

After setting up HyperBEAM, you should:

1. [Configure your installation](configuration.md) to match your requirements
2. [Run tests](testing.md) to verify everything is working correctly
3. [Connect to the Compute Unit](../compute-unit/setup.md) to complete your setup


