# Router Networks: Joining vs Running

Router networks in HyperBEAM have two distinct roles that are often confused:

!!! info "Two Different Concepts"
    - **Joining a router** = Registering your worker node with an existing router to receive work
    - **Running a router** = Operating a router that manages and distributes work to other nodes

## Joining a Router Network (Worker Node)

Most users want to **join** an existing router to offer computational services. This is what the configuration below accomplishes.

!!! tip "Recommended Approach"
    Use [HyperBEAM OS](https://github.com/permaweb/hb-os) for the simplest setup with built-in TEE security and automatic configuration.

### Quick Start with HyperBEAM OS

The fastest way to join a router network is using HyperBEAM OS, which creates a measured VM image with everything pre-configured:

```bash
# Clone and build the worker node image
git clone https://github.com/permaweb/hb-os.git && cd hb-os
./run init && ./run build_base && ./run build_guest
```

### Configure Your Worker Node

Edit `resources/hyperbeam/config.flat` to set your worker node registration:

```flat
port: 10000
operator: your-wallet-address
router-peer-location: https://router-1.forward.computer
router-prefix: /my-unique-node~
router-template: /my-unique-node~process@1.0/.*
router-price: 100
```

### Launch Your Worker Node

```bash
./run start  # Starts the TEE-protected worker node
```

Your node will automatically register with the specified router network and begin receiving work.

!!! note "Using Routers as a Client"
    Make requests to any router URL: `https://router-1.forward.computer/<process_id>~<device>/<key>...`

### Configuration Options

Your worker node can be customized with these key parameters in `config.flat`:

- **`router-peer-location`**: Target router to register with (e.g., `https://router-1.forward.computer`)
- **`router-prefix`**: Unique identifier for your node (e.g., `/my-node~`)
- **`router-template`**: Path pattern to handle (e.g., `/my-node~process@1.0/.*`)
- **`router-price`**: Cost per computation unit (competitive pricing recommended)
- **`operator`**: Your wallet address to receive payments

### How Registration Works

When your worker node starts, it automatically:

1. **Registers** with the target router using your configured parameters
2. **Gets validated** by the router's admissibility checks (most routers accept all nodes)
3. **Starts receiving** requests routed to your prefix
4. **Reports performance** metrics back to the router for load balancing

### Monitoring Your Worker Node

Check your worker node's status with:

```bash
# View logs for registration confirmation
DEBUG=HB_PRINT ./run start

# Look for messages like:
# [info] Router registration successful
# [debug] Route added to routing table
```

## Running Your Own Router

If you want to **operate a router** that manages other worker nodes (advanced use case):

### Router Requirements

- Deploy the dynamic router Lua process to handle registrations
- Configure trusted software hashes for TEE validation
- Set up load balancing and performance monitoring
- Manage worker node admissibility policies

### Basic Router Setup

```erlang
% Start a node that can act as a router
hb:start_mainnet(#{
    % Standard node configuration
    port => 8080,
    
    % Router-specific configuration
    routes => [
        #{template => "/~meta@1.0/.*", target => self},
        #{template => "/.*", target => "http://default-worker.com"}
    ],
    
    % Trust and validation settings
    snp_trusted => [#{
        % Trusted software configurations for TEE validation
    }]
}).
```

!!! warning "Advanced Configuration"
    Running a production router requires careful consideration of security, performance, and economic incentives. Most users should join existing routers rather than run their own.

## Alternative: Manual Setup

<details>
<summary>Advanced users can set up HyperBEAM manually (click to expand)</summary>

**Direct Installation**

```bash
# Clone HyperBEAM
git clone https://github.com/permaweb/HyperBEAM.git && cd HyperBEAM
rebar3 compile

# Create config.flat with router settings
cat > config.flat << EOF
port: 10000
operator: your-wallet-address
router-peer-location: https://router-1.forward.computer
router-prefix: /my-node~
router-template: /my-node~process@1.0/.*
router-price: 100
EOF

# Start the node
rebar3 shell --eval "hb:start_mainnet()."
```

**Environment Variables**

```bash
# Alternative: use environment variables
export ROUTER_PEER_LOCATION="https://router-1.forward.computer"
export ROUTER_PREFIX="/my-prefix~"
export ROUTER_TEMPLATE="/my-prefix~process@1.0/.*"
export ROUTER_PRICE=100
rebar3 shell --eval "hb:start_mainnet()."
```

</details>

## Further Exploration

*   Examine the `dev_router.erl` source code for detailed implementation.
*   Review the `scripts/dynamic-router.lua` for router-side logic.
*   Review the available configuration options in `hb_opts.erl` related to routing (`routes`, strategies, etc.).
*   Consult community channels for best practices on deploying production routers.
