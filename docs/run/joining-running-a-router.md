# Router Networks: Joining vs Running

Router networks in HyperBEAM have two distinct roles that are often confused:

!!! info "Two Different Concepts"
    - **Joining a router** = Registering your worker node with an existing router to receive work
    - **Running a router** = Operating a router that manages and distributes work to other nodes

## When to use HB-OS

| Operation                | Use HB-OS?    | Purpose                                             |
|-------------------------|:-------------:|-----------------------------------------------------|
| **TEE Node (SNP)**      |   Recommended | Secure, attested computation (hardware isolation)    |
| **Router Registration** |   Optional    | Registering/joining a router (TEE not required)      |

- *You can join or run a router without HB-OS.*
- *If you want to run a TEE node, HB-OS or an equivalent TEE setup is recommended for convenience and security.*

## Configuration Files: config.json vs config.flat

Configuration can be set in either `config.json` (JSON syntax) or `config.flat` (flat syntax). The examples below use JSON for clarity, but you can use either format depending on your deployment. The syntax differs:

- **config.json** uses standard JSON structure (see examples below)
- **config.flat** uses key-value pairs

## Joining a Router Network (Worker Node)

Most users want to **join** an existing router to offer computational services. This does NOT require HB-OS or TEE unless you specifically want TEE security.

### Step-by-Step: Registering as a Worker Node

#### 1. Prepare Your Configuration (config.json example)

Use the following configuration as a template for your worker node:

```jsonc
{
    // ─── Initial Configuration ─────────────────────────────────────────────────
    // Lock this configuration so it cannot be changed again
    "operator": "trustless",
    "initialized": "permanent",

    // ─── SNP-Based TEE Attestation Parameters ──────────────────────────────────
    // These values let the TEE verify its own environment—and any other VM
    // instantiated from the same image—before granting access.
    "snp_trusted": [],

    // ─── Request/Response Processing Configuration ─────────────────────────────
    // Defines how requests and responses are processed through the p4 device
    "on": {
        "request": {
            "device": "p4@1.0",
            "ledger-device": "lua@5.3a",
            "pricing-device": "simple-pay@1.0",
            "ledger-path": "/ledger~node-process@1.0",
            "module": ""        // Automatically injected
        },
        "response": {
            "device": "p4@1.0",
            "ledger-device": "lua@5.3a",
            "pricing-device": "simple-pay@1.0",
            "ledger-path": "/ledger~node-process@1.0",
            "module": ""        // Automatically injected
        }
    },

    // ─── Non-Chargeable Routes Configuration ──────────────────────────────────
    // Routes that should not incur charges when accessed through p4
    "p4_non_chargable_routes": [
        { "template": "/.*~node-process@1.0/.*" },
        { "template": "/.*~greenzone@1.0/.*" },
        { "template": "/.*~router@1.0/.*" },
        { "template": "/.*~meta@1.0/.*" },
        { "template": "/schedule" },
        { "template": "/push" },
        { "template": "/~hyperbuddy@1.0/.*" }
    ],

    // ─── Node Process Spawn Configuration ─────────────────────────────────────
    // Codec used for spawning new node processes
    "node_process_spawn_codec": "ans104@1.0",

    // ─── Node Process Definitions ─────────────────────────────────────────────
    // Configuration for individual node processes
    "node_processes": {
        "ledger": {
            "device": "process@1.0",
            "execution-device": "lua@5.3a",
            "scheduler-device": "scheduler@1.0",
            "authority-match": 1,
            "admin": "",                   // Automatically injected
            "token": "",                   // Automatically injected
            "module": "",                  // Automatically injected
            "authority": ""                // Automatically injected
        }
    },

    // ─── Router Registration Options ──────────────────────────────────────────
    // Configuration for how processes register with the router
    "router_opts": {
        "offered": [
            // {
            //     "registration-peer": {},            // Automatically injected
            //     "template": "/*~process@1.0/*",   // The routes that the node will register with
            //     "prefix": "",                       // Automatically injected
            //     "price": 4500000                    // Registration fee in smallest units
            // }
        ]
    },

    // ─── Greenzone Registration Options ────────────────────────────────────────
    // Configuration for how processes register with the greenzone
    "green_zone_peer_location": "",         // Automatically injected
    "green_zone_peer_id": "",               // Automatically injected

    // ─── P4 Recipient ──────────────────────────────────────────────────────────
    // The Address of the node that will receive the P4 messages
    "p4_recipient": ""                      // Automatically injected
}
```

#### 2. Register Your Node

Perform the following API calls in order:

- **Meta Info Post:**
  - Endpoint: `~meta@1.0/info POST`
  - Example:
    ```javascript
    const response = await fetch(`${nodeUrl}/~meta@1.0/info`, {
        method: 'POST',
        headers: {
            'codec-device': 'json@1.0',
            'accept-bundle': true
        },
        body: JSON.stringify(configContent)
    });
    ```

- **Join Green Zone:**
  - Endpoint: `~greenzone@1.0/join GET`

- **Become Green Zone Member:**
  - Endpoint: `~greenzone@1.0/become GET`

- **Register as Router:**
  - Endpoint: `~router@1.0/register GET`

#### 3. Verify Registration

- Check your node's status in the network
- Confirm green zone membership
- Test routing functionality

#### 4. Troubleshooting

If registration fails:
1. Verify all configuration parameters are correct
2. Check network connectivity to the node URL
3. Ensure proper headers are set in API requests
4. Review logs for specific error messages
5. Confirm green zone availability and accessibility

## Running Your Own Router (Advanced)

If you want to **operate a router** that manages other worker nodes:

- Deploy the dynamic router Lua process to handle registrations
- Configure trusted software hashes for TEE validation (if using TEE)
- Set up load balancing and performance monitoring
- Manage worker node admissibility policies

### Example Router Configuration (config.json example)

```jsonc
{
    // ─── Router Node Preprocessing Settings ───────────────────────────────────
    // Defines the router process and how it preprocesses incoming requests
    "on": {
        "request": {
            "device": "router@1.0",
            "path": "preprocess",
            "commit-request": true         // Enable request commitment for routing
        }
    },

    // ─── Route Provider Configuration ─────────────────────────────────────────
    // Specifies where to get routing information from the router node process
    "router_opts": {
        "provider": {
            "path": "/router~node-process@1.0/compute/routes~message@1.0"
        },
        "registrar": {
            "path": "/router~node-process@1.0"
        },
        "registrar-path": "schedule"
    },

    // ─── Relay Configuration ──────────────────────────────────────────────────
    // Allow the relay to commit requests when forwarding
    "relay_allow_commit_request": true,

    // ─── Router Node Process Configuration ────────────────────────────────────
    // Specifies the Lua-based router logic, weights for scoring, and admission check
    "node_processes": {
        "router": {
            "type": "Process",
            "device": "process@1.0",
            "execution-device": "lua@5.3a",
            "scheduler-device": "scheduler@1.0",
            "pricing-weight": 9,           // Weight for pricing in routing decisions
            "performance-weight": 1,       // Weight for performance in routing decisions
            "score-preference": 4,         // Preference scoring for route selection
            "performance-period": 2,       // Period for performance measurement
            "initial-performance": 1000,   // Initial performance score
            // Default admission policy (currently set to false)
            "is-admissible": {
                "path": "default",
                "default": "false"
            },
            "module": "",                  // Automatically injected
            "trusted-peer": "",            // Automatically injected
            "trusted": ""                  // Automatically injected
        }
    }
}
```

!!! warning "Advanced Configuration"
    Running a production router requires careful consideration of security, performance, and economic incentives. Most users should join existing routers rather than run their own.

## Further Exploration

- Examine the `dev_router.erl` source code for detailed implementation.
- Review the `scripts/dynamic-router.lua` for router-side logic.
- Review the available configuration options in `hb_opts.erl` related to routing (`routes`, strategies, etc.).
- Consult community channels for best practices on deploying production routers.