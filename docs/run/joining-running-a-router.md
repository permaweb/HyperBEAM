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

- **You can join or run a router without HB-OS.**
- **If you want to run a TEE node, HB-OS or an equivalent TEE setup is recommended for convenience and security.**

## Configuration Files: config.json vs config.flat

Configuration can be set in either `config.json` (JSON syntax) or `config.flat` (flat syntax). The examples below use JSON for clarity, but you can use either format depending on your deployment. The syntax differs:

- **config.json** uses standard JSON structure (see examples below)
- **config.flat** uses key-value pairs, e.g.:

```flat
operator: trustless
initialized: permanent
snp_trusted: []
# ...and so on
```

## Joining a Router Network (Worker Node)

Most users want to **join** an existing router to offer computational services. This does NOT require HB-OS or TEE unless you specifically want TEE security.

### Step-by-Step: Registering as a Worker Node

#### 1. Prepare Your Configuration (config.json example)

Use the following configuration as a template for your worker node:

```json
{
  "operator": "trustless",
  "initialized": "permanent",
  "snp_trusted": [],
  "on": {
    "request": {
      "device": "p4@1.0",
      "ledger-device": "lua@5.3a",
      "pricing-device": "simple-pay@1.0",
      "ledger-path": "/ledger~node-process@1.0",
      "module": ""
    },
    "response": {
      "device": "p4@1.0",
      "ledger-device": "lua@5.3a",
      "pricing-device": "simple-pay@1.0",
      "ledger-path": "/ledger~node-process@1.0",
      "module": ""
    }
  },
  "p4_non_chargable_routes": [
    {"template": "/.*~node-process@1.0/.*"},
    {"template": "/.*~greenzone@1.0/.*"},
    {"template": "/.*~router@1.0/.*"},
    {"template": "/.*~meta@1.0/.*"},
    {"template": "/schedule"},
    {"template": "/push"},
    {"template": "/~hyperbuddy@1.0/.*"}
  ],
  "node_process_spawn_codec": "ans104@1.0",
  "node_processes": {
    "ledger": {
      "device": "process@1.0",
      "execution-device": "lua@5.3a",
      "scheduler-device": "scheduler@1.0",
      "authority-match": 1,
      "admin": "",
      "token": "",
      "module": "",
      "authority": ""
    }
  },
  "router_opts": {
    "offered": [ /* ... */ ]
  },
  "green_zone_peer_location": "",
  "green_zone_peer_id": "",
  "p4_recipient": ""
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

```json
{
  "on": {
    "request": {
      "device": "router@1.0",
      "path": "preprocess",
      "commit-request": true
    }
  },
  "router_opts": {
    "provider": {
      "path": "/router~node-process@1.0/compute/routes~message@1.0"
    },
    "registrar": {
      "path": "/router~node-process@1.0"
    },
    "registrar-path": "schedule"
  },
  "relay_allow_commit_request": true,
  "node_processes": {
    "router": {
      "type": "Process",
      "device": "process@1.0",
      "execution-device": "lua@5.3a",
      "scheduler-device": "scheduler@1.0",
      "pricing-weight": 9,
      "performance-weight": 1,
      "score-preference": 4,
      "performance-period": 2,
      "initial-performance": 1000,
      "is-admissible": {
        "path": "default",
        "default": "false"
      },
      "module": "",
      "trusted-peer": "",
      "trusted": ""
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