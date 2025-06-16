# Integrating AO-Core into UIs

## Overview

This guide provides a practical approach to using the `patch@1.0` AO-Core device to create RESTful-like APIs for AO processes. If you're familiar with dryruns, this method offers a more efficient alternative by making process data directly accessible via HTTP endpoints from any operational AO Mainnet HyperBEAM node, eliminating the need for repeated dryruns. Responses are cryptographically signed and linked to individual nodes.

## Key Features

- **HTTP Endpoints**: Access process data via HTTP, similar to RESTful APIs
- **Cryptographic Signatures**: Each response is signed, ensuring data integrity
- **No DryRuns**: Directly access and update process states without the overhead of dryruns
- **Real-time State Access**: Get the latest process state without waiting for dryrun computation

## Implementation Steps

### Initial State Synchronization

Add an initial sync at the top of your process code to export the initial state:

```lua
-- Sync state on spawn
InitialSync = InitialSync or 'INCOMPLETE'
if InitialSync == 'INCOMPLETE' then
   Send({
    device = 'patch@1.0',
    cache = {
      table1 = { 
        [recordId1] = table1[recordId1] 
      },
      table2 = {
        [recordId2] = table2[recordId2]
      }
    }
  })
  InitialSync = 'COMPLETE'
end
```

### State Updates During Operation

Incorporate patch messages wherever state changes occur. Example for an auction system:

```lua
-- Inside any handler that modifies data
Handlers.add('update-data', function(msg)
  -- Process your logic...
  table1[recordId1].field = msg.newValue
  table2[recordId2] = { 
    field1 = msg.value1, 
    field2 = msg.From 
  }
  
  -- Export the updated state
  Send({
    device = 'patch@1.0',
    cache = {
      table1 = { 
        [recordId1] = table1[recordId1] 
      },
      table2 = {
        [recordId2] = table2[recordId2]
      }
    }
  })
  
  -- Rest of handler logic...
end)
```

### Accessing Your API

Access your process data via any HyperBEAM node. This provides immediate access to the latest state without the need for dryruns:

- **Latest State**: `GET /YOUR_PROCESS_ID~process@1.0/now/cache`
- **Pre-computed State**: `GET /YOUR_PROCESS_ID~process@1.0/compute/cache`

## Best Practices

- **Selective Updates**: For large datasets, update only the altered data
- **Consistent State Updates**: Ensure all state-changing handlers include patch messages
- **Custom State Naming**: Name the state something other than `cache` if needed, and adjust HTTP requests accordingly
- **Migration Strategy**: Consider gradually transitioning from dryruns to this approach for existing applications

## Important Note

This approach leverages HyperBEAM milestone 3 functionality and is currently in preview. It is not recommended for applications that may lead to loss of value due to potential changes and existing bugs. If you're currently using dryruns, consider this as a more efficient alternative for state access, but maintain your existing dryrun-based validation where needed. 