
# Device: ~query@1.0

## Overview

The [`~query@1.0`](./source-code/dev_query.md) device serves as HyperBEAM's primary data discovery engine, providing flexible search capabilities over cached messages. It enables efficient data retrieval with multiple matching modes and return formats.

This device is useful for:

* Searching cached messages by key-value pairs with flexible filtering
* Supporting various query modes for different use cases
* Providing GraphQL query capabilities for complex data operations
* Enabling efficient data discovery in conjunction with replication systems

## Core Concept: Message Discovery

The query device operates as a search interface over a node's cached messages, supporting various matching strategies and flexible return formats. It can search by specific keys, all message fields, or custom specifications with configurable filtering and exclusion rules, making it the central hub for data discovery in HyperBEAM applications.

## Key Functions (Keys)

### `all`

* **`GET /~query@1.0/all`**
  * **Action:** Matches all keys in the request against cached messages.
  * **Inputs:**
    * `exclude`: List of keys to exclude from search (default: ["path", "commitments", "return", "exclude", "only"])
    * `return`: Return format specification (paths, messages, count, first-path, first-message, boolean)
  * **Response:** Query results in the specified format.

### `base`

* **`GET /~query@1.0/base`**
  * **Action:** Matches all keys in the base message against cached messages.
  * **Response:** Query results based on base message keys.
  * This is useful when query parameters are pre-configured in the base message.

### `only`

* **`GET /~query@1.0/only`**
  * **Action:** Searches for specific keys specified in the request.
  * **Inputs:**
    * `only`: Key specification in multiple formats:
      * Binary: Comma-separated key names ("key1,key2,key3")
      * Map: Direct match specification {"key": "value"}
      * List: Array of keys to extract from request/base
  * **Response:** Results matching only specified keys.

### `graphql`

* **`POST /~query@1.0/graphql`**
  * **Action:** Processes GraphQL queries for advanced data retrieval.
  * **Inputs:** GraphQL query with variables and operation names in the request body.
  * **Response:** GraphQL-formatted results with schema validation.
  * Delegates to the `dev_query_graphql` module for processing.

### `has_results`

* **`GET /~query@1.0/has_results`**
  * **Action:** Determines if a GraphQL response contains transaction results.
  * **Response:** Boolean indicating presence of results.
  * Enables HyperBEAM's multirequest configuration for gateway clients.

## Return Format Specifications

The `return` parameter controls the format of query results:

* **`paths`** (Default): Returns list of message identifiers for efficient reference.
  ```
  {"return": "paths"}
  // Response: ["msg_id_1", "msg_id_2", "msg_id_3"]
  ```

* **`messages`**: Returns complete message objects for detailed analysis.
  ```
  {"return": "messages"}
  // Response: [complete_message_1, complete_message_2, ...]
  ```

* **`count`**: Returns numerical count of matches for analytics.
  ```
  {"return": "count"}
  // Response: 42
  ```

* **`first-path`** / **`first-message`**: Returns first match for single-result queries.
  ```
  {"return": "first-message"}
  // Response: {first_matching_message}
  ```

* **`boolean`**: Returns existence check for conditional logic.
  ```
  {"return": "boolean"}
  // Response: true
  ```

## Device Integration

### With Copycat Device (`~copycat@1.0`)

The query device works seamlessly with the copycat device to enable comprehensive data lifecycle management:

1. **Replication Phase:** Copycat imports external data into local cache
2. **Discovery Phase:** Query provides search and discovery over imported data
3. **Application Phase:** Combined pattern enables offline-first applications with complete datasets

**Example Workflow:**
```
// First: Replicate data from external source
POST /~copycat@1.0/graphql
{
    "tag": "App-Name",
    "value": "MyApp"
}

// Then: Query replicated data locally
GET /~query@1.0/all
{
    "tag": "App-Name",
    "return": "count"
}
```

### With Process Device (`~process@1.0`)

The query device can be used for process state discovery and analysis:

```
GET /<ProcessID>~query@1.0/all
{
    "action": "get-messages",
    "return": "messages"
}
```

This enables:
* Query process message sequences for state reconstruction
* Analysis of process evolution over time
* Support for process debugging and audit workflows

## Advanced Query Patterns

### Multi-Key Filtering

All specified keys must match (AND logic):
```
{
    "type": "message",
    "device": "process@1.0",
    "status": "active",
    "return": "count"
}
```

### Nested Message Support

Search within nested message structures:
```
{
    "nested.field": "value",
    "complex.path.data": "target",
    "return": "paths"
}
```

### Exclusion-Based Queries

Filter out unwanted data during search:
```
{
    "type": "message",
    "exclude": ["internal-timestamp", "debug-info", "system-metadata"],
    "return": "messages"
}
```

## Performance Optimization

The query device implements several optimization strategies:

* **Index Utilization:** Leverages underlying store's native indexing capabilities
* **Memory Efficiency:** Returns paths by default to minimize memory usage
* **Lazy Loading:** Full messages loaded only when explicitly requested
* **Query Result Caching:** Frequently accessed queries cached for improved performance

## Security Considerations

* **Cache Access:** Queries operate within node's security context
* **Key Filtering:** Sensitive keys excluded by default from search results
* **Resource Limits:** Underlying cache implements query limits and rate limiting
* **Authentication Integration:** Subject to node's access control policies

## Error Handling

* **Not Found:** Returns appropriate error codes or empty results based on return type
* **Invalid Specifications:** Clear error messages for malformed queries
* **Resource Exhaustion:** Graceful handling of resource limits
* **Partial Failures:** Robust error isolation in complex queries

## Implementation Examples

### Basic Message Discovery
```
GET /~query@1.0/all
{
    "device": "process@1.0",
    "status": "active",
    "return": "count"
}
```

### Complex Filtered Search
```
GET /~query@1.0/only
{
    "only": "owner,type,timestamp",
    "owner": "wallet-address",
    "exclude": ["internal-data", "system-info"],
    "return": "messages"
}
```

### GraphQL Query
```
POST /~query@1.0/graphql
{
    "query": "query GetProcessMessages($processId: String!) {
        messages(processId: $processId) {
            id owner timestamp data
        }
    }",
    "variables": {"processId": "process-123"}
}
```

## See Also

- [`~cache@1.0`](./source-code/dev_cache.md) - Primary data source for query operations
- [`~store@1.0`](./source-code/dev_store.md) - Underlying persistent storage
- [`~message@1.0`](./source-code/dev_message.md) - Message format handling
- [`~copycat@1.0`](./source-code/dev_copycat.md) - Data replication device

[query module](./source-code/dev_query.md)
