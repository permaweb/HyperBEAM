# Device: ~query@1.0

## Overview

The [`~query@1.0`](./source-code/dev_query.md) device provides flexible search capabilities over cached messages in HyperBEAM. It enables efficient data discovery through multiple matching strategies and return formats, making it the central hub for searching and retrieving messages stored in the node's cache.

## Core Concept: Message Discovery

The query device operates as a search interface over a node's cached messages. It supports various matching strategies—searching by specific keys, all message fields, or custom specifications with configurable filtering and exclusion rules. This flexibility allows applications to efficiently discover and retrieve exactly the data they need, whether performing simple lookups or complex multi-criteria searches.

## Key Functions (Keys)

*   **`GET /~query@1.0/all`**
    *   **Action:** Matches all keys in the request against cached messages.
    *   **Parameters:**
        *   `exclude` (default: `["path", "commitments", "return", "exclude", "only"]`): Filters out system keys from the search
        *   `return`: Controls the response format

*   **`GET /~query@1.0/base`**
    *   **Action:** Matches all keys in the base message against cached messages. This is useful when query parameters are pre-configured in the base message rather than passed as request parameters.

*   **`GET /~query@1.0/only`**
    *   **Action:** Searches for specific keys specified in the request.
    *   **Formats:**
        *   **Binary**: Comma-separated key names (`"key1,key2,key3"`)
        *   **Map**: Direct match specification (`{"key": "value"}`)
        *   **List**: Array of keys to extract from request/base

*   **`POST /~query@1.0/graphql`**
    *   **Action:** Processes GraphQL queries for advanced data retrieval with full schema validation.
    *   **Request Body:** GraphQL query, variables, and operation names.
    *   **Processing:** Delegated to the `dev_query_graphql` module.

*   **`GET /~query@1.0/has_results`**
    *   **Action:** Determines if a GraphQL response contains transaction results. This enables HyperBEAM's multirequest configuration for gateway clients by allowing the system to verify whether a response should be considered admissible.

## Return Format Specifications

The `return` parameter controls how query results are formatted:

* **`paths`** (Default): Returns list of message identifiers for efficient reference
* **`messages`**: Returns complete message objects for detailed analysis
* **`count`**: Returns numerical count of matches for analytics
* **`first-path` / `first-message`**: Returns first match for single-result queries
* **`boolean`**: Returns existence check for conditional logic

## Device Integration

### With Copycat Device (`~copycat@1.0`)

The query device works seamlessly with copycat for comprehensive data lifecycle management. After copycat imports external data into local cache, query provides search and discovery over that imported data, enabling offline-first applications with complete datasets.

**Example Workflow:**
```
// Replicate data from external source
POST /~copycat@1.0/graphql
{
    "tag": "App-Name",
    "value": "MyApp"
}

// Query replicated data locally
GET /~query@1.0/all
{
    "tag": "App-Name",
    "return": "count"
}
```

### With Process Device (`~process@1.0`)

Query process message sequences for state reconstruction and analysis:

```
GET /<ProcessID>~query@1.0/all
{
    "action": "get-messages",
    "return": "messages"
}
```

This enables process debugging, audit workflows, and analysis of process evolution over time.

## Advanced Query Patterns

### Multi-Key Filtering

All specified keys must match using AND logic:
```
{
    "type": "message",
    "device": "process@1.0",
    "status": "active",
    "return": "count"
}
```

### Nested Message Support

Search within nested message structures using dot notation:
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

!!! tip "Optimization Best Practices"
    The query device returns paths by default to minimize memory usage. Only request full messages (`return: "messages"`) when you actually need the complete message content. For counting or existence checks, use `return: "count"` or `return: "boolean"` for optimal performance.

The query device implements several optimization strategies to ensure fast searches. It leverages the underlying store's native indexing capabilities and only loads full messages when explicitly requested. Frequently accessed queries are cached for improved performance, reducing latency for repeated searches.

## Security Considerations

Queries operate within the node's security context and respect access control policies. Sensitive keys are excluded by default from search results. The underlying cache implements query limits and rate limiting to prevent resource exhaustion. All queries are subject to the node's authentication and authorization framework.


## Practical Implications

**For Application Developers:**

The query device provides instant search over locally cached messages without external API calls. Use `return: "count"` for analytics dashboards, `return: "boolean"` for conditional UI logic, and `return: "paths"` for efficient pagination. GraphQL support enables complex filtering for data-rich applications.

**For Node Operators:**

Query performance scales with cache size—index frequently queried keys in your underlying store. The default `exclude` parameter protects system metadata from leaking in search results. Configure appropriate query limits to prevent resource exhaustion from unbounded searches.

**For End Users:**

Applications built on query device respond instantly even offline. Once data replicates via copycat, all searches happen locally—no network latency, no gateway dependencies. Your data queries work identically whether the network is available or not.

## See Also

- [`~cache@1.0`](./source-code/dev_cache.md) - Primary data source for query operations
- [`~store@1.0`](./source-code/dev_store.md) - Underlying persistent storage
- [`~message@1.0`](./source-code/dev_message.md) - Message format handling
- [`~copycat@1.0`](./source-code/dev_copycat.md) - Data replication device

[query module](./source-code/dev_query.md)