# Device: Data Discovery Engine (~query@1.0)

## Overview

The `~query@1.0` device serves as HyperBEAM's primary data discovery engine, providing flexible search capabilities over cached messages with multiple matching modes and return formats. It enables efficient data retrieval, supports GraphQL queries, and integrates seamlessly with the broader HyperBEAM ecosystem for comprehensive data workflows.

This device is essential for:

- Searching cached messages by key-value pairs with flexible filtering
- Supporting various query modes and return formats for different use cases
- Providing GraphQL query capabilities for complex data operations
- Enabling efficient data discovery in conjunction with replication systems

## Core Concept: Message Discovery

The query device operates as a sophisticated search interface over a node's cached messages, supporting various matching strategies and flexible return formats. It can search by specific keys, all message fields, or custom specifications with configurable filtering and exclusion rules, making it the central hub for data discovery in HyperBEAM applications.

## Key Functions (Keys)

### `all`
Searches for all keys and values in the request message against cached data.

*   **`GET /~query@1.0/all`**
    *   **Action:** Matches all keys in the request against cached messages
    *   **Parameters:**
        *   `exclude`: List of keys to exclude from search (default: ["path", "commitments", "return", "exclude", "only"])
        *   `return`: Return format specification (paths, messages, count, first-path, first-message, boolean)
    *   **Response:** Query results in specified format

### `base`
Searches for all keys and values in the base message.

*   **`GET /~query@1.0/base`**
    *   **Action:** Matches all keys in the base message against cached messages
    *   **Usage:** Useful when query parameters are pre-configured in the base message
    *   **Response:** Query results based on base message keys

### `only`
Searches for specific keys specified in the request.

*   **`GET /~query@1.0/only`**
    *   **Parameters:**
        *   `only`: Key specification in multiple formats:
            *   Binary: Comma-separated key names ("key1,key2,key3")
            *   Map: Direct match specification {"key": "value"}
            *   List: Array of keys to extract from request/base
    *   **Response:** Results matching only specified keys

### `graphql`
Executes GraphQL queries against the node's data.

*   **`POST /~query@1.0/graphql`**
    *   **Action:** Processes GraphQL queries for advanced data retrieval
    *   **Integration:** Delegates to `dev_query_graphql` module
    *   **Request Body:** GraphQL query with variables and operation names
    *   **Response:** GraphQL-formatted results with schema validation

### `has_results`
Determines if a GraphQL response contains transaction results.

*   **`GET /~query@1.0/has_results`**
    *   **Action:** Validates GraphQL responses for gateway client configuration
    *   **Usage:** Enables HyperBEAM's multirequest configuration
    *   **Response:** Boolean indicating presence of results

## Return Format Specifications

### `paths` (Default)
Returns list of message identifiers for efficient reference:
```text
{"return": "paths"}
// Response: ["msg_id_1", "msg_id_2", "msg_id_3"]
```

### `messages`
Returns complete message objects for detailed analysis:
```text
{"return": "messages"}
// Response: [complete_message_1, complete_message_2, ...]
```

### `count`
Returns numerical count of matches for analytics:
```text
{"return": "count"}
// Response: 42
```

### `first-path` / `first-message`
Returns first match for single-result queries:
```text
{"return": "first-message"}
// Response: {first_matching_message}
```

### `boolean`
Returns existence check for conditional logic:
```text
{"return": "boolean"}
// Response: true
```

## Core Dependencies & Architecture

### Cache Layer (`~cache@1.0`)
Primary data source providing:
- Indexed access to stored messages
- Match functionality for key-value searches
- Message loading and serialization management
- Performance optimization through caching strategies

### Storage Layer (`~store@1.0`)
Underlying persistent storage:
- Houses actual message data with durability guarantees
- Supports various storage backends (LMDB, RocksDB)
- Provides indexing capabilities for efficient queries
- Manages storage partitioning and conflict resolution

### Message Processing (`~message@1.0`)
Message format handling:
- Validates message structure and format compliance
- Handles commitment and signature processing
- Manages message serialization across different formats
- Ensures data integrity during query operations

## Device Integration & Synergy

### With Copycat Device (`~copycat@1.0`)
Comprehensive data lifecycle management:

**Data Pipeline:**
```text
External Source → Copycat → Cache → Query → Application
       ↓             ↓         ↓       ↓        ↓
   Remote Data   Replication Storage Discovery  Usage
```

**Workflow Integration:**
1. **Replication Phase:** Copycat imports external data into local cache
2. **Discovery Phase:** Query provides search and discovery over imported data
3. **Application Phase:** Combined pattern enables offline-first applications with complete datasets

**Example Workflow:**
```text
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

### With Authentication Ecosystem
Authenticated and access-controlled queries:

**Protected Data Discovery:**
- Query operations can be protected by `~auth-hook@1.0`
- Enables user-specific data discovery based on wallet identity
- Supports access-controlled search patterns with fine-grained permissions

**Identity-Based Queries:**
```text
GET /<ProcessID>~query@1.0/all
Authorization: Bearer <auth-token>
{
    "owner": "<user-wallet-address>",
    "return": "messages"
}
```

### With Process Device (`~process@1.0`)
Process state discovery and analysis:

**Process Message Discovery:**
```text
GET /<ProcessID>~query@1.0/all
{
    "action": "get-messages",
    "return": "messages"
}
```

**Historical Analysis:**
- Query process message sequences for state reconstruction
- Analyze process evolution over time
- Support process debugging and audit workflows

### With Meta Device (`~meta@1.0`)
Node configuration and resource management:
- Query device configuration through meta device
- Resource allocation for query operations
- Performance monitoring and optimization

## Advanced Query Patterns

### Multi-Key Filtering
All specified keys must match (AND logic):
```text
{
    "type": "message",
    "device": "process@1.0",
    "status": "active",
    "return": "count"
}
```

### Nested Message Support
Search within nested message structures:
```text
{
    "nested.field": "value",
    "complex.path.data": "target",
    "return": "paths"
}
```

### Exclusion-Based Queries
Filter out unwanted data during search:
```text
{
    "type": "message",
    "exclude": ["internal-timestamp", "debug-info", "system-metadata"],
    "return": "messages"
}
```

### Key Priority Resolution
When using key lists in `only` mode:
1. Search in request message first
2. Fall back to base message if not found
3. Filter out `not_found` values automatically
4. Maintain search order for result consistency

## Performance Optimization

### Caching Strategy
- **Index Utilization:** Leverages underlying store's native indexing capabilities
- **Memory Efficiency:** Returns paths by default to minimize memory usage
- **Lazy Loading:** Full messages loaded only when explicitly requested
- **Query Result Caching:** Frequently accessed queries cached for improved performance

### Search Optimization
- **Filtered Matching:** Excludes metadata keys by default for focused searches
- **Batch Processing:** Supports bulk query operations for improved throughput
- **Parallel Processing:** Multiple queries can be processed concurrently
- **Resource Management:** Configurable limits prevent resource exhaustion

## GraphQL Integration

### Advanced Query Processing
The GraphQL integration provides:
- **Schema Validation:** Ensures query structure compliance
- **Variable Substitution:** Dynamic query parameterization
- **Result Formatting:** Standardized GraphQL response format
- **Error Handling:** Comprehensive error reporting and recovery

### Gateway Client Support
The `has_results` function enables:
- **Response Validation:** Ensures query completeness across multiple nodes
- **Multirequest Configuration:** Supports distributed query coordination
- **Result Admissibility:** Determines acceptable response criteria
- **Consensus Building:** Enables distributed data discovery patterns

## Security Considerations

### Access Control
- **Cache Access:** Queries operate within node's security context
- **Key Filtering:** Sensitive keys excluded by default from search results
- **Resource Limits:** Underlying cache implements query limits and rate limiting
- **Authentication Integration:** Subject to node's access control policies

### Data Privacy
- **Selective Exposure:** Configurable key exclusion for privacy protection
- **Audit Logging:** Query operations logged for security monitoring
- **Permission Enforcement:** Integration with authentication ecosystem
- **Secure Defaults:** Conservative security settings by default

## Error Handling & Recovery

### Graceful Degradation
- **Not Found:** Returns appropriate error codes or empty results based on return type
- **Invalid Specifications:** Clear error messages for malformed queries
- **Resource Exhaustion:** Graceful handling of resource limits
- **Partial Failures:** Robust error isolation in complex queries

### Monitoring & Debugging
- **Query Analytics:** Performance metrics and usage statistics
- **Error Tracking:** Comprehensive error logging with context
- **Performance Monitoring:** Query execution time and resource usage tracking
- **Debug Support:** Detailed logging for development and troubleshooting

## Implementation Examples

### Basic Message Discovery
```text
GET /~query@1.0/all
{
    "device": "process@1.0",
    "status": "active",
    "return": "count"
}
```

### Complex Filtered Search
```text
GET /~query@1.0/only
{
    "only": "owner,type,timestamp",
    "owner": "wallet-address",
    "exclude": ["internal-data", "system-info"],
    "return": "messages"
}
```

### GraphQL Query
```text
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

---

**Related Documentation:**
- [Device Overview](./hyperbeam-devices.md) - Understanding the device architecture
- [Building Devices](./building-devices.md) - Creating custom devices
- [Core Devices Index](./index.md) - Complete device catalog