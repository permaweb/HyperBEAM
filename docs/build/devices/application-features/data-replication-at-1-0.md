# Device: ~copycat@1.0

## Overview

The [`~copycat@1.0`](../source-code/dev_copycat.md) device replicates data from external sources into HyperBEAM node caches. It fetches messages from GraphQL endpoints and Arweave nodes, enabling offline-first applications through local data caching.

## Core Concept: Data Ingestion

The `~copycat@1.0` device acts as a data ingestion orchestrator that fetches messages from external sources and imports them into the local node's cache system. It supports multiple engines (GraphQL, Arweave) and handles pagination automatically during large-scale replication operations.

## Key Functions (Keys)

*   **`POST /~copycat@1.0/graphql`**
    *   **Action:** Queries remote GraphQL endpoints and indexes results locally.
    *   **Parameters:**
        *   `query`: GraphQL query string or structured specification
        *   `variables`: Query variables for parameterized queries
        *   `operationName`: Specific operation in multi-operation queries
        *   `node`: Target GraphQL endpoint URL
    *   **Response:** Total number of successfully indexed messages with batch statistics.
    *   **Processing:** Automatic pagination handling, message parsing, and cache integration.

*   **`POST /~copycat@1.0/arweave`**
    *   **Action:** Connects directly to Arweave nodes and imports transaction/block data.
    *   **Parameters:**
        *   `node`: Target Arweave node URL
        *   `from`: Starting block height
        *   `to`: Ending block height
        *   `filter`: Transaction filtering criteria
    *   **Response:** Replication status with imported message count and range coverage.
    *   **Integration:** Uses `~arweave@2.9-pre` device for native Arweave communication.

## Data Sources

*   **GraphQL Endpoints:** Arweave Gateway APIs (arweave.net, ar.io gateways), custom GraphQL services, and federated endpoints with multi-endpoint coordination.
*   **Arweave Nodes:** Direct node integration for block-level replication with height-based ranges and transaction indexing.

## Filter Examples

**Tag-Based Filtering:**

```json
{
    "tag": "Content-Type",
    "value": "application/json"
}
```

**Owner-Based Filtering:**

```json
{
    "owner": "wallet-address-here"
}
```

**Multi-Tag Filtering:**

```json
{
    "tags": {
        "App-Name": "MyApplication",
        "Content-Type": "application/json",
        "Version": "1.0"
    }
}
```

**Comprehensive Replication:**

```json
{
    "all": true,
    "node": "https://source-gateway.net/graphql"
}
```

## Replication Workflow

The device processes data through these steps:

1. **Fetch Batch**: Retrieve data from GraphQL endpoints
2. **Parse Messages**: Convert to HyperBEAM format
3. **Validate Format**: Ensure structure compliance
4. **Write to Cache**: Store in node cache
5. **Update Progress**: Track statistics

Automatic pagination continues fetching until all data is replicated, with configurable batch sizes for optimal performance.

## Integration Example

Query replicated data after import:

```
// Phase 1: Replicate application data
POST /~copycat@1.0/graphql
{
    "tag": "App-Name",
    "value": "MyApp",
    "node": "https://arweave.net/graphql"
}

// Phase 2: Query replicated data locally
GET /~query@1.0/all
{
    "tag": "App-Name",
    "return": "count"
}
```

## Practical Implications

**For Application Developers:**

Copycat transforms your HyperBEAM node into a complete data mirror. Instead of hitting external gateways for every query, replicate once and query locally via `~query@1.0`. This enables offline-first architectures where your application continues functioning even when external networks fail. Automatic pagination handles large datasets without memory constraints.

**For Node Operators:**

Strategic replication reduces external API dependencies and improves response times. Replicate application-specific data subsets using tag filters rather than entire chains. Monitor cache growth—replicated data consumes disk space proportional to message count and size. Schedule periodic replication jobs to keep local caches synchronized with source networks.

**For End Users:**

Applications using copycat load faster and work offline. Once data replicates to your node, subsequent interactions happen instantly without network round-trips. This is particularly valuable for mobile or low-connectivity scenarios where traditional blockchain applications struggle.

## See Also

- [`~query@1.0`](../source-code/dev_query.md) - For querying replicated data
- [`~cache@1.0`](../source-code/dev_cache.md) - Storage target for replicated data
- [`~arweave@2.9-pre`](../source-code/dev_arweave.md) - Arweave node communication

[copycat module](../source-code/dev_copycat.md)
