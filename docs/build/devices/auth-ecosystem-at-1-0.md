# Device: Authentication Ecosystem (~auth-hook@1.0, ~secret@1.0, ~cookie@1.0, ~http-auth@1.0)

## Overview

The HyperBEAM Authentication Ecosystem provides a comprehensive suite of devices that enable wallet-less blockchain applications through server-side key management and transparent authentication. This ecosystem eliminates traditional blockchain UX friction by handling cryptographic operations transparently while maintaining security through trusted execution environments.

The ecosystem consists of four interconnected devices:

- **`~auth-hook@1.0`**: Main authentication interceptor and request signer
- **`~secret@1.0`**: Wallet generation and secret management
- **`~cookie@1.0`**: Session-based authentication via HTTP cookies
- **`~http-auth@1.0`**: HTTP Basic authentication with PBKDF2 key derivation

## Core Concept: Zero-Friction Authentication

The authentication ecosystem transforms blockchain application UX from multi-step wallet connection processes into single-click experiences. Users interact with decentralized applications exactly like traditional web applications, while HyperBEAM handles all cryptographic operations server-side.

## Primary Device: ~auth-hook@1.0

### Authentication Hook Overview
The auth-hook device serves as the main entry point for wallet-less authentication, intercepting HTTP requests and automatically signing them with node-hosted wallets according to operator configuration.

### Key Functions

#### `request`
Processes incoming requests through a configured secret provider:
*   **Action:** Intercepts HTTP requests containing the `&!` parameter
*   **Process Flow:**
    1. Detect `&!` pattern in URL parameters
    2. Find and validate secret provider configuration
    3. Check relevance based on `when` conditions
    4. Generate or retrieve authentication secrets via provider
    5. Create wallet if needed through `~secret@1.0`
    6. Sign request and message sequences
    7. Finalize with provider-specific post-processing

### Configuration Parameters

#### Secret Provider Integration
```text
"secret-provider": {
    "device": "cookie@1.0",
    "generate-path": "generate",
    "finalize-path": "finalize"
}
```

#### Activation Conditions
```text
"when": {
    "committers": "uncommitted" | "always" | ["address1", "address2"],
    "keys": "always" | ["authorization", "custom-header"]
}
```

## Supporting Device: ~secret@1.0

### Secret Management Overview
The secret device manages wallet creation, storage, and commitment operations for node-hosted secrets with configurable access control and persistence modes.

### Key Functions

#### `generate`
Creates new wallets with authentication setup:

*   **Parameters:** `access-control`, `persist`, `controllers`, `required-controllers`
*   **Persistence Modes:**
    - `client`: Generated server-side, returned to client
    - `in-memory`: Session-based storage
    - `non-volatile`: Persistent disk storage

#### `commit`
Signs messages using hosted secrets:

*   **Access Control:** Dual-layer security through access-control messages and controller verification
*   **Multi-Signature:** Supports required-controllers threshold

#### `export` / `sync`
Manages wallet portability and node synchronization:

*   **Export:** Access-controlled wallet extraction
*   **Sync:** Cross-node wallet synchronization

### Integration with Auth-Hook

1. Auth-hook generates/retrieves secrets via provider
2. Passes secrets to `~secret@1.0` for wallet operations
3. Uses generated wallets for request signing
4. Maintains session consistency through provider

## Provider Device: ~cookie@1.0

### Cookie Authentication Overview
Manages HTTP cookie-based authentication and session persistence, implementing the generator interface for auth-hook integration.

### Key Functions

#### Generator Interface Implementation
```text
generate: Creates or retrieves authentication secrets from cookies
finalize: Adds set-cookie headers to response sequences
```

#### Cookie Management

*   **Formats:** set-cookie (full attributes), cookie (simple), structured-fields (internal)
*   **Security:** HMAC-SHA256 commitments, HTTPOnly, Secure, SameSite support
*   **Storage:** `wallet-<address>` and `secret-<keyid>` cookie patterns

### Session Flow

1. **Initial Request:** Generate secret, store in cookie, return Set-Cookie header
2. **Subsequent Requests:** Extract secret from cookie, validate, use for signing
3. **Session Persistence:** Maintain authentication across browser sessions

## Provider Device: ~http-auth@1.0

### HTTP Authentication Overview
Implements HTTP Basic authentication with PBKDF2 key derivation, providing enterprise-grade authentication for protected resources.

### Key Functions

#### `generate`
Processes HTTP Authorization headers:

*   **PBKDF2 Parameters:** 1,200,000 iterations, SHA256, 64-byte keys
*   **Security:** 2x OWASP recommendation, ~5-10 derivations/second
*   **401 Response:** Triggers browser authentication prompts

#### Authentication Flow

1. **No Credentials:** Return 401 with WWW-Authenticate header
2. **Basic Auth:** Extract and decode Authorization header
3. **Key Derivation:** Apply PBKDF2 to credentials
4. **Secret Generation:** Use derived key for signing operations

## Device Integration & Synergy

### Complete Authentication Workflow
```text
HTTP Request → Auth-Hook → Provider (Cookie/HTTP-Auth) → Secret → Wallet → Signed Response
     ↓             ↓            ↓                        ↓        ↓           ↓
  &! Parameter  Intercept   Generate Secret         Create Key  Sign Msg   Set Cookie
```

### Integration with Core Devices

#### With Process Device (`~process@1.0`)

- Automatic signing of process communications
- Seamless AO process interaction without wallet setup
- Transparent message commitment in ANS-104 format

#### With Query Device (`~query@1.0`)
- Authenticated data discovery and search
- User-specific query results based on wallet identity
- Access-controlled message retrieval

#### With Copycat Device (`~copycat@1.0`)
- Authenticated data replication from external sources
- Signed import operations for data integrity
- Identity-based data synchronization

#### With Meta Device (`~meta@1.0`)
- Node configuration for authentication providers
- Access control policy management
- Resource allocation for auth operations

### Cross-Device Authentication Patterns

#### Multi-Provider Support
```text
"on": {
    "request": [
        {
            "device": "auth-hook@1.0",
            "secret-provider": {"device": "cookie@1.0"}
        },
        {
            "device": "auth-hook@1.0",
            "secret-provider": {"device": "http-auth@1.0"},
            "when": {"keys": ["authorization"]}
        }
    ]
}
```

#### Chained Authentication
```text
Cookie Auth → HTTP Auth → Secret Management → Process Interaction
     ↓            ↓              ↓                    ↓
  Session     Enterprise    Wallet Creation    Signed Messages
```

## Security Architecture

### Trust Model
- **Server-Trust:** Users trust HyperBEAM nodes for key management
- **TEE Integration:** Designed for Trusted Execution Environments
- **Cryptographic Auditability:** All operations create verifiable signatures
- **Key Isolation:** Private keys never leave server memory

### Multi-Layer Security
1. **Provider Layer:** Cookie/HTTP authentication
2. **Access Control:** Configurable access-control messages
3. **Controller Verification:** Multi-signature support
4. **Request Signing:** RSA-PSS/HMAC cryptographic proofs

### Security Best Practices
- HTTPS-only deployment
- Secure cookie attributes (HTTPOnly, Secure, SameSite)
- Strong PBKDF2 parameters
- Regular session key rotation
- Comprehensive audit logging

## Performance Considerations

### Optimization Strategies
- **In-Memory Caching:** Active wallet cache for sessions
- **Lazy Loading:** On-demand wallet retrieval from storage
- **Batch Operations:** Parallel message signing
- **Session Reuse:** Authentication state persistence

### Scalability Metrics
- **Memory:** ~2KB per active session (in-memory mode)
- **Computation:** RSA signing ~5ms, HMAC ~0.1ms per operation
- **Throughput:** ~10K concurrent sessions per GB RAM
- **Cookie Limits:** 4KB browser limit for session data

## Implementation Examples

### Basic Cookie Authentication
```text
"on": {
    "request": {
        "device": "auth-hook@1.0",
        "secret-provider": {"device": "cookie@1.0"}
    }
}
```

### Enterprise HTTP Authentication
```text
"on": {
    "request": {
        "device": "auth-hook@1.0",
        "secret-provider": {
            "device": "http-auth@1.0",
            "realm": "Protected Resources",
            "iterations": 2000000
        }
    }
}
```

### Multi-Signature Wallet Management
```text
"secret-provider": {
    "device": "cookie@1.0",
    "access-control": {
        "device": "http-auth@1.0"
    },
    "controllers": ["admin1", "admin2", "admin3"],
    "required-controllers": 2
}
```

## Error Handling & Recovery

### Graceful Degradation
- Skip authentication if provider not found
- Return original request if relevance conditions not met
- Detailed error logging with context preservation
- Partial failure isolation in batch operations

### Monitoring & Debugging
- Event-driven logging across all devices
- Performance metrics for auth operations
- Session tracking and analytics
- Security audit trail maintenance

**Related Documentation:**
- [Device Overview](./hyperbeam-devices.md) - Understanding the device architecture
- [Building Devices](./building-devices.md) - Creating custom devices
- [Core Devices Index](./index.md) - Complete device catalog