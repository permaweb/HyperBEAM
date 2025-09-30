# Device: Authentication Ecosystem (~auth-hook@1.0, ~secret@1.0, ~cookie@1.0, ~http-auth@1.0)

## Overview

The Authentication Ecosystem provides wallet-less blockchain applications through server-side key management. The ecosystem consists of `~auth-hook@1.0` (authentication interceptor), `~secret@1.0` (wallet storage), `~cookie@1.0` (session management), and `~http-auth@1.0` (HTTP Basic authentication).

## Core Concept: Zero-Friction Authentication

Users interact with decentralized applications like traditional web apps while HyperBEAM handles cryptographic operations server-side. The `&!` parameter triggers automatic request signing with node-hosted wallets.

## Primary Device: ~auth-hook@1.0

Intercepts HTTP requests containing the `&!` parameter and processes them through a configured secret provider.

### Key Functions

*   **`request`**
    *   **Action:** Detects `&!` pattern, validates configuration, generates/retrieves secrets, creates wallets via `~secret@1.0`, signs requests, and applies provider post-processing.
    *   **Configuration:**
        ```
        "secret-provider": {
            "device": "cookie@1.0",
            "generate-path": "generate",
            "finalize-path": "finalize"
        }
        ```
    *   **Activation Conditions:**
        ```
        "when": {
            "committers": "uncommitted" | "always" | ["address1"],
            "keys": "always" | ["authorization"]
        }
        ```

## Supporting Devices

### ~secret@1.0

Manages wallet creation, storage, and signing operations.

*   **`generate`**: Creates wallets with persistence modes (`client`, `in-memory`, `non-volatile`)
*   **`commit`**: Signs messages with access-control and multi-signature support
*   **`export` / `sync`**: Manages wallet portability and cross-node synchronization

### ~cookie@1.0

Implements HTTP cookie-based session management with HMAC-SHA256 commitments.

*   **`generate`**: Creates/retrieves secrets from cookies
*   **`finalize`**: Adds Set-Cookie headers to responses
*   **Security**: HTTPOnly, Secure, SameSite attributes; `wallet-<address>` patterns

### ~http-auth@1.0

Provides HTTP Basic authentication with PBKDF2 key derivation (1,200,000 iterations, SHA256).

*   **`generate`**: Processes Authorization headers and derives signing keys
*   **Flow**: Returns 401 with WWW-Authenticate when credentials missing, otherwise derives key and signs

!!! note "Performance"
    PBKDF2 performs ~5-10 derivations/second for brute-force protection.

## Device Integration

The authentication workflow: HTTP request with `&!` → auth-hook intercepts → provider generates secret → secret device signs → authenticated response returned.

**Integration with Core Devices:**

* **`~process@1.0`**: Automatic signing of process communications
* **`~query@1.0`**: Authenticated data discovery
* **`~copycat@1.0`**: Authenticated data replication
* **`~meta@1.0`**: Authentication provider configuration

**Multi-Provider Configuration:**

```
"on": {
    "request": [
        {"device": "auth-hook@1.0", "secret-provider": {"device": "cookie@1.0"}},
        {"device": "auth-hook@1.0", "secret-provider": {"device": "http-auth@1.0"}}
    ]
}
```

## Security Considerations


**Security Layers:** Provider authentication (Cookie/HTTP) → Access control messages → Controller verification → Request signing (RSA-PSS/HMAC)

**Best Practices:** HTTPS-only, secure cookie attributes, strong PBKDF2 parameters, session key rotation, audit logging

!!! warning "Trust Model"
    Intended for deployment in Trusted Execution Environments (TEE) with `~snp@1.0` or trusted nodes. Private keys never leave server memory. All operations create cryptographically auditable signatures.

## Configuration Examples

**Cookie Authentication:**
```
"on": {"request": {"device": "auth-hook@1.0", "secret-provider": {"device": "cookie@1.0"}}}
```

**HTTP Basic Authentication:**
```
"secret-provider": {"device": "http-auth@1.0", "realm": "Protected", "iterations": 2000000}
```

**Multi-Signature:**
```
"secret-provider": {
    "device": "cookie@1.0",
    "controllers": ["admin1", "admin2"],
    "required-controllers": 2
}
```

## Practical Implications

**For Application Developers:**

This ecosystem eliminates blockchain onboarding friction. Users access your dApp without installing wallets or managing keys—authentication happens transparently via cookies or HTTP Basic Auth. Your application simply adds `&!` to requests that need signing.

**For Node Operators:**

You control the trust model. Deploy in TEE for trustless environments or trusted nodes for enterprise scenarios. Cookie-based auth suits consumer applications, while HTTP Basic Auth serves API integrations and enterprise SSO systems. Multi-signature configurations enable shared custody models.

**For End Users:**

Single-click access to blockchain applications with traditional web experience. No seed phrases to manage, no browser extensions to install. Sessions persist across devices when using `non-volatile` storage mode. Authentication state syncs via cookies or HTTP headers.

## See Also

- [`~process@1.0`](../foundational/process-at-1-0.md) - Process communication signing
- [`~query@1.0`](./data-discovery-at-1-0.md) - Authenticated data queries
- [`~snp@1.0`](./source-code/dev_snp.md) - Trusted execution environment