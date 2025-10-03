# Core Devices

HyperBEAM provides a comprehensive suite of devices that handle different aspects of computation, data management, authentication, and system operations. This page provides a complete overview of all available devices organized by their primary function.

## Essential Core Devices

These devices provide the fundamental building blocks for HyperBEAM applications:

### **Process & State Management**

- [**~process@1.0**](./process-at-1-0.md) - Manages persistent, shared computational states and orchestrates device execution
- [**~scheduler@1.0**](./scheduler-at-1-0.md) - Handles ordering and execution of messages within processes
- [**~meta@1.0**](./meta-at-1-0.md) - Configures node settings, hardware specs, and operational parameters

### **Message & Data Handling**

- [**~message@1.0**](./message-at-1-0.md) - Core message state and processing management
- [**~json@1.0**](./json-at-1-0.md) - Provides structured access to JSON data
- [**~relay@1.0**](./relay-at-1-0.md) - Forwards messages between AO nodes and external endpoints

### **Execution Environments**

- [**~wasm64@1.0**](./wasm64-at-1-0.md) - Executes WebAssembly code for high-performance computation
- [**~lua@5.3a**](./lua-at-5-3a.md) - Executes Lua scripts for flexible scripting capabilities

## Device Ecosystems

These comprehensive ecosystems provide advanced functionality through coordinated device interactions:

### **Authentication & Security Ecosystem**
[**Authentication Ecosystem**](./auth-ecosystem-at-1-0.md) - Complete wallet-less authentication system

**Included Devices:**

- **~auth-hook@1.0** - Main authentication interceptor and request signer
- **~secret@1.0** - Wallet generation and secret management with access control
- **~cookie@1.0** - HTTP cookie-based session authentication
- **~http-auth@1.0** - HTTP Basic authentication with PBKDF2 key derivation

**Key Capabilities:**

- Zero-friction blockchain authentication
- Server-side wallet management
- Session persistence across requests
- Enterprise HTTP authentication
- Multi-signature wallet support

### **Data Management Ecosystem**

#### **Data Discovery Engine**
[**Data Discovery Engine**](./data-discovery-at-1-0.md) - Advanced message search and query system

**Primary Device:**

- **~query@1.0** - Flexible message discovery with multiple search modes and return formats

**Key Capabilities:**

- Complex message searching with flexible filtering
- GraphQL query support for advanced data operations
- Multiple return formats (paths, messages, counts, booleans)
- Integration with authentication for access-controlled queries

#### **Data Replication Engine**
[**Data Replication Engine**](./data-replication-at-1-0.md) - External data ingestion and synchronization

**Primary Device:**

- **~copycat@1.0** - Orchestrates data replication from external sources

**Key Capabilities:**

- GraphQL endpoint data replication
- Direct Arweave node integration
- Automatic pagination and batch processing
- Comprehensive error handling and recovery

## Security & TEE Devices

Advanced security features and Trusted Execution Environment support:

- [**~snp@1.0**](./source-code/dev_snp.md) - Secure Network Protocol for TEE operations
- [**dev_codec_httpsig**](./source-code/dev_codec_httpsig.md) - HTTP signature validation and processing

## Payment & Access Control Devices

Metering, billing, and access management:

- [**~p4@1.0**](./source-code/dev_p4.md) - Payment processing and metering system
- [**~faff@1.0**](./source-code/dev_faff.md) - Fine-grained access control and permissions

## Workflow & Utility Devices

Process coordination and system utilities:

- [**dev_cron**](./source-code/dev_cron.md) - Scheduled task execution and automation
- [**dev_stack**](./source-code/dev_stack.md) - Device stack management and coordination
- [**dev_monitor**](./source-code/dev_monitor.md) - System monitoring and health checks

## Storage & Cache Devices

Data persistence and caching infrastructure:

- [**dev_cache**](./source-code/dev_cache.md) - Message caching and retrieval system
- [**hb_store**](./source-code/hb_store.md) - Persistent storage backend management

## Communication & Network Devices

Inter-node communication and network operations:

- [**hb_gateway_client**](./source-code/hb_gateway_client.md) - Gateway communication client
- [**hb_http_client**](./source-code/hb_http_client.md) - HTTP client operations
- [**hb_http_server**](./source-code/hb_http_server.md) - HTTP server management

## Development & Testing Devices

Tools for development, testing, and debugging:

- [**dev_test**](./source-code/dev_test.md) - Testing framework and utilities
- [**hb_debugger**](./source-code/hb_debugger.md) - Debugging tools and inspection
- [**dev_multipass**](./source-code/dev_multipass.md) - Multi-pass processing utilities

## Legacy & Specialized Devices

Specialized functionality and legacy support:

- [**~patch@1.0**](https://cookbook_ao.arweave.net/guides/migrating-to-hyperbeam/exposing-process-state.html) - Direct state updates for process migration
- [**dev_wasi**](./source-code/dev_wasi.md) - WebAssembly System Interface support
- [**dev_poda**](./source-code/dev_poda.md) - Proof of Data Availability validation

## Device Integration Patterns

### **Complete Application Stack**
```text
Authentication → Data Replication → Data Discovery → Process Execution
      ↓                ↓                 ↓              ↓
  Auth Ecosystem   Copycat Device   Query Device   Process Device
```

### **Data Workflow Integration**
1. **Ingestion:** Copycat replicates external data into local cache
2. **Discovery:** Query provides search and filtering over cached data
3. **Authentication:** Auth ecosystem controls access to data operations
4. **Processing:** Process devices utilize data for computation

### **Security Integration**
- Authentication ecosystem provides transparent user authentication
- TEE devices enable secure computation environments
- Access control devices manage permissions and resource usage
- HTTP signature devices ensure message integrity

## Getting Started

### **For Authentication:**
Start with the [Authentication Ecosystem](./auth-ecosystem-at-1-0.md) to enable wallet-less blockchain applications.

### **For Data Management:**
Begin with [Data Replication](./data-replication-at-1-0.md) to import external data, then use [Data Discovery](./data-discovery-at-1-0.md) for search and analysis.

### **For Process Development:**
Review [~process@1.0](./process-at-1-0.md) for state management and [~scheduler@1.0](./scheduler-at-1-0.md) for message ordering.

### **For Custom Devices:**
See [Building Devices](./building-devices.md) for guidance on creating your own devices.

---

**Next Steps:**
- [Building Devices](./building-devices.md) - Learn to create custom devices
- [HyperBEAM Overview](./hyperbeam-devices.md) - Understand the device architecture
- [Source Code Reference](./source-code/) - Detailed technical documentation