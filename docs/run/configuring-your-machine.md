# Configuring Your HyperBEAM Node

This guide details the various ways to configure your HyperBEAM node's behavior, including ports, storage, keys, and logging.

## Configuration (`config.json`)

The primary way to configure your HyperBEAM node is through a `config.json` file located in the node's working directory or specified by the `HB_CONFIG` environment variable.

Configuration is an AO-Core message whose values may include linked data.

### Flat config file

Another possibility is to use `config.flat` that uses a simple `Key: Value` format.

**Example `config.flat`:**

```
port: 8080

priv_key_location: /path/to/your/wallet.json

default_store/lmdb/ao-types: atom=("store-module")
default_store/lmdb/store-module: hb_store_lmdb
default_store/lmdb/name: /tmp/store

store/ao-types: list=(".")
store/1/ao-types: atom=("store-module")
store/1/store-module: hb_store_lmdb
store/1/name: /tmp/store

store/2/ao-types: atom=("store-module")
store/2/store-module: hb_store_s3
store/2/bucket: hb-s3
store/2/priv_access_key_id: minioadmin
store/2/priv_secret_access_key: minioadmin
store/2/endpoint: http://localhost:9000
store/2/force_path_style: true
store/2/region: us-east-1
```

Below is a reference of commonly used configuration keys. `config.flat` supports
atoms, strings, integers, booleans, nested messages (forward slashes in keys)
and lists (numbered message keys and an `ao-types` annotation).

### Core Configuration

These options control fundamental HyperBEAM behavior.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `port` | Integer | 8734 | HTTP API port |
| `hb_config_location` | String | "config.flat" | Path to configuration file |
| `priv_key_location` | String | "hyperbeam-key.json" | Path to operator wallet key file |
| `mode` | Atom | debug | Execution mode (debug, prod) |

### Server & Network Configuration

These options control networking behavior and HTTP settings.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `host` | String | "localhost" | Choice of remote node for non-local tasks |
| `gateway` | String | "https://arweave.net" | Default gateway |
| `bundler_ans104` | String | "https://up.arweave.net:443" | Location of ANS-104 bundler |
| `protocol` | Atom | http2 | Protocol for HTTP requests (http1, http2, http3) |
| `http_client` | Atom | gun | HTTP client to use (gun, httpc) |
| `http_connect_timeout` | Integer | 5000 | HTTP connection timeout in milliseconds |
| `http_keepalive` | Integer | 120000 | HTTP keepalive time in milliseconds |
| `http_request_send_timeout` | Integer | 60000 | HTTP request send timeout in milliseconds |
| `relay_http_client` | Atom | httpc | HTTP client for the relay device |

### Security & Identity

These options control identity and security settings.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `location_ttl` | Integer | 2419200000 | TTL of the node's location records (28 days in ms) |

#### TLS termination

TLS is opt-in and terminates inside the BEAM using the node's RSA `priv-wallet`.
Supply a certificate issued for that key, or use ACME to obtain and renew one
automatically. ACME is an issuance protocol, not a certificate authority;
Let's Encrypt is the issuer used in the examples below.

##### Supplied certificates and signing requests

`GET /~tls@1.0/csr` returns a PEM-encoded PKCS#10 certificate signing request
for the node wallet. It uses `tls/domains` by default, or a `domains` list in
the request. The private key never leaves the node.

To obtain the first certificate, start an ordinary HTTP node without `tls`
configured, then fetch its CSR locally, supplying the required names:

```sh
curl --fail --get \
  --data-urlencode 'domains+list=\"node.example.com\", \"*.node.example.com\"' \
  http://127.0.0.1:8734/~tls@1.0/csr \
  --output node.csr.pem
```

The escaped quotes preserve string values in the AO-Core list.

Submit this CSR to your chosen issuer and complete its domain validation.
Save the issued certificate and intermediate certificates in one PEM file,
leaf first. Keep the same node wallet, then restart with:

```json
{
  "ao-types": "atom=(\"protocol\")",
  "port": 443,
  "protocol": "http2",
  "tls": {
    "domains": ["node.example.com", "*.node.example.com"],
    "certificate-path": "node.fullchain.pem"
  }
}
```

`certificate-path` is relative to the node's working directory, or an absolute
path. It takes precedence over `tls/acme`. This mode starts no ACME runtime,
challenge listener or automatic renewal; replace the PEM file and restart to
adopt a renewed certificate. `domains` is optional when loading a file, but
provides the defaults for subsequent CSR requests.

The certificate must carry the exact public key of `priv-wallet`; a different
key or an unreadable/malformed PEM file fails startup. There is no separate
TLS private-key file and no fallback to plaintext. If the node generates a new
wallet on every boot, it needs a certificate issued for each new key.

##### Automatic issuance with ACME

`tls/acme/directory-url` selects the issuer's ACME endpoint. The client supports
HTTP-01 (the default) and [DNS-01](#dns-01-and-wildcard-certificates), including
wildcard names with DNS-01. Issuers requiring External Account Binding are not
yet supported.

Save this as `config.json`, replacing `node.example.com` with your domain:

```json
{
  "ao-types": "atom=(\"protocol\")",
  "port": 443,
  "protocol": "http2",
  "tls": {
    "domains": ["node.example.com"],
    "acme": {
      "directory-url": "https://acme-v02.api.letsencrypt.org/directory",
      "terms-of-service-agreed": true,
      "http-port": 80
    }
  }
}
```

Before starting:

- Point the domain's DNS records (`A`, and `AAAA` if present) to this node.
  Do not put a TLS-terminating proxy in front of it if the browser must see
  the node's wallet key.
- Make TCP ports 80 and 443 reachable from the internet and available to
  HyperBEAM. Port 80 must remain reachable for
  [HTTP-01 validation and renewal](https://letsencrypt.org/docs/challenge-types/#http-01-challenge);
  it serves ACME challenges only, not application requests or HTTPS redirects.
- Give the BEAM process permission to bind ports 80 and 443. Set
  `terms-of-service-agreed` to `true` only if you accept the CA's terms.

Start from the directory containing the config and node wallet:

```sh
HB_CONFIG=config.json rebar3 shell
```

The node uses `hyperbeam-key.json` by default; set `HB_KEY` to use a different
node wallet. It must be an RSA wallet. No separate TLS private key, certificate
files, or request hooks need to be configured.

Each startup obtains a certificate before opening the HTTPS listener. If initial
issuance fails, startup fails; there is no self-signed fallback. For repeated
setup tests, use the
[Let's Encrypt staging directory](https://letsencrypt.org/docs/staging-environment/):
`https://acme-staging-v02.api.letsencrypt.org/directory`. Staging certificates are
not browser-trusted. Use the production URL above for normal service.

For `config.flat`, use the equivalent configuration below and start with
`HB_CONFIG=config.flat rebar3 shell`:

```text
ao-types: atom=("protocol")
port: 443
protocol: http2

tls/domains/ao-types: list=(".")
tls/domains/1: node.example.com

tls/acme/ao-types: atom=("terms-of-service-agreed"), integer=("http-port")
tls/acme/directory-url: https://acme-v02.api.letsencrypt.org/directory
tls/acme/terms-of-service-agreed: true
tls/acme/http-port: 80
```

`tls/acme/http-port` defaults to `80` and must be reachable for HTTP-01
validation. This cleartext listener serves only the exact ACME challenge path.
The ACME directory uses the operating-system trust store unless
`tls/acme/ca-certificate` supplies a PEM CA certificate. The TLS listener
prefers HTTP/2 (`h2`) through ALPN, with HTTP/1.1 fallback. The `ao-types`
annotation makes `protocol` an atom; explicitly choosing `http2` also avoids
selecting HTTP/3 in an HTTP/3-enabled build. HTTP/3 is not supported by the
wallet-key TLS adapter.

The certificate leaf contains the exact `priv-wallet` public key. Certificate
viewers expose a fingerprint of that TLS key; the node address instead hashes
the wallet's raw RSA modulus.

The `tls` node-message field cannot be changed while the listener is running;
restart the node to adopt a different TLS policy.

##### DNS-01 and wildcard certificates

DNS-01 serves temporary TXT records through `dns@1.0`, with `tls@1.0` as its
resolver. No DNS-provider API credentials or separate certificate files are
needed. The certificate still uses the node's wallet key.

Merge these settings into your node configuration, replacing the example names:

```json
{
  "ao-types": "atom=(\"protocol\")",
  "port": 443,
  "protocol": "http2",
  "dns": {
    "port": 53,
    "address": "0.0.0.0"
  },
  "tls": {
    "domains": ["example.com", "*.example.com"],
    "acme": {
      "directory-url": "https://acme-v02.api.letsencrypt.org/directory",
      "terms-of-service-agreed": true,
      "challenge-type": "dns-01",
      "dns-nameserver": "ns-acme.example.com"
    }
  },
  "on": {
    "start": { "device": "dns@1.0" },
    "dns-resolve": { "device": "tls@1.0" }
  }
}
```

The `on` entries above are additions, not a replacement for your existing hook
message. Preserve the node's other handlers, and append DNS startup to any
existing `on/start` list. An explicit `on` message replaces the defaults; if your
configuration does not already include it, retain the handlers from
`hb_opts:default_message/0` for normal request routing, authentication, rate
limiting and cache indexing.

At your existing DNS provider, add:

| Name | Type | Value |
| --- | --- | --- |
| `ns-acme.example.com` | `A` | The DNS listener's public IPv4 address |
| `_acme-challenge.example.com` | `NS` | `ns-acme.example.com.` |

This delegates only the challenge zone. Keep the normal domain's nameservers
and application records unchanged. Do not put a CNAME at the delegated name.
For additional domain names, delegate their corresponding `_acme-challenge`
names too. The apex and its wildcard share one challenge zone. The certificate
needs both names to cover both `example.com` and `anything.example.com`.

Make **UDP and TCP port 53** on the nameserver address reachable from the internet.
You can forward both to a different local `dns/port`; an NS record cannot specify
a port. Give the BEAM permission to bind privileged ports if using them directly.
Before requesting a certificate, allow the NS/address changes and any old negative
DNS answers to expire. Add an AAAA record only if the listener is also reachable
over that IPv6 address.

Start with `HB_CONFIG=config.json rebar3 shell`. The startup hook brings DNS up
before ACME validation. The TLS resolver answers TXT, NS and SOA queries for its
configured challenge zones, refuses unrelated names, and removes challenge
values after validation. Challenge answers and negative answers have zero TTL.
Keep DNS reachable for automatic renewal. Use the staging ACME directory during
setup, as with HTTP-01.

DNS-01 does not open the HTTP challenge listener and does not require port 80.
HTTPS clients still need a route to the node's TLS listener. If several nodes
share public port 443, use TLS passthrough (for example, SNI-based routing) rather
than TLS termination at the router, so the browser sees the node's own key.

The local ACME example in `src/core/test/hb_tls_examples.erl` supports DNS-01 by
setting `HB_PEBBLE_DNS_PORT` to the port used by Pebble's `-dnsserver` resolver.
It exercises apex and wildcard issuance, renewal, HTTP/2 and DNS cleanup over
both UDP and TCP. The normal example without that variable exercises HTTP-01.

### Caching & Storage

These options control caching behavior. The `store` option accepts nested messages
and lists in `config.flat`.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `cache_lookup_hueristics` | Boolean | false | Whether to use caching heuristics or always consult the local data store |
| `access_remote_cache_for_client` | Boolean | false | Whether to access data from remote caches for client requests |
| `store_all_signed` | Boolean | true | Whether the node should store all signed messages |
| `await_inprogress` | Atom/Boolean | named | Whether to await in-progress executions (false, named, true) |

### Execution & Processing

These options control how HyperBEAM executes messages and processes.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `scheduling_mode` | Atom | local_confirmation | When to inform recipients about scheduled assignments (aggressive, local_confirmation, remote_confirmation) |
| `compute_mode` | Atom | lazy | Whether to execute more messages after returning a result (aggressive, lazy) |
| `process_workers` | Boolean | true | Whether the node should use persistent processes |
| `client_error_strategy` | Atom | throw | What to do if a client error occurs |
| `wasm_allow_aot` | Boolean | false | Allow ahead-of-time compilation for WASM |

### Device Management

These options control how HyperBEAM manages devices.

Remote device loading is enabled by configuring `trusted_device_signers`;
omit it or set it to `[]` to disable remote signer lookup.

### Debug & Development

These options control debugging and development features.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `debug_stack_depth` | Integer | 40 | Maximum stack depth for debug printing |
| `debug_print_map_line_threshold` | Integer | 30 | Maximum lines for message printing |
| `debug_print_binary_max` | Integer | 60 | Maximum binary size for debug printing |
| `debug_print_indent` | Integer | 2 | Indentation for debug printing |
| `debug_print_trace` | Atom | short | Trace mode (short, false) |
| `short_trace_len` | Integer | 5 | Length of short traces |
| `debug_hide_metadata` | Boolean | true | Whether to hide metadata in debug output |
| `debug_ids` | Boolean | false | Whether to print IDs in debug output |
| `debug_hide_priv` | Boolean | true | Whether to hide private data in debug output |

**Note:** For the complete and most up-to-date list, refer to the
`default_message/0` function in the `hb_opts` module source code.

## Overrides (Environment Variables & Args)

You can override settings from `config.flat` or provide values if the file is missing using environment variables or command-line arguments.

**Using Environment Variables:**

Environment variables typically use an `HB_` prefix followed by the configuration key in uppercase.

*   **`HB_PORT=<port_number>`:** Overrides `hb_port`.
    *   Example: `HB_PORT=8080 rebar3 shell`
*   **`HB_KEY=<path/to/wallet.key>`:** Overrides `hb_key`.
    *   Example: `HB_KEY=~/.keys/arweave_key.json rebar3 shell`
*   **`HB_STORE=<directory_path>`:** Overrides `hb_store`.
    *   Example: `HB_STORE=./node_data_1 rebar3 shell`
*   **`HB_PRINT=<setting>`:** Overrides `hb_print`. `<setting>` can be `true` (or `1`), or a comma-separated list of modules/topics (e.g., `hb_path,hb_ao,ao_result`).
    *   Example: `HB_PRINT=hb_http,dev_router rebar3 shell`
*   **`HB_CONFIG_LOCATION=<path/to/config.flat>`:** Specifies a custom location for the configuration file.

**Using `erl_opts` (Direct Erlang VM Arguments):**

You can also pass arguments directly to the Erlang VM using the `-<key> <value>` format within `erl_opts`. This is generally less common for application configuration than `config.flat` or environment variables.

```bash
rebar3 shell --erl_opts "-hb_port 8080 -hb_key path/to/key.json"
```

**Order of Precedence:**

1.  Command-line arguments (`erl_opts`).
2.  Settings in `config.flat`.
3.  Environment variables (`HB_*`).
4.  Default values from `hb_opts.erl`.

## Configuration in Releases

When running a release build (see [Running a HyperBEAM Node](./running-a-hyperbeam-node.md)), configuration works similarly:

1.  Supply the node's configuration at deployment time. For example, start the release with `HB_CONFIG=/path/to/config.json bin/hb foreground`.
2.  Local configuration files are not copied into the release during the build. JSON and flat configuration files remain supported by `HB_CONFIG`.
