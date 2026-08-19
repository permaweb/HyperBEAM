# ACME DNS-01 with DigitalOcean or Cloudflare

This guide configures HyperBEAM to obtain and renew a TLS certificate through
ACME DNS-01. DNS-01 is required for wildcard certificates such as
`*.example.com`.

DigitalOcean and Cloudflare are alternative providers. Use the provider that
hosts the authoritative DNS zone; do not configure both for the same
certificate order.

## Before you begin

You need:

- A domain whose authoritative DNS is managed by DigitalOcean or Cloudflare.
- A public HyperBEAM node reachable on TCP port 443.
- An API token scoped only to the relevant DNS zone.
- An ACME directory, such as Let's Encrypt.

Create DNS routing records independently of the certificate configuration. For
a node at `example.com`, route both names to the node:

- `example.com`
- `*.example.com`

The wildcard DNS record routes per-ID hostnames to HyperBEAM. Adding
`*.example.com` to `tls.domains` makes the certificate valid for those
hostnames. One does not replace the other.

If Cloudflare hosts the zone and HyperBEAM should terminate TLS itself, set the
records to **DNS only**. A proxied record terminates public TLS at Cloudflare
instead of directly at HyperBEAM.

## Common TLS configuration

Start with the following `config.json` structure, replacing the example domain:

```json
{
  "port": 443,
  "protocol": "http2",
  "tls": {
    "domains": [
      "example.com",
      "*.example.com"
    ],
    "acme": {
      "directory-url": "https://acme-v02.api.letsencrypt.org/directory",
      "terms-of-service-agreed": true,
      "dns-provider": "PROVIDER",
      "dns-zone": "example.com",
      "dns-propagation-timeout": 60000,
      "dns-poll-interval": 2000
    }
  }
}
```

Add the API token as `priv-dns-api-token` inside `tls.acme`. The `priv-` prefix
marks it as private so HyperBEAM excludes it from public messages and sanitized
event output. Restrict access to the configuration file. DNS-01 does not use
`tls/acme/http-port`, so that setting can be removed.

**NOTE:** JSON config doesn't support private parameters. You need a `config.flat`
to also be loaded that contains the `priv-dns-api-token`. Example:

```
tls/acme/priv-dns-api-token: TOKEN
```

## DigitalOcean

### 1. Prepare the zone

In the DigitalOcean control panel, ensure `example.com` is a managed domain and
create records equivalent to:

| Type | Hostname | Value |
|------|----------|-------|
| A/AAAA | `@` | The node's public address |
| A/AAAA | `*` | The node's public address |

Use CNAME records instead when appropriate for the deployment.

### 2. Create the API token

Create a DigitalOcean personal access token with only the permissions needed to
create and delete domain records. DigitalOcean supports custom token scopes.
See [Creating a DigitalOcean personal access token](https://docs.digitalocean.com/reference/api/create-personal-access-token/).

The token must be able to call the domain-record create and delete endpoints.
With custom scopes, grant `domain:create` and `domain:delete`.

### 3. Configure HyperBEAM

Replace `PROVIDER` in the common configuration with `digitalocean`:

```json
{
  "dns-provider": "digitalocean",
  "dns-zone": "example.com",
  "dns-propagation-timeout": 60000,
  "dns-poll-interval": 2000
}
```

Update in a separate `config.flat` the API token:

`tls/acme/priv-dns-api-token: replace-with-token`

The token is required.

### 4. Start the node

For a development shell:

```bash
HB_CONFIG=config.json,config-priv.flat rebar3 shell
```

For a release console:

```bash
./bin/hb console
```

HyperBEAM creates a relative DigitalOcean TXT record named
`_acme-challenge`, queries every authoritative nameserver until the expected
value is visible, completes validation, and deletes the record.

## Cloudflare

### 1. Prepare the zone

Add `example.com` to Cloudflare and ensure its assigned nameservers are
authoritative. Create records equivalent to:

| Type | Name | Content | Proxy status |
|------|------|---------|--------------|
| A/AAAA | `@` | The node's public address | DNS only |
| A/AAAA | `*` | The node's public address | DNS only |

DNS-only mode is necessary when clients should inspect the certificate served
by HyperBEAM. Cloudflare-proxied deployments use Cloudflare's public edge
certificate instead.

### 2. Create the API token

In the Cloudflare dashboard, create a token from the **Edit Zone DNS** template
and restrict its resources to the specific zone. Cloudflare documents this in
[Create API token](https://developers.cloudflare.com/fundamentals/api/get-started/create-token/).

Grant:

- `DNS Write` for the selected zone.
- `Zone Read` for the selected zone if HyperBEAM will look up the zone ID.

You can omit `Zone Read` by configuring `dns-zone-id`. The zone ID is available
from the zone overview; see [Find account and zone IDs](https://developers.cloudflare.com/fundamentals/account/find-account-and-zone-ids/).

### 3. Configure HyperBEAM

With automatic zone-ID lookup:

```json
{
  "dns-provider": "cloudflare",
  "dns-zone": "example.com",
  "dns-propagation-timeout": 60000,
  "dns-poll-interval": 2000
}
```

Update in a separate `config.flat` the API token:

`tls/acme/priv-dns-api-token: replace-with-token`

With an explicit zone ID and no `Zone Read` permission:

```json
{
  "dns-provider": "cloudflare",
  "dns-zone": "example.com",
  "dns-zone-id": "023e105f4ecef8ad9ca31a8372d0c353",
  "dns-propagation-timeout": 60000,
  "dns-poll-interval": 2000
}
```

The token is required. HyperBEAM does not read it from the environment.

### 4. Start the node

For a development shell:

```bash
HB_CONFIG=config.json,config-priv rebar3 shell
```

For a release console:

```bash
./bin/hb console
```

HyperBEAM creates the full Cloudflare TXT record
`_acme-challenge.example.com`, queries every authoritative nameserver until the
expected value is visible, completes validation, and deletes the record.

`dns-propagation-timeout` bounds the complete propagation check and defaults to
30000 milliseconds. `dns-poll-interval` defaults to 2000 milliseconds. The old
`dns-propagation-delay` property remains accepted as a compatibility alias for
the timeout when `dns-propagation-timeout` is absent.

## What to expect

At boot, HyperBEAM emits sanitized events on the `tls` topic. The successful
flow includes:

1. `certificate_issuance_started`
2. `dns_challenge` with provider and action only
3. `dns_record_created`
4. `dns_propagation_started`
5. `dns_record_propagated`
6. `dns_record_deleted`
7. `acme_certificate_generation_requested`
8. `acme_certificate_generated`
9. `acme_certificate_downloaded`
10. `acme_certificate_ready`

Renewals additionally emit `acme_certificate_installed` after the live
certificate is replaced. These events exclude API tokens, authorization
headers, TXT values, certificate bytes, wallets, and private keys.

HyperBEAM normally renews before expiration. If issuance or installation fails,
it retains or starts with the wallet-key self-signed fallback and retries later.

## Verify the certificate

Inspect the certificate served for the base hostname:

```bash
openssl s_client -connect example.com:443 -servername example.com </dev/null 2>/dev/null \
  | openssl x509 -noout -subject -issuer -dates -ext subjectAltName
```

The subject alternative names must include both `example.com` and
`*.example.com`.

Test hostname verification for a wildcard hostname:

```bash
curl --head https://tls-test.example.com/
```

The HTTP response may be a redirect or an application error; the important
part is that TLS hostname and chain verification succeed without `-k`.

The TXT record is temporary. It may disappear before a manual `dig` query is
run because HyperBEAM deletes it immediately after ACME validation.

## Troubleshooting

| Symptom or error | Check |
|------------------|-------|
| `acme-dns-api-token-missing` | `HB_CONFIG` includes the `config.flat` file containing `tls/acme/priv-dns-api-token`. |
| Provider returns 401 or 403 | The token is valid, scoped to the correct zone, and has create/delete permission. Cloudflare automatic lookup also needs `Zone Read`. |
| `cloudflare-zone-not-found` | `dns-zone` is the authoritative Cloudflare zone, or configure its exact `dns-zone-id`. |
| `acme-dns-record-outside-zone` | `dns-zone` must contain every configured TLS hostname. For `example.com` and `*.example.com`, use `example.com`. |
| `acme-dns-propagation-timeout` | Increase `dns-propagation-timeout` and confirm every authoritative nameserver serves the expected TXT record. |
| ACME validation times out | Confirm authoritative nameservers and check whether restrictive CAA records block the selected CA. |
| Base hostname works but asset subdomains fail | Configure both the wildcard DNS routing record and `*.example.com` in `tls.domains`. |
| Browser still sees a self-signed certificate | Inspect `acme_bootstrap_failed` or `acme_renewal_failed`, correct the cause, and allow the scheduled retry or restart the node. |
| Cloudflare certificate appears instead of the HyperBEAM certificate | Set the Cloudflare routing records to DNS only. |

## Token security

- Scope the token to one zone and only the required permissions.
- Store it as `tls/acme/priv-dns-api-token` in a protected `config.flat` file.
- Never put it in shell history, logs, public messages, or source control.
- Rotate the token after accidental exposure and restart the node with the new
  value.
