# TLS With A Shared Identity

`tls/identity` selects an existing named RSA identity. It does not install a
key, admit a zone member, or make that identity globally trusted. With
`tls/port`, the ordinary HTTP node starts first; HTTPS starts only after the
identity exists and a matching certificate has been validated and saved.
Both listeners use the same AO-Core node and live request configuration.

For a zone, ordinary attested admission installs `zone/<name>`. No zone-device
change is required. A shared TLS key identifies the **zone**, not a unique
physical member. Derive the Arweave address from the certificate's RSA public
key, not the certificate fingerprint: renewal changes the certificate.
TLS alone does not verify the zone's measurement policy.

## Five Members

Start with two members also acting as authoritative DNS-01 responders. All
five use this TLS configuration, replacing the example domains and direct
control origins. These origins must address individual members, not the
geographically steered public service. The machine-readable template is
[member.json](named-identity-tls/member.json); do not run the example domains
unchanged.

```json
{
  "protocol": "http2",
  "port": 8734,
  "tls": {
    "identity": "zone/gateway",
    "port": 443,
    "domains": ["example.net", "*.example.net"],
    "state-path": "/var/lib/lapee/nonvolatile/store/tls-state.etf",
    "peers": [
      "http://node-1.example.net:8734",
      "http://node-2.example.net:8734",
      "http://node-3.example.net:8734",
      "http://node-4.example.net:8734",
      "http://node-5.example.net:8734"
    ],
    "acme": {
      "directory-url": "https://acme-v02.api.letsencrypt.org/directory",
      "terms-of-service-agreed": true,
      "challenge-type": "dns-01",
      "dns-nameserver": "ns1.example.net",
      "dns-nameservers": ["ns1.example.net", "ns2.example.net"],
      "dns-peers": [
        "http://node-1.example.net:8734",
        "http://node-2.example.net:8734"
      ]
    }
  }
}
```

The state file is node-local, mode 0600, outside the AO cache. It retains the
ACME account key, public chain and retry deadline, never the TLS identity key.
Its parent must already exist on protected persistent storage; the TLS device
does not provision or mount it. On Linux PermawebOS, the example `store/`
directory is created inside the zone's encrypted nonvolatile mount. Verify
successful activation before deployment; do not precreate that path on the
unmounted filesystem. Each member must keep its own state file across restart.
Do not delete it to bypass a retry deadline. Atomic replacement does not provide
anti-rollback protection against restoring an older volume snapshot.

HTTP control origins are supported intentionally: certificates are independently
CA/key/domain/time validated, and DNS publications are signed by the zone key.
This does not make the entire HTTP listener confidential or safe to expose.
Preserve the appliance's admission and API policy; expose only the required
routes. A network intermediary can still deny service. HTTPS origins are also
supported and use normal system CA verification; do not depend on an unissued
HTTPS certificate to bootstrap the DNS authorities.

## DNS Authorities

On members 1 and 2, configure the existing DNS device. The combined measured
configuration fragment is [dns-authority.json](named-identity-tls/dns-authority.json):

```json
{
  "dns": {"address": "0.0.0.0", "port": 53},
  "on": {
    "dns-resolve": {"device": "tls@1.0"}
  }
}
```

Append `{"device":"dns@1.0"}` to the effective `on/start` list. Preserve
every existing hook. In Linux PermawebOS, `/etc/lapee/lapee.json` is loaded
after the operator configuration and its startup list wins. Its measured
startup configuration on those two members must therefore contain:

```json
{
  "on": {
    "start": [
      {
        "device": "measurement@1.0",
        "path": "boot",
        "method": "POST",
        "target": "body",
        "measurement-body-source": "hook-body",
        "hook": {"result": "ignore"}
      },
      {
        "device": "zone@1.0",
        "path": "start",
        "method": "POST",
        "target": "body",
        "hook": {"result": "ignore"}
      },
      {"device": "dns@1.0"}
    ]
  }
}
```

Members 3-5 retain their existing measurement/zone hooks and need no DNS
listener. Include both measured configurations in the operator-approved zone
admission policy. Never weaken measurement checks just to join an image.

In the parent DNS zone, delegate `_acme-challenge.example.net` with NS records
for `ns1.example.net` and `ns2.example.net`, whose DNS-only A/AAAA records point
to these two authorities. Both must be reachable over UDP **and** TCP port 53.
The apex and wildcard use the same challenge name; multiple tokens coexist.
Delegate every other configured domain's challenge name too. The resolver
refuses unrelated names and is not a general recursive DNS service.

Every issuing member publishes to **all** `dns-peers` before asking the CA to
validate. Missing authorities cause issuance to fail; it never silently issues
with inconsistent DNS. Successful and failed validations remove their tokens;
abandoned signed records expire within five minutes. Keep the two authorities
running and zone-admitted throughout bootstrap and renewal. A failed initial
attempt is retried under the same durable backoff as a failed renewal.
Keep member clocks accurate: publications use a four-minute lifetime inside
the five-minute acceptance window, allowing a minute of skew, not arbitrary
clock disagreement.

DNS-only steering of the public apex/wildcard can select any healthy HTTPS
member. Do not terminate TLS at a CDN, host proxy or external load balancer
if the browser-to-zone connection is the requirement. Ordinary layer-4 port
forwarding does not terminate TLS. Do not publish AAAA records without a working
IPv6 path to the TEE listener. DNS delegation and traffic changes require their
own deployment approval; these files do not apply them.

## Admission And Renewal

Use existing zone initialization with the approved measured-image templates
on the first member. On joining PermawebOS members, the relevant configuration
fragment is [joiner.json](named-identity-tls/joiner.json):

```json
{
  "public-url": "http://node-2.example.net:8734",
  "zone-allow": {"gateway": "http://node-1.example.net:8734"},
  "zone-init-allow": false
}
```

Change `public-url` per member. Do not use an unrestricted admission policy or
initialize a fresh zone on every machine. Keep at least one admitted keeper
available when restarting members; TLS state does not recreate a lost zone key.

The lifecycle checks peers at startup and at least every six hours, more often
near expiry. It accepts only locally trusted chains with the exact domain set,
matching key, current validity and server key usage. It keeps whichever valid
chain has the later signed expiry. Equal-expiry chains do not cause churn.
There is no globally authoritative "latest certificate" endpoint.

Issuance is due in the final third of the signed lifetime, capped at 30 days
before expiry. Before issuing, a member waits a random 0-300 seconds and checks
peers again (`tls/renewal-jitter` changes that maximum, in seconds). One operation
runs per member. The account and one-hour retry gate are saved before any ACME
request; failures honor a longer issuer `Retry-After`. Public certificate reads
and repeated public `obtain` requests cannot trigger issuance.
Without a certificate, peer checks continue every 30 seconds even during that
issuance backoff, so a recovered peer can supply a certificate promptly.

Members can temporarily serve different, still-valid certificates for the same
key. Renewal replaces the live certificate without restarting AO-Core or
closing established connections. Unavailable peers do not remove a valid local
certificate. Removing the named identity closes its added HTTPS listener, not
the ordinary AO-Core listener. TLS configuration changes require restarting the
member; ordinary identity installation is detected live.

Jitter is **not** a distributed lock. Simultaneous starts or partitions can
produce multiple legitimate orders. Five is the initial fleet size, not a
promise of exactly one order globally. Let's Encrypt currently permits five
certificates for an exact identifier set per seven days across accounts; local
backoff cannot guarantee a global quota. This client does not implement ACME
Renewal Information (ARI). See the issuer's
[rate limits](https://letsencrypt.org/docs/rate-limits/).

## Deployment Gates

1. Build the TLS change into the measured firmware; retain the measurement and
   zone devices and include both authority/member configurations in the zone
   policy. Start AO-Core, admit all members, and verify encrypted storage.
2. Verify public DNS delegation, both DNS transports, exact peer origins and
   the effective merged startup hooks before production issuance.
3. Exercise the real deployment with a test zone and the
   [staging CA](https://letsencrypt.org/docs/staging-environment/). Set its
   directory URL, supply its public root PEM through `tls/ca-certificates`, and
   use a separate state file. `acme/ca-certificate` instead controls trust in
   the ACME HTTPS API; these are distinct trust decisions. Never add staging
   roots to a production browser or production TLS configuration.
4. For production use the production directory and system CA trust, without
   test CA overrides. Keep production state separate from staging state.
   Confirm each direct node's HTTPS certificate, SANs, zone public key and
   actual application responses before adding it to DNS steering.
5. Verify restart/rejoin and renewal on the actual images, including a held
   connection and one unavailable peer. Monitor certificate expiry, TLS
   lifecycle errors and public health externally. There is no automatic DNS
   promotion or rollback in this change.

## Reproducing The Network Test

Run Pebble on loopback with a 120-second certificate validity, DNS resolver
`127.0.0.1:18653`, no forced validation sleep, and authorization reuse disabled.
The test starts five actual HyperBEAM nodes and two DNS authorities on 18653
and 18654. It performs real ACME issuance and timer-driven renewal; it does not
mock the issuer, HTTP transport or DNS.

```sh
HB_TLS_PEER_PEBBLE=https://localhost:14600/dir \
HB_PEBBLE_CA=/absolute/path/to/pebble.minica.pem \
HB_PEBBLE_ISSUER_CA=/absolute/path/to/pebble-issuer-root.pem \
rebar3 device test --devices dev_tls --with-core \
  --module hb_tls_peer_tests --timeout 300

rebar3 eunit-all
```

Without the Pebble environment, the local five-member, validation, monotonicity
and restart tests still run, but real ACME issuance/renewal is not exercised.
Local integration evidence does not establish a deployed TEE measurement,
public DNS reachability, production issuance or browser attestation UX.
