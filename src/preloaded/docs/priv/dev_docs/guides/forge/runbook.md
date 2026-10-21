# Device Forge Runbook

The Device Forge is how a `dev_*` Erlang module becomes a loadable HyperBEAM device. The runtime loads generated `_hb_device_*` modules from signed implementation messages; it does not load raw source modules.

Inspect workflow examples show request, configuration, or operator command shapes. Run them only after providing the stated local prerequisites; the browser runner does not execute these blocks.

## What The Forge Produces

For each root such as `dev_echo_lens`, the Forge packages the root and helper modules into:

- A deterministic BEAM archive under `_build/device-packages/`.
- A device specification message derived from the root module docs.
- A device implementation message containing the archive and metadata.
- A local preloaded store for tests and `rebar3 device local`.
- Optional published Arweave messages when you run `rebar3 device publish`.

## Install The Template

From a HyperBEAM checkout:

```text
cd /tmp/hyperbeam-source
./install-template --local /tmp/hyperbeam-source
```

For a project pinned to upstream edge instead of a local checkout:

```text
./install-template --branch edge
```

Create a new device project:

```text
mkdir -p /tmp/hb-device-docs-forge
cd /tmp/hb-device-docs-forge
rebar3 new device name=echo_lens
cd echo_lens
```

## Write The Device

Replace `src/dev_echo_lens.erl` with this complete device:

```erlang
-module(dev_echo_lens).
-export([info/1, echo/3, upper/3]).
-include_lib("eunit/include/eunit.hrl").

info(_) ->
    #{
        exports => [<<"echo">>, <<"upper">>]
    }.

echo(_Base, Req, Opts) ->
    {ok, hb_maps:get(<<"input">>, Req, <<>>, Opts)}.

upper(_Base, Req, Opts) ->
    Input = hb_maps:get(<<"input">>, Req, <<>>, Opts),
    Upper = string:uppercase(unicode:characters_to_list(Input)),
    {ok, unicode:characters_to_binary(Upper)}.

echo_test() ->
    ?assertEqual(
        {ok, <<"hello">>},
        echo(#{}, #{ <<"input">> => <<"hello">> }, #{})
    ).

upper_test() ->
    ?assertEqual(
        {ok, <<"HELLO">>},
        upper(#{}, #{ <<"input">> => <<"hello">> }, #{})
    ).
```

The public device name is derived from the root module name: `dev_echo_lens` implements `~echo-lens@1.0`.

## Package, Verify, And Test

```text
rebar3 device package
rebar3 device verify
rebar3 device test
```

What each command checks:

| Command | Result |
|---|---|
| `rebar3 device package` | Writes `_hb_device_echo_lens_1_0_<hash>.beam-archive.zip`. |
| `rebar3 device verify` | Confirms the archive loads as generated `_hb_device_*` modules and helpers are not loaded under raw names. |
| `rebar3 device test` | Builds a fresh preloaded store containing core devices plus your device, then runs EUnit. |

A local run of this exact flow passed with two EUnit tests.

## Run A Local Node With The Device

Use a different port if your normal HyperBEAM node already uses `8734`:

```text
cat > device-test-8799.json <<'JSON'
{
  "port": 8799
}
JSON

HB_CONFIG=device-test-8799.json rebar3 device local
```

In another shell:

```text
curl -sS "http://localhost:8799/~echo-lens@1.0/echo?input=hello"
curl -sS "http://localhost:8799/~echo-lens@1.0/upper?input=hello"
```

Expected output:

```text
hello
HELLO
```

## Publish

Publishing signs and uploads the spec and implementation messages using your wallet keyfile:

```text
rebar3 device publish --key wallet.json
```

The command prints the spec and implementation IDs. Keep both: operators can trust a signer, pin an implementation, or resolve by a published name that maps to the spec.

## Load Through Trust Policy

A node can load non-core devices by direct pin, trusted signer, or a local preloaded store.

| Node key | Role |
|---|---|
| `preloaded-store` | Local store containing packaged device specs and implementations. |
| `preloaded-devices-index` | Resolver message mapping names to specs. |
| `trusted-devices` | Direct map from a device name or spec ID to an implementation ID. |
| `trusted-device-signers` | Addresses whose implementation signatures the operator accepts. |
| `load-remote-devices` | Enables fetching unmatched devices from the configured gateway. |
| `admissible-devices` | Optional per-execution allowlist, used by sandboxed execution. |

Inspect the current policy on the node you operate. For device loading, the important keys are `preloaded-store`, `preloaded-devices-index`, `trusted-devices`, `trusted-device-signers`, `load-remote-devices`, and `admissible-devices`.

## Operator Pattern

Use a local preloaded store while developing. Use direct pins for production devices that must not move. Use trusted signers when you want a signer to be able to publish upgrades without every operator editing a pin.

The trust decision belongs to the node operator. A device package may be content-addressed and signed, but loading it into a live node still grants executable behavior.
