# Spectrum beta deployment

This runbook deploys an `~arweave-scheduler@1.0` `all` process whose
execution stack is:

```text
process@1.0
  scheduler: arweave-scheduler@1.0 (all)
  execution: spectrum@1.0
  pricing:   probability-time@1.0
  model:     markov@1.0
```

The launch is immutable. Prepare and verify the namespace artifact before
signing the process transaction; a correction requires a replacement process
and resolver update.

## Finalize the launch state

Record these values in the release log. Every runtime parameter below is read
from process state.

| Key | Beta value | Unit |
| --- | --- | --- |
| `initial-namespace` | prepared artifact ID | Arweave ID |
| `initial-nametime` | `52560000` | blocks (200 × 262800) |
| `spectrum-height` | chain tip used when signing | block height |
| `grace-factor` | governance choice | basis points of bought time |
| `target-occupancy` | governance choice | probability mass, `0 < t < 1` |
| `price-at-target` | governance choice | winston per probability-block at `t` |
| `pricing-device` | `probability-time@1.0` | device |
| `probability-device` | `markov@1.0` | device |

Omit `weighting-device` to use exact-name probability directly. The Markov
`order` is retained in the prepared model; use order 4 for this beta.
`include-end=true` is part of `~probability-time@1.0`, not a launch setting.

If the intended reference price is `K` AR per probability-year, convert it to
the process value with:

```text
price-at-target = ceil(K * 10^12 / 262800)
```

## Prepare the namespace

Use `fQXYPE9MAcfI1wV2CwJ3sJIhgT9btBOlYFOKFDGhAs0` as the source snapshot,
then review the resulting `name -> resolver target` table. That manifest has
16,621 paths: 11,270 contain `_` and are undernames, while 5,348 are direct
lowercase `[a-z0-9-]+` roots. Do not issue undernames independently. Preserve
them by retaining each root's existing reference tree as that root's value.
Three direct paths use scientific-notation punctuation and are outside the
beta alphabet; exclude them unless the launch policy explicitly changes.

Fetch and decode the exact source transaction rather than a gateway-rendered
route:

```bash
SOURCE_ID=fQXYPE9MAcfI1wV2CwJ3sJIhgT9btBOlYFOKFDGhAs0
curl -fsS "https://arweave.net/tx/$SOURCE_ID/data" |
python3 -c '
import base64, sys
data = sys.stdin.buffer.read().strip()
print(base64.urlsafe_b64decode(data + b"=" * (-len(data) % 4)).decode())
' \
  > manifest.json
jq -e '.manifest == "arweave/paths" and .version == "0.2.0"' manifest.json
jq -r '.paths | to_entries[] | [.key, .value.id] | @tsv' manifest.json \
  > manifest-paths.tsv
```

Normalize to lowercase, reject names outside `[a-z0-9-]+`, and fail on any
normalization collision in the root target map. Do not silently choose between
conflicting targets.

The training corpus and issued roots are deliberately different. Train on the
component before the first `_` from every manifest path, retaining duplicate
observations; issue only the reviewed roots. For this snapshot, filtering that
training corpus to `[a-z0-9-]+` leaves 16,614 samples and 14,639 distinct
strings. Record both final counts.

The prepared artifact is an AO structured message, not JSON:

```text
model: <trained markov@1.0 model>
names:
  <name>:
    value: <existing resolver target>
    pricing:
      weight: <likelihood(name, include-end=true)>
```

Build it through the packaged devices. The following is the normative Erlang
flow; `TrainingNames` is the filtered observation list, `Targets` is the
reviewed root map, and `Opts` names the complete preloaded store:

```erlang
Names = lists:sort(maps:keys(Targets)),
{ok, Trained} = hb_ao:resolve(
    #{ <<"device">> => <<"markov@1.0">> },
    #{
        <<"path">> => <<"train">>,
        <<"body">> => TrainingNames,
        <<"order">> => 4
    },
    Opts#{ <<"hashpath">> => ignore }
),
Model = hb_ao:get(
    <<"model">>,
    {as, <<"message@1.0">>, Trained},
    Opts
),
Records = maps:from_list([
    begin
        {ok, Weight} = hb_ao:resolve(
            Trained,
            #{
                <<"path">> => <<"likelihood">>,
                <<"body">> => Name,
                <<"include-end">> => true,
                <<"result-mode">> => <<"float">>
            },
            Opts#{ <<"hashpath">> => ignore }
        ),
        true = Weight > 0.0,
        {Name, #{
            <<"value">> => maps:get(Name, Targets),
            <<"pricing">> => #{ <<"weight">> => Weight }
        }}
    end
 || Name <- Names
]),
Occupancy = lists:sum([
    maps:get(<<"weight">>, maps:get(<<"pricing">>, Record))
 || Record <- maps:values(Records)
]),
true = Occupancy > 0.0 andalso Occupancy < 1.0,
InitialNamespace = #{ <<"model">> => Model, <<"names">> => Records }.
```

Write the unsigned artifact locally and read it back before publishing:

```erlang
{ok, LocalID} = hb_cache:write(InitialNamespace, Opts),
{ok, ReadBack} = hb_cache:read(LocalID, Opts),
Names = lists:sort(hb_maps:keys(
    hb_maps:get(<<"names">>, ReadBack, #{}, Opts),
    Opts
)).
```

Then sign and upload it with the deployment wallet:

```erlang
Wallet = ar_wallet:load_keyfile("/absolute/path/to/key.json"),
UploadOpts = Opts#{ <<"priv-wallet">> => Wallet },
Signed = hb_message:commit(
    InitialNamespace,
    UploadOpts,
    #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true }
),
ArtifactID = hb_message:id(Signed, all, UploadOpts),
{ok, _} = hb_client_remote:upload(
    Signed,
    UploadOpts,
    <<"ans104@1.0">>
).
```

Wait until a fresh node can read `ArtifactID` through its normal configured
store. Verify the model order, name count, targets, positive weights, and the
same `0 < Occupancy < 1` invariant from that read-back.

## Publish and pin the devices

Run the release checks before any upload:

```bash
rebar3 device test --module dev_markov,dev_probability_time,dev_spectrum
rebar3 eunit-all
rebar3 device publish --dry-run \
  --devices dev_markov,dev_probability_time,dev_spectrum \
  --key /absolute/path/to/key.json
```

Record the specification IDs, implementation IDs, signer, source commit, and
artifact ID. Remove `--dry-run` only for the release invocation. Beta nodes
must either preload this exact source commit or pin the resulting
implementations under `trusted-devices`; do not rely on an unrecorded moving
signer policy.

## Spawn the process

Create one **data-free layer-1 Arweave transaction** with these tags:

```text
device             = process@1.0
execution-device   = spectrum@1.0
scheduler-device   = arweave-scheduler@1.0
scheduler-mode     = all
pricing-device     = probability-time@1.0
probability-device = markov@1.0
initial-namespace  = <ArtifactID>
initial-nametime   = 52560000
spectrum-height    = <recorded tip at signing>
grace-factor       = <final value>
target-occupancy   = <final value>
price-at-target    = <final value>
```

Add `grace-notice=<ID>` if the beta UI should resolve names in grace to a
specific notice. Keep transaction data empty: `all` mode currently schedules
base-layer data-free transactions.

Sign, inspect every tag and the zero data size, dispatch, and record the
process ID and confirmation height. Do not switch any live resolver yet.

## Synchronize and verify

On a fresh beta node:

1. Run Copycat in `headers` mode through the process spawn height.
2. Resolve the process with `~arweave-scheduler@1.0` in `all` mode.
3. Confirm its imported `model/order` is 4 and its name count matches the
   reviewed artifact.
4. For several roots, confirm `value`, `pricing/weight`, `deadline`, and
   `grace`. Every initial deadline must equal
   `spectrum-height + 52560000`; grace must use the recorded factor.
5. Quote a new name and an existing name through `price`, then make one real
   minimum-value purchase on a disposable beta name and resolve the resulting
   lease.
6. Restart the node from an empty process cache and repeat the state and quote
   checks. The namespace artifact may be cached, but correctness must not
   depend on the previous process state cache.
7. Configure the resolver only after those checks:

```text
<ProcessID>~process@1.0/now/~spectrum@1.0
```

Keep the previous resolver available for rollback. A rollback changes the
node resolver; it does not mutate the immutable beta process.
