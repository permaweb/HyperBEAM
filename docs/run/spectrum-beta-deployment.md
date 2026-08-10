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

The launch is immutable. Prepare and verify the linked state items before
signing the process transaction; a correction requires a replacement process
and resolver update.

## Finalize the launch state

Record these values in the release log. Every runtime parameter below is read
from process state.

| Key | Beta value | Unit |
| --- | --- | --- |
| `model+link` | prepared Markov model item ID | Arweave ID |
| `names+link` | prepared lease table item ID | Arweave ID |
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

Prepare the two AO structured state items that `~spectrum@1.0` reads directly:

```text
model item: <trained markov@1.0 model>

names item:
  <name>:
    value: <existing resolver target>
    deadline: <spectrum-height + 52560000>
    grace: <deadline + floor(grace-factor × 52560000 / 10000)>
    pricing:
      weight: <likelihood(name, include-end=true)>
```

The items are not constrained by Arweave's tag-size limit because `bundle=true`
places their nested messages in ANS-104 data. They remain subject to the
selected bundler's data-size limits. The data-free process transaction then
carries only the two 43-byte IDs as native `+link` tags. On decoding, AO-Core
exposes those links as the process's ordinary `model` and `names` state; no
initialization pointer or import step is required.

The ANS-104 items are immutable state dependencies, not process inputs. They
are read by content ID and never enter the schedule. After the process spawn,
`~arweave-scheduler@1.0` continues to admit only data-free `tx@1.0` layer-1
headers.

Build them through the packaged devices. The following is the normative Erlang
flow; `TrainingNames` is the filtered observation list, `Targets` is the
reviewed root map, `Height` is the recorded chain tip, `GraceFactor` is the
final basis-point value, and `Opts` names the complete preloaded store:

```erlang
Names = lists:sort(maps:keys(Targets)),
Nametime = 200 * 262800,
Deadline = Height + Nametime,
Grace = Deadline + ((GraceFactor * Nametime) div 10000),
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
            <<"deadline">> => Deadline,
            <<"grace">> => Grace,
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
true = maps:size(Records) =:= maps:size(Targets).
```

Sign both items with `bundle=true`, write them locally, and read them back
before publishing. Use the signed message IDs, not the return value of the
cache write:

```erlang
Wallet = ar_wallet:load_keyfile("/absolute/path/to/key.json"),
UploadOpts = Opts#{ <<"priv-wallet">> => Wallet },
Bundle = #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true },
SignedModel = hb_message:commit(Model, UploadOpts, Bundle),
SignedNames = hb_message:commit(Records, UploadOpts, Bundle),
ModelID = hb_message:id(SignedModel, all, UploadOpts),
NamesID = hb_message:id(SignedNames, all, UploadOpts),
{ok, _} = hb_cache:write(SignedModel, UploadOpts),
{ok, _} = hb_cache:write(SignedNames, UploadOpts),
{ok, ModelReadBack} = hb_cache:read(ModelID, UploadOpts),
{ok, NamesReadBack} = hb_cache:read(NamesID, UploadOpts),
4 = hb_maps:get(<<"order">>, ModelReadBack, not_found, UploadOpts),
Names = lists:sort(hb_maps:keys(
    NamesReadBack,
    UploadOpts
)).
```

Upload both items through the configured bundler:

```erlang
{ok, _} = hb_client_remote:upload(
    SignedModel,
    UploadOpts,
    <<"ans104@1.0">>
),
{ok, _} = hb_client_remote:upload(
    SignedNames,
    UploadOpts,
    <<"ans104@1.0">>
).
```

Wait until a fresh node can read `ModelID` and `NamesID` through its normal
configured store. Verify the model order, name count, targets, deadlines,
grace, positive weights, and the same `0 < Occupancy < 1` invariant from those
read-backs. This must work without priming that node's process cache.

## Publish and pin the devices

Run the release checks before any upload:

```bash
rebar3 device test --module dev_markov,dev_probability_time,dev_spectrum
rebar3 eunit-all
rebar3 device publish --dry-run \
  --devices dev_markov,dev_probability_time,dev_spectrum \
  --key /absolute/path/to/key.json
```

Record the specification IDs, implementation IDs, signer, source commit,
`ModelID`, and `NamesID`. Remove `--dry-run` only for the release invocation.
Beta nodes must either preload this exact source commit or pin the resulting
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
model+link         = <ModelID>
names+link         = <NamesID>
spectrum-height    = <recorded tip at signing>
grace-factor       = <final value>
target-occupancy   = <final value>
price-at-target    = <final value>
```

Add `grace-notice=<ID>` if the beta UI should resolve names in grace to a
specific notice. Commit the process with `tx@1.0`, `bundle=false`. Inspect that
`data_size` is zero and that `model+link` and `names+link` are signed tags. The
process itself is therefore header-complete, while the linked state is fetched
by content ID. Every later scheduled input remains a real data-free base-layer
Arweave transaction.

Do not put the state bundle in the process transaction's data. In `all` mode,
slot zero is reconstructed from the process header; omitting transaction data
would then also omit the nested `model` and `names`. Explicit `+link` tags keep
the process header sufficient without imposing tag-size limits on either item.

Sign, inspect every tag and the zero data size, dispatch, and record the
process ID and confirmation height. Do not switch any live resolver yet.

## Synchronize and verify

On a fresh beta node:

1. Run Copycat in `headers` mode through the process spawn height.
2. Resolve the process with `~arweave-scheduler@1.0` in `all` mode.
3. Confirm its linked `model/order` is 4 and its name count matches the
   reviewed table.
4. For several roots, confirm `value`, `pricing/weight`, `deadline`, and
   `grace`. Every initial deadline must equal
   `spectrum-height + 52560000`; grace must use the recorded factor.
5. Quote a new name and an existing name through `price`, then make one real
   minimum-value purchase on a disposable beta name and resolve the resulting
   lease.
6. Restart the node from an empty process cache and repeat the state and quote
   checks. The linked items may be cached, but correctness must not
   depend on the previous process state cache.
7. Configure the resolver only after those checks:

```text
<ProcessID>~process@1.0/now/~spectrum@1.0
```

Keep the previous resolver available for rollback. A rollback changes the
node resolver; it does not mutate the immutable beta process.
