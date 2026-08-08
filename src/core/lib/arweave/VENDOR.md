# Vendored Arweave modules

Upstream: `ArweaveTeam/arweave` @ `50e47de` (release 2.9.6-alpha1,
`?RELEASE_NUMBER 92`). Upstream paths below are relative to that repo's
`apps/arweave/`.

These modules keep upstream's formatting, tabs, comments and function order so
that `diff -r src/core/lib/arweave <upstream>/apps/arweave/src` is the upgrade
workflow. Most deviations from upstream are marked with a `%% VENDOR:` comment
at the site and recorded below. Do not apply HyperBEAM style here — the `ar_`
prefix signals vendored style deliberately.

> **Marker coverage is incomplete, and this is a known defect.** An independent
> audit measured **72 of 132 substantive hunks (55%) carrying no `%% VENDOR:`
> marker** — worst in `ar_pricing` (13 of 16 unmarked), `ar_serialize` (9 of
> 13), `ar_block` (9 of 14), `ar_node_utils` (7 of 15), and all four NIF
> wrappers (zero). Most are mechanical removals of `prometheus_*`, `?LOG_*`,
> `-ifdef(LOCALNET)` and test blocks, and the audit independently verified that
> **no fork height, constant, hash preimage or arithmetic function on the block
> path diverges from upstream**. But until the markers are complete, `diff -r`
> is not the clean upgrade workflow this directory's whole design rests on.
> Treat the per-module sections below as the authority and the markers as
> partial.

---

## Account state modules

Vendored for full account-state (wallet tree) validation: the block header
field `wallet_list` is the root hash over every account after applying the
block, so validating it is a cryptographic checksum on the whole account state
transition.

| Module | Upstream path | LoC upstream → here |
|---|---|---|
| `ar_patricia_tree.erl` | `src/ar_patricia_tree.erl` | 910 → 916 |
| `ar_diff_dag.erl` | `src/ar_diff_dag.erl` | 394 → 394 (byte-identical) |
| `ar_tx_replay_pool.erl` | `src/ar_tx_replay_pool.erl` | 259 → 269 |
| `../../include/ar_wallets.hrl` | `include/ar_wallets.hrl` | 12 → 12 (byte-identical) |

`ar_wallets.erl` is **not** vendored. Upstream it is a `gen_server` wrapping
`ar_diff_dag` over the patricia tree, with `ar_storage` casts, `ar_events`
sends, `prometheus_counter` and peer HTTP I/O baked in. Its pure residue is
about forty lines of glue; it is reproduced verbatim below so the HyperBEAM
bridge (`lib_arweave_accounts`) can lift it without re-deriving it.

### Deviations

**`ar_patricia_tree.erl`** — one deviation, in the test block only.

- `stochastic_test/0`: upstream's `lists:foldl/3` returns `Acc` unchanged, so
  the accumulator stays `start` for every permutation and the
  permutation-invariance assertion (`?assertEqual(H, Acc)`) never fires. The
  fold now returns `H`. That is the property the account-tree bootstrap
  depends on — a peer serves `GET /wallet_list/<root>[/<cursor>]` pages in its
  own order and the root hash must come out the same — so leaving the
  assertion dead would be leaving the structure untested exactly where we lean
  on it. The invariant holds: 1000 random 3-account trees × 6 insertion orders
  each, plus a negative control confirming the assertion now fails when fed a
  wrong value.

No production-code deviation. The module is pure: `maps`, `gb_sets`,
`binary`, and `ar_deep_hash:hash/1`. No config, no ets, no prometheus, no
events.

**`ar_diff_dag.erl`** — none. Byte-identical to upstream. Pure `maps` +
`sets`.

**`ar_tx_replay_pool.erl`** — three deviations.

1. `-include_lib("arweave/include/ar.hrl")` → `-include("include/ar.hrl")`
   (`{i, "src/core"}` is set in `rebar.config`).
2. `?BLOCK_TX_DATA_SIZE_LIMIT` comes from `include/ar.hrl`, as upstream. It was
   briefly defined locally while `ar.hrl` lacked it; that define is gone,
   because a bare redefine of a header macro is a hard error.
3. `#tx.last_tx` → `#tx.anchor` (6 sites). Same field — the previous
   transaction of the wallet, or a recent block hash — spelled differently in
   HyperBEAM's `ar.hrl`. Renaming `anchor` back to `last_tx` in `ar.hrl` would
   touch every HyperBEAM codec, so the vendored module maps to HyperBEAM's
   spelling instead.

`os:system_time(seconds)` in `verify_tx/2` is left in place: that is the
on-edge mempool path, not the block-validation path. `verify_block_txs/1`
takes its timestamp from the caller.

**`include/ar_wallets.hrl`** — none. Byte-identical, `-ifdef(AR_TEST)` and
all. `AR_TEST` is never defined in HyperBEAM, so `?WALLET_LIST_CHUNK_SIZE` is
the mainnet 2500 and `?MAX_SERIALIZED_WALLET_LIST_CHUNK_SIZE` is 505000 — the
`limit` to put on a `GET /wallet_list/...` fetch.

### Required from modules owned elsewhere

`ar_tx_replay_pool` calls these; they do not exist in HyperBEAM's `ar_tx.erl`
today and must be vendored by whoever owns it:

```erlang
ar_tx:verify(TX, {Rate, PricePerGiBMinute, KryderPlusRateMultiplier,
        Denomination, RedenominationHeight, Height, Wallets, Timestamp},
        VerifySignature) -> boolean().
ar_tx:check_last_tx(Wallets, TX)  -> boolean().   % upstream ar_tx.erl:152
ar_tx:utility(TX)                 -> {1|2, Denomination, Reward}.  % :238
ar_tx:get_addresses(TXs)          -> [Address].   % :217
```

Note the `AR_TEST` clause of upstream `ar_tx:verify/3` short-circuits unsigned
transactions to `true`. That clause is not consensus — vendor the `-else.`
branch.

`ar_node_utils:apply_tx/3` is called from `verify_block_txs/2` and
`pick_txs_under_size_limit/2`; `ar_node_utils.erl` is vendored separately.

Still missing from `src/core/include/ar.hrl` (reported, not added here):
`?TX_DATA_SIZE_LIMIT` and `?BLOCK_TX_DATA_SIZE_LIMIT`, plus `?TX_SIZE_BASE` and
`?DEPRIORITIZE_V1_TX_SIZE_THRESHOLD` for `ar_tx:utility/1`.

### The account hash — two forms, both live

`ar_block:hash_wallet_list/1` (upstream `src/ar_block.erl:802`) drives
`ar_patricia_tree:compute_hash/2` with a leaf function that has **two
clauses**, and the account tuple's arity selects between them:

```erlang
%% 2-tuple accounts (denomination 1 and mining permitted) - the legacy form.
ar_deep_hash:hash([Addr, binary:encode_unsigned(Balance), LastTX])

%% 4-tuple accounts.
sha384(encode_bin(Addr, 8) || encode_int(Balance, 8) || encode_bin(LastTX, 8)
        || encode_int(Denomination, 8) || MiningPermissionBin)
```

**Mainnet accounts are overwhelmingly the 2-tuple form.** `denomination` is 1
and mining is permitted, and `ar_node_utils:update_account/6` writes a 2-tuple
exactly when `Denomination == 1 andalso MiningPermission == true`. So the
legacy `ar_deep_hash` path — which has no length prefixes and encodes the
balance as a bare `binary:encode_unsigned/1` — is the dominant one, not the
`sha384` concatenation. An account that is otherwise identical hashes
differently in the two forms, so producing a 4-tuple where upstream produces a
2-tuple silently breaks the root hash.

Interior nodes combine children with `ar_deep_hash:hash/1` over the child
hashes in **ascending** key order (`gb_sets_foldr` visits largest-first and
conses, so the list comes out ascending), a node that has both a value and
children hashes as `ar_deep_hash:hash([OwnHash | ChildHashes])`, and a
valueless node with exactly one child passes that child's hash straight
through. An empty tree hashes to `<<>>`, not to a digest.

Test vectors (this tree, OTP 28), hex:

```
leaf {12345, <<3:256>>}                  609A645EB8071581B7607E69AFAFE5BF
                                         92C1830E6B4D71A4BDACD3CA5E5F71BF
                                         CFA7F61B732F0F0DFFF2D21143240A09
leaf {0, <<>>}                           67AB3E5EFA4BB218A989328932EB3465
                                         CB2DBCF91120634776AB70FCF2E98385
                                         DCC7AD07CB057E7F471748E76102CE0D
leaf {12345, <<3:256>>, 1, true}         B823EAC651BFB8A4F9E646322C9C4226
                                         B9F99D8D4D5CE40714C76AF2B54241E8
                                         02AA6BCD8B0ADD318A09AC581CFB8B4F
leaf {12345, <<3:256>>, 2, true}         5CF4BC77FBB60349AC6952FA90B0EE5D
                                         43095028784B4AC6BDB937D697EEF5F4
                                         3F29EF9EB7D71E365C5D54D3204A5AE5
leaf {12345, <<3:256>>, 2, false}        4491414B2E18961761553916DDB8DCAE
                                         95EC0C13B8AD534941AD0742BB0F4EA7
                                         C9B11EC37926F639B04035312D57F0A4
```

with `Addr = <<1:256>>`. Note the first and third differ: same balance, same
anchor, denomination 1, mining permitted — only the tuple arity changes.

### The pure residue of `ar_wallets.erl`

Lift these into `lib_arweave_accounts` unchanged. Upstream sources:
`apply_diff/2` `ar_wallets.erl:389`, `reverse_diff/2` `:403`, `get_map/2`
`:416`, `get_account_tree_range/2` `:430`, `maybe_add_node/5` `:450`.

```erlang
apply_diff(Diff, Tree) ->
	maps:fold(
		fun (Addr, remove, Acc) ->
				ar_patricia_tree:delete(Addr, Acc);
			(Addr, {Balance, LastTX}, Acc) ->
				ar_patricia_tree:insert(Addr, {Balance, LastTX}, Acc);
			(Addr, {Balance, LastTX, Denomination, MiningPermission}, Acc) ->
				ar_patricia_tree:insert(Addr,
						{Balance, LastTX, Denomination, MiningPermission}, Acc)
		end,
		Tree,
		Diff
	).

reverse_diff(Diff, Tree) ->
	maps:map(
		fun(Addr, _Value) ->
			case ar_patricia_tree:get(Addr, Tree) of
				not_found ->
					remove;
				Value ->
					Value
			end
		end,
		Diff
	).

get_map(Tree, Addresses) ->
	lists:foldl(
		fun(Addr, Acc) ->
			case ar_patricia_tree:get(Addr, Tree) of
				not_found ->
					Acc;
				Value ->
					maps:put(Addr, Value, Acc)
			end
		end,
		#{},
		Addresses
	).

get_account_tree_range(Tree, Cursor) ->
	Range =
		case Cursor of
			first ->
				ar_patricia_tree:get_range(?WALLET_LIST_CHUNK_SIZE + 1, Tree);
			_ ->
				ar_patricia_tree:get_range(Cursor, ?WALLET_LIST_CHUNK_SIZE + 1, Tree)
		end,
	case length(Range) of
		?WALLET_LIST_CHUNK_SIZE + 1 ->
			{element(1, hd(Range)), tl(Range)};
		_ ->
			{last, Range}
	end.

maybe_add_node(DAG, RootHash, RootHash, _Wallets, _Metadata) ->
	%% The wallet list has not changed - there are no transactions
	%% and the miner did not claim the reward.
	DAG;
maybe_add_node(DAG, UpdatedRootHash, RootHash, Wallets, Metadata) ->
	case ar_diff_dag:is_node(DAG, UpdatedRootHash) of
		true ->
			%% The new wallet list is already known from a different fork.
			DAG;
		false ->
			ar_diff_dag:add_node(DAG, UpdatedRootHash, RootHash, Wallets, Metadata)
	end.
```

`apply_diff/2` and `reverse_diff/2` are the two functions handed to
`ar_diff_dag:reconstruct/3` and `ar_diff_dag:move_sink/4`. The DAG's metadata
slot carries the block's `denomination`.

Upstream's `set_current/4` (`ar_wallets.erl:373`) is the same three steps minus
the plumbing — `ar_diff_dag:move_sink/4`, then `ar_diff_dag:update_sink/3` with
a function that recomputes and re-keys by the root hash, then
`ar_diff_dag:filter/2`. The `gen_server:cast(ar_storage, {store_account_tree_
update, ...})`, the `true = Height >= ar_fork:height_2_2()` assertion and the
`prometheus_counter:inc(wallet_list_size, ...)` line are the parts to drop.
The prune depth upstream passes is `ar_block:get_consensus_window_size() * 2`
= **100** (`ar_node_worker.erl:1495`).

`ar_diff_dag:filter/2` measures distance by each node's insertion counter
against the current sink's, not by graph distance, and removing a node cascades
to its whole source subtree. So a sibling fork hanging off a pruned ancestor is
pruned with it. At depth 100 that cannot bite — `?CHECKPOINT_DEPTH` is 18, so
every eligible fork branches well inside the window — but it does mean the
depth is not a per-branch budget.

---

## Native NIF wrappers

Thin `-on_load` modules over the vendored Arweave C NIFs. They contain no
logic — every exported function is `erlang:nif_error(nif_not_loaded)` until
the `.so` binds over it.

| Module | Upstream path | Loads |
|---|---|---|
| `ar_rx512_nif.erl` | `src/ar_rx512_nif.erl` | `priv/rx512_arweave.so` |
| `ar_rx4096_nif.erl` | `src/ar_rx4096_nif.erl` | `priv/rx4096_arweave.so` |
| `ar_rxsquared_nif.erl` | `src/ar_rxsquared_nif.erl` | `priv/rxsquared_arweave.so` |
| `ar_vdf_nif.erl` | `src/ar_vdf_nif.erl` | `priv/vdf_arweave.so` |

The C sources they bind to live at `native/arweave_randomx/` and
`native/arweave_vdf/`, each with its own `VENDOR.md` recording the upstream
pin (`50e47de`), the RandomX fork pin (`eef4dc8`) and the build deviations.
The `.so` files are produced by those directories' Makefiles, wired into
`rebar.config`'s `pre_hooks`.

### Deviations

1. `code:priv_dir(arweave)` → `code:priv_dir(hb)` in each `init_nif/0`.
2. `-include_lib("arweave/include/ar.hrl")` and its `?LOG_ERROR` calls
   dropped from `ar_rx512_nif`, `ar_rx4096_nif` and `ar_rxsquared_nif`.
   Those lines only ever ran immediately before
   `erlang:nif_error(nif_not_loaded)`, i.e. when the `.so` failed to load,
   and the include was needed for nothing else. `ar_vdf_nif` never had it.

Module names, function names, arities and argument order are unchanged, so
these stay drop-in for Arweave call sites. `ERL_NIF_INIT` in the C names the
module (`ar_rx512_nif`, `ar_rx4096_nif`, `ar_rxsquared_nif`, `ar_vdf_nif`) —
renaming an Erlang module means editing its C source too.

### Why here and not `src/preloaded/`

`src/preloaded/` is excluded from rebar's `src_dirs` and packaged by the
Forge preloader, which globs only `dev_*.erl` and `lib_*.erl` — an
`ar_*_nif.erl` there would never be compiled. `src/core/` is compiled
recursively by rebar and already holds `secp256k1_nif.erl`, the existing
`-on_load` NIF wrapper that resolves `code:priv_dir(hb)` the same way.

### ECDSA (secp256k1) — already present, nothing added

Arweave verifies 2.9-era ECDSA block signatures through
`ar_wallet:verify({{?ECDSA_SIGN_ALG, secp256k1}, Pub}, Data, Sig)`, which is
`secp256k1_nif:ecrecover(Data, Sig)` plus `PubExtracted =:= Pub`.
HyperBEAM's existing `src/core/lib/secp256k1_nif.erl` and
`native/secp256k1/` already provide exactly that: `ecrecover/2` defaults to
a `sha256` digest and returns `{true, CompressedPubKey}` / `{false, <<>>}`,
matching upstream `src/secp256k1_nif.erl` line for line. The recovered key
is 33 bytes (`?ECDSA_PUB_KEY_SIZE`), the signature 65
(`?ECDSA_SIG_SIZE`). The only difference in the C is that HyperBEAM's copy
drops `ERL_NIF_DIRTY_JOB_CPU_BOUND` from the two NIF entries — a scheduling
choice, not a correctness one. Verified in `scripts/secp_check.erl`.

---

## Pure consensus modules

The height/difficulty/pricing/entropy-mapping arithmetic. These modules carry
no process state and touch no I/O, so almost all of them are vendored with the
include paths rewritten and nothing else. `diff -u <upstream>/apps/arweave/src
src/core/lib/arweave` is the whole review.

| Module | Upstream path | LoC upstream → here | Diff lines vs upstream |
|---|---|---|---|
| `ar_fraction.erl` | `src/ar_fraction.erl` | 129 → 129 | **0 (byte-identical)** |
| `ar_unbalanced_merkle.erl` | `src/ar_unbalanced_merkle.erl` | 69 → 69 | 2 |
| `ar_inflation.erl` | `src/ar_inflation.erl` | 186 → 186 | 2 |
| `ar_block_time_history.erl` | `src/ar_block_time_history.erl` | 130 → 130 | 2 |
| `ar_rewards.erl` | `src/ar_rewards.erl` | 258 → 258 | 2 |
| `ar_difficulty.erl` | `src/ar_difficulty.erl` | 188 → 188 | 4 |
| `ar_pricing_transition.erl` | `src/ar_pricing_transition.erl` | 287 → 287 | 8 |
| `ar_fork.erl` | `src/ar_fork.erl` | 176 → 177 | 5 |
| `ar_merkle.erl` | `src/ar_merkle.erl` | 1220 → 1225 | 15 |
| `ar_retarget.erl` | `src/ar_retarget.erl` | 407 → 287 | 136 (tests only) |
| `ar_replica_2_9.erl` | `src/ar_replica_2_9.erl` | 675 → 205 | 533 (tests + storage) |
| `ar_testnet.erl` | `src/ar_testnet.erl` | 116 → 38 | 102 (resolved to mainnet) |
| `../../include/ar_consensus.hrl` | `include/ar_consensus.hrl` | 211 → 211 | **0 (byte-identical)** |
| `../../include/ar_pricing.hrl` | `include/ar_pricing.hrl` | 282 → 282 | **0 (byte-identical)** |
| `../../include/ar_inflation.hrl` | `include/ar_inflation.hrl` | 43 → 43 | 2 |

Every one compiles clean — zero errors, zero warnings — with
`erlc -I src/core -o <dir> src/core/lib/arweave/<file>.erl`.

### The include rewrite (applies to every file above)

`-include_lib("arweave/include/ar*.hrl")` → `-include("include/ar*.hrl")`.
`rebar.config` sets `{i, "src/core"}`, so `include/ar.hrl` resolves to
`src/core/include/ar.hrl`. This is the only change in six of the modules and in
`ar_inflation.hrl`.

### Per-module deviations

**`ar_fraction.erl`** — none. Byte-identical. This matters more than anywhere
else in the tree: `natural_exponent/2` is the 24-term Taylor series over exact
bigint fractions that `ar_inflation` and `ar_pricing` are built on. A float
port silently diverges from mainnet. Byte-identity is the guarantee.

**`ar_unbalanced_merkle.erl`, `ar_block_time_history.erl`, `ar_rewards.erl`,
`ar_difficulty.erl`, `ar_pricing_transition.erl`, `ar_inflation.erl`** — the
include rewrite only. Upstream's `-ifdef(AR_TEST)` and `-ifdef(LOCALNET)`
branches are left in place; neither symbol is ever defined in HyperBEAM, so
the mainnet arm is what compiles.

**`ar_fork.erl`** — the two `-include_lib` lines that HyperBEAM's reduced copy
had dropped are restored in rewritten form. All sixteen fork heights were
already at full upstream parity, `height_2_9() -> 1602350` included. No
behavioural change.

**`ar_merkle.erl`** — grown from an older upstream snapshot to 2.9.6-alpha1.
Three deviations, all pre-existing HyperBEAM adaptations carried forward:

1. The two `-include_lib` lines → `-include("include/hb.hrl")`. `hb.hrl`
   includes `ar.hrl` and additionally supplies `?event`. `ar_consensus.hrl` is
   not referenced by this module.
2. `?LOG_ERROR([...ar_util:encode(ID)...])` → `?event({...hb_util:encode(ID)...})`
   in the `RightBound =< 0` guard clause. The `throw(invalid_right_bound)` that
   follows is unchanged; only the log sink differs.
3. `ar_util:floor_int/2` → `hb_util:floor_int/2` in the strict-split leaf
   check. The two implementations are the same expression,
   `IntValue - (IntValue rem Nearest)`.

What grew: the `#node.note` field is now `#node.tree_midpoint_offset`; the
rebase dispatch moved from clause-order fall-through to an explicit
`has_leading_rebase_marker/1` test; and `has_leading_rebase_marker/1`,
`has_positive_leaf_size/3`, `has_redundant_rebase_marker/2,5`,
`strip_rebase_markers/1`, `get_branch_id/3` and `get_leaf_id/2` are now
exported. Every function HyperBEAM already exported keeps its exact previous
behaviour — see "ar_merkle: old vs grown" below.

**`ar_retarget.erl`** — the include rewrite, plus the test section is dropped.
`simple_retarget_test_/0` mines on a live node through `ar_weave`,
`ar_test_node`, `ar_node` and `ar_storage`; `calculate_difficulty_linear_test_/0`
meck-mocks `ar_fork:height_2_5/0` through `ar_test_node:test_with_mocked_functions/3`.
None of that harness is vendored. Their `hashes/1` helper went with them. A
`%% VENDOR:` note stands in their place. All production code — including the
whole pre-2.5 ladder — is untouched.

**`ar_replica_2_9.erl`** — the include rewrite, plus three removals:

1. `get_entropy_partition_range/1` and `get_next_fetch_offset/3` are dropped
   and un-exported. They are chunk-storage syncing helpers that reverse the
   partition mapping via `ar_chunk_storage:get_chunk_byte_from_bucket_end/1`.
   `ar_chunk_storage` is a gen_server-backed storage module and chunk storage
   is an explicit non-goal, so neither function can work here. Neither is
   reachable from proof validation.
2. `get_entropy_partition/1` called `ar_node:get_partition_number(BucketStart)`.
   `ar_node` is the node gen_server. Its integer clause is exactly
   `Offset div ar_block:partition_size()` and `BucketStart` is always an
   integer at that call site, so the body is inlined.
3. `get_entropy_bucket_start/1` asserted
   `BucketStart == ar_chunk_storage:get_chunk_bucket_start(PaddedEndOffset)`.
   That function is a restatement of the three lines above the assert, and
   `ar_chunk_storage` is not vendored, so the redundant self-check is dropped.

The test section is dropped for the same reason as `ar_retarget`'s — every
test runs under `ar_test_node:test_with_mocked_functions/3` with meck-mocked
`ar_block` partition sizes. The consensus surface — `get_entropy_key/3`,
`get_slice_index/1`, `get_entropy_index/2`, `get_partition_offset/1`,
`get_entropy_partition/1` — is untouched, including the
`sha256(<<Partition:256, EntropyIndex:256, RewardAddr/binary>>)` key.

**`ar_testnet.erl`** — resolved to mainnet, as briefed. Upstream wraps every
function in `-ifdef(TESTNET)` and carries a parallel `?TESTNET_*` constant set;
`TESTNET` is never defined in HyperBEAM, so only the `-else.` arms could ever
compile. Those arms are kept and the branches, the `?TESTNET_*` defines and the
`-ifndef` ladder are resolved away. One further deviation:
`locked_rewards_blocks/1` upstream first consults
`application:get_env(arweave, locked_rewards_blocks)` and falls back to
`locked_rewards_blocks2/1`. HyperBEAM has no `arweave` application environment,
so the mainnet `?LOCKED_REWARDS_BLOCKS` is returned unconditionally and the
helper is folded in. **All seven exported signatures are unchanged**, so
callers (`ar_retarget`, `ar_rewards`, `ar_inflation`, `ar_pricing`) are
untouched.

### `include/ar.hrl` additions

`src/core/include/ar.hrl` is shared and pre-existing. It was **added to only** —
nothing was removed, reordered or re-valued. Two changes:

1. An `-ifndef(AR_HRL)` / `-define(AR_HRL, true)` guard around the whole file,
   matching upstream's. Needed because `ar_inflation.hrl` includes `ar.hrl`,
   so a module including both `hb.hrl` and `ar_inflation.hrl` would otherwise
   define every record twice. Erlang treats a macro redefinition as an *error*,
   not a warning.
2. A fenced `%%% VENDOR:` section at the end, copied verbatim from upstream's
   `include/ar.hrl` with its tabs and comments intact:

   - Records: `#poa{}`, `#nonce_limiter_info{}`, `#block{}`.
   - Macros: `?NETWORK_NAME`, `?DEFAULT_NETWORK_NAME`, `?CLIENT_VERSION`,
     `?RELEASE_NUMBER`, `?FORK_1_6`, `?DEEP_HASH_ALG`, `?MERKLE_HASH_ALG`,
     `?RSA_BLOCK_SIG_SIZE`, `?ECDSA_PUB_KEY_SIZE`, `?ECDSA_SIG_SIZE`,
     `?DEFAULT_DIFF`, `?TARGET_BLOCK_TIME`, `?RETARGET_BLOCKS`,
     `?RETARGET_TOLERANCE`, `?JOIN_CLOCK_TOLERANCE`,
     `?MAX_BLOCK_PROPAGATION_TIME`, `?CLOCK_DRIFT_MAX`, `?GENESIS_TOKENS`,
     `?KiB`, `?MiB`, `?GiB`, `?TiB`, `?STORE_BLOCKS_BEHIND_CURRENT`,
     `?CHECKPOINT_DEPTH`, `?BLOCK_TX_COUNT_LIMIT`, `?RANDOMX_DIFF_ADJUSTMENT`,
     `?DIFF_ADJUSTMENT_DOWN_LIMIT`, `?DIFF_ADJUSTMENT_UP_LIMIT`,
     `?MAX_PACKING_DIFFICULTY`, `?COMPOSITE_PACKING_SUB_CHUNK_COUNT`,
     `?COMPOSITE_PACKING_SUB_CHUNK_SIZE`, `?COMPOSITE_PACKING_ROUND_COUNT`,
     `?MAX_PATH_SIZE`, `?PACKING_2_5_THRESHOLD_CHUNKS_PER_SECOND`,
     `?PADDING_NODE_DATA_ROOT`, `?INITIAL_VDF_DIFFICULTY`, `?IS_BLOCK/1`,
     `?BI_TO_BHL/1`, `?OK/1`.
   - `-include_lib("kernel/include/logger.hrl")`, upstream's last line, which
     is where `?LOG_INFO` / `?LOG_DEBUG` / `?LOG_ERROR` come from. HyperBEAM
     defines no `?LOG_*` macro of its own, so there is no collision.

   Upstream's `#tx{}` is deliberately **not** vendored. HyperBEAM's own `#tx{}`
   higher up the same file is the one the codec stack uses and it has diverged:
   `anchor` where upstream spells `last_tx`, `format = ans104` rather than `1`,
   plus `unsigned_id` / `owner_address` / `manifest`. Vendoring upstream's
   would be a redefinition error and would break every HyperBEAM codec.

   `?TX_DATA_SIZE_LIMIT`, `?BLOCK_TX_DATA_SIZE_LIMIT`, `?TX_SIZE_BASE` and
   `?DEFAULT_HTTP_IFACE_PORT` are carried in `ar.hrl`, verbatim from upstream
   and in upstream's own order. The rule: a constant upstream's `ar.hrl` owns
   is vendored into `src/core/include/ar.hrl`; a constant upstream keeps
   module-local stays module-local here. That is why
   `?DEPRIORITIZE_V1_TX_SIZE_THRESHOLD` is **not** here — upstream defines it
   at `src/ar_tx.erl:21`, so it belongs in `ar_tx.erl`.

### External modules these depend on

Not vendored by this section; supplied by the rest of the tree.

| Module | Used by | Status |
|---|---|---|
| `ar_block` | `ar_block_time_history`, `ar_retarget`, `ar_rewards`, `ar_replica_2_9` | present |
| `ar_pricing` | `ar_difficulty`, `ar_rewards` | present |
| `ar_deep_hash` | `ar_unbalanced_merkle` | present |
| `hb_util` | `ar_merkle` | present |
| `ar_serialize` | `ar_block_time_history`, `ar_rewards` | present |
| `ar_serialize:binary_to_packing/2` | catch-all returning the caller's error where upstream raises `function_clause` | marked at `ar_serialize.erl:1150` |
| `ar_node_utils` | `ar_rewards` | present |
| **`ar_util`** | `ar_difficulty`, `ar_retarget`, `ar_rewards`, `ar_replica_2_9`, `ar_pricing_transition` | Vendored — see the *Utilities* section |

`ar_util:between/3` is the important one: `ar_difficulty:scale_diff/3` and
`ar_pricing_transition:get_transition_price/2` both clamp with it, and
`get_transition_price/2` passes `infinity` as the upper bound, relying on
Erlang term order (integer < atom) so that the clamp is a no-op. Any
substitute must reproduce that.

### Verification run

**Compilation.** All twelve modules, zero errors and zero warnings:

```
$ for F in ar_fork ar_merkle ar_difficulty ar_block_time_history ar_retarget \
           ar_rewards ar_replica_2_9 ar_unbalanced_merkle ar_fraction \
           ar_inflation ar_pricing_transition ar_testnet; do
    erlc -I src/core -o /tmp/vendor-pure-check src/core/lib/arweave/$F.erl; done
```

**Inflation schedule — the exact-integer proof.** Upstream's own inflation
tests, run against the vendored modules, all 11 pass in 28 s: year 1 emits
5,500,000 AR and each subsequent year halves, to a 0.001 % tolerance. This
exercises `ar_fraction:natural_exponent/2` at
`?INFLATION_NATURAL_EXPONENT_DECIMAL_FRACTION_PRECISION` = 24 over 2.6 M block
heights.

```
$ erl -noshell -pa <ebin> -eval 'eunit:test(ar_inflation, [verbose]), halt().'
  ar_inflation: test_year_1...[2.413 s] ok
  ... test_year_10...[3.530 s] ok
  All 11 tests passed.
```

**Merkle.** Upstream's `ar_merkle` suite (9 tests, including the rebase
shallow/nested/bad-path/partial-chunk cases and `extract_root_test`'s real
mainnet path fixture) and `ar_unbalanced_merkle` (2 tests) all pass.

**`ar_merkle`: old vs grown.** HyperBEAM's previous `ar_merkle` was not a
trimmed upstream — it was an *older* upstream snapshot. To prove the grow is
behaviour-preserving for the functions HyperBEAM already called, both versions
were compiled side by side and compared over:

- 4,000 randomly generated trees, including nested (rebased) subtrees, with
  every generated path checked under all four rulesets — `generate_tree/1`,
  `generate_path/3`, `validate_path/4`, `validate_path/5`, `extract_note/1`
  and `extract_root/1` compared on each;
- 200,000 fuzzed paths spread across every length class, weighted towards the
  96–127 byte window where the old clause-order fall-through and the new
  `has_leading_rebase_marker/1` dispatch could in principle disagree;
- 21,000 adversarial paths of every length from 96 to 200 that begin with 32
  zero bytes and are anchored to a root chosen so the zero-left-child branch
  node actually hash-verifies, forcing the old code down its fall-through path.

Result: **zero mismatches** on all three. The refactor is a rename plus helper
extraction, not a semantic change.

**No regression in existing HyperBEAM consumers.** Every `.erl` under
`src/core` that existed before this work was compiled twice — once against the
original `ar.hrl` from `HEAD`, once against the extended one — and the two
diagnostic logs were diffed. 700 pre-existing diagnostics disappeared (the
`ar_block` growth work depends on the new macros); **zero new diagnostics were
introduced**. `ar_merkle`'s existing callers — `dev_arweave_offset`
(`extract_note/1`), `dev_arweave`, `dev_bundler`, `dev_bundler_task`,
`dev_copycat_arweave` and `ar_tx` — all still compile unchanged.

---

## VDF, packing, pricing and validation modules

Six modules covering the VDF chain, RandomX packing, the two reward formulas
and the field-consistency stage of block validation. Four of them are heavy
`gen_server`s upstream; only their pure core is vendored.

| Module | Upstream LoC | Vendored LoC |
|---|---|---|
| `ar_nonce_limiter.erl` | 1683 | 232 |
| `ar_vdf.erl` | 215 | 228 |
| `ar_packing_server.erl` | 1044 | 331 |
| `ar_mine_randomx.erl` | 422 | 218 |
| `ar_pricing.erl` | 869 | 700 |
| `ar_node_utils.erl` | 939 | 695 |

`ar_vdf.erl` grows because the `%% VENDOR:` notes are longer than the six lines
they replace.

**Zero `ets`, zero `persistent_term`, zero `arweave_config` reads across all
six.** Where upstream held state in `ets`, the state is now an explicit
argument; where it read config, the mainnet default is inlined as a constant or
the value became a function argument.

### Explicit-state APIs

Upstream `ar_packing_server` keeps the RandomX state in an `ets` table
(`init_packing_state/0` writes it, `get_packing_state/0` reads it). We build it
and hand it back:

```erlang
ar_packing_server:init_packing_state(Mode)              -> PackingState.
ar_packing_server:init_packing_state(Mode, Variants)    -> PackingState.
    %% Mode     :: fast | light
    %% Variants :: [rx512 | rx4096 | rxsquared]  (default: all three)
    %% PackingState :: {RandomXState512, RandomXState4096, RandomXStateSquared}
    %%   each element either {rx512|rx4096|rxsquared, NifResource}
    %%   or the atom `not_initialised' if the variant was not requested.

ar_packing_server:get_randomx_state_for_h0(PackingDifficulty, PackingState).
ar_packing_server:get_randomx_state_by_packing(Packing, PackingState).
ar_packing_server:unpack(Packing, ChunkOffset, TXRoot, Chunk, ChunkSize, PackingState)
        -> {ok, Unpacked} | {error, invalid_packed_size | invalid_chunk_size
                                    | invalid_padding | invalid_randomx_mode}
         | {exception, Error}.
ar_packing_server:unpack_sub_chunk(Packing, AbsoluteEndOffset, TXRoot, Chunk,
        SubChunkStartOffset, PackingState) -> {ok, UnpackedSubChunk} | {error, _}.
ar_packing_server:generate_replica_2_9_entropy(RewardAddr, BucketEndOffset,
        SubChunkStartOffset, RandomXState) -> Entropy.
ar_packing_server:do_generate_entropy(RandomXState, Key) -> Entropy.
ar_packing_server:chunk_key(Packing, ChunkOffset, TXRoot) -> {PackingAtom, Key}.
ar_packing_server:unpad_chunk/3,4, packing_atom/1 -- unchanged from upstream.
```

`?RANDOMX_PACKING_KEY = <<"default arweave 2.5 pack key">>` is a fixed protocol
constant, so a state built once is valid for the life of the network. It is a
long-lived NIF resource: register it once through `hb_name:singleton/2` and pass
it down.

`generate_replica_2_9_entropy/4` and `do_generate_entropy/2` keep their upstream
arity but repurpose an argument. Upstream's fourth argument to
`generate_replica_2_9_entropy/4` was a `CacheEntropy` boolean which, when true,
took a per-key lock and consulted `ar_entropy_cache`. Every recall byte we
validate has a distinct entropy key, so the cache can never hit; the argument is
now the `rxsquared` RandomX state, which upstream re-derived from the `ets`
packing state inside `do_generate_entropy/2` (whose first argument was
`RewardAddr` for exactly that purpose).

A variant left out of `init_packing_state/2` holds `not_initialised`, which the
`ar_mine_randomx` dispatchers reject rather than handing a bad term to a NIF.
Measured:

```
state = {not_initialised,not_initialised,rxsquared_state}
get_randomx_state_for_h0(10, PS)  = not_initialised
hash(not_initialised, ...)        = {error,invalid_randomx_mode}
unpack(spora_2_6 w/ uninit state) = {error,invalid_randomx_mode}
entropy w/ uninit rxsquared       = {error,function_clause}
```

The nonce limiter's two verification entry points take the thread count that
upstream read from `Config#config.max_nonce_limiter_*_thread_count`:

```erlang
ar_nonce_limiter:verify(StartStepNumber, PrevOutput, NumCheckpointsBetweenHashes,
        Hashes, ResetStepNumber, ResetSeed, ThreadCount, VDFDifficulty,
        NextVDFDifficulty) -> {true, ValidatedSteps} | false.
ar_nonce_limiter:verify_no_reset(StartStepNumber, PrevOutput,
        NumCheckpointsBetweenHashes, Hashes, ThreadCount, VDFDifficulty)
        -> {true, ValidatedSteps} | false.
ar_nonce_limiter:validate_last_step_checkpoints(B, PrevB, PrevOutput, ThreadCount)
        -> boolean().
```

`validate_last_step_checkpoints/3` gains a fourth argument for the same reason
and loses upstream's `{true, cache_match}` / `{false, cache_mismatch, Steps}`
results, which reported on a VDF session cache we do not have. The remaining
pure functions — `get_seed_data/2`, `get_entropy_reset_point/2`,
`maybe_add_entropy/4`, `mix_seed/2`, `mix_seed2/2`, `session_key/1,3`,
`get_reset_frequency/0`, `is_ahead_on_the_timeline/2` — keep their upstream
signatures exactly.

### Deviations

**`ar_nonce_limiter.erl`** — the `gen_server` (27 `gen_server:` calls), the six
`ets` tables, the VDF session store, the compute worker, the four prometheus
metrics and `skip_already_computed_steps/5` are all dropped.

The whole VDF client/server protocol is dropped deliberately, not for size:
`apply_external_update/2` and `apply_chain/2` accept VDF steps from a configured
peer *without recomputing them*. That is a trust path. A trustless validator
must not have one.

`debug_double_check/4`, which re-runs each verification in pure Erlang under the
`double_check_nonce_limiter` option, is dropped — but `ar_vdf`'s Erlang
reference implementations are still vendored, so the cross-check can be run by
hand.

`get_or_init_nonce_limiter_info/1`, which synthesises an info record for pre-2.6
blocks, is dropped; every block we validate is post-2.9, so `PrevB`'s own
`#nonce_limiter_info{}` is read directly. All `-ifdef(LOCALNET)` clauses are
dropped in favour of the `-else.` branch.

*Added, not upstream:* `verify_no_reset/6` raises `error(invalid_step_count)` on
an empty hash list. Upstream would fall through the NIF's binary match and
return a quiet `false`, indistinguishable from a failed proof — and a validator
that accepted an empty step list would be verifying the VDF chain **vacuously**.
This is the one behaviour added rather than removed.

**`ar_vdf.erl`** — vendored whole apart from three things. The single
`arweave_config` read (`Config#config.vdf`, the SHA-2 backend selector) becomes
`-define(VDF_BACKEND, openssl)`, its mainnet default. This is not consensus:
all three backends compute the same function, and **verification never consults
it at all** — `verify/8` goes straight to
`ar_vdf_nif:vdf_parallel_sha_verify_with_reset_nif/10`, which picks its SHA-256
kernel in C from the CPU's own feature bits (deviation 4 of
`native/arweave_vdf/VENDOR.md`), falling back to OpenSSL where the hardware has
no SHA-256 extensions. The selector only affects `compute/3`, which a validator
does not call.

The `-ifdef(AR_TEST)` clause of `compute2/3` (a 50 ms sleep) is dropped.
`take_every_nth/2` is upstream's `ar_util:take_every_nth/2`, called rather than
copied: `ar_util` is vendored here and byte-identical, and two copies of a
VDF-verification helper can drift.

**`ar_packing_server.erl`** — the `gen_server`, the worker pool, all 17 `ets`
operations, the 13 prometheus histograms, `ar_entropy_cache` and the buffer
back-pressure are dropped, along with every packing (as opposed to unpacking)
entry point: `pack/4`, `repack/6`, `pack_replica_2_9_chunk/3`,
`encipher_replica_2_9_chunk/2`, `decipher_replica_2_9_chunk/2` and
`pad_chunk/1,2`. A validator unpacks; it never packs.

`unpack/6` is upstream's *internal* `unpack/7` with the prometheus wrapper and
the `External` label removed and the `{ok, Chunk, WasAlreadyUnpacked}` triples
collapsed to upstream's *public* `unpack/5` contract, `{ok, Chunk}` — which is
what `ar_poa:validate2/2` consumes. The `already_unpacked` tag only ever fed the
repack path.

Two `?LOG_ERROR` terms carried `ar_serialize:encode_packing(Packing, true)`;
they carry `packing_atom(Packing)` instead, so a log line cannot depend on
`ar_serialize`.

**`ar_mine_randomx.erl`** — **upstream bug, corrected.** Upstream has

```erlang
init_light(RxMode, Key) ->
	init_light2(RxMode, jit(), large_pages(), Key).
```

against a callee of `init_light2(RxMode, Key, JIT, LargePages)`. The arguments
are transposed: `jit()` arrives as `Key`, `large_pages()` as `JIT`, and the key
binary as `LargePages`, so the NIF raises `badarg` immediately:

```
{badarg,[{ar_rx4096_nif,rx4096_init_nif,[1,1,0,<<"default arweave 2.5 pack key">>,0],[]},
         {ar_mine_randomx,init_light2,4,...}]}
```

Upstream never trips over it because nothing there calls `init_light/2` — a node
always builds fast states, and the light-mode tests call `init_light2/4`
directly. **Light mode is our default**, so we hit it on the first call. The
argument order is corrected in place. A future upgrade must not silently
reintroduce it.

`jit()`, `large_pages()` and `hardware_aes()` become the constants `1`, `0` and
`1` — the mainnet defaults, since `randomx_jit` is not in the default disable
list, `randomx_large_pages` is not in the default enable list, and
`randomx_hardware_aes` is not in the default disable list.

Every `-ifdef(STUB_RANDOMX)` / AR_TEST clause is dropped: the STUB build replaces
RandomX with SHA-256 and shrinks the replica-2.9 entropy to 32 KiB, and those
code paths are not consensus. With them go `split_into_sub_chunks/1,2` and
`packing_rounds/1`, which only the STUB and re-encrypt paths used. All the
encrypt/re-encrypt entry points are dropped.

*Kept verbatim despite being wrong:* the fallback clause of `init_light2/4`
returns `{exceperrortion, invalid_randomx_mode}` — an upstream typo for
`{error, ...}`. It is unreachable (we only ever pass literal mode atoms) and
fixing it would be a silent divergence from upstream, so it stands.

**`ar_pricing.erl`** — vendored nearly whole, because **both reward formulas are
live**. v2 pricing does not activate until height 2,069,870, so every block we
validate today takes the 2.5-era `get_miner_reward_and_endowment_pool/1` path,
while `price_per_gib_minute` is already computed by the v2 code. All post-2.5
arithmetic here is exact integer or exact `{Dividend, Divisor}` fraction
arithmetic over `ar_fraction`; the float branches are unreachable above fork 2.5
and are kept only so the diff stays mechanical.

Removed: the four `prometheus_gauge:set/2,3` calls and the debug logging beside
them (`log_price_metrics/11` and `network_data_size/5` — which was also the only
call into `ar_testnet`), the `-ifdef(LOCALNET)` overrides of
`get_redenomination_threshold/0` and `get_redenomination_delay_blocks/0`, the
`?LOG_DEBUG` in `recalculate_price_per_gib_minute/1` and
`recalculate_usd_to_ar_rate3/1` (both used `ar_util:safe_divide/2`), and the
tests.

Also removed: `get_tx_fee/4` and `get_storage_cost/4`, and with them
`get_miner_fee_share/2`, `get_perpetual_gb_cost_at_timestamp/2` and
`get_perpetual_gb_cost/2`. `get_tx_fee/4` has **no caller anywhere in upstream**
— `ar_tx` reaches the 2.6 `get_tx_fee/1` instead — and `get_storage_cost/4` is
called once, from `ar_node_worker`, to publish a price metric. Neither is on a
validation path. They are also the only users of `?TX_SIZE_BASE`, which
`src/core/include/ar.hrl` now defines, so restoring them is mechanical.

**`ar_node_utils.erl`** — `validate/6` is **not** a superset of block
validation. Its own doc comment says so: it performs no PoW, no PoA, no VDF and
no RandomX. It is the field-consistency stage that runs after the pre-validator
and the nonce limiter. A port that implements only this validates nothing
cryptographic.

**The two dead clauses are not vendored.** `validate_block(difficulty, ...)`
(upstream `:496`) and `validate_block(block_field_sizes, ...)` (`:614`) are
unreachable: the chain routes `strict_data_split_threshold → usd_to_ar_rate` and
`txs → tx_root`, stepping over both. Difficulty and the retarget are validated
in the pre-validator by `ar_retarget:validate_difficulty/2`; the 2.6 field-size
limits are enforced structurally by `ar_serialize:binary_to_block/1`. A
`%% VENDOR:` note sits at each of the two routing sites so the gap is visible in
the source. Upstream's own tests call `validate_block(difficulty, ...)`
directly, which is the only reason the clause still exists there.

`solution_passes_diff_check/2` is dropped — it destructures
`#mining_solution{}` and calls `ar_mining_server`, so with it goes
`-include("ar_mining.hrl")`. The `-ifdef(LOCALNET)` `passes_diff_check/4` (which
skips the check entirely) and the `-ifdef(AR_TEST)` `is_wallet_invalid/2` (which
lets unsigned transactions through) are dropped in favour of their `-else.`
branches. The `ar_testnet:top_up_test_wallet/2` call in `update_accounts5/3` is
dropped: on mainnet it is the identity, the crediting clause being behind
`-ifdef(TESTNET)`.

Log terms that carried `ar_util:encode(indep_hash)` carry the raw binary
instead. The `try`/`catch` in `validate/6` is load-bearing and kept — it turns
an exception raised by any check into `{invalid, validation_exception}` rather
than killing the caller.

`validate_block/2` is exported (upstream keeps it private, reaching it only from
same-module tests) so a single check can be driven in isolation for
mutation testing.

`update_accounts/3` takes a plain account **map**, not a patricia tree — the
tree lives in `ar_wallets`. It calls `ar_patricia_tree` nowhere.

### The VDF reset path — read this before touching `verify/9`

The NIF's reset branch is live code that is never taken, and the mechanism is
not obvious.

`verify_no_reset/6` passes `ResetStepNumber = 0`. `ar_vdf:verify2/8` then
computes `step_number_to_salt_number(0 - 1)`, which misses the `0 -> 0` clause
and evaluates `(-1 - 1) * 25 + 1 = -49`. That is handed to the NIF as
`<< -49:256 >>`, i.e. `2^256 - 49` — a salt no step can ever reach, so
`fast_rev_cmp256` never fires:

```
verify_no_reset ResetStepNumber=0 -> ResetSalt=-49 -> << ResetSalt:256 >> =
  115792089237316195423570985008687907853269984665640564039457584007913129639887
(2^256-49 =
  115792089237316195423570985008687907853269984665640564039457584007913129639887)
```

Passing `ResetStepNumber = 1` instead — the obvious "fix" — yields
`step_number_to_salt_number(0) = 0`, and salt 0 is exactly what step 1 uses. The
NIF would mix entropy where the network does not. **Do not "fix" it.**

The entropy mix therefore happens in Erlang, in `verify/9`, between two
`verify_no_reset/6` calls made at two different difficulties: the steps before
the reset line at `VDFDifficulty`, `mix_seed2/2`, then the steps from the line on
at `NextVDFDifficulty`.

Related, and equally easy to get wrong: salt numbering is discontinuous at step
1. `step_number_to_salt_number/1` gives `[{0,0},{1,1},{2,26},{3,51},{10,226}]` —
every gap is 25 except the first, which is 1. Step 1 therefore consumes salts
`0..24` and step 2 consumes `1..25`; they overlap in 24 salts. Every later step
is clean.

### `?VDF_DIFFICULTY` is a genesis default, not the verification difficulty

`?VDF_DIFFICULTY` (`?VDF_SHA_1S div 25` = 600,000) appears in exactly one place
in upstream outside the benchmark tool: `ar_block:compute_next_vdf_difficulty/1`
returns it when `ar_block_time_history:has_history(Height)` is false, i.e.
before fork 2.7 plus the history length. Verification always uses the per-block
`#nonce_limiter_info.vdf_difficulty` / `.next_vdf_difficulty`, threaded through
`verify/9` and `validate_last_step_checkpoints/4` as ordinary arguments. Mainnet
is running 1,111,546.

Grepping the six vendored modules for a hardcoded difficulty finds one hit, and
it is an upstream comment:

```
src/core/lib/arweave/ar_vdf.erl:25:%% default IterationCount = ?VDF_DIFFICULTY
```

### Verification run

Compiled with `erlc -Werror -I src/core` against `src/core/include/`; all six
emit a `.beam` with zero warnings.

**Seed rotation against mainnet.** `get_seed_data/2` was run over 13 consecutive
block transitions from `test/fixtures/arweave/`, comparing the full five-tuple
against the successor block's own `#nonce_limiter_info{}`:

```
  block 1974240 seed data OK      block 1974875 seed data OK
  block 1974850 seed data OK      block 1974876 seed data OK
  block 1974860 seed data OK      block 1974877 seed data OK
  block 1974871 seed data OK      block 1974878 seed data OK
  block 1974872 seed data OK      block 1974879 seed data OK
  block 1974873 seed data OK      block 1974880 seed data OK
  block 1974874 seed data OK

13/13 seed-data checks passed
```

Both branches are exercised: three of the thirteen cross an entropy reset line
(1974239→1974240, 1974849→1974850, 1974876→1974877) and rotate all five fields,
including `next_seed := PrevB.indep_hash` and
`next_partition_upper_bound := PrevB.weave_size`; the other ten cross no line and
carry every field through unchanged.

**Measured RandomX cost, light mode**, on the vendored modules against the built
NIFs:

| Operation | Time |
|---|---|
| `init_packing_state(light, [rxsquared])` | 1104 ms, once |
| `init_packing_state(light, [rx4096])` | 1051 ms, once |
| `rsp_fused_entropy_nif` — 8 MiB entropy | **240 ms** (233.9–252.7, cold 289) |
| rx4096 hash (H0) | **23.6 ms** (22–25) |

`ar_mine_randomx:info/1` on the rxsquared state returns
`{ok,{rxsquared,light,0,2097152}}`, confirming light mode and a 2 MiB
scratchpad. A two-chunk replica-2.9 block therefore costs
`1 × 23.6 + 2 × 240 ≈ 504 ms` of RandomX against a ~128 s block interval —
**0.4% of the budget**. Fast mode's ~6.6 GiB of datasets and minutes of
construction buy nothing here.

---

## Block, serialization and TX modules

Upstream pin: `ArweaveTeam/arweave` @ `50e47de`, release 2.9.6-alpha1,
`?RELEASE_NUMBER 92`. Paths below are relative to `apps/arweave/`.

The block header itself: its binary and JSON wire formats, the hash the block
producer signs, the signature check, the proof-of-access validation path, and
the block-index arithmetic the recall byte resolves through.

| Module | Upstream path | LoC upstream → here | Diff lines vs upstream |
|---|---|---|---|
| `ar_block.erl` | `src/ar_block.erl` | 1303 → 770 | 667 |
| `ar_poa.erl` | `src/ar_poa.erl` | 397 → 354 | 99 |
| `ar_serialize.erl` | `src/ar_serialize.erl` | 2437 → 1157 | 2151 (subset) |
| `ar_block_index.erl` | `src/ar_block_index.erl` | 200 → 196 | rewritten, see below |
| `ar_tx.erl` | `src/ar_tx.erl` | 964 → 1171 | **diverged fork; ~399 lines added by later sections — see "the L1 consensus interface" and "the transaction field-shape predicates"** |
| `ar_wallet.erl` | `src/ar_wallet.erl` | 505 → 449 | **diverged fork; 49 lines added, see below** |
| `../../include/ar_vdf.hrl` | `include/ar_vdf.hrl` | 27 → 27 | **0 (byte-identical)** |
| `../../include/ar_block.hrl` | `include/ar_block.hrl` | 2 → 2 | **0 (byte-identical)** |
| `../../include/ar_poa.hrl` | `include/ar_poa.hrl` | 25 → 25 | **0 (byte-identical)** |

Every one compiles clean via
`erlc -I src/core -o /tmp/vendor-block-check src/core/lib/arweave/<file>.erl`.
`ar_wallet.erl` and `ar_tx.erl` emit warnings, all of them pre-existing and in
code this section did not touch.

`src/core/include/ar.hrl` also gained `#chunk_metadata{}` inside the existing
`-ifndef(AR_HRL)` guard — `ar_poa.hrl`'s `#chunk_proof{}` has it as a field
type, and `ar_poa:validate/1` builds one. Nothing else in `ar.hrl` was changed
by this section.

### `ar_block.erl`

Vendored verbatim other than the removals below. `?LOG_DEBUG` survives intact:
upstream's `ar.hrl` ends with `-include_lib("kernel/include/logger.hrl")` and
the vendored copy keeps that line.

Dropped, each with a `%% VENDOR:` note at the site:

- **The pre-2.6 block data segment path.** `generate_block_data_segment/1,2`,
  `generate_block_data_segment_base/1`, `indep_hash/2`, `poa_to_list/1`,
  `encode_tags/1`, and `indep_hash/1`'s pre-2.6 branch. This subsystem
  validates post-2.9 blocks; the BDS is fork-2.4-era.
- **`block_field_size_limit/1`** plus `validate_tags_size/1` and
  `validate_tags_length/2`. Its only caller is
  `ar_node_utils:validate_block(block_field_sizes, ...)`, which
  is unreachable: `validate_block(txs, ...)` routes to the
  `tx_root` clause. The same limits are enforced structurally by
  `ar_serialize:binary_to_block/1`.
- **`test_wallet_list_performance/0,1,2,3`** and `random_wallet/0`. A
  benchmark harness that pulls in `ar_serialize:wallet_list_to_json_struct/3`
  and `jiffy`.
- **The EUnit block.**
- **`generate_tx_root_for_block/2`'s TXID clause**, which resolved identifiers
  through `ar_storage:read_tx/1`. There is no `ar_storage` here — HyperBEAM
  resolves transaction bodies through `hb_cache` — so the caller passes
  resolved `#tx{}` records.
- **`-ifdef(LOCALNET)` in `get_recall_range/5`.** Only the mainnet clause is
  kept. `get_recall_range/5` still takes the two ignored arguments so the
  arity matches upstream.

Refactored:

- **`get_block_bounds/2,3`.** Upstream takes a `CacheTab` atom and reads the
  `ar_block_cache` and `ar_block_index` ETS tables directly
  (`src/ar_block.erl:749-776`). HyperBEAM keeps both the recent block cache and
  the block index in `hb_store`/`hb_cache`, so the third argument is now a map
  supplying the three lookups upstream performs, all keys optional:

  ```erlang
  #{ oldest_block_start => non_neg_integer(),                    % default 0
     get_block          => fun((BH :: binary()) -> #block{} | not_found),
     seek               => ar_block_index:seek() }
  ```

  `#{}` means "no cache, no index" and degrades to walking backwards from
  `PrevB` alone. `get_block_bounds/2` passes `#{}`.

Kept but note the dependency: `compute_h0/6` calls
`ar_packing_server:get_randomx_state_for_h0/2` and `ar_mine_randomx:hash/2`;
`hash_wallet_list/1` calls `ar_patricia_tree:compute_hash/2`;
`shift_packing_2_5_threshold/1` calls `ar_testnet:target_block_time/1`.

`compute_h0/5` is the exception, and it is a **behavioural** deviation:
upstream's arity-5 clause fetches the packing state from `ar_packing_server`,
which is not vendored, so this one raises `error(packing_state_not_threaded)`
instead. Nothing in the block path reaches it -- `~arweave-spora@2.9` threads
the state explicitly and calls the arity-6 form -- but the arity-5 name is
upstream's and answers differently here, so an upgrade must not assume it
works.

### `ar_poa.erl`

**Correction:** `validate/1`, `validate2/2` and `validate3/2` are vendored but
**cannot run** — they call `ar_packing_server:unpack/5` and
`unpack_sub_chunk/5`, which take the RandomX state from a process-wide
singleton this port does not have; the vendored `ar_packing_server` exports
those at arity 6. `~arweave-spora@2.9` implements the same validation against
the arity-6 forms and is what the block device actually calls. They are kept
for diff cleanliness and annotated at the site.

Vendored and used: `validate_paths/1`, `chunk_proof/4,5`, `get_recall_bucket_offset/2`,
`get_data_path_validation_ruleset/1,2,3`, `get_padded_offset/1,2`, and the
local-admission `validate_data_path/5,6` leg.

Deviations:

- **The two `prometheus_counter:inc(validating_packed_spora, ...)` calls** in
  `validate2/2` and `validate3/2` are dropped. HyperBEAM does not declare that
  counter.
- **`chunk_proof/2` and `chunk_proof/3`** (`src/ar_poa.erl:172-193`) are
  dropped. They resolve block bounds by reading the `ar_block_index` ETS table
  and serve the local chunk-storage and chunk-verification paths, not block
  validation. `ar_block_index:get_block_bounds/2` is the replacement.
- **`validate_pre_fork_2_5/4`** and its two helpers are dropped.
- **`ar_serialize:encode_packing/2`** inside the two `?LOG_WARNING`s becomes
  the raw packing term; `ar_util:format_peer/1` in `log_invalid_data_path/3`
  likewise. `ar_util:encode/1` → `hb_util:encode/1`.

The parts that are easy to get wrong are unchanged and worth restating: the
`tx_path` is validated by `ar_merkle:validate_path/4` — **arity 4, the basic
ruleset**, no border/split/rebase checks — while only the `data_path` gets the
offset-derived ruleset from `get_data_path_validation_ruleset/3`, keyed off the
**block start offset of the block containing the recall byte** and compared
against the module constants `?MERKLE_REBASE_SUPPORT_THRESHOLD` and
`?STRICT_DATA_SPLIT_THRESHOLD`, not against the validating block's own fields.

### `ar_serialize.erl`

A subset, not a reduction of the whole: the block/proof leg only. Vendored —

- `block_to_binary/1`, `binary_to_block/1` and every `parse_*` helper the
  latter chains through (`parse_block_tags_transactions/2`,
  `parse_block_transactions/1,2,3`, `parse_block_post_2_6_fields/2`,
  `parse_checkpoints/2`, `parse_block_tags/1,4`, `parse_double_signing_proof/2`,
  `parse_post_2_7_fields/2`, `parse_post_2_8_fields/2`,
  `parse_post_2_9_fields/2`).
- `encode_post_2_6_fields/1` → `encode_post_2_7_fields/1` →
  `encode_post_2_8_fields/1` → `encode_post_2_9_fields/1`,
  `encode_nonce_limiter_info/1`.
- `encode_int/2`, `encode_bin/2`, `encode_bin_list/3`,
  `encode_double_signing_proof/2` — the primitives `ar_block:generate_signed_hash/1`
  is built out of.
- `block_to_json_struct/1`, `json_struct_to_block/1`, `poa_to_json_struct/1`,
  `json_struct_to_poa/1`, `nonce_limiter_info_to_json_struct/2`,
  `find_value/2`, `delete_keys/2`.
- `binary_to_poa/1` with `binary_to_packing/2`.
- `block_index_to_binary/1`, `binary_to_block_index/1`,
  `block_index_to_json_struct/1`, `json_struct_to_block_index/1`.
- `reward_history_to_binary/1`, `binary_to_reward_history/1`,
  `block_time_history_to_binary/1`, `binary_to_block_time_history/1`,
  `parse_32b_list/1` — the `/reward_history/<BH>` and
  `/block_time_history/<BH>` wire formats, which are §2 carried state.
- `jsonify/1`, `dejsonify/1`, `json_decode/1`.

Not vendored: the transaction, wallet-list, ARQL, mining-pool, VDF-server,
coordinated-mining, block-announcement, `#chunk_metadata{}`-map and
`data_roots` legs.

#### The JSON library

Upstream uses **jiffy**. HyperBEAM has no jiffy dependency and runs on OTP 28,
so the JSON leg goes through OTP's built-in `json` module. Both directions are
driven to produce jiffy's exact **eep18** term shape, because upstream's
`find_value/2` and every `json_struct_to_*` function pattern-match on it:

```erlang
-define(JSON_EEP18_DECODERS, #{
    object_start  => fun(_Acc) -> [] end,
    object_push   => fun(Key, Value, Acc) -> [{Key, Value} | Acc] end,
    object_finish => fun(Acc, OldAcc) -> {{lists:reverse(Acc)}, OldAcc} end
}).
```

`jsonify/1` dispatches through a custom value encoder that renders `{Proplist}`
via `json:encode_key_value_list/2`, which emits atom keys as strings and `{[]}`
as `{}` — byte-for-byte what jiffy produces. `json_decode/2`'s jiffy-options
arity has no equivalent and is not vendored; only `json_decode/1` is.

#### `json_struct_to_block/1` is pre-2.6 only — upstream, not a port artefact

`src/ar_serialize.erl:1357` asserts `true = is_integer(Height) andalso Height <
ar_fork:height_2_6()`. There is **no post-2.6 JSON block parser anywhere in the
upstream tree** — post-2.6 blocks are only ever parsed from `/block2`'s binary
form by `binary_to_block/1`. The assertion is preserved verbatim rather than
"fixed"; inventing a post-2.6 JSON parser would be new consensus code, not a
port. `block_to_json_struct/1` is fully fork-aware in the other direction and
is the function the round-trip evidence below exercises.

A later agent building `dev_arweave_block`'s codec should ingest `/block2`, not
`/block`.

#### `nonce_limiter_info_to_json_struct/2` renames three fields

The binary encoder renames none. In JSON:

| record field | JSON key |
|---|---|
| `partition_upper_bound` | `zone_upper_bound` |
| `next_partition_upper_bound` | `next_zone_upper_bound` |
| `steps` | `checkpoints` |

There is therefore **no `steps` key in the JSON at all**. `checkpoints` carries
the per-step VDF outputs; `last_step_checkpoints` carries the 25 intra-step
checkpoints of the final step. A parser that looks for a JSON `steps` key finds
nothing, yields an empty step list, and makes downstream VDF chain verification
pass vacuously. Verified explicitly across all 17 fixtures — see below.

#### Transactions inside blocks

`encode_tx/1` and `parse_tx/1`'s body clause are **not** vendored, only the
bare-32-byte-identifier clauses. Upstream's `encode_tx/1` reads `TX#tx.last_tx`;
HyperBEAM's `#tx{}` spells that field `anchor`. Blocks served by `/block2`
carry identifiers only, which is the clause that is kept. A block carrying
inline transaction bodies fails to serialize rather than serializing wrongly.

### `ar_block_index.erl`

**Not a port of upstream's implementation.** Upstream is 24 ETS operations
against a private `block_index` ordered_set of `{{WeaveSize, Height, H, TXRoot}}`
tuples. HyperBEAM keeps the block index in `hb_store`/`hb_cache`, so none of
that plumbing is vendored. What *is* vendored is the arithmetic upstream layers
on top of its three ETS seeks, expressed against an explicit lookup function so
any backing store can supply it.

**The API a store adapter must implement.** A `seek()` is a `fun/1` answering
three queries. All offsets are absolute weave offsets; `WeaveSize` is the
block's *end* offset, exactly as upstream stores it.

```erlang
-type entry() :: {BH :: binary(), WeaveSize :: non_neg_integer(), TXRoot :: binary()}.
-type seek()  :: fun((term()) -> term()).

Seek({height, Height})            -> entry() | not_found
Seek({hash, BH})                  -> {Height, entry()} | not_found
Seek({weave_size_above, Offset})  -> {Height, entry()} | not_found
```

`{weave_size_above, Offset}` must return the **lowest-height** entry whose
`WeaveSize` is strictly greater than `Offset` — the block containing byte
`Offset`. It is upstream's `ets:next(block_index, {Offset, n, n, n})`, where
the atom `n` sorts above every integer so the seek lands past every entry with
`WeaveSize =< Offset`.

**The lowest-height rule is load-bearing, not pedantry.** A block with no
transactions does not move the weave, so several consecutive heights share one
`WeaveSize`. Upstream's ordered_set keys on the whole 4-tuple, so those coexist
and the seek lands on the lowest height of the group. A `WeaveSize -> Height`
map that overwrites returns the *last* such height and produces the wrong
`{BlockStart, BlockEnd, TXRoot}` for every offset in the preceding block. This
was caught by the block-index verification below, on real mainnet data that
happens to contain two zero-transaction blocks; `from_list/1` now keeps the
first height seen.

Functions:

| Here | Upstream |
|---|---|
| `get_block_bounds/2` | `get_block_bounds/1` (`src/ar_block_index.erl:53`) |
| `get_block_bounds_with_height/2` | `get_block_bounds_with_height/1` (`:63`) |
| `get_element_by_height/2` | `get_element_by_height/1` (`:41`) |
| `member/2` | `member/1` (`:27`) |
| `get_range/3` | `get_range/2` (`:106`) |
| `get_list/2` | `get_list/1` (`:32`) |
| `get_list_by_hash/2` | `get_list_by_hash/1` (`:37`) |
| `from_list/1` | — builds a `seek()` over an in-memory BI, for tests and small indices |
| `empty/0` | — a `seek()` that always answers `not_found` |
| — | `init/1`, `update/2`, `get_intersection/1,2`, `get_last/0`: ETS lifecycle and join-time reconciliation, not vendored |

`get_list/2` and `get_range/3` keep upstream's ordering — largest height first.

### `ar_tx.erl` and `ar_wallet.erl` have DIVERGED from upstream

**Read this before assuming `diff -r` against upstream is meaningful for these
two files.** They are not reduced upstream subsets like the rest of this tree.
They are HyperBEAM forks with different semantics, and they are load-bearing
for `ar_bundles`, `dev_ans104`, `dev_tx`, `lib_arweave_common` and the ECDSA
test suite. Growing them to upstream would break the codec stack. **Neither was
converted**; the block subsystem adapts to them instead.

**`ar_tx.erl` — not touched at all by this section.** The divergence is mostly
structural, in the `#tx{}` record it operates on: `anchor` where upstream
spells `last_tx`; `format` defaulting to `ans104` rather than `1`; extra
`unsigned_id`, `owner_address` and `manifest` fields; and ANS-104, Ethereum,
Solana and typed-Ethereum signature support upstream does not have.

One divergence is **behavioural**, and it predates this branch:
`get_owner_address/1` (`ar_tx.erl:309`) answers the atom `not_set` when
`owner` is `?DEFAULT_OWNER`, 512 zero bytes. Upstream has no such clause and
returns `sha256(Owner)` for that input like any other. The clause is left alone
-- `ar_tx:sign/3,4` populates `owner_address` through it, so removing it would
change what a zero-owner transaction signs over -- but nothing in the block
subsystem calls the function. The record field carries upstream's value, and
that is what is read. `dev_arweave_block:zero_owner_is_an_address_not_an_atom_test`
pins both halves. The three
functions block validation needs — `generate_chunk_id/1`,
`get_weave_size_increase/2`, `generate_chunk_tree/1` — are byte-identical to
upstream, which is why nothing had to change.

**`ar_block:generate_size_tagged_list_from_txs/2` has diverged and was left
diverged.** Upstream tags each entry with `TX#tx.id` (`src/ar_block.erl:850`);
HyperBEAM carries the whole `#tx{}` record, because
`dev_copycat_arweave:process_tx/4` reads `TX#tx.id` and `TX#tx.data_size` off
it. **This is consensus-neutral**: every consensus consumer immediately
projects the pair away with `[{Root, Offset} || {{_, Root}, Offset} <- ...]`,
so the `tx_root` it feeds is identical either way. HyperBEAM's `PaddingRoot =
<<>>` is `?PADDING_NODE_DATA_ROOT`, so that matches too.

**`ar_wallet.erl` — 49 lines added, nothing changed or removed.** Two gaps had
to be filled for `ar_block` and `ar_serialize` to work:

1. **`to_address/1` did not accept `{KeyType, Pub}`.** Upstream's arity-1
   dispatches on `{KeyType, Pub}` and `{KeyType, Priv, Pub}`; HyperBEAM's takes
   a bare public key binary and assumes `?DEFAULT_KEY_TYPE`.
   `ar_block:verify_signature/3` passes `{KeyType, Pub}`, which previously
   raised inside `to_rsa_address/1`. The two upstream clauses are added *ahead*
   of HyperBEAM's, guarded on `is_binary(Pub)` so a full `{Priv, Pub}` keypair
   still falls through to the existing clause. Both added forms currently
   raise, so no working call site changes behaviour.
2. **`base64_address_with_optional_checksum_to_decoded_address/1` was absent.**
   `ar_serialize:json_struct_to_block/1` calls it. Vendored from
   `src/ar_wallet.erl:331` with `decoded_address_to_checksum/1`, substituting
   `hb_util:decode/1` for `ar_util:decode/1`.

Confirmed equivalent, so left alone: HyperBEAM's `to_address/2` for
`{rsa, 65537}` and `{ecdsa, secp256k1}` both reduce to
`crypto:hash(sha256, PubKey)`, which is upstream's `hash_pub_key/1`; and
`verify/3` for RSA is `rsa_pss:verify(Data, sha256, Sig, ...)`, matching
upstream exactly. The 21-block signature verification below is the evidence.

### `hb_util` substitutes for `ar_util` in this section

`ar_util` is vendored (see the *Utilities* section). This section predates that and uses `hb_util:encode/1`,
`hb_util:decode/1` and `hb_util:safe_decode/1` for `ar_util:encode/1`,
`ar_util:decode/1` and `ar_util:safe_decode/1`. Both are URL-safe base64
without padding, and the 21-block JSON comparison below is a byte-level proof
of equivalence over roughly a thousand encoded values.

Note one behavioural difference that does not bite here but will elsewhere:
`hb_util:decode/1` is the *unchecked* decoder — malformed input yields garbage
rather than reliably raising, where `ar_util:decode/1` (b64fast) raises. Any
code path that decodes untrusted base64 and relies on an exception must use
`hb_util:safe_decode/1` and check the result.

### Verification run

Harnesses live in the session scratchpad (`verify_vendor.erl`,
`verify_mutate.erl`, `verify_index.erl`, `verify_chain.erl`); fixtures are
`test/fixtures/arweave/`. Everything below is real mainnet data.

**1. Binary round trip, `indep_hash`, block signature, and JSON, over 17
fixtures.** For each: `binary_to_block/1` parses; `block_to_binary/1` of the
result is byte-identical to the bytes the node served;
`indep_hash2(generate_signed_hash(B), B#block.signature) == B#block.indep_hash`;
`verify_signature/3` passes — a full RSA-PSS check against the block producer's
key, which also proves `get_block_signature_preimage/4`, the new
`ar_wallet:to_address/1` clause and `ar_wallet:verify/3`; and
`block_to_json_struct/1` agrees with the node's own JSON key for key, in both
directions, with nested objects compared structurally.

```
height    parse    bin==    indep_hash sig      json       notes
----------------------------------------------------------------------------------------
1974239   ok       ok       ok         ok       ok         51 json keys
1974240   ok       ok       ok         ok       ok         51 json keys
1974849   ok       ok       ok         ok       ok         51 json keys
1974850   ok       ok       ok         ok       ok         51 json keys
1974859   ok       ok       ok         ok       ok         51 json keys
1974860   ok       ok       ok         ok       ok         48 json keys
1974870   ok       ok       ok         ok       ok         51 json keys
1974871   ok       ok       ok         ok       ok         51 json keys
1974872   ok       ok       ok         ok       ok         51 json keys
1974873   ok       ok       ok         ok       ok         51 json keys
1974874   ok       ok       ok         ok       ok         51 json keys
1974875   ok       ok       ok         ok       ok         51 json keys
1974876   ok       ok       ok         ok       ok         51 json keys
1974877   ok       ok       ok         ok       ok         51 json keys
1974878   ok       ok       ok         ok       ok         51 json keys
1974879   ok       ok       ok         ok       ok         51 json keys
1974880   ok       ok       ok         ok       ok         51 json keys
----------------------------------------------------------------------------------------
17/17 blocks fully verified
```

1974860 is the one-chunk solution: 48 keys rather than 51 because
`recall_byte2`, `chunk2_hash` and `unpacked_chunk2_hash` are all absent. The
`poa2 == #poa{}` path round-trips.

**2. Four more blocks fetched independently, including the fork 2.9 block
itself** (`http://tip-1.arweave.xyz:1984/block2/height/<H>` and `/block/height/<H>`):

```
height    parse    bin==    indep_hash sig      json       notes
----------------------------------------------------------------------------------------
1602350   ok       ok       ok         ok       ok         49 json keys
1602400   ok       ok       ok         ok       ok         49 json keys
1975000   ok       ok       ok         ok       ok         51 json keys
1975040   ok       ok       ok         ok       ok         51 json keys
----------------------------------------------------------------------------------------
4/4 blocks fully verified
```

21 mainnet blocks in total, spanning the 2.9 fork block and heights ~373k
blocks later.

**3. The three `nonce_limiter_info` JSON renames, asserted directly.** For
every fixture: the JSON has no `steps` key; `checkpoints` equals the record's
`steps`; `last_step_checkpoints` matches; `zone_upper_bound` and
`next_zone_upper_bound` equal `partition_upper_bound` and
`next_partition_upper_bound`.

```
height    steps==chkp lsc==lsc    zone==pub nzone==np n(steps)  n(lsc)
1974239   ok          ok          ok        ok        35        25
1974240   ok          ok          ok        ok        361       25
1974849   ok          ok          ok        ok        81        25
1974850   ok          ok          ok        ok        270       25
1974859   ok          ok          ok        ok        43        25
1974860   ok          ok          ok        ok        383       25
1974870   ok          ok          ok        ok        98        25
1974871   ok          ok          ok        ok        239       25
1974872   ok          ok          ok        ok        46        25
1974873   ok          ok          ok        ok        105       25
1974874   ok          ok          ok        ok        95        25
1974875   ok          ok          ok        ok        59        25
1974876   ok          ok          ok        ok        2         25
1974877   ok          ok          ok        ok        291       25
1974878   ok          ok          ok        ok        165       25
1974879   ok          ok          ok        ok        18        25
1974880   ok          ok          ok        ok        149       25
```

`last_step_checkpoints` is 25 everywhere, as `?VDF_CHECKPOINT_COUNT_IN_STEP`
requires.

**4. Mutation tests — the checks are not vacuous.** Each mutant must be
rejected by *both* `indep_hash` and the signature:

```
  signature: last byte flipped                         indep_hash=false sig=false rejected
  reward_addr: last byte flipped                       indep_hash=false sig=false rejected
  previous_cumulative_diff + 1                         indep_hash=false sig=false rejected
  cumulative_diff + 1                                  indep_hash=false sig=false rejected
  previous_solution_hash: last byte flipped            indep_hash=false sig=false rejected
  nonce_limiter_info.partition_upper_bound + 1         indep_hash=false sig=false rejected
  nonce_limiter_info.steps: drop the tail              indep_hash=false sig=false rejected
  nonce_limiter_info.vdf_difficulty + 1                indep_hash=false sig=false rejected
  replica_format 1 -> 0                                indep_hash=false sig=false rejected
  packing_difficulty 10 -> 9                           indep_hash=false sig=false rejected
  chunk_hash: last byte flipped                        indep_hash=false sig=false rejected
  poa.data_path: last byte flipped                     indep_hash=false sig=false rejected

== validate_replica_format/3 ==
  replica_format=1, difficulty=10, at 2.9              true ok
  replica_format=1, difficulty=10, one below 2.9       false ok
  replica_format=1, difficulty=9,  at 2.9              false ok
  replica_format=0, difficulty=0 (spora_2_6), live     true ok
  replica_format=0, difficulty=1 (composite), live     false ok
```

**5. Block index, against a real ranged index**
(`/block_index2/1974860/1974880`, 21 entries). `get_block_bounds/2` is compared
against an independent oracle that reproduces the ETS ordered_set seek by
brute-force scan, at both the last byte and the first byte of every block:

```
block_index_to_binary(binary_to_block_index(Bin)) == Bin : ok
all 12 in-range fixture blocks' {indep_hash, weave_size, tx_root} match the index : ok

idx       block_start      block_end        probe offset     result
1         389381016822006  389381016822006  389381016822005  ok
2         389381016822006  389381018919158  389381018919157  ok
3         389381018919158  389381018919158  389381018919157  ok
...
20        389381290762486  389381315141878  389381315141877  ok

height lookups round-trip: ok
member/2 for every hash: ok
member/2 for an absent hash: ok
get_range/3 is largest-height-first: ok
empty/0 answers not_found: ok
ar_block:get_block_bounds/3 defers to the index: ok
ar_block:get_block_bounds/3 rejects an out-of-range recall byte: ok

ALL INDEX CHECKS PASSED
```

Rows 1, 3, 4 and 16 have `block_start == block_end` — zero-transaction blocks.
Those rows are what caught the `from_list/1` overwrite bug described above.

**6. Chained `(PrevB, B)` header checks** over the consecutive fixture run,
including the difficulty retarget at 1974850 and the VDF-difficulty retarget at
1974240:

```
height    timestamp  last_retarget cumul_diff  hl_merkle      prev_sol_h  next_vdf_diff
1974240   ok         ok            ok          ok             ok          n/a
1974850   ok         ok            ok          ok             ok          ok
1974860   ok         ok            ok          ok             ok          ok
1974871   ok         ok            ok          ok             ok          ok
1974872   ok         ok            ok          ok             ok          ok
1974873   ok         ok            ok          ok             ok          ok
1974874   ok         ok            ok          ok             ok          ok
1974875   ok         ok            ok          ok             ok          ok
1974876   ok         ok            ok          ok             ok          ok
1974877   ok         ok            ok          ok             ok          ok
1974878   ok         ok            ok          ok             ok          ok
1974879   ok         ok            ok          ok             ok          ok
1974880   ok         ok            ok          ok             ok          ok
13/13 consecutive pairs fully verified
```

`hl_merkle` is `verify_block_hash_list_merkle/2` — a full
`ar_unbalanced_merkle:root/3` recomputation of `hash_list_merkle` from the
previous block's triplet. `next_vdf_diff` is `n/a` at 1974240 because
`compute_next_vdf_difficulty/1` reads `PrevB#block.block_time_history` at a
retarget height, and that history is carried state that is never gossiped in
the header — a wire-parsed `PrevB` cannot supply it. That is upstream's design,
not a port defect, but it is a real constraint on the sync path: the block-time
history must be maintained forward from the bootstrap checkpoint.

### Not covered by the evidence above

- **`ar_poa:validate/1` end to end.** It needs `ar_packing_server:unpack/5` and
  `unpack_sub_chunk/5` (RandomX), and it needs `{BlockStart, BlockEnd, TXRoot}`
  for a recall byte that can point anywhere in the weave — which needs the
  block index from genesis, not the 21-entry range used above. The merkle
  wiring beneath it (`ar_merkle:validate_path/4,5`) is covered by the
  `ar_merkle` section.
- **`ar_block:compute_h0/5,6`, `compute_h1/3`, `compute_h2/3`,
  `get_recall_range/5`, `get_recall_byte/3`, `get_max_nonce/1`,
  `get_sub_chunk_index/2`.** Vendored verbatim; H0 needs RandomX.
- **`json_struct_to_block/1`** — pre-2.6 only, and there are no pre-2.6 blocks
  in the fixture set. See the note above; this is an upstream limitation.
- **`encode_tx/1` / full-`#tx{}` blocks** — deliberately not vendored.
- **A durable regression test.** The four harnesses are scratchpad scripts. The
  fixtures they run against are in-tree under `test/fixtures/arweave/`, so promoting
  them to an EUnit suite is mechanical and should happen before merge.

---

## Utilities

| Module | Upstream path | LoC upstream → here | Diff lines vs upstream |
|---|---|---|---|
| `ar_util.erl` | `src/ar_util.erl` | 657 → 675 | 99 (4 hunks) |

Vendored **in full**, not as the measured subset, so that
`diff -u <upstream>/apps/arweave/src/ar_util.erl src/core/lib/arweave/ar_util.erl`
stays the upgrade workflow. Compiles clean — zero errors, zero warnings — with
`erlc -I src/core -o <dir> src/core/lib/arweave/ar_util.erl`.

Live callers today — nine call sites, all inside this directory:

| Function | Sites |
|---|---|
| `between/3` | `ar_difficulty:scale_diff/3`; `ar_pricing_transition:get_transition_price/2`; five in `ar_retarget` (`calculate_difficulty/4`, `_at_2_4/4`, `_at_and_after_1_9_before_2_4/4` ×2, `_after_1_8_before_1_9/4`) |
| `floor_int/2` | `ar_replica_2_9:get_entropy_bucket_start/1` |
| `encode/1` | `ar_rewards:log_reward_history/3` |

Until this file existed every one of those was an `undefined function` at run
time, not at compile time — the modules compile because Erlang resolves remote
calls late. `decode/1`, `unique/1`, `take_every_nth/2` and `format_peer/1` have
no live caller yet; they are referenced only from `%% VENDOR:` notes recording
where other modules routed around the missing module (see the last table).

### Deviations

**1. Include path.** `-include("ar.hrl")` → `-include("include/ar.hrl")`.
`rebar.config` sets `{i, "src/core"}`.

**2. `encode/1`.** Upstream is `b64fast:encode(Bin)`. HyperBEAM has no b64fast
NIF; its base64url is `hb_util:encode/1` over the b64veryfast NIF, so `encode/1`
delegates there. Equivalence is proved below, not assumed.

**3. `decode/1`.** Upstream is `b64fast:decode(Input)`, which **raises** on
invalid input — `safe_decode/1` is built on exactly that, catching the raise to
return `{error, invalid}`. `hb_util:decode/1` is the *unchecked* b64veryfast
decoder and returns garbage rather than raising for some malformed inputs (26
divergences in 40 000 fuzzed inputs, below), which would quietly turn
`safe_decode/1` into a function that can never report `{error, invalid}`. So
`decode/1` uses the **checked** decoder underneath `hb_util`,
`b64veryfast:decode64_url/1`, wrapped in `iolist_to_binary/1` to stand in for
the iolist clause `hb_util:decode/1` has and the raw NIF does not
(`iolist_to_binary/1` returns a binary argument unchanged). On every *valid*
base64url input the two agree byte-for-byte, so nothing on a consensus path
sees a difference; the difference is confined to rejecting malformed input.

**4. `parse_peer_test/0` dropped.** It drives `parse_peer/2` through
`ar_test_inet_mock`, a module of the upstream test harness that is not
vendored. `parse_peer/1,2` themselves are untouched.

**5. `between_test/0` added** (not upstream). See below.

Nothing else is trimmed. In particular the peer/port helpers (`parse_peer/1,2`,
`parse_port/1`, `peer_to_str/1`, `peer_to_ip/1`, `format_peer/1`,
`safe_parse_peer/1,2`) are kept: they need only `inet` and
`?DEFAULT_HTTP_IFACE_PORT`, both of which HyperBEAM has. There is no `#config{}`,
`ar_events` or `prometheus_*` dependency anywhere in this module.

`genesis_wallets/0` is kept verbatim and is **dead at run time**: it reads
`genesis_data/genesis_wallets.csv`, which HyperBEAM does not ship. It has no
caller. Kept rather than trimmed so the file stays diffable.

### `include/ar.hrl` additions required by this module

Four constants, copied verbatim from upstream `include/ar.hrl` and placed in
upstream's own order inside the fenced `%%% VENDOR:` section:

| Macro | Upstream `ar.hrl` line | Value |
|---|---|---|
| `?TX_DATA_SIZE_LIMIT` | 160 | `10 * ?MiB` |
| `?BLOCK_TX_DATA_SIZE_LIMIT` | 165 | `?TX_DATA_SIZE_LIMIT` |
| `?TX_SIZE_BASE` | 175 | `3210` |
| `?DEFAULT_HTTP_IFACE_PORT` | 306 | `1984` |

`?DEFAULT_HTTP_IFACE_PORT` is what `ar_util` itself needs. The other three
resolve standing TODOs elsewhere in this file, under one rule:

> **A constant that upstream's `include/ar.hrl` owns is vendored into
> `src/core/include/ar.hrl`. A constant upstream keeps module-local stays
> module-local here.**

Consequences, which **supersede the notes in the sections above**:

- The local `-define(BLOCK_TX_DATA_SIZE_LIMIT, 10 * ?MiB)` in
  `ar_tx_replay_pool.erl` is **removed**. It had to go in the same change: a
  bare `-define` of a macro the header now carries is a redefinition *error*.
  This supersedes deviation 2 of the "Account state modules" section, and the
  "deliberately still absent … adding it here is an error" note at the end of
  "Pure consensus modules".
- `?TX_SIZE_BASE` is now available to `ar_pricing`'s `get_tx_fee/4` /
  `get_storage_cost/4` and to `ar_tx:utility/1` when they are vendored. The
  `%% VENDOR:` note at `ar_pricing.erl:30` saying `ar.hrl` "does not yet define"
  it is now stale.
- `?DEPRIORITIZE_V1_TX_SIZE_THRESHOLD` is **not** added. It is not in upstream's
  `ar.hrl` — upstream defines it module-locally at `src/ar_tx.erl:21`. It
  belongs in `ar_tx.erl` here too, when `ar_tx:utility/1` is vendored.

Nothing in `ar.hrl` was removed, reordered or re-valued; the file is still a
pure superset of `git show HEAD:src/core/include/ar.hrl`.

### Verification run

**Compilation, and no regression in any other module.** Every `.erl` under
`src/core` (106 modules) was compiled twice — once against `ar.hrl` without
these four macros, once against the current one — and the two diagnostic logs
diffed. The *only* difference is 16 diagnostic lines disappearing:
`ar_tx_replay_pool` resolving `?BLOCK_TX_DATA_SIZE_LIMIT`. **Zero new
diagnostics.**

**The `AR_HRL` include guard is load-bearing.** A probe module including
`hb.hrl`, `ar_inflation.hrl` and `ar_poa.hrl` (all three pull in `ar.hrl`)
compiles clean. With the guard stripped, the same probe fails on
`redefining macro 'DATA_CHUNK_SIZE'` and every macro after it.

**base64url — byte-for-byte equivalence.** `hb_util:encode/1` and this module's
`decode/1` were compared against an independent implementation of RFC 4648 §5
unpadded base64url (OTP's own `base64:encode/2` / `decode/2` with
`#{mode => urlsafe, padding => false}`), which is exactly what upstream's
b64fast NIF produces and consumes. 4195 inputs: every size from 0 to 200 bytes
(so every padding residue, many times), 20 random samples each, plus 32-, 48-,
64-, 65-, 256-, 512-, 1024- and 4096-byte cases, plus hand-picked edges
(`<<>>`, single bytes, all-zero and all-`0xFF` runs, and inputs whose last
sextets force the `-`/`_` alphabet).

```
samples:                4195
encode mismatches:      0
round-trip mismatches:  0
hb_decode(ref_encode):  0 mismatches
ref_decode(hb_encode):  0 mismatches
padding/alphabet leaks: 0
length-law violations:  0
RESULT: PASS
negative control (padded encoder vs ours): AQIDBA== vs AQIDBA -> differ=true
standard-alphabet control +/++ vs urlsafe -_--
```

Both cross directions are checked, not just the round trip: a round trip alone
passes for any self-consistent codec. The two negative controls confirm the
harness can see the two classic divergences — padding, and the `+/` versus `-_`
alphabet.

**The checked/unchecked decoder gap** (why deviation 3 exists). 40 000 fuzzed
inputs — 20 000 random bytes, 20 000 random strings over the base64url alphabet
at every length — comparing `hb_util:decode/1` against
`b64veryfast:decode64_url/1`: **26 inputs where the unchecked decoder returns a
value (typically an `0xFF` run) and the checked one raises `badarg`.** With
`decode/1` on the checked decoder, `ar_util:safe_decode(<<"!!!!">>)` returns
`{error, invalid}` and `ar_util:safe_decode(ar_util:encode(<<1,2,3>>))` returns
`{ok, <<1,2,3>>}`, both as upstream.

**`between/3` and the `infinity` upper bound.** `between_test/0` pins the
ordinary three-way clamp, asserts the Erlang term-order premise directly
(`0 < infinity`, `(1 bsl 256) < infinity`, `not (0 > infinity)`), and asserts
that `between(N, 0, infinity)` returns `N` — never the atom — across
`[0, 1, 42, 1000000, 1 bsl 64, 1 bsl 256]`, and that the *lower* clamp still
bites under an `infinity` upper bound.

It is mutation-tested. Three single-token mutants of `between/3` were compiled
and run against it:

| Mutant | Result |
|---|---|
| upper clause `N > Max` → `N < Max` | killed |
| lower clause `N < Min` → `N > Min` | killed |
| lower clause `N < Min` → `Min < N` | killed |

The whole module's suite passes, upstream's own tests included:

```
$ erl -pa <ebin> -pa <ar_util ebin> -noshell -eval 'eunit:test(ar_util, [verbose]), halt().'
  ar_util: between_test...ok
  ar_util: basic_unique_test...ok
  ar_util: basic_peer_format_test...ok
  ar_util: pick_random_test...ok
  ar_util: round_trip_encode_test...[0.016 s] ok
  ar_util: pmap_test...[0.302 s] ok
  ar_util: encode_list_indices_test...ok
  All 7 tests passed.
```

**The real call sites, driven end to end** against the compiled tree. The one
that matters is the first: `get_transition_price/2` runs at a height where
`transition_upper_bound/1` is `infinity`, and a `1 bsl 64` V2 price comes back
as an integer rather than being clamped to the atom.

```
transition_start_2_7_2 = 1551470, length = 518400, probe height = 1810670
  get_transition_price(1810670, 0)                    -> 170                    integer=true
  get_transition_price(1810670, 1000)                 -> 670                    integer=true
  get_transition_price(1810670, 100000000)            -> 50000170               integer=true
  get_transition_price(1810670, 18446744073709551616) -> 9223372036854775978    integer=true
  scale_diff(1000, {100,1}, 1602350)                  -> 1146341683...998343547 integer=true
  scale_diff(1 bsl 255, {1,100}, 1602350)             -> 8684406692...847229952 integer=true
  calculate_difficulty(.., height=50000)              -> 999999                 integer=true
  calculate_difficulty(.., height=300000)             -> 1157912058...483330048 integer=true
  calculate_difficulty(.., height=1602350)            -> 8684406692...847229952 integer=true
  ar_replica_2_9:get_entropy_partition(1 bsl 40)      -> 0
  ar_util:floor_int(262145, 262144)                   -> 262144
  ar_util:encode(<<1,2,3>>)                           -> <<"AQID">>
  ar_util:format_peer({127,0,0,1})                    -> "127.0.0.1:1984"
  ar_util:take_every_nth(3, lists:seq(1,10))          -> [1,4,7,10]
  ar_util:unique([a,a,b,c,b])                         -> [a,b,c]
  ar_util:safe_decode(<<"!!!!">>)                     -> {error,invalid}
  ar_util:between(5, 0, infinity)                     -> 5
```

`calculate_difficulty/5` is driven at six heights so that every historical
clause — pre-1.8, 1.8–1.9, 1.9–2.4, 2.4, 2.5 and post-2.5 — is exercised;
each returns an integer, so no `between/3` call anywhere in the ladder leaks a
bound.

### Duplicate work now redundant

`ar_util` not existing led four other vendored modules to route around it. None
is wrong, and none is touched here, but each is now avoidable:

| Site | Work-around |
|---|---|
| `ar_merkle.erl:155` | `ar_util:floor_int/2` → `hb_util:floor_int/2` (same expression) |
| `ar_merkle.erl:55` | `?LOG_ERROR([… ar_util:encode(ID) …])` → `?event({… hb_util:encode(ID) …})` |
| `ar_poa.erl:131` | `ar_util:format_peer/1` dropped from a log term |
| `ar_node_utils.erl:84` | `ar_util:encode(indep_hash)` dropped from a log term |
| `ar_wallet.erl:380` | `ar_util:decode/1` → `hb_util:decode/1` |

---

## `ar_tx.erl` — the L1 consensus interface

`ar_tx.erl` is a HyperBEAM fork, not a reduced upstream subset (see "`ar_tx.erl`
and `ar_wallet.erl` have DIVERGED from upstream" above). The four functions
`ar_tx_replay_pool` needs were absent, and their absence was a runtime `undef`
rather than a compile error. They are now ported **additively** from upstream
`src/ar_tx.erl` — no existing function was moved, reformatted or changed:

```erlang
ar_tx:verify(TX, Args)                     -> boolean().   % upstream :99
ar_tx:verify(TX, Args, VerifySignature)    -> boolean().   % upstream :108
    %% Args :: {Rate, PricePerGiBMinute, KryderPlusRateMultiplier, Denomination,
    %%          RedenominationHeight, Height, Accounts, Timestamp}
    %% VerifySignature :: verify_signature | do_not_verify_signature
ar_tx:check_last_tx(Wallets, TX)           -> boolean().   % upstream :152
ar_tx:get_addresses(TXs)                   -> [Address].   % upstream :217
ar_tx:utility(TX)                          -> {1 | 2, Denomination, Reward}.  % :238
ar_tx:get_tx_fee(Args)                     -> Winston.     % upstream :629
ar_tx:get_tx_fee2(Args)                    -> Winston.     % upstream :653
```

`get_tx_fee/1,2` are exported because upstream exports them and upstream's
`is_tx_fee_sufficient/1` reaches `get_tx_fee/1` module-qualified.

The ported code sits in two fenced `%%% VENDOR:` blocks — public entry points at
the end of the "Public interface" section, their private helpers at the end of
the "Private functions" section — each in upstream's own relative order, with
upstream's tabs and comments. The pre-existing HyperBEAM code above them is
byte-for-byte unchanged.

### `last_tx` → `anchor`

Upstream's `#tx.last_tx` is spelled `#tx.anchor` in HyperBEAM's `ar.hrl`. Same
field, same `<<>>` default. The rename is applied at **3 sites** in the ported
code, matching `ar_tx_replay_pool`'s existing approach exactly:

| Function | Upstream line |
|---|---|
| `check_last_tx/2` (both account-tuple clauses) | `:160`, `:162` |
| `tx_field_size_limit_v1/3` | `:485` |
| `tx_field_size_limit_v2/3` | `:702` |

No other ported function reads the field.

### Deviations

1. **Both `-ifdef(AR_TEST)` clauses are dropped; only the `-else.` branches are
   vendored.** Upstream's `verify/3` short-circuits `#tx{ signature = <<>> }` to
   `true` and its `check_last_tx/2` short-circuits `TX#tx.owner == <<>>` to
   `true`. A validator that accepts unsigned transactions verifies nothing.
2. **`format` must be an integer.** HyperBEAM's `#tx.format` defaults to the
   atom `ans104`, which upstream has no notion of. Guard clauses on `verify/3`
   and `utility/1` raise `{unexpected_tx_format, Format}` for any non-integer
   format. Without them the failure is silent: upstream's `do_verify/3` would
   answer `false` — indistinguishable from a genuinely invalid transaction —
   and `utility/1` would drop an `ans104` item into the `{2, _, _}`
   "not deprioritized" bucket. This is the same hardening
   the `lib_arweave_block` bridge applies, repeated at the
   module boundary.
3. **`collect_validation_results/2` and `verify_hash/1` are not re-ported** —
   HyperBEAM's fork already has both. Its `collect_validation_results/2` reports
   failures through `?event` instead of `ar_tx_db:put_error_codes/2` (`ar_tx_db`
   is a node-local ets error-code store, not vendored, not consensus); the
   boolean result is identical. Its `verify_hash/1` is
   `ID == generate_id(TX, signed)`, and `generate_id(TX, signed)` is
   `crypto:hash(sha256, TX#tx.signature)` — the same expression as upstream's
   `ID == crypto:hash(?HASH_ALG, Sig)`. `tags_to_binary/1` is likewise already
   present and byte-identical.
4. **`verify_signature_v1/2` and `verify_signature_v2/2` are not ported.**
   Upstream reaches them only from `verify_tx_id/2`; HyperBEAM's `verify_tx_id/2`
   is a fork that goes through its own `verify_signature/1`, so both arity-2
   functions would be dead code here.
5. **`?DEPRIORITIZE_V1_TX_SIZE_THRESHOLD` is defined module-locally**, with
   upstream's comment, exactly as upstream does at `src/ar_tx.erl:21`.
   `?TX_SIZE_BASE`, `?TX_DATA_SIZE_LIMIT` come from `include/ar.hrl`;
   `?STATIC_2_6_8_FEE_WINSTON` and `?NEW_ACCOUNT_FEE_DATA_SIZE_EQUIVALENT` come
   from a newly added `-include("include/ar_pricing.hrl")`, which upstream's
   `ar_tx.erl` also includes. `hb.hrl` already pulls in `ar.hrl`, so that is the
   only include added.

All post-2.5 arithmetic is exact integer: `get_tx_fee/1` → `get_tx_fee2/1` →
`ar_pricing:get_tx_fee/1`, and `ar_pricing:redenominate/3` on both sides of the
fee comparison. No float appears anywhere in the ported chain.

### Required from modules owned elsewhere

`ar_wallet:verify_pre_fork_2_4/3` (upstream `src/ar_wallet.erl:287`,
`rsa_pss:verify_legacy/4` — which HyperBEAM *does* have, at
`src/core/lib/rsa_pss.erl`) is **not exported by HyperBEAM's `ar_wallet` fork**.
The pre-fork-2.4 branch of `verify_signature_v1/3` and `verify_signature_v2/3`
therefore raises `undef` rather than verifying. That is the correct failure mode
— a validator that does not implement pre-2.4 signatures must not silently check
them with the post-2.4 scheme — and it is unreachable for every height this tree
validates. The calls are left exactly as upstream wrote them, so restoring the
function in `ar_wallet` is all that is needed.

### Verification run

Compiled per-file, no new diagnostics. The single warning is pre-existing, in
code this work did not touch (`sign_v1/2` is unused because HyperBEAM's fork
does not export it):

```
$ erlc -I src/core -o /tmp/artx-check src/core/lib/arweave/ar_tx.erl
src/core/lib/arweave/ar_tx.erl:63:1: Warning: function sign_v1/2 is unused
```

Everything below is real mainnet data from `test/fixtures/arweave/`, run against
the built tree with the freshly compiled module ahead of it on the path
(`code:which(ar_tx)` = `/tmp/artx-check/ar_tx.beam`). Arguments are sourced
exactly as `ar_node_utils:validate_block(txs, ...)` sources them: pricing,
denomination and redenomination height from the *previous* block, `Height` =
`B#block.height - 1`, timestamp from the block being validated.

**1. `verify/3` over every transaction of every fixture block whose predecessor
is also on disk**, each also with a single flipped signature bit:

```
block     txs    accepted  rejected
----------------------------------------
1974240   17     17        17
1974850   4      4         4
1974860   18     18        18
1974871   26     26        26
1974872   3      3         3
1974880   65     65        65
1975040   0      0         0
----------------------------------------
total     133    133       133
```

133/133 real transactions accepted; 133/133 single-bit signature mutations
rejected. (The mutation flips the low bit of the first signature byte, so both
`verify_hash/1` and `verify_signature_v2/3` fail — the id is the hash of the
original signature.)

**2. Non-vacuity — every check in `do_verify_v2/3` is live.** One mutation per
check on a real transaction from 1974880, run with `do_not_verify_signature` so
the signature and hash checks stay green and cannot mask the check under test
(no field mutation disturbs `verify_hash/1`, since the id is the hash of the
signature):

```
  (baseline, unmutated)           -> true  (expected true)
  quantity_negative               -> false (expected false)
  same_owner_as_target            -> false (expected false)
  tx_too_cheap                    -> false (expected false)
  tx_fields_too_large (anchor)    -> false (expected false)
  tx_fields_too_large (data_root) -> false (expected false)
  tx_id_not_valid                 -> false (expected false)
  overspend                       -> false (expected false)
  tx_data_size_negative           -> false (expected false)
  tx_data_size_data_root_mismatch -> false (expected false)
  invalid_target_length           -> false (expected false)
  invalid_denomination            -> false (expected false)
```

**3. The `anchor` rename is the field actually being read.** `anchor` is capped
at 48 bytes by `tx_field_size_limit_v2/3`; had the port read some other field,
an oversized anchor would sail through:

```
  real 48-byte anchor    (48 bytes) -> verify/3 do_not_verify_signature = true
  random 48-byte anchor  (48 bytes) -> verify/3 do_not_verify_signature = true
  random 49-byte anchor  (49 bytes) -> verify/3 do_not_verify_signature = false
  empty anchor           (0 bytes)  -> verify/3 do_not_verify_signature = true
```

**4. The fee arithmetic reproduces mainnet to the winston.** `get_tx_fee/1` at
height 1974880 against the rewards those transactions actually paid — five of
the first six are exact:

```
height 1974879, price_per_gib_minute 4897, kryder_plus_rate_multiplier 1
tx                                            weave increase  min fee       reward paid
rbG7Jzn2xGmFljoJYtWvAkWVICxo5guDwyfdEuGK2ns   1835008         18460518314   18460518314
a8qFNnDRnA0NrVUIqRebacwozy_vqo542oajSXccZJQ   1835008         18460518314   18460518314
2dz9n-i6NmTL_SC8eJTrQ6qF1tbYuF5y_kYsNffvwqA   1572864         15827906669   15843734576
__iXkCGG9wHnkC4sLoTbyXbl-F4sOKlJSbnatk0jTeI   1572864         15827906669   15827906669
hIHb2MiZmN_pge0XSdPSkUxrLSYHEGkpdCNnLiQOFdg   1310720         13195295024   13195295024
R5E0DjqytNHUbaKJ8RfQqeTjw4Clgqx8GKyP6zD3D-k   786432          7930071734    7930071734
```

**5. `do_verify_v1/3`.** No fixture block carries a `format = 1` transaction —
all 133 are v2 — so the v1 path is exercised with a locally signed one at the
same height. (`ar_tx:sign/2` dispatches on `TX#tx.format`, so a `format = 1`
record is v1-signed; `sign_v1/2` itself is not exported by HyperBEAM's fork.)

```
  format=1, signed, funded            -> verify=true
  same tx, anchor grown to 49 bytes   -> verify=false
  same tx, flipped signature byte     -> verify=false
  same tx, no account (overspend)     -> verify=false
```

**6. `get_addresses/1`** over block 1974880's 65 transactions, against the
independently computed union of `ar_wallet:to_address(owner, sig_type)` and
`target`:

```
  65 transactions -> 4 distinct senders, 1 distinct recipient value
  senders:    FPjbN_btYKzcf8QASjs30v5C0FPv7XpwKXENBW8dqVw
              OJBx0djSiBDU_JZefh13QdQUnCc6B017BQg0Q6EVo-Y
              2VbQF5jwjs7WAHM4Bq_NoMbYtRZb1CBGYEaFouSZhnY
              6DTqSgzXVErOuLhaP0fmAjqF4yzXkvth58asTxP3pNw
  recipients: <<>>   (no transfers in this block)
  get_addresses/1 returned 5 addresses; matches the union: true
  get_addresses([]) = []
  a cached owner_address is used rather than recomputed: true
```

**7. `utility/1`** on a real transaction and at the deprioritization boundary:

```
  real v2 tx (format=2, data_size=1584631, reward=18460518314, denom=0)
                                              -> {2,0,18460518314}
  buckets across all 65 fixture txs           -> [2]
  same tx as format=1, data_size=1584631      -> {1,0,18460518314}
  format=1, data_size=100 (== threshold)      -> {2,0,18460518314}
  format=1, data_size=101 (>  threshold)      -> {1,0,18460518314}
  format=2, denomination=3                    -> {2,3,18460518314}
  utility(denom=3) > utility(denom=0)         -> true
  utility(v2)      > utility(large v1)        -> true
```

**8. `check_last_tx/2`**, both account-tuple arities, on a real transaction and
then swept across the block:

```
  empty wallet list                                -> true
  wallet list without the sender                   -> false
  2-tuple account whose last_tx == tx anchor       -> true
  2-tuple account whose last_tx =/= tx anchor      -> false
  4-tuple account whose last_tx == tx anchor       -> true
  4-tuple account whose last_tx =/= tx anchor      -> false
  52/65 fixture txs anchor to their own wallet entry
  65/65 fail once every stored last_tx is perturbed by one bit
```

(52 rather than 65 because the four senders issue several transactions each and
the map holds one `last_tx` per address; the perturbed control is what shows
the comparison is live.)

**9. The `ans104` guard.**

```
  default #tx{} format = ans104
  ar_tx:verify/3  -> {'EXIT',{{unexpected_tx_format,ans104}, [{ar_tx,verify,3,…}]}}
  ar_tx:utility/1 -> {'EXIT',{{unexpected_tx_format,ans104}, [{ar_tx,utility,1,…}]}}
```

**10. End to end through `ar_tx_replay_pool`** — the real caller of all four
functions, block 1974880:

```
  wallet-anchored (check_last_tx/2 true), no block anchors -> valid
  wallet last_tx randomised, anchor in block anchors       -> valid
  wallet last_tx randomised, no matching block anchor      -> {invalid,tx_bad_anchor}
  verify_block_txs/1 over all 65 transactions              -> valid
  same, with one flipped signature byte                    -> invalid
  pick_txs_to_mine/1 (sorts by ar_tx:utility/1)            -> 65 of 65 picked
  picked utilities are in descending order                 -> true
```

The second line is `dev_arweave_tx`'s `block_anchor_test` in one call:
`check_last_tx/2` answers false, so `verify_block_anchor/2` has to find the
transaction's `anchor` among the recent block hashes.

### Not verified

- **`dev_arweave_tx`'s own eunit suite was not run.** Preloaded devices are
  packaged by the Forge and resolved out of the store, which needs a `rebar3`
  build; the build was under embargo for this change (several agents sharing the
  worktree). What is shown instead is the same call chain those tests take,
  driven directly on the same fixtures. The `undef` cause is gone: all four
  functions exist and are exported.
- **The pre-fork-2.4 signature branches** — see "Required from modules owned
  elsewhere" above. They raise `undef`; that was not exercised.
- The harness's `ar_wallet:new()` call makes the BEAM exit 139 on `halt/0` about
  two runs in three. That is a pre-existing teardown crash in the wallet pool,
  reproducible with `erl -eval 'ar_wallet:new(), halt(0).'` alone, and it
  happens strictly after every assertion has run and `ALL CHECKS PASSED` has
  printed.

## `ar_tx.erl` — the transaction field-shape predicates

`~arweave-tx@2.9/verify` previously answered six questions about a transaction
and upstream's `ar_tx:verify/3` answers ten. The four it did not ask —
`verify_denomination/4`, `tx_field_size_limit_v1/3`, `tx_field_size_limit_v2/3`,
`verify_target_length/2` and `verify_malleability/1` — were already vendored
into this file, byte-for-byte apart from the `last_tx` → `anchor` rename the
section header above describes, but upstream keeps them private and so did the
port.

### The whole change is five export entries

```erlang
-export([verify_denomination/4, verify_target_length/2, verify_malleability/1]).
-export([tx_field_size_limit_v1/3, tx_field_size_limit_v2/3]).
```

Nothing else in `ar_tx.erl` moved. Upstream keeps these private because its
only caller — `do_verify_v1/3` and `do_verify_v2/3` — reduces them to one
boolean through `collect_validation_results/2`. The device names the failing
check to its caller instead, so it needs to call them one at a time. Both
callers now exist and both get the same answers from the same code.

`verify_malleability/1` keeps upstream's six-tuple argument
(`{TX, PricePerGiBMinute, KryderPlusRateMultiplier, Denomination, Height,
Accounts}`) rather than being flattened into an arity-6 function, so a diff
against `apps/arweave/src/ar_tx.erl` still lines up.

### The height convention at the boundary

Upstream's validators take the height of the *parent* block; `~arweave-tx@2.9`
takes the height of the block the transaction is included in, and converts
once. Every call from the device therefore passes `Height - 1`, which is what
`ar_node_utils:validate_block(txs, ...)` and
`dev_arweave_block:check_txs/5` (`Next#block.height - 1`) pass upstream. The
same convention already governed the device's format gates and its fee call,
where `is_tx_fee_sufficient/1`'s `Height + 1` is the device's `Height`.

`verify_denomination/4`'s fourth argument is the parent block's
`redenomination_height`. The device reads it from the new
`block-redenomination-height` key, defaulting to `0` — the same default
`#block.redenomination_height` carries in `include/ar.hrl`, meaning no
redenomination has been scheduled.

### `get_tx_fee/1` is now used, and the device's inline copy is gone

`dev_arweave_tx:minimum_fee/6` was an inlined copy of the one branch of
upstream's `ar_tx:get_tx_fee/1` that post-2.9 heights can reach, carrying a
`VENDOR:` note asking for its own deletion once `ar_tx` exported the real
thing. `ar_tx` exports it now, so the inline is deleted and
`dev_arweave_tx:sufficient_fee/6` calls `ar_tx:get_tx_fee/1` directly. The
device is strictly more faithful for it: the inline asserted the pre-static-
pricing branch was unreachable, where `get_tx_fee/1` actually implements it.

### What is still not checked — the replay family

Upstream's block validator rejects a transaction whose identifier is already on
the weave (`ar_tx_replay_pool:tx_already_in_weave/2`), already in the mempool
(`tx_already_in_mempool/2`), or whose anchor names a mempool transaction
(`last_tx_in_mempool/2`). None is a question about the transaction in hand;
each is a question about a set of *other* transactions, and a mempool is a
declared non-goal of this tree. They are **not** ported and must not be
mistaken for an oversight: the boundary is stated in the doc comment on
`dev_arweave_tx:verify/3` (`NOT CHECKED HERE, deliberately: ...`), so a reader
of the source sees it without reading this file. `~arweave-block@2.9/check-txs`
supplies the rule for block validation by folding
`ar_tx_replay_pool:verify_block_txs/1` over the block, which is where the
cumulative-balance rule lives too.

Two further single-transaction checks upstream applies were absent from the
device when the section above was written — `same_owner_as_target`
(`apps/arweave/src/ar_tx.erl:411,453`) and `tx_data_size_negative` (`:460`).
Both are now present; the subsection below records them. Nothing else upstream
asks of a transaction in isolation is missing.

### Verification run

`HB_PORT=8821 rebar3 device test --devices dev_arweave_tx` — 22 tests, all
passing. The evidence that matters:

**1. No regression over real transactions.** `verify_test` verifies all 26
transactions of mainnet block 1974871 as published. A wider one-off harness
(`all_fixture_blocks_verify_harness.erl` in the session scratchpad) ran the
same check over every fixture block that has transactions, pricing each from
its *parent* block as upstream does, and asserted the exact per-block result:

```
[{1974239,1,[]}, {1974240,17,[]}, {1974849,15,[]}, {1974850,4,[]},
 {1974859,1,[]}, {1974860,18,[]}, {1974870,23,[]}, {1974871,26,[]},
 {1974872,3,[]}, {1974879,3,[]}, {1974880,65,[]}, {1975039,1,[]}]
```

177 mainnet transactions, no rejections. The 1974849 → 1974850 pair is the one
in the fixture set where `price_per_gib_minute` moves (4901 → 4897), so the
parent-pricing convention is exercised rather than assumed.

**2. Each check has a failing mutant, and each mutant carries a valid
signature.** A mutated fixture transaction is worthless as evidence here — the
signature check catches it first, and would have caught it before this change
too. Every field these checks guard is *covered by the signature*, so the
transactions upstream rejects and this device used to accept are correctly
signed ones. Three of the four mutants are therefore signed with a fresh key
inside the test; the fourth needs no mutation at all.

| Check | Error | Mutant |
|---|---|---|
| `verify_denomination/4` | `invalid-denomination` | an **unaltered** mainnet transaction, offered to a block whose `redenomination-height` is its own height; plus `denomination = 2` against a block denomination of 1 |
| `tx_field_size_limit_v1/3`, `_v2/3` | `invalid-field-size` | a signed transaction with a 64-byte anchor (limit 48 since fork 1.8) |
| `verify_target_length/2` | `invalid-target-length` | a signed transaction with a 33-byte target |
| `verify_malleability/1` | `malleable-transaction` | a signed **format 1** transaction with `quantity = 0` and a 32-byte target — the concatenated v1 preimage does not distinguish it from a targetless one. No fixture block contains a format 1 transaction, so it is signed in the test, and its targetless twin is asserted to verify. |

**3. The mutants are live.** With the four checks skipped in `first_failure/1`
and nothing else changed, all four tests fail and every one of them fails the
same way — `{value,{ok,true}}` against the expected error. The device accepted
each of these four transactions before this change and rejects them now; the
other 18 tests were unaffected:

```
arweave-tx@2.9: invalid_denomination_test...*failed*
              {expected,{error,<<"invalid-denomination">>}},
              {value,{ok,true}}]}
arweave-tx@2.9: oversized_field_test...*failed*
              {expected,{error,<<"invalid-field-size">>}},
              {value,{ok,true}}]}
arweave-tx@2.9: invalid_target_length_test...*failed*
              {expected,{error,<<"invalid-target-length">>}},
              {value,{ok,true}}]}
arweave-tx@2.9: malleable_v1_test...*failed*
              {expected,{error,<<"malleable-transaction">>}},
              {value,{ok,true}}]}
  Failed: 4.  Skipped: 0.  Passed: 18.
```

### Not covered by the evidence above

- Every fixture transaction is format 2 with no explicit denomination, so the
  `tx_field_size_limit_v1/3` and `verify_malleability/1` paths are exercised
  only by transactions signed inside the test, never by mainnet data.
- `verify_malleability2/1`'s truncated-fee branch is reached by the format 1
  control (a 10^12 reward truncates to 0, which is insufficient, so the check
  passes) but no test makes that branch *reject*; the two target/quantity
  clauses and `ends_with_digit/1` are what the mutant exercises.
- The pre-fork-1.8 32-byte anchor limit, the pre-fork-2.5 tag-size rule and the
  pre-fork-2.6 21-digit limit are all vendored and all unreached: no fixture
  block is that old, and the device's height converts to a post-2.9 parent.

### `same_owner_as_target` and `tx_data_size_negative` — the last two

Nothing in `ar_tx.erl` changed for these. Both are one expression over a
`#tx{}` field the record already exposes, so they are written in the device as
`not_self_targeted/1` and `valid_data_size/1` rather than called across the
boundary:

```erlang
not_self_targeted(TX) -> TX#tx.owner_address =/= TX#tx.target.
valid_data_size(TX)   -> TX#tx.data_size >= 0.
```

`TX#tx.owner_address` is read directly rather than through
`ar_tx:get_owner_address/1`, and that is deliberate. `lib_arweave_tx:to_tx/2`
fills the field with `ar_wallet:to_address(Owner, KeyType)` on every conversion,
which is upstream's value for every owner. `get_owner_address/1` does not:
HyperBEAM's fork carries a first clause answering the atom `not_set` for an
owner of 512 zero bytes (`ar_tx.erl:309`), which is a well-formed RSA owner
rather than a sentinel. Since a peer chooses that field and the encoder raises
on the atom, every consumer in the block subsystem reads the record field --
`valid_anchor/3`, `accounts/3`, `sufficient_balance/3`, and `balances/5`.

`verify/3` now runs twelve checks. The two arrivals take their upstream
neighbours' positions:

| # | Error | Upstream |
|---|---|---|
| 1 | `invalid-format` | the v1/v2 dispatch, not a listed check |
| 2 | `invalid-denomination` | `verify_denomination/4`, the gate on the list |
| 3 | **`self-targeted-transaction`** | **`same_owner_as_target`, second in both lists** |
| 4 | `invalid-field-size` | `tx_fields_too_large` |
| 5 | `invalid-target-length` | `invalid_target_length` |
| 6 | **`negative-data-size`** | **`tx_data_size_negative`, immediately before the next** |
| 7 | `invalid-data-root` | `tx_data_size_data_root_mismatch` |
| 8 | `invalid-signature` | `tx_id_not_valid` + `tx_signature_not_valid` |
| 9 | `invalid-anchor` | `last_tx_not_valid` |
| 10 | `insufficient-balance` | `overspend` + `quantity_negative` |
| 11 | `invalid-fee` | `tx_too_cheap` |
| 12 | `malleable-transaction` | `tx_malleable` |

The device's own order is cheapest-first, which is not upstream's — upstream
runs every check and collects the failures, so its order carries no meaning
beyond the reading order of the list. Where upstream's order *is* load-bearing
it is preserved: `tx_data_size_negative` sits immediately before
`tx_data_size_data_root_mismatch` there, and `negative-data-size` sits
immediately before `invalid-data-root` here, because the pair is a pair.

**Why the mismatch check cannot absorb the negative one.**
`(TX#tx.data_size == 0) == (TX#tx.data_root == <<>>)` asks whether an empty
size and an empty root agree. A transaction claiming a negative `data_size`
under a real 32-byte root is a pair of non-empties, `false == false`, and
passes — as does the device's `valid_data_root/1`, whose first clause is that
same expression. Upstream does not fold the sign into the pairing either; it
adds a second check, and so does this.

**Why the balance check cannot absorb the self-target one.** A transfer to
oneself debits and credits the same account, so `ar_node_utils:apply_tx/3`
nets to a solvent balance for any quantity the sender can afford. Nothing else
in `verify/3` can see it: the target is a well formed 32-byte address, so
`verify_target_length/2` is content, and it is covered by the signature, so a
signer may produce one freely.

### Verification run

`HB_PORT=8831 rebar3 device test --devices dev_arweave_tx` — 24 tests, all
passing (22 before, plus `self_targeted_test` and `negative_data_size_test`).

**1. No regression over real transactions.** The one-off harness described in
the section above was re-run, widened from 12 fixture blocks to all 18 that
have transactions, still pricing each block from its *parent*. It is a
temporary 25th test in the two runs quoted below, and is not in the tree:

```
TMP-REGRESSION total=251
[{1974239,1,[]}, {1974240,17,[]}, {1974849,15,[]}, {1974850,4,[]},
 {1974859,1,[]}, {1974860,18,[]}, {1974870,23,[]}, {1974871,26,[]},
 {1974872,3,[]}, {1974873,7,[]}, {1974874,4,[]}, {1974875,5,[]},
 {1974876,0,[]}, {1974877,26,[]}, {1974878,32,[]}, {1974879,3,[]},
 {1974880,65,[]}, {1975039,1,[]}]
```

251 mainnet transactions, no rejections — the 177 the earlier run covered plus
74 more. The list is identical with the two new checks skipped and with them
live, so neither costs a real transaction. The 1974849 → 1974850 pair still
moves `price_per_gib_minute` (4901 → 4897), so parent-pricing is exercised.

**2. Each check has a failing mutant, and each mutant carries a valid
signature.** Both fields are covered by the signature, so mutating a fixture
transaction proves nothing — `invalid-signature` would catch it either way.
Both mutants are therefore signed with a fresh `ar_wallet:new()` inside the
test, and each is paired with a control that differs in exactly one thing and
is asserted to verify:

| Check | Error | Control (verifies) | Mutant (rejected) |
|---|---|---|---|
| `same_owner_as_target` | `self-targeted-transaction` | signed `#tx{format = 2}` with a random 32-byte target | the same, with `target = ar_wallet:to_address(Wallet)` |
| `tx_data_size_negative` | `negative-data-size` | signed `#tx{format = 2}`, `data_size = 262144` under a random 32-byte `data_root` | the same, with `data_size = -262144` — only the sign differs |

**3. The mutants are live.** With the two checks skipped in `first_failure/1`
and nothing else changed, both tests fail, and both fail on the mutant
assertion with `{value,{ok,true}}` — the control assertion above it having
already passed. The device accepted both of these signed transactions before
this change and rejects them now. Every other test was unaffected, the
251-transaction harness among them (it is the 23rd pass below), and it
returned the same all-empty list:

```
arweave-tx@2.9: self_targeted_test...*failed*
              {expression,"verify_result ( Signed ( ar_wallet : to_address ( Wallet ) ) , # { } , Opts )"},
              {expected,{error,<<"self-targeted-transaction">>}},
              {value,{ok,true}}]}
arweave-tx@2.9: negative_data_size_test...*failed*
              {expression,"verify_result ( Signed ( - 262144 ) , # { } , Opts )"},
              {expected,{error,<<"negative-data-size">>}},
              {value,{ok,true}}]}
  Failed: 2.  Skipped: 0.  Passed: 23.
```

### Not covered by the evidence above

- No mainnet transaction in the fixture set is self-targeted or carries a
  negative `data_size` — that is the point, but it means both checks are
  exercised only by transactions signed inside the test.
- `not_self_targeted/1` is asked of format 1 transactions too, as upstream asks
  it in `do_verify_v1/3`, but no test signs a self-targeted format 1
  transaction; a v1 mutant would stop at `malleable-transaction` anyway when
  its quantity is zero.
- `valid_data_size/1` is unreachable for a transaction that carries its data:
  `valid_data_root/1`'s later clauses compare `data_size` against
  `byte_size(data)`, which is never negative. Only the dataless format 2 path
  needs it, and only that path is tested.
