%%% @doc Checkpoint bootstrap and real-time Arweave validation for
%%% `~arweave@2.9'.
%%%
%%% This module fetches peer data, persists chain state in `hb_cache', and
%%% delegates each consensus rule to the device that owns it. It is pull-only:
%%% it does not gossip, mine, or serve the Arweave peer protocol.
%%%
%%% == Trust boundary ==
%%%
%%% Bootstrap is a standard trusted-state join. An explicit
%%% `arweave-checkpoint-block' trusts the complete state carried at that block;
%%% without one, `arweave-trusted-peers' choose a shared ancestor near the tip.
%%% The checkpoint's identity and committed roots are checked, but transitions
%%% before it are not replayed. This is the same fundamental trust boundary as
%%% an Arweave node joining from trusted peers. Every block after the checkpoint
%%% is fully validated locally, including transactions and the exact account
%%% root, unless the operator explicitly disables `arweave-require-accounts'.
%%%
%%% The account tree must be the selected checkpoint's own tree. Bootstrap
%%% fails if peers have already pruned it; it never substitutes state from a
%%% different block.
%%%
%%% == Operator setup ==
%%%
%%% Configure a persistent `store', `arweave-untrusted-peers', and either an
%%% explicit checkpoint block or `arweave-trusted-peers'. Call
%%% `GET /~arweave@2.9/bootstrap' once, then schedule
%%% `GET /~arweave@2.9/sync' with `~cron@1.0', for example every 30 seconds.
%%% `tip' returns the selected local chain head; `validated' returns only state
%%% this node produced and never falls back to a peer.
%%%
%%% `arweave-max-vdf-workers' is the node-wide ceiling on native VDF workers.
%%% A request may ask for fewer with `arweave-vdf-threads', never more.
%%%
%%% The explicit live integration vector hydrates a recent checkpoint into an
%%% ignored `_build' store and validates a transaction-bearing mainnet block.
%%% A cold bootstrap fetches a 155 MiB block index and the whole account tree,
%%% which outlasts the default EUnit timeout on anything but an idle machine:
%%%
%%% <pre>
%%% rebar3 device test --devices dev_arweave \
%%%   --test all:live_account_transition --timeout 5400
%%% </pre>
%%%
%%% == Store layout ==
%%%
%%% A validated block is filed under its own Arweave block hash, and that is the
%%% whole of the index: the message holds the header, the components the checks
%%% produced, and a link to the block below it, so the chain is a linked list
%%% reachable as `tip/previous/previous'. `~arweave@2.9/tip' points at the
%%% selected head, `~arweave@2.9/placements/&lt;txid&gt;' at the current
%%% placement of a transaction, `~arweave@2.9/settled/&lt;indep-hash&gt;'
%%% marks a block whose transactions have been announced, and
%%% `~arweave@2.9/accounts-anchor' records the checkpoint account tree.
%%% The store is the only durable state, so sync resumes after restart.
%%%
%%% A block's presence under its hash is the completion marker. Publication
%%% writes transactions, placements, offsets and components first and links the
%%% hash last, so a block that reads back is a block whose local indexes are
%%% finished. Nothing else records progress, and a partial pass leaves nothing
%%% to undo.
-module(dev_arweave_sync).
-export([bootstrap/3, sync/3, tip/3, validated/3, backfill/3, placement/3]).
%%% Exported so that `dev_arweave' can refuse on `block' the shapes this module
%%% refuses on `validated'. One definition, because two would drift.
-export([block_hash/1]).
-ifdef(TEST).
-export([
    account_anchor/3,
    adopt/2,
    above_fork/1,
    block_index/4,
    block_key/1,
    branch_point/4,
    candidate_hashes/1,
    choose/3,
    cumulative_diff/2,
    decode_rewards/2,
    decode_times/2,
    do_backfill/2,
    do_bootstrap/2,
    do_sync/2,
    eligible/2,
    eligible_height/1,
    forced/2,
    height_hash/3,
    histories/4,
    identified_block/3,
    incumbent/1,
    int/3,
    network_height/2,
    parent/2,
    peer_block/3,
    peers/1,
    publish/4,
    ranges/3,
    rotate/2,
    settle/2,
    settled_path/1,
    shared_ancestor/3,
    stored_height/1,
    transactions/3,
    trusted_peers/2,
    unbootstrapped/2,
    validated_hash/2,
    wallet_list/6
]).
-endif.
-include("include/hb.hrl").

%%% Consensus constants come from the vendored tree, never from a local
%%% redefinition: `?CHECKPOINT_DEPTH' -- the reorganisation depth beyond which
%%% a branch can never take the tip, and the only finality rule Arweave has --
%%% arrives via `ar.hrl', and the 2.9 fork height via `ar_fork:height_2_9/0'.
%%% A checkpoint below that fork anchors on blocks whose proofs this node
%%% cannot verify in full, so it is refused.

%%% Peer protocol constants, fixed by the peer rather than chosen here.

%% Entries per ranged `/block_index2' request. Each entry is ~90 bytes on the
%% wire, so a page is ~450 KiB -- large enough that the round trip is not the
%% cost, small enough that a lost page is cheap to lose.
-define(BLOCK_INDEX_PAGE_SIZE, 5000).

%% The most `/wallet_list' pages a bootstrap will read. Unlike the block index,
%% which is bounded by height, this walk ends only when a peer says it has
%% ended -- so a bound belongs here. See `wallet_list/5'.
-define(MAX_WALLET_LIST_PAGES, 1000).

%%% Defaults for the node-message options this device reads.

%% Keep the checkpoint inside peers' short carried-history retention window,
%% with enough room for the network tip to advance during bootstrap.
-define(DEFAULT_CHECKPOINT_DEPTH, 30).
-define(DEFAULT_SYNC_BATCH, 50).
-define(DEFAULT_BACKFILL_BATCH, 50).
-define(DEFAULT_SETTLE_BATCH, 50).
-define(DEFAULT_PEER_WORKERS, 8).
-define(DEFAULT_PEER_TIMEOUT, 60000).
-define(DEFAULT_PEER_CONNECT_TIMEOUT, 10000).
%% Approximate CPU-seconds used only for the pre-bootstrap cost estimate.
-define(VDF_SECONDS_PER_BLOCK, 130).

%%% The device that owns the block rules, and the hook a settled transaction is
%%% announced on.

-define(BLOCK_DEVICE, <<"arweave-block@2.9">>).
-define(SETTLED_HOOK, <<"arweave-settled-transaction">>).

%%% Every durable name this device writes is built by `lib_arweave_paths', so
%%% that the block device -- which writes a block's `previous' link -- and this
%%% one, which follows it, cannot come to disagree about the store layout.

%% @doc Establish the node's initial chain state.
%%
%% This is the only point at which the node adopts state it did not derive.
%% The selected block's identity and committed roots are checked, but its
%% carried consensus and account state is the trusted starting point; earlier
%% transitions are not replayed.
%%
%% The checkpoint comes from `arweave-checkpoint-block' when the operator names
%% one. Otherwise it is the block every trusted peer agrees on at
%% `arweave-checkpoint-depth' below the network tip -- the shared ancestor.
bootstrap(_Base, Req, Opts) ->
    exclusive(bootstrap, fun() -> do_bootstrap(Req, Opts) end, Opts).

%% @doc Ingest blocks from peers on top of the locally cached chain.
%%
%% Idempotent and resumable by construction rather than by bookkeeping: a block
%% whose chain state is already stored is neither fetched nor applied, so a
%% pass interrupted anywhere leaves a consistent tree and the next pass
%% continues from the block after the last one it finished. Intended to be
%% driven by `~cron@1.0/every'.
%%
%% Each pass advances at most `arweave-sync-batch' blocks, walking back from
%% each peer's target to the deepest block it already has, applying what is
%% missing ancestor-first, and re-running fork choice over the heads it built.
sync(_Base, Req, Opts) ->
    exclusive(sync, fun() -> do_sync(Req, Opts) end, Opts).

%% @doc Materialise blocks below the ones this node holds, checked against the
%% block index its selected tip carries. See `do_backfill/2'.
backfill(_Base, Req, Opts) ->
    exclusive(backfill, fun() -> do_backfill(Req, Opts) end, Opts).

%% @doc Return the block at the tip of the heaviest eligible branch.
%%
%% `candidates' is a list of branch-head block hashes to weigh against the
%% stored tip; `sync' passes the heads it has just validated. It defaults to
%% `[]', so a bare `tip' has nothing to weigh and reports the stored tip
%% unchanged. Fork choice is read-only -- `sync' is what moves the pointer.
tip(_Base, Req, Opts) ->
    maybe
        {ok, Incumbent} ?= incumbent(Opts),
        % Candidates come from a request, and each becomes a store path. They
        % are checked here for the same reason `block_hash/1' exists.
        {ok, Candidates} ?=
            candidate_hashes(hb_maps:get(<<"candidates">>, Req, [], Opts)),
        Winner = choose(Incumbent, Candidates, Opts),
        % Not `hb_cache:read/2' directly: on a miss that answers `{error,
        % not_found}', a bare atom, which `dev_meta:embed_status/2' renders as
        % HTTP 400 with an underscored atom as the body. Every other error this
        % device returns is a `#{status, message, detail}' message.
        read_block(Winner, <<"no-tip">>,
            <<"The stored tip does not resolve to a block.">>, Opts)
    end.

%% @doc Return the block this node validated, named by `indep-hash', by height,
%% or as `current'.
%%
%% This key never contacts a peer, and that is the whole of its purpose.
%% `~arweave@2.9/block' answers from a gateway on a cache miss, so a block this
%% node verified and a block it was simply handed arrive looking the same. A
%% block that reaches a caller through this key was validated here, by this
%% node, under the checks its `validation/checks' names -- or it does not
%% arrive at all.
%%
%% The absence of a fallback is the feature. `not-validated' means "this node
%% has not verified that block", which is a different and more useful answer
%% than a block fetched from elsewhere to fill the gap.
validated(_Base, Req, Opts) ->
    maybe
        {ok, Hash} ?= validated_hash(Req, Opts),
        read_block(Hash, <<"not-validated">>,
            <<"This node has not validated that block. Blocks are not fetched "
                "from peers here: only blocks this node verified are served.">>,
            Opts)
    end.

%% @doc Return the placement this node holds for a transaction: which block
%% included it, where in that block, and where its bytes begin in the weave.
%%
%% A placement is what this node last saw; it is not a claim that the block
%% carrying it is still selected. A caller that needs one checks the placement's
%% `block' against the hash the tip's block index records at its `height'. This
%% key deliberately does not perform that check on the caller's behalf: the
%% answer depends on what the caller needs it for, and the data to make it is
%% right here.
placement(_Base, Req, Opts) ->
    maybe
        {ok, ID} ?= transaction_id(Req, Opts),
        case hb_cache:read(lib_arweave_paths:placement(ID), Opts) of
            {ok, Placement} ->
                {ok, Placement};
            _ ->
                {error, error_message(404, <<"no-placement">>,
                    <<"This node holds no placement for that transaction.">>)}
        end
    end.

%% @doc Accept only something that is actually a transaction identifier, for
%% the same reason `block_hash/1' exists: the value becomes a store path.
transaction_id(Req, Opts) ->
    case hb_maps:get(<<"tx">>, Req, not_found, Opts) of
        ID when is_binary(ID), byte_size(ID) == 43 ->
            case is_base64url(ID) of
                true -> {ok, ID};
                false -> invalid_transaction()
            end;
        _ ->
            invalid_transaction()
    end.

%% @doc Refuse a value that is not a transaction identifier.
invalid_transaction() ->
    {error,
        error_message(
            400,
            <<"invalid-transaction">>,
            <<"`tx' must be a base64url transaction identifier of 32 bytes.">>
        )
    }.

%% @doc Resolve the `block' request parameter to an `indep-hash'. `current' is
%% the stored tip; anything else must be a block hash.
%%
%% Naming a block by height would be the friendlier interface, and it is
%% deliberately absent: resolving one means reading the tip's block index, and
%% the synthetic chain these tests build carries no index, so the path could not
%% be pinned by a test. An unpinned lookup in a device whose answers are
%% supposed to mean "this node verified it" is worth less than no lookup.
validated_hash(Req, Opts) ->
    case hb_maps:get(<<"block">>, Req, <<"current">>, Opts) of
        <<"current">> ->
            incumbent(Opts);
        Block ->
            block_hash(Block)
    end.

%% @doc Accept only something that is actually a block identifier.
%%
%% The value is caller-supplied and becomes a store path, so it is checked
%% before it names anything. Requiring it to be a 48-byte base64url hash leaves
%% no separator for a path to be built out of.
block_hash(Hash) when is_binary(Hash) ->
    % The alphabet check is the one that matters, and decoding does not
    % perform it: `hb_util:decode/1' is the *unchecked* decoder, so any 64
    % characters yield 48 bytes. A length test alone is not a check.
    case is_base64url(Hash) andalso byte_size(Hash) == 64 of
        true ->
            {ok, Hash};
        false ->
            invalid_block()
    end;
block_hash(_NotABinary) ->
    invalid_block().

%% @doc Every candidate must be a block hash before any of them names a path.
candidate_hashes(Candidates) when is_list(Candidates) ->
    lists:foldl(
        fun
            (_Hash, {error, _} = Error) -> Error;
            (Hash, {ok, Acc}) ->
                case block_hash(Hash) of
                    {ok, Checked} -> {ok, Acc ++ [Checked]};
                    Error -> Error
                end
        end,
        {ok, []},
        Candidates
    );
candidate_hashes(_NotAList) ->
    invalid_block().

%% @doc Whether every byte is in the base64url alphabet. A block hash is 48
%% bytes encoded, so 64 characters drawn from `A-Za-z0-9-_' and nothing else.
is_base64url(Bin) ->
    lists:all(
        fun(C) ->
            (C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z)
                orelse (C >= $0 andalso C =< $9)
                orelse C == $- orelse C == $_
        end,
        binary_to_list(Bin)
    ).

invalid_block() ->
    {error,
        error_message(
            400,
            <<"invalid-block">>,
            <<"`block' must be a base64url block hash of 48 bytes, or "
                "`current'.">>
        )
    }.

%%% Bootstrap.

%% @doc Fetch, verify and record everything a chain state is made of.
%%
%% The order is not arbitrary. The two carried histories are perishable: peers
%% serve them only while the checkpoint is still in their block cache, and the
%% chain keeps moving while a bootstrap runs. The block index is not perishable
%% -- genesis does not age -- but it is 155 MiB and half a minute of paging.
%% Spending that half-minute of the histories' remaining life before asking for
%% them is how a bootstrap that would have worked returns HTTP 404 instead.
do_bootstrap(Req, Opts) ->
    maybe
        ok ?= unbootstrapped(forced(Req, Opts), stored_height(Opts)),
        {ok, Peers} ?= peers(Opts),
        {ok, Trusted} ?= trusted_peers(Req, Opts),
        {ok, TipHeight} ?= network_height(Trusted, Opts),
        {ok, Hash} ?= checkpoint(Trusted, TipHeight, Req, Opts),
        {ok, Block} ?= identified_block(Peers, Hash, Opts),
        Height = int(<<"height">>, Block, Opts),
        ok ?= above_fork(Height),
        ok = report_cost(Height, TipHeight, Opts),
        {ok, Rewards, Times} ?= histories(Peers, Block, Hash, Opts),
        {ok, RewardHistory} ?=
            stored_history(<<"reward-history">>, Rewards, Height, Opts),
        {ok, TimeHistory} ?=
            stored_history(<<"block-time-history">>, Times, Height, Opts),
        ok ?= account_anchor(Peers, Block, Opts),
        {ok, Index} ?= block_index(Peers, Block, Height, Opts),
        ok ?= anchor_window(Peers, Index, Height, Opts),
        % The checkpoint goes through `with_anchor/3' like any other block, and
        % for the same reason. The anchor probe starts at the checkpoint, so in
        % the default configuration the tree it finds belongs to this very
        % block -- and if it were only attached to blocks `sync' applies later,
        % it would never be attached at all, leaving the account checks
        % silently disabled on a node that had fetched and verified the tree.
        {ok, ID} ?=
            publish(
                with_anchor(
                    checkpoint_block(
                        Block, Index, RewardHistory, TimeHistory, Opts),
                    Hash,
                    Opts
                ),
                Hash,
                [],
                Opts
            ),
        hb_cache:link(block_key(Hash), lib_arweave_paths:tip(), Opts),
        record_peers(
            hb_opts:get(arweave_untrusted_peers, [], Opts),
            Peers,
            Opts
        ),
        ?event(arweave_sync_short,
            {bootstrapped,
                {indep_hash, {string, Hash}},
                {height, Height},
                {network_height, TipHeight}
            },
            Opts
        ),
        hb_cache:read(ID, Opts)
    end.

%% @doc The block message the checkpoint is stored as.
%%
%% The checks it names are the ones a trusted-state join establishes: the
%% header's own identity, recomputed by `identified_block/3', and each carried
%% component against the hash this very header commits to it under -- the block
%% index against `hash-list-merkle', the two histories against their history
%% hashes, and, once `with_anchor/3' has attached it, the account tree against
%% `wallet-list'. What the list does not name is everything that needs a
%% parent. That is the join's trust boundary, stated in the record rather than
%% only in a comment.
%%
%% `transactions' is empty because a join does not fetch the checkpoint's
%% transactions: nothing checked them, so nothing was placed. The list says so
%% the same way the checks do -- by what is missing from it.
checkpoint_block(Block, Index, Rewards, Times, Opts) ->
    Block#{
        <<"device">> => ?BLOCK_DEVICE,
        <<"previous">> => previous_link(Block, Opts),
        <<"transactions">> => [],
        <<"block-index">> => Index,
        <<"accounts">> => [],
        <<"reward-history">> => Rewards,
        <<"block-time-history">> => Times,
        <<"validation">> =>
            #{
                <<"checks">> =>
                    [
                        <<"identity">>,
                        <<"block-index">>,
                        <<"reward-history">>,
                        <<"block-time-history">>
                    ]
            }
    }.

%% @doc Link the block below one this device is storing, by the Arweave block
%% hash that names it. The target need not be present; it becomes traversable
%% if `backfill' materialises it.
previous_link(Block, Opts) ->
    {link,
        lib_arweave_paths:block(
            hb_maps:get(<<"previous-block">>, Block, <<>>, Opts)),
        #{ <<"type">> => <<"link">>, <<"lazy">> => false }
    }.

%% @doc Refuse a bootstrap on a node that already has a chain.
%%
%% `bootstrap' is the one moment this node trusts anything, and it is reachable
%% over HTTP by anyone the node answers. Left unguarded it is a repeatable trust
%% reset: every call re-asks the peer set where the chain starts and relinks the
%% tip onto their answer, discarding what the node had validated for itself.
%%
%% Checked before any peer is contacted, because a refusal should not cost the
%% network round trips it is refusing to make.
%%
%% Forcing it stays available: a node stranded behind a chain it can no longer
%% close the gap to needs a way to re-anchor, and its checkpoint may have aged
%% out of what peers will serve. What forcing is not is something that happens
%% by accident.
unbootstrapped(_Forced, []) ->
    ok;
unbootstrapped(true, _Stored) ->
    ok;
unbootstrapped(_Forced, _Stored) ->
    {error, error_message(<<"already-bootstrapped">>,
        <<"This node has a validated chain. Bootstrapping again would "
            "discard it and re-trust the peer set; `sync` extends it instead. "
            "Set the node option `arweave-force-bootstrap` to re-anchor "
            "deliberately.">>)}.

%% @doc Whether to re-anchor over a chain this node has already validated.
%%
%% Read from the **node message only**, never from the request. `bootstrap' is
%% reachable by anyone the node answers, and `force' discards a validated chain
%% and re-trusts the peer set -- so honouring it from the request makes the one
%% guarded trust boundary in the subsystem unguarded again, for any caller who
%% can reach the port. An operator who wants it sets
%% `arweave-force-bootstrap' in the node message, which a remote caller cannot
%% reach.
forced(_Req, Opts) ->
    hb_util:atom(hb_opts:get(arweave_force_bootstrap, false, Opts)).

%% @doc The height of the chain this node has already validated for itself, or
%% `[]' when it has none.
stored_height(Opts) ->
    case incumbent(Opts) of
        {ok, Hash} -> height(Hash, Opts);
        _ -> []
    end.

%% @doc The peers whose agreement may establish a checkpoint.
%%
%% The distinction the brief draws between the two peer lists exists for exactly
%% this call and no other. A shared-ancestor bootstrap believes what these peers
%% agree on; everything afterwards is checked against the block they named, so
%% it may be fetched from anywhere. Collapsing the two lists here would let a
%% peer discovered during a previous bootstrap decide where the next one
%% anchors, which is the one thing the untrusted list is not for.
%%
%% A node given a checkpoint block outright trusts no peer at all, so it needs
%% no trusted list: the hash is the trust root, and the shared-ancestor walk
%% that would have needed one never runs.
trusted_peers(_Req, Opts) ->
    trusted_peers(
        hb_opts:get(arweave_trusted_peers, [], Opts),
        configured(arweave_checkpoint_block, Opts),
        Opts
    ).
trusted_peers([], [], _Opts) ->
    {error, error_message(<<"no-trusted-peers">>,
        <<"A shared-ancestor bootstrap decides where the chain anchors from "
            "what the peers agree on. Set `arweave-trusted-peers`, or name an "
            "`arweave-checkpoint-block` and trust no peer at all.">>)};
trusted_peers([], _Checkpoint, Opts) ->
    peers(Opts);
trusted_peers(Trusted, _Checkpoint, _Opts) ->
    {ok, hb_util:unique(Trusted)}.

%% @doc Choose the block every fetched byte will be verified against.
%%
%% An operator-supplied `arweave-checkpoint-block' is used as given: it is the
%% one input this device does not derive from a peer, and the whole point of
%% the mode. Otherwise the checkpoint is the shared ancestor -- the deepest
%% block at or below the requested height that every peer in the set reports
%% the same hash for.
checkpoint(Peers, TipHeight, Req, Opts) ->
    case configured(arweave_checkpoint_block, Opts)
    of
        [] ->
            shared_ancestor(
                Peers,
                checkpoint_height(TipHeight, Req, Opts),
                Opts
            );
        Hash ->
            {ok, hb_util:bin(Hash)}
    end.

%% @doc The height a shared-ancestor bootstrap starts looking at: the height
%% the operator named, or `arweave-checkpoint-depth' below the network tip.
checkpoint_height(TipHeight, _Req, Opts) ->
    case configured(arweave_checkpoint_height, Opts) of
        [] ->
            TipHeight - hb_opts:get(
                arweave_checkpoint_depth,
                ?DEFAULT_CHECKPOINT_DEPTH,
                Opts
            );
        Height ->
            hb_util:int(Height)
    end.

%% @doc Walk down from `Height' until every peer reports the same block hash,
%% and take that block as the checkpoint. Disagreement means at least one peer
%% is on a different branch, and a branch point is never deeper than
%% `?CHECKPOINT_DEPTH', so a walk that fails to converge in that many steps is
%% peers disagreeing about settled history rather than a live reorg.
shared_ancestor(Peers, Height, Opts) ->
    shared_ancestor(Peers, Height, ?CHECKPOINT_DEPTH, Opts).
shared_ancestor(_Peers, _Height, 0, _Opts) ->
    {error, error_message(<<"no-shared-ancestor">>,
        <<"The peers do not agree on any block in the reorg window.">>)};
shared_ancestor(Peers, Height, Steps, Opts) ->
    case height_hash(Peers, Height, Opts) of
        {ok, Hash} ->
            % Reported by default, not behind a topic nobody prints: this is
            % the number of independent peers that had to agree before the node
            % trusted the one block its whole chain rests on. Every other part
            % of the trust model is visible in the record; without this line
            % that one is not.
            ?event(arweave_sync_short,
                {shared_ancestor,
                    {height, Height},
                    {indep_hash, {string, Hash}},
                    {peers, length(Peers)}
                },
                Opts
            ),
            {ok, Hash};
        _ ->
            shared_ancestor(Peers, Height - 1, Steps - 1, Opts)
    end.

%% @doc Fetch a block and prove it is the block that was asked for, by
%% recomputing its `indep-hash' rather than reading the one the peer put in the
%% response. Everything bootstrap does afterwards rests on this one check, and
%% every header bootstrap admits goes through it: a header whose identifier is
%% taken on a peer's word is a peer's claim, however it was addressed.
identified_block(Peers, Hash, Opts) ->
    maybe
        {ok, Block} ?= peer_block(Peers, Hash, Opts),
        {ok, Computed} ?=
            hb_ao:resolve(
                Block#{ <<"device">> => <<"arweave-block@2.9">> },
                <<"id">>,
                Opts
            ),
        Recomputed = hb_maps:get(<<"indep-hash">>, Computed, [], Opts),
        ok ?= match_hash(Recomputed, Hash),
        {ok, Block}
    end.

match_hash(Hash, Hash) ->
    ok;
match_hash(_Computed, _Hash) ->
    {error, error_message(<<"invalid-indep-hash">>,
        <<"The block does not hash to the checkpoint.">>)}.

%% @doc Refuse a checkpoint below the 2.9 fork. Blocks below it carry proofs in
%% formats this subsystem does not validate, so anchoring there would give the
%% node a chain it cannot check rather than a longer one it can.
above_fork(Height) ->
    above_fork(Height, ar_fork:height_2_9()).
above_fork(Height, Fork) when Height >= Fork ->
    ok;
above_fork(_Height, _Fork) ->
    {error, error_message(<<"checkpoint-below-fork">>,
        <<"The checkpoint is below the 2.9 fork height.">>)}.

%% @doc Report what validating from the checkpoint to the tip will cost before
%% any of it is spent. The VDF chain is the whole of the estimate because it
%% dominates every other check by orders of magnitude, and it is linear in
%% blocks, so a checkpoint chosen carelessly is not slow but infeasible.
report_cost(Height, TipHeight, Opts) ->
    Blocks = max(0, TipHeight - Height),
    Threads = vdf_threads(Opts),
    % Printed by default: this is the number an operator needs *before* a
    % bootstrap spends hours of their machine, and a cost estimate nobody sees
    % is not an estimate.
    ?event(arweave_sync_short,
        {bootstrap_cost_estimate,
            {blocks, Blocks},
            {vdf_cpu_hours, (Blocks * ?VDF_SECONDS_PER_BLOCK) div 3600},
            {vdf_wall_hours,
                (Blocks * ?VDF_SECONDS_PER_BLOCK) div (3600 * Threads)},
            {threads, Threads}
        },
        Opts
    ),
    ok.

vdf_threads(Opts) ->
    Max =
        max(
            1,
            hb_util:int(
                hb_opts:get(
                    arweave_max_vdf_workers,
                    max(1, erlang:system_info(schedulers) div 2),
                    Opts
                )
            )
        ),
    min(
        Max,
        max(1, hb_util:int(hb_opts:get(arweave_vdf_threads, Max, Opts)))
    ).

%%% The block index.

%% @doc Assemble the block index the checkpoint's chain state carries, and
%% prove all ~2M of its entries against one hash.
%%
%% A block's `hash-list-merkle' is the unbalanced Merkle root over the entries
%% *preceding* it -- it is built by folding the parent's entry into the
%% parent's own root -- so the range that can be checked against the checkpoint
%% is `[0, Height - 1]', not `[0, Height]'. The checkpoint's own entry is then
%% appended, and it needs no separate proof: its three fields are read from the
%% header whose `indep-hash' was recomputed and matched. The resulting index
%% covers `[0, Height]', which is what a chain state at that block must carry,
%% because the next block's recall byte may land anywhere in the weave the
%% checkpoint wrote.
%%
%% Pages are fetched in parallel -- 155 MiB over ~400 requests -- but ingested
%% in order, because each page extends the index the one before it produced.
%% The un-ranged `/block_index' is never used: it answers HTTP 400
%% `not_supported_since_fork_2_6' on mainnet.
block_index(Peers, Block, Height, Opts) ->
    Pages =
        hb_pmap:parallel_map(
            ranges(0, Height - 1, ?BLOCK_INDEX_PAGE_SIZE),
            fun({Start, End}) -> index_page(Peers, Start, End, Opts) end,
            hb_opts:get(arweave_peer_workers, ?DEFAULT_PEER_WORKERS, Opts)
        ),
    maybe
        {ok, Index} ?= ingest_index(Pages, #{}, 0, Opts),
        {ok, _} ?=
            hb_ao:resolve(
                Index,
                #{
                    <<"path">> => <<"verify">>,
                    <<"expected-root">> =>
                        hb_maps:get(<<"hash-list-merkle">>, Block, [], Opts)
                },
                Opts
            ),
        hb_ao:resolve(
            Index,
            #{
                <<"path">> => <<"append">>,
                <<"indep-hash">> =>
                    hb_maps:get(<<"indep-hash">>, Block, [], Opts),
                <<"weave-size">> =>
                    hb_maps:get(<<"weave-size">>, Block, 0, Opts),
                <<"tx-root">> => hb_maps:get(<<"tx-root">>, Block, <<>>, Opts)
            },
            Opts
        )
    end.

%% @doc Split `[Start, End]' into inclusive ranges of at most `Size' entries.
ranges(Start, End, _Size) when Start > End ->
    [];
ranges(Start, End, Size) ->
    [{Start, min(End, Start + Size - 1)} | ranges(Start + Size, End, Size)].

index_page(Peers, Start, End, Opts) ->
    first_peer_with(
        rotate(Peers, Start div ?BLOCK_INDEX_PAGE_SIZE),
        <<
            "/block_index2/",
            (hb_util:bin(Start))/binary,
            "/",
            (hb_util:bin(End))/binary
        >>,
        Opts
    ).

%% @doc Rotate a peer list so that consecutive pages ask different peers first.
%% Every page still falls back through the whole list; this only spreads the
%% first attempt, so that ~400 back-to-back requests for 155 MiB do not all
%% land on whichever peer happens to be listed first.
rotate([], _N) ->
    [];
rotate(Peers, N) ->
    {Head, Tail} = lists:split(N rem length(Peers), Peers),
    Tail ++ Head.

%% @doc Extend the index with each page in turn. `from-binary' rejects a page
%% that does not start where the index ends, so a page lost or reordered in
%% flight is caught by the device rather than splicing a hole into the weave.
ingest_index([], Index, _Entries, _Opts) ->
    {ok, Index};
ingest_index([{ok, Page, _Peer} | Pages], Index, Entries, Opts) ->
    maybe
        {ok, Extended} ?=
            hb_ao:resolve(
                Index#{ <<"device">> => <<"arweave-block-index@2.9">> },
                #{
                    <<"path">> => <<"from-binary">>,
                    <<"body">> => Page,
                    <<"start-height">> => Entries
                },
                Opts
            ),
        ingest_index(
            Pages,
            Extended,
            hb_util:int(hb_maps:get(<<"length">>, Extended, 0, Opts)),
            Opts
        )
    end;
ingest_index([Error | _Pages], _Index, _Entries, _Opts) ->
    Error.

%%% The transaction anchor window.

%% @doc Materialise the blocks below the checkpoint that a transaction may
%% anchor on, so that the chain extends far enough back for the anchor rules to
%% read.
%%
%% A transaction anchors on a block within `get_max_tx_anchor_depth' of the one
%% carrying it, and may not repeat a transaction already inside that window.
%% Both rules walk the chain back from the block being extended, so a node whose
%% chain begins at its checkpoint rejects every block-anchored transaction on
%% the network -- which is to say every real block, as soon as an account tree
%% is present to check transactions against at all. The blocks below the
%% checkpoint are therefore fetched and stored, as blocks, and the walk finds
%% them like any other.
%%
%% They are stored as headers and say so: their identity is established, and
%% nothing else about them is. Mainnet peers refuse the sub-field request that
%% would return a block's transaction list on its own (HTTP 421,
%% `Subfield block querying is disabled'), so a whole header is fetched either
%% way, and storing the whole of it costs nothing over storing a summary of it.
%%
%% A block published as a header is part of the chain and cannot be extended
%% from: it carries no account tree and neither history, so
%% `~arweave-block@2.9/apply' onto one refuses the first component check. The
%% checkpoint is the lowest block this node can build on, which is what its
%% `validation/checks' says and what makes `sync' stop there: `walk_back/5'
%% ends at the deepest *published* block on the branch it is closing, and on
%% the selected chain that is the checkpoint.
%%
%% The identifiers come from the index this bootstrap has already proven
%% against the checkpoint's `hash-list-merkle', and each header is checked
%% against the identifier it was asked for, so the window admits no peer claim:
%% a peer that drops a transaction from a block to let it be replayed changes
%% that block's `indep-hash' and is caught by the identity check.
anchor_window(Peers, Index, Height, Opts) ->
    maybe
        {ok, Hashes} ?= window_hashes(Index, Height, Opts),
        {ok, _Published} ?=
            collect(
                hb_pmap:parallel_map(
                    Hashes,
                    fun(Hash) -> anchor_entry(Peers, Hash, Opts) end,
                    workers(Opts)
                ),
                []
            ),
        ok
    end.

%% @doc The identifiers of the blocks below the checkpoint that fall inside the
%% anchor window, newest first. The window holds exactly the depth an anchor may
%% reach back, counting the checkpoint itself, which is stored separately as the
%% block the chain is anchored on.
window_hashes(Index, Height, Opts) ->
    collect(
        [
            index_hash(Index, At, Opts)
        ||
            At <-
                lists:seq(
                    Height - 1,
                    max(0, Height - ar_block:get_max_tx_anchor_depth() + 1),
                    -1
                )
        ],
        []
    ).

index_hash(Index, Height, Opts) ->
    maybe
        {ok, Entry} ?= index_entry(Index, Height, Opts),
        {ok, hb_maps:get(<<"indep-hash">>, Entry, [], Opts)}
    end.

%% @doc The authenticated triplet the block index records at a height.
index_entry(Index, Height, Opts) ->
    hb_ao:resolve(
        Index#{ <<"device">> => <<"arweave-block-index@2.9">> },
        #{ <<"path">> => <<"at">>, <<"height">> => Height },
        Opts
    ).

%% @doc Fetch one block of the window, check its identity against the hash the
%% authenticated index records for it, and publish it as a header.
anchor_entry(Peers, Hash, Opts) ->
    maybe
        {ok, Header, TXs} ?=
            materialized(
                Peers,
                #{ <<"indep-hash">> => Hash },
                #{},
                <<"headers">>,
                Opts
            ),
        publish(Header, Hash, TXs, Opts)
    end.

%% @doc How many peer requests this node may have in flight at once.
workers(Opts) ->
    hb_opts:get(arweave_peer_workers, ?DEFAULT_PEER_WORKERS, Opts).

%%% The carried histories.

%% @doc Fetch the reward history and the block-time history and check both
%% against the hashes the checkpoint block commits to.
%%
%% Neither can be reconstructed from block headers -- `next-vdf-difficulty' is
%% computed from the block-time history, and the history is carried state that
%% is never gossiped -- so they are fetched once here and maintained forward
%% from then on. What is checked here is the bytes the peer served; turning
%% them into the persistent form a chain state carries belongs to
%% `~arweave-history@2.9', which parses them with the same vendored decoder.
%%
%% The parent is identified rather than merely fetched: its
%% `reward-history-hash' is one of the two values the reward history is checked
%% against, so a header taken on a peer's word would let that peer choose half
%% of what its own history has to match.
histories(Peers, Block, Hash, Opts) ->
    maybe
        {ok, Parent} ?=
            identified_block(
                Peers,
                hb_maps:get(<<"previous-block">>, Block, [], Opts),
                Opts
            ),
        {ok, Rewards} ?= history(Peers, <<"/reward_history/">>, Hash, Opts),
        {ok, Times} ?= history(Peers, <<"/block_time_history/">>, Hash, Opts),
        ok ?= verify_rewards(Rewards, Block, Parent, Opts),
        ok ?= verify_times(Times, Block, Opts),
        {ok, Rewards, Times}
    end.

%% @doc Turn a verified history into the persistent list a chain state carries.
%%
%% The height goes with the bytes because it is what bounds the history's
%% length, and the checkpoint's height is the one the state that holds it will
%% be extended from.
stored_history(Kind, Body, Height, Opts) ->
    hb_ao:resolve(
        #{
            <<"device">> => <<"arweave-history@2.9">>,
            <<"kind">> => Kind,
            <<"height">> => Height
        },
        #{ <<"path">> => <<"from-binary">>, <<"body">> => Body },
        Opts
    ).

%% @doc Fetch one carried history, distinguishing "the peers cannot answer this"
%% from "the peers no longer hold it". They serve a history only while its block
%% is in their cache -- measured at exactly `?STORE_BLOCKS_BEHIND_CURRENT'
%% heights -- so a checkpoint further back than that cannot be bootstrapped at
%% all, however available its header still is. That is a property of the network
%% rather than of this node, and it is worth naming as such: reporting it as an
%% unanswered request sends the reader looking for a peer problem.
history(Peers, Prefix, Hash, Opts) ->
    case first_peer_with(Peers, << Prefix/binary, Hash/binary >>, Opts) of
        {ok, Body, _Peer} ->
            {ok, Body};
        _ ->
            {error, error_message(<<"history-unavailable">>,
                <<"No peer still holds the carried history for this "
                    "checkpoint. Peers drop it once the block leaves their "
                    "cache, ~50 blocks below the tip.">>)}
    end.

%% @doc Check the newest reward-history entry against the checkpoint block's
%% `reward-history-hash' and its parent's. The older tail is carried checkpoint
%% state and is trusted at join, as it is by the standard Arweave join path.
verify_rewards(Bin, Block, Parent, Opts) ->
    maybe
        {ok, Rewards} ?= decode_rewards(Bin, []),
        valid(
            ar_rewards:validate_reward_history_hashes(
                int(<<"height">>, Block, Opts),
                lists:reverse(Rewards),
                [
                    decoded(<<"reward-history-hash">>, Block, Opts),
                    decoded(<<"reward-history-hash">>, Parent, Opts)
                ]
            ),
            <<"invalid-reward-history-hash">>
        )
    end.

%% @doc Check the block-time history against the checkpoint block's
%% `block-time-history-hash'. That hash covers the whole history, so this is a
%% complete check of all 21,600 entries against one signed value.
verify_times(Bin, Block, Opts) ->
    maybe
        {ok, Times} ?= decode_times(Bin, []),
        valid(
            ar_block_time_history:validate_hashes(
                lists:reverse(Times),
                [decoded(<<"block-time-history-hash">>, Block, Opts)]
            ),
            <<"invalid-block-time-history-hash">>
        )
    end.

valid(true, _Message) ->
    ok;
valid(false, Message) ->
    {error,
        error_message(Message, <<"The history does not match the block.">>)}.

%% @doc Decode a `/reward_history' response. Entries are oldest-first on the
%% wire and newest-first in every consumer, hence the reversal at the call
%% site. The address is a fixed 32 bytes; only the two amounts are
%% length-prefixed, and the denomination is a bare 24-bit integer.
decode_rewards(<<>>, Parsed) ->
    {ok, lists:reverse(Parsed)};
decode_rewards(<< Addr:32/binary, RateSize:8, Rate:(RateSize * 8),
        RewardSize:8, Reward:(RewardSize * 8), Denomination:24,
        Rest/binary >>, Parsed) ->
    decode_rewards(Rest, [{Addr, Rate, Reward, Denomination} | Parsed]);
decode_rewards(_Rest, _Parsed) ->
    {error, error_message(<<"invalid-reward-history">>,
        <<"The body is not a whole number of reward history entries.">>)}.

%% @doc Decode a `/block_time_history' response, oldest-first as served.
decode_times(<<>>, Parsed) ->
    {ok, lists:reverse(Parsed)};
decode_times(<< IntervalSize:8, Interval:(IntervalSize * 8),
        StepsSize:8, Steps:(StepsSize * 8),
        ChunksSize:8, Chunks:(ChunksSize * 8),
        Rest/binary >>, Parsed) ->
    decode_times(Rest, [{Interval, Steps, Chunks} | Parsed]);
decode_times(_Rest, _Parsed) ->
    {error, error_message(<<"invalid-block-time-history">>,
        <<"The body is not a whole number of block time history entries.">>)}.

%%% The account anchor.

%% @doc Assemble and record the selected checkpoint's own account tree.
%%
%% Substituting a later tree would leave the checkpoint state accountless, and
%% `~arweave-block@2.9/apply' would refuse the first transition. Bootstrap
%% therefore fails instead of recording a state it cannot extend.
account_anchor(Peers, Block, Opts) ->
    Height = int(<<"height">>, Block, Opts),
    case anchor_at(Peers, Block, Opts) of
        {ok, Anchor} ->
            ?event(arweave_sync_short,
                {account_anchor, {height, Height}},
                Opts
            ),
            {ok, ID} = hb_cache:write(Anchor, Opts),
            hb_cache:link(ID, lib_arweave_paths:accounts_anchor(), Opts),
            ok;
        _ ->
            {error, error_message(<<"no-account-anchor">>,
                <<"No trusted peer served an account tree hashing to the "
                    "checkpoint's `wallet-list'. Peers serve it only within "
                    "roughly a hundred blocks of the tip, so a deeper "
                    "checkpoint cannot be anchored; choose a shallower one, or "
                    "retry if a peer was briefly unavailable.">>)}
    end.

%% @doc Assemble the account tree the selected checkpoint committed to, and
%% return the anchor record: the tree, and the block that vouches for it. The
%% checkpoint has already been identified; asking peers which hash occupies its
%% height again would let an unrelated peer disagreement replace or veto it.
anchor_at(Peers, Block, Opts) ->
    maybe
        Hash = hb_maps:get(<<"indep-hash">>, Block, [], Opts),
        Height = int(<<"height">>, Block, Opts),
        Root = hb_maps:get(<<"wallet-list">>, Block, [], Opts),
        {ok, Accounts} ?= wallet_list(Peers, Root, <<>>, #{}, Opts),
        {ok, _} ?=
            hb_ao:resolve(
                Accounts,
                #{ <<"path">> => <<"verify">>, <<"expected-root">> => Root },
                Opts
            ),
        {ok,
            #{
                <<"indep-hash">> => Hash,
                <<"height">> => Height,
                <<"accounts">> => Accounts
            }
        }
    end.

%% @doc Page `/wallet_list/<root>[/<cursor>]' into `~arweave-wallets@2.9',
%% which parses each 2500-account body and accumulates the tree.
%%
%% The accumulated state is threaded back in as the base of the next call
%% rather than re-read from the cache, because it carries the memoised tree in
%% its private section -- dropping it would make every page rebuild the whole
%% trie from its chunks.
%%
%% The peer chooses the cursor, so the walk ends only when a peer says it has,
%% and the root check that would refuse a bogus tree runs only once the walk
%% returns. A peer answering every request with a fresh cursor therefore never
%% terminates. The bound ends the walk; mainnet's tree is ~312,000 accounts at
%% 2,500 a page, around 125, so this leaves room for it to grow eightfold before
%% an honest peer is refused.
%%
%% What this bounds is the number of round trips, not the bytes in any one of
%% them: a single oversized body is `hb_http''s business, not this walk's, and
%% it is the same for every peer fetch here -- the block index, the histories,
%% a transaction. Bounding the loop is what belongs at this layer.
wallet_list(Peers, Root, Cursor, Accounts, Opts) ->
    wallet_list(Peers, Root, Cursor, Accounts, ?MAX_WALLET_LIST_PAGES, Opts).

wallet_list(_Peers, _Root, <<"last">>, Accounts, _Left, _Opts) ->
    {ok, Accounts};
wallet_list(_Peers, _Root, _Cursor, _Accounts, 0, _Opts) ->
    {error,
        error_message(
            502,
            <<"wallet-list-too-long">>,
            <<"A peer kept answering with a new cursor. The account tree is "
                "bounded so that it cannot be made to grow without end.">>
        )
    };
wallet_list(Peers, Root, Cursor, Accounts, Left, Opts) ->
    maybe
        {ok, Body, _} ?=
            first_peer_with(Peers, wallet_list_path(Root, Cursor), Opts),
        {ok, Page} ?=
            hb_ao:resolve(
                Accounts#{ <<"device">> => <<"arweave-wallets@2.9">> },
                #{ <<"path">> => <<"page">>, <<"body">> => Body },
                Opts
            ),
        wallet_list(
            Peers,
            Root,
            hb_maps:get(<<"next-cursor">>, Page, <<"last">>, Opts),
            hb_maps:get(<<"accounts">>, Page, #{}, Opts),
            Left - 1,
            Opts
        )
    end.

wallet_list_path(Root, <<>>) ->
    << "/wallet_list/", Root/binary >>;
wallet_list_path(Root, Cursor) ->
    << "/wallet_list/", Root/binary, "/", Cursor/binary >>.

%%% Sync.

%% @doc Advance the chain by at most one batch and re-run fork choice.
do_sync(_Req, Opts) ->
    maybe
        {ok, Peers} ?= peers(Opts),
        {ok, Tip} ?= incumbent(Opts),
        Batch = hb_opts:get(arweave_sync_batch, ?DEFAULT_SYNC_BATCH, Opts),
        Targets = targets(Peers, height(Tip, Opts) + Batch, Opts),
        Applied = advance(Peers, Targets, Batch, 0, Opts),
        % The incumbent is re-read rather than reused: `advance' moves the tip
        % as each block lands, so the one this pass started from is stale, and
        % measuring the reorg window against a stale height would widen it.
        {ok, Current} ?= incumbent(Opts),
        Winner = choose(Current, heads(Targets), Opts),
        ok ?= link_tip(Winner, Opts),
        Settled = settle(Winner, Opts),
        ?event(arweave_sync_short,
            {synced,
                {applied, Applied},
                {settled, Settled},
                {height, height(Winner, Opts)},
                {indep_hash, {string, Winner}},
                {peers, length(Peers)}
            },
            Opts
        ),
        {ok,
            #{
                <<"applied">> => Applied,
                <<"settled">> => Settled,
                <<"indep-hash">> => Winner,
                <<"height">> => height(Winner, Opts)
            }
        }
    end.

%% @doc The block each peer is asked to advance this node to: its own tip, or
%% the ceiling one batch above the local tip, whichever is lower. A node at the
%% tip therefore follows its peers exactly and sees their reorgs, while a node
%% far behind advances a bounded amount per pass.
targets(Peers, Ceiling, Opts) ->
    hb_util:unique(
        lists:filtermap(
            fun(Peer) -> target(Peer, Ceiling, Opts) end,
            Peers
        )
    ).

target(Peer, Ceiling, Opts) ->
    case peer_info(Peer, Opts) of
        {ok, Height, Current} when Height =< Ceiling ->
            {true, {Peer, Current, Height}};
        {ok, _Height, _Current} ->
            ceiling_target(Peer, height_hash([Peer], Ceiling, Opts), Ceiling);
        _ ->
            false
    end.

ceiling_target(Peer, {ok, Hash}, Ceiling) ->
    {true, {Peer, Hash, Ceiling}};
ceiling_target(_Peer, _Error, _Ceiling) ->
    false.

heads(Targets) ->
    hb_util:unique([ Hash || {_Peer, Hash, _Height} <- Targets ]).

%% @doc Bring the chain up to each target in turn, stopping when the batch is
%% exhausted, and report how many blocks were applied.
%%
%% A target this node cannot reach -- a peer further ahead than a batch, a
%% branch forking outside the reorg window, a block that fails validation -- is
%% that peer's problem and not a reason to abandon the pass, so it is logged
%% and the next target is tried. It is logged as a warning rather than into a
%% tracing topic: this clause is where a genuine consensus failure and an
%% unreachable peer both land, so a node that is refusing every block a peer
%% offers must say so without an operator having had to ask in advance.
%% The count is what makes idempotence observable: a second pass over the same
%% range reports zero.
%%
%% The batch bounds the pass, not each target: a target that is closing a reorg
%% may overshoot it by up to `?CHECKPOINT_DEPTH', because a branch applied
%% halfway is not a branch and the tip could not move onto it.
advance(_Peers, [], _Batch, Applied, _Opts) ->
    Applied;
advance(_Peers, _Targets, Batch, Applied, _Opts) when Applied >= Batch ->
    Applied;
advance(Peers, [{Peer, Hash, _Height} | Targets], Batch, Applied, Opts) ->
    case catch_up(Peers, Peer, Hash, Batch + ?CHECKPOINT_DEPTH, Opts) of
        {ok, Count} ->
            advance(Peers, Targets, Batch, Applied + Count, Opts);
        Error ->
            ?event(warning,
                {arweave_target_skipped,
                    {peer, {string, Peer}},
                    {indep_hash, {string, Hash}},
                    {message, {string, reason(Error, Opts)}},
                    {detail, {string, detail(Error, Opts)}}
                },
                Opts
            ),
            advance(Peers, Targets, Batch, Applied, Opts)
    end.

catch_up(Peers, Peer, Hash, Steps, Opts) ->
    maybe
        {ok, Missing} ?= walk_back(Peer, Hash, [], Steps, Opts),
        apply_blocks(Peers, Missing, 0, Opts)
    end.

%% @doc Collect the blocks between a target and the deepest block this node has
%% already published, ancestor-first.
%%
%% A walk that runs out of steps without reaching one is not a gap this node
%% can close in this pass -- the peer is either further ahead than a batch or
%% forked deeper than the reorg window -- so it fails and the next target is
%% tried.
walk_back(_Peer, _Hash, _Missing, 0, _Opts) ->
    {error, error_message(<<"no-known-ancestor">>,
        <<"The branch does not meet this node inside the window.">>)};
walk_back(Peer, Hash, Missing, Steps, Opts) ->
    case hb_cache:read(block_key(Hash), Opts) of
        {ok, _Block} ->
            {ok, Missing};
        _ ->
            maybe
                {ok, Block} ?= peer_block([Peer], Hash, Opts),
                walk_back(
                    Peer,
                    hb_maps:get(<<"previous-block">>, Block, [], Opts),
                    [Block | Missing],
                    Steps - 1,
                    Opts
                )
            end
    end.

%% @doc Apply each block onto the block its parent produced and publish the
%% result before moving on, so that an interrupted pass leaves every block it
%% finished permanently done.
%%
%% A block that fails ends the run over that target, and the count is not
%% reported -- but the blocks published before it are kept, and the next pass
%% walks back only as far as the newest of them. Progress is retained even
%% though it is not counted, so a peer serving one bad block costs the work of
%% that block and nothing before it.
apply_blocks(_Peers, [], Applied, _Opts) ->
    {ok, Applied};
apply_blocks(Peers, [Block | Blocks], Applied, Opts) ->
    Hash = hb_maps:get(<<"indep-hash">>, Block, [], Opts),
    Started = erlang:monotonic_time(millisecond),
    case apply_block(Peers, Block, Hash, Opts) of
        {ok, Next} ->
            report_applied(Block, Hash, Next, Started, Opts),
            apply_blocks(Peers, Blocks, Applied + 1, Opts);
        Error ->
            report_rejected(Block, Hash, Error, Started, Opts),
            Error
    end.

apply_block(Peers, Block, Hash, Opts) ->
    maybe
        {ok, Parent} ?=
            hb_cache:read(
                block_key(hb_maps:get(<<"previous-block">>, Block, [], Opts)),
                Opts
            ),
        {ok, TXs} ?= transactions(Peers, Block, Opts),
        {ok, Next} ?=
            hb_ao:resolve(
                Parent#{ <<"device">> => ?BLOCK_DEVICE },
                #{
                    <<"path">> => <<"apply">>,
                    <<"next">> => Block,
                    <<"transactions">> => TXs
                },
                Opts
            ),
        Anchored = with_anchor(Next, Hash, Opts),
        {ok, _} ?= publish(Anchored, Hash, TXs, Opts),
        ok ?= adopt(Hash, Opts),
        {ok, Anchored}
    end.

%% @doc Report a validated block as it lands.
%%
%% On `arweave_sync_short', which the node prints without being asked, because
%% a pass closing a thirty-block gap is otherwise several minutes of silence
%% that an operator cannot distinguish from a stalled validator.
%%
%% The checks are part of the line rather than something to be inferred: a block
%% validated without an account tree has passed a strictly weaker set, and the
%% node says which for every block rather than letting the weaker mode look
%% identical to the stronger one.
report_applied(Block, Hash, Next, Started, Opts) ->
    ?event(arweave_sync_short,
        {block_applied,
            {height, int(<<"height">>, Block, Opts)},
            {indep_hash, {string, Hash}},
            {elapsed_ms, erlang:monotonic_time(millisecond) - Started},
            {txs, length(hb_util:message_to_ordered_list(
                hb_maps:get(<<"txs">>, Block, [], Opts), Opts))},
            {checks, checks(Next, Opts)}
        },
        Opts
    ).

%% @doc The checks a block records having been validated by. Read off the block
%% as a field rather than through `lib_arweave_state', which belongs to
%% `~arweave-block@2.9': a bridge module is packaged with the device that
%% declares it, so calling one from here compiles cleanly and raises `undef' in
%% the packaged runtime. A block is a message, and this layer reads it as one.
checks(Block, Opts) ->
    hb_util:message_to_ordered_list(
        hb_maps:get(
            <<"checks">>,
            hb_maps:get(<<"validation">>, Block, #{}, Opts),
            [],
            Opts
        ),
        Opts
    ).

%% @doc Report a block this node refused, with the reason it refused it.
%%
%% A rejection is the single most important thing this device has to say, so it
%% is said on `warning' and names the rule that refused the block. Folding it
%% into a general `target_skipped' would not do: a bad peer and a real consensus
%% failure both reach that, and only one of them matters.
report_rejected(Block, Hash, Error, Started, Opts) ->
    ?event(warning,
        {arweave_block_rejected,
            {height, int(<<"height">>, Block, Opts)},
            {indep_hash, {string, Hash}},
            {elapsed_ms, erlang:monotonic_time(millisecond) - Started},
            {message, {string, reason(Error, Opts)}},
            {detail, {string, detail(Error, Opts)}}
        },
        Opts
    ).

%% @doc Name why a block or a target was refused. Errors from the devices carry
%% the standard body; anything else is rendered as it arrived, so a shape this
%% device did not anticipate is still legible rather than silently flattened.
reason({error, Body}, Opts) when is_map(Body) ->
    hb_maps:get(<<"message">>, Body, <<"unknown">>, Opts);
reason(Error, _Opts) ->
    term(Error).

detail({error, Body}, Opts) when is_map(Body) ->
    hb_maps:get(<<"detail">>, Body, <<>>, Opts);
detail(_Error, _Opts) ->
    <<>>.

%% @doc Offer a block that has just been applied to fork choice, and move the
%% tip if it wins.
%%
%% Per block, not per pass. A node thirty blocks behind spends twenty-five
%% minutes of VDF closing that gap, and a tip that only moves once at the end
%% of it is indistinguishable from a tip that is stuck -- to an operator
%% watching, to a monitor comparing against the network, and to a restart,
%% which would otherwise rerun fork choice from the height the pass began at
%% and re-offer every block it had already validated.
%%
%% Moving early is safe because it is still fork choice that decides: a block
%% on a competing branch is adopted only once that branch is genuinely heavier,
%% which for a reorg is not until enough of it has been applied.
adopt(Hash, Opts) ->
    maybe
        {ok, Tip} ?= incumbent(Opts),
        link_tip(choose(Tip, [Hash], Opts), Opts)
    end.

link_tip(Hash, Opts) ->
    hb_cache:link(block_key(Hash), lib_arweave_paths:tip(), Opts),
    ok.

%% @doc Fetch the transaction bodies a block's validation needs.
%%
%% A block header carries only transaction identifiers, but the weave size,
%% the `tx-root' and the per-transaction checks all need each transaction's
%% data root and size, so `~arweave-block@2.9/apply' takes the bodies alongside
%% the header. They are returned in the header's own order, because that is the
%% order `tx-root' is built in and the device rejects any other.
%%
%% Each is fetched once and kept in one form: the committed `tx@1.0' message.
%% That is the form the consensus checks convert to a record, and the form
%% publication writes -- which is what puts a transaction in the generic match
%% index, and what makes a placement's link to it resolve, because the
%% commitment's identifier is the Arweave transaction identifier.
transactions(Peers, Block, Opts) ->
    collect(
        hb_pmap:parallel_map(
            hb_util:message_to_ordered_list(
                hb_maps:get(<<"txs">>, Block, [], Opts),
                Opts
            ),
            fun(ID) -> transaction(Peers, ID, Opts) end,
            hb_opts:get(arweave_peer_workers, ?DEFAULT_PEER_WORKERS, Opts)
        ),
        []
    ).

transaction(Peers, ID, Opts) ->
    maybe
        {ok, Body, _} ?= first_peer_with(Peers, <<"/tx/", ID/binary>>, Opts),
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-tx@2.9">>, <<"body">> => Body },
            <<"from-json">>,
            Opts
        )
    end.

%% @doc Reduce a list of per-item results to one result over the list. A single
%% item that could not be fetched fails the whole, rather than being dropped
%% from a list whose order and contents a consensus rule depends on -- the
%% `tx-root' is built over exactly the transactions in exactly their order, and
%% an anchor window missing an entry rejects the transactions that anchor on it.
collect([], Fetched) ->
    {ok, lists:reverse(Fetched)};
collect([{ok, Item} | Rest], Fetched) ->
    collect(Rest, [Item | Fetched]);
collect([Error | _Rest], _Fetched) ->
    Error.

%% @doc Attach the bootstrapped account tree to the very block it was fetched
%% at, and only then. Below that block the tree's root is committed by a header
%% this node has not validated, so it is not yet anything to trust; at that
%% block the header has just been validated, and from there forward every
%% account transition is checked in full.
%%
%% Attaching the tree also adds `accounts' to the block's checks, because at
%% that one block the tree was checked against the `wallet-list' the header
%% commits to. Every block after it earns the same name by a stronger route:
%% `~arweave-block@2.9/apply' replays the transition and produces the root.
%%
%% The anchor's fields are read with `hb_maps:get/4' rather than matched in the
%% head: a value that came back from the cache may be a link, and a link never
%% pattern-matches the binary it stands for. Matching here silently attached
%% the tree to nothing.
with_anchor(Block, Hash, Opts) ->
    case hb_cache:read(lib_arweave_paths:accounts_anchor(), Opts) of
        {ok, Anchor} ->
            attach(
                hb_maps:get(<<"indep-hash">>, Anchor, [], Opts),
                Hash,
                Anchor,
                Block,
                Opts
            );
        _ ->
            Block
    end.

attach(Hash, Hash, Anchor, Block, Opts) ->
    Block#{
        <<"accounts">> => hb_maps:get(<<"accounts">>, Anchor, [], Opts),
        <<"validation">> =>
            #{
                <<"checks">> =>
                    hb_util:unique(checks(Block, Opts) ++ [<<"accounts">>])
            }
    };
attach(_Anchored, _Hash, _Anchor, Block, _Opts) ->
    Block.

%%% Settled transactions.

%% @doc Announce every transaction of every block that has passed beyond the
%% depth a reorganisation may reach, oldest block first.
%%
%% Settlement is not part of consensus and deliberately runs after it. A block
%% deeper than `?CHECKPOINT_DEPTH' below the tip can no longer be reorganised
%% away, which is the only guarantee an archive consumer needs before it spends
%% real work on a transaction -- unpacking a bundle, indexing its items,
%% fetching its data. Operators attach that work as handlers of the
%% `arweave-settled-transaction' hook, so nothing downstream of it is coupled to
%% block validation.
%%
%% A block's marker is written only after all of its transactions have been
%% announced, so the pair is idempotent per block hash and transaction: a pass
%% that fails partway re-announces the block's transactions on the next pass,
%% and a handler that cannot be idempotent about that is a handler that would
%% also break on a restart.
%%
%% A failure stops the walk and is retried, and it never moves the consensus
%% tip: settlement reads the chain, and nothing in it writes to the chain.
settle(Tip, Opts) ->
    announce_blocks(
        unsettled(
            descend(Tip, ?CHECKPOINT_DEPTH, Opts),
            settle_batch(Opts),
            [],
            Opts
        ),
        0,
        Opts
    ).

%% @doc Walk down from the newest settled block, collecting the blocks whose
%% transactions have not been announced, oldest first.
%%
%% Two things end the walk, and both are ordinary. A block that has been
%% announced ends it because everything below it has been too. A block this node
%% does not hold ends it because there is nothing to announce: the chain reaches
%% back to the block the node joined at, and the hash below that one is a name
%% for something it has never seen.
%%
%% Each block is read once, here, and travels with its hash. Collecting hashes
%% and reading them again to announce them would put the block below the oldest
%% one this node holds into the list -- the walk cannot tell a chain's end from
%% a block it has yet to fetch without reading it -- and announcing runs
%% oldest-first, so that one unreadable entry would report the whole pass as
%% failed and settle nothing.
unsettled([], _Left, Blocks, _Opts) ->
    Blocks;
unsettled(_Hash, 0, Blocks, _Opts) ->
    Blocks;
unsettled(Hash, Left, Blocks, Opts) ->
    case hb_cache:read(block_key(Hash), Opts) of
        {ok, Block} -> unsettled(Hash, Block, Left, Blocks, Opts);
        _ -> Blocks
    end.

unsettled(Hash, Block, Left, Blocks, Opts) ->
    case hb_cache:read(settled_path(Hash), Opts) of
        {ok, _Marker} ->
            Blocks;
        _ ->
            unsettled(
                hb_maps:get(<<"previous-block">>, Block, [], Opts),
                Left - 1,
                [{Hash, Block} | Blocks],
                Opts
            )
    end.

%% @doc Announce each block in turn, stopping at the first that fails. What it
%% failed on is retried by the next pass, which walks down to it again.
announce_blocks([], Settled, _Opts) ->
    Settled;
announce_blocks([{Hash, Block} | Blocks], Settled, Opts) ->
    case announce(Hash, Block, Opts) of
        ok ->
            announce_blocks(Blocks, Settled + 1, Opts);
        Error ->
            ?event(warning,
                {arweave_settle_failed,
                    {indep_hash, {string, Hash}},
                    {message, {string, reason(Error, Opts)}},
                    {detail, {string, detail(Error, Opts)}}
                },
                Opts
            ),
            Settled
    end.

%% @doc Announce one block's transactions and mark the block as settled.
announce(Hash, Block, Opts) ->
    maybe
        ok ?=
            announce_transactions(
                hb_util:message_to_ordered_list(
                    hb_maps:get(<<"transactions">>, Block, [], Opts),
                    Opts
                ),
                Opts
            ),
        hb_cache:link(block_key(Hash), settled_path(Hash), Opts),
        ok
    end.

%% @doc Run the hook once per placement. The request is the placement itself,
%% carrying the link to the transaction it places, so a handler has both the
%% weave location and the message without another lookup.
announce_transactions([], _Opts) ->
    ok;
announce_transactions([Placement | Placements], Opts) ->
    maybe
        {ok, _Result} ?=
            hb_hook:on(
                ?SETTLED_HOOK,
                hb_cache:ensure_loaded(Placement, Opts),
                Opts
            ),
        announce_transactions(Placements, Opts)
    end.

%% @doc The block that is `Depth' blocks below the one given, or `[]' when the
%% chain does not reach that far back.
descend(Hash, 0, _Opts) ->
    Hash;
descend([], _Depth, _Opts) ->
    [];
descend(Hash, Depth, Opts) ->
    descend(parent(Hash, Opts), Depth - 1, Opts).

%% @doc How many blocks one pass may settle. A node that has just closed a long
%% gap has a run of blocks to announce, and the bound keeps that run from
%% holding the runner for the whole of it.
settle_batch(Opts) ->
    max(0, hb_util:int(
        hb_opts:get(arweave_settle_batch, ?DEFAULT_SETTLE_BATCH, Opts))).

%% @doc The marker recording that a block's transactions have been announced.
settled_path(Hash) ->
    lib_arweave_paths:settled(Hash).

%%% Historical materialisation.

%% @doc Materialise blocks below the ones this node holds, checked against the
%% block index its selected tip carries.
%%
%% The index is the authority. Its root was committed by a header this node
%% validated, so the hash, weave size and transaction root it records at every
%% height from genesis are as trustworthy as the tip itself -- and a serving
%% peer therefore cannot substitute another block or another transaction set,
%% however far below the join the request reaches.
%%
%% `from' is the highest height to materialise and is required. There is no
%% frontier to resume from, by design: the block index says what every height
%% should hold, a block already published is skipped, and a pass that stops
%% early is repeated by re-issuing the same request. Recording how far the
%% backfill had got would be a second kind of progress state beside the blocks
%% themselves, and the brief has exactly one.
do_backfill(Req, Opts) ->
    maybe
        {ok, Peers} ?= peers(Opts),
        {ok, Tip} ?= incumbent(Opts),
        {ok, TipBlock} ?=
            read_block(Tip, <<"no-tip">>,
                <<"The stored tip does not resolve to a block.">>, Opts),
        {ok, Index} ?= backfill_index(TipBlock, Opts),
        {ok, From} ?= backfill_from(Req, Opts),
        Profile = hb_maps:get(<<"profile">>, Req, <<"archive">>, Opts),
        Count = backfill_count(Req, From, Opts),
        Started = erlang:monotonic_time(millisecond),
        Materialized =
            backfill(Peers, Index, From, Count, Profile, 0, Opts),
        ?event(arweave_sync_short,
            {backfilled,
                {materialized, Materialized},
                {from, From},
                {to, From - Count + 1},
                {profile, {string, Profile}},
                {elapsed_ms, erlang:monotonic_time(millisecond) - Started}
            },
            Opts
        ),
        {ok,
            #{
                <<"materialized">> => Materialized,
                <<"from">> => From,
                <<"to">> => From - Count + 1,
                <<"profile">> => Profile
            }
        }
    end.

%% @doc The block index the tip carries, which is what a materialisation is
%% checked against. A node whose tip carries none cannot authenticate a
%% historical header at all, and is told so rather than fetching one.
backfill_index(TipBlock, Opts) ->
    case hb_maps:get(<<"block-index">>, TipBlock, [], Opts) of
        [] ->
            {error, error_message(<<"no-block-index">>,
                <<"The selected tip carries no block index, so a historical "
                    "block cannot be checked against anything.">>)};
        Index ->
            {ok, Index}
    end.

%% @doc The highest height a backfill materialises, which the caller names.
backfill_from(Req, Opts) ->
    case hb_maps:get(<<"from">>, Req, not_found, Opts) of
        not_found ->
            {error,
                error_message(400, <<"missing-from">>,
                    <<"`from' must name the highest height to materialise.">>)};
        From ->
            {ok, max(0, hb_util:int(From))}
    end.

%% @doc How many blocks one call may materialise: what the request asked for,
%% never more than the node permits, and never below genesis.
backfill_count(Req, From, Opts) ->
    Ceiling =
        max(
            1,
            hb_util:int(
                hb_opts:get(
                    arweave_backfill_batch,
                    ?DEFAULT_BACKFILL_BATCH,
                    Opts
                )
            )
        ),
    Asked =
        max(1, hb_util:int(hb_maps:get(<<"count">>, Req, Ceiling, Opts))),
    min(From + 1, min(Ceiling, Asked)).

%% @doc Materialise each height in turn, downwards. A height whose expected
%% block is already published costs a cache read and nothing else, which is what
%% makes re-issuing the same request the way to resume.
backfill(_Peers, _Index, _Height, 0, _Profile, Done, _Opts) ->
    Done;
backfill(_Peers, _Index, Height, _Left, _Profile, Done, _Opts)
        when Height < 0 ->
    Done;
backfill(Peers, Index, Height, Left, Profile, Done, Opts) ->
    case materialize_at(Peers, Index, Height, Profile, Opts) of
        {ok, present} ->
            backfill(Peers, Index, Height - 1, Left - 1, Profile, Done, Opts);
        {ok, published} ->
            backfill(
                Peers, Index, Height - 1, Left - 1, Profile, Done + 1, Opts);
        Error ->
            ?event(warning,
                {arweave_backfill_stopped,
                    {height, Height},
                    {message, {string, reason(Error, Opts)}},
                    {detail, {string, detail(Error, Opts)}}
                },
                Opts
            ),
            Done
    end.

%% @doc Materialise the block the index records at a height, unless this node
%% has already published it.
materialize_at(Peers, Index, Height, Profile, Opts) ->
    maybe
        {ok, Expected} ?= index_entry(Index, Height, Opts),
        Hash = hb_maps:get(<<"indep-hash">>, Expected, <<>>, Opts),
        case hb_cache:read(block_key(Hash), Opts) of
            {ok, _Block} ->
                {ok, present};
            _ ->
                materialize_new(
                    Peers, Index, Height, Hash, Expected, Profile, Opts)
        end
    end.

%% @doc Fetch, check and publish one historical block.
materialize_new(Peers, Index, Height, Hash, Expected, Profile, Opts) ->
    maybe
        {ok, Previous} ?= previous_entry(Index, Height, Opts),
        {ok, Block, TXs} ?=
            materialized(Peers, Expected, Previous, Profile, Opts),
        {ok, _ID} ?= publish(Block, Hash, TXs, Opts),
        {ok, published}
    end.

%% @doc The index entry below a height. Genesis has none, and carries a weave
%% size of zero, which is what the weave arithmetic of the block above it reads.
previous_entry(_Index, 0, _Opts) ->
    {ok, #{ <<"weave-size">> => 0 }};
previous_entry(Index, Height, Opts) ->
    index_entry(Index, Height - 1, Opts).

%% @doc Fetch a header from peers and materialise it against the authenticated
%% entries the caller resolved from the block index. Returns the block and the
%% transaction bodies, which publication writes before the block that places
%% them.
materialized(Peers, Expected, Previous, Profile, Opts) ->
    Hash = hb_maps:get(<<"indep-hash">>, Expected, <<>>, Opts),
    maybe
        {ok, Header} ?= peer_block(Peers, Hash, Opts),
        {ok, TXs} ?=
            materialized_transactions(Peers, Header, Profile, Opts),
        {ok, Block} ?=
            hb_ao:resolve(
                Header#{ <<"device">> => ?BLOCK_DEVICE },
                #{
                    <<"path">> => <<"materialize">>,
                    <<"profile">> => Profile,
                    <<"expected">> => Expected,
                    <<"previous-entry">> => Previous,
                    <<"transactions">> => TXs
                },
                Opts
            ),
        {ok, Block, TXs}
    end.

%% @doc Fetch the transaction bodies a materialisation needs. A header-only
%% materialisation establishes nothing about them, so it fetches none.
materialized_transactions(_Peers, _Header, <<"headers">>, _Opts) ->
    {ok, []};
materialized_transactions(Peers, Header, _Profile, Opts) ->
    transactions(Peers, Header, Opts).

%%% Fork choice.

%% @doc Arweave's fork-choice rule, exactly (reference §8).
%%
%% A branch takes the tip only by being strictly heavier: an equal
%% `cumulative-diff' leaves the incumbent in place, first-seen, and there is no
%% secondary key. A height or timestamp tie-break would look harmless and would
%% be a consensus divergence.
%%
%% Eligibility is the only finality rule upstream has: a branch may win only
%% while it forks from the current chain within `?CHECKPOINT_DEPTH' of the tip.
%% Eligibility is measured against the incumbent and its height rather than
%% against the running winner, so the outcome does not depend on the order the
%% candidates arrive in.
choose(Incumbent, Candidates, Opts) ->
    TipHeight = height(Incumbent, Opts),
    lists:foldl(
        fun(Candidate, Winner) ->
            better(Incumbent, TipHeight, Winner, Candidate, Opts)
        end,
        Incumbent,
        Candidates
    ).

better(Incumbent, TipHeight, Winner, Candidate, Opts) ->
    BranchPoint = branch_point(Candidate, Incumbent, TipHeight, Opts),
    case eligible(BranchPoint, TipHeight)
            andalso cumulative_diff(Candidate, Opts)
                > cumulative_diff(Winner, Opts) of
        true -> Candidate;
        false -> Winner
    end.

%% @doc Whether a branch forks from the chain recently enough to take the tip.
%% `branch_point/4' stops searching at the height this refuses, which is why an
%% unfound branch point and an ineligible one are the same answer -- and why
%% both read `eligible_height/1' rather than restating the arithmetic. A rule
%% written down twice is a rule that can drift, and drift here is a consensus
%% divergence rather than a bug.
eligible([], _TipHeight) ->
    false;
eligible(BranchPoint, TipHeight) ->
    BranchPoint >= eligible_height(TipHeight).

%% @doc The shallowest height at which a branch may fork and still take the
%% tip. Upstream is `ar_block_cache:get_checkpoint_height/1' --
%% `TipHeight - ?CHECKPOINT_DEPTH + 1' -- and `is_valid_fork/4' refuses a
%% branch point below it. The `+ 1' is load-bearing: without it this node would
%% accept a branch forking one block deeper than every reference node accepts,
%% and take a tip the network does not have.
eligible_height(TipHeight) ->
    TipHeight - ?CHECKPOINT_DEPTH + 1.

%% @doc The height at which a candidate branch meets the incumbent chain, or
%% `[]' when they do not meet at or above the height `eligible/2' would accept.
%%
%% The search is bounded by that height rather than by a step count, because a
%% candidate is not necessarily at the tip's height: one that has run ahead
%% must first be walked down to the tip's height before the two can be stepped
%% together, and a step budget large enough for that would be an unbounded walk
%% for a candidate that is merely deep.
branch_point(Candidate, Incumbent, TipHeight, Opts) ->
    converge(
        Candidate,
        height(Candidate, Opts),
        Incumbent,
        height(Incumbent, Opts),
        eligible_height(TipHeight),
        Opts
    ).

converge(_A, AHeight, _B, BHeight, Floor, _Opts)
        when AHeight < Floor; BHeight < Floor ->
    [];
converge(Hash, Height, Hash, _BHeight, _Floor, _Opts) when is_binary(Hash) ->
    Height;
converge(A, AHeight, B, BHeight, Floor, Opts) when AHeight > BHeight ->
    converge(parent(A, Opts), AHeight - 1, B, BHeight, Floor, Opts);
converge(A, AHeight, B, BHeight, Floor, Opts) when BHeight > AHeight ->
    converge(A, AHeight, parent(B, Opts), BHeight - 1, Floor, Opts);
converge(A, AHeight, B, BHeight, Floor, Opts) ->
    converge(
        parent(A, Opts),
        AHeight - 1,
        parent(B, Opts),
        BHeight - 1,
        Floor,
        Opts
    ).

parent(Hash, Opts) ->
    field(<<"previous-block">>, Hash, [], Opts).

height(Hash, Opts) ->
    hb_util:int(field(<<"height">>, Hash, 0, Opts)).

cumulative_diff(Hash, Opts) ->
    hb_util:int(field(<<"cumulative-diff">>, Hash, 0, Opts)).

%% @doc Read a field of a published block. A branch head this node has not
%% published has no fields to read, which is what makes it ineligible rather
%% than an error.
field(_Key, Hash, Default, _Opts) when not is_binary(Hash) ->
    Default;
field(Key, Hash, Default, Opts) ->
    case hb_cache:read(block_key(Hash), Opts) of
        {ok, Block} -> hb_maps:get(Key, Block, Default, Opts);
        _ -> Default
    end.

%%% Publication.

%% @doc Publish a validated block, in the order that makes its presence mean
%% something.
%%
%% Nothing records how far indexing has got, because the block itself is the
%% record: it is linked under its own hash only once every index derived from it
%% is written, so a block that reads back is a block whose local indexes are
%% finished. That ordering is the whole of the guarantee, and it is why the
%% steps are not interchangeable:
%%
%% <ol>
%% <li>the transaction messages, each of which the generic match index picks up
%%     as it is written, so a query can find them;</li>
%% <li>the placements and their byte offsets, which link the transactions
%%     written in step one;</li>
%% <li>the components and the block message itself, which links the placements
%%     written in step two;</li>
%% <li>the block hash, which names the message written in step three.</li>
%% </ol>
%%
%% A pass interrupted anywhere before the last step leaves content-addressed
%% messages nothing points at, which the next pass rewrites at the same
%% identifiers. There is nothing to clean up and nothing to resume from.
publish(Block, Hash, TXs, Opts) ->
    maybe
        ok ?= write_transactions(TXs, Opts),
        {ok, Placed} ?= write_placements(Block, Opts),
        {ok, ID} ?= hb_cache:write(Placed, Opts),
        hb_cache:link(ID, block_key(Hash), Opts),
        {ok, ID}
    end.

%% @doc Write each transaction of the block as a `tx@1.0' message. Writing them
%% is what puts them in the generic match index, and it is what makes the
%% placement's link to a transaction resolvable.
write_transactions([], _Opts) ->
    ok;
write_transactions([TX | TXs], Opts) ->
    maybe
        {ok, _ID} ?= hb_cache:write(TX, Opts),
        write_transactions(TXs, Opts)
    end.

%% @doc Write the block's placements, alias each under its transaction, record
%% the byte offsets, and return the block carrying the placements as links.
write_placements(Block, Opts) ->
    maybe
        {ok, Links} ?=
            lib_arweave_placement:write(
                hb_util:message_to_ordered_list(
                    hb_maps:get(<<"transactions">>, Block, [], Opts),
                    Opts
                ),
                Opts
            ),
        {ok, Block#{ <<"transactions">> => Links }}
    end.

%% @doc The store key a validated block is filed under.
block_key(Hash) ->
    lib_arweave_paths:block(Hash).

%% @doc Read a published block by hash, answering with a message rather than
%% the bare `{error, not_found}' the cache returns. A device key must not put an
%% atom on the wire: `dev_meta:embed_status/2' renders a non-map error as HTTP
%% 400 with the atom as the body, so `not_found' would reach a client as an
%% underscored term with the wrong status.
read_block(Hash, Message, Detail, Opts) ->
    case hb_cache:read(block_key(Hash), Opts) of
        {ok, Block} ->
            {ok, Block};
        _ ->
            {error, error_message(404, Message, Detail)}
    end.

%% @doc The block hash at the stored tip. A node that has not bootstrapped has
%% no tip, and `sync' has nothing to extend.
incumbent(Opts) ->
    case hb_cache:read(lib_arweave_paths:tip(), Opts) of
        {ok, Block} ->
            {ok, hb_maps:get(<<"indep-hash">>, Block, [], Opts)};
        _ ->
            {error, error_message(<<"not-bootstrapped">>,
                <<"The node has no chain to extend.">>)}
    end.

%%% Peers.

%% @doc The peers this node reads from. Both lists default to `[]', so a node
%% that names neither has no source and is told so, rather than quietly
%% reaching for a hardcoded host.
peers(Opts) ->
    case
        hb_util:unique(
            hb_opts:get(arweave_trusted_peers, [], Opts)
                ++ hb_opts:get(arweave_untrusted_peers, [], Opts)
        )
    of
        [] ->
            {error, error_message(<<"no-peers">>,
                <<"Set `arweave-untrusted-peers` to a list of peer URLs.">>)};
        Peers ->
            {ok, Peers}
    end.

%% @doc Record the peers a bootstrap used in the node message, so that `sync'
%% has a source without further configuration. An operator-supplied list is
%% never overwritten, and a node with no HTTP server -- a test resolving
%% directly -- has nothing to record into.
record_peers([], Peers, Opts) ->
    publish_peers(hb_opts:get(<<"http-server">>, no_server, Opts), Peers, Opts);
record_peers(_Configured, _Peers, _Opts) ->
    ok.

%% @doc Record the peers into the node message, if there is a server holding
%% one. `no_server' is the sentinel `hb_http_server:get_opts/1' itself writes
%% for a node with no listener, so it is the value to match: a node message that
%% has been through `set_opts' carries the key *present and set to that*, not
%% absent. Matching `undefined' instead let the key through to
%% `hb_http_server:get_opts/1', which asks ranch for a listener that does not
%% exist and raises `badarg' -- reporting a bootstrap that had already
%% succeeded and moved the tip as `sync-failed'.
publish_peers(no_server, _Peers, _Opts) ->
    ok;
publish_peers(_Server, Peers, Opts) ->
    merge_peers(hb_http_server:get_opts(Opts), Peers).

merge_peers(NodeMsg, Peers) when is_map(NodeMsg) ->
    hb_http_server:set_opts(
        NodeMsg#{ <<"arweave-untrusted-peers">> => Peers }
    ),
    ok;
merge_peers(_NoNodeMessage, _Peers) ->
    ok.

%% @doc The greatest height any peer reports.
network_height(Peers, Opts) ->
    case lists:filtermap(fun(Peer) -> peer_height(Peer, Opts) end, Peers) of
        [] ->
            {error, error_message(<<"no-peer-served">>,
                <<"No peer answered `/info`.">>)};
        Heights ->
            {ok, lists:max(Heights)}
    end.

peer_height(Peer, Opts) ->
    case peer_info(Peer, Opts) of
        {ok, Height, _Current} -> {true, Height};
        _ -> false
    end.

%% @doc A peer's height and tip hash, from the one endpoint that reports both.
peer_info(Peer, Opts) ->
    maybe
        {ok, Body} ?= body(peer_get(Peer, <<"/info">>, Opts)),
        #{ <<"height">> := Height, <<"current">> := Current } ?=
            hb_json:decode(Body),
        {ok, hb_util:int(Height), Current}
    end.

%% @doc The block hash every peer reports at a height, read from the one-entry
%% ranged block index: the answer is the hash itself, so there is no header to
%% download and nothing to decode. Peers that disagree have no single answer,
%% which is what `shared_ancestor/4' walks down through.
height_hash(Peers, Height, Opts) ->
    Bin = hb_util:bin(Height),
    Path = << "/block_index2/", Bin/binary, "/", Bin/binary >>,
    Hashes =
        hb_util:unique(
            lists:map(fun(Peer) -> hash_at(Peer, Path, Opts) end, Peers)
        ),
    case Hashes of
        [Hash] when is_binary(Hash) ->
            {ok, Hash};
        _ ->
            {error, error_message(<<"no-agreed-hash">>,
                <<"The peers do not agree on the block at the height.">>)}
    end.

hash_at(Peer, Path, Opts) ->
    maybe
        {ok, Body} ?= body(peer_get(Peer, Path, Opts)),
        << Hash:48/binary, _/binary >> ?= Body,
        hb_util:encode(Hash)
    end.

%% @doc Fetch a block header and decode it.
%%
%% Blocks are fetched in binary. Arweave's JSON block parser asserts a pre-2.6
%% height and has no post-2.6 clause at all -- upstream nodes have used
%% `/block2' since that fork -- so a JSON fetch would have nothing to decode it
%% with.
peer_block(Peers, Hash, Opts) when is_binary(Hash) ->
    maybe
        {ok, Body, _} ?=
            first_peer_with(Peers, <<"/block2/hash/", Hash/binary>>, Opts),
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-block@2.9">>, <<"body">> => Body },
            <<"from-binary">>,
            Opts
        )
    end;
peer_block(_Peers, _Hash, _Opts) ->
    {error, error_message(<<"invalid-indep-hash">>,
        <<"The block reference is not a hash.">>)}.

%% @doc Fetch a path from the first peer that serves it. A peer that errors is
%% the wrong peer to ask, not a failure; only an exhausted list is one.
first_peer_with([], _Path, _Opts) ->
    {error, error_message(<<"no-peer-served">>,
        <<"No peer served the requested path.">>)};
first_peer_with([Peer | Peers], Path, Opts) ->
    case body(peer_get(Peer, Path, Opts)) of
        {ok, Body} -> {ok, Body, Peer};
        _ -> first_peer_with(Peers, Path, Opts)
    end.

%% @doc Issue a read-only `GET' against an Arweave peer.
%%
%% The timeouts ride in the route's per-node `opts', which is the only place
%% `hb_http' reads request options from: the same keys on the node message
%% never reach the outbound call, and hackney's 120-second default receive
%% timeout would otherwise stall a whole bootstrap behind one silent peer.
peer_get(Peer, Path, Opts) ->
    hb_http:get(
        #{
            <<"uri">> => << Peer/binary, Path/binary >>,
            <<"opts">> =>
                #{
                    <<"http-client">> => hackney,
                    <<"http-client-hackney-recv-timeout">> =>
                        hb_opts:get(
                            arweave_peer_timeout,
                            ?DEFAULT_PEER_TIMEOUT,
                            Opts
                        ),
                    <<"http-client-connect-timeout">> =>
                        hb_opts:get(
                            arweave_peer_connect_timeout,
                            ?DEFAULT_PEER_CONNECT_TIMEOUT,
                            Opts
                        )
                }
        },
        Path,
        Opts
    ).

%% @doc The body of a peer response. Arweave sends no content type on most of
%% these endpoints, so every response arrives as a body to be interpreted by
%% the caller that knows what it asked for.
body({ok, #{ <<"body">> := Body }}) ->
    {ok, Body};
body({ok, _Response}) ->
    {error, error_message(<<"empty-peer-response">>,
        <<"The peer answered without a body.">>)};
body(Error) ->
    Error.

%%% Exclusion.

%% @doc Run `Fun' as the node's only `Task' at a time.
%%
%% A single `~cron@1.0/every' task cannot overlap itself -- its worker sleeps
%% the interval after the call returns, not on a fixed schedule. Concurrency
%% arrives from every other direction instead: an operator calling `sync' over
%% HTTP while a pass is already running, a second `every' task registered
%% against the same node, or a `tip' read racing a transition. Without this they
%% would fetch and validate the same blocks twice over. Callers queue behind
%% the runner and receive its result, so the coalescing is invisible to them.
%%
%% `hb_name' is BEAM-global, so the name carries the node's own identity: two
%% nodes sharing a BEAM, as the multi-node tests do, must not share a runner.
exclusive(Task, Fun, Opts) ->
    Runner = hb_name:singleton(name(Task, Opts), fun runner/0),
    Monitor = erlang:monitor(process, Runner),
    Runner ! {run, self(), Monitor, Fun},
    receive
        {ran, Monitor, Result} ->
            erlang:demonitor(Monitor, [flush]),
            Result;
        {'DOWN', Monitor, process, Runner, Reason} ->
            {error, error_message(<<"sync-runner-down">>, term(Reason))}
    end.

%% @doc The runner's loop: one task at a time, each replying to its caller.
runner() ->
    receive
        {run, From, Ref, Fun} ->
            From ! {ran, Ref, run(Fun)},
            runner()
    end.

%% @doc Run one task, reporting a crash rather than dying of it: a runner that
%% died would leave every queued caller waiting on a reply that never comes.
run(Fun) ->
    try Fun()
    catch
        Class:Reason:Stacktrace ->
            ?event(warning,
                {arweave_sync_task_failed,
                    {class, Class},
                    {reason, Reason},
                    {stacktrace, {trace, Stacktrace}}
                }
            ),
            {error, error_message(<<"sync-failed">>, term(Reason))}
    end.

%% @doc The registration name of this node's runner for a task.
name(Task, Opts) ->
    {
        arweave_sync,
        Task,
        hb_util:bin(hb_opts:get(<<"http-server">>, <<"local">>, Opts))
    }.

%%% Internal functions.

%% @doc Read a bootstrap parameter from the **node message only**.
%%
%% Preferring the request and falling back to the node message would hand the
%% chain's trust root to whoever could reach the port. `bootstrap' is
%% unauthenticated, and on a node with no chain -- a fresh install, a wiped
%% cache, or the window before the operator's own bootstrap -- the checkpoint
%% block *is* the single hash the whole chain is anchored to. A caller
%% supplying it would choose what the node believes Arweave to be, and
%% `checkpoint-height' is worse still: unbounded, so a height near the 2.9 fork
%% commits the node to millions of blocks of VDF.
%%
%% The same reasoning already governs `forced/2' below. It was not carried
%% across to the checkpoint itself, which is the trust root rather than a switch
%% on it.
configured(Option, Opts) ->
    hb_opts:get(Option, [], Opts).

int(Key, Msg, Opts) ->
    hb_util:int(hb_maps:get(Key, Msg, 0, Opts)).

%% @doc Decode a base64url field of a peer-sourced message. Peer data is
%% untrusted by definition, so it goes through the checked decoder.
decoded(Key, Msg, Opts) ->
    hb_util:ok_or(hb_util:safe_decode(hb_maps:get(Key, Msg, <<>>, Opts)), <<>>).

%% @doc Render an arbitrary term as an error detail.
term(Term) ->
    hb_util:bin(io_lib:format("~p", [Term])).

%% @doc Build the standard error body.
error_message(Message, Detail) ->
    error_message(422, Message, Detail).
error_message(Status, Message, Detail) ->
    #{
        <<"status">> => Status,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.
