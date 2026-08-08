%%% @doc The sync layer of `~arweave@2.9': `bootstrap', `sync' and `tip'.
%%%
%%% This module is the orchestration spine of the Arweave block-validation
%%% subsystem. It owns everything to do with peers -- discovering their tips,
%%% paging the block index, paging the account tree, fetching the two carried
%%% histories -- and owns the store layout the chain lives in. It validates
%%% nothing itself: every check is delegated to the `~arweave-*@2.9' device
%%% that owns it. Peer I/O in, chain state out.
%%%
%%% All network access is read-only `GET'. There is no write path here, and
%%% gossip participation is an explicit non-goal: this is a pull-only node.
%%%
%%% == The store layout ==
%%%
%%% Everything lives in `hb_cache'. There is no process state, no
%%% `persistent_term' and no side table; a node that is restarted mid-sync
%%% picks up exactly where it stopped because the store is the only thing that
%%% ever knew where it was.
%%%
%%% <ul>
%%%   <li>`~arweave@2.9/tip' -- a link to the chain state at the tip.</li>
%%%   <li>`~arweave@2.9/state/&lt;indep-hash&gt;' -- a link to the chain state
%%%       that block produced. This is both the parent lookup `sync' needs and
%%%       the record that makes it idempotent: a block whose state is already
%%%       present is never fetched or applied again.</li>
%%%   <li>`~arweave@2.9/accounts-anchor' -- the block the account tree was
%%%       adopted at, and the tree itself.</li>
%%% </ul>
%%%
%%% == The account anchor ==
%%%
%%% Peers keep the account tree as a diff DAG pruned at depth 100, so the tree
%%% at a checkpoint more than ~100 blocks below the tip cannot be fetched at
%%% all. What this subsystem does about it:
%%% adopt the tree at the checkpoint, and check its root against that block's
%%% signed `wallet-list'. `bootstrap' records the checkpoint as the anchor and
%%% attaches the tree to its chain state, so every block this node goes on to
%%% validate has a tree to spend from and every account transition after the
%%% checkpoint is replayed in full.
%%%
%%% A bootstrap that cannot fetch and verify that tree **fails**. It does not
%%% search upward for one: an anchor above the checkpoint leaves the checkpoint
%%% state without a tree, and since `~arweave-block@2.9/apply' refuses a
%%% transition that has none, every block between the checkpoint and the anchor
%%% is refused -- so the anchor is never reached and the node never syncs.
%%%
%%% == What is only obtainable near the tip ==
%%%
%%% Consensus data goes back to genesis: the block index is served for any
%%% range, and so are headers. Carried state is not. Peers serve
%%% `/reward_history' and `/block_time_history' only while the block is in
%%% their cache -- measured at depth 50 exactly -- and the account tree only to
%%% ~100. So a checkpoint far from the tip cannot be bootstrapped at all, not
%%% because its proofs are unavailable but because the state they are applied
%%% to is. `arweave-checkpoint-depth' is bounded by that on one side and by
%%% `?CHECKPOINT_DEPTH' on the other.
%%%
%%% == Cost ==
%%%
%%% The VDF chain dominates validation by orders of magnitude. Measured on
%%% mainnet blocks with the fused verification kernel and fourteen threads: a
%%% block lands in 17-38 seconds against the network's ~122-second interval, so
%%% a node has real headroom to close a gap but not an unlimited amount of it.
%%% That is why the checkpoint defaults to a recent height rather than the 2.9
%%% fork, why `bootstrap' logs an estimate of what an operator's chosen
%%% checkpoint will cost before spending any of it, and why the tip moves as
%%% each block lands rather than at the end of a pass -- a node closing a
%%% thirty-block gap is busy for around twenty-five minutes, and must not look
%%% stopped while it is.
-module(dev_arweave_sync).
-export([bootstrap/3, sync/3, tip/3, validated/3]).
%%% Exported so that `dev_arweave' can refuse on `block' the shapes this module
%%% refuses on `validated'. One definition, because two would drift.
-export([block_hash/1]).
%%% Live mainnet probes. Exported so that they can be named individually on a
%%% `rebar3 device test --test' line; see the bottom of this file.
-export([live_peers_/0, live_block_index_/0, live_bootstrap_/0, live_sync_/0]).
-export([live_apply_/0]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

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

%% Peers serve `/reward_history' and `/block_time_history' only for the last
%% `?STORE_BLOCKS_BEHIND_CURRENT' (50) blocks -- measured: HTTP 200 at depth 50,
%% 404 at 60. A checkpoint at exactly 50 therefore sits on the boundary and
%% falls outside it the moment the network produces another block, which it does
%% every ~122 seconds and reliably does while bootstrap is still paging the
%% block index. 30 leaves room for the tip to advance during a bootstrap without
%% the histories vanishing underneath it.
-define(DEFAULT_CHECKPOINT_DEPTH, 30).
-define(DEFAULT_SYNC_BATCH, 50).
-define(DEFAULT_PEER_WORKERS, 8).
-define(DEFAULT_PEER_TIMEOUT, 60000).
-define(DEFAULT_PEER_CONNECT_TIMEOUT, 10000).
%% Measured CPU-seconds of VDF verification per mainnet block: 116.9 steps at
%% 1.68 seconds each.
%% Measured through the verification path, which is the only one a validator
%% takes: 116.9 steps per block at 25 x 1,111,546 SHA-256, against the fused
%% crypto-extension kernel's 25.66 M SHA/s. Derived from kernel throughput this
%% is 134 CPU-seconds; timing a whole 239-step mainnet block end to end gives
%% 128 for an average block. The estimate this drives is an order-of-magnitude
%% guide for an operator choosing a checkpoint, so the two agreeing within 5%
%% is the accuracy that matters.
%%
%% A machine whose kernel self-test fails falls back to the OpenSSL back-end at
%% 4.03 M SHA/s and is ~6.4x this figure.
-define(VDF_SECONDS_PER_BLOCK, 130).

%%% Store paths. Every key this device writes is namespaced by its device path.

-define(TIP_PATH, <<"~arweave@2.9/tip">>).
-define(STATE_PATH, <<"~arweave@2.9/state">>).
-define(ANCHOR_PATH, <<"~arweave@2.9/accounts-anchor">>).

%% @doc Establish the node's initial chain state.
%%
%% This is the only point at which the node trusts anything, and what it trusts
%% is a single block hash. Everything else fetched here is checked against that
%% block: the block index against its `hash-list-merkle', the account tree
%% against its `wallet-list', both histories against their hashes. No peer is
%% believed; the checkpoint vouches for all of it. This is the `bitcoind'
%% model.
%%
%% The checkpoint comes from `arweave-checkpoint-block' when the operator names
%% one, which is the trustless configuration. Otherwise it is the block every
%% peer agrees on at `arweave-checkpoint-depth' below the network tip -- the
%% shared ancestor -- which trusts the peer set not to be uniformly lying about
%% a block 50 deep, and is the default because validating from the 2.9 fork is
%% four orders of magnitude too expensive to be one.
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

%% @doc Return the chain state at the tip of the heaviest eligible branch.
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
        read_state(Winner, <<"no-tip">>,
            <<"The stored tip does not resolve to a chain state.">>, Opts)
    end.

%% @doc Return the chain state this node validated for a block, named by
%% `indep-hash', by height, or as `current'.
%%
%% This key never contacts a peer, and that is the whole of its purpose.
%% `~arweave@2.9/block' answers from a gateway on a cache miss, so a block this
%% node verified and a block it was simply handed arrive looking the same. A
%% block that reaches a caller through this key was validated here, against its
%% parent, by this node -- or it does not arrive at all.
%%
%% The absence of a fallback is the feature. `not-validated' means "this node
%% has not verified that block", which is a different and more useful answer
%% than a block fetched from elsewhere to fill the gap.
validated(_Base, Req, Opts) ->
    maybe
        {ok, Hash} ?= validated_hash(Req, Opts),
        read_state(Hash, <<"not-validated">>,
            <<"This node has not validated that block. Blocks are not fetched "
                "from peers here: only blocks this node verified are served.">>,
            Opts)
    end.

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
%% The value becomes a store path, and `hb_path:to_binary/1' does not collapse
%% `..': `hb_store_fs' walks the components and hands the traversal to the OS.
%% So a request naming `../../../hyperbeam-key.json' read the operator's signing
%% key straight off disk, unauthenticated, from a key whose whole purpose is to
%% answer strangers. Requiring the value to decode as base64url to 48 bytes
%% leaves nothing for a path to be built out of.
block_hash(Hash) when is_binary(Hash) ->
    % The alphabet check is the one that matters, and decoding does not
    % perform it: `hb_util:decode/1' is the *unchecked* decoder, so any 64
    % characters yield 48 bytes -- including 64 characters of `..' and `/'.
    % Checking only the decoded length let a traversal through to the
    % chokepoint, where it surfaced as an uncaught throw rather than as this
    % device's own `invalid-block'.
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
        ok ?= account_anchor(Peers, Height, TipHeight, Opts),
        {ok, Index} ?= block_index(Peers, Block, Height, Opts),
        {ok, Recent} ?= anchor_window(Peers, Index, Block, Height, Opts),
        % The checkpoint state goes through `with_anchor/3' like any other, and
        % for the same reason. The anchor probe starts at the checkpoint, so in
        % the default configuration the tree it finds belongs to this very
        % block -- and if it were only attached to blocks `sync' applies later,
        % it would never be attached at all, leaving the account checks
        % silently disabled on a node that had fetched and verified the tree.
        {ok, ID} ?=
            record_state(
                with_anchor(
                    #{
                        <<"block">> => Block,
                        <<"block-index">> => Index,
                        <<"accounts">> => [],
                        <<"recent-blocks">> => Recent,
                        <<"reward-history">> => #{ <<"body">> => Rewards },
                        <<"block-time-history">> => #{ <<"body">> => Times }
                    },
                    Hash,
                    Opts
                ),
                Hash,
                Opts
            ),
        hb_cache:link(state_path(Hash), ?TIP_PATH, Opts),
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

%% @doc Refuse a bootstrap on a node that already has a chain.
%%
%% `bootstrap' is the one moment this node trusts anything, and it is reachable
%% over HTTP by anyone the node answers. Left unguarded it is a repeatable trust
%% reset: every call re-asks the peer set where the chain starts and relinks the
%% tip onto their answer, discarding what the node had validated for itself.
%%
%% The rule is having a chain at all, not having a *longer* one. A bootstrap
%% that lands a few blocks ahead is not the harmless case it looks like: those
%% are blocks the node could have validated and instead took on trust, and it is
%% the case that actually happened during a live run -- the tip moved 1975068 to
%% 1975070 by re-bootstrapping while a pass was validating that very range.
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
            "Pass `force` to re-anchor deliberately.">>)}.

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
    Threads = hb_opts:get(arweave_vdf_threads, vdf_threads(), Opts),
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

vdf_threads() ->
    max(1, erlang:system_info(schedulers) div 2).

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

%% @doc Assemble the window of recent blocks the checkpoint's chain state
%% carries: the checkpoint and the blocks below it, newest first, each reduced
%% to its identifier and the identifiers of its transactions.
%%
%% A transaction anchors on a block within `get_max_tx_anchor_depth' of the one
%% carrying it, and may not repeat a transaction already inside that window.
%% Both rules read this list and nothing else, so a chain state that does not
%% carry it rejects every block-anchored transaction on the network -- which is
%% to say every real block, as soon as an account tree is present to check
%% transactions against at all. Neither rule is derivable from the checkpoint
%% header, and mainnet peers refuse the sub-field request that would return a
%% block's transaction list on its own (HTTP 421,
%% `Subfield block querying is disabled'), so the headers are fetched whole and
%% reduced here.
%%
%% The identifiers come from the index this bootstrap has already proven
%% against the checkpoint's `hash-list-merkle', and each header is checked
%% against the identifier it was asked for, so the window admits no peer claim:
%% a peer that drops a transaction from a block to let it be replayed changes
%% that block's `indep-hash' and is caught by `identified_block/3'.
anchor_window(Peers, Index, Block, Height, Opts) ->
    maybe
        {ok, Hashes} ?= window_hashes(Index, Height, Opts),
        {ok, Recent} ?=
            collect(
                hb_pmap:parallel_map(
                    Hashes,
                    fun(Hash) -> anchor_entry(Peers, Hash, Opts) end,
                    workers(Opts)
                ),
                []
            ),
        {ok, [ summarise_block(Block, Opts) | Recent ]}
    end.

%% @doc The identifiers of the blocks below the checkpoint that fall inside the
%% anchor window, newest first. The window holds exactly the depth an anchor may
%% reach back, counting the checkpoint itself, which `lib_arweave_state' then
%% maintains at that length as each block is applied. The checkpoint's own entry
%% is not among these: it is read from the header bootstrap already holds rather
%% than fetched a second time.
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
        {ok, Entry} ?=
            hb_ao:resolve(
                Index#{ <<"device">> => <<"arweave-block-index@2.9">> },
                #{ <<"path">> => <<"at">>, <<"height">> => Height },
                Opts
            ),
        {ok, hb_maps:get(<<"indep-hash">>, Entry, [], Opts)}
    end.

%% @doc Fetch one block of the window and reduce it to what the two rules read.
anchor_entry(Peers, Hash, Opts) ->
    maybe
        {ok, Block} ?= identified_block(Peers, Hash, Opts),
        {ok, summarise_block(Block, Opts)}
    end.

%% @doc Reduce a header to what an anchor check reads from it.
summarise_block(Block, Opts) ->
    #{
        <<"indep-hash">> => hb_maps:get(<<"indep-hash">>, Block, <<>>, Opts),
        <<"txs">> => hb_maps:get(<<"txs">>, Block, [], Opts)
    }.

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
%% from then on. They are stored as the exact bytes the peer served: that
%% encoding is the network's own and it round-trips, so inventing a second one
%% would only add a way for the two to disagree.
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

%% @doc Check the reward history against the checkpoint block's
%% `reward-history-hash' and its parent's. Post-2.8 that hash is a chain, so
%% the pair pins the newest element and the hash it extends; the entries behind
%% it are proven by the first block `~arweave-block@2.9/apply' prices, because
%% every one of them feeds that price.
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

%% @doc Find the deepest block at or above the checkpoint whose account tree a
%% peer still serves, assemble the tree from its pages, and record it against
%% that block.
%%
%% The tree must be the checkpoint's own. It is not searched for.
%%
%% Searching upward for a tree, or giving up with `ok' and no tree, would each
%% be a liveness trap, because `~arweave-block@2.9/apply' refuses a transition
%% with no account tree:
%%
%% An anchor recorded at checkpoint+k is attached only to the state whose hash
%% matches it, so the checkpoint state keeps `accounts => []'. Applying
%% checkpoint+1 then reads a parent with no tree, `apply/3' refuses it with
%% `accounts-not-checked', and the block is skipped -- so the anchor block at
%% checkpoint+k is never reached and the tree is never attached. Every later
%% pass repeats it. One transient peer failure during the ~125-page wallet-list
%% walk turned into a node that could never sync again and could only be
%% recovered by setting `arweave-force-bootstrap' and restarting.
%%
%% Failing the bootstrap instead is strictly better: the operator retries, or
%% chooses a shallower checkpoint, and gets a working node. A bootstrap that
%% half-succeeds is worth less than one that says so.
account_anchor(Peers, Height, _TipHeight, Opts) ->
    case anchor_at(Peers, Height, Opts) of
        {ok, Anchor} ->
            ?event(arweave_sync_short,
                {account_anchor, {height, Height}},
                Opts
            ),
            {ok, ID} = hb_cache:write(Anchor, Opts),
            hb_cache:link(ID, ?ANCHOR_PATH, Opts),
            ok;
        _ ->
            {error, error_message(<<"no-account-anchor">>,
                <<"No trusted peer served an account tree hashing to the "
                    "checkpoint's `wallet-list'. Peers serve it only within "
                    "roughly a hundred blocks of the tip, so a deeper "
                    "checkpoint cannot be anchored; choose a shallower one, or "
                    "retry if a peer was briefly unavailable.">>)}
    end.

%% @doc Assemble the account tree the block at `Height' committed to, and
%% return the anchor record: the tree, and the block that vouches for it.
%%
%% The header is identified rather than merely fetched. The whole trust argument
%% for the tree is that its root is committed by a signed header, so a header
%% taken on a peer's word would let the same peer choose the root the tree is
%% checked against -- and the account check would then pass against a tree of
%% that peer's invention.
anchor_at(Peers, Height, Opts) ->
    maybe
        {ok, Hash} ?= height_hash(Peers, Height, Opts),
        {ok, Block} ?= identified_block(Peers, Hash, Opts),
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
        ?event(arweave_sync_short,
            {synced,
                {applied, Applied},
                {height, height(Winner, Opts)},
                {indep_hash, {string, Winner}},
                {peers, length(Peers)}
            },
            Opts
        ),
        {ok,
            #{
                <<"applied">> => Applied,
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

%% @doc Collect the blocks between a target and the deepest block this node
%% already has a chain state for, ancestor-first.
%%
%% A walk that runs out of steps without reaching one is not a gap this node
%% can close in this pass -- the peer is either further ahead than a batch or
%% forked deeper than the reorg window -- so it fails and the next target is
%% tried.
walk_back(_Peer, _Hash, _Missing, 0, _Opts) ->
    {error, error_message(<<"no-known-ancestor">>,
        <<"The branch does not meet this node inside the window.">>)};
walk_back(Peer, Hash, Missing, Steps, Opts) ->
    case hb_cache:read(state_path(Hash), Opts) of
        {ok, _State} ->
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

%% @doc Apply each block onto the chain state its parent produced and record
%% the result before moving on, so that an interrupted pass leaves every block
%% it finished permanently done.
%%
%% A block that fails ends the run over that target, and the count is not
%% reported -- but the states written before it are kept, and the next pass
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
                state_path(hb_maps:get(<<"previous-block">>, Block, [], Opts)),
                Opts
            ),
        {ok, TXs} ?= transactions(Peers, Block, Opts),
        {ok, Next} ?=
            hb_ao:resolve(
                Parent#{ <<"device">> => <<"arweave-block@2.9">> },
                #{
                    <<"path">> => <<"apply">>,
                    <<"next">> => Block,
                    <<"transactions">> => TXs
                },
                Opts
            ),
        Anchored = with_anchor(Next, Hash, Opts),
        {ok, _} ?= record_state(Anchored, Hash, Opts),
        ok ?= adopt(Hash, Opts),
        {ok, Anchored}
    end.

%% @doc Report a validated block as it lands.
%%
%% On `arweave_sync_short', which the node prints without being asked, because
%% a pass closing a thirty-block gap is otherwise eleven minutes of silence that
%% an operator cannot tell from a hang -- and that silence is precisely how a
%% sync that never validated anything went unnoticed through a live run.
%%
%% The mode is part of the line rather than something to be inferred: a block
%% validated without an account tree has passed a strictly weaker set of checks,
%% and the node says so for every block rather than letting the weaker mode look
%% identical to the stronger one.
report_applied(Block, Hash, Next, Started, Opts) ->
    ?event(arweave_sync_short,
        {block_applied,
            {height, int(<<"height">>, Block, Opts)},
            {indep_hash, {string, Hash}},
            {elapsed_ms, erlang:monotonic_time(millisecond) - Started},
            {txs, length(hb_util:message_to_ordered_list(
                hb_maps:get(<<"txs">>, Block, [], Opts), Opts))},
            {accounts_checked, accounts_checked(Next, Opts)}
        },
        Opts
    ).

%% @doc Whether the block that produced a chain state was validated against an
%% account tree. Read off the state as a field rather than through
%% `lib_arweave_state', which belongs to `~arweave-block@2.9': a bridge module
%% is packaged with the device that declares it, so calling one from here
%% compiles cleanly and raises `undef' in the packaged runtime. A chain state is
%% a message, and this layer reads it as one.
accounts_checked(State, Opts) ->
    hb_util:atom(hb_maps:get(<<"accounts-checked">>, State, false, Opts)).

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
    hb_cache:link(state_path(Hash), ?TIP_PATH, Opts),
    ok.

%% @doc Fetch the transaction bodies a block's validation needs.
%%
%% A block header carries only transaction identifiers, but the weave size,
%% the `tx-root' and the per-transaction checks all need each transaction's
%% data root and size, so `~arweave-block@2.9/apply' takes the bodies alongside
%% the header. They are returned in the header's own order, because that is the
%% order `tx-root' is built in and the device rejects any other.
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

%% @doc Attach the bootstrapped account tree to the chain state of the very
%% block it was fetched at, and only then. Below that block the tree's root is
%% committed by a header this node has not validated, so it is not yet anything
%% to trust; at that block the header has just been validated, and from there
%% forward every account transition is checked in full.
%% The anchor's fields are read with `hb_maps:get/4' rather than matched in the
%% head: a value that came back from the cache may be a link, and a link never
%% pattern-matches the binary it stands for. Matching here silently attached
%% the tree to nothing.
with_anchor(State, Hash, Opts) ->
    case hb_cache:read(?ANCHOR_PATH, Opts) of
        {ok, Anchor} ->
            attach(
                hb_maps:get(<<"indep-hash">>, Anchor, [], Opts),
                Hash,
                Anchor,
                State,
                Opts
            );
        _ ->
            State
    end.

attach(Hash, Hash, Anchor, State, Opts) ->
    State#{ <<"accounts">> => hb_maps:get(<<"accounts">>, Anchor, [], Opts) };
attach(_Anchored, _Hash, _Anchor, State, _Opts) ->
    State.

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

%% @doc Read a field of the block a stored chain state validated. A branch head
%% this node has no state for has no fields to read, which is what makes it
%% ineligible rather than an error.
field(_Key, Hash, Default, _Opts) when not is_binary(Hash) ->
    Default;
field(Key, Hash, Default, Opts) ->
    case hb_cache:read(state_path(Hash), Opts) of
        {ok, State} ->
            hb_maps:get(
                Key,
                hb_maps:get(<<"block">>, State, #{}, Opts),
                Default,
                Opts
            );
        _ ->
            Default
    end.

%%% Chain state storage.

%% @doc Write a chain state and index it by the block it validated. The index
%% is what `sync' looks a parent up in, and what makes it skip a block it has
%% already applied.
record_state(State, Hash, Opts) ->
    maybe
        {ok, ID} ?= hb_cache:write(State, Opts),
        hb_cache:link(ID, state_path(Hash), Opts),
        {ok, ID}
    end.

%% @doc The store path a chain state is filed under.
%%
%% The separator check is a backstop, not the guard: callers that take a hash
%% from a request validate it first (`block_hash/1'). It is here because this is
%% the one place a hash becomes a path, and `hb_path:to_binary/1' does not
%% collapse `..' -- so anything that reaches here carrying a separator would be
%% resolved by the filesystem rather than treated as a name.
state_path(Hash) when is_binary(Hash) ->
    case binary:match(Hash, [<<"/">>, <<"..">>, <<0>>]) of
        nomatch ->
            hb_path:to_binary([?STATE_PATH, Hash]);
        _ ->
            throw({unsafe_state_path, Hash})
    end.

%% @doc Read a chain state by block hash, answering with a message rather than
%% the bare `{error, not_found}' the cache returns. A device key must not put an
%% atom on the wire: `dev_meta:embed_status/2' renders a non-map error as HTTP
%% 400 with the atom as the body, so `not_found' would reach a client as an
%% underscored term with the wrong status.
read_state(Hash, Message, Detail, Opts) ->
    case hb_cache:read(state_path(Hash), Opts) of
        {ok, State} ->
            {ok, State};
        _ ->
            {error, error_message(404, Message, Detail)}
    end.

%% @doc The block hash at the stored tip. A node that has not bootstrapped has
%% no tip, and `sync' has nothing to extend.
incumbent(Opts) ->
    case hb_cache:read(?TIP_PATH, Opts) of
        {ok, State} ->
            {ok,
                hb_maps:get(
                    <<"indep-hash">>,
                    hb_maps:get(<<"block">>, State, #{}, Opts),
                    [],
                    Opts
                )
            };
        _ ->
            {error, error_message(<<"not-bootstrapped">>,
                <<"The node has no chain state to extend.">>)}
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

%%% Tests.

%% @doc A real-shaped block hash for a readable test name.
%%
%% A request-supplied hash must be a real block hash before it can name a store
%% path, so the synthetic chain is keyed by hashes rather than by names like
%% `main-50'. Hashing the name keeps the tests readable while giving every
%% block an identifier of the shape production uses.
test_hash(Name) ->
    hb_util:encode(crypto:hash(sha384, Name)).

%% @doc A chain state as `bootstrap' and `sync' write them, reduced to the
%% fields fork choice reads. Written and indexed exactly as the real ones are,
%% so the tests exercise the real lookups.
test_state(Hash, Height, CDiff, Parent, Opts) ->
    {ok, _} =
        record_state(
            #{
                <<"block">> =>
                    #{
                        <<"indep-hash">> => test_hash(Hash),
                        <<"height">> => Height,
                        <<"cumulative-diff">> => CDiff,
                        <<"previous-block">> => Parent
                    }
            },
            test_hash(Hash),
            Opts
        ),
    test_hash(Hash).

%% @doc Write a run of blocks, each extending the last, and return the head.
test_branch(Prefix, Parent, From, To, CDiff, Step, Opts) ->
    lists:foldl(
        fun(Height, Previous) ->
            test_state(
                << Prefix/binary, (hb_util:bin(Height))/binary >>,
                Height,
                CDiff + ((Height - From) * Step),
                Previous,
                Opts
            )
        end,
        Parent,
        lists:seq(From, To)
    ).

%% @doc A chain of 100 blocks with the tip pointer set. Block 100's
%% `cumulative-diff' is 1990.
test_chain() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Tip =
        test_branch(
            <<"main-">>,
            test_hash(<<"genesis">>),
            1,
            100,
            1000,
            10,
            Opts
        ),
    hb_cache:link(state_path(Tip), ?TIP_PATH, Opts),
    {Opts, Tip}.

%% @doc The height at which a branch is exactly as deep as fork choice can
%% still act on, and one deeper than that. The two are adjacent, so the pair of
%% tests below pins the boundary itself rather than merely sampling either side
%% of it. Upstream's `ar_block_cache:get_checkpoint_height/1' is
%% `TipHeight - ?CHECKPOINT_DEPTH + 1' and `is_valid_fork/4' refuses anything
%% below it, so the shallowest refused height is `TipHeight - ?CHECKPOINT_DEPTH'
%% -- one block above where a naive reading of the constant would put it.
test_eligible_fork_height() -> 100 - ?CHECKPOINT_DEPTH + 1.
test_ineligible_fork_height() -> 100 - ?CHECKPOINT_DEPTH.

%% @doc A branch that is strictly heavier and forks inside the window takes the
%% tip, and `tip' resolved as a device key reports the chain state it produced.
fork_choice_prefers_heavier_branch_test() ->
    {Opts, Tip} = test_chain(),
    Fork =
        test_branch(
            <<"fork-">>,
            test_hash(<<"main-90">>),
            91,
            100,
            1901,
            10,
            Opts
        ),
    ?assert(cumulative_diff(Fork, Opts) > cumulative_diff(Tip, Opts)),
    {ok, State} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"tip">>, <<"candidates">> => [Fork] },
            Opts
        ),
    ?assertEqual(
        Fork,
        hb_maps:get(
            <<"indep-hash">>,
            hb_maps:get(<<"block">>, State, #{}, Opts),
            not_found,
            Opts
        )
    ).

%% @doc The mutation partner of the test above: the same branch, forking at the
%% same height, with a `cumulative-diff' equal to the incumbent's rather than
%% greater, does not take the tip. Equal keeps the incumbent -- first-seen --
%% and there is no secondary tie-break to rescue it.
fork_choice_keeps_incumbent_on_equal_diff_test() ->
    {Opts, Tip} = test_chain(),
    Fork =
        test_branch(
            <<"fork-">>,
            test_hash(<<"main-90">>),
            91,
            100,
            1900,
            10,
            Opts
        ),
    ?assertEqual(90, branch_point(Fork, Tip, 100, Opts)),
    ?assertEqual(cumulative_diff(Tip, Opts), cumulative_diff(Fork, Opts)),
    ?assertEqual(Tip, choose(Tip, [Fork], Opts)).

%% @doc A branch far heavier than the incumbent still loses when it forks
%% deeper than `?CHECKPOINT_DEPTH' below the tip. This is the only finality
%% rule there is. The branch really does fork one block below the boundary --
%% its lowest block names the incumbent block there as its parent -- and fork
%% choice still declines to find, or act on, a branch point that deep.
fork_choice_rejects_branch_below_checkpoint_depth_test() ->
    {Opts, Tip} = test_chain(),
    Height = test_ineligible_fork_height(),
    Parent = test_hash(<< "main-", (hb_util:bin(Height))/binary >>),
    Fork = test_branch(<<"deep-">>, Parent, Height + 1, 100, 5000, 10, Opts),
    ?assertEqual(
        Parent,
        parent(test_hash(<< "deep-", (hb_util:bin(Height + 1))/binary >>), Opts)
    ),
    ?assert(cumulative_diff(Fork, Opts) > cumulative_diff(Tip, Opts)),
    ?assertEqual([], branch_point(Fork, Tip, 100, Opts)),
    ?assertEqual(Tip, choose(Tip, [Fork], Opts)).

%% @doc `validated' serves the states this node produced, and nothing else.
%%
%% The last assertion is the one that gives the key meaning.
%% `~arweave@2.9/block' answers a cache miss by fetching from a gateway, so
%% "I got a block back" says
%% nothing about who checked it. This key has no fallback, and the test pins
%% that absence: a hash the node never validated must answer `not-validated'
%% rather than any block at all. The peer list is empty here, so a fallback
%% could not even succeed -- it would fail differently, and asserting the
%% specific message is what tells the two apart.
validated_serves_only_verified_blocks_test() ->
    {Opts, Tip} = test_chain(),
    Base = #{ <<"device">> => <<"arweave@2.9">> },
    Resolve =
        fun(Block) ->
            hb_ao:resolve(
                Base,
                #{ <<"path">> => <<"validated">>, <<"block">> => Block },
                Opts
            )
        end,
    Field =
        fun(Key, State) ->
            hb_maps:get(
                Key,
                hb_maps:get(<<"block">>, State, #{}, Opts),
                not_found,
                Opts
            )
        end,
    % A block this node validated resolves by its hash.
    {ok, State} = Resolve(test_hash(<<"main-50">>)),
    ?assertEqual(50, hb_util:int(Field(<<"height">>, State))),
    ?assertEqual(test_hash(<<"main-50">>), Field(<<"indep-hash">>, State)),
    % `current' agrees with `tip'.
    {ok, Current} = Resolve(<<"current">>),
    {ok, FromTip} = hb_ao:resolve(Base, #{ <<"path">> => <<"tip">> }, Opts),
    ?assertEqual(
        Field(<<"indep-hash">>, FromTip),
        Field(<<"indep-hash">>, Current)
    ),
    ?assertEqual(Tip, Field(<<"indep-hash">>, Current)),
    % A block this node never validated is refused, not fetched.
    ?assertEqual(
        <<"not-validated">>,
        case Resolve(test_hash(<<"never-seen-this-block">>)) of
            {error, Error} -> hb_maps:get(<<"message">>, Error, none, Opts);
            Other -> Other
        end
    ).

%% @doc The eligibility rule itself, asserted on the arithmetic rather than
%% through a chain. `eligible/2' is reached only with a branch point
%% `branch_point/4' was willing to return, and `branch_point/4' stops searching
%% at the very height `eligible/2' refuses -- so a test that goes through a
%% chain cannot tell the two apart, and neither would notice the rule moving by
%% a block. Both read `eligible_height/1', and this is what pins it.
fork_choice_eligibility_boundary_test() ->
    ?assertEqual(100 - ?CHECKPOINT_DEPTH + 1, eligible_height(100)),
    ?assert(eligible(100 - ?CHECKPOINT_DEPTH + 1, 100)),
    ?assertNot(eligible(100 - ?CHECKPOINT_DEPTH, 100)),
    ?assertNot(eligible([], 100)).

%% @doc The mutation partner: the same heavier branch forking one block
%% shallower -- at `eligible_height/1' exactly -- is eligible and wins.
%% Together with the test above this pins the boundary rather than merely
%% asserting that deep branches lose.
fork_choice_accepts_branch_at_checkpoint_depth_test() ->
    {Opts, Tip} = test_chain(),
    Height = test_eligible_fork_height(),
    Fork =
        test_branch(
            <<"edge-">>,
            test_hash(<< "main-", (hb_util:bin(Height))/binary >>),
            Height + 1,
            100,
            5000,
            10,
            Opts
        ),
    ?assertEqual(Height, branch_point(Fork, Tip, 100, Opts)),
    ?assertEqual(Fork, choose(Tip, [Fork], Opts)).

%% @doc A candidate that has run ahead of the tip is walked down to the tip's
%% height before the two are stepped together, so a node catching up still
%% finds its branch point. A step budget sized for the reorg window alone would
%% run out during the descent and reject every candidate a sync produces.
fork_choice_finds_branch_point_above_the_tip_test() ->
    {Opts, Tip} = test_chain(),
    Ahead = test_branch(<<"ahead-">>, Tip, 101, 150, 2000, 10, Opts),
    ?assertEqual(100, branch_point(Ahead, Tip, 100, Opts)),
    ?assertEqual(Ahead, choose(Tip, [Ahead], Opts)).

%% @doc Fork choice with no candidates is the identity on the stored tip, which
%% is what makes `tip' safe to resolve at any time.
fork_choice_without_candidates_keeps_tip_test() ->
    {Opts, Tip} = test_chain(),
    ?assertEqual(Tip, choose(Tip, [], Opts)).

%% @doc A candidate this node has no chain state for has no branch point, and
%% so cannot win however heavy it claims to be.
fork_choice_ignores_unknown_candidate_test() ->
    {Opts, Tip} = test_chain(),
    ?assertEqual([], branch_point(<<"unknown">>, Tip, 100, Opts)),
    ?assertEqual(Tip, choose(Tip, [<<"unknown">>], Opts)).

%% @doc A block takes the tip as soon as it is applied, not at the end of the
%% pass that applied it. This is what keeps a node closing a thirty-block gap
%% from being indistinguishable from a stalled one for twenty-five minutes.
adopt_moves_the_tip_per_block_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    _Chain =
        test_branch(
            <<"main-">>,
            test_hash(<<"genesis">>),
            1,
            100,
            1000,
            10,
            Opts
        ),
    hb_cache:link(state_path(test_hash(<<"main-99">>)), ?TIP_PATH, Opts),
    ?assertEqual({ok, test_hash(<<"main-99">>)}, incumbent(Opts)),
    ?assertEqual(ok, adopt(test_hash(<<"main-100">>), Opts)),
    ?assertEqual({ok, test_hash(<<"main-100">>)}, incumbent(Opts)).

%% @doc Adoption is still fork choice: a block does not take the tip merely by
%% having been applied. Its branch has to be heavier.
adopt_refuses_a_lighter_block_test() ->
    {Opts, Tip} = test_chain(),
    Lighter =
        test_branch(
            <<"light-">>,
            test_hash(<<"main-99">>),
            100,
            100,
            1000,
            0,
            Opts
        ),
    ?assert(cumulative_diff(Lighter, Opts) < cumulative_diff(Tip, Opts)),
    ?assertEqual(ok, adopt(Lighter, Opts)),
    ?assertEqual({ok, Tip}, incumbent(Opts)).

%% @doc The branch point between two branches is the last block they shared.
branch_point_is_the_last_shared_block_test() ->
    {Opts, Tip} = test_chain(),
    Fork =
        test_branch(
            <<"fork-">>,
            test_hash(<<"main-95">>),
            96,
            100,
            2000,
            10,
            Opts
        ),
    ?assertEqual(95, branch_point(Fork, Tip, 100, Opts)).

%% @doc The paging plan covers `[0, Height]' exactly: contiguous, ordered, no
%% gap and no overlap. A hole here would be a hole in the weave.
block_index_ranges_are_contiguous_test() ->
    Ranges = ranges(0, 12345, 5000),
    ?assertEqual([{0, 4999}, {5000, 9999}, {10000, 12345}], Ranges),
    ?assertEqual(
        12346,
        lists:sum([ (End - Start) + 1 || {Start, End} <- Ranges ])
    ).

%% @doc A range shorter than a page is one page, and an empty range is none.
block_index_ranges_edge_cases_test() ->
    ?assertEqual([{0, 0}], ranges(0, 0, 5000)),
    ?assertEqual([], ranges(1, 0, 5000)).

%% @doc Rotation spreads the first attempt without dropping any peer, so a page
%% whose preferred peer is down still falls back through all the others.
peer_rotation_preserves_every_peer_test() ->
    Peers = [<<"a">>, <<"b">>, <<"c">>],
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], rotate(Peers, 0)),
    ?assertEqual([<<"b">>, <<"c">>, <<"a">>], rotate(Peers, 1)),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], rotate(Peers, 3)),
    ?assertEqual([], rotate([], 2)),
    lists:foreach(
        fun(N) ->
            ?assertEqual(lists:sort(Peers), lists:sort(rotate(Peers, N)))
        end,
        lists:seq(0, 7)
    ).

%% @doc The reward history decoder, on the shape of a real response. The
%% address is fixed-width and the two amounts are length-prefixed, so a misread
%% of either would shift every later entry -- which is why a body that is not a
%% whole number of entries is an error rather than a truncation.
reward_history_decodes_test() ->
    Addr = crypto:strong_rand_bytes(32),
    ?assertEqual(
        {ok, [{Addr, 159839725, 355263029606, 1}, {Addr, 7, 9, 1}]},
        decode_rewards(
            <<
                Addr/binary, 4:8, 159839725:32, 5:8, 355263029606:40, 1:24,
                Addr/binary, 1:8, 7:8, 1:8, 9:8, 1:24
            >>,
            []
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-reward-history">> }},
        decode_rewards(<<0, 1, 2>>, [])
    ).

%% @doc The block-time history decoder, on the first three entries of a real
%% mainnet response.
block_time_history_decodes_test() ->
    ?assertEqual(
        {ok, [{123, 112, 2}, {87, 86, 2}, {116, 77, 2}]},
        decode_times(
            <<
                1:8, 123:8, 1:8, 112:8, 1:8, 2:8,
                1:8, 87:8, 1:8, 86:8, 1:8, 2:8,
                1:8, 116:8, 1:8, 77:8, 1:8, 2:8
            >>,
            []
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-block-time-history">> }},
        decode_times(<<1, 1, 1>>, [])
    ).

%% @doc A `sync' pass over a range this node has already applied fetches no
%% block and moves no pointer, and a second pass returns exactly what the first
%% did. This is the idempotence property stated directly: the mock peer counts
%% the block requests that would prove otherwise.
sync_over_known_range_is_a_no_op_test() ->
    {Opts, Tip} = test_chain(),
    {ok, Peer, Handle} =
        hb_mock_server:start([
            {"/info", info,
                {200,
                    hb_json:encode(
                        #{ <<"height">> => 100, <<"current">> => Tip }
                    )
                }
            },
            {"/block2/hash/[...]", block, {404, <<>>}}
        ]),
    try
        PeerOpts = Opts#{ <<"arweave-untrusted-peers">> => [Peer] },
        First = do_sync(#{}, PeerOpts),
        Second = do_sync(#{}, PeerOpts),
        ?assertMatch(
            {ok, #{ <<"applied">> := 0, <<"indep-hash">> := Tip }},
            First
        ),
        ?assertEqual(First, Second),
        ?assertEqual([], hb_mock_server:get_requests(Handle, block))
    after
        hb_mock_server:stop(Handle)
    end.

%% @doc A node that has not bootstrapped has nothing to extend, and says so
%% rather than inventing a tip.
sync_without_bootstrap_is_an_error_test() ->
    ?assertMatch(
        {error, #{ <<"message">> := <<"not-bootstrapped">> }},
        do_sync(
            #{},
            #{
                <<"store">> => [hb_test_utils:test_store()],
                <<"arweave-untrusted-peers">> => [<<"http://localhost:1">>]
            }
        )
    ).

%% @doc A node with no peers configured is told so, rather than reaching for a
%% hardcoded host.
peers_default_to_none_test() ->
    ?assertMatch({error, #{ <<"message">> := <<"no-peers">> }}, peers(#{})).

%% @doc A checkpoint below the 2.9 fork is refused: its blocks carry proofs
%% this subsystem does not validate.
checkpoint_below_fork_is_refused_test() ->
    ?assertEqual(ok, above_fork(ar_fork:height_2_9())),
    ?assertMatch(
        {error, #{ <<"message">> := <<"checkpoint-below-fork">> }},
        above_fork(ar_fork:height_2_9() - 1)
    ).

%%% Live probes. These talk to mainnet, so they are not part of the suite and
%%% carry no `_test' suffix; run one by name with `rebar3 device test --devices
%%% dev_arweave --test all:<name>'. They exist because everything above this
%%% line is either pure or mocked, and a peer protocol is only ever really
%%% tested against a peer.

%% @doc Each probe is a generator carrying its own timeout, because validating
%% one mainnet block costs ~45 seconds of VDF and a bootstrap costs a minute --
%% both comfortably past what EUnit allows a plain test. The trailing
%% underscore is what makes `--test all:live_sync' resolve to the generator;
%% it is deliberately not `_test_', so the suite does not pick these up.
live_peers_() -> {timeout, 600, fun live_peers/0}.
live_block_index_() -> {timeout, 900, fun live_block_index/0}.
live_bootstrap_() -> {timeout, 900, fun live_bootstrap/0}.
live_sync_() -> {timeout, 1800, fun live_sync/0}.
live_apply_() -> {timeout, 1800, fun live_apply/0}.

%% @doc The four mainnet peers these probes read from.
mainnet_peers() ->
    [
        <<"http://tip-1.arweave.xyz:1984">>,
        <<"http://tip-2.arweave.xyz:1984">>,
        <<"http://tip-3.arweave.xyz:1984">>,
        <<"http://tip-4.arweave.xyz:1984">>
    ].

%% @doc The probes share one on-disk store rather than a fresh temporary one,
%% for two reasons. A bootstrap costs a minute and 155 MiB of paging, and
%% repeating it per probe wastes the peers' bandwidth as much as ours. More to
%% the point, resumability across processes is the property `sync' claims: a
%% probe that reads a chain state written by an earlier run, in an earlier VM,
%% is testing that claim rather than asserting it.
mainnet_opts() ->
    #{
        <<"store">> =>
            [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-arweave-probe">>
                }
            ],
        <<"arweave-untrusted-peers">> => mainnet_peers(),
        % A shared-ancestor bootstrap needs an explicit trusted list; these
        % probes name the same peers for both, as an operator with no
        % checkpoint hash would.
        <<"arweave-trusted-peers">> => mainnet_peers()
    }.

%% @doc Bootstrap only if this store has no tip yet.
ensure_bootstrapped(Opts) ->
    case tip(#{}, #{}, Opts) of
        {ok, State} ->
            ?debugFmt(
                "reusing chain state at height ~p",
                [
                    int(
                        <<"height">>,
                        hb_maps:get(<<"block">>, State, #{}, Opts),
                        Opts
                    )
                ]
            ),
            {ok, State};
        _ ->
            timed("bootstrap", fun() -> do_bootstrap(#{}, Opts) end)
    end.

%% @doc Everything `bootstrap' does except the full block index: peer tip
%% discovery, the shared-ancestor walk across all four peers, the checkpoint
%% block and the recomputation of its hash, both carried histories against
%% their committed hashes, and the account-anchor probe.
live_peers() ->
    Opts = mainnet_opts(),
    {ok, Peers} = peers(Opts),
    {ok, TipHeight} = network_height(Peers, Opts),
    ?debugFmt("network height: ~p", [TipHeight]),
    {ok, Hash} = shared_ancestor(Peers, TipHeight - 50, Opts),
    ?debugFmt("shared ancestor at ~p: ~s", [TipHeight - 50, Hash]),
    {ok, Block} = identified_block(Peers, Hash, Opts),
    ?debugFmt(
        "checkpoint block verified: height ~p, cumulative-diff ~p",
        [
            int(<<"height">>, Block, Opts),
            hb_maps:get(<<"cumulative-diff">>, Block, none, Opts)
        ]
    ),
    ?assertEqual(TipHeight - 50, int(<<"height">>, Block, Opts)),
    {ok, Rewards, Times} = histories(Peers, Block, Hash, Opts),
    {ok, RewardList} = decode_rewards(Rewards, []),
    {ok, TimeList} = decode_times(Times, []),
    ?debugFmt(
        "histories verified: ~p reward entries (~p bytes), "
        "~p block-time entries (~p bytes)",
        [
            length(RewardList), byte_size(Rewards),
            length(TimeList), byte_size(Times)
        ]
    ),
    ok = account_anchor(Peers, int(<<"height">>, Block, Opts), TipHeight, Opts),
    {ok, Anchor} = hb_cache:read(?ANCHOR_PATH, Opts),
    ?debugFmt(
        "account anchor at height ~p, ~p blocks below the tip",
        [
            int(<<"height">>, Anchor, Opts),
            TipHeight - int(<<"height">>, Anchor, Opts)
        ]
    ),
    ?assert(int(<<"height">>, Anchor, Opts) >= TipHeight - 100).

%% @doc The full block index from genesis, assembled and proven against the
%% checkpoint block's `hash-list-merkle', with the assembled index spot-checked
%% for ordering: the entry the index reports at a height must be the block the
%% peers report at that height.
live_block_index() ->
    Opts = mainnet_opts(),
    {ok, Peers} = peers(Opts),
    {ok, TipHeight} = network_height(Peers, Opts),
    Height = TipHeight - 50,
    {ok, Hash} = height_hash(Peers, Height, Opts),
    {ok, Block} = identified_block(Peers, Hash, Opts),
    Started = os:system_time(millisecond),
    {ok, Index} = block_index(Peers, Block, Height, Opts),
    Length = hb_util:int(hb_maps:get(<<"length">>, Index, 0, Opts)),
    ?debugFmt(
        "block index: ~p entries assembled and verified in ~p ms",
        [Length, os:system_time(millisecond) - Started]
    ),
    ?assertEqual(Height + 1, Length),
    lists:foreach(
        fun(At) ->
            {ok, Expected} = height_hash(Peers, At, Opts),
            {ok, Entry} =
                hb_ao:resolve(
                    Index,
                    #{ <<"path">> => <<"at">>, <<"height">> => At },
                    Opts
                ),
            ?assertEqual(
                Expected,
                hb_maps:get(<<"indep-hash">>, Entry, none, Opts)
            )
        end,
        [0, 1, ar_fork:height_2_9(), Height - 1, Height]
    ).

%% @doc Bootstrap, then advance the chain by a single block and prove the
%% result: the tip moves on by exactly one, the new state's block names the old
%% tip as its parent, and a second pass over the same range applies nothing.
%% This is the smallest run that exercises `~arweave-block@2.9/apply', which is
%% where the whole cost of a sync lives.
live_sync() ->
    Opts = maps:put(<<"arweave-sync-batch">>, 3, mainnet_opts()),
    {ok, Peers} = peers(Opts),
    {ok, Before} = ensure_bootstrapped(Opts),
    Block = hb_maps:get(<<"block">>, Before, #{}, Opts),
    Start = int(<<"height">>, Block, Opts),
    Parent = hb_maps:get(<<"indep-hash">>, Block, none, Opts),
    % Report what the block being applied actually contains, so a run over an
    % empty block is not mistaken for one that exercised the transaction path.
    {ok, NextHash} = height_hash(Peers, Start + 1, Opts),
    {ok, Next} = peer_block(Peers, NextHash, Opts),
    ?debugFmt(
        "block ~p carries ~p transactions",
        [
            Start + 1,
            length(
                hb_util:message_to_ordered_list(
                    hb_maps:get(<<"txs">>, Next, [], Opts),
                    Opts
                )
            )
        ]
    ),
    {ok, First} =
        timed(
            "sync",
            fun() ->
                hb_ao:resolve(
                    #{ <<"device">> => <<"arweave@2.9">> },
                    #{ <<"path">> => <<"sync">> },
                    Opts
                )
            end
        ),
    ?debugFmt(
        "sync from ~p: applied ~p, now at height ~p",
        [
            Start,
            hb_maps:get(<<"applied">>, First, none, Opts),
            hb_maps:get(<<"height">>, First, none, Opts)
        ]
    ),
    Applied = hb_util:int(hb_maps:get(<<"applied">>, First, 0, Opts)),
    Tip = hb_maps:get(<<"indep-hash">>, First, none, Opts),
    ?assert(Applied >= 1),
    ?assertEqual(
        Start + Applied,
        hb_util:int(hb_maps:get(<<"height">>, First, 0, Opts))
    ),
    % Walking the new tip back by as many blocks as were applied must land on
    % the block the pass started from. Every state in between therefore exists
    % and names its parent, which is the only evidence that a multi-block pass
    % applied them in ancestor order rather than merely ending in the right
    % place -- a pass that applied them out of order could not have found each
    % block's parent state to apply it onto.
    ?assertEqual(
        Parent,
        lists:foldl(
            fun(_, Hash) -> parent(Hash, Opts) end,
            Tip,
            lists:seq(1, Applied)
        )
    ),
    % Re-running over the same range must apply nothing and leave the same tip.
    {ok, Second} =
        timed(
            "second sync",
            fun() -> do_sync(#{}, Opts#{ <<"arweave-sync-batch">> => 0 }) end
        ),
    ?assertEqual(0, hb_util:int(hb_maps:get(<<"applied">>, Second, 1, Opts))),
    ?assertEqual(Tip, hb_maps:get(<<"indep-hash">>, Second, none, Opts)).

%% @doc Apply exactly one block onto the stored tip and report the result
%% verbatim. Separated from `live_sync/0' because `sync' treats a block that
%% fails as that peer's problem and moves on, which is right for a running node
%% and useless for finding out why.
live_apply() ->
    Opts = mainnet_opts(),
    {ok, Peers} = peers(Opts),
    {ok, State} = ensure_bootstrapped(Opts),
    Block = hb_maps:get(<<"block">>, State, #{}, Opts),
    Height = int(<<"height">>, Block, Opts),
    {ok, NextHash} = height_hash(Peers, Height + 1, Opts),
    {ok, Next} = peer_block(Peers, NextHash, Opts),
    {ok, TXs} = transactions(Peers, Next, Opts),
    ?debugFmt(
        "applying ~p onto ~p, ~p transactions, accounts=~p",
        [
            Height + 1,
            Height,
            length(TXs),
            case hb_maps:get(<<"accounts">>, State, [], Opts) of
                [] -> absent;
                _ -> present
            end
        ]
    ),
    Result =
        timed(
            "apply",
            fun() ->
                hb_ao:resolve(
                    State#{ <<"device">> => <<"arweave-block@2.9">> },
                    #{
                        <<"path">> => <<"apply">>,
                        <<"next">> => Next,
                        <<"transactions">> => TXs
                    },
                    Opts
                )
            end
        ),
    ?debugFmt("apply result: ~p", [element(1, Result)]),
    ?assertMatch({ok, _}, Result).

%% @doc Run a stage, reporting how long it took as it finishes rather than at
%% the end of the probe, so that a run which never finishes still says how far
%% it got.
timed(Name, Fun) ->
    Started = os:system_time(millisecond),
    Result = Fun(),
    ?debugFmt("~s: ~p ms", [Name, os:system_time(millisecond) - Started]),
    Result.

%% @doc A whole `bootstrap' against mainnet, resolved as a device key, and the
%% tip it leaves behind.
live_bootstrap() ->
    Opts = mainnet_opts(),
    Started = os:system_time(millisecond),
    {ok, State} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"bootstrap">> },
            Opts
        ),
    Block = hb_maps:get(<<"block">>, State, #{}, Opts),
    ?debugFmt(
        "bootstrapped to height ~p in ~p ms",
        [int(<<"height">>, Block, Opts), os:system_time(millisecond) - Started]
    ),
    % The account tree must be attached to the state, not merely fetched. An
    % empty `accounts' link is what disables the account checks, so a bootstrap
    % that verified a tree and then dropped it would leave the node validating
    % everything except the property the tree exists for -- and would do it
    % silently.
    ?assertMatch(
        #{ <<"root">> := _ },
        hb_cache:ensure_loaded(
            hb_maps:get(<<"accounts">>, State, [], Opts),
            Opts
        )
    ),
    {ok, Tip} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"tip">> },
            Opts
        ),
    ?assertEqual(
        hb_maps:get(<<"indep-hash">>, Block, none, Opts),
        hb_maps:get(
            <<"indep-hash">>,
            hb_maps:get(<<"block">>, Tip, #{}, Opts),
            none,
            Opts
        )
    ).

%% @doc `bootstrap' is reachable by anyone the node answers, so an unguarded one
%% is a repeatable trust reset: it re-asks the peers where the chain starts and
%% relinks the tip onto their answer, discarding whatever the node had validated
%% for itself. A node that already has a chain is refused whichever side of its
%% tip the new checkpoint falls on -- a checkpoint a few blocks ahead is still
%% blocks taken on trust that the node could have validated -- and only a
%% deliberate `force' overrides that.
bootstrap_refuses_on_an_established_chain_test() ->
    {Opts, _Tip} = test_chain(),
    Stored = stored_height(Opts),
    ?assertEqual(100, Stored),
    ?assertMatch({error, #{ <<"message">> := <<"already-bootstrapped">> }},
        unbootstrapped(false, Stored)),
    ?assertEqual(ok, unbootstrapped(true, Stored)),
    % A node with no chain of its own has nothing to protect, so the first
    % bootstrap is never the one that is refused.
    ?assertEqual(ok, unbootstrapped(false, [])).

%% @doc A request cannot shape a store path.
%%
%% `validated' and `tip' both turn a caller-supplied value into a path under
%% `~arweave@2.9/state/'. Nothing between here and the filesystem collapses
%% `..' -- `hb_path:to_binary/1' leaves it alone and `hb_store_fs' walks the
%% components -- so before this was guarded, `block=../../../hyperbeam-key.json'
%% read the operator's signing key off disk, unauthenticated, from a device key
%% whose whole purpose is to answer strangers.
%%
%% Asserted at both layers: the request boundary refuses the value, and
%% `state_path/1' refuses to build a path out of one even if a future caller
%% forgets to check.
request_cannot_shape_a_store_path_test() ->
    Traversals =
        [
            <<"../../../hyperbeam-key.json">>,
            % 64 characters, so it decodes to exactly 48 bytes and passes a
            % length check. `hb_util:decode/1' is the unchecked decoder and
            % validates no alphabet, so only checking the decoded size let this
            % through to the chokepoint, where it surfaced as an uncaught throw
            % instead of this device's own `invalid-block'.
            << "../../../hyperbeam-key.json",
                (binary:copy(<<"A">>, 37))/binary >>,
            <<"..">>,
            <<"a/b">>,
            <<"/etc/passwd">>,
            << 0 >>,
            % Well-formed base64url, but not 48 bytes: still not a block hash.
            hb_util:encode(<<"short">>)
        ],
    lists:foreach(
        fun(Bad) ->
            ?assertMatch(
                {error, #{ <<"message">> := <<"invalid-block">> }},
                validated_hash(#{ <<"block">> => Bad }, #{})
            ),
            ?assertMatch(
                {error, #{ <<"message">> := <<"invalid-block">> }},
                candidate_hashes([Bad])
            )
        end,
        Traversals
    ),
    % The chokepoint refuses a separator whatever the caller did.
    ?assertThrow({unsafe_state_path, _}, state_path(<<"../../secret">>)),
    ?assertThrow({unsafe_state_path, _}, state_path(<<"a/b">>)),
    % A real block hash is accepted at both layers.
    Good = hb_util:encode(crypto:strong_rand_bytes(48)),
    ?assertEqual({ok, Good}, validated_hash(#{ <<"block">> => Good }, #{})),
    ?assertEqual({ok, [Good]}, candidate_hashes([Good])),
    ?assertMatch(<<"~arweave@2.9/state/", _/binary>>, state_path(Good)).

%% @doc `force' is honoured from the node message and ignored from the request.
%%
%% `bootstrap' is reachable by anyone the node answers, and `force' discards a
%% chain this node validated for itself and re-anchors on whatever the peer set
%% says. Reading it from the request made the subsystem's one guarded trust
%% boundary unguarded again for any caller who could reach the port: the
%% guard would refuse the accidental second bootstrap and wave through the
%% deliberate one from a stranger.
%%
%% Pinned on `forced/2' rather than through `bootstrap/3' because the property
%% is about where the value is read, and a test that went through the device
%% would need a peer set to get far enough to observe it.
force_is_not_request_supplied_test() ->
    % A request asking for it, with nothing in the node message: refused.
    ?assertEqual(false, forced(#{ <<"force">> => true }, #{})),
    ?assertEqual(false, forced(#{ <<"force">> => <<"true">> }, #{})),
    % The node message asking for it: honoured, whichever way it is written.
    ?assertEqual(
        true,
        forced(#{}, #{ <<"arweave-force-bootstrap">> => true })
    ),
    ?assertEqual(
        true,
        forced(#{}, #{ <<"arweave-force-bootstrap">> => <<"true">> })
    ),
    % A request cannot override a node message that says no.
    ?assertEqual(
        false,
        forced(
            #{ <<"force">> => true },
            #{ <<"arweave-force-bootstrap">> => false }
        )
    ),
    % Absent everywhere: no.
    ?assertEqual(false, forced(#{}, #{})).

%% @doc A bootstrap that cannot anchor the account tree fails, rather than
%% producing a node that can never sync.
%%
%% This pins a liveness trap. An anchor recorded at checkpoint+k attaches only
%% to the block whose hash matches it, so the checkpoint state would keep
%% `accounts => []'; applying checkpoint+1 then reads a parent with no tree,
%% `~arweave-block@2.9/apply' refuses it with `accounts-not-checked', and the
%% block is skipped -- so the anchor block is never reached and the tree never
%% attaches. Searching upward on a failed anchor would therefore turn one
%% transient peer error during the wallet-list walk into a node that cannot
%% sync at all, recoverable only by setting `arweave-force-bootstrap' and
%% restarting. Failing the bootstrap outright is the safe answer.
anchor_failure_fails_the_bootstrap_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    % A peer that answers nothing stands for every way the walk can fail: a
    % pruned root, a 404 on a page, a timeout mid-walk.
    Peers = [<<"http://127.0.0.1:1">>],
    ?assertMatch(
        {error, #{ <<"message">> := <<"no-account-anchor">> }},
        account_anchor(Peers, 100, 130, Opts)
    ).

%% @doc Only the trusted peers may decide where a shared-ancestor bootstrap
%% anchors. A node that names a checkpoint block trusts no peer at all, so it
%% needs no trusted list; a node that names neither has nothing to anchor on and
%% is told so rather than quietly asking whoever a previous bootstrap found.
trusted_peers_test() ->
    Trusted = [<<"http://trusted">>],
    Untrusted = [<<"http://untrusted">>],
    Both = #{
        <<"arweave-trusted-peers">> => Trusted,
        <<"arweave-untrusted-peers">> => Untrusted
    },
    Neither = #{ <<"arweave-untrusted-peers">> => Untrusted },
    ?assertEqual({ok, Trusted}, trusted_peers(#{}, Both)),
    ?assertMatch({error, #{ <<"message">> := <<"no-trusted-peers">> }},
        trusted_peers(#{}, Neither)),
    % A node that names a checkpoint in its **node message** needs no trusted
    % list: the hash is the trust root and no peer is asked where the chain
    % starts.
    ?assertEqual(
        {ok, Untrusted},
        trusted_peers(
            #{},
            Neither#{ <<"arweave-checkpoint-block">> => <<"anything">> }
        )
    ),
    % The same value in the *request* is ignored. `bootstrap' is
    % unauthenticated, so honouring a request-supplied checkpoint would let any
    % caller who can reach the port choose the single hash the entire chain is
    % anchored to.
    ?assertMatch({error, #{ <<"message">> := <<"no-trusted-peers">> }},
        trusted_peers(#{ <<"checkpoint-block">> => <<"anything">> }, Neither)).

%% @doc The wallet-list walk cannot be made to run for ever.
%%
%% The peer chooses the cursor, so only a bound here ends the walk when the peer
%% will not. This pins the terminating clause and the message it answers with;
%% pinning the decrement end to end would need a peer that serves
%% `/wallet_list' pages, and there is no stub for one.
wallet_list_is_bounded_test() ->
    ?assert(?MAX_WALLET_LIST_PAGES > 125),
    ?assertMatch(
        {error, #{ <<"message">> := <<"wallet-list-too-long">> }},
        wallet_list([], <<"root">>, <<"cursor">>, #{}, 0, #{})
    ),
    % A peer that has finished is answered before the bound is consulted, so an
    % honest short tree is not refused by an exhausted counter.
    ?assertEqual(
        {ok, #{}},
        wallet_list([], <<"root">>, <<"last">>, #{}, 0, #{})
    ).

%% @doc `block_hash/1' is exported so that `dev_arweave' can refuse on `block'
%% what this module refuses on `validated'. Both keys reach `hb_cache:read/2'
%% with a caller-supplied string, and a length test alone lets a traversal
%% through: 64 characters of `..' and `/' are 64 bytes.
block_hash_is_the_shared_definition_test() ->
    ?assertMatch({ok, _}, block_hash(binary:copy(<<"a">>, 64))),
    ?assertMatch({error, _}, block_hash(binary:copy(<<"a">>, 63))),
    ?assertMatch({error, _}, block_hash(<<"a/../b">>)),
    ?assertMatch({error, _}, block_hash(not_a_binary)),
    % 64 bytes, so the length gate admits it; no character is in the base64URL
    % alphabet, so the shared definition does not.
    Traversal = << (binary:copy(<<"../">>, 21))/binary, "x" >>,
    ?assertEqual(64, byte_size(Traversal)),
    ?assertMatch({error, #{ <<"message">> := <<"invalid-block">> }},
        block_hash(Traversal)).
