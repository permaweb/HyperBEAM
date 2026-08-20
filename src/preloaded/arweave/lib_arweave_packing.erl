%%% @doc The RandomX state Arweave's packing is built from, and the operations
%%% that take a chunk between its unpacked and its packed form.
%%%
%%% RandomX state is the one genuinely process-bound resource in the subsystem:
%%% a NIF resource costing a second to build in light mode and minutes in fast
%%% mode, holding a gibibyte or more. It is keyed on `?RANDOMX_PACKING_KEY', a
%%% fixed protocol constant, so a state built once stays valid for the life of
%%% the network -- which is why it is a singleton rather than a cache. The three
%%% variants are independent, so a node pays only for the ones it uses.
%%%
%%% Both the device that proves access to a chunk and the one that stores it
%%% need that state, so it is owned here rather than by either of them. A second
%%% owner would mean a second gibibyte and a second minute to build it.
-module(lib_arweave_packing).
-export([state/2, variant/1, mode/1]).
-export([entropy/4, entropies/3, encipher/2, decipher/2, pad/1]).
-include("include/hb.hrl").
-include("include/ar.hrl").
-include("include/ar_consensus.hrl").

%% @doc Return an `ar_packing_server' packing state carrying an initialised
%% RandomX state for `Variant', starting the singleton that owns it if this is
%% its first use.
%%
%% `hb_name' is BEAM-global, so the name is scoped by the node's address: nodes
%% share a BEAM throughout the test suite, and two of them wanting different
%% modes must not be handed each other's state.
state(Variant, Opts) ->
    Mode = mode(hb_opts:get(<<"arweave-randomx-mode">>, <<"light">>, Opts)),
    Name =
        {
            arweave_randomx,
            Variant,
            Mode,
            node_scope(hb_opts:get(priv_wallet, [], Opts))
        },
    % Named for what it is: every caller asking for the state, not the state
    % being built. The build happens once, inside the singleton below. Read as
    % a creation, a run of these looks like a singleton dying and respawning --
    % which is a diagnosis this event has already caused once.
    ?event(arweave_spora,
        {randomx_state_requested, {variant, Variant}, {mode, Mode}},
        Opts
    ),
    PID = hb_name:singleton(Name, fun() -> owner(Variant, Mode) end),
    Ref = monitor(process, PID),
    PID ! {randomx_state, self(), Ref},
    receive
        {randomx_state, Ref, PackingState} ->
            demonitor(Ref, [flush]),
            PackingState;
        {'DOWN', Ref, process, PID, Reason} ->
            throw({'randomx-state-unavailable', Variant, Reason})
    end.

%% @doc Return the RandomX variant a packing format is built from.
variant({spora_2_6, _RewardAddr}) -> rx512;
variant({composite, _RewardAddr, _PackingDifficulty}) -> rx4096;
variant({replica_2_9, _RewardAddr}) -> rxsquared.

%% @doc Map the node's configured RandomX mode onto the atom the vendored
%% packing code takes. Mapped explicitly rather than coerced: the two atoms are
%% a closed vocabulary, and neither is guaranteed to be interned before the
%% packing code is first loaded -- which is exactly when this runs.
mode(<<"light">>) -> light;
mode(<<"fast">>) -> fast.

%% @doc Generate the entropy one sub-chunk of a bucket is enciphered with.
entropy(RewardAddr, BucketEndOffset, SubChunkStartOffset, Opts) ->
    Packing = {replica_2_9, RewardAddr},
    ar_packing_server:generate_replica_2_9_entropy(
        RewardAddr,
        BucketEndOffset,
        SubChunkStartOffset,
        ar_packing_server:get_randomx_state_by_packing(
            Packing,
            state(variant(Packing), Opts)
        )
    ).

%% @doc Generate the thirty-two entropies a bucket's footprint is built from,
%% one per sub-chunk of a chunk, in the order the vendored slicing expects.
%%
%% This is where a node preparing a partition spends nearly all of its time: one
%% entropy is a RandomX run over 8 MiB, and a partition needs thirty-two of them
%% for every two hundred and fifty-six mebibytes it holds. They are independent,
%% so they are generated in parallel up to `arweave-packing-workers'. The
%% RandomX state itself is shared: it is a reference-counted NIF resource that
%% the vendored code only reads.
entropies(RewardAddr, BucketEndOffset, Opts) ->
    Packing = {replica_2_9, RewardAddr},
    RandomXState =
        ar_packing_server:get_randomx_state_by_packing(
            Packing,
            state(variant(Packing), Opts)
        ),
    hb_pmap:parallel_map(
        lists:seq(
            0,
            ?DATA_CHUNK_SIZE - ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
            ?COMPOSITE_PACKING_SUB_CHUNK_SIZE
        ),
        fun(SubChunkStartOffset) ->
            ar_packing_server:generate_replica_2_9_entropy(
                RewardAddr,
                BucketEndOffset,
                SubChunkStartOffset,
                RandomXState
            )
        end,
        workers(Opts)
    ).

%% @doc Encipher a whole padded chunk with the whole entropy assembled for it.
encipher(Chunk, Entropy) ->
    ar_packing_server:encipher_replica_2_9_chunk(Chunk, Entropy).

%% @doc Decipher a stored chunk with the entropy it was enciphered under. The
%% replica-2.9 cipher is its own inverse, so this is the same operation.
decipher(Chunk, Entropy) ->
    ar_packing_server:decipher_replica_2_9_chunk(Chunk, Entropy).

%% @doc Extend a chunk to the full size the packing operates on.
pad(Chunk) ->
    ar_packing_server:pad_chunk(Chunk).

%%% Internal functions.

%% @doc How many entropies a node generates at once. Each is a RandomX run that
%% saturates one scheduler, so the default leaves half the machine for
%% everything else the node is doing.
workers(Opts) ->
    hb_util:int(
        hb_opts:get(
            <<"arweave-packing-workers">>,
            max(1, erlang:system_info(schedulers) div 2),
            Opts
        )
    ).

%% @doc Own a RandomX state for the life of the node. The state is built before
%% the first request is served, and then handed out unchanged: NIF resources are
%% reference-counted, so every caller may hold one, while the owner keeps the
%% underlying allocation alive.
owner(Variant, Mode) ->
    owner(ar_packing_server:init_packing_state(Mode, [Variant])).
owner(PackingState) ->
    receive
        {randomx_state, From, Ref} ->
            From ! {randomx_state, Ref, PackingState},
            owner(PackingState)
    end.

%% @doc Name the node a RandomX singleton belongs to. A node with no wallet
%% configured owns the `[]' scope, which is a scope like any other.
node_scope([]) -> [];
node_scope(Wallet) -> hb_util:human_id(ar_wallet:to_address(Wallet)).
