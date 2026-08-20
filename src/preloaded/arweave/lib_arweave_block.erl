%%% @doc The bridge between Arweave's block record family -- `#block{}',
%%% `#poa{}' and `#nonce_limiter_info{}' -- and the canonical AO-Core block
%%% message.
%%%
%%% Every conversion of those three records happens here and nowhere else. The
%%% field tables below are the single description of the mapping, so a key
%%% cannot drift between the record and the message.
%%%
%%% Binaries are base64url on the message, matching the rest of the Arweave
%%% device family.
-module(lib_arweave_block).
-export([from/2, to/2, to_header/2, with_transactions/2]).
-export([from_poa/2]).
-export([from_nonce_limiter/2]).
-export([selected/2, selected/3]).
-export([
    block_time_history_hash/2,
    check_block_time_history_hash/2,
    check_reward_history_hash/2,
    check_step_number/2,
    extend_block_index/3,
    holds/3,
    reward_history_hash/2,
    to_poa/2
]).
-include("include/hb.hrl").
-include("include/ar_consensus.hrl").

%% @doc The canonical block message key, `#block{}' field position and value
%% type for every field a block carries on the wire. Fields the record uses
%% internally -- `tx_tree', `size_tagged_txs', `reward_history',
%% `block_time_history', and the two proof caches -- are not wire fields and
%% are not carried by the message.
block_fields() ->
    [
        {<<"nonce">>, #block.nonce, nonce},
        {<<"previous-block">>, #block.previous_block, bin},
        {<<"timestamp">>, #block.timestamp, int},
        {<<"last-retarget">>, #block.last_retarget, int},
        {<<"diff">>, #block.diff, str_int},
        {<<"height">>, #block.height, int},
        {<<"hash">>, #block.hash, bin},
        {<<"indep-hash">>, #block.indep_hash, bin},
        {<<"txs">>, #block.txs, bins},
        {<<"tx-root">>, #block.tx_root, bin},
        {<<"wallet-list">>, #block.wallet_list, bin},
        {<<"reward-addr">>, #block.reward_addr, address},
        {<<"tags">>, #block.tags, bins},
        {<<"reward-pool">>, #block.reward_pool, str_int},
        {<<"weave-size">>, #block.weave_size, str_int},
        {<<"block-size">>, #block.block_size, str_int},
        {<<"cumulative-diff">>, #block.cumulative_diff, str_int},
        {<<"hash-list-merkle">>, #block.hash_list_merkle, bin},
        {<<"poa">>, #block.poa, poa},
        {<<"usd-to-ar-rate">>, #block.usd_to_ar_rate, rate},
        {<<"scheduled-usd-to-ar-rate">>,
            #block.scheduled_usd_to_ar_rate, rate},
        {<<"packing-2-5-threshold">>, #block.packing_2_5_threshold, str_int},
        {<<"strict-data-split-threshold">>,
            #block.strict_data_split_threshold, str_int},
        {<<"hash-preimage">>, #block.hash_preimage, bin},
        {<<"recall-byte">>, #block.recall_byte, opt_str_int},
        {<<"reward">>, #block.reward, str_int},
        {<<"previous-solution-hash">>, #block.previous_solution_hash, bin},
        {<<"partition-number">>, #block.partition_number, int},
        {<<"nonce-limiter-info">>, #block.nonce_limiter_info, nonce_limiter},
        {<<"poa2">>, #block.poa2, poa},
        {<<"recall-byte2">>, #block.recall_byte2, opt_str_int},
        {<<"signature">>, #block.signature, bin},
        {<<"reward-key">>, #block.reward_key, reward_key},
        {<<"price-per-gib-minute">>, #block.price_per_gib_minute, str_int},
        {<<"scheduled-price-per-gib-minute">>,
            #block.scheduled_price_per_gib_minute, str_int},
        {<<"reward-history-hash">>, #block.reward_history_hash, bin},
        {<<"debt-supply">>, #block.debt_supply, str_int},
        {<<"kryder-plus-rate-multiplier">>,
            #block.kryder_plus_rate_multiplier, str_int},
        {<<"kryder-plus-rate-multiplier-latch">>,
            #block.kryder_plus_rate_multiplier_latch, str_int},
        {<<"denomination">>, #block.denomination, str_int},
        {<<"redenomination-height">>, #block.redenomination_height, int},
        {<<"double-signing-proof">>,
            #block.double_signing_proof, double_signing_proof},
        {<<"previous-cumulative-diff">>, #block.previous_cumulative_diff, str_int},
        {<<"merkle-rebase-support-threshold">>,
            #block.merkle_rebase_support_threshold, str_int},
        {<<"chunk-hash">>, #block.chunk_hash, bin},
        {<<"chunk2-hash">>, #block.chunk2_hash, opt_bin},
        {<<"block-time-history-hash">>, #block.block_time_history_hash, bin},
        {<<"packing-difficulty">>, #block.packing_difficulty, int},
        {<<"unpacked-chunk-hash">>, #block.unpacked_chunk_hash, opt_bin},
        {<<"unpacked-chunk2-hash">>, #block.unpacked_chunk2_hash, opt_bin},
        {<<"replica-format">>, #block.replica_format, int}
    ].

%% @doc The fields of a proof of access. `unpacked-chunk' is empty at packing
%% difficulty 0 and carries the full 0-padded chunk above it.
poa_fields() ->
    [
        {<<"option">>, #poa.option, str_int},
        {<<"tx-path">>, #poa.tx_path, bin},
        {<<"data-path">>, #poa.data_path, bin},
        {<<"chunk">>, #poa.chunk, bin},
        {<<"unpacked-chunk">>, #poa.unpacked_chunk, bin}
    ].

%% @doc The fields of the nonce limiter info. `steps' and
%% `last-step-checkpoints' are newest-first, as they are on the wire.
nonce_limiter_fields() ->
    [
        {<<"output">>, #nonce_limiter_info.output, bin},
        {<<"global-step-number">>,
            #nonce_limiter_info.global_step_number, int},
        {<<"seed">>, #nonce_limiter_info.seed, bin},
        {<<"next-seed">>, #nonce_limiter_info.next_seed, bin},
        {<<"partition-upper-bound">>,
            #nonce_limiter_info.partition_upper_bound, int},
        {<<"next-partition-upper-bound">>,
            #nonce_limiter_info.next_partition_upper_bound, int},
        {<<"prev-output">>, #nonce_limiter_info.prev_output, bin},
        {<<"last-step-checkpoints">>,
            #nonce_limiter_info.last_step_checkpoints, bins},
        {<<"steps">>, #nonce_limiter_info.steps, bins},
        {<<"vdf-difficulty">>, #nonce_limiter_info.vdf_difficulty, str_int},
        {<<"next-vdf-difficulty">>,
            #nonce_limiter_info.next_vdf_difficulty, str_int}
    ].

%% @doc The nine elements of a double signing proof, in tuple order.
double_signing_proof_fields() ->
    [
        {<<"pub-key">>, 1, bin},
        {<<"sig1">>, 2, bin},
        {<<"cdiff1">>, 3, str_int},
        {<<"prev-cdiff1">>, 4, str_int},
        {<<"preimage1">>, 5, bin},
        {<<"sig2">>, 6, bin},
        {<<"cdiff2">>, 7, str_int},
        {<<"prev-cdiff2">>, 8, str_int},
        {<<"preimage2">>, 9, bin}
    ].

%%% Record conversion.

%% @doc Convert a block record into its canonical message form.
from(Block, Opts) ->
    from_record(block_fields(), Block, Block#block.height, Opts).

%% @doc Convert a canonical block message into a block record. Reads only the
%% keys the table names, so a message carrying links for its chunks and paths
%% loads exactly those and nothing else.
to(Msg, Opts) ->
    Height = hb_util:int(hb_maps:get(<<"height">>, Msg, 0, Opts)),
    to_record(block_fields(), Msg, #block{}, Height, Opts).

%% @doc Convert a canonical block message into a block record without its two
%% proofs of access, leaving them at the record's empty default. The proofs are
%% the only large part of a block -- two chunks and four Merkle paths -- so a
%% consumer that does not check them, such as every check performed against a
%% block's parent, reads the header alone and loads neither chunk.
to_header(Msg, Opts) ->
    Height = hb_util:int(hb_maps:get(<<"height">>, Msg, 0, Opts)),
    to_record(
        [ Field || Field = {_, _, Type} <- block_fields(), Type =/= poa ],
        Msg,
        #block{},
        Height,
        Opts
    ).

%% @doc Replace a block record's transaction identifiers with resolved
%% transaction records. `ar_block:verify_tx_root/1' and
%% `ar_block:verify_weave_size/3' read the data root and data size off each
%% one, which a block header does not carry.
%%
%% HyperBEAM's `#tx{}' and Arweave's have diverged in `format': a bundled item
%% carries the atom `ans104' where an L1 transaction carries an integer.
%% `ar_block:get_tx_data_root/1' matches on `format = 2', so an item reaching
%% it would fall through to the format-1 clause and silently produce a
%% different transaction root. This is the boundary that invariant crosses, so
%% it is asserted here: in consensus code a wrong answer is worse than none.
with_transactions(Block, TXs) ->
    [] = [ TX || TX <- TXs, not is_integer(TX#tx.format) ],
    Block#block{ txs = TXs }.

%% @doc Convert a proof of access record into its message form.
from_poa(PoA, Opts) ->
    from_record(poa_fields(), PoA, 0, Opts).

%% @doc Convert a proof of access message into its record form.
to_poa(Msg, Opts) ->
    to_record(poa_fields(), Msg, #poa{}, 0, Opts).

%% @doc Convert a nonce limiter info record into its message form.
from_nonce_limiter(Info, Opts) ->
    from_record(nonce_limiter_fields(), Info, 0, Opts).

%% @doc Convert a nonce limiter info message into its record form.
to_nonce_limiter(Msg, Opts) ->
    to_record(nonce_limiter_fields(), Msg, #nonce_limiter_info{}, 0, Opts).

%%% Internal functions.

%% @doc Project a record onto a message, dropping the fields whose value is
%% `undefined' -- the record's way of spelling "this block does not carry the
%% field", which the message spells by omitting the key.
from_record(Fields, Record, Height, Opts) ->
    maps:from_list(
        lists:filtermap(
            fun({Key, Index, Type}) ->
                case from_value(Type, element(Index, Record), Height, Opts) of
                    absent -> false;
                    Value -> {true, {Key, Value}}
                end
            end,
            Fields
        )
    ).

%% @doc Build a record from a message, leaving the record's own default in
%% place wherever the message omits the key.
to_record(Fields, Msg, Empty, Height, Opts) ->
    lists:foldl(
        fun({Key, Index, Type}, Record) ->
            case hb_maps:get(Key, Msg, absent, Opts) of
                absent -> Record;
                Value ->
                    setelement(
                        Index, Record, to_value(Type, Value, Height, Opts))
            end
        end,
        Empty,
        Fields
    ).

%% @doc Convert a record's value into its canonical message value. `undefined'
%% and the empty proof of access both mean "absent", and the nonce is held as
%% the integer it denotes rather than the big-endian bytes the wire carries.
from_value(_Type, undefined, _Height, _Opts) -> absent;
from_value(nonce, Nonce, _Height, _Opts) -> Nonce;
from_value(int, Int, _Height, _Opts) -> Int;
from_value(str_int, Int, _Height, _Opts) -> Int;
from_value(opt_str_int, Int, _Height, _Opts) -> Int;
from_value(bin, Bin, _Height, _Opts) -> hb_util:encode(Bin);
from_value(opt_bin, Bin, _Height, _Opts) -> hb_util:encode(Bin);
from_value(address, Addr, _Height, _Opts) -> hb_util:encode(Addr);
from_value(bins, Bins, _Height, _Opts) ->
    [ hb_util:encode(Bin) || Bin <- Bins ];
from_value(rate, {Dividend, Divisor}, _Height, _Opts) -> [Dividend, Divisor];
from_value(reward_key, {_Type, Pub}, _Height, _Opts) -> hb_util:encode(Pub);
from_value(poa, PoA, _Height, Opts) -> from_poa(PoA, Opts);
from_value(nonce_limiter, Info, _Height, Opts) ->
    from_nonce_limiter(Info, Opts);
from_value(double_signing_proof, Proof, Height, Opts) ->
    from_record(
        double_signing_proof_fields(),
        Proof,
        Height,
        Opts
    ).

%% @doc Convert a canonical message value into the record's representation.
to_value(nonce, Nonce, _Height, _Opts) -> hb_util:int(Nonce);
to_value(int, Int, _Height, _Opts) -> hb_util:int(Int);
to_value(str_int, Int, _Height, _Opts) -> hb_util:int(Int);
to_value(opt_str_int, Int, _Height, _Opts) -> hb_util:int(Int);
to_value(bin, Bin, _Height, _Opts) -> decode(Bin);
to_value(opt_bin, Bin, _Height, _Opts) -> decode(Bin);
to_value(address, Addr, _Height, _Opts) -> decode(Addr);
to_value(bins, Bins, _Height, Opts) ->
    [ decode(Bin) || Bin <- hb_util:message_to_ordered_list(Bins, Opts) ];
to_value(rate, Rate, _Height, Opts) ->
    [Dividend, Divisor] = hb_util:message_to_ordered_list(Rate, Opts),
    {hb_util:int(Dividend), hb_util:int(Divisor)};
to_value(reward_key, Pub, Height, _Opts) ->
    ar_block:get_reward_key(decode(Pub), Height);
to_value(poa, Msg, _Height, Opts) -> to_poa(Msg, Opts);
to_value(nonce_limiter, Msg, _Height, Opts) -> to_nonce_limiter(Msg, Opts);
to_value(double_signing_proof, Msg, Height, Opts) ->
    to_record(
        double_signing_proof_fields(),
        Msg,
        list_to_tuple(lists:duplicate(9, <<>>)),
        Height,
        Opts
    ).

%% @doc Decode a base64url field of a block message. The checked decoder makes
%% a malformed field an error rather than silently decoding another value.
decode(Bin) ->
    case hb_util:safe_decode(Bin) of
        {ok, Decoded} -> Decoded;
        {error, _} -> throw({'invalid-base64', Bin})
    end.

%%% Consensus helpers shared by the block device and its pure vectors.

%% @doc Every block check, in execution order, with its dependencies.
checks() ->
    [
        {<<"linkage">>, []},
        {<<"fields">>, []},
        {<<"identity">>, []},
        {<<"block-index">>, []},
        {<<"reward-history">>, []},
        {<<"block-time-history">>, []},
        {<<"transactions">>, []},
        {<<"pow">>, []},
        {<<"poa">>, [<<"pow">>]},
        {<<"vdf">>, []},
        {<<"accounts">>, [<<"transactions">>]}
    ].

%% @doc Resolve the validation checks a request asks for.
selected(Req, Opts) ->
    selected(Req, <<"full">>, Opts).
selected(Req, Default, Opts) ->
    case hb_maps:get(<<"verify">>, Req, not_found, Opts) of
        not_found ->
            profile(hb_maps:get(<<"profile">>, Req, Default, Opts));
        Verify ->
            explicit(names(Verify, Opts))
    end.

profile(<<"full">>) ->
    {ok, [ Name || {Name, _Needs} <- checks() ]};
profile(<<"archive">>) ->
    {ok,
        ordered(
            [
                <<"identity">>,
                <<"linkage">>,
                <<"block-index">>,
                <<"transactions">>
            ]
        )
    };
profile(<<"headers">>) ->
    {ok, ordered([<<"identity">>])};
profile(Unknown) ->
    {error,
        request_error(<<"unknown-profile">>,
            <<"`", (hb_util:bin(Unknown))/binary, "' is not a validation "
                "profile. The profiles are `full', `archive' and "
                "`headers'.">>)}.

names(Verify, _Opts) when is_binary(Verify) ->
    [ Name || Name <- binary:split(Verify, <<",">>, [global]), Name =/= <<>> ];
names(Verify, Opts) ->
    [
        hb_util:bin(Name)
    ||
        Name <- hb_util:message_to_ordered_list(Verify, Opts)
    ].

explicit(Names) ->
    Selected = ordered(Names),
    case Names -- Selected of
        [] -> complete(Selected);
        Unknown ->
            {error,
                request_error(<<"unknown-check">>,
                    <<"No such check: ", (join(Unknown))/binary,
                        ". The checks are ",
                        (join([ Name || {Name, _Needs} <- checks() ]))/binary,
                        ".">>)}
    end.

ordered(Names) ->
    [ Name || {Name, _Needs} <- checks(), lists:member(Name, Names) ].

complete(Selected) ->
    case
        [
            <<Name/binary, " needs ", Need/binary>>
        ||
            {Name, Needs} <- checks(),
            lists:member(Name, Selected),
            Need <- Needs,
            not lists:member(Need, Selected)
        ]
    of
        [] ->
            {ok, Selected};
        Missing ->
            {error,
                request_error(<<"incomplete-checks">>,
                    <<"The requested checks omit one they read from: ",
                        (join(Missing))/binary, ".">>)}
    end.

join(Names) ->
    hb_util:bin(lists:join(<<", ">>, Names)).

%% @doc Check the VDF step-number relationship between adjacent blocks.
check_step_number(Next, Prev) ->
    Info = Next#block.nonce_limiter_info,
    PrevInfo = Prev#block.nonce_limiter_info,
    Distance = ar_block:vdf_step_number(Next) - ar_block:vdf_step_number(Prev),
    holds(
        ar_nonce_limiter:is_ahead_on_the_timeline(Info, PrevInfo)
            andalso length(Info#nonce_limiter_info.steps)
                == min(?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT, Distance)
            andalso Info#nonce_limiter_info.prev_output
                == PrevInfo#nonce_limiter_info.output,
        <<"invalid-step-number">>,
        <<"The VDF step number, step count or previous output does not follow "
            "the parent's.">>
    ).

%% @doc The hash chaining this block's reward-history element onto the
%% parent's. The block producer fills the field from this and the check below
%% recomputes it from the same expression, because a producer and a checker
%% that spell one rule twice are a node that mines blocks it will not accept.
reward_history_hash(Next, Prev) ->
    Element =
        {
            Next#block.reward_addr,
            ar_difficulty:get_hash_rate_fixed_ratio(Next),
            Next#block.reward,
            Next#block.denomination
        },
    ar_rewards:reward_history_hash(
        Next#block.height,
        Prev#block.reward_history_hash,
        ar_rewards:trim_locked_rewards(
            Next#block.height,
            [Element | Prev#block.reward_history]
        )
    ).

%% @doc Check the hash chaining the next reward-history element.
check_reward_history_hash(Next, Prev) ->
    equal(
        reward_history_hash(Next, Prev),
        Next#block.reward_history_hash,
        <<"invalid-reward-history-hash">>,
        <<"The reward history hash does not chain onto the parent's.">>
    ).

%% @doc The hash over the parent's block-time history extended with this block.
block_time_history_hash(Next, Prev) ->
    ar_block_time_history:hash(
        ar_block_time_history:update_history(Next, Prev)).

%% @doc Check the hash over the extended block-time history.
check_block_time_history_hash(Next, Prev) ->
    equal(
        block_time_history_hash(Next, Prev),
        Next#block.block_time_history_hash,
        <<"invalid-block-time-history-hash">>,
        <<"The block time history hash does not cover the parent's history "
            "extended with this block.">>
    ).

%% @doc Extend the carried block index with the block being applied.
extend_block_index([], _Next, _Opts) ->
    {ok, []};
extend_block_index(Index, Next, Opts) ->
    hb_ao:resolve(
        Index,
        #{
            <<"path">> => <<"append">>,
            <<"indep-hash">> => hb_util:encode(Next#block.indep_hash),
            <<"weave-size">> => Next#block.weave_size,
            <<"tx-root">> => hb_util:encode(Next#block.tx_root)
        },
        Opts
    ).

%% @doc Return an error message unless a consensus condition holds.
holds(true, _Message, _Detail) -> ok;
holds(false, Message, Detail) ->
    {error, consensus_error(Message, Detail)}.

equal(Value, Value, _Message, _Detail) -> ok;
equal(_Value, _Expected, Message, Detail) ->
    {error, consensus_error(Message, Detail)}.

request_error(Message, Detail) ->
    #{ <<"status">> => 400, <<"message">> => Message, <<"detail">> => Detail }.

consensus_error(Message, Detail) ->
    #{ <<"status">> => 422, <<"message">> => Message, <<"detail">> => Detail }.
