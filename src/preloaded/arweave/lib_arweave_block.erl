%%% @doc The bridge between Arweave's block record family -- `#block{}',
%%% `#poa{}' and `#nonce_limiter_info{}' -- and the canonical AO-Core block
%%% message, together with the two wire codecs expressed in terms of it.
%%%
%%% Every conversion of those three records happens here and nowhere else. The
%%% field tables below are the single description of the mapping: `from/2' and
%%% `to/2' read the record column, `from_json/2' and `to_json/2' read the JSON
%%% column, and both share the canonical key column, so a key cannot drift
%%% between the two representations.
%%%
%%% The binary codec is `ar_serialize''s, reached through the record. The JSON
%%% codec is not: upstream's `json_struct_to_block/1' asserts a pre-2.6 height
%%% and has no post-2.6 clause, because a 2.6+ node fetches blocks over
%%% `/block2'. The JSON column therefore describes Arweave's JSON encoder
%%% directly, which makes it an independent second implementation -- parsing
%%% the same block from both forms and comparing the results is a real check
%%% rather than a tautology.
%%%
%%% Arweave's JSON encoder renames three nonce limiter fields:
%%% `partition_upper_bound' becomes `zone_upper_bound',
%%% `next_partition_upper_bound' becomes `next_zone_upper_bound', and -- the
%%% trap -- `steps' becomes `checkpoints'. There is no JSON `steps' key; a
%%% codec that looks for one yields an empty step list and the VDF chain then
%%% verifies over nothing. The canonical message keeps the protocol's own
%%% naming and the JSON column restores Arweave's spelling on the way out.
%%%
%%% Binaries are base64url on the message, matching the rest of the Arweave
%%% device family, so the b64url-valued JSON fields are an identity mapping.
-module(lib_arweave_block).
-export([from/2, to/2, to_header/2, with_transactions/2]).
-export([from_poa/2, to_poa/2]).
-export([from_nonce_limiter/2, to_nonce_limiter/2]).
-export([from_binary/2, to_binary/2, from_json/2, to_json/2]).
-include("include/hb.hrl").

%% @doc The canonical block message key, Arweave's JSON key, the `#block{}'
%% field position and the value type, for every field a block carries on the
%% wire. Fields the record uses internally -- `tx_tree', `size_tagged_txs',
%% `reward_history', `block_time_history', the two proof caches -- are not
%% wire fields and are not carried by the message.
block_fields() ->
    [
        {<<"nonce">>, <<"nonce">>, #block.nonce, nonce},
        {<<"previous-block">>, <<"previous_block">>, #block.previous_block,
            bin},
        {<<"timestamp">>, <<"timestamp">>, #block.timestamp, int},
        {<<"last-retarget">>, <<"last_retarget">>, #block.last_retarget, int},
        {<<"diff">>, <<"diff">>, #block.diff, str_int},
        {<<"height">>, <<"height">>, #block.height, int},
        {<<"hash">>, <<"hash">>, #block.hash, bin},
        {<<"indep-hash">>, <<"indep_hash">>, #block.indep_hash, bin},
        {<<"txs">>, <<"txs">>, #block.txs, bins},
        {<<"tx-root">>, <<"tx_root">>, #block.tx_root, bin},
        {<<"wallet-list">>, <<"wallet_list">>, #block.wallet_list, bin},
        {<<"reward-addr">>, <<"reward_addr">>, #block.reward_addr, address},
        {<<"tags">>, <<"tags">>, #block.tags, bins},
        {<<"reward-pool">>, <<"reward_pool">>, #block.reward_pool, str_int},
        {<<"weave-size">>, <<"weave_size">>, #block.weave_size, str_int},
        {<<"block-size">>, <<"block_size">>, #block.block_size, str_int},
        {<<"cumulative-diff">>, <<"cumulative_diff">>, #block.cumulative_diff,
            str_int},
        {<<"hash-list-merkle">>, <<"hash_list_merkle">>,
            #block.hash_list_merkle, bin},
        {<<"poa">>, <<"poa">>, #block.poa, poa},
        {<<"usd-to-ar-rate">>, <<"usd_to_ar_rate">>, #block.usd_to_ar_rate,
            rate},
        {<<"scheduled-usd-to-ar-rate">>, <<"scheduled_usd_to_ar_rate">>,
            #block.scheduled_usd_to_ar_rate, rate},
        {<<"packing-2-5-threshold">>, <<"packing_2_5_threshold">>,
            #block.packing_2_5_threshold, str_int},
        {<<"strict-data-split-threshold">>, <<"strict_data_split_threshold">>,
            #block.strict_data_split_threshold, str_int},
        {<<"hash-preimage">>, <<"hash_preimage">>, #block.hash_preimage, bin},
        {<<"recall-byte">>, <<"recall_byte">>, #block.recall_byte, opt_str_int},
        {<<"reward">>, <<"reward">>, #block.reward, str_int},
        {<<"previous-solution-hash">>, <<"previous_solution_hash">>,
            #block.previous_solution_hash, bin},
        {<<"partition-number">>, <<"partition_number">>,
            #block.partition_number, int},
        {<<"nonce-limiter-info">>, <<"nonce_limiter_info">>,
            #block.nonce_limiter_info, nonce_limiter},
        {<<"poa2">>, <<"poa2">>, #block.poa2, poa},
        {<<"recall-byte2">>, <<"recall_byte2">>, #block.recall_byte2,
            opt_str_int},
        {<<"signature">>, <<"signature">>, #block.signature, bin},
        {<<"reward-key">>, <<"reward_key">>, #block.reward_key, reward_key},
        {<<"price-per-gib-minute">>, <<"price_per_gib_minute">>,
            #block.price_per_gib_minute, str_int},
        {<<"scheduled-price-per-gib-minute">>,
            <<"scheduled_price_per_gib_minute">>,
            #block.scheduled_price_per_gib_minute, str_int},
        {<<"reward-history-hash">>, <<"reward_history_hash">>,
            #block.reward_history_hash, bin},
        {<<"debt-supply">>, <<"debt_supply">>, #block.debt_supply, str_int},
        {<<"kryder-plus-rate-multiplier">>, <<"kryder_plus_rate_multiplier">>,
            #block.kryder_plus_rate_multiplier, str_int},
        {<<"kryder-plus-rate-multiplier-latch">>,
            <<"kryder_plus_rate_multiplier_latch">>,
            #block.kryder_plus_rate_multiplier_latch, str_int},
        {<<"denomination">>, <<"denomination">>, #block.denomination, str_int},
        {<<"redenomination-height">>, <<"redenomination_height">>,
            #block.redenomination_height, int},
        {<<"double-signing-proof">>, <<"double_signing_proof">>,
            #block.double_signing_proof, double_signing_proof},
        {<<"previous-cumulative-diff">>, <<"previous_cumulative_diff">>,
            #block.previous_cumulative_diff, str_int},
        {<<"merkle-rebase-support-threshold">>,
            <<"merkle_rebase_support_threshold">>,
            #block.merkle_rebase_support_threshold, str_int},
        {<<"chunk-hash">>, <<"chunk_hash">>, #block.chunk_hash, bin},
        {<<"chunk2-hash">>, <<"chunk2_hash">>, #block.chunk2_hash, opt_bin},
        {<<"block-time-history-hash">>, <<"block_time_history_hash">>,
            #block.block_time_history_hash, bin},
        {<<"packing-difficulty">>, <<"packing_difficulty">>,
            #block.packing_difficulty, int},
        {<<"unpacked-chunk-hash">>, <<"unpacked_chunk_hash">>,
            #block.unpacked_chunk_hash, opt_bin},
        {<<"unpacked-chunk2-hash">>, <<"unpacked_chunk2_hash">>,
            #block.unpacked_chunk2_hash, opt_bin},
        {<<"replica-format">>, <<"replica_format">>, #block.replica_format, int}
    ].

%% @doc The fields of a proof of access. `unpacked-chunk' is empty at packing
%% difficulty 0 and carries the full 0-padded chunk above it.
poa_fields() ->
    [
        {<<"option">>, <<"option">>, #poa.option, str_int},
        {<<"tx-path">>, <<"tx_path">>, #poa.tx_path, bin},
        {<<"data-path">>, <<"data_path">>, #poa.data_path, bin},
        {<<"chunk">>, <<"chunk">>, #poa.chunk, bin},
        {<<"unpacked-chunk">>, <<"unpacked_chunk">>, #poa.unpacked_chunk, bin}
    ].

%% @doc The fields of the nonce limiter info. `steps' and
%% `last-step-checkpoints' are newest-first, as they are on the wire.
nonce_limiter_fields() ->
    [
        {<<"output">>, <<"output">>, #nonce_limiter_info.output, bin},
        {<<"global-step-number">>, <<"global_step_number">>,
            #nonce_limiter_info.global_step_number, int},
        {<<"seed">>, <<"seed">>, #nonce_limiter_info.seed, bin},
        {<<"next-seed">>, <<"next_seed">>, #nonce_limiter_info.next_seed, bin},
        {<<"partition-upper-bound">>, <<"zone_upper_bound">>,
            #nonce_limiter_info.partition_upper_bound, int},
        {<<"next-partition-upper-bound">>, <<"next_zone_upper_bound">>,
            #nonce_limiter_info.next_partition_upper_bound, int},
        {<<"prev-output">>, <<"prev_output">>, #nonce_limiter_info.prev_output,
            bin},
        {<<"last-step-checkpoints">>, <<"last_step_checkpoints">>,
            #nonce_limiter_info.last_step_checkpoints, bins},
        {<<"steps">>, <<"checkpoints">>, #nonce_limiter_info.steps, bins},
        {<<"vdf-difficulty">>, <<"vdf_difficulty">>,
            #nonce_limiter_info.vdf_difficulty, str_int},
        {<<"next-vdf-difficulty">>, <<"next_vdf_difficulty">>,
            #nonce_limiter_info.next_vdf_difficulty, str_int}
    ].

%% @doc The nine elements of a double signing proof, in tuple order.
double_signing_proof_fields() ->
    [
        {<<"pub-key">>, <<"pub_key">>, 1, bin},
        {<<"sig1">>, <<"sig1">>, 2, bin},
        {<<"cdiff1">>, <<"cdiff1">>, 3, str_int},
        {<<"prev-cdiff1">>, <<"prev_cdiff1">>, 4, str_int},
        {<<"preimage1">>, <<"preimage1">>, 5, bin},
        {<<"sig2">>, <<"sig2">>, 6, bin},
        {<<"cdiff2">>, <<"cdiff2">>, 7, str_int},
        {<<"prev-cdiff2">>, <<"prev_cdiff2">>, 8, str_int},
        {<<"preimage2">>, <<"preimage2">>, 9, bin}
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
        [ Field || Field = {_, _, _, Type} <- block_fields(), Type =/= poa ],
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

%%% Wire codecs.

%% @doc Parse a block from Arweave's binary block format.
from_binary(Bin, Opts) ->
    case ar_serialize:binary_to_block(Bin) of
        {ok, Block} -> {ok, from(Block, Opts)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Serialize a block message into Arweave's binary block format.
to_binary(Msg, Opts) ->
    ar_serialize:block_to_binary(to(Msg, Opts)).

%% @doc Parse a block from Arweave's JSON block format, given either the raw
%% document or an already-decoded struct.
from_json(Bin, Opts) when is_binary(Bin) ->
    from_json(hb_json:decode(Bin), Opts);
from_json(Struct, Opts) ->
    Height = hb_util:int(hb_maps:get(<<"height">>, Struct, 0, Opts)),
    from_struct(block_fields(), Struct, Height, Opts).

%% @doc Serialize a block message into Arweave's JSON block format.
to_json(Msg, Opts) ->
    hb_json:encode(to_struct(block_fields(), Msg, Opts)).

%%% Internal functions.

%% @doc Project a record onto a message, dropping the fields whose value is
%% `undefined' -- the record's way of spelling "this block does not carry the
%% field", which the message spells by omitting the key.
from_record(Fields, Record, Height, Opts) ->
    maps:from_list(
        lists:filtermap(
            fun({Key, _JSONKey, Index, Type}) ->
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
        fun({Key, _JSONKey, Index, Type}, Record) ->
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

%% @doc Project a JSON struct onto a message, renaming each key and normalising
%% each value into its canonical type.
from_struct(Fields, Struct, Height, Opts) ->
    maps:from_list(
        lists:filtermap(
            fun({Key, JSONKey, _Index, Type}) ->
                case hb_maps:get(JSONKey, Struct, absent, Opts) of
                    absent -> false;
                    Value ->
                        case from_json_value(Type, Value, Height, Opts) of
                            absent -> false;
                            Canonical -> {true, {Key, Canonical}}
                        end
                end
            end,
            Fields
        )
    ).

%% @doc Build a JSON struct from a message, restoring Arweave's spelling and
%% its choice of integer representation for each field.
to_struct(Fields, Msg, Opts) ->
    maps:from_list(
        lists:filtermap(
            fun({Key, JSONKey, _Index, Type}) ->
                case hb_maps:get(Key, Msg, absent, Opts) of
                    absent ->
                        case absent_json_value(Type) of
                            absent -> false;
                            Value -> {true, {JSONKey, Value}}
                        end;
                    Value ->
                        {true, {JSONKey, to_json_value(Type, Value, Opts)}}
                end
            end,
            Fields
        )
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

%% @doc Normalise a JSON value into its canonical message value. Arweave sends
%% the large integers as decimal strings and the small ones as JSON integers;
%% both are integers on the message.
from_json_value(nonce, Nonce, _Height, _Opts) ->
    binary:decode_unsigned(hb_util:decode(Nonce), big);
from_json_value(int, Int, _Height, _Opts) -> hb_util:int(Int);
from_json_value(str_int, Int, _Height, _Opts) -> hb_util:int(Int);
from_json_value(opt_str_int, Int, _Height, _Opts) -> hb_util:int(Int);
from_json_value(bin, Bin, _Height, _Opts) -> Bin;
from_json_value(opt_bin, Bin, _Height, _Opts) -> Bin;
from_json_value(address, Addr, _Height, _Opts) ->
    hb_util:encode(
        ar_wallet:base64_address_with_optional_checksum_to_decoded_address(Addr)
    );
from_json_value(bins, Bins, _Height, _Opts) -> Bins;
from_json_value(rate, Rate, _Height, _Opts) ->
    [ hb_util:int(Element) || Element <- Rate ];
from_json_value(reward_key, Pub, _Height, _Opts) -> Pub;
from_json_value(poa, Struct, Height, Opts) ->
    from_struct(poa_fields(), with_empty_chunk(Struct), Height, Opts);
from_json_value(nonce_limiter, Struct, Height, Opts) ->
    from_struct(nonce_limiter_fields(), Struct, Height, Opts);
from_json_value(double_signing_proof, Struct, _Height, _Opts)
        when map_size(Struct) == 0 ->
    absent;
from_json_value(double_signing_proof, Struct, Height, Opts) ->
    from_struct(double_signing_proof_fields(), Struct, Height, Opts).

%% @doc Convert a canonical message value into Arweave's JSON representation.
to_json_value(nonce, Nonce, _Opts) ->
    hb_util:encode(binary:encode_unsigned(hb_util:int(Nonce), big));
to_json_value(int, Int, _Opts) -> hb_util:int(Int);
to_json_value(str_int, Int, _Opts) -> integer_to_binary(hb_util:int(Int));
to_json_value(opt_str_int, Int, _Opts) -> integer_to_binary(hb_util:int(Int));
to_json_value(bin, Bin, _Opts) -> Bin;
to_json_value(opt_bin, Bin, _Opts) -> Bin;
to_json_value(address, Addr, _Opts) -> Addr;
to_json_value(bins, Bins, Opts) -> hb_util:message_to_ordered_list(Bins, Opts);
to_json_value(rate, Rate, Opts) ->
    [
        integer_to_binary(hb_util:int(Element))
    ||
        Element <- hb_util:message_to_ordered_list(Rate, Opts)
    ];
to_json_value(reward_key, Pub, _Opts) -> Pub;
to_json_value(poa, Msg, Opts) ->
    without_empty_chunk(to_struct(poa_fields(), Msg, Opts));
to_json_value(nonce_limiter, Msg, Opts) ->
    to_struct(nonce_limiter_fields(), Msg, Opts);
to_json_value(double_signing_proof, Msg, Opts) ->
    to_struct(double_signing_proof_fields(), Msg, Opts).

%% @doc Return the JSON value Arweave's encoder emits for a field a block does
%% not carry. Every optional field is simply omitted, apart from the double
%% signing proof, which is emitted as an empty object.
absent_json_value(double_signing_proof) -> #{};
absent_json_value(_Type) -> absent.

%% @doc Decode a base64url field of a block message. Every one of them reached
%% the message from a peer, so the checked decoder is used and a field that is
%% not base64url is reported rather than silently decoded to something else.
decode(Bin) ->
    case hb_util:safe_decode(Bin) of
        {ok, Decoded} -> Decoded;
        {error, _} -> throw({invalid_base64, Bin})
    end.

%% @doc Supply the empty unpacked chunk Arweave's JSON encoder omits, so that
%% the proof of access message always carries all five of its fields.
with_empty_chunk(Struct) ->
    maps:merge(#{ <<"unpacked_chunk">> => <<>> }, Struct).

%% @doc Drop the unpacked chunk again when it is empty, matching the encoder.
without_empty_chunk(Struct = #{ <<"unpacked_chunk">> := <<>> }) ->
    maps:remove(<<"unpacked_chunk">>, Struct);
without_empty_chunk(Struct) ->
    Struct.
