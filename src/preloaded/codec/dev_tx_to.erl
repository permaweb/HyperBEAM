%%% @doc Library functions for decoding L1 TXs to TABM form.
-module(dev_tx_to).
-export([fields_to_tx/4, excluded_tags/3]).
-include("include/hb.hrl").

fields_to_tx(TX, Prefix, Map, Opts) ->
    TX#tx{
        format = format_field(Prefix, Map, Opts),
        target = target_field(Prefix, Map, Opts),
        anchor = anchor_field(Prefix, Map, Opts),
        quantity = quantity_field(Prefix, Map, Opts),
        reward = reward_field(Prefix, Map, Opts),
        data_root = data_root_field(Prefix, Map, Opts),
        data_size = data_size_field(Prefix, Map, Opts)
    }.

format_field(Prefix, Map, Opts) ->
    decoded_field(Prefix, <<"format">>, Map, 2, fun decode_format/1, Opts).

target_field(Prefix, Map, Opts) ->
    decoded_field(Prefix, <<"target">>, Map, ?DEFAULT_TARGET, fun decode_id/1, Opts).

anchor_field(Prefix, Map, Opts) ->
    decoded_field(Prefix, <<"anchor">>, Map, ?DEFAULT_ANCHOR, fun hb_util:safe_decode/1, Opts).

quantity_field(Prefix, Map, Opts) ->
    decoded_field(Prefix, <<"quantity">>, Map, ?DEFAULT_QUANTITY, fun hb_util:safe_int/1, Opts).

reward_field(Prefix, Map, Opts) ->
    decoded_field(Prefix, <<"reward">>, Map, ?DEFAULT_REWARD, fun hb_util:safe_int/1, Opts).

data_root_field(Prefix, Map, Opts) ->
    case hb_maps:get(<<"data">>, Map, ?DEFAULT_DATA, Opts) of
        ?DEFAULT_DATA ->
            decoded_field(
                Prefix,
                <<"data_root">>,
                Map,
                ?DEFAULT_DATA_ROOT,
                fun decode_id/1,
                Opts
            );
        _ ->
            ?DEFAULT_DATA_ROOT
    end.

data_size_field(Prefix, Map, Opts) ->
    case hb_maps:get(<<"data">>, Map, ?DEFAULT_DATA, Opts) of
        ?DEFAULT_DATA ->
            decoded_field(
                Prefix,
                <<"data_size">>,
                Map,
                ?DEFAULT_DATA_SIZE,
                fun hb_util:safe_int/1,
                Opts
            );
        _ ->
            ?DEFAULT_DATA_SIZE
    end.

excluded_tags(TX, TABM, Opts) ->
    lib_arweave_common:excluded_tags(TX, TABM, Opts) ++
    exclude_quantity_tag(TX, TABM, Opts) ++
    exclude_reward_tag(TX, TABM, Opts) ++
    exclude_data_root_tag(TX) ++
    exclude_data_size_tag(TX).

decoded_field(Prefix, Key, Map, Default, Decode, Opts) ->
    case hb_maps:find(<<Prefix/binary, Key/binary>>, Map, Opts) of
        {ok, Encoded} ->
            case Decode(Encoded) of
                {ok, Value} -> Value;
                _ -> Default
            end;
        error ->
            Default
    end.

decode_format(<<"1">>) -> {ok, 1};
decode_format(_) -> {ok, 2}.

decode_id(Encoded) ->
    case hb_util:safe_decode(Encoded) of
        {ok, ID} when ?IS_ID(ID) -> {ok, ID};
        _ -> error
    end.

exclude_quantity_tag(TX, TABM, Opts) ->
    case {TX#tx.quantity, hb_maps:get(<<"quantity">>, TABM, undefined, Opts)} of
        {?DEFAULT_QUANTITY, _} -> [];
        {FieldQuantity, TagQuantity} when FieldQuantity =/= TagQuantity ->
            [<<"quantity">>];
        _ -> []
    end.

exclude_reward_tag(TX, TABM, Opts) ->
    case {TX#tx.reward, hb_maps:get(<<"reward">>, TABM, undefined, Opts)} of
        {?DEFAULT_REWARD, _} -> [];
        {FieldReward, TagReward} when FieldReward =/= TagReward ->
            [<<"reward">>];
        _ -> []
    end.

exclude_data_root_tag(TX) ->
    case TX#tx.data_root of
        ?DEFAULT_DATA_ROOT -> [];
        _ -> [<<"data_root">>]
    end.

exclude_data_size_tag(TX) ->
    case TX#tx.data_size of
        ?DEFAULT_DATA_SIZE -> [];
        _ -> [<<"data_size">>]
    end.
