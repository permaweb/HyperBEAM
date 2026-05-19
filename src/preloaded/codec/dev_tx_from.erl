%%% @doc Library functions for decoding L1 TXs to TABM form.
-module(dev_tx_from).
-export([fields/3]).
-include("include/hb.hrl").

%% @doc Return a TABM message containing the fields of the given decoded
%% ANS-104 data item that should be included in the base message.
fields(TX, Prefix, Opts) ->
    lists:foldl(
        fun hb_maps:merge/2,
        #{},
        [
            format_field(TX, Prefix, Opts),
            target_field(TX, Prefix, Opts),
            anchor_field(TX, Prefix, Opts),
            quantity_field(TX, Prefix, Opts),
            reward_field(TX, Prefix, Opts),
            data_root_field(TX, Prefix, Opts),
            data_size_field(TX, Prefix, Opts)
        ]
    ).

format_field(TX, Prefix, _Opts) ->
    encoded_field(TX#tx.format, 2, Prefix, <<"format">>, fun(_) -> <<"1">> end).

target_field(TX, Prefix, _Opts) ->
    encoded_field(
        TX#tx.target,
        ?DEFAULT_TARGET,
        Prefix,
        <<"target">>,
        fun hb_util:encode/1
    ).

anchor_field(TX, Prefix, _Opts) ->
    encoded_field(
        TX#tx.anchor,
        ?DEFAULT_ANCHOR,
        Prefix,
        <<"anchor">>,
        fun hb_util:encode/1
    ).

quantity_field(TX, Prefix, _Opts) ->
    encoded_field(
        TX#tx.quantity,
        ?DEFAULT_QUANTITY,
        Prefix,
        <<"quantity">>,
        fun integer_to_binary/1
    ).

reward_field(TX, Prefix, _Opts) ->
    encoded_field(
        TX#tx.reward,
        ?DEFAULT_REWARD,
        Prefix,
        <<"reward">>,
        fun integer_to_binary/1
    ).

data_root_field(#tx{data = ?DEFAULT_DATA, data_root = DataRoot}, Prefix, _Opts) ->
    encoded_field(
        DataRoot,
        ?DEFAULT_DATA_ROOT,
        Prefix,
        <<"data_root">>,
        fun hb_util:encode/1
    );
data_root_field(_TX, _Prefix, _Opts) ->
    #{}.

data_size_field(#tx{data = ?DEFAULT_DATA, data_size = DataSize}, Prefix, _Opts) ->
    encoded_field(
        DataSize,
        ?DEFAULT_DATA_SIZE,
        Prefix,
        <<"data_size">>,
        fun integer_to_binary/1
    );
data_size_field(_TX, _Prefix, _Opts) ->
    #{}.

encoded_field(Value, Default, Prefix, Key, Encode) ->
    case Value =:= Default of
        true -> #{};
        false -> #{ <<Prefix/binary, Key/binary>> => Encode(Value) }
    end.
