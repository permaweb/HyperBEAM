%%% @doc Library functions for decoding L1 TXs to TABM form.
-module(dev_codec_tx_from).
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
            reward_field(TX, Prefix, Opts)
        ]
    ).

format_field(TX, Prefix, _Opts) ->
    case TX#tx.format of
        1 -> #{
            <<Prefix/binary, "format">> => <<"1">>
        };
        _ -> #{}
    end.

target_field(TX, Prefix, _Opts) ->
    case TX#tx.target of
        ?DEFAULT_TARGET -> #{};
        Target -> #{
            <<Prefix/binary, "target">> => hb_util:encode(Target)
        }
    end.

anchor_field(TX, Prefix, _Opts) ->
    case TX#tx.anchor of
        ?DEFAULT_ANCHOR -> #{};
        Anchor -> #{
            <<Prefix/binary, "anchor">> => hb_util:encode(Anchor)
        }
    end.

quantity_field(TX, Prefix, _Opts) ->
    case TX#tx.quantity of
        ?DEFAULT_QUANTITY -> #{};
        Quantity -> #{
            <<Prefix/binary, "quantity">> => integer_to_binary(Quantity)
        }
    end.

reward_field(TX, Prefix, _Opts) ->
    case TX#tx.reward of
        ?DEFAULT_REWARD -> #{};
        Reward -> #{
            <<Prefix/binary, "reward">> => integer_to_binary(Reward)
        }
    end.

