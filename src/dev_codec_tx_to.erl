%%% @doc Library functions for decoding L1 TXs to TABM form.
-module(dev_codec_tx_to).
-export([fields_to_tx/4, excluded_tags/3]).
-include("include/hb.hrl").

fields_to_tx(TX, Prefix, Map, Opts) ->
    TX#tx{
        format = format_field(Prefix, Map, Opts),
        target = target_field(Prefix, Map, Opts),
        anchor = anchor_field(Prefix, Map, Opts),
        quantity = quantity_field(Prefix, Map, Opts),
        reward = reward_field(Prefix, Map, Opts)
    }.

format_field(Prefix, Map, Opts) ->
    case hb_maps:find(<<Prefix/binary, "format">>, Map, Opts) of
        {ok, EncodedFormat} ->
            case EncodedFormat of
                <<"1">> -> 1;
                _ -> 2
            end;
        error -> 2
    end.

target_field(Prefix, Map, Opts) ->
    case hb_maps:find(<<Prefix/binary, "target">>, Map, Opts) of
        {ok, EncodedTarget} ->
            case hb_util:safe_decode(EncodedTarget) of
                {ok, Target} when ?IS_ID(Target) -> Target;
                _ -> ?DEFAULT_TARGET
            end;
        error -> ?DEFAULT_TARGET
    end.

anchor_field(Prefix, Map, Opts) ->
    case hb_maps:find(<<Prefix/binary, "anchor">>, Map, Opts) of
        {ok, EncodedAnchor} ->
            case hb_util:safe_decode(EncodedAnchor) of
                {ok, Anchor} -> Anchor;
                _ -> ?DEFAULT_ANCHOR
            end;
        error -> ?DEFAULT_ANCHOR
    end.

quantity_field(Prefix, Map, Opts) ->
    case hb_maps:find(<<Prefix/binary, "quantity">>, Map, Opts) of
        {ok, EncodedQuantity} ->
            case hb_util:safe_int(EncodedQuantity) of
                {ok, Quantity} -> Quantity;
                _ -> ?DEFAULT_QUANTITY
            end;
        error -> ?DEFAULT_QUANTITY
    end.

reward_field(Prefix, Map, Opts) ->
    case hb_maps:find(<<Prefix/binary, "reward">>, Map, Opts) of
        {ok, EncodedReward} ->
            case hb_util:safe_int(EncodedReward) of
                {ok, Reward} -> Reward;
                _ -> ?DEFAULT_REWARD
            end;
        error -> ?DEFAULT_REWARD
    end.

excluded_tags(TX, TABM, Opts) ->
    exclude_target_tag(TX, TABM, Opts) ++
    exclude_anchor_tag(TX, TABM, Opts) ++
    exclude_quantity_tag(TX, TABM, Opts) ++
    exclude_reward_tag(TX, TABM, Opts).

exclude_target_tag(TX, TABM, Opts) ->
    case {TX#tx.target, hb_maps:get(<<"target">>, TABM, undefined, Opts)} of
        {?DEFAULT_TARGET, _} -> [];
        {FieldTarget, TagTarget} when FieldTarget =/= TagTarget -> 
            [<<"target">>];
        _ -> []
    end.

exclude_anchor_tag(TX, TABM, Opts) ->
    case {TX#tx.anchor, hb_maps:get(<<"anchor">>, TABM, undefined, Opts)} of
        {?DEFAULT_ANCHOR, _} -> [];
        {FieldAnchor, TagAnchor} when FieldAnchor =/= TagAnchor -> 
            [<<"anchor">>];
        _ -> []
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