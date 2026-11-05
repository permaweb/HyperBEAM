-module(dev_mint_authority).
-export([device_info/0, mint/3]).
-include("include/hb.hrl").

-implements(<<"mint-authority@1.0">>).

-define(PROCESS_OUTBOX_DEVICE, <<"process-outbox@1.0">>).

device_info() ->
    #{ exports => [<<"mint">>] }.

mint(Base, Assignment, Opts) ->
    maybe
        {ok, Req} ?= field(<<"body">>, Assignment, Opts),
        true ?= is_map(Req) orelse {error, <<"Mint body must be a message.">>},
        true ?= is_mint(field(<<"action">>, Req, Opts))
            orelse {error, <<"Not a mint request.">>},
        {ok, Minter} ?= committer(<<"from">>, Req, Opts),
        {ok, Authority} ?= committer(<<"mint-authority">>, Base, Opts),
        true ?= (Minter =:= Authority)
            orelse {error, <<"Minter is not the mint authority.">>},
        {ok, Recipient} ?= committer(<<"recipient">>, Req, Opts),
        {ok, Quantity} ?= quantity(field(<<"quantity">>, Req, Opts)),
        {ok, Trigger} ?= trigger(field(<<"mint-trigger">>, Req, Opts)),
        TriggerKey = hb_committer:encode({id, undefined}, Trigger),
        Triggers = hb_maps:get(<<"mint-triggers">>, Base, #{}, Opts),
        true ?= (hb_ao:get(TriggerKey, Triggers, not_found, Opts) =:= not_found)
            orelse {error, <<"Mint trigger already used.">>},
        {ok, Supply} ?=
            amount(
                hb_maps:get(<<"total-supply">>, Base, 0, Opts),
                <<"Invalid total supply.">>
            ),
        {ok, MaxSupply} ?=
            amount(
                hb_maps:get(<<"max-supply">>, Base, not_found, Opts),
                <<"Invalid max supply.">>
            ),
        true ?= (Supply + Quantity =< MaxSupply)
            orelse {error, <<"Mint exceeds max supply.">>},
        Balances = hb_maps:get(<<"balances">>, Base, #{}, Opts),
        Balance = hb_ao:get(Recipient, Balances, 0, Opts),
        true ?= (is_integer(Balance) andalso Balance >= 0)
            orelse {error, <<"Invalid balance values.">>},
        {ok, NewBalances} ?=
            hb_ao:resolve(
                Balances,
                #{ <<"path">> => <<"set">>, Recipient => Balance + Quantity },
                Opts
            ),
        {ok, NewTriggers} ?=
            hb_ao:resolve(
                Triggers,
                #{ <<"path">> => <<"set">>, TriggerKey => Quantity },
                Opts
            ),
        outbox_send(
            #{
                <<"target">> => Recipient,
                <<"action">> => <<"Mint-Notice">>,
                <<"recipient">> => Recipient,
                <<"quantity">> => Quantity,
                <<"mint-trigger">> => Trigger
            },
            Base#{
                <<"balances">> => NewBalances,
                <<"total-supply">> => Supply + Quantity,
                <<"mint-triggers">> => NewTriggers
            },
            Opts
        )
    end.

field(Key, Msg, Opts) ->
    case hb_maps:find(Key, Msg, Opts) of
        {ok, Value} -> {ok, Value};
        error -> {error, <<"Mint `", Key/binary, "` not found.">>}
    end.

committer(Key, Msg, Opts) ->
    maybe
        {ok, Value} ?= field(Key, Msg, Opts),
        hb_committer:name(Value)
    end.

is_mint({ok, Action}) when is_binary(Action) ->
    hb_util:lowercase_key(Action) =:= <<"mint">>;
is_mint(_) ->
    false.

quantity({ok, Value}) ->
    case hb_util:safe_int(Value) of
        {ok, Quantity} when Quantity > 0 -> {ok, Quantity};
        _ -> {error, <<"Mint quantity must be a positive integer.">>}
    end;
quantity(Error) ->
    Error.

trigger({ok, Trigger}) ->
    case hb_util:is_human_id(Trigger) of
        true -> {ok, Trigger};
        false -> {error, <<"Mint trigger must be a message ID.">>}
    end;
trigger(Error) ->
    Error.

amount(Value, Reason) ->
    case hb_util:safe_int(Value) of
        {ok, Amount} when Amount >= 0 -> {ok, Amount};
        _ -> {error, Reason}
    end.

outbox_send(Message, Base, Opts) ->
    case hb_device_load:reference(?PROCESS_OUTBOX_DEVICE, Opts) of
        {ok, Outbox} -> Outbox:send(Base, #{ <<"messages">> => Message }, Opts);
        {error, Reason} -> {error, {process_outbox_not_loadable, Reason}}
    end.
