%%% @doc A fast, simple implementation of AO token specification.
%%% Specification: https://cookbook_ao.arweave.net/references/api/token.html
-module(dev_token).
-export([compute/3]).
-include_lib("include/hb.hrl").

%% @doc Entrypoint for computations on token processes. Expects the `action'
%% key to hold the function to call.
compute(Base, Req, Opts) ->
    case enforce_security(Base, Req, Opts) of
        {ok, SecureBase} ->
            route(SecureBase, Req, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Enforce the security constraints of the base state upon the request.
enforce_security(Base, Req, Opts) ->
    {ok, Base}.

%% @doc Route the request to the appropriate key resolution function, depending
%% upon the `action' specified.
route(Base, Req, Opts) ->
    case hb_util:atom(ActionBin = hb_ao:get(<<"action">>, Req, Opts)) of
        transfer -> transfer(Base, Req, Opts);
        mint -> mint(Base, Req, Opts);
        set -> secure_set(Base, Req, Opts);
        _ ->
            ?event(warning, {unsupported_token_action, ActionBin}),
            {error, <<"Unsupported token action: `", ActionBin/binary, "'.">>}
    end.

transfer(Base, Assignment, Opts) ->
    maybe
        % Gather transfer data from the request.
        {ok, Req} ?= hb_ao:resolve(Assignment, <<"body">>, Opts),
        {ok, From} ?= hb_ao:resolve(Req, <<"from">>, Opts),
        {ok, Recipient} ?= hb_ao:resolve(Req, <<"recipient">>, Opts),
        {ok, Quantity} ?= hb_ao:resolve(Req, <<"quantity">>, Opts),
        % Retrieve balances from the base state.
        Balances =
            hb_ao:get(
                <<"balances">>,
                Base,
                #{ <<"device">> => <<"trie@1.0">> },
                Opts
            ),
        SenderBalance = hb_ao:get(From, Balances, 0, Opts),
        RecipientBalance = hb_ao:get(Recipient, Balances, 0, Opts),
        % Sanity check the transfer request.
        true ?=
            (is_integer(SenderBalance) and is_integer(RecipientBalance))
                orelse {error, <<"Invalid balance types.">>},
        true ?=
            (is_integer(Quantity) and (Quantity >= 0))
                orelse {error, <<"Quantity must be a non-negative integer.">>},
        true ?=
            (SenderBalance >= Quantity) orelse {error, <<"Insufficient balance.">>},
        % Update the balances.
        {ok, NewBalances} ?=
            hb_ao:resolve(
                Balances,
                #{
                    From => SenderBalance - Quantity,
                    Recipient => RecipientBalance + Quantity
                },
                Opts
            ),
        % Update the base state.
        NewBase = hb_maps:put(<<"balances">>, NewBalances, Base, Opts),
        send(
            [
                #{
                    <<"action">> => <<"Credit-Notice">>,
                    <<"sender">> => From,
                    <<"recipient">> => Recipient
                },
                #{
                    <<"action">> => <<"Debit-Notice">>,
                    <<"recipient">> => Recipient,
                    <<"quantity">> => Quantity
                }
            ],
            NewBase,
            Opts
        )
    end.

mint(Base, Assignment, Opts) ->
    maybe
        {ok, Req} ?= hb_ao:resolve(Assignment, <<"body">>, Opts),
        {ok, Base} ?= enforce_mint_authority(Base, Req, Opts),
        case hb_ao:get(<<"mode">>, Req, <<"single">>, Opts) of
            <<"single">> -> mint_single(Base, Req, Opts);
            <<"batch">> -> mint_batch(Base, Req, Opts);
            _ -> {error, <<"Invalid mint mode.">>}
        end
    end.

enforce_mint_authority(Base, Req, Opts) ->
    Minter = hb_ao:get(<<"from">>, Req, Opts),
    case hb_ao:get(<<"mint-authority">>, Base, Opts) of
        not_found -> {error, <<"Mint authority not found.">>};
        Minter -> {ok, Base};
        _ -> {error, <<"Mint authority mismatch.">>}
    end.

mint_single(Base, Req, Opts) ->
    {ok, To} = hb_ao:resolve(Req, <<"recipient">>, Opts),
    {ok, Quantity} = hb_ao:resolve(Req, <<"quantity">>, Opts),
    perform_mint(Base, #{ To => Quantity }, Opts).

mint_batch(Base, Req, Opts) ->
    {ok, Quantities} = hb_ao:resolve(Req, <<"body">>, Opts),
    perform_mint(Base, Quantities, Opts).

perform_mint(Base, Quantities, Opts) ->
    maybe
        true ?=
            lists:all(
                fun({_, Q}) -> is_integer(Q) andalso (Q >= 0) end,
                hb_maps:values(Quantities)
            )
            orelse {error, <<"Mint quantities must be non-negative integers.">>},
        todo
        % {ok, ExistingBalance} = hb_ao:get(Path, Base, 0, Opts),
        % {ok, NewBase} ?= hb_ao:resolve(
        %     Base,
        %     Opts
        % )
    end.

secure_set(Base, Req, Opts) ->
    todo.

%%% Process helper functions.

send(Msg, Base, Opts) when not is_list(Msg) ->
    send([Msg], Base, Opts);
send(Msgs, Base, Opts) ->
    CurrentOutbox = hb_ao:get(<<"results/outbox">>, Base, Opts),
    NewOutbox = hb_util:message_to_ordered_list(CurrentOutbox, Opts) ++ Msgs,
    {
        ok,
        hb_ao:set(Base, <<"results/outbox">>, NewOutbox, Opts)
    }.