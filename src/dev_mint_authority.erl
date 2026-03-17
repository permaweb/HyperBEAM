%%% @doc A simple device that allows a particular address to mint units in a 
%%% `~token@1.0'-compatible message.
-module(dev_mint_authority).
-include("include/hb.hrl").
-export([mint/3]).

mint(Base, Assignment, Opts) ->
    maybe
        {ok, Req} ?= hb_ao:resolve(Assignment, <<"body">>, Opts),
        true ?= enforce_mint_authority(Base, Req, Opts),
        ?event(debug_mint, {before_mint_mode, Req}),
        case hb_ao:get(<<"mode">>, Req, <<"single">>, Opts) of
            <<"single">> -> mint_single(Base, Req, Opts);
            <<"batch">> -> mint_batch(Base, Req, Opts);
            _ -> {error, <<"Invalid mint mode.">>}
        end
    end.

enforce_mint_authority(Base, Req, Opts) ->
    Minter = hb_ao:get(<<"from">>, Req, Opts),
    MintAuthority = hb_ao:get(<<"mint-authority">>, Base, Opts),
    case {Minter, MintAuthority} of
        {not_found, _} -> 
            {error, <<"Minter not found.">>};
        {_, not_found} -> 
            {error, <<"Mint authority not found.">>};
        {M, M} -> 
            true;
        _ -> {error, <<"Mint authority mismatch.">>}
    end.
    
mint_single(Base, Req, Opts) ->
    maybe
        ?event(debug_mint, {before_resolve_recipient, Req}),
        {ok, To} ?= hb_ao:resolve(Req, <<"recipient">>, Opts),
        {ok, Quantity} ?= hb_ao:resolve(Req, <<"quantity">>, Opts),
        true ?= (is_integer(Quantity) and (Quantity >= 0))
            orelse {error, <<"Quantity must be a non-negative integer.">>},
        true ?= dev_token:validate_address(To),
        ?event(debug_mint, {before_perform_mint, {to, To}, {quantity, Quantity}}),
        perform_mint(Base, #{ To => Quantity }, Opts)
    end.

mint_batch(Base, Req, Opts) ->
    maybe
        {ok, Quantities} ?= hb_ao:resolve(Req, <<"quantities">>, Opts),
        perform_mint(Base, Quantities, Opts)
    end.

perform_mint(Base, RawQuantities, Opts) ->
    maybe
        % Filter to only account-quantity pairs
        Quantities = maps:filter(
            fun(K, V) -> is_binary(K) andalso is_integer(V) end,
            RawQuantities
        ),
        ?event({filtered_quantities, Quantities}),
        true ?=
            lists:all(
                fun(Q) -> is_integer(Q) andalso (Q >= 0) end,
                maps:values(Quantities)
            )
            orelse {error, <<"Mint quantities must be non-negative integers.">>},
        % Get current balances 
        Balances =
            hb_ao:get(
                <<"balances">>,
                Base,
                Opts
            ),
        % Calculate new balances for all recipients
        NewBalanceMap =
            hb_maps:map(
                fun(Recipient, MintQuantity) ->
                    CurrentBalance = hb_ao:get(Recipient, Balances, 0, Opts),
                    CurrentBalance + MintQuantity
                end,
                Quantities
            ),
        {ok, NewBalances} ?=
            hb_ao:resolve(
                Balances,
                NewBalanceMap#{<<"path">> => <<"set">>},
                Opts
            ),
        % Calculate total minted in this operation
        TotalMinted = lists:sum(hb_maps:values(Quantities)),
        % Update total supply
        CurrentSupply = hb_ao:get(<<"total-supply">>, Base, 0, Opts),
        NewSupply = CurrentSupply + TotalMinted,
        % Update base state with new balances and supply
        NewBaseWithBalances = 
            hb_maps:put(
                <<"balances">>, 
                NewBalances, 
                Base, 
                Opts
            ),
        NewBaseWithBalAndSupply = 
            hb_maps:put(
                <<"total-supply">>, 
                NewSupply, 
                NewBaseWithBalances, 
                Opts
            ),
        % Send mint notices for each recipient
        Notices =
            lists:map(
                fun({Recipient, Quantity}) ->
                    #{
                        <<"action">> => <<"Mint-Notice">>,
                        <<"recipient">> => Recipient,
                        <<"quantity">> => Quantity
                    }
                end,
                maps:to_list(Quantities)
            ),
        {ok, dev_process_outbox:send(Notices, NewBaseWithBalAndSupply, Opts)}
    end.