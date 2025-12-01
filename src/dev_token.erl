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
enforce_security(Base, _Req, _Opts) ->
    {ok, Base}.

%% @doc Route the request to the appropriate key resolution function, depending
%% upon the `action' specified.
route(Base, Req, Opts) ->
    ActionBin = hb_ao:get(<<"action">>, Req, Opts),
    case ActionBin of
        <<"transfer">> -> transfer(Base, Req, Opts);
        <<"mint">> -> mint(Base, Req, Opts);
        <<"set">> -> secure_set(Base, Req, Opts);
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
        ?event({req, Req, from, From}),
        true ?= validate_address(Recipient),
        true ?= (is_integer(Quantity) and (Quantity >= 0))
            orelse {error, <<"Quantity must be a non-negative integer.">>},
        % Handle self-transfer: skip balance updates
        case From =:= Recipient of
            true ->
                send(
                    transfer_notices(From, Recipient, Quantity, Req, Opts),
                    Base, 
                    Opts
                );
            false ->
                transfer_between_accounts(
                    Base, 
                    From, 
                    Recipient, 
                    Quantity, 
                    Req, 
                    Opts
                )
        end
    end.

transfer_between_accounts(Base, From, Recipient, Quantity, Req, Opts) ->
    maybe
        % Retrieve balances from the base state.
        Balances = 
            hb_ao:get(
                <<"balances">>, 
                Base, 
                #{ <<"device">> => <<"trie@1.0">> }, 
                Opts
            ),
        ?event({balances_structure, Balances}),
        SenderBalance = hb_ao:get(From, Balances, 0, Opts),
        RecipientBalance = hb_ao:get(Recipient, Balances, 0, Opts),
        ?event(
            {transfer_balances, 
                {from, From}, 
                {sender_balance, SenderBalance},
                {recipient_balance, RecipientBalance}
            }
        ),
        % Sanity check the transfer request.
        true ?= (is_integer(SenderBalance) and is_integer(RecipientBalance))
            orelse {error, <<"Invalid balance types.">>},
        true ?= (is_integer(Quantity) and (Quantity >= 0))
            orelse {error, <<"Quantity must be a non-negative integer.">>},
        true ?= (SenderBalance >= Quantity) 
            orelse {error, <<"Insufficient balance.">>},
        % Update the balances.
        {ok, NewBalances} ?=
            hb_ao:resolve(
                Balances,
                #{
                    <<"path">> => <<"set">>,
                    From => SenderBalance - Quantity,
                    Recipient => RecipientBalance + Quantity
                },
                Opts
            ),
        % Update the base state and send notices.
        NewBase = hb_maps:put(<<"balances">>, NewBalances, Base, Opts),
        send(
            transfer_notices(From, Recipient, Quantity, Req, Opts), 
            NewBase, 
            Opts
        )
    end.

transfer_notices(From, Recipient, Quantity, Req, Opts) ->
    % Extract forwarded tags (X- prefixed fields from request)
    ForwardedTags = extract_forwarded_tags(Req, Opts),
    DebitNotice = maps:merge(
        #{
            <<"action">> => <<"Debit-Notice">>,
            <<"recipient">> => Recipient,    
            <<"quantity">> => Quantity,
            <<"target">> => From              
        },
        ForwardedTags
    ),
    CreditNotice = maps:merge(
        #{
            <<"target">> => Recipient,       
            <<"action">> => <<"Credit-Notice">>,
            <<"sender">> => From,             
            <<"quantity">> => Quantity
        },
        ForwardedTags
    ),
    [DebitNotice, CreditNotice].

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
    maybe
        {ok, To} ?= hb_ao:resolve(Req, <<"recipient">>, Opts),
        {ok, Quantity} ?= hb_ao:resolve(Req, <<"quantity">>, Opts),
        true ?= (is_integer(Quantity) and (Quantity >= 0))
            orelse {error, <<"Quantity must be a non-negative integer.">>},
        true ?= validate_address(To),
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
        % Get current balances trie
        Balances =
            hb_ao:get(
                <<"balances">>,
                Base,
                #{ <<"device">> => <<"trie@1.0">> },
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
        % Update balances in the trie
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
        send(Notices, NewBaseWithBalAndSupply, Opts)
    end.

secure_set(Base, Assignment, Opts) ->
    maybe
        {ok, Req} ?= hb_ao:resolve(Assignment, <<"body">>, Opts),
        {ok, Base} ?= enforce_set_authority(Base, Req, Opts),
        {ok, Updates} ?= hb_ao:resolve(Req, <<"updates">>, Opts),
        % Apply updates to base state
        NewBase = hb_maps:merge(Base, Updates, Opts),
        {ok, NewBase}
    end.

enforce_set_authority(Base, Req, Opts) ->
    Setter = hb_ao:get(<<"from">>, Req, Opts),
    % Check if setter is the owner (or mint-authority as fallback)
    Owner = hb_ao:get(<<"owner">>, Base, Opts),
    MintAuthority = hb_ao:get(<<"mint-authority">>, Base, Opts),
    case {Owner, MintAuthority} of
        {not_found, not_found} ->
            {error, <<"No owner or mint-authority found.">>};
        {Setter, _} ->
            {ok, Base};
        {_, Setter} ->
            {ok, Base};
        _ ->
            {error, <<"Set authority mismatch.">>}
    end.

%%% Process helper functions.

send(Msg, Base, Opts) when not is_list(Msg) ->
    send([Msg], Base, Opts);
send(Msgs, Base, Opts) ->
    CurrentOutbox = hb_ao:get(<<"results/outbox">>, Base, [], Opts),
    NewOutbox = hb_util:message_to_ordered_list(CurrentOutbox, Opts) ++ Msgs,
    {
        ok,
        hb_ao:set(Base, <<"results/outbox">>, NewOutbox, Opts)
    }.

%% @doc Validate address format for security
validate_address(Address) when is_binary(Address) ->
    case byte_size(Address) of
        0 -> {error, <<"Recipient address cannot be empty.">>};
        _ ->
            % Check for path separators (security: prevent path traversal)
            case binary:match(Address, [<<"/">>, <<"\\">>]) of
                nomatch -> true; 
                _ -> 
                    {
                        error, 
                        <<"Recipient address cannot contain path separators.">>
                    }
            end
    end;
validate_address(_) ->
    {error, <<"Recipient address must be a binary.">>}.

%% @doc Extract tags with X- prefix for forwarding in notices
%% Follows AO token spec: tags beginning with "X-" are forwarded
%% Case-insensitive matching (both "x-" and "X-" are forwarded)
extract_forwarded_tags(Req, _Opts) ->
    case is_map(Req) of
        true ->
            maps:fold(
                fun(Key, Value, Acc) when is_binary(Key) ->
                    case byte_size(Key) >= 2 of
                        true ->
                            Prefix = binary:part(Key, 0, 2),
                            case string:lowercase(Prefix) of
                                <<"x-">> -> maps:put(Key, Value, Acc);
                                _ -> Acc
                            end;
                        false -> Acc
                    end;
                (_Key, _Value, Acc) -> Acc
                end,
                #{},
                Req
            );
        false -> #{}
    end.