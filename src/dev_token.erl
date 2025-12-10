%%% @doc A fast, simple implementation of AO token specification.
%%% Specification: https://cookbook_ao.arweave.net/references/api/token.html
-module(dev_token).
-export([compute/3, init/3, normalize/3, snapshot/3, balance/3, mint/3]).
-include_lib("include/hb.hrl").

%% @doc No-op on process initialization.
init(Base, _Req, _Opts) ->
    {ok, Base}.

%% @doc No-op on normalization.
normalize(Base, _Req, _Opts) ->
    {ok, Base}.

%% @doc No special processing for the creation of snapshots.
snapshot(Base, _Req, _Opts) ->
    {ok, Base}.

%% @doc Entrypoint for computations on token processes. Expects the `action'
%% key to hold the function to call.
compute(Base, Req, Opts) ->
    maybe
        {ok, SecureBase} ?= enforce_security(Base, Req, Opts),
        route(SecureBase, Req, Opts)
    end.

%% @doc Enforce the security constraints of the base state upon the request.
enforce_security(Base, _Req, _Opts) ->
    {ok, Base}.

%% @doc Route the request to the appropriate key resolution function, depending
%% upon the `action' specified.
route(Base, Req, Opts) ->
    ActionBin = hb_ao:get(<<"body/action">>, Req, Opts),
    case hb_util:to_lower(hb_ao:normalize_key(ActionBin)) of
        <<"transfer">> -> transfer(Base, Req, Opts);
        <<"mint">> -> mint(Base, Req, Opts);
        <<"set">> -> secure_set(Base, Req, Opts);
        _ ->
            ?event(error, {unsupported_token_action, ActionBin}),
            {ok, Base}
    end.

%% @doc Get the balance for an account. Normalize the minting state for that
%% account before returning.
balance(Base, Req, Opts) ->
    {ok, Account} = hb_ao:resolve(Req, <<"balance">>, Opts),
    {TimestampSource, Timestamp} =
        case hb_ao:get(<<"timestamp">>, Req, Opts) of
            not_found ->
                ?event(warning, {balance_request_without_timestamp, Req}, Opts),
                {<<"system">>, os:system_time(millisecond)};
            TS ->
                {<<"request">>, TS}
        end,
    ?event(
        debug_token,
        {balance_request,
            {account, Account},
            {timestamp_source, TimestampSource},
            {timestamp, Timestamp},
            {base, Base}
        },
        Opts
    ),
    {ok, NormBase} =
        normalize_mint(
            Base,
            #{
                <<"subject">> => Account,
                <<"timestamp">> => Timestamp
            },
            Opts
        ),
    ?event(debug_token, {after_mint_normalization, NormBase}, Opts),
    hb_ao:resolve_many(
        [
            NormBase,
            <<"balances">>,
            Account
        ],
        Opts
    ).

transfer(Base, Assignment, Opts) ->
    maybe
        {ok, _Timestamp} ?=
            hb_maps:find(
                <<"timestamp">>,
                Assignment,
                Opts
            ),
        % Gather transfer data from the request.
        {ok, Req} ?= hb_ao:resolve(Assignment, <<"body">>, Opts),
        {ok, From} ?= hb_ao:resolve(Req, <<"from">>, Opts),
        {ok, Recipient} ?= hb_ao:resolve(Req, <<"recipient">>, Opts),
        {ok, Quantity} ?= hb_ao:resolve(Req, <<"quantity">>, Opts),
        % Normalize the base's minting state for the sender.
        {ok, NormBase} ?=
            normalize_mint(
                Base,
                Assignment#{ <<"subject">> => From },
                Opts
            ),
        ?event({req, Req, from, From}),
        true ?= validate_address(Recipient),
        true ?= (is_integer(Quantity) and (Quantity >= 0))
            orelse {error, <<"Quantity must be a non-negative integer.">>},
        % Handle self-transfer: skip balance updates
        case From =:= Recipient of
            true ->
                dev_process_lib:send(
                    transfer_notices(From, Recipient, Quantity, Req, Opts),
                    NormBase, 
                    Opts
                );
            false ->
                transfer_between_accounts(
                    NormBase, 
                    From, 
                    Recipient, 
                    Quantity, 
                    Req, 
                    Opts
                )
        end
    else
        error ->
            {error, <<"Timestamp is required.">>}
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
        dev_process_lib:send(
            transfer_notices(From, Recipient, Quantity, Req, Opts), 
            NewBase, 
            Opts
        )
    end.

transfer_notices(From, Recipient, Quantity, Req, Opts) ->
    % Extract forwarded keys (X- prefixed fields from request)
    ForwardedKeys = dev_process_lib:forwarded_keys(Req, Opts),
    DebitNotice = maps:merge(
        #{
            <<"action">> => <<"Debit-Notice">>,
            <<"recipient">> => Recipient,    
            <<"quantity">> => Quantity,
            <<"target">> => From              
        },
        ForwardedKeys
    ),
    CreditNotice = maps:merge(
        #{
            <<"target">> => Recipient,       
            <<"action">> => <<"Credit-Notice">>,
            <<"sender">> => From,             
            <<"quantity">> => Quantity
        },
        ForwardedKeys
    ),
    [DebitNotice, CreditNotice].

mint(Base, Assignment, Opts) ->
    ?event(debug_mint,
        {running_mint,
            {base, Base},
            {assignment, Assignment}
        }
    ),
    HasMintDevice = has_mint_device(Base, Opts),
    ?event(debug_mint, {has_mint_device, HasMintDevice}),
    case HasMintDevice of
        false -> default_mint(Base, Assignment, Opts);
        true ->
            dev_process_lib:run_as(
                <<"mint">>,
                Base,
                Assignment#{ <<"path">> => <<"mint">> },
                Opts
            )
    end.

normalize_mint(Base, Req, Opts) ->
    case has_mint_device(Base, Opts) of
        false -> {ok, Base};
        true ->
            ReqWithPath = Req#{ <<"path">> => <<"mint">> },
            ?event(debug_mint,
                {running_mint,
                    {base, Base},
                    {req, Req}
                }
            ),
            dev_process_lib:run_as(
                <<"mint">>,
                Base,
                ReqWithPath,
                Opts
            )
    end.

default_mint(Base, Assignment, Opts) ->
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

has_mint_device(Base, Opts) ->
    case hb_ao:get(<<"mint-device">>, Base, Opts) of
        not_found -> false;
        _ -> true
    end.

enforce_mint_authority(Base, Req, Opts) ->
    Minter = hb_ao:get(<<"from">>, Req, Opts),
    case hb_ao:get(<<"mint-authority">>, Base, Opts) of
        Minter -> true;
        not_found -> {error, <<"Mint authority not found.">>};
        _ -> {error, <<"Mint authority mismatch.">>}
    end.

mint_single(Base, Req, Opts) ->
    maybe
        ?event(debug_mint, {before_resolve_recipient, Req}),
        {ok, To} ?= hb_ao:resolve(Req, <<"recipient">>, Opts),
        {ok, Quantity} ?= hb_ao:resolve(Req, <<"quantity">>, Opts),
        true ?= (is_integer(Quantity) and (Quantity >= 0))
            orelse {error, <<"Quantity must be a non-negative integer.">>},
        true ?= validate_address(To),
        ?event(debug_mint, {before_perform_mint, {to, To}, {quantity, Quantity}}),
        perform_mint(Base, #{ To => Quantity }, Opts)
    else
        Error -> ?event(debug_mint, {error, Error})
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
        dev_process_lib:send(Notices, NewBaseWithBalAndSupply, Opts)
    end.

secure_set(Base, Assignment, Opts) ->
    maybe
        {ok, Req} ?= hb_ao:resolve(Assignment, <<"body">>, Opts),
        true ?= enforce_set_authority(Base, Req, Opts),
        %NewKeys = hb_ao:set(Req, <<"path">>, unset, Opts),
        % Apply updates to base state
        hb_ao:resolve(Base, Req#{ <<"path">> => <<"set">> }, Opts)
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
            true;
        {_, Setter} ->
            true;
        _ ->
            {error, <<"Set authority mismatch.">>}
    end.

%%% Helper functions.

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