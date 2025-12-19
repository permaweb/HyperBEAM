%%% @doc A fast, simple implementation of AO token specification.
%%% Specification: https://cookbook_ao.arweave.net/references/api/token.html
-module(dev_token).
-export([compute/3, init/3, normalize/3, snapshot/3, balance/3, mint/3]).
%%% Non-public device API functions. Note: Ensure that these are not exported
%%% as publicly callable device keys, either by having arity >= 3, or explicitly
%%% excluding in an `info/1` response.
-export([handle_action/4]).
%%% Public helpers.
-export([validate_address/1]).
-include_lib("include/hb.hrl").

%% @doc `Action' values that should be handled by the `mint-device'.
-define(MINT_ACTIONS,
    [
        <<"mint">>,
        <<"deposit">>,
        <<"withdraw">>,
        <<"delegate">>,
        <<"undelegate">>,
        <<"notify">>,
        <<"register">>
    ]
).

%%% `~process@1.0' interface implementation.

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
%% key to hold the `path' to execute after enforcing the token's security
%% constraints. Always returns the base state unmodified in the event of 
%% downstream device errors, such that invalid interactions do not result in
%% invalid `~process@1.0' states.
compute(Base, Assignment, Opts) ->
    ?event({token_call, Assignment}),
    maybe
        {ok, SecureReq} ?= enforce_security(Base, Assignment, Opts),
        {ok, Action} ?= hb_ao:resolve(Assignment, <<"body/action">>, Opts),
        {ok, Res} ?= handle_action(Action, Base, SecureReq, Opts),
        ?event(debug_token, {route_result, Res}, Opts),
        {ok, Res}
    else
        {error, Reason} ->
            ?event(token_short, {error_during_token_call, Reason}, Opts),
            send_error(Base, Assignment, Reason, Opts)
    end.

%% @doc Enforce the security constraints of the base state upon the request.
enforce_security(Base, Req, Opts) ->
    case dev_process_lib:run_as(<<"security">>, Base, Req, Opts) of
        {ok, SecureReq} -> {ok, SecureReq};
        {skip, Reason} -> {error, Reason}
    end.

%% @doc Route the request to the appropriate key resolution function, depending
%% upon the `action' specified.
handle_action(Action, Base, Req, Opts) ->
    ?event(debug_token, {action, Action}, Opts),
    case hb_util:to_lower(hb_ao:normalize_key(Action)) of
        <<"transfer">> -> transfer(Base, Req, Opts);
        <<"set">> -> secure_set(Base, Req, Opts);
        <<"subscribe">> -> dev_process_outbox:subscribe(Base, Req, Opts);
        <<"unsubscribe">> -> dev_process_outbox:unsubscribe(Base, Req, Opts);
        MintDevAction -> action_as_mint_device(MintDevAction, Base, Req, Opts)
    end.

%% @doc Get the balance for an account. Normalize the minting state for that
%% account before returning.
balance(Base, Req, Opts) ->
    {ok, Account} = hb_ao:resolve(Req, <<"balance">>, Opts),
    ?event(
        debug_token,
        {balance_request,
            {account, Account},
            {base, Base}
        },
        Opts
    ),
    {ok, NormBase} =
        normalize_mint(
            Base,
            hb_ao:set(Req, <<"subject">>, Account, Opts),
            Opts
        ),
    Balances =
        hb_ao:resolve_many(
            [
                NormBase,
                <<"balances">>,
                Account
            ],
            Opts
        ),
    ?event(
        debug_token,
        {balance_after_mint_normalization,
            {account, Account},
            {balances, Balances}
        },
        Opts
    ),
    {ok, Balances}.

transfer(Base, Assignment, Opts) ->
    maybe
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
        % Retrieve balances from the base state.
        Balances = hb_ao:get(<<"balances">>, NormBase, Opts),
        ?event(debug_token, {balances_before_transfer, Balances}, Opts),
        SenderBalance = hb_ao:get(From, Balances, 0, Opts),
        RecipientBalance = hb_ao:get(Recipient, Balances, 0, Opts),
        ?event(
            debug_token,
            {transfer_balances, 
                {from, From}, 
                {to, Recipient},
                {quantity, Quantity},
                {sender_balance, SenderBalance},
                {recipient_balance, RecipientBalance}
            },
            Opts
        ),
        % Sanity check the transfer request.
        true ?= (is_integer(SenderBalance) and is_integer(RecipientBalance))
            orelse {error, <<"Invalid balance types.">>},
        true ?= (is_integer(Quantity) and (Quantity >= 0))
            orelse {error, <<"Quantity must be a non-negative integer.">>},
        true ?= (SenderBalance >= Quantity) 
            orelse {error, <<"Insufficient balance.">>},
        true ?= validate_address(Recipient),
        % Handle self-transfer: skip balance updates
        NewBaseAfterTransfer =
            case From =:= Recipient of
                true -> NormBase;
                false ->
                    {ok, NewBalances} =
                        hb_ao:resolve(
                            Balances,
                            #{
                                <<"path">> => <<"set">>,
                                From => SenderBalance - Quantity,
                                Recipient => RecipientBalance + Quantity
                            },
                            Opts
                        ),
                    hb_maps:put(<<"balances">>, NewBalances, NormBase, Opts)
            end,
        % Send transfer notices.
        WithNotices = dev_process_outbox:send(
            transfer_notices(From, Recipient, Quantity, Req, Opts),
            NewBaseAfterTransfer,
            Opts
        ),
        {ok, WithNotices}
    else
        {error, Reason} ->
            ?event(token_short, {ignoring_errored_transfer, Reason}, Opts),
            ?event(debug_token,
                {errored_transfer,
                    {reason, Reason},
                    {returning_base, Base}
                },
                Opts
            ),
            {ok, send_error(Base, Assignment, Reason, Opts)}
    end.

transfer_notices(From, Recipient, Quantity, Req, Opts) ->
    % Extract forwarded keys (X- prefixed fields from request)
    ForwardedKeys = dev_process_outbox:forwarded_keys(Req, Opts),
    DebitNotice =
        ForwardedKeys#{
            <<"action">> => <<"Debit-Notice">>,
            <<"recipient">> => Recipient,    
            <<"quantity">> => Quantity,
            <<"target">> => From              
        },
    CreditNotice =
        ForwardedKeys#{
            <<"target">> => Recipient,       
            <<"action">> => <<"Credit-Notice">>,
            <<"sender">> => From,             
            <<"quantity">> => Quantity
        },
    [DebitNotice, CreditNotice].

%%% Mint device orchestration.

%% @doc Call the mint device's main entrypoint, allowing it to handle explicit
%% mint requests, normalize its state (prior to `transfer's, etc), or ignore
%% the request altogether.
mint(Base, Assignment, Opts) ->
    as_mint_device(<<"mint">>, Base, Assignment, Opts).

%% @doc Execute the mint device's main key, but return the state in its 
%% unmodified form if the execution returns an error.
normalize_mint(Base, Assignment, Opts) ->
    case mint(Base, Assignment, Opts) of
        {ok, NewBase} -> {ok, NewBase};
        {error, _} -> {ok, Base}
    end.

%% @doc Check if the action is supported by the mint device interface.
is_supported_mint_action(Action) ->
    lists:member(Action, ?MINT_ACTIONS).

%% @doc Verify if the action is a supported path on the mint device interdface,
%% and if so, switch to the mint device and run it.
action_as_mint_device(Action, Base, Req, Opts) ->
    case is_supported_mint_action(Action) of
        true -> as_mint_device(Action, Base, Req, Opts);
        false ->
            ?event(error, {unsupported_token_action, Action}, Opts),
            {ok, Base}
    end.

%% @doc Run a given `path' on the mint device.
as_mint_device(Path, Base, Req, Opts) ->
    dev_process_lib:run_as(
        <<"mint">>,
        ensure_mint_device(Base, Opts),
        Req#{ <<"path">> => Path },
        Opts
    ).

%% @doc Add the default mint device if none is present already.
ensure_mint_device(Base, Opts) ->
    hb_ao:set(
        Base,
        #{
            <<"mint-device">> =>
                hb_ao:get(
                    <<"mint-device">>,
                    Base,
                    <<"mint-authority@1.0">>,
                    Opts
                )
        },
        Opts
    ).

%%% Secure `set' call orchestration.

%% @doc Ensure that the caller is the `set' authority, and apply changes to the
%% base state if so.
secure_set(Base, Assignment, Opts) ->
    maybe
        {ok, Req} ?= hb_ao:resolve(Assignment, <<"body">>, Opts),
        true ?= enforce_set_authority(Base, Req, Opts),
        % Apply updates to base state
        hb_ao:resolve(Base, Req#{ <<"path">> => <<"set">> }, Opts)
    end.

%% @doc Enforce that the caller is the `set' authority.
enforce_set_authority(Base, Req, Opts) ->
    Setter = hb_ao:get(<<"from">>, Req, Opts),
    case hb_ao:get(<<"set-authority">>, Base, Opts) of
        Setter -> true;
        not_found -> {error, <<"No set-authority found in `Base' state.">>};
        _ -> {error, <<"Caller is not the `set-authority'.">>}
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

send_error(Base, Assignment, Reason, Opts) when is_binary(Reason) ->
    case hb_ao:resolve(Assignment, <<"body/from">>, Opts) of
        {error, Error} ->
            ?event(token_short, {skipping_error_report, Error}, Opts),
            {ok, Base};
        {ok, Target} ->
            dev_process_outbox:send(
                #{
                    <<"target">> => Target,       
                    <<"reason">> => Reason
                },
                Base,
                Opts
            )
    end.