%%% @doc A fast, simple implementation of AO token specification.
%%% Specification: https://cookbook_ao.arweave.net/references/api/token.html
-module(dev_token).
-export([device_info/0, compute/3, init/3, normalize/3, snapshot/3, balance/3, mint/3]).
%%% Non-public device API functions. Note: Ensure that these are not exported
%%% as publicly callable device keys, either by having arity > 3, or by gating
%%% the public surface in `device_info/0`.
-export([handle_action/4, restore_signed_quantity/4]).
%%% Public helpers.
-export([validate_address/2]).
-include("include/hb.hrl").

-implements(<<"token@1.0">>).

-define(PROCESS_OUTBOX_DEVICE, <<"process-outbox@1.0">>).

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
%% @doc Root-level request envelope/control keys that must never become token
%% state through `Set'. Authority checks still see the original request.
-define(SET_CONTROL_KEYS,
    [
        <<"from">>,
        <<"action">>,
        <<"path">>,
        <<"body">>,
        <<"commitments">>,
        <<"committers">>,
        <<"ao-types">>,
        <<"target">>,
        <<"type">>,
        <<"id">>,
        <<"timestamp">>,
        <<"variant">>,
        <<"data-protocol">>,
        <<"set-mode">>,
        <<"priv">>,
        <<"hashpath">>
    ]
).
%% @doc `validate_address/2` built-in reserved keys list
-define(AO_RESERVED_ADDRESS_KEYS,
    [
        <<"path">>,
        <<"get">>,
        <<"set">>,
        <<"remove">>,
        <<"verify">>,
        <<"keys">>,
        <<"id">>,
        <<"commit">>,
        <<"committed">>,
        <<"committers">>,
        <<"index">>,
        <<"info">>,
        <<"set_path">>,
        <<"reserved_keys">>,
        <<"is_reserved_key">>,
        <<"dedup">>,
        <<"dedup-subject">>
    ]
).

%% @doc Return the configured `set` field whitelist. Defaults to open policy
%% via wildcard unless `whitelisted-fields` is explicitly restricted.
whitelisted_auth_fields(Base, Opts) ->
    maybe
        WhitelistedFields = hb_ao:get(
            <<"whitelisted-fields">>,
            Base,
            [<<"*">>],
            Opts
        ),
        ValidList = case WhitelistedFields of
            V when is_list(V) -> V;
            _ -> {error, <<"Invalid `whitelisted-fields` type.">>}
        end,
        true ?= is_list(ValidList),
        
        lists:filter(
            fun(X) -> is_binary(X) andalso byte_size(X) > 0 end,
            ValidList
        )
end.

%%% `~process@1.0' interface implementation.

%% @doc Return the public token device API.
device_info() ->
    #{
        exports =>
            [
                <<"compute">>,
                <<"init">>,
                <<"normalize">>,
                <<"snapshot">>,
                <<"balance">>,
                <<"mint">>
            ]
    }.

%% @doc Seed a flat on-chain process with a canonical account key.
init(Base, _Req, Opts) ->
    {ok, seed_holding(Base, Opts)}.

%% @doc No-op on normalization.
normalize(Base, _Req, _Opts) ->
    {ok, Base}.

%% @doc No special processing for the creation of snapshots.
snapshot(Base, _Req, _Opts) ->
    {ok, Base}.

%% @doc Entrypoint for computations on token processes. A token configured with
%% a scalar `swap-device' is scheduled in `all' mode, so every assignment is
%% offered to that device first. Only messages addressed to this process then
%% enter normal token routing, and the swap's own controls are not routed twice.
compute(Base, Assignment, Opts) ->
    case hb_maps:get(<<"swap-device">>, Base, not_found, Opts) of
        not_found ->
            compute_token(Base, Assignment, Opts);
        _ ->
            Seeded = seed_holding(Base, Opts),
            Settled = swap(Seeded, Assignment, Opts),
            Body = hb_maps:get(<<"body">>, Assignment, #{}, Opts),
            ProcID = hb_maps:get(<<"process">>, Assignment, <<>>, Opts),
            case tx_field(Body, <<"target">>, <<>>, Opts) of
                ProcID ->
                    case normalize_action(
                        hb_maps:get(<<"action">>, Body, <<>>, Opts)
                    ) of
                        <<"make-offer">> -> {ok, Settled};
                        <<"cancel-order">> -> {ok, Settled};
                        <<"register-interest">> -> {ok, Settled};
                        _ -> compute_token(Settled, Assignment, Opts)
                    end;
                _ ->
                    {ok, Settled}
            end
    end.

%% @doc Normal token routing for an addressed assignment.
compute_token(Base, Assignment, Opts) ->
    ?event({token_call, Assignment}),
    case deduplicate(Base, Assignment, Opts) of
        {skip, DedupedBase} ->
            ?event(token_short, {skipping_duplicate_assignment, Assignment}, Opts),
            {ok, DedupedBase};
        {ok, DedupedBase} ->
            maybe
                {ok, SecureReq} ?= enforce_security(DedupedBase, Assignment, Opts),
                {ok, Body} ?= field(<<"body">>, Assignment, Opts),
                {ok, Action} ?= field(<<"action">>, Body, Opts),
                {ok, ActionReq} ?=
                    restore_signed_quantity(Action, Assignment, SecureReq, Opts),
                {ok, Res} ?= handle_action(Action, DedupedBase, ActionReq, Opts),
                ?event(debug_token, {route_result, Res}, Opts),
                {ok, Res}
            else
                {error, Reason} ->
                    ?event(token_short, {error_during_token_call, Reason}, Opts),
                    send_error(Base, Assignment, Reason, Opts)
            end;
        {error, Reason} ->
            ?event(token_short, {error_during_token_dedup, Reason}, Opts),
            send_error(Base, Assignment, Reason, Opts)
    end.

%% @doc Hand every scheduled assignment to the configured selling device and
%% take back the resulting token state. This is the scalar equivalent of a
%% device stack, matching `carrier@1.0': nested stack configuration cannot be
%% represented by flat Arweave transaction tags.
swap(Base, Assignment, Opts) ->
    Device = hb_maps:get(<<"swap-device">>, Base, not_found, Opts),
    try hb_ao:resolve(Base#{ <<"device">> => Device }, Assignment, Opts) of
        {ok, Settled} -> Settled#{ <<"device">> => <<"token@1.0">> };
        _ -> Base
    catch
        _:_ -> Base
    end.

%% @doc Give a flat on-chain process its initial supply once. `initial-holder'
%% and `total-supply' are scalars that survive an Arweave process transaction;
%% `balances' is a submessage and does not.
seed_holding(Base, Opts) ->
    case {
        hb_maps:get(<<"initial-holder">>, Base, not_found, Opts),
        hb_maps:get(<<"balances">>, Base, not_found, Opts)
    } of
        {not_found, _} ->
            Base;
        {_, Balances} when Balances =/= not_found ->
            Base;
        {Holder, not_found} when is_binary(Holder) ->
            case hb_util:safe_int(
                hb_maps:get(<<"total-supply">>, Base, 1, Opts)
            ) of
                {ok, Supply} when Supply >= 0 ->
                    Base#{
                        <<"balances">> => #{ account_key(Holder) => Supply }
                    };
                _ ->
                    Base
            end;
        _ ->
            Base
    end.

%% @doc Read a value from the real L1 transaction fields recorded in its
%% `tx@1.0' commitment. A top-level key can be an ordinary tag with the same
%% spelling and is not proof that value moved to that address.
tx_field(Body, Field, Default, Opts) ->
    case hb_message:commitment(
        #{ <<"commitment-device">> => <<"tx@1.0">> },
        Body,
        Opts
    ) of
        {ok, _ID, Commitment} ->
            hb_maps:get(<<"field-", Field/binary>>, Commitment, Default, Opts);
        _ ->
            Default
    end.

transfer_quantity(Body, Opts) ->
    case hb_message:commitment(
        #{ <<"commitment-device">> => <<"tx@1.0">> },
        Body,
        Opts
    ) of
        {ok, _ID, Commitment} ->
            case hb_maps:get(<<"original-tags">>, Commitment, not_found, Opts) of
                Tags when is_map(Tags) -> quantity_tag(Tags, Opts);
                _ -> field(<<"quantity">>, Body, Opts)
            end;
        _ ->
            field(<<"quantity">>, Body, Opts)
    end.

restore_signed_quantity(Action, Assignment, SecureReq, Opts) ->
    case lists:member(
        normalize_action(Action),
        [<<"transfer">>, <<"deposit">>, <<"withdraw">>,
            <<"delegate">>, <<"undelegate">>]
    ) of
        false ->
            {ok, SecureReq};
        true ->
            case field(<<"body">>, Assignment, Opts) of
                {ok, SignedBody} ->
                    case hb_message:commitment(
                        #{ <<"commitment-device">> => <<"tx@1.0">> },
                        SignedBody,
                        Opts
                    ) of
                        {ok, _ID, _Commitment} ->
                            restore_signed_quantity(SignedBody, SecureReq, Opts);
                        not_found ->
                            {ok, SecureReq};
                        _ ->
                            {error, <<"Ambiguous transaction commitments.">>}
                    end;
                Error ->
                    Error
            end
    end.

restore_signed_quantity(SignedBody, SecureReq, Opts) ->
    maybe
        {ok, Quantity} ?= transfer_quantity(SignedBody, Opts),
        {ok, SecureBody} ?= field(<<"body">>, SecureReq, Opts),
        {ok, SecureReq#{
            <<"body">> =>
                (hb_message:uncommitted(SecureBody, Opts))#{
                    <<"quantity">> => Quantity
                }
        }}
    end.

quantity_tag(Tags, Opts) ->
    Matches = hb_maps:fold(
        fun(_Index, #{ <<"name">> := Name, <<"value">> := Value }, Acc) ->
                case is_quantity_tag(Name) of
                    true -> [Value | Acc];
                    false -> Acc
                end;
           (_Index, _Tag, Acc) ->
                Acc
        end,
        [],
        Tags,
        Opts
    ),
    case Matches of
        [Value] -> {ok, Value};
        [] -> {error, <<"Quantity tag is required.">>};
        _ -> {error, <<"Ambiguous Quantity tags.">>}
    end.

is_quantity_tag(Name) when is_binary(Name) ->
    try hb_util:to_lower(Name) =:= <<"quantity">>
    catch
        _:_ -> false
    end;
is_quantity_tag(_) ->
    false.

%% @doc Deduplicate token computations by the signed assignment body. Replayed
%% assignments get fresh slots, so the assignment itself cannot be the subject.
deduplicate(Base, Assignment, Opts) ->
    BaseDevice = hb_maps:get(<<"device">>, Base, not_found, Opts),
    case hb_ao:resolve(Base, {as, <<"dedup@1.0">>, Assignment}, Opts) of
        {Status, DedupedBase} when Status =:= ok; Status =:= skip ->
            {Status, restore_device(BaseDevice, DedupedBase, Opts)};
        Error ->
            Error
    end.

restore_device(not_found, Base, _Opts) ->
    Base;
restore_device(Device, Base, Opts) ->
    hb_ao:set(Base, <<"device">>, Device, Opts).

%% @doc Enforce the security constraints of the base state upon the request.
enforce_security(Base, Req, Opts) ->
    SecurityDevice = hb_maps:get(
        <<"security-device">>,
        Base,
        <<"security@1.0">>,
        Opts
    ),
    case run_as_device(<<"security">>, SecurityDevice, Base, Req, Opts) of
        {ok, SecureReq} -> {ok, SecureReq};
        {skip, Reason} -> {error, Reason}
    end.

%% @doc Route the request to the appropriate key resolution function, depending
%% upon the `action' specified.
handle_action(Action, Base, Req, Opts) ->
    ?event(token_short, {token_action, Action}, Opts),
    case normalize_action(Action) of
        <<"transfer">> -> transfer(Base, Req, Opts);
        <<"set">> -> secure_set(Base, Req, Opts);
        <<"subscribe">> -> outbox_subscribe(Base, Req, Opts);
        <<"unsubscribe">> -> outbox_unsubscribe(Base, Req, Opts);
        MintDevAction -> action_as_mint_device(MintDevAction, Base, Req, Opts)
    end.

%% @doc Malformed action values follow the unsupported-action path.
normalize_action(Action) ->
    try hb_util:to_lower(hb_ao:normalize_key(Action))
    catch
        error:_ -> <<>>
    end.

%% @doc Get the balance for an account. Normalize the minting state for that
%% account before returning.
balance(Base, Req, Opts) ->
    maybe
        {ok, Account0} ?= field(<<"balance">>, Req, Opts),
        true ?= validate_address(Account0, [], Opts),
        Account = account_key(Account0),
        ?event(
            debug_token,
            {balance_request,
                {account, Account0},
                {canonical_account, Account},
                {base, Base}
            },
            Opts
        ),
        {ok, NormBase} ?=
            normalize_mint(
                Base,
                hb_ao:set(
                    Req#{ <<"device">> => <<"message@1.0">> },
                    <<"subject">>,
                    Account,
                    Opts
                ),
                Opts
            ),
        BalanceRes =
            hb_ao:resolve_many(
                [
                    NormBase,
                    <<"balances">>,
                    #{ <<"path">> => Account }
                ],
                Opts
            ),
        ?event(
            debug_token,
            {balance_after_mint_normalization,
                {account, Account},
                {balance, BalanceRes}
            },
            Opts
        ),
        case BalanceRes of
            {ok, Balance} -> {ok, Balance};
            {error, not_found} -> {ok, 0};
            {error, Reason} -> {error, Reason}
        end
    end.

transfer(Base, Assignment, Opts) ->
    maybe
        % Gather transfer data from the request.
        {ok, Req} ?= field(<<"body">>, Assignment, Opts),
        {ok, From0} ?= field(<<"from">>, Req, Opts),
        {ok, Recipient0} ?= field(<<"recipient">>, Req, Opts),
        {ok, Quantity0} ?= transfer_quantity(Req, Opts),
        Quantity =
            case hb_util:safe_int(Quantity0) of
                {ok, Integer} -> Integer;
                _ -> Quantity0
            end,
        % validate From/Recipient sanity
        true ?= validate_address(From0, [], Opts),
        true ?= validate_address(Recipient0, [], Opts),
        From = account_key(From0),
        Recipient = account_key(Recipient0),
        % Normalize the base's minting state for the sender.
        {ok, NormBase} ?=
            case Quantity of
                0 -> {ok, Base};
                _ ->
                    normalize_mint(
                        Base,
                        Assignment#{ <<"subject">> => From },
                        Opts
                    )
            end,
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
        true ?= (is_integer(SenderBalance) and is_integer(RecipientBalance)
                and (SenderBalance >= 0) and (RecipientBalance >= 0))
            orelse {error, <<"Invalid balance values.">>},
        true ?= (is_integer(Quantity) and (Quantity >= 0))
            orelse {error, <<"Quantity must be a non-negative integer.">>},
        true ?= (SenderBalance >= Quantity) 
            orelse {error, <<"Insufficient balance.">>},
        % Handle self-transfer: skip balance updates
        NewBaseAfterTransfer =
            case (Quantity =:= 0) orelse (From =:= Recipient) of
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
        {ok, WithNotices} ?= outbox_send(
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
            send_error(Base, Assignment, Reason, Opts)
    end.

transfer_notices(From, Recipient, Quantity, Req, Opts) ->
    % Extract forwarded keys (X- prefixed fields from request)
    ForwardedKeys = forwarded_keys(Req, Opts),
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
%% mint requests, normalize its state (prior to `transfer`s, etc), or ignore
%% the request altogether. If a public token `mint` request includes
%% `body.subject`, hoist it to the top-level request shape expected by the mint
%% device before dispatch.
mint(Base, Assignment, Opts) ->
    case field(<<"body">>, Assignment, Opts) of
        {error, _} ->
            as_mint_device(<<"mint">>, Base, Assignment, Opts);
        {ok, Req} ->
            case hb_maps:find(<<"subject">>, Req, Opts) of
                error ->
                    as_mint_device(<<"mint">>, Base, Assignment, Opts);
                {ok, Subject} ->
                    maybe
                        true ?= validate_address(Subject, [], Opts),
                        MintReq1 =
                            hb_ao:set(
                                Assignment#{ <<"device">> => <<"message@1.0">> },
                                <<"subject">>,
                                account_key(Subject),
                                Opts
                            ),
                        as_mint_device(<<"mint">>, Base, MintReq1, Opts)
                    end
            end
    end.

%% @doc Execute the mint device's main key, but return the state in its 
%% unmodified form if the execution returns an error.
normalize_mint(Base, Assignment, Opts) ->
    try mint(Base, Assignment, Opts) of
        {ok, NewBase} -> {ok, NewBase};
        {error, _} -> {ok, Base}
    catch
        throw:{error, {device_not_loadable, _Device, _Reason}} ->
            {ok, Base}
    end.

%% @doc Check if the action is supported by the mint device interface.
is_supported_mint_action(Action) ->
    lists:member(Action, ?MINT_ACTIONS).

%% @doc Verify if the action is a supported path on the mint device interface,
%% and if so, switch to the mint device and run it. Unsupported actions fall through
%% send_error/4 codepath.
action_as_mint_device(Action, Base, Req, Opts) ->
    case hb_ao:get(<<"mint-device">>, Base, undefined, Opts) of
        undefined ->
            send_error(Base, Req, <<"mint-device not configured">>, Opts);
        _ ->
            case is_supported_mint_action(Action) of
                true when Action =:= <<"mint">> -> mint(Base, Req, Opts);
                true -> as_mint_device(Action, Base, Req, Opts);
                false ->
                    ?event(error, {unsupported_token_action, Action}, Opts),
                    send_error(Base, Req, <<"unsupported action: ", Action/binary>>, Opts)
            end
    end.

%% @doc Run a given `path' on the mint device.
as_mint_device(Path, Base, Req, Opts) ->
    MintBase = ensure_mint_device(Base, Opts),
    MintDevice = hb_ao:get(<<"mint-device">>, MintBase, Opts),
    run_as_device(
        <<"mint">>,
        MintDevice,
        MintBase,
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
%% base state if so. The setter can only mutate whitelisted fields.
secure_set(Base, Assignment, Opts) ->
    maybe
        Req = hb_maps:get(<<"body">>, Assignment, not_found, Opts),
        true ?= is_map(Req) orelse {error, <<"Set body must be a message.">>},
        true ?= enforce_set_authority(Base, Req, Opts),
        Mutation = set_mutation_fields(Req, Opts),
        % Check the auth is touching whitelisted fields only.
        true ?= enforce_whitelisted_fields(Base, Mutation, Opts),
        % Apply updates to base state.
        hb_ao:resolve(Base, Mutation#{ <<"path">> => <<"set">> }, Opts)
    end.

set_mutation_fields(Req, Opts) ->
    hb_maps:without(?SET_CONTROL_KEYS, Req, Opts).

enforce_whitelisted_fields(Base, Req, Opts) ->
    maybe
        Keys = hb_maps:keys(Req, Opts),
        WhitelistedFields = whitelisted_auth_fields(Base, Opts),
        true ?= is_list(WhitelistedFields) orelse
                    {error, <<"Invalid `whitelisted-fields` type.">>},
        case lists:member(<<"*">>, WhitelistedFields) of
            true ->
                true;
            false ->
                case lists:all(
                    fun(Key) -> lists:member(Key, WhitelistedFields) end,
                    Keys
                ) of
                    true -> true;
                    false -> {error, <<"Attempted to set non-whitelisted fields.">>}
                end
            end
    end.

%% @doc Enforce that the caller is the `set` authority. The configured security
%% device owns both static signer-set and dynamic ownership semantics.
enforce_set_authority(Base, Req, Opts) ->
    maybe
        Setter = hb_maps:get(<<"from">>, Req, not_found, Opts),
        true ?= (Setter =/= not_found) orelse
                {error, <<"Setter not found.">>},
        AuthRes =
            security_validate(
                <<"set-authority">>,
                Base,
                Req,
                Setter,
                Opts
            ),
        true ?= AuthRes
    end.

%%% Helper functions.

%% @doc Read request data without invoking the message's device.
field(Key, Msg, Opts) ->
    case hb_maps:find(Key, Msg, Opts) of
        {ok, Value} -> {ok, Value};
        error -> {error, not_found}
    end.

%% @doc Validate address format for security. the validation
%% allows binary addresses up to 128 bytes and prevent invalid
%% addresses such as trie reserved keys.
validate_address(Address, CustomList) ->
    validate_address(Address, CustomList, #{}).

validate_address(Address, CustomList, Opts) when is_binary(Address), is_list(CustomList) ->
    ReservedKeys = ?AO_RESERVED_ADDRESS_KEYS ++ CustomList,
    CanonicalReservedKeys = [hb_util:to_lower(Key) || Key <- ReservedKeys, is_binary(Key)],
    case byte_size(Address) of
        0 -> {error, <<"Address cannot be empty.">>};
        N when N > 128 -> {error, <<"Address is too long.">>};
        _ ->
            TrieReservedKeys = trie_reserved_keys(Opts),
            maybe
                {ok, AccountKey} ?= address_comparison_key(Address),
                true ?= (not is_reserved_trie_key(Address, TrieReservedKeys))
                    orelse {error, <<"Address uses a reserved trie internal key.">>},
                true ?= (not is_reserved_trie_key(AccountKey, TrieReservedKeys))
                    orelse {error, <<"Address uses a reserved trie internal key.">>},
                true ?= (not is_reserved_custom_key(Address, ReservedKeys))
                    orelse {error, <<"Address is a reserved ao/custom key">>},
                true ?= (not is_reserved_custom_key(AccountKey, CanonicalReservedKeys))
                    orelse {error, <<"Address is a reserved ao/custom key">>},
                % Check for path separators (security: prevent path traversal) and whitespaces.
                case binary:match(Address, [<<"/">>, <<"\\">>, <<" ">>, <<"\n">>, <<"\r">>, <<"\t">>]) of
                    nomatch -> true;
                    _ -> {error, <<"Address cannot contain path separators or whitespaces">>}
                end
            end
    end;
validate_address(_, _, _) ->
    {error, <<"Address must be a binary.">>}.

address_comparison_key(Address) ->
    try {ok, hb_util:to_lower(Address)}
    catch
        error:_ -> {error, <<"Address contains unsupported characters.">>}
    end.

account_key(Address) when is_binary(Address) ->
    hb_committer:uri(Address).

is_reserved_trie_key(Key, ReservedKeys) ->
    lists:member(Key, ReservedKeys).

trie_reserved_keys(Opts) ->
    {ok, Trie} = hb_device_load:reference(<<"trie@1.0">>, Opts),
    maps:get(reserved, Trie:device_info(), []).

%% @doc Check if the given Key exists in the passed List
is_reserved_custom_key(Key, List) when is_binary(Key), is_list(List) ->
    lists:member(Key, List);
is_reserved_custom_key(_, _) -> 
    false.

outbox_send(Messages, Base, Opts) ->
    maybe
        {ok, Outbox} ?= process_outbox(Opts),
        Outbox:send(
            Base,
            #{ <<"messages">> => Messages },
            Opts
        )
    end.

outbox_subscribe(Base, Req, Opts) ->
    maybe
        {ok, Outbox} ?= process_outbox(Opts),
        Outbox:subscribe(Base, Req, Opts)
    end.

outbox_unsubscribe(Base, Req, Opts) ->
    maybe
        {ok, Outbox} ?= process_outbox(Opts),
        Outbox:unsubscribe(Base, Req, Opts)
    end.

process_outbox(Opts) ->
    case hb_device_load:reference(?PROCESS_OUTBOX_DEVICE, Opts) of
        {ok, Outbox} ->
            {ok, Outbox};
        {error, Reason} ->
            {error, {process_outbox_not_loadable, Reason}}
    end.

forwarded_keys(Req, Opts) ->
    hb_maps:filter(
        fun(Key, _Value) ->
            KeyBin = hb_util:to_lower(hb_util:bin(Key)),
            binary:match(KeyBin, <<"x-">>) =:= {0, 2}
        end,
        Req,
        Opts
    ).

security_validate(Key, Base, _SubjectMsg, From, Opts) ->
    SecurityDevice = hb_maps:get(
        <<"security-device">>,
        Base,
        <<"security@1.0">>,
        Opts
    ),
    % `From' has already been normalized by `security@1.0' in the compute path.
    % Do not pass the Set body back as a validation subject: root keys such as
    % `path' are user mutation data here and must not affect security routing.
    ValidateReq = #{
        <<"path">> => <<"validate">>,
        <<"key">> => Key,
        <<"from">> => From
    },
    case run_as_device(<<"security">>, SecurityDevice, Base, ValidateReq, Opts) of
        {ok, true} -> true;
        {error, Reason} -> {error, Reason};
        {skip, Reason} -> {error, Reason};
        Other -> {error, {security_validate_unexpected_result, Other}}
    end.

run_as_device(Key, Device, Base, Path, Opts) when not is_map(Path) ->
    run_as_device(Key, Device, Base, #{ <<"path">> => Path }, Opts);
run_as_device(Key, Device, Base, Req, Opts) ->
    BaseDevice = hb_maps:get(<<"device">>, Base, not_found, Opts),
    {ok, PreparedMsg} =
        hb_ao:resolve(
            ensure_process_key(Base, Opts),
            #{
                <<"path">> => <<"set">>,
                <<"device">> => Device,
                <<"input-prefix">> =>
                    case hb_maps:get(<<"input-prefix">>, Base, not_found, Opts) of
                        not_found -> <<"process">>;
                        Prefix -> Prefix
                    end,
                <<"output-prefixes">> =>
                    hb_maps:get(
                        <<Key/binary, "-output-prefixes">>,
                        Base,
                        undefined,
                        Opts
                    )
            },
            Opts
        ),
    {Status, BaseResult} = hb_ao:resolve(PreparedMsg, Req, Opts),
    case {Status, BaseResult} of
        {ok, #{ <<"device">> := Device }} ->
            {ok, hb_ao:set(BaseResult, #{ <<"device">> => BaseDevice }, Opts)};
        _ ->
            {Status, BaseResult}
    end.

ensure_process_key(Base, Opts) ->
    case hb_maps:get(<<"process">>, Base, not_found, Opts) of
        not_found ->
            {ok, Committed} = hb_message:with_only_committed(Base, Opts),
            hb_ao:set(
                hb_message:uncommitted(Base, Opts),
                #{ <<"process">> => Committed },
                Opts#{ <<"hashpath">> => ignore }
            );
        _ ->
            Base
    end.

send_error(Base, Assignment, Reason, Opts) when is_atom(Reason) ->
    send_error(Base, Assignment, atom_to_binary(Reason), Opts);
send_error(Base, Assignment, Reason, Opts) when not is_binary(Reason) ->
    send_error(
        Base,
        Assignment,
        iolist_to_binary(io_lib:format("~0p", [Reason])),
        Opts
    );
send_error(Base, Assignment, Reason, Opts) when is_binary(Reason) ->
    Body = hb_maps:get(<<"body">>, Assignment, #{}, Opts),
    case field(<<"from">>, Body, Opts) of
        {error, Error} ->
            ?event(token_short, {skipping_error_report, Error}, Opts),
            {ok, Base};
        {ok, Target} ->
            outbox_send(
                #{
                    <<"target">> => Target,
                    <<"reason">> => Reason
                },
                Base,
                Opts
            )
    end.
