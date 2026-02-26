%%% @doc The core mathematical functions of the `~mint@1.0` device.
%%% 
%%% The mint device makes use of Erlang's built-in bignum support to perform
%%% all mathematical operations without loss of precision or conversion.
-module(dev_mint_math).
-export([should_mint/3, mint/3]).
-include("include/hb.hrl").
-define(UNITS_TO_PRINTABLE(X), X).

%% @doc Determine if we should execute the next mint cycle.
should_mint(State, Req, Opts) ->
    % Gather the current state of the mint process.
    Cycle = hb_util:int(hb_maps:get(<<"cycle">>, State, Opts)),
    PeriodMs = hb_util:int(hb_maps:get(<<"period">>, State, Opts)),
    StartTimeMs = hb_util:int(hb_maps:get(<<"start-time">>, State, Opts)),
    CurrentTimeMs = hb_util:int(hb_maps:get(<<"timestamp">>, Req, Opts)),
    % Calculate whether we have exceeded the timestamp of the next cycle.
    NextCycleTimeMs = StartTimeMs + (Cycle * PeriodMs),
    CurrentTimeMs >= NextCycleTimeMs.

%% @doc Perform a single mint cycle, returning a new state containing updated 
%% metadata and instructions to the `Client` address.
mint(State, _Req, Opts) ->
    Cycle = hb_util:int(hb_maps:get(<<"cycle">>, State, Opts)),
    AlreadyMinted = hb_util:int(hb_maps:get(<<"minted">>, State, Opts)),
    ?event(debug,
        {starting_mint_cycle,
            {already_minted, AlreadyMinted},
            {cycle, Cycle},
            {state, State}
        }
    ),
    TotalToDistribute = units_to_distribute(State, Opts),
    ?event(debug, {tokens_to_distribute, ?UNITS_TO_PRINTABLE(TotalToDistribute)}),
    % Calculate the number of units to issue in total for each resource.
    UnitsPerResource = units_per_resource(TotalToDistribute, State, Opts),
    ?event(debug, {units_per_resource, UnitsPerResource}),
    % Calculate the distribution per address of units assigned to each resource.
    ResourceDistributions =
        hb_maps:map(
            fun(Resource, Units) ->
                % Find the resource in the state. If the resource is not found,
                % return an empty `~trie@1.0` message.
                Addresses =
                    hb_util:deep_get(
                        <<"balances/", Resource/binary>>,
                        State,
                        #{ <<"device">> => <<"trie@1.0">> },
                        Opts
                    ),
                % Calculate a distribution message for each address in the resource.
                distribution_per_address(Resource, Units, Addresses, Opts)
            end,
            UnitsPerResource
        ),
    ?event(debug, {resource_distributions, ResourceDistributions}),
    Distributions = lists:flatten(hb_maps:values(ResourceDistributions)),
    Allocated = distributions_to_total_units(Distributions, Opts),
    ?event(debug, {combined_distributions, Distributions}),
    {ok, Results} = distributions_to_results(State, Distributions, Opts),
    DustCarriedForward = TotalToDistribute - Allocated,
    NewMintedSupply = AlreadyMinted + Allocated,
    NewState =
        hb_ao:set(
            State,
            #{
                <<"cycle">> => Cycle + 1,
                <<"minted">> => NewMintedSupply,
                <<"results">> => Results
            },
            Opts
        ),
    CheckRes = dev_mint_stats:sanity_check(State, NewState, Opts),
    ?event(mint_short,
        {mint_cycle_complete,
            {sanity_check, CheckRes},
            {cycles, Cycle},
            {supply_to_mint, ?UNITS_TO_PRINTABLE(TotalToDistribute)},
            {allocated_units, ?UNITS_TO_PRINTABLE(Allocated)},
            {dust_carried_forward, ?UNITS_TO_PRINTABLE(DustCarriedForward)},
            {new_minted_supply, ?UNITS_TO_PRINTABLE(NewMintedSupply)},
            {distributions, length(Distributions)}
        }
    ),
    case CheckRes of
        ok -> {ok, NewState};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Return the total number of units to distribute.
units_to_distribute(State, Opts) ->
    MintTotal = hb_util:int(hb_maps:get(<<"mint-total">>, State, Opts)),
    AlreadyMinted = hb_util:int(hb_maps:get(<<"minted">>, State, Opts)),
    CycleProportionNumerator =
        hb_util:int(hb_maps:get(<<"cycle-proportion">>, State, Opts)),
    CycleProportionDenominator =
        hb_util:int(hb_maps:get(<<"cycle-proportion-denominator">>, State, Opts)),
    Remaining = MintTotal - AlreadyMinted,
    case CycleProportionDenominator of
        0 -> throw({error, zero_denominator_division});
        _ -> (Remaining * CycleProportionNumerator) div CycleProportionDenominator
    end.
    
%% @doc Return the number of units to distribute accross the addresses in each
%% resource of the state.
units_per_resource(TotalToDistribute, State, Opts) ->
    Resources =
        hb_private:reset(hb_message:uncommitted(
            hb_maps:get(<<"resources">>, State, Opts)
        )),
    ResourceWeights =
        hb_maps:map(
            fun(_ResourceID, Resource) ->
                hb_util:int(hb_maps:get(<<"weight">>, Resource, Opts))
            end,
            Resources
        ),
    TotalWeights = lists:sum(hb_maps:values(ResourceWeights)),
    case {map_size(ResourceWeights), TotalWeights} of
        {0, _} ->
            ResourceWeights;
        {_, 0} ->
            throw({error, zero_denominator_division});
        _ ->
            hb_maps:map(
                fun(_Resource, Weight) ->
                    (TotalToDistribute * Weight) div TotalWeights
                end,
                ResourceWeights
            )
    end.

%% @doc Return the number of units to distribute for each address in a resource.
distribution_per_address(Resource, UnitsForResource, BalanceMessages, Opts) ->
    ?event(debug, {units_for_resource, UnitsForResource}),
    % Get the accounts from the balance messages. Note that we are careful to
    % ensure that the hashpath is not generated and that we do not write to the
    % store, such that no IDs or writes are performed.
    Accounts = resolve_accounts(BalanceMessages, Opts),
    ?event(debug, {addresses, Accounts}),
    TotalQuantity =
        lists:sum(
            hb_maps:values(
                hb_maps:map(
                    fun(_Address, AddressDetails) ->
                        hb_util:int(
                            hb_maps:get(
                                <<"quantity">>,
                                AddressDetails,
                                0,
                                Opts
                            )
                        )
                    end,
                    Accounts
                )
            )
        ),
    case TotalQuantity of
        0 -> throw({error, zero_denominator_division});
        _ -> ok
    end,
    ?event(debug, {total_quantity, TotalQuantity}),
    hb_maps:values(
        hb_maps:map(
            fun(Minter, AddressDetails) ->
                % Gather details for the address.
                Quantity =
                    hb_util:int(
                        hb_maps:get(
                            <<"quantity">>,
                            AddressDetails,
                            0,
                            Opts
                        )
                    ),
                Recipient =
                    hb_maps:get(
                        <<"recipient">>,
                        AddressDetails,
                        Minter,
                        Opts
                    ),
                % Calculate units to mint for the address.
                % We are mutiplying before because of the way the div operator
                % works, it will return 0 if Q < TQ which will be most of the
                % time. Subsequently, instead we multiply before division.
                Units = (UnitsForResource * Quantity) div TotalQuantity,
                % Return the mint message for the address, containing:
                % -> The number of units to mint.
                % -> The address of the recipient.
                % -> The address of the source of the minted units.
                % -> The resource ID as the reason for the minted tokens.
                #{
                    <<"quantity">> => Units,
                    <<"minter">> => Minter,
                    <<"recipient">> => Recipient,
                    <<"reason">> => Resource
                }
            end,
            Accounts
        ),
        Opts
    ).

resolve_accounts(BalanceMessages, Opts) ->
    ResolveOpts =
        Opts#{
            hashpath => ignore,
            cache_control => [<<"no-cache">>, <<"no-store">>]
        },
    case hb_ao:resolve(BalanceMessages, <<"from">>, ResolveOpts) of
        {ok, Accounts} when is_map(Accounts), map_size(Accounts) > 0 ->
            Accounts;
        _ ->
            Parsed = resolve_flattened_accounts(BalanceMessages, Opts),
            case map_size(Parsed) of
                0 -> throw({accounts_resolve_failed, {error, not_found}});
                _ -> Parsed
            end
    end.

resolve_flattened_accounts(BalanceMessages, Opts) ->
    Keys = hb_ao:keys(BalanceMessages, Opts),
    RawAccounts =
        lists:foldl(
            fun(Key, Acc) ->
                case parse_balance_key(Key) of
                    {ok, Account, <<"quantity">>} ->
                        Qty = hb_util:int(hb_ao:get(Key, BalanceMessages, 0, Opts)),
                        put_account_field(Account, <<"quantity">>, Qty, Acc);
                    {ok, Account, <<"recipient">>} ->
                        Recipient = hb_ao:get(Key, BalanceMessages, Account, Opts),
                        put_account_field(Account, <<"recipient">>, Recipient, Acc);
                    ignore ->
                        Acc
                end
            end,
            #{},
            Keys
        ),
    maps:map(
        fun(Account, Details) ->
            #{
                <<"quantity">> => hb_util:int(maps:get(<<"quantity">>, Details, 0)),
                <<"recipient">> => maps:get(<<"recipient">>, Details, Account)
            }
        end,
        RawAccounts
    ).

put_account_field(Account, Field, Value, Accounts) ->
    Current = maps:get(Account, Accounts, #{}),
    Accounts#{ Account => Current#{ Field => Value } }.

parse_balance_key(Key) when is_binary(Key) ->
    case binary:split(Key, <<"/">>, [global]) of
        [Account, <<"quantity">>] when byte_size(Account) > 0 ->
            {ok, Account, <<"quantity">>};
        [Account, <<"recipient">>] when byte_size(Account) > 0 ->
            {ok, Account, <<"recipient">>};
        _ ->
            parse_concatenated_balance_key(Key)
    end.

parse_concatenated_balance_key(Key) ->
    case split_suffix(Key, <<"quantity">>) of
        {ok, Account} ->
            {ok, Account, <<"quantity">>};
        error ->
            case split_suffix(Key, <<"recipient">>) of
                {ok, Account} -> {ok, Account, <<"recipient">>};
                error -> ignore
            end
    end.

split_suffix(Key, Suffix) ->
    KeySize = byte_size(Key),
    SuffixSize = byte_size(Suffix),
    case KeySize > SuffixSize andalso
            binary:part(Key, KeySize - SuffixSize, SuffixSize) =:= Suffix of
        true ->
            {ok, binary:part(Key, 0, KeySize - SuffixSize)};
        false ->
            error
    end.

%% @doc Calculate the total number of units to mint from a list of distributions.
distributions_to_total_units(Distributions, Opts) ->
    lists:sum(
        lists:map(
            fun(D) -> hb_util:int(hb_maps:get(<<"quantity">>, D, Opts)) end,
            Distributions
        )
    ).

%% @doc Convert a list of distributions to outbox messages that can be pushed
%% to the `Client` process.
distributions_to_results(State, Distributions, Opts) ->
    {
        ok,
        #{
            <<"outbox">> =>
                [
                    #{
                        <<"target">> =>
                            hb_maps:get(<<"client">>, State, Opts),
                        <<"action">> => <<"mint-batch">>,
                        <<"content-type">> => <<"text/csv">>,
                        <<"body">> => to_csv(Distributions, Opts)
                    }
                ],
            <<"distributions">> => Distributions
        }
    }.

%% @doc Convert a list of mint messages to a single CSV string.
to_csv(Msgs, Opts) ->
    hb_util:bin(
        lists:map(
            fun(M) ->
                Recpt = hb_util:human_id(hb_maps:get(<<"recipient">>, M, Opts)),
                Quantity = hb_util:bin(hb_maps:get(<<"quantity">>, M, Opts)),
                Minter = hb_util:human_id(hb_maps:get(<<"minter">>, M, Opts)),
                Reason = hb_util:human_id(hb_maps:get(<<"reason">>, M, Opts)),
                <<
                    Recpt/binary, ",",
                    Quantity/binary, ",",
                    Minter/binary, ",",
                    Reason/binary, "\n"
                >>
            end,
            Msgs
        )
    ).
