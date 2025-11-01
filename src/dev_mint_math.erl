%%% @doc The core mathematical functions of the `~mint@1.0` device.
%%% 
%%% The mint device makes use of Erlang's built-in bignum support to perform
%%% all mathematical operations without loss of precision or conversion.
-module(dev_mint_math).
-export([should_mint/3, mint/3]).
-include("include/hb.hrl").
-define(UNITS_TO_TOKENS(X), X / 1_000_000_000_000).
-define(TOKENS_TO_UNITS(X), X * 1_000_000_000_000).

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
    ?event(debug, {starting_mint_cycle, {cycle, Cycle}, {state, State}}),
    TotalToDistribute = units_to_distribute(State, Opts),
    ?event(debug, {tokens_to_distribute, ?UNITS_TO_TOKENS(TotalToDistribute)}),
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
                % Calculate the number of units from the resource to distribute
                % to each address.
                {ok, Dists} = units_per_address(Resource, Units, Addresses, Opts),
                Dists
            end,
            UnitsPerResource
        ),
    ?event(debug, {resource_distributions, ResourceDistributions}),
    Distributions = lists:flatten(ResourceDistributions),
    ?event(debug, {combined_distributions, Distributions}),
    {ok, Outbox, Allocated} = distributions_to_outbox(State, Distributions, Opts),
    DustCarriedForward = TotalToDistribute - Allocated,
    NewMintedSupply = AlreadyMinted + Allocated,
    ?event(mint_short,
        {mint_cycle_complete,
            {cycles, Cycle},
            {allocated_units, ?UNITS_TO_TOKENS(Allocated)},
            {dust_carried_forward, ?UNITS_TO_TOKENS(DustCarriedForward)},
            {new_minted_supply, ?UNITS_TO_TOKENS(NewMintedSupply)},
            {distributions, length(Distributions)}
        }
    ),
    NewState =
        hb_ao:set(
            State,
            #{
                <<"cycle">> => Cycle + 1,
                <<"minted">> => NewMintedSupply,
                <<"outbox">> => Outbox
            },
            Opts
        ),
    {ok, NewState}.

%% @doc Return the total number of units to distribute.
units_to_distribute(State, Opts) ->
    MintTotal = hb_util:int(hb_maps:get(<<"mint-total">>, State, Opts)),
    AlreadyMinted = hb_util:int(hb_maps:get(<<"minted">>, State, Opts)),
    CycleProportionNumerator =
        hb_util:int(hb_maps:get(<<"cycle-proportion">>, State, Opts)),
    CycleProportionDenominator =
        hb_util:int(hb_maps:get(<<"cycle-proportion-denominator">>, State, Opts)),
    CycleProportion = CycleProportionNumerator div CycleProportionDenominator,
    Remaining = MintTotal - AlreadyMinted,
    floor(Remaining * CycleProportion).

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
    hb_maps:map(
        fun(_Resource, Weight) ->
            floor((TotalToDistribute * Weight) div TotalWeights)
        end,
        ResourceWeights
    ).

%% @doc Return the number of units to distribute for each address in a resource.
units_per_address(Resource, UnitsForResource, BalanceMessages, Opts) ->
    ?event(debug, {units_for_resource, UnitsForResource}),
    Accounts = hb_ao:keys(hb_message:uncommitted(BalanceMessages), Opts),
    ?event(debug, {addresses, Accounts}),
    TotalQuantity =
        lists:sum(
            hb_maps:values(
                hb_maps:map(
                    fun(_Address, AddressDetails) ->
                        hb_util:int(
                            hb_maps:get(<<"quantity">>, AddressDetails, 0, Opts)
                        )
                    end,
                    Addresses
                )
            )
        ),
    hb_maps:values(
        hb_maps:map(
            fun(Minter, AddressDetails) ->
                % Gather details for the address.
                Quantity =
                    hb_util:int(
                        hb_maps:get(<<"quantity">>, AddressDetails, 0, Opts)
                    ),
                Recipient =
                    hb_maps:get(
                        <<"recipient">>,
                        AddressDetails,
                        Minter,
                        Opts
                    ),
                % Calculate units to mint for the address.
                Proportion = Quantity div TotalQuantity,
                Units = floor(UnitsForResource * Proportion),
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
            Addresses
        ),
        Opts
    ).

%% @doc Convert a list of distributions to outbox messages that can be pushed
%% to the `Client` process.
distributions_to_outbox(State, Distributions, Opts) ->
    {
        ok,
        hb_ao:set(
            State,
            #{
                <<"results">> =>
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
            },
            Opts
        )
    }.

%% @doc Convert a list of mint messages to a single CSV string.
to_csv(Msgs, Opts) ->
    hb_util:bin(
        lists:map(
            fun(M) ->
                Recpt = hb_util:human_id(hb_maps:get(<<"recipient">>, M, Opts)),
                Quantity = hb_maps:get(<<"quantity">>, M, Opts),
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