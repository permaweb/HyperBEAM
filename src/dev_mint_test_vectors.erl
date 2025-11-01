%%% @doc Test utilities and vectors for the `~mint@1.0` device.
-module(dev_mint_test_vectors).
-include_lib("eunit/include/eunit.hrl").

-define(AO_TO_ARMS(X), X * 1_000_000_000_000).
-define(ARMS_TO_AO(X), X div 1_000_000_000_000).
-define(DEFAULT_MAX_WEIGHT, 10_000_000).

%%% Test Utilities

%% @doc Return a random resource ID in human-readable (43-character) format.
random_id() -> hb_util:human_id(crypto:strong_rand_bytes(32)).

%% @doc Generate a random resource with a random weight and oracle ID.
random_resource(Max) ->
    random_resource(random_id(), Max).
random_resource(ID, Weight) when is_integer(Weight) ->
    random_resource(ID, #{ <<"weight">> => Weight });
random_resource(ID, Res) ->
    {
        ID,
        Res#{
            <<"weight">> =>
                hb_maps:get(
                    <<"weight">>,
                    Res,
                    rand:uniform(?DEFAULT_MAX_WEIGHT)
                ),
            <<"oracle">> =>
                hb_maps:get(<<"oracle">>, Res, random_id())
        }
    }.

%% @doc Generate a set of random resources with random weights in given bounds.
random_resources(N) ->
    random_resources(N, ?DEFAULT_MAX_WEIGHT).
random_resources(NumResources, Max) when is_integer(NumResources) ->
    random_resources(
        [ {random_id(), #{}} || _ <- lists:seq(1, NumResources) ],
        Max
    );
random_resources(Resources, Max) when is_map(Resources) ->
    hb_maps:map(fun(ID, _) -> random_resource(ID, Max) end, Resources).

%% @doc Generate a set of initial balances for a given set or number of accounts.
initial_balances(AccountIDs, Max, Opts) when is_list(AccountIDs) ->
    initial_balances(
        hb_maps:from_list([ {ID, #{}} || ID <- AccountIDs ]),
        Max,
        Opts
    );
initial_balances(Accounts, Max, Opts) when is_map(Accounts) ->
    hb_ao:set(
        #{ <<"device">> => <<"trie@1.0">> },
        hb_maps:map(
            fun(AccountID, Account) ->
                Account#{
                    <<"quantity">> =>
                        hb_maps:get(
                            <<"quantity">>,
                            Account,
                            ?AO_TO_ARMS(rand:uniform(Max)),
                            Opts
                        ),
                    <<"recipient">> =>
                        hb_maps:get(
                            <<"recipient">>,
                            Account,
                            AccountID,
                            Opts
                        )
                }
            end,
            Accounts
        ),
        Opts
    ).

%% @doc Generate an initial state for `~mint@1.0` execution with randomized
%% parameters within given bounds. The `Params` map is used to override the
%% default generated values:
%% - `resources`: The resources to use for the initial state. Either a list of
%%   resource IDs or a map of resource IDs to resource definition messages.
%% - `balances`: The balances for each resource.
%% - `resource_count`: The number of resources to generate.
%% - `account_count`: The number of accounts to generate for each resource. This
%%   will result in `account * resources` total accounts.
%% - `max_resource_weight`: The maximum weight to generate for each resource.
%% - `max_balance`: The maximum balance to generate for each account.
%% - `client`: The client ID that should be the target of mint messages.
%% - `total_supply`: The total supply of the token.
%% - `cycle_proportion`: The proportion of the total supply to mint each cycle.
%% - `cycle_proportion_denominator`: The denominator of the cycle proportion.
%% By default, the returned state will mint 100% of its total supply of 1 token
%% in a single cycle.
initial_state(Params, Opts) ->
    Resources =
        case maps:get(resources, Params, undefined) of
            undefined ->
                random_resources(maps:get(resource_count, Params, 2));
            Res -> Res
        end,
    Balances = maps:get(balances, Params, #{}),
    #{
        <<"device">> => <<"mint@1.0">>,
        <<"cycle-proportion">> =>
            maps:get(cycle_proportion, Params, 1),
        <<"cycle-proportion-denominator">> =>
            maps:get(cycle_proportion_denominator, Params, 1),
        <<"total-supply">> =>
            ?AO_TO_ARMS(maps:get(total_supply, Params, 1_000_000_000)),
        <<"client">> =>
            maps:get(client, Params, random_id()),
        <<"resources">> => Resources,
        <<"balances">> =>
            hb_maps:map(
                fun(ResID, _) ->
                    case hb_maps:find(ResID, Balances, Opts) of
                        {ok, ResourceBalances} ->
                            ResourceBalances;
                        error ->
                            initial_balances(
                                maps:get(
                                    accounts,
                                    Params,
                                    maps:get(account_count, Params, 10)
                                ),
                                maps:get(max_balance, Params, 1_000_000_000),
                                Opts
                            )
                    end
                end,
                Resources
            ),
        <<"total-supply">> => ?AO_TO_ARMS(1_000_000_000),
        <<"period">> => 1000,
        <<"start-time">> => os:system_time(millisecond)
    }.

%%% Tests

single_resource_single_account_test() ->
    AccID = random_id(),
    ResID = random_id(),
    {ok, NewS} =
        dev_mint_math:mint(
            initial_state(
                #{
                    resources => [ResID],
                    accounts => [AccID],
                    total_supply => 1
                },
                #{}
            ),
            #{},
            #{}
        ),
    ?assertMatch(
        #{
            <<"quantity">> := ?AO_TO_ARMS(1),
            <<"recipient">> := AccID
        },
        hb_ao:get(<<"results/distributions/", AccID/binary>>, NewS, #{})
    ).