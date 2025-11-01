%%% @doc Test utilities and vectors for the `~mint@1.0` device.
-module(dev_mint_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(AO_TO_ARMS(X), X * 1_000_000_000_000).
-define(ARMS_TO_AO(X), X div 1_000_000_000_000).
-define(DEFAULT_MAX_WEIGHT, 10_000_000).

%%% Test Utilities

%% @doc Return random ID(s) in human-readable (43-character) format.
random_id() -> hb_util:human_id(crypto:strong_rand_bytes(32)).
random_ids(N) -> [ random_id() || _ <- lists:seq(1, N) ].

%% @doc Generate a random resource with a random weight and oracle ID.
generate_resource(Max) ->
    generate_resource(random_id(), Max).
generate_resource(ID, undefined) ->
    generate_resource(ID, rand:uniform(?DEFAULT_MAX_WEIGHT));
generate_resource(ID, Weight) when is_integer(Weight) ->
    generate_resource(ID, #{ <<"weight">> => Weight });
generate_resource(ID, Res) ->
    {
        ID,
        Res#{
            <<"weight">> =>
                hb_maps:get(
                    <<"weight">>,
                    Res
                ),
            <<"oracle">> =>
                hb_maps:get(<<"oracle">>, Res, random_id())
        }
    }.

%% @doc Generate a set of resources given a specification, either in the form of
%% specific resources to employ, or a number of resources to generate, or a
%% list of resource IDs.
generate_resources(N) ->
    generate_resources(N, undefined).
generate_resources(NumResources, GlobalSpec) when is_integer(NumResources) ->
    generate_resources(random_ids(NumResources), GlobalSpec);
generate_resources(ResourceIDs, GlobalSpec) when is_list(ResourceIDs) ->
    generate_resources(
        hb_maps:from_list([ {ID, GlobalSpec} || ID <- ResourceIDs ]),
        undefined
    );
generate_resources(Resources, GlobalSpec) when is_map(Resources) ->
    hb_maps:map(
        fun(ID, ResourceSpec) ->
            element(2, generate_resource(
                ID,
                if is_map(ResourceSpec) andalso is_map(GlobalSpec) ->
                    hb_maps:merge(GlobalSpec, ResourceSpec);
                ResourceSpec =/= undefined ->
                    ResourceSpec;
                true ->
                    GlobalSpec
                end
            ))
        end,
        Resources
    ).

%% @doc For each resource, generate a set of balances for each account.
generate_all_balances(_, #{ balances := Balances }, _) ->
    Balances;
generate_all_balances(Resources, Params, Opts) ->
    Accounts =
        case maps:find(accounts, Params) of
            {ok, AccountSpecs} -> AccountSpecs;
            error -> random_ids(maps:get(account_count, Params, 10))
        end,
    hb_maps:map(
        fun(_, _) ->
            generate_balances(
                Accounts,
                maps:get(max_balance, Params, 1_000_000_000),
                Opts
            )
        end,
        Resources
    ).

%% @doc Generate a set of initial balances for a given set or number of accounts.
%% The resulting message may be used as a sub-message of the top-level balances
%% message, attached to a specific resource ID. For example,
%% `balances/resourceID/accountID`.
generate_balances(AccountIDs, Max, Opts) when is_list(AccountIDs) ->
    generate_balances(
        hb_maps:from_list([ {ID, #{}} || ID <- AccountIDs ]),
        Max,
        Opts
    );
generate_balances(Accounts, Max, Opts) when is_map(Accounts) ->
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
generate_state(Params, Opts) ->
    Resources =
        generate_resources(
            maps:get(
                resources,
                Params,
                maps:get(resource_count, Params, 2)
            )
        ),
    Balances = generate_all_balances(Resources, Params, Opts),
    #{
        <<"device">> => <<"mint@1.0">>,
        <<"cycle">> => maps:get(cycle, Params, 0),
        <<"cycle-proportion">> =>
            maps:get(cycle_proportion, Params, 1),
        <<"cycle-proportion-denominator">> =>
            maps:get(cycle_proportion_denominator, Params, 1),
        <<"mint-total">> =>
            ?AO_TO_ARMS(maps:get(total_supply, Params, 1_000_000_000)),
        <<"minted">> =>
            ?AO_TO_ARMS(maps:get(minted, Params, 0)),
        <<"client">> =>
            maps:get(client, Params, random_id()),
        <<"resources">> => Resources,
        <<"balances">> => Balances,
        <<"total-supply">> => ?AO_TO_ARMS(1_000_000_000),
        <<"period">> => 1000,
        <<"start-time">> => os:system_time(millisecond)
    }.

%%% Tests

single_resource_single_account_test() ->
    hb:init(),
    AccID = random_id(),
    ResID = random_id(),
    {ok, NewS} =
        dev_mint_math:mint(
            generate_state(
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
    ?event(debug_test, {after_mint, NewS}),
    ?assertMatch(
        #{
            <<"quantity">> := ?AO_TO_ARMS(1),
            <<"recipient">> := AccID
        },
        hb_ao:get(<<"results/distributions/1">>, NewS, #{})
    ).