%%% @doc Test vectors and benchmarks for configurations of `~token@1.0',
%%% using the `~pot@1.0' mint device, as a `~process@1.0' message.
-module(dev_token_pot_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% Test Helpers: State Accessors.
%%% ----------------------------------------------------------------------------

%% @doc Get balance for an account.
balance(Process, Wallet, Opts) when is_tuple(Wallet) ->
    balance(Process, id(Wallet), Opts);
balance(Process, Account, Opts) when is_binary(Account) ->
    balance(Process, #{ <<"balance">> => Account }, Opts);
balance(Process, Req, Opts) ->
    CurrentSlot = hb_ao:get(<<"slot/current">>, Process, Opts),
    ?event(debug_test, {current_slot, CurrentSlot}, Opts),
    Res =
        hb_ao:resolve_many(
            [
                Process,
                #{ <<"path">> => <<"now">> },
                #{
                    <<"path">> => <<"as">>,
                    <<"as">> => <<"execution">>
                },
                Req#{
                    <<"path">> => <<"balance">>,
                    <<"slot">> => hb_cache:ensure_loaded(CurrentSlot, Opts)
                }
            ],
            Opts
        ),
    case Res of
        {ok, B} -> B;
        {error, not_found} -> 0
    end.

%% @doc Return the deposit quantity for the given resource and address.
deposit(Process, Resource, Address, Opts) ->
    deposit(Process, Resource, Address, <<>>, Opts).
deposit(Process, Resource, Address, SubPath, Opts) ->
    hb_ao:get(
        <<
            "now/resources/",
            Resource/binary,
            "/deposits/",
            Address/binary,
            "/",
            SubPath/binary
        >>,
        Process,
        Opts
    ).

%% @doc Return the delegated quantity for the given edge.
delegation(Process, Resource, FromAddr, ToAddr, Opts) ->
    hb_ao:get(
        <<
            "now/resources/",
            Resource/binary,
            "/deposits/",
            FromAddr/binary,
            "/delegations/",
            ToAddr/binary
        >>,
        Process,
        0,
        Opts
    ).

%% @doc Return the weight for a given resource on a `~pot@1.0' process.
weight(RawProcess, Resource, Opts) ->
    Process = dev_token_lib:now(RawProcess, Opts),
    hb_ao:get(
        <<"now/resources/", Resource/binary, "/weight">>,
        Process,
        Opts
    ).

%%% Test Helpers: Generators.
%%% ----------------------------------------------------------------------------


%% @doc Generate generic isolated node messages for testing.
test_opts() ->
    hb:init(),
    #{
        priv_wallet => ar_wallet:new(),
        store => [hb_test_utils:test_store()]
    }.

%% @doc Generate a random ID, or an 'ID' value of the correct length starting
%% with the given binary and padded with zeros.
id(AlreadyID) when is_binary(AlreadyID) -> AlreadyID;
id(Bin) when is_binary(Bin) ->
    BitSize = byte_size(Bin) * 8,
    Suffix = << 0:(256 - BitSize) >>,
    << Bin/binary, Suffix/binary >>;
id(Other) -> hb_util:human_id(Other).

set_weight_req(Resource, Weight) ->
    #{
        <<"action">> => <<"register">>,
        <<"resource">> => Resource,
        <<"weight">> => Weight
    }.
set_weight_req(Resource, Weight, ResourceAuthority, WeightAuthority) ->
    #{
        <<"action">> => <<"register">>,
        <<"resource">> => Resource,
        <<"weight">> => Weight,
        <<"resource-authority">> => ResourceAuthority,
        <<"weight-authority">> => WeightAuthority
    }.

deposit_req(Resource, Addr, Qty) ->
    #{
        <<"action">> => <<"deposit">>,
        <<"resource">> => Resource,
        <<"address">> => Addr,
        <<"quantity">> => Qty
    }.

delegate_req(Resource, Addr, Qty) ->
    #{
        <<"action">> => <<"delegate">>,
        <<"resource">> => Resource,
        <<"address">> => Addr,
        <<"quantity">> => Qty
    }.

undelegate_req(Resource, Addr, Qty) ->
    #{
        <<"action">> => <<"undelegate">>,
        <<"resource">> => Resource,
        <<"address">> => Addr,
        <<"quantity">> => Qty
    }.

withdraw_req(Resource, Addr, Qty) ->
    #{
        <<"action">> => <<"withdraw">>,
        <<"resource">> => Resource,
        <<"address">> => Addr,
        <<"quantity">> => Qty
    }.

transfer_req(Addr, Qty) ->
    transfer_req(Addr, Qty, #{}).
transfer_req(Addr, Qty, Params) ->
    Params#{
        <<"action">> => <<"transfer">>,
        <<"recipient">> => Addr,
        <<"quantity">> => Qty
    }.

%% @doc Generate a base token state with default configuration.
generate_token_state(Params, Opts) ->
    ?event({generate_token_base_state, {params, Params}}),
    DefaultBalances = #{ <<"device">> => <<"trie@1.0">> },
    InitialBalances = maps:get(initial_balances, Params, #{}),
    Total =
        case maps:find(total_supply, Params) of
            {ok, TotalSupply} -> TotalSupply;
            error -> lists:sum(hb_maps:values(InitialBalances, Opts))
        end,
    ?event({initial_supply, {total, Total}, {balances, InitialBalances}}),
    {ok, Balances} =
        hb_ao:resolve(
            DefaultBalances,
            InitialBalances#{ <<"path">> => <<"set">> },
            Opts
        ),
    DefaultState = #{
        <<"device">> => <<"token@1.0">>,
        <<"set-authority">> => id(hb_opts:get(priv_wallet, no_wallet, Opts)),
        <<"t-source">> => <<"slot">>,
        <<"name">> => <<"Test Token">>,
        <<"ticker">> => <<"TEST">>,
        <<"denomination">> => 12,
        <<"total-supply">> => Total,
        <<"balances">> => Balances
    },
    FinalState = maps:merge(DefaultState, maps:get(extra, Params, #{})),
    ?event({final_state_generated, {balances_size, map_size(Balances)}}),
    FinalState.

%% @doc Helper to generate a generic process state with the given extra keys
%% added. The result is returned committed with the base wallet of the given
%% `Opts'.
generate_base_process_state(ExtraKeys, Opts) ->
    Addr = id(hb_opts:get(priv_wallet, no_wallet, Opts)),
    ProcessBase =
        ExtraKeys#{
            <<"device">> => <<"process@1.0">>,
            <<"type">> => <<"Process">>,
            <<"execution-device">> => <<"token@1.0">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"push-device">> => <<"push@1.0">>,
            <<"scheduler">> => Addr,
            <<"authority">> => Addr
        },
    Proc = hb_message:commit(ProcessBase, Opts),
    hb_cache:write(Proc, Opts),
    Proc.

%% @doc Generate pot state for integration testing
generate_pot_state(Params, Opts) ->
    Authority = id(hb_opts:get(priv_wallet, no_wallet, Opts)),
    MintCap = hb_maps:get(mint_cap, Params, 10000, Opts),
    MintPropN = hb_maps:get(mint_prop_numerator, Params, 1, Opts),
    MintPropD = hb_maps:get(mint_prop_denominator, Params, 2, Opts),
    T = hb_maps:get(t, Params, 0, Opts),
    LastDrip = hb_maps:get(last_drip, Params, 0, Opts),
    ?event({generate_pot_fields, {params, Params}}),
    MaybeParent =
        case hb_maps:get(parent, Params, not_found, Opts) of
            not_found -> #{};
            Parent -> #{ <<"parent">> => Parent }
        end,
    MaybeIndexKeys =
        case hb_maps:get(partner_device, Params, not_found, Opts) of
            not_found -> #{};
            Default ->
                #{
                    <<"provider-mint-device">> => Default,
                    <<"update-every">> =>
                        hb_maps:get(update_every, Params, 1, Opts),
                    <<"indexed-mints">> =>
                        hb_maps:get(indexed_mints, Params, [], Opts)
                }
        end,
    Merged = maps:merge(MaybeParent, MaybeIndexKeys),
    Merged#{
        <<"mint-device">> => hb_maps:get(mint_device, Params, <<"pot@1.0">>, Opts),
        <<"mint-authority">> => hb_maps:get(mint_authority, Params, Authority, Opts),
        <<"mint-cap">> => MintCap,
        <<"mint-prop-numerator">> => MintPropN,
        <<"mint-prop-denominator">> => MintPropD,
        <<"total-weighted-units">> => 0,
        <<"resources">> => #{},
        <<"t">> => T,
        <<"last-drip">> => LastDrip
    }.

%% @doc Return a signed token process with a `pot@1.0` mint device.
generate_process(PotFields, Opts) ->
    generate_process(PotFields, #{}, Opts).
generate_process(PotFields, TokenFields, Opts) ->
    PotBase = generate_pot_state(PotFields, Opts),
    TokenBase = 
        generate_token_state(
            TokenFields#{
                extra => PotBase
            },
            Opts
        ),
    generate_base_process_state(TokenBase, Opts).

%% @doc Create a request message.
schedule_request(Process, Body, Opts) ->
    schedule_request(
        Process,
        Body,
        hb_opts:get(priv_wallet, no_wallet, Opts),
        Opts
    ).
schedule_request(Process, Body, Wallet, Opts) ->
    ?event(debug_test, {scheduling_request, {body, Body}}, Opts),
    Signed =
        hb_message:commit(
            Body,
            Opts#{ priv_wallet => Wallet }
        ),
    ?event(debug_test, {signed_request, Signed}, Opts),
    Req =
        Signed#{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>
        },
    ?event(schedule_request,
        {scheduling_request, 
            {process, Process}, 
            {req, Req}
        },
        Opts
    ),
    {ok, Res} = hb_ao:resolve(Process, Req, Opts),
    ?event(debug_test, {schedule_result, Res}, Opts),
    Res.

push_request(Process, Body, Opts) ->
    push_request(
        Process,
        Body,
        hb_opts:get(priv_wallet, no_wallet, Opts),
        Opts
    ).
push_request(Process, Body, Wallet, Opts) ->
    PushReq =
        hb_message:commit(#{
            <<"path">> => <<"push">>,
            <<"body">> =>
                hb_message:commit(
                    Body,
                    Opts#{ priv_wallet => Wallet }
                )
            },
            Opts#{ priv_wallet => Wallet }
        ),
    hb_ao:resolve(
        Process,
        PushReq,
        Opts
    ).

push_set_weight(Process, Resource, Weight, Opts) ->
    push_request(
        Process,
        set_weight_req(
            Resource,
            Weight,
            id(hb_opts:get(priv_wallet, no_wallet, Opts)),
            id(hb_opts:get(priv_wallet, no_wallet, Opts))
        ),
        Opts
    ),
    dev_token_lib:now(Process, Opts).

%% @doc Helper to create a pot resource with deposits
push_modify_resource(Process, Resource, UserDeposits, Opts) ->
    hb_maps:map(
        fun(UserWallet, Qty) ->
            push_request(
                Process,
                deposit_req(Resource, id(UserWallet), Qty),
                Opts
            )
        end,
        UserDeposits
    ),
    dev_token_lib:now(Process, Opts).

push_deposit(Process, Resource, User, Qty, Opts) ->
    push_modify_resource(Process, Resource, #{ id(User) => Qty }, Opts).

push_delegate(Process, Resource, User, ToAddr, Qty, Opts) ->
    push_request(
        Process,
        delegate_req(Resource, ToAddr, Qty),
        User,
        Opts
    ).

push_undelegate(Process, Wallet, FromAddr, Resource, Qty, Opts) ->
    push_request(
        Process,
        undelegate_req(Resource, FromAddr, Qty),
        Wallet,
        Opts
    ).

push_withdraw(Process, Resource, Addr, Qty, Opts) ->
    push_request(
        Process,
        withdraw_req(Resource, Addr, Qty),
        Opts
    ).

%%% Test Cases.
%%% ----------------------------------------------------------------------------

simple_process_test() ->
    hb:init(),
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddr = id(BobWallet),
    Base =
        generate_base_process_state(
            generate_token_state(
                #{
                    initial_balances => #{ AliceAddr => 1_000_000_000 }
                },
                Opts
            ),
            Opts
        ),
    ?event({base_state, Base}),
    push_request(
        Base,
        transfer_req(BobAddr, 1),
        AliceWallet,
        Opts
    ),
    State = dev_token_lib:now(Base, Opts),
    ?assertEqual(999_999_999, balance(State, AliceAddr, Opts)),
    ?assertEqual(1, balance(State, BobAddr, Opts)),
    ?assertEqual(1_000_000_000, hb_ao:get(<<"total-supply">>, State, Opts)).

%% @doc Basic test to see what happens when transfer is called with mint-device=pot
simple_pot_process_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    ResourceOxygen = <<"oxygen">>,
    PotFields = #{
        mint_cap => 10000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2
    },
    TokenFields = #{
        initial_balances => #{ id(Alice) => 1000 },
        total_supply => 1000
    },
    Process = generate_process(PotFields, TokenFields, Opts),
    ?event({process, Process}),
    push_set_weight(Process, ResourceOxygen, 100, Opts),
    push_deposit(
        Process,
        ResourceOxygen,
        Alice,
        10,
        Opts
    ),
    push_request(
        Process,
        #{ <<"action">> => <<"mint">> },
        Alice,
        Opts
    ),
    push_request(
        Process,
        transfer_req(id(Bob), 1),
        Alice,
        Opts
    ),
    ?event(debug_test, {state, Process}, Opts),
    ?assertEqual(1, balance(Process, id(Bob),Opts)),
    ?assertEqual(8999, balance(Process, id(Alice), Opts)),
    ?assertEqual(9000, hb_ao:get(<<"now/total-supply">>, Process, Opts)).

weight_authority_can_update_weight_without_resource_config_authority_test() ->
    Opts = test_opts(),
    Resource = <<"oxygen">>,
    WeightWallet = ar_wallet:new(),
    ResourceWallet = ar_wallet:new(),
    Process =
        generate_process(
            #{
                mint_cap => 10000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2
            },
            Opts
        ),
    push_request(
        Process,
        set_weight_req(Resource, 100, id(ResourceWallet), id(WeightWallet)),
        Opts
    ),
    ?assertEqual(100, weight(Process, Resource, Opts)),
    ?assertMatch(
        {error, _},
        push_request(
            Process,
            set_weight_req(Resource, 200),
            ResourceWallet,
            Opts
        )
    ),
    ?assertEqual(100, weight(Process, Resource, Opts)),
    push_request(
        Process,
        set_weight_req(Resource, 200),
        WeightWallet,
        Opts
    ),
    ?assertEqual(200, weight(Process, Resource, Opts)).

pot_delegation_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Resource = <<"oxygen">>,
    Process =
        generate_process(
            #{
                mint_cap => 10000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2
            },
            Opts
        ),
    push_set_weight(Process, Resource, 100, Opts),
    push_deposit(Process, Resource, Alice, 100, Opts),
    push_delegate(Process, Resource, Alice, id(Bob), 10, Opts),
    ?assertEqual(
        10,
        deposit(Process, Resource, id(Bob), <<"quantity">>, Opts)
    ),
    ?assertEqual(
        90,
        deposit(Process, Resource, id(Alice), <<"quantity">>, Opts)
    ),
    ?assertEqual(100,
        hb_ao:get(
            <<"now/resources/", Resource/binary, "/total-deposits">>,
            Process,
            Opts
        )
    ).

cyclic_undelegate_liquidation_fails_cleanly_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Charlie = ar_wallet:new(),
    AliceAddr = id(Alice),
    BobAddr = id(Bob),
    CharlieAddr = id(Charlie),
    Resource = <<"oxygen">>,
    Process =
        generate_process(
            #{
                mint_cap => 10_000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2
            },
            Opts
        ),
    push_set_weight(Process, Resource, 100, Opts),
    push_deposit(Process, Resource, Alice, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    push_delegate(Process, Resource, Bob, CharlieAddr, 10, Opts),
    push_delegate(Process, Resource, Charlie, AliceAddr, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    ?assertEqual(0, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(20, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, CharlieAddr, AliceAddr, Opts)),
    ?assertMatch(
        {error, _},
        push_undelegate(Process, Alice, BobAddr, Resource, 20, Opts)
    ),
    ?assertEqual(0, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(20, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, CharlieAddr, AliceAddr, Opts)).

withdraw_through_cyclic_liquidation_fails_cleanly_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Charlie = ar_wallet:new(),
    AliceAddr = id(Alice),
    BobAddr = id(Bob),
    CharlieAddr = id(Charlie),
    Resource = <<"oxygen">>,
    Process =
        generate_process(
            #{
                mint_cap => 10_000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2
            },
            Opts
        ),
    push_set_weight(Process, Resource, 100, Opts),
    push_deposit(Process, Resource, Alice, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    push_delegate(Process, Resource, Bob, CharlieAddr, 10, Opts),
    push_delegate(Process, Resource, Charlie, AliceAddr, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    ?assertEqual(0, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(20, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, CharlieAddr, AliceAddr, Opts)),
    ?assertMatch(
        {error, _},
        push_withdraw(Process, Resource, AliceAddr, 20, Opts)
    ),
    ?assertEqual(0, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(20, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, CharlieAddr, AliceAddr, Opts)).

withdraw_through_cyclic_liquidation_can_recover_principal_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Charlie = ar_wallet:new(),
    AliceAddr = id(Alice),
    BobAddr = id(Bob),
    CharlieAddr = id(Charlie),
    Resource = <<"oxygen">>,
    Process =
        generate_process(
            #{
                mint_cap => 10_000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2
            },
            Opts
        ),
    push_set_weight(Process, Resource, 100, Opts),
    push_deposit(Process, Resource, Alice, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    push_delegate(Process, Resource, Bob, CharlieAddr, 10, Opts),
    push_delegate(Process, Resource, Charlie, AliceAddr, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    push_delegate(Process, Resource, Bob, CharlieAddr, 10, Opts),
    ?assertEqual(0, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(20, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(20, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, CharlieAddr, AliceAddr, Opts)),
    ?assertMatch(
        {error, _},
        push_withdraw(Process, Resource, AliceAddr, 20, Opts)
    ),
    ?assertEqual(0, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(20, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(20, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, CharlieAddr, AliceAddr, Opts)),
    {ok, _} = push_withdraw(Process, Resource, AliceAddr, 10, Opts),
    ?assertEqual(0, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, CharlieAddr, AliceAddr, Opts)).

cyclic_undelegation_can_be_unwound_in_safe_steps_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Charlie = ar_wallet:new(),
    AliceAddr = id(Alice),
    BobAddr = id(Bob),
    CharlieAddr = id(Charlie),
    Resource = <<"oxygen">>,
    Process =
        generate_process(
            #{
                mint_cap => 10_000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2
            },
            Opts
        ),
    push_set_weight(Process, Resource, 100, Opts),
    push_deposit(Process, Resource, Alice, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    push_delegate(Process, Resource, Bob, CharlieAddr, 10, Opts),
    push_delegate(Process, Resource, Charlie, AliceAddr, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    ?assertMatch(
        {error, _},
        push_undelegate(Process, Alice, BobAddr, Resource, 20, Opts)
    ),
    {ok, _} = push_undelegate(Process, Alice, BobAddr, Resource, 10, Opts),
    {ok, _} = push_undelegate(Process, Charlie, AliceAddr, Resource, 10, Opts),
    {ok, _} = push_undelegate(Process, Alice, BobAddr, Resource, 10, Opts),
    ?assertEqual(10, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(0, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    ?assertEqual(0, delegation(Process, Resource, CharlieAddr, AliceAddr, Opts)).

cyclic_undelegation_liquidation_still_succeeds_when_edge_is_not_reentered_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Charlie = ar_wallet:new(),
    AliceAddr = id(Alice),
    BobAddr = id(Bob),
    CharlieAddr = id(Charlie),
    Resource = <<"oxygen">>,
    Process =
        generate_process(
            #{
                mint_cap => 10_000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2
            },
            Opts
        ),
    push_set_weight(Process, Resource, 100, Opts),
    push_deposit(Process, Resource, Alice, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    push_delegate(Process, Resource, Bob, CharlieAddr, 10, Opts),
    push_delegate(Process, Resource, Charlie, AliceAddr, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    {ok, _} = push_undelegate(Process, Alice, BobAddr, Resource, 10, Opts),
    {ok, _} = push_undelegate(Process, Charlie, AliceAddr, Resource, 10, Opts),
    ?assertEqual(0, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    ?assertEqual(0, delegation(Process, Resource, CharlieAddr, AliceAddr, Opts)),
    {ok, _} = push_undelegate(Process, Alice, BobAddr, Resource, 10, Opts),
    ?assertEqual(10, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(0, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    ?assertEqual(0, delegation(Process, Resource, CharlieAddr, AliceAddr, Opts)).

cyclic_undelegation_from_charlie_to_alice_succeeds_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Charlie = ar_wallet:new(),
    AliceAddr = id(Alice),
    BobAddr = id(Bob),
    CharlieAddr = id(Charlie),
    Resource = <<"oxygen">>,
    Process =
        generate_process(
            #{
                mint_cap => 10_000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2
            },
            Opts
        ),
    push_set_weight(Process, Resource, 100, Opts),
    push_deposit(Process, Resource, Alice, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    push_delegate(Process, Resource, Bob, CharlieAddr, 10, Opts),
    push_delegate(Process, Resource, Charlie, AliceAddr, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    ?assertEqual(0, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(20, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, CharlieAddr, AliceAddr, Opts)),
    {ok, _} = push_undelegate(Process, Charlie, AliceAddr, Resource, 10, Opts),
    ?assertEqual(0, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    ?assertEqual(0, delegation(Process, Resource, CharlieAddr, AliceAddr, Opts)).

plain_chain_undelegation_liquidates_successfully_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Charlie = ar_wallet:new(),
    AliceAddr = id(Alice),
    BobAddr = id(Bob),
    CharlieAddr = id(Charlie),
    Resource = <<"oxygen">>,
    Process =
        generate_process(
            #{
                mint_cap => 10_000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2
            },
            Opts
        ),
    push_set_weight(Process, Resource, 100, Opts),
    push_deposit(Process, Resource, Alice, 10, Opts),
    push_delegate(Process, Resource, Alice, BobAddr, 10, Opts),
    push_delegate(Process, Resource, Bob, CharlieAddr, 10, Opts),
    ?assertEqual(0, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(10, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(10, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)),
    {ok, _} = push_undelegate(Process, Alice, BobAddr, Resource, 10, Opts),
    ?assertEqual(10, deposit(Process, Resource, AliceAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, BobAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, deposit(Process, Resource, CharlieAddr, <<"quantity">>, Opts)),
    ?assertEqual(0, delegation(Process, Resource, AliceAddr, BobAddr, Opts)),
    ?assertEqual(0, delegation(Process, Resource, BobAddr, CharlieAddr, Opts)).

balance_without_explicit_mint_same_slot_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    ResourceOxygen = <<"oxygen">>,
    PotFields = #{
        mint_cap => 10000
    },
    Process = generate_process(PotFields, Opts),
    push_set_weight(Process, ResourceOxygen, 100, Opts),
    push_deposit(Process, ResourceOxygen, Alice, 10, Opts),
    ?event(debug_test, 
        {processes, 
            {balance, balance(Process, id(Alice), Opts)}, 
            {post_deposit, dev_token_lib:now(Process, Opts)}
        },
        Opts
    ),
    % Real prod path uses outer assignment slot time. The deposit request drips
    % the pot at the current slot before the new quantity is added, so the new
    % deposit does not realize yield until a later scheduled assignment.
    ?assertEqual(0, balance(Process, Alice, Opts)).

%% @doc Test that transfer works when balance is insufficient but 
%% balance + unclaimed_yield is sufficient
%% This validates that normalize_mint properly claims yields before transfer
transfer_with_unclaimed_yield_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob  = ar_wallet:new(),
    ResourceOxygen = <<"oxygen">>,
    % Alice has 500 tokens in balance
    % Alice has deposits in pot that will yield tokens
    % Alice wants to transfer 700 tokens
    % Should succeed because: balance + yield > 700
    PotFields = #{
        mint_cap => 10000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0
    },
    TokenFields = #{
        initial_balances => #{ id(Alice) => 500 },
        total_supply => 500
    },
    Process = generate_process(PotFields, TokenFields, Opts),
    push_set_weight(Process, ResourceOxygen, 100, Opts),
    push_deposit(
        Process,
        ResourceOxygen,
        Alice,
        10,
        Opts
    ),
    % Advance time to generate yield
    % With mint_cap=10000, mint_prop={1,2}, going from t=0 to t=1:
    % ToMint = 10000 * (2^1 - 1^1) / 2^1 = 10000 * 1 / 2 = 5000
    % GlobalAcc = 0 + (5000 / 1000) = 5 (per weighted unit)
    % ResourceAcc = 0 + (5 * 100) = 500
    % Alice's yield = (500 - 0) * 10 = 5000 tokens!
    %?assertEqual(500, dev_token_lib:balance(Process, id(Alice), Opts)),
    % % Try to transfer 700 tokens
    % % Should fail without normalize_mint (500 < 700)
    % % Should succeed with normalize_mint (500 + 5000 = 5500 > 700)
    push_request(
        Process,
        transfer_req(id(Bob), 700),
        Alice,
        Opts
    ),
    % Alice should have: (500 + 5000) - 700 = 4800
    % Bob should have: 700
    ?assertEqual(6800, balance(Process, id(Alice), Opts)),
    ?assertEqual(700, balance(Process, id(Bob), Opts)),
    % Total supply should be updated
    % Initial: 500, Minted: 5000, New total: 5500
    ?assertEqual(7500, hb_ao:get(<<"now/total-supply">>, Process, Opts)).

normalized_balance_normalizes_lazy_mint_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    AliceAddr = id(Alice),
    ResourceOxygen = <<"oxygen">>,
    PotFields = #{
        mint_cap => 10000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0
    },
    TokenFields = #{
        initial_balances => #{AliceAddr => 500},
        total_supply => 500
    },
    Process = generate_process(PotFields, TokenFields, Opts),
    push_set_weight(Process, ResourceOxygen, 100, Opts),
    push_deposit(
        Process,
        ResourceOxygen,
        Alice,
        10,
        Opts
    ),
    push_request(
        Process,
        #{
            <<"action">> => <<"mint">>
        },
        Bob,
        Opts
    ),
    % Raw `now/balances` remains stale because explicit `mint` advances global
    % pot state, but does not claim Alice's user-specific lazy yield.
    ?assertEqual(500, dev_token_lib:balance(Process, AliceAddr, Opts)),
    % After the global mint, Alice has 7000 lazy claimable yield on top of her
    % explicit 500 token balance.
    ?assertEqual(7500, dev_token_lib:normalized_balance(Process, AliceAddr, Opts)),
    ?assertEqual(7500, balance(Process, AliceAddr, Opts)).

%% @doc Test that public persisted mint cannot claim yield for another account.
public_mint_rejects_foreign_subject_test() ->
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    BobWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    ResourceOxygen = <<"oxygen">>,
    PotFields = #{
        mint_cap => 10000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0
    },
    TokenFields = #{
        initial_balances => #{AliceAddr => 1000},
        total_supply => 1000
    },
    Process = generate_process(PotFields, TokenFields, Opts),
    push_set_weight(Process, ResourceOxygen, 100, Opts),
    push_deposit(
        Process,
        ResourceOxygen,
        AliceWallet,
        10,
        Opts
    ),
    _ = push_request(
        Process,
        #{
            <<"action">> => <<"mint">>,
            <<"subject">> => AliceAddr
        },
        BobWallet,
        Opts
    ),
    ?assertEqual(1000, dev_token_lib:balance(Process, AliceAddr, Opts)),
    ?assertEqual(1000, hb_ao:get(<<"now/total-supply">>, Process, Opts)).

%% @doc Test direct claim_yield functionality from a single resource
claim_yield_single_resource_test() ->
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    ResourceOxygen = <<"oxygen">>,
    PotFields = #{
        mint_cap => 10000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0
    },
    TokenFields = #{
        initial_balances => #{AliceAddr => 1000},
        total_supply => 1000
    },
    Process = generate_process(PotFields, TokenFields, Opts),
    push_set_weight(Process, ResourceOxygen, 100, Opts),
    push_deposit(
        Process,
        ResourceOxygen,
        AliceWallet,
        10,
        Opts
    ),
    push_request(
        Process,
        #{
            <<"action">> => <<"mint">>
        },
        AliceWallet,
        Opts
    ),
    BaseAfterClaim = dev_token_lib:now(Process, Opts),
    ?event({after_claim, BaseAfterClaim}),
    ?assertEqual(8000, balance(BaseAfterClaim, AliceAddr,Opts)).

%% @doc Test claim_yield across multiple resources
claim_yield_multiple_resources_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    ResourceOxygen = <<"oxygen">>,
    ResourceHydrogen = <<"hydrogen">>,
    % Alice has deposits in two different resources
    PotFields = #{
        mint_cap => 10_000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2
    },
    Process = generate_process(PotFields, Opts),
    State = dev_token_lib:now(Process, Opts),
    push_set_weight(State, ResourceOxygen, 100, Opts),
    push_set_weight(State, ResourceHydrogen, 50, Opts),
    push_deposit(State, ResourceOxygen, Alice, 10, Opts),
    push_deposit(State, ResourceHydrogen, Alice, 5, Opts),
    push_request(
        State,
        #{
            <<"action">> => <<"mint">>
        },
        Alice,
        Opts
    ),
    State2 = dev_token_lib:now(State, Opts),
    % Under real scheduled slot timing, each setup action consumes a slot.
    % By the explicit mint, oxygen has already realized one more accumulator
    % step than in the old lazy body.t model, so Alice ends at 9250.
    ?assertEqual(9250, balance(State2, id(Alice), Opts)).

%% @doc Test claim_yield when address has no deposits (edge case)
claim_yield_no_deposits_test() ->
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    PotFields = #{
        mint_cap => 10000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0
    },
    TokenFields = #{
        initial_balances => #{AliceAddr => 100},
        total_supply => 100
    },
    Base = generate_process(PotFields, TokenFields, Opts),
    push_request(
        Base,
        #{
            <<"action">> => <<"mint">>
        },
        AliceWallet,
        Opts
    ),
    BaseAfterClaim = dev_token_lib:now(Base, Opts),
    % Alice's balance should be unchanged (still 100)
    ?assertEqual(100, balance(BaseAfterClaim, AliceAddr,Opts)),
    ?assertEqual(100, hb_ao:get(<<"total-supply">>, BaseAfterClaim, Opts)).

pot_subscriptions_test() ->
    Opts = test_opts(),
    Resource = <<"oxygen">>,
    % Generate a parent mint process and a child mint process.
    ParentProcess =
        generate_process(
            #{
                mint_cap => 10_000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2
            },
            Opts
        ),
    ParentID = dev_process_lib:process_id(ParentProcess, Opts),
    % Generate a child mint with the parent ID.
    ChildProcess =
        generate_process(
            #{
                mint_cap => 10_000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2,
                parent => ParentID
            },
            Opts
        ),
    ChildID = dev_process_lib:process_id(ChildProcess, Opts),
    ?event(
        debug_test,
        {test_processes,
            {parent, ParentID},
            {child, ChildID}
        },
        Opts
    ),
    % Push an action on the child mint to initialize it, subsribing to all 
    % messages on the parent mint's set-weight action.
    push_request(ChildProcess, #{ <<"action">> => <<"mint">> }, Opts),
    ?assertEqual(
        [dev_process_lib:process_id(ChildProcess, Opts)],
        dev_token_lib:subscribers(ParentProcess, <<"register">>, Opts)
    ),
    % Push set-weight actions on the parent mint and verify that the child mint
    % also updates accordingly.
    push_set_weight(ParentProcess, Resource, 100, Opts),
    ?assertEqual(100, weight(ParentProcess, Resource, Opts)),
    ?assertEqual(100, weight(ChildProcess, Resource, Opts)),
    push_set_weight(ParentProcess, Resource, 200, Opts),
    ?assertEqual(200, weight(ParentProcess, Resource, Opts)),
    ?assertEqual(200, weight(ChildProcess, Resource, Opts)).

child_pot_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    % Create the parent mint, which will deliver units to the child mint.
    Resource = <<"stETH">>,
    ParentPotParams = #{
        mint_cap => 1_000_000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0
    },
    ParentToken = generate_process(ParentPotParams, Opts),
    ParentID = dev_process_lib:process_id(ParentToken, Opts),
    ?event(process, {parent_mint, ParentID}, Opts),
    % Create the child mint, which will receive units from the parent mint in
    % exchange for its own tokens.
    ChildPotParams = #{
        mint_cap => 1_000_000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0,
        parent => ParentID
    },
    ChildToken = generate_process(ChildPotParams, Opts),
    ChildID = dev_process_lib:process_id(ChildToken, Opts),
    push_request(
        ChildToken,
        #{ <<"action">> => <<"mint">> },
        Opts
    ),
    % Set the weights mints such that all units in the parent are given for
    % providing `stETH', and all units in the child are given for providing
    % `Parent'.
    push_set_weight(ParentToken, Resource, 1, Opts),
    % Deposit units of the resource into the parent mint for Alice.
    push_deposit(ParentToken, Resource, Alice, 2, Opts),
    % Delegate half of Alice's units in the parent mint to the child mint.
    Res = push_delegate(ParentToken, Resource, Alice, ChildID, 1, Opts),
    ?event(debug_test, {delegate_result, Res}, Opts),
    % Check that tokens are being minted in the parent for both the child token
    % and Alice.
    push_request(
        ParentToken,
        #{ <<"action">> => <<"mint">> },
        Alice,
        Opts
    ),
    push_request(
        ChildToken,
        #{ <<"action">> => <<"mint">> },
        Alice,
        Opts
    ),
    ParentState = dev_token_lib:now(ParentToken, Opts),
    ChildState = dev_token_lib:now(ChildToken, Opts),
    ?assert(balance(ParentState, Alice, Opts) > 0),
    ?assert(balance(ParentState, ChildID, Opts) > 0),
    % Check that Alice has received tokens in the child mint.
    ?event(debug_test,
        {states_after_mint,
            {parent, ParentState},
            {child, ChildState}
        },
        Opts
    ),
    ?assert(balance(ChildState, Alice, Opts) > 0).

%% @doc Test the viability of the `~mint-index@1.0` device, replicating delegation
%% choices of other users.
%% TODO: will fail till index is patched
child_pots_with_index_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    BaseParams =
        #{
            mint_cap => 1_000_000,
            mint_prop_numerator => 1,
            mint_prop_denominator => 2,
            t => 0,
            last_drip => 0
        },
    Resource = <<"oxygen">>,
    % Create the parent mint, which will deliver units to the child mint.
    Parent = generate_process(BaseParams, Opts),
    ParentID = dev_process_lib:process_id(Parent, Opts),
    ?event(process, {parent_mint, ParentID}, Opts),
    % Create two child mints, which will receive units from the parent mint in
    % exchange for their own tokens.
    ChildA= generate_process(BaseParams#{ <<"parent">> => ParentID }, Opts),
    ChildAID = dev_process_lib:process_id(ChildA, Opts),
    push_request(ChildA, #{ <<"action">> => <<"mint">> }, Opts),
    ChildB = generate_process(BaseParams#{ <<"parent">> => ParentID }, Opts),
    ChildBID = dev_process_lib:process_id(ChildB, Opts),
    push_request(ChildB, #{ <<"action">> => <<"mint">> }, Opts),
    % Spawn the mint index, tracking delegations to `ChildAID' and `ChildBID'.
    IndexParams =
        BaseParams#{
            parent => ParentID,
            mint_device => <<"mint-index@1.0">>,
            indexed_mints => [ChildAID, ChildBID],
            partner_device => <<"pot@1.0">>
        },
    Index = generate_process(IndexParams, Opts),
    IndexID = dev_process_lib:process_id(Index, Opts),
    push_request(Index, #{ <<"action">> => <<"mint">> }, Opts),
    push_request(ChildA, #{ <<"action">> => <<"mint">> }, Opts),
    push_request(ChildB, #{ <<"action">> => <<"mint">> }, Opts),
    ?hr(),
    ?event(
        {network_map,
            {parent, ParentID},
            {child_a, ChildAID},
            {child_b, ChildBID},
            {index, IndexID},
            {alice, id(Alice)},
            {bob, id(Bob)}
        }
    ),
    ?hr(),
    ParentState1 = dev_token_lib:now(Parent, Opts),
    ?event(debug_test, {parent_state_after_index_init, ParentState1}, Opts),
    ?hr("DEPOSITING FOR ALICE AND BOB"),
    % Alice and Bob both deposit 10 of the resource.
    push_set_weight(Parent, Resource, 100, Opts),
    push_deposit(Parent, Resource, Alice, 10, Opts),
    push_deposit(Parent, Resource, Bob, 10, Opts),
    ?hr("ESTABLISHING DELEGATIONS"),
    % Let Alice delegate completely to the index, Bob splits equally between
    % ChildA and ChildB.
    push_delegate(Parent, Resource, Alice, IndexID, 10, Opts),
    push_delegate(Parent, Resource, Bob, ChildAID, 5, Opts),
    push_delegate(Parent, Resource, Bob, ChildBID, 5, Opts),
    ?hr("MINTING"),
    % Push a `mint` operation to the parent to force a mint with the new
    % delegations.
    push_request(Parent, #{ <<"action">> => <<"mint">> }, Opts),
    push_request(ChildA, #{ <<"action">> => <<"mint">> }, Opts),
    push_request(ChildB, #{ <<"action">> => <<"mint">> }, Opts),
    push_request(Index, #{ <<"action">> => <<"mint">> }, Opts),
    ?hr("VERIFYING"),
    % ParentState2 = dev_token_lib:now(Parent, Opts),
    % IndexState = dev_token_lib:now(Index, Opts),
    % ChildAState = dev_token_lib:now(ChildA, Opts),
    % ChildBState = dev_token_lib:now(ChildB, Opts),
    % ?event(
    %     debug_test,
    %     {
    %         final_network_state,
    %         {parent, ParentState2},
    %         {index, IndexState},
    %         {child_a, ChildAState},
    %         {child_b, ChildBState}
    %     }
    % ),
    % Ensure that the index process minted tokens in both of the child mints.
    ?assert(balance(ChildAID, IndexID, Opts) > 0),
    ?assert(balance(ChildBID, IndexID, Opts) > 0).

%%% Benchmark Tests

benchmark_transfers_process_test() ->
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddr = id(BobWallet),
    % Benchmark N transfers
    Transfers = 100,
    Accounts = 1_000,
    % Setup: Alice has 1 billion tokens, the rest have 1 billion tokens each
    Base = generate_base_process_state(
        generate_token_state(
            #{
            initial_balances =>
                hb_maps:from_list(
                    [
                        {AliceAddr, 1_000_000_000}
                    ] ++
                    [
                            {
                                hb_util:human_id(crypto:strong_rand_bytes(32)),
                                1_000_000_000
                            }
                        ||
                            _ <- lists:seq(1, Accounts - 1)
                    ]
                )
            },
            Opts
        ), 
        Opts
    ),
    Reqs =
        [
            schedule_request(
                Base,
                transfer_req(BobAddr, 1, #{ <<"transfer-number">> => I }),
                AliceWallet,
                Opts
            )
            ||
                I <- lists:seq(1, Transfers)
        ],
    AOCoreStartTime = erlang:monotonic_time(millisecond),
    AOCoreInvokedState =
        lists:foldl(
            fun(Req, State) ->
                {ok, NewState} = hb_ao:resolve(State, Req, Opts),
                NewState#{ <<"results">> => #{} }
            end,
            Base,
            Reqs
        ),
    AOCoreEndTime = erlang:monotonic_time(millisecond),
    hb_test_utils:benchmark_print(
        <<"AOCore invoked transfers">>,
        <<"transfers">>,
        Transfers,
        (AOCoreEndTime - AOCoreStartTime) / 1000
    ),
    % Verify correctness
    ?assertEqual(
        1_000_000_000 - Transfers,
        balance(AOCoreInvokedState, AliceAddr, Opts)
    ),
    ?assertEqual(
        Transfers,
        balance(AOCoreInvokedState, BobAddr, Opts)
    ).

benchmark_process_transfers_test_() ->
    {timeout, 180, fun benchmark_process_transfers/0}.
benchmark_process_transfers() ->
    hb:init(),
    % Benchmark N transfers
    Transfers = 100,
    Accounts = 3_000,
    Opts =
        #{
            priv_wallet => ar_wallet:new(),
            store => [hb_test_utils:test_store()]
        },
    AliceWallet = ar_wallet:new(),
    AliceAddr = hb_util:human_id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddr = hb_util:human_id(BobWallet),
    % Setup: Alice has 1 billion tokens, the rest have 1 billion tokens each
    Base =
        generate_base_process_state(
            generate_token_state(
                #{
                    initial_balances =>
                        hb_maps:from_list(
                            [
                                {AliceAddr, 1_000_000_000}
                            ] ++
                            [
                                {
                                    hb_util:human_id(crypto:strong_rand_bytes(32)),
                                    1_000_000_000
                                }
                                ||
                                    _ <- lists:seq(1, Accounts - 1)
                            ]
                        )
                },
                Opts
            ),
            Opts
        ),
    lists:foreach(
        fun(I) ->
            schedule_request(
                Base,
                transfer_req(BobAddr, 1, #{ <<"transfer-number">> => I }),
                AliceWallet,
                Opts
            )
        end,
        lists:seq(1, Transfers)
    ),
    NowStartTime = erlang:monotonic_time(millisecond),
    State = dev_token_lib:now(Base, Opts),
    NowEndTime = erlang:monotonic_time(millisecond),
    hb_test_utils:benchmark_print(
        <<"Process transfers">>,
        <<"transfers">>,
        Transfers,
        (NowEndTime - NowStartTime) / 1000
    ),
    ?assertEqual(Transfers, balance(State, BobAddr, Opts)),
    ?assertEqual(1_000_000_000 - Transfers, balance(State, AliceAddr, Opts)),
    ?assertEqual(
        1_000_000_000 * Accounts,
        hb_ao:get(<<"total-supply">>, State, Opts)
    ).
