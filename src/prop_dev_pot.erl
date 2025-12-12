-module(prop_dev_pot).
-include_lib("proper/include/proper.hrl").
-include("include/hb.hrl").

-behaviour(proper_statem).

-export([initial_state/0, command/1, precondition/2, next_state/3, postcondition/3]).

% Our state model maintains some necessary state that isn't maintained by the pot,
% and it also duplicates some state from the pot in ways that are helpful for
% command generation. 'resources' and 'addrs' are lists of resource IDs and user
% addresses that have been generated in previous steps. 'orig_deposits' maintains
% the quantity of each resource *originally introduced* by each user address, which
% is necessary to prevent overdrafting. 'inverted_orig_deposits' indexes the same
% quantities inversely. This is necessary to generate withdraw() commands, as we
% might have resource types in the system without any deposits yet, since the
% set_weight() command can add a new resource type to the system at any given step.
-record(
    state,
    {
        resources = [],
        addrs = [],
        orig_deposits = #{},
        inverted_orig_deposits = #{},
        s
     }
).

prop_state_machine() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin
            {History, State, Results} = run_commands(?MODULE, Cmds),
            Results =:= ok
        end
    ).

addr_gen() ->
    base64:encode(crypto:strong_rand_bytes(32), #{mode => urlsafe, padding => false}).

weight_gen() ->
    1 + rand:uniform(10_000).

resource_gen() ->
    base64:encode(crypto:strong_rand_bytes(32), #{mode => urlsafe, padding => false}).

qty_gen() ->
    1 + rand:uniform(1_000_000).

t_gen(LastT) ->
    LastT + rand:uniform(100_000).

initial_state() ->
    StartResource = resource_gen(),
    StartAddr = addr_gen(),
    StartQty = qty_gen(),
    StartWeight = weight_gen(),
    MintCap = 100 + rand:uniform(1_000_000_000_000_000),
    PropN = 1 + rand:uniform(1_000),
    PropD = PropN + rand:uniform(10_000),
    #state{
        resources = [StartResource],
        addrs = [StartAddr],
        orig_deposits = #{StartResource => #{StartAddr => StartQty}},
        inverted_orig_deposits = #{StartAddr => #{StartResource => StartQty}},
        s = #{
            <<"device">> => <<"pot@1.0">>,
            <<"t">> => 0,
            <<"last-drip">> => 0,
            <<"mint-cap">> => MintCap,
            <<"mint-prop-numerator">> => PropN,
            <<"mint-prop-denominator">> => PropD,
            <<"resources">> => #{
                StartResource => #{
                    <<"accumulator">> => 1, % TODO: randomize this?
                    <<"last-global-accumulator">> => 1,
                    <<"weight">> => StartWeight,
                    <<"total-deposits">> => StartQty,
                    <<"deposits">> => #{
                        StartAddr => #{
                            <<"quantity">> => StartQty,
                            <<"last-resource-accumulator">> => 1 % TODO: randomize this?
                        }
                    }
                }
            },
            <<"balances">> => #{ },
            <<"users">> => #{
                StartAddr => #{
                    <<"deposits">> => #{
                        StartResource => StartQty
                    }
                }
            }
        }
    }.

command(
    State = #state{
        resources = Resources,
        addrs = Addrs,
        orig_deposits = OrigDeposits,
        inverted_orig_deposits = InvertedOrigDeposits,
        s = S
    }
) ->
    NextT = t_gen(maps:get(<<"last-drip">>, S)),
    ExistingResourceGen = elements(Resources),
    % TODO: the resource ID generated here can collide with existing resources, do we care?
    NewResourceGen = resource_gen(),
    % Call set_weight() for an existing resource or a new resource and a random weight
    SetWeightGen =
        ?LET(
                {ResourceID, Weight},
                {
                    frequency([{5, ExistingResourceGen}, {5, NewResourceGen}]),
                    weight_gen()
                },
                {
                    call,
                    dev_pot,
                    set_weight,
                    [
                        ResourceID,
                        Weight,
                        S#{ <<"t">> => NextT },
                        #{}
                    ]
                }
        ),
    ExistingAddrGen = elements(Addrs),
    NewAddrGen = addr_gen(),
    % Call balance() for an existing address
    BalanceGen =
        ?LET(Addr, ExistingAddrGen, {call, dev_pot, balance, [Addr, S#{ <<"t">> => NextT }]}),
    % Call deposit() for an existing resource, an existing or new address, and a random qty
    DepositGen =
        ?LET(
                {ResourceID, DepositAddr, DepositAmount},
                {
                    ExistingResourceGen,
                    frequency([{5, ExistingAddrGen}, {5, NewAddrGen}]),
                    qty_gen()
                },
                {
                    call,
                    dev_pot,
                    deposit,
                    [
                        ResourceID,
                        DepositAddr,
                        DepositAmount,
                        S,
                        #{}
                    ]
                }
        ),
    KeysA = maps:keys(InvertedOrigDeposits),
    SizeA = length(KeysA),
    WithdrawableAddr =
        lists:nth(rand:uniform(SizeA), KeysA),
    WithdrawableDeposits = maps:get(WithdrawableAddr, InvertedOrigDeposits),
    KeysR = maps:keys(WithdrawableDeposits),
    SizeR = length(KeysR),
    WithdrawableResourceID =
        lists:nth(rand:uniform(SizeR), KeysR),
    WithdrawableQty = maps:get(WithdrawableResourceID, WithdrawableDeposits),

    % Call withdraw() for a deposit that actually exists, and for a safe qty
    WithdrawGen =
        ?LET(
            {WithdrawAmount},
            {choose(1, WithdrawableQty)},
            {
                call,
                dev_pot,
                withdraw,
                [
                    WithdrawableAddr,
                    WithdrawableResourceID,
                    WithdrawAmount,
                    S,
                    #{}
                ]
            }
        ),
    % TODO:
    % delegate: pick a sender address that has an actual (not orig) deposit > 0, and
    % and a recipient address that may or may not already exist, and delegate <= the sender's qty
    % undelegate: pick a delegation in the delegation table and undelegate <= the qty

    % TODO: tweak these weights
    frequency([
        {3, SetWeightGen},
        {3, BalanceGen},
        {0, DepositGen},
        {0, WithdrawGen}
    ]);
command(_State) ->
   % TODO: this clause is necessary for proper reasons, but are we doing it right?
   {call, erlang, self, []}.

next_state(
    _State,
    {call, _Mod, set_weight, [ResourceID, _Weight, _S, _Opts]},
    Result = #state{resources = Resources}
) ->
    % If we generated a new resource type, add it to the state model
    NewResources =
        case lists:member(ResourceID, Resources) of
            true -> Resources;
            false -> [ResourceID | Resources]
        end,
    Result#{
        resources => NewResources
    };
next_state(
    _State,
    {call, _Mod, deposit, [ResourceID, Addr, Amount, _S, _Opts]},
    Result = #state{
        addrs = Addrs,
        orig_deposits = OrigDeposits,
        inverted_orig_deposits = OrigDepositsInverted
    }
) ->
    NewAddrs =
        case lists:member(Addr, Addrs) of
            true -> Addrs;
            false -> [Addr | Addrs]
        end,
    Deposits = maps:get(ResourceID, OrigDeposits, #{}),
    UserDeposit = maps:get(Addr, Deposits, 0),
    DepositsInverted = maps:get(Addr, OrigDepositsInverted, #{}),
    UserDepositInverted = maps:get(ResourceID, DepositsInverted, 0),
    Result#{
        addrs => NewAddrs,
        orig_deposits => OrigDeposits#{ResourceID => Deposits#{Addr => UserDeposit + Amount}},
        inverted_orig_deposits => OrigDepositsInverted#{
            Addr => #{ResourceID => UserDepositInverted + Amount}
        }
    };
% TODO: next_state for withdraw and deposit can be collapsed into a single function,
% it's just addition vs subtraction
next_state(
    _State,
    {call, _Mod, withdraw, [ResourceID, Addr, Amount, _S, _Opts]},
    Result = #state{
        addrs = Addrs,
        orig_deposits = OrigDeposits,
        inverted_orig_deposits = OrigDepositsInverted
    }
) ->
    NewAddrs =
        case lists:member(Addr, Addrs) of
            true -> Addrs;
            false -> [Addr | Addrs]
        end,
    Deposits = maps:get(ResourceID, OrigDeposits, #{}),
    UserDeposit = maps:get(Addr, Deposits, 0),
    DepositsInverted = maps:get(Addr, OrigDepositsInverted, #{}),
    UserDepositInverted = maps:get(ResourceID, DepositsInverted, 0),
    Result#{
        addrs => NewAddrs,
        orig_deposits => OrigDeposits#{ResourceID => Deposits#{Addr => UserDeposit - Amount}},
        inverted_orig_deposits => OrigDepositsInverted#{
            Addr => #{ResourceID => UserDepositInverted - Amount}
        }
    };

next_state(_State, _Call, Result) -> Result.

% TODO: are any preconditions necessary?
precondition(_State, _Call) -> true.

% TODO: deeply verify the whole output state, not just specifically chosen keys?
postcondition(
    _State,
    {call, _Mod, set_weight, [ResourceID, Weight, _S, _Opts]},
    ResultS
) ->
    Weight =:= hb_ao:get(
        <<"/resources/", ResourceID/binary, "/weight">>,
        ResultS,
        0,
        #{}
    );
postcondition(
    _State,
    {
        call,
        _Mod,
        balance,
        [
            Addr,
            S = #{
                <<"t">> := T,
                <<"last-drip">> := Last,
                <<"balances">> := Balances,
                <<"resources">> := Resources
           }
        ]
    },
    ResultBalance
) ->
    % TODO: can we improve upon this property?
    ResIDs = maps:keys(Resources),
    AddrDeposits = lists:map(fun(Res) -> dev_pot:get_deposit(Addr, Res, S) end, ResIDs),
    HasPositiveDeposit = lists:any(fun(Dep) -> Dep > 0 end, AddrDeposits),
    StartBalance = hb_maps:get(Addr, Balances, 0, #{}),
    DeltaT = T - Last,
    case DeltaT of
        DT when DT =:= 0 ->
            ResultBalance =:= StartBalance;
        DT when DT > 0 andalso HasPositiveDeposit ->
            ResultBalance > StartBalance
    end;
postcondition(_State, _Command, _ResultS) ->
    true.
