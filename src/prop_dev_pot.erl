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
            {History, State, Result} = run_commands(?MODULE, Cmds),
            Result =:= ok
        end).

addr_gen() ->
    ?LET(Bytes, binary(32), base64:encode(Bytes)).

weight_gen() ->
    rand:uniform(10_000).

resource_gen() ->
    rand:uniform(1_000_000_000).

qty_gen() ->
    rand:uniform(1_000_000).

initial_state() ->
    StartResource = resource_gen(),
    StartAddr = addr_gen(),
    StartQty = qty_gen(),
    StartWeight = weight_gen(),
    ?LET({MintCap, PropN, PropD},
        % TODO: PropD must be > PropN
        {
            choose(1_000, 1_000_000_000_000_000),
            choose(1, 1_000),
            choose(1, 10_000)
        },
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
                <<"mint-prop">> => {PropN, PropD},
                <<"resources">> => #{
                    StartResource => #{
                        <<"weight">> => StartWeight,
                        <<"total-deposits">> => StartQty,
                        <<"deposits">> => #{
                            StartAddr => #{
                                <<"quantity">> => StartQty,
                                <<"last-resource-accumulator">> => 0
                            }
                        }
                    }
                },
                <<"balances">> => #{ }
            }
        }).

command(_State) ->
    % TODO: this clause is necessary for proper reasons, but are we doing it right?
    ?LET(_, integer(), {call, erlang, self, []});
command(
    State = #state{
        resources = Resources,
        addrs = Addrs,
        orig_deposits = OrigDeposits,
        inverted_orig_deposits = InvertedOrigDeposits,
        s = S
    }
) ->
    % TODO: must we transform args to binary while generating the calls?
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
                        S,
                        #{}
                    ]
                }
        ),
    ExistingAddrGen = elements(Addrs),
    NewAddrGen = addr_gen(),
    % Call balance() for an existing address
    BalanceGen =
        ?LET(Addr, ExistingAddrGen, {call, dev_pot, balance, [Addr, S]}),
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
    WithdrawableAddr =
        lists:nth(
            rand:uniform(maps:size(InvertedOrigDeposits), maps:keys(InvertedOrigDeposits))
        ),
    WithdrawableDeposits = maps:get(WithdrawableAddr, InvertedOrigDeposits),
    WithdrawableResourceID =
        lists:nth(
            rand:uniform(maps:size(WithdrawableDeposits), maps:keys(WithdrawableDeposits))
        ),
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
        {3, DepositGen},
        {3, WithdrawGen}
    ]).

next_state(
    _State,
    {call, _Mod, set_weight, [ResourceID, _Weight, _S, _Opts]},
    Result = #state{resources = Resources}
) ->
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
    {call, _Mod, balance, [Addr, _S]},
    Result = #state{addrs = Addrs}
) ->
    NewAddrs =
        case lists:member(Addr, Addrs) of
            true -> Addrs;
            false -> [Addr | Addrs]
        end,
    Result#{
        addrs => NewAddrs
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
% TODO: next_state for withdraw and deposit can be collapsed into a single function
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

postcondition(_State, _Command, _Result) ->
    % TODO: actually write assertions
    true.
