%%% @doc A client library for interacting with processes implementing the AO
%%% token standard, intended for use in development and testing. Offers support
%%% for generating ledgers (with support for varied implementations via separate
%%% `execution-device's), transferring tokens, and verifying the state of
%%% ledger and sub-ledgers networks.
-module(dev_token_lib).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
%%% Initialization and Push wrappers.
-export([ledger/1, ledger/2, transfer/5, transfer/6]).
-export([subledger/2, subledger/3]).
-export([register/3, subscribe/3, unsubscribe/3]).
%%% Query wrappers.
-export([now/2, balance/3, balance_total/3, balances/2, balances/3]).
-export([normalized_balance/3]).
-export([supply/2, supply/3]).
-export([subscribers/3, subscribers/4]).
-export([subledger_supply/3, user_supply/3]).
-export([ledgers/2, map/2, map/3]).
-export([verify_net/3, verify_root_supply/2, verify_net_supply/3]).
-export([verify_net_peer_balances/2]).

%%% Initialization and Push wrappers.

%% @doc Generate a token process definition message.
ledger(Opts) ->
    ledger(#{ <<"execution-device">> => <<"token@1.0">> }, Opts).
ledger(Extra, Opts) ->
    % If the `balances' key is set in the `Extra' map, ensure that any wallets
    % given as keys in the message are converted to human-readable addresses.
    HostWallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    ModExtra =
        case maps:get(<<"balances">>, Extra, undefined) of
            undefined -> Extra;
            RawBalance ->
                Extra#{
                    <<"balances">> =>
                        maps:from_list(
                            lists:filtermap(
                                fun({ID, Amount}) when ?IS_ID(ID) ->
                                    {true, {hb_util:human_id(ID), Amount}};
                                ({Wallet, Amount}) when is_tuple(Wallet) ->
                                    {
                                        true,
                                        {
                                            hb_util:human_id(Wallet),
                                            Amount
                                        }
                                    };
                                (_Other) ->
                                    false
                                end,
                                maps:to_list(RawBalance)
                            )
                        )
                }
        end,
    ?event(debug_test, {mod_extra, ModExtra}),
    Proc =
        hb_message:commit(
            maps:merge(
                #{
                    <<"device">> => <<"process@1.0">>,
                    <<"type">> => <<"Process">>,
                    <<"scheduler-device">> => <<"scheduler@1.0">>,
                    <<"scheduler">> => hb_util:human_id(HostWallet),
                    <<"authority">> => hb_util:human_id(HostWallet)
                },
                ModExtra
            ),
            Opts#{ priv_wallet => HostWallet }
        ),
    hb_cache:write(Proc, Opts),
    Proc.

%% @doc Generate a test sub-ledger process definition message.
subledger(Root, Opts) ->
    subledger(Root, #{}, Opts).
subledger(Root, Extra, Opts) ->
    BareRoot =
        maps:without(
            [<<"token">>, <<"balances">>],
            hb_message:uncommitted(Root, Opts)
        ),
    Proc = 
        hb_message:commit(
            maps:merge(
                BareRoot#{
                    <<"token">> => hb_message:id(Root, all)
                },
                Extra
            ),
            Opts#{ priv_wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts) }
        ),
    hb_cache:write(Proc, Opts),
    Proc.

%% @doc Generate a test transfer message.
transfer(ProcMsg, Sender, Recipient, Quantity, Opts) ->
    transfer(ProcMsg, Sender, Recipient, Quantity, undefined, Opts).
transfer(ProcMsg, Sender, Recipient, Quantity, Route, Opts) ->
    MaybeRoute =
        if Route == undefined -> #{};
           true ->
                #{
                    <<"route">> =>
                        if is_map(Route) -> hb_message:id(Route, all);
                        true -> Route
                        end
                }
        end,
    push(
        ProcMsg,
        MaybeRoute#{
            <<"action">> => <<"Transfer">>,
            <<"recipient">> => hb_util:human_id(Recipient),
            <<"quantity">> => Quantity
        },
        Sender,
        Opts
    ).

%% @doc Request that a peer register with a without sub-ledger.
register(ProcMsg, Peer, Opts) when is_map(Peer) ->
    register(ProcMsg, hb_message:id(Peer, all), Opts);
register(ProcMsg, PeerID, Opts) ->
    push(
        ProcMsg,
        #{
            <<"action">> => <<"register-remote">>,
            <<"peer">> => PeerID
        },
        Opts
    ).

%% @doc Subscribe to receive notifications upon a given `action' and (optionally)
%% `target' from a given process.
subscribe(ProcMsg, Action, Opts) ->
    subscribe(ProcMsg, Action, <<"broadcast">>, Opts).
subscribe(ProcMsg, Action, Target, Opts) ->
    push(
        ProcMsg,
        #{
            <<"action">> => <<"subscribe">>,
            <<"subscribe-action">> => Action,
            <<"subscribe-target">> => Target
        },
        Opts
    ).

%% @doc Unsubscribe from receiving notifications upon a given `action' and (optionally)
%% `target' from a given process.
unsubscribe(ProcMsg, Action, Opts) ->
    unsubscribe(ProcMsg, Action, <<"broadcast">>, Opts).
unsubscribe(ProcMsg, Action, Target, Opts) ->
    push(
        ProcMsg,
        #{
            <<"action">> => Action,
            <<"subscribe-action">> => Action,
            <<"subscribe-target">> => Target
        },
        Opts
    ).

%%% Query wrappers.

%% @doc Get the current state of a process.
now(ProcMsg, Opts) ->
    {ok, State} = hb_ao:resolve(ProcMsg, #{ <<"path">> => <<"now">> }, Opts),
    State.

%% @doc Helper function to push a message to a process. Signs the message with the
%% default key in the `Opts'.
push(Process, Msg, RawOpts) ->
    push(Process, Msg, hb_opts:get(priv_wallet, hb:wallet(), RawOpts), RawOpts).
push(Process, Msg, MsgWallet, RawOpts) ->
    UserOpts = RawOpts#{ priv_wallet => MsgWallet },
    SystemOpts =
        RawOpts#{
            priv_wallet => hb_opts:get(priv_wallet, hb:wallet(), RawOpts)
        },
    Req =
        hb_message:commit(
            #{
                <<"path">> => <<"push">>,
                <<"body">> =>
                    hb_message:commit(
                        Msg#{
                            <<"target">> =>
                                if is_binary(Process) ->
                                    Process;
                                true ->
                                    dev_process_lib:process_id(Process, SystemOpts)
                                end
                        },
                        UserOpts
                    )
            },
            UserOpts
        ),
    hb_ao:resolve(Process, Req, SystemOpts).

%% @doc Retreive a single balance from the ledger.
balance(ProcMsg, User, Opts) when not ?IS_ID(User) ->
    balance(ProcMsg, hb_util:human_id(ar_wallet:to_address(User)), Opts);
balance(ProcMsg, ID, Opts) ->
    hb_ao:get(<<"now/balances/", ID/binary>>, ProcMsg, 0, Opts).

%% @doc Retrieve a single balance through the execution device's `balance`
%% path, allowing lazy mint devices to normalize account state first.
normalized_balance(ProcMsg, User, Opts) when not ?IS_ID(User) ->
    normalized_balance(ProcMsg, hb_util:human_id(ar_wallet:to_address(User)), Opts);
normalized_balance(ProcMsg, ID, Opts) ->
    CurrentSlot = hb_ao:get(<<"slot/current">>, ProcMsg, Opts),
    Res =
        hb_ao:resolve_many(
            [
                ProcMsg,
                #{ <<"path">> => <<"now">> },
                #{
                    <<"path">> => <<"as">>,
                    <<"as">> => <<"execution">>
                },
                #{
                    <<"path">> => <<"balance">>,
                    <<"balance">> => ID,
                    <<"slot">> => hb_cache:ensure_loaded(CurrentSlot, Opts)
                }
            ],
            Opts
        ),
    case Res of
        {ok, Balance} -> Balance;
        {error, not_found} -> 0
    end.

%% @doc Get the total balance for an ID across all ledgers in a set.
balance_total(Procs, ID, Opts) ->
    lists:sum(
        lists:map(
            fun(Proc) -> balance(Proc, ID, Opts) end,
            maps:values(normalize_env(Procs))
        )
    ).

%% @doc Get the balances of a ledger.
balances(ProcMsg, Opts) ->
    balances(now, ProcMsg, Opts).
balances(initial, ProcMsg, Opts) ->
    balances(<<"">>, ProcMsg, Opts);
balances(Mode, ProcMsg, Opts) when is_atom(Mode) ->
    balances(hb_util:bin(Mode), ProcMsg, Opts);
balances(Prefix, ProcMsg, Opts) ->
    Balances = hb_ao:get(<<Prefix/binary, "/balances">>, ProcMsg, #{}, Opts),
    hb_private:reset(
        hb_message:uncommitted(
            hb_cache:ensure_all_loaded(Balances, Opts),
            Opts
        )
    ).

%% @doc Get the supply of a ledger, either `now` or `initial`.
supply(ProcMsg, Opts) ->
    supply(now, ProcMsg, Opts).
supply(Mode, ProcMsg, Opts) ->
    BalancesVal = maps:values(balances(Mode, ProcMsg, Opts)),
    Balances = lists:filter(fun is_number/1, BalancesVal),
    lists:sum(Balances).

%% @doc Calculate the supply of tokens in all sub-ledgers, from the balances of
%% the root ledger.
subledger_supply(RootProc, AllProcs, Opts) ->
    supply(now, RootProc, Opts) - user_supply(RootProc, AllProcs, Opts).

%% @doc Calculate the supply of tokens held by users on a ledger, excluding
%% those held in sub-ledgers.
user_supply(Proc, AllProcs, Opts) ->
    NormProcs = normalize_without_root(Proc, AllProcs),
    SubledgerIDs = maps:keys(NormProcs),
    Vals =
        maps:values(
            maps:without(
                SubledgerIDs,
                balances(now, Proc, Opts)
            )
        ),
    NumericVals = lists:filter(fun is_number/1, Vals),
    lists:sum(NumericVals).

%% @doc Get the local expectation of a ledger's balances with peer ledgers.
ledgers(ProcMsg, Opts) ->
    case hb_cache:ensure_all_loaded(
        hb_ao:get(<<"now/ledgers">>, ProcMsg, #{}, Opts),
        Opts
    ) of
        Msg when is_map(Msg) -> hb_private:reset(Msg);
        [] -> #{}
    end.

%% @doc Get the subscribers of a process for a given action and target.
subscribers(ProcMsg, Action, Opts) ->
    subscribers(
        ProcMsg,
        Action,
        <<"broadcast">>,
        Opts
    ).
subscribers(ProcMsg, Action, Target, Opts) ->
    dev_process_outbox:subscribers(now(ProcMsg, Opts), Action, Target, Opts).

%% @doc Generate a complete overview of the test environment's balances and 
%% ledgers. Optionally, a map of environment names can be provided to make the
%% output more readable.
map(Procs, Opts) ->
    NormProcs = normalize_env(Procs),
    maps:merge_with(
        fun(Key, Balances, Ledgers) ->
            MaybeRoot =
                case maps:get(Key, NormProcs, #{}) of
                    #{ <<"token">> := _ } -> #{};
                    _ -> #{ root => true }
                end,
            MaybeRoot#{
                balances => Balances,
                ledgers => Ledgers
            }
        end,
        maps:map(fun(_, Proc) -> balances(Proc, Opts) end, NormProcs),
        maps:map(fun(_, Proc) -> ledgers(Proc, Opts) end, NormProcs)
    ).
map(Procs, EnvNames, Opts) ->
    apply_names(map(Procs, Opts), EnvNames, Opts).

%% @doc Apply a map of environment names to elements in either a map or list.
%% Expects a map of `ID or ProcMsg or Wallet => Name' as the `EnvNames' argument,
%% and a potentially deep map or list of elements to apply the names to.
apply_names(Map, EnvNames, Opts) ->
    IDs =
        maps:from_list(
            lists:filtermap(
                fun({Key, V}) ->
                    try {true, {hb_util:human_id(Key), V}}
                    catch _:_ ->
                        try {true, {hb_message:id(Key, all), V}}
                        catch _:_ -> false
                        end
                    end
                end,
                maps:to_list(EnvNames)
            )
        ),
    do_apply_names(Map, maps:merge(IDs, EnvNames), Opts).
do_apply_names(Map, EnvNames, Opts) when is_map(Map) ->
    maps:from_list(
        lists:map(
            fun({Key, Proc}) ->
                {
                    apply_names(Key, EnvNames, Opts),
                    apply_names(Proc, EnvNames, Opts)
                }
            end,
            maps:to_list(Map)
        )
    );
do_apply_names(List, EnvNames, Opts) when is_list(List) ->
    lists:map(
        fun(Proc) ->
            apply_names(Proc, EnvNames, Opts)
        end,
        List
    );
do_apply_names(Item, Names, _Opts) when is_map_key(Item, Names) ->
    maps:get(Item, Names);
do_apply_names(Item, Names, _Opts) ->
    try maps:get(hb_util:human_id(Item), Names, Item)
    catch _:_ -> Item
    end.

%%% Test ledger network invariants.
%%% 
%%% Complex assertions that verify specific invariants about the state of
%%% ledgers in a test environment. These are used to validate the correctness
%%% of the `hyper-token.lua` script. Tested invariants are listed below.
%%% 
%%% For every timestep `t_n`, the following invariants must hold:
%%% 1. The root ledger supply at `t_0` must match the current supply.
%%% 2. For every sub-ledger `l`, each expected balance held in `l/now/ledgers`
%%%    must equal the balance found at `peer/now/balances/l`.
%%% 3. The sum of all values in `/now/balances` across all sub-ledgers must
%%%    equal the root ledger's supply.

%% @doc Execute all invariant checks for a pair of root ledger and sub-ledgers.
verify_net(RootProc, AllProcs, Opts) ->
    verify_net_supply(RootProc, AllProcs, Opts),
    verify_net_peer_balances(AllProcs, Opts).

%% @doc Verify that the initial supply of tokens on the root ledger is the same
%% as the current supply. This invariant will not hold for sub-ledgers, as they
%% 'mint' tokens in their local supply when they receive them from other ledgers.
verify_root_supply(RootProc, Opts) ->
    ?assert(
        supply(initial, RootProc, Opts) ==
        supply(now, RootProc, Opts) +
            lists:sum(maps:values(ledgers(RootProc, Opts)))
    ).

%% @doc Verify that the sum of all spendable balances held by ledgers in a
%% test network is equal to the initial supply of tokens.
verify_net_supply(RootProc, AllProcs, Opts) ->
    verify_root_supply(RootProc, Opts),
    StartingRootSupply = supply(initial, RootProc, Opts),
    NormProcsWithoutRoot = normalize_without_root(RootProc, AllProcs),
    RootUserSupply = user_supply(RootProc, NormProcsWithoutRoot, Opts),
    SubledgerSupply = subledger_supply(RootProc, AllProcs, Opts),
    ?event({verify_net_supply, {root, RootUserSupply}, {subledger, SubledgerSupply}}),
    ?assert(
        StartingRootSupply ==
        RootUserSupply + SubledgerSupply
    ).

%% @doc Verify the consistency of all expected ledger balances with their peer
%% ledgers and the actual balances held.
verify_net_peer_balances(AllProcs, Opts) ->
    NormProcs = normalize_env(AllProcs),
    maps:map(
        fun(ValidateProc, _) ->
            verify_peer_balances(ValidateProc, NormProcs, Opts)
        end,
        NormProcs
    ).

%% @doc Verify that a ledger's expectation of its balances with peer ledgers
%% is consistent with the actual balances held.
verify_peer_balances(ValidateProc, AllProcs, Opts) ->
    Ledgers = ledgers(ValidateProc, Opts),
    NormProcs = normalize_env(AllProcs),
    maps:map(
        fun(PeerID, ExpectedBalance) ->
            ?assertEqual(
                ExpectedBalance,
                balance(ValidateProc,
                    maps:get(PeerID, NormProcs),
                    Opts
                )
            )
        end,
        Ledgers
    ).

%%% Test utilities.

%% @doc Normalize a set of processes, representing ledgers in a test environment,
%% to a canonical form: A map of `ID => Proc`.
normalize_env(Procs) when is_map(Procs) ->
    normalize_env(maps:values(Procs));
normalize_env(Procs) when is_list(Procs) ->
    maps:from_list(
        lists:map(
            fun(Proc) ->
                {hb_message:id(Proc, all), Proc}
            end,
            Procs
        )
    ).

%% @doc Return the normalized environment without the root ledger.
normalize_without_root(RootProc, Procs) ->
    maps:without([hb_message:id(RootProc, all)], normalize_env(Procs)).
