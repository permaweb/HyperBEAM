%%% @doc A client library for interacting with processes implementing the AO
%%% token standard, intended for use in development and testing. Offers support
%%% for generating ledgers (with support for varied implementations via separate
%%% `execution-device's), transferring tokens, and verifying the state of
%%% ledger and sub-ledgers networks.
-module(dev_pot_lib).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
%%% Initialization and Push wrappers.
-export([pot/1 ,pot/2]).
-export([register/4, deposit/5, withdraw/5, delegate/6, undelegate/6]).
% %%% Query wrappers.
-export([get_deposit/4, get_weight/3, get_total_deposit/3]).
-export([get_deposits/2, get_deposits/3]).
-export([now/2, balance/3]).
% -export([ledgers/2, map/2, map/3]).

%%% Initialization and Push wrappers.
pot(Opts) ->
    pot(#{}, Opts).
pot(ProcMsg, Opts) ->
    IndexKeys =
        case hb_maps:is_key(<<"provider-mint-device">>, ProcMsg, Opts) of
            true ->
                maps:merge(
                    #{
                        <<"update-every">> => 1,
                        <<"indexed-mints">> => []
                    },
                    ProcMsg
                );
            false ->
                ProcMsg
        end,
    Defaults = #{
        <<"execution-device">> => <<"token@1.0">>,
        <<"t-source">> => <<"slot">>,
        <<"mint-device">> => <<"pot@1.0">>,
        <<"mint-cap">> => 10_000,
        <<"mint-prop-numerator">> => 1,
        <<"mint-prop-denominator">> => 2,
        <<"total-weighted-units">> => 0,
        <<"resources">> => #{},
        <<"t">> => 0,
        <<"last-drip">> => 0
    },
    BaseState = maps:merge(Defaults, IndexKeys),
    dev_token_lib:ledger(BaseState, Opts).
    
register(Process, Resource, Weight, Opts) ->
    dev_process_lib:push(
        Process,
        #{
            <<"action">> => <<"register">>,
            <<"resource">> => Resource,
            <<"weight">> => Weight
        },
        Opts
    ).

deposit(Process, Resource, User, Qty, Opts) ->
    dev_process_lib:push(
        Process,
        #{
            <<"action">> => <<"deposit">>,
            <<"resource">> => Resource,
            <<"address">> => id(User),
            <<"quantity">> => Qty
        },
        Opts
    ).

withdraw(Process, Resource, User, Qty, Opts) ->
    dev_process_lib:push(
        Process,
        #{
            <<"action">> => <<"withdraw">>,
            <<"resource">> => Resource,
            <<"address">> => id(User),
            <<"quantity">> => Qty
        },
        User,
        Opts
    ).

delegate(Process, Resource, User, ToAddr, Qty, Opts) ->
    dev_process_lib:push(
        Process,
        #{
            <<"action">> => <<"delegate">>,
            <<"resource">> => Resource,
            <<"address">> => ToAddr,
            <<"quantity">> => Qty
        },
        User,
        Opts
    ).

undelegate(Process, Resource, User, FromAddr, Qty, Opts) ->
    dev_process_lib:push(
        Process,
        #{
            <<"action">> => <<"undelegate">>,
            <<"resource">> => Resource,
            <<"address">> => FromAddr,
            <<"quantity">> => Qty
        },
        User,
        Opts
    ).

%% @doc Get the deposit quantity for a specific address in a specific resource.
get_deposit(Process, ResourceID, User, Opts) when is_tuple(User) ->
    get_deposit(Process, ResourceID, id(User), Opts);
get_deposit(Process, ResourceID, Addr, Opts) ->
    hb_ao:get(
        <<"now", (dev_pot:deposit_qty_path(ResourceID, Addr))/binary>>, 
        Process, 
        0,
        Opts
    ).

%% @doc Return only the deposits submessage for all resources in the state.
get_deposits(S = #{ <<"resources">> := Resources }, Opts) ->
    hb_maps:map(
        fun(ResourceID, _) -> get_deposits(S, ResourceID, Opts) end,
        Resources,
        Opts
    ).
get_deposits(S, ResourceID, Opts) ->
    Ds = hb_ao:get(
        <<"now", (dev_pot:resource_path(ResourceID))/binary, "/deposits">>,
        S,
        #{},
        Opts
    ),
    hb_maps:map(
        fun(Addr, _) -> get_deposit(S, ResourceID, Addr, Opts) end,
        Ds,
        Opts
    ).

%% @doc Return the weight for a given resource on a `~pot@1.0' process.
get_weight(Process, ResourceID, Opts) ->
    hb_ao:get(
        <<"now", (dev_pot:resource_weight_path(ResourceID))/binary>>, 
        Process, 
        0,
        Opts
    ).

get_total_deposit(Process, ResourceID, Opts) ->
    hb_ao:get(
        <<"now", (dev_pot:resource_total_deposits_path(ResourceID))/binary>>, 
        Process, 
        0, 
        Opts
    ).

% %%% Query wrappers.

%% @doc Get the current state of a process.
now(ProcMsg, Opts) ->
    dev_process_lib:now(ProcMsg, Opts).

id(Wallet) ->
    dev_process_lib:wallet_id(Wallet).

%% @doc Get balance for an account.
balance(Process, Wallet, Opts) when is_tuple(Wallet) ->
    balance(Process, id(Wallet), Opts);
balance(Process, Account, Opts) when is_binary(Account) ->
    balance(Process, #{ <<"balance">> => Account }, Opts);
balance(Process, Req, Opts) ->
    CurrentSlot = hb_ao:get(<<"slot/current">>, Process, Opts),
    Query = 
        [
            Process,
            #{ <<"path">> => <<"now">> },
            #{ <<"path">> => <<"as">>, <<"as">> => <<"execution">> },
            Req#{
                <<"path">> => <<"balance">>,
                <<"slot">> => hb_cache:ensure_loaded(CurrentSlot, Opts)
            }
        ],
    Res = hb_ao:resolve_many(Query, Opts),
    case Res of
        {ok, B} -> B;
        {error, not_found} -> 0
    end.