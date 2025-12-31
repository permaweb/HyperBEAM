
%%% ~rate-limit@1.0
%%% ===
%%% A `~p4@1.0`-compatible `ledger' device, tracking rate-limits for users of a HyperBEAM node. 
%%% Stores state information in the `node message' of a HyperBEAM HTTP server, 
%%% in a similar fashion to `~simple-pay@1.0`.
%%% 
%%% The device's pricing rules are as follows:
%%%
%%% 1. There are 2 node options, rate_limit_window, and
%%%    rate_limit_balance.
%%% 2. On the first request, an IP address receives a balance, 
%%%    each value of 1 in the balance represents a single request 
%%%    allowed to the user. The balance will be initialized to the 
%%%    rate_limit_balance value from Opts.
%%% 3. The price of each request is always 1. On each request including the 
%%%    first request, which gave them the balance, the charge function will 
%%%    reduce balance by 1
%%% 4. Before it reduces by 1 however, the balance function will replenish 
%%%    the ledger based on the timestamps of the current request and the
%%%    previous request, according to the rate_limit_window Opt.
%%% 
%%% 
-module(dev_rate_limit).
-behaviour(gen_server).
-export([balance/3, charge/3, estimate/3, start_link/0, ensure_started/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").


%% gen_server interface

start_link() ->
      gen_server:start_link({local, ?MODULE}, ?MODULE, #{}, []).

ensure_started() ->
    case whereis(?MODULE) of
        undefined ->
            case start_link() of
                {ok, Pid} -> {ok, Pid};
                {error, {already_started, Pid}} -> {ok, Pid}
            end;
        Pid ->
            {ok, Pid}
    end.

init(_Args) ->
      {ok, #{}}.

handle_call({get_or_init_balance, ClientIP, ReferenceId, ServerRef}, _From, State) ->
    %% Fetch NodeMsg from the HTTP server to get configuration
    NodeMsg = hb_http_server:get_opts(#{http_server => ServerRef}),

    %% Get rate limit configuration
    InitAmount = hb_ao:get(rate_limit_balance, NodeMsg, 100, NodeMsg),

    %% Get ledger from State
    Ledger = maps:get(ServerRef, State, #{}),
    Balances = maps:get(<<"balances">>, Ledger, #{}),
    IpReferences = maps:get(<<"ip_references">>, Ledger, #{}),

    case maps:get(ClientIP, Balances, not_found) of
        not_found ->
            NewBalances = maps:put(ClientIP, InitAmount, Balances),
            NewIpReferences = maps:put(ReferenceId, ClientIP, IpReferences),
            NewLedger = #{
                <<"balances">> => NewBalances,
                <<"ip_references">> => NewIpReferences
            },
            NewState = maps:put(ServerRef, NewLedger, State),
            {reply, {ok, InitAmount, initialized}, NewState};
        Balance ->
            NewIpReferences = maps:put(ReferenceId, ClientIP, IpReferences),
            NewLedger = #{
                <<"balances">> => Balances,
                <<"ip_references">> => NewIpReferences
            },
            NewState = maps:put(ServerRef, NewLedger, State),
            {reply, {ok, Balance, existing}, NewState}
    end;

handle_call({ensure_reference, ReferenceId, ClientIP, ServerRef}, _From, State) ->
    %% Get ledger from State
    Ledger = maps:get(ServerRef, State, #{}),
    Balances = maps:get(<<"balances">>, Ledger, #{}),
    IpReferences = maps:get(<<"ip_references">>, Ledger, #{}),

    NewIpReferences = maps:put(ReferenceId, ClientIP, IpReferences),
    NewLedger = #{
        <<"balances">> => Balances,
        <<"ip_references">> => NewIpReferences
    },

    NewState = maps:put(ServerRef, NewLedger, State),
    {reply, ok, NewState};

handle_call({get_ip_by_reference, ReferenceId, ServerRef}, _From, State) ->
    %% Get ledger from State
    Ledger = maps:get(ServerRef, State, #{}),
    IpReferences = maps:get(<<"ip_references">>, Ledger, #{}),
    ClientIP = maps:get(ReferenceId, IpReferences, not_found),
    {reply, ClientIP, State};

handle_call({decrement_balance, ClientIP, ServerRef}, _From, State) ->
    %% Get ledger from State
    Ledger = maps:get(ServerRef, State, #{}),
    Balances = maps:get(<<"balances">>, Ledger, #{}),
    IpReferences = maps:get(<<"ip_references">>, Ledger, #{}),
    CurrentBalance = maps:get(ClientIP, Balances, 0),

    case CurrentBalance > 0 of
        true ->
            NewBalance = CurrentBalance - 1,
            NewBalances = maps:put(ClientIP, NewBalance, Balances),
            NewLedger = #{
                <<"balances">> => NewBalances,
                <<"ip_references">> => IpReferences
            },

            NewState = maps:put(ServerRef, NewLedger, State),
            {reply, {ok, NewBalance}, NewState};
        false ->
            {reply, {error, insufficient_balance}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% P4 interface

estimate(_, _, _) ->
    {ok, 1}.

gen_reference(RawReq, Req, NodeMsg) ->
    Commitments = case Req of
        not_found ->
            hb_ao:get(<<"commitments">>, RawReq, #{}, NodeMsg);
        _ ->
            hb_ao:get(<<"commitments">>, Req, #{}, NodeMsg)
    end,
    %% ?event({ref_commitments, Commitments}),
    SortedKeys = lists:sort(maps:keys(Commitments)),

    case SortedKeys of
        [FirstKey | _] -> FirstKey;
        [] -> <<"unknown">>
    end.

balance(_, RawReq, NodeMsg) ->
    ensure_started(),
    ServerRef = get(server_id),
    Req = hb_ao:get(<<"request">>, RawReq, NodeMsg),
    ClientIP = case Req of
        not_found ->
            hb_ao:get(<<"client-ip">>, RawReq, <<"0.0.0.0">>, NodeMsg);
        _ ->
            hb_ao:get(<<"client-ip">>, Req, <<"0.0.0.0">>, NodeMsg)
    end,

    ReferenceId = gen_reference(RawReq, Req, NodeMsg),
    ?event({balance_ref, ReferenceId}),

    %% Atomically get or initialize balance with single reference
    {ok, Balance, _Status} = gen_server:call(?MODULE, {get_or_init_balance, ClientIP, ReferenceId, ServerRef}),
    {ok, Balance}.

charge(_, RawReq, NodeMsg) ->
    ensure_started(),
    ServerRef = get(server_id),
    Req = hb_ao:get(<<"request">>, RawReq, NodeMsg),
    ReferenceId = gen_reference(RawReq, Req, NodeMsg),
    ?event({charge_ref, ReferenceId}),

    case gen_server:call(?MODULE, {get_ip_by_reference, ReferenceId, ServerRef}) of
        not_found ->
            ?event({charge_error, no_reference_found, {reference_id, ReferenceId, RawReq, Req}}),
            {error, no_reference};
        ClientIP ->
            %% ?event({charge_client_ip, ClientIP}),
            case gen_server:call(?MODULE, {decrement_balance, ClientIP, ServerRef}) of
                {ok, NewBalance} ->
                    %% ?event({charge_success, {new_balance, NewBalance}}),
                    {ok, true};
                {error, insufficient_balance} ->
                    %% ?event({charge_insufficient_balance, ClientIP}),
                    {error, insufficient_balance}
            end
    end.






test_opts() ->
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"rate-limit@1.0">>,
            <<"pricing-device">> => <<"rate-limit@1.0">>
        },
    {
        Address,
        Wallet,
        #{
            rate_limit_ledger => #{
                <<"balances">> => #{},
                <<"ip_references">> => #{}
            },
            rate_limit_window => <<"10s">>,
            rate_limit_balance => 100,
            operator => Address,
            on => #{
                <<"request">> => ProcessorMsg,
                <<"response">> => ProcessorMsg
            }
        }
    }.

rate_limit_ledger_test() ->
    ClientWallet = ar_wallet:new(),
    {_HostAddress, _HostWallet, Opts} = test_opts(),
    Node = hb_http_server:start_node(Opts),

    {ok, Res} =
        hb_http:get(
            Node,
            _Req = hb_message:commit(
                #{<<"path">> => <<"/~rate-limit@1.0/balance">>},
                Opts#{ priv_wallet => ClientWallet }
            ),
            Opts
        ),
    ?assertEqual(100, Res),

    {ok, Res2} =
        hb_http:get(
            Node,
            _Req2 = hb_message:commit(
                #{<<"path">> => <<"/~rate-limit@1.0/balance">>},
                Opts#{ priv_wallet => ClientWallet }
            ),
            Opts
        ),
    ?assertEqual(99, Res2).

concurrent_charge_test() ->
    ClientWallet = ar_wallet:new(),
    {_HostAddress, _HostWallet, Opts} = test_opts(),
    Node = hb_http_server:start_node(Opts),

    NumRequests = 15,
    Parent = self(),
    Pids = [spawn(fun() ->
        Result = hb_http:post(
            Node,
            hb_message:commit(
                #{<<"path">> => <<"/~rate-limit@1.0/balance">>},
                Opts#{ priv_wallet => ClientWallet }
            ),
            Opts
        ),
        Parent ! {self(), done, Result}
    end) || _ <- lists:seq(1, NumRequests)],

    [receive {Pid, done, Result} -> Result end || Pid <- Pids],

    {ok, FinalBalance} =
        hb_http:get(
            Node,
            hb_message:commit(
                #{<<"path">> => <<"/~rate-limit@1.0/balance">>},
                Opts#{ priv_wallet => ClientWallet }
            ),
            Opts
        ),

    ?event({concurrent_test_final_balance, FinalBalance}).
    %% ?assertEqual(50, FinalBalance).