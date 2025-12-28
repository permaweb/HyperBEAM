
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
-export([balance/3, charge/3, estimate/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The cost is always 1
estimate(_Base, EstimateReq, NodeMsg) ->
    ?event(rate_limit_estimate, { EstimateReq }),
    {ok, 1}.

set_balance(ClientIp, Amount, NodeMsg) ->
    Ledger = hb_opts:get(rate_limit_ledger, #{}, NodeMsg),
    ?event(rate_limit,
        {modifying_balance,
            {client_ip, ClientIp},
            {amount, Amount},
            {ledger_before, Ledger}
        }
    ),
    hb_http_server:set_opts(
        #{},
        NewMsg = NodeMsg#{
            rate_limit_ledger =>
                hb_ao:set(
                    Ledger,
                    ClientIp,
                    Amount,
                    NodeMsg
                )
        }
    ),
    {ok, NewMsg}.

balance(_, RawReq, NodeMsg) ->
    ?event(rate_limit_balance, { RawReq }),
    ClientIP = hb_ao:get(<<"client-ip">>, RawReq, <<"0.0.0.0">>, NodeMsg),
    Ledger = hb_opts:get(rate_limit_ledger, #{}, NodeMsg),
    Balance = hb_ao:get(ClientIP, Ledger, -1, NodeMsg),
    ?event(rate_limit, { balance, ClientIP, Balance }),
    case Balance of
        -1 ->
            {ok, 1};
        _ ->
            {ok, Balance}
    end.

charge(_, Req, NodeMsg) ->
    ClientIP = hb_ao:get(<<"client-ip">>, Req, <<"0.0.0.0">>, NodeMsg),
    {ok, NewMsg} = set_balance(ClientIP, 100, NodeMsg),
    Ledger = hb_opts:get(rate_limit_ledger, #{}, NewMsg),
    Balance = hb_ao:get(ClientIP, Ledger, 0, NewMsg),
    ?event(rate_limit, { modified_balance, ClientIP, Balance }),
    {ok, true}.

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
            rate_limit_ledger => #{},
            rate_limit_window => <<"10s">>,
            rate_limit_balance => 100,
            operator => Address,
            on => #{
                <<"request">> => ProcessorMsg,
                <<"response">> => ProcessorMsg
            }
        }
    }.

single_request_test() ->
    ClientWallet = ar_wallet:new(),
    {HostAddress, HostWallet, Opts} = test_opts(),
    Node = hb_http_server:start_node(Opts),

    {ok, Res} =
        hb_http:get(
            Node,
            Req = hb_message:commit(
                #{<<"path">> => <<"/~rate-limit@1.0/balance">>},
                Opts#{ priv_wallet => ClientWallet }
            ),
            Opts
        ),
    ?assertEqual(1, Res),

    {ok, Res2} =
        hb_http:get(
            Node,
            Req2 = hb_message:commit(
                #{<<"path">> => <<"/~rate-limit@1.0/balance">>},
                Opts#{ priv_wallet => ClientWallet }
            ),
            Opts
        ),
    ?assertEqual(100, Res2).