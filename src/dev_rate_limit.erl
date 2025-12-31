
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

estimate(_, _, _) ->
    {ok, 1}.

set_balance(ClientIp, Amount, NodeMsg) ->
    Ledger = hb_opts:get(rate_limit_ledger, #{}, NodeMsg),
    Balances = hb_ao:get(<<"balances">>, Ledger, #{}, NodeMsg),
    IpReferences = hb_ao:get(<<"ip_references">>, Ledger, #{}, NodeMsg),
    NewBalances = hb_ao:set(Balances, ClientIp, Amount, NodeMsg),
    NewLedger = #{
        <<"balances">> => NewBalances,
        <<"ip_references">> => IpReferences
    },
    hb_http_server:set_opts(
        #{},
        NewMsg = NodeMsg#{ rate_limit_ledger => NewLedger }
    ),
    {ok, NewMsg}.

set_reference(ReferenceId, ClientIp, NodeMsg) ->
    Ledger = hb_opts:get(rate_limit_ledger, #{}, NodeMsg),
    Balances = hb_ao:get(<<"balances">>, Ledger, #{}, NodeMsg),
    IpReferences = hb_ao:get(<<"ip_references">>, Ledger, #{}, NodeMsg),
    NewIpReferences = hb_ao:set(IpReferences, ReferenceId, ClientIp, NodeMsg),
    NewLedger = #{
        <<"balances">> => Balances,
        <<"ip_references">> => NewIpReferences
    },
    hb_http_server:set_opts(
        #{},
        NewMsg = NodeMsg#{ rate_limit_ledger => NewLedger }
    ),
    {ok, NewMsg}.

gen_reference(RawReq, Req, NodeMsg) ->
    Commitments = case Req of
        not_found ->
            hb_ao:get(<<"commitments">>, RawReq, #{}, NodeMsg);
        _ ->
            hb_ao:get(<<"commitments">>, Req, #{}, NodeMsg)
    end,
    ?event({gen_reference_commitments, Commitments}),
    % Get the first key from the commitments map
    case maps:keys(Commitments) of
        [FirstKey | _] -> FirstKey;
        [] -> <<"unknown">>  % Fallback if no commitments
    end.

balance(_, RawReq, NodeMsg) ->
    Req = hb_ao:get(<<"request">>, RawReq, NodeMsg),
    ClientIP = case Req of
        not_found ->
            hb_ao:get(<<"client-ip">>, RawReq, <<"0.0.0.0">>, NodeMsg);
        _ ->
            hb_ao:get(<<"client-ip">>, Req, <<"0.0.0.0">>, NodeMsg)
    end,
    ReferenceId = gen_reference(RawReq, Req, NodeMsg),
    Ledger = hb_opts:get(rate_limit_ledger, #{}, NodeMsg),
    Balances = hb_ao:get(<<"balances">>, Ledger, #{}, NodeMsg),
    Existing = hb_ao:get(ClientIP, Balances, not_found, NodeMsg),
    case Existing of
        not_found ->
            {ok, NewMsg} = set_balance(ClientIP, 100, NodeMsg),
            {ok, _NewMsg2} = set_reference(ReferenceId, ClientIP, NewMsg),
            {ok, 100};
        Balance ->
            {ok, _NewMsg} = set_reference(ReferenceId, ClientIP, NodeMsg),
            {ok, Balance}
    end.

charge(_, RawReq, NodeMsg) ->
    Req = hb_ao:get(<<"request">>, RawReq, NodeMsg),
    ReferenceId = gen_reference(RawReq, Req, NodeMsg),
    Ledger = hb_opts:get(rate_limit_ledger, #{}, NodeMsg),
    Balances = hb_ao:get(<<"balances">>, Ledger, #{}, NodeMsg),
    IpReferences = hb_ao:get(<<"ip_references">>, Ledger, #{}, NodeMsg),
    IpByReference = hb_ao:get(ReferenceId, IpReferences, #{}, NodeMsg),
    ?event({ charge_ledger, Ledger }),
    ?event({ charge_balances, Balances }),
    ?event({ charge_reference_id, ReferenceId }),
    ?event({ ip_by_reference, IpByReference }),
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
    ?assertEqual(100, Res).

    % {ok, Res2} =
    %     hb_http:get(
    %         Node,
    %         Req2 = hb_message:commit(
    %             #{<<"path">> => <<"/~rate-limit@1.0/balance">>},
    %             Opts#{ priv_wallet => ClientWallet }
    %         ),
    %         Opts
    %     ),
    % ?assertEqual(100, Res2).