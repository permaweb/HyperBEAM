%%% @doc A device for returning the IP/host information of a requester or
%%% itself.
-module(dev_whois).
%%% Device API
-export([node/3, echo/3]).
%%% Public utilities
-export([ensure_host/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Return the calculated host information for the requester.
echo(_, Req, Opts) ->
    {ok, hb_maps:get(<<"ao-peer">>, Req, <<"unknown">>, Opts)}.

%% @doc Return the host information for the node. Sets the `host' key in the
%% node message if it is not already set.
node(_, _, Opts) ->
    case ensure_host(Opts) of
        {ok, NewOpts} ->
            {ok, hb_opts:get(host, <<"unknown">>, NewOpts)};
        Error ->
            Error
    end.

%% @doc Return the node message ensuring that the host is set. If it is not, we
%% attempt to find the host information from the specified bootstrap node.
ensure_host(Opts) ->
    case hb_opts:get(host, <<"unknown">>, Opts) of
        <<"unknown">> ->
            case bootstrap_node_echo(Opts) of
                {ok, Host} ->
                    % Set the host information in the persisted node message.
                    hb_http_server:set_opts(NewOpts = Opts#{ host => Host }),
                    {ok, NewOpts};
                Error ->
                    Error
            end;
        _ ->
            {ok, Opts}
    end.

%% @doc Find the local host information from the specified bootstrap node.
bootstrap_node_echo(Opts) ->
    case hb_opts:get(host_bootstrap_node, false, Opts) of
        false ->
            {error, <<"No bootstrap node configured.">>};
        BootstrapNode ->
            hb_http:get(BootstrapNode, <<"/~whois@1.0/echo">>, Opts)
    end.

%%% Tests

find_self_test() ->
    BoostrapNode =
        hb_http_server:start_node(#{
            priv_wallet => ar_wallet:new()
        }),
    PeerNode =
        hb_http_server:start_node(#{
            port => Port = rand:uniform(40000) + 10000,
            priv_wallet => ar_wallet:new(),
            host_bootstrap_node => BoostrapNode,
            http_client => httpc
        }),
    ?event({nodes, {peer, PeerNode}, {bootstrap, BoostrapNode}}),
    {ok, ReceivedPeerHost} = hb_http:get(PeerNode, <<"/~whois@1.0/node">>, #{}),
    ?event({find_self_test, ReceivedPeerHost}),
    ?assertEqual(<<"127.0.0.1:", (hb_util:bin(Port))/binary>>, ReceivedPeerHost).