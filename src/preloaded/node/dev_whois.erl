%%% @doc A device for returning the IP/host information of a requester or
%%% itself.
-module(dev_whois).
%%% Device API
-export([node/3, echo/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Return the calculated host information for the requester.
-spec echo(#{ _ => _ }, #{ 'ao-peer' => binary(), _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }}.
echo(_, Req, Opts) ->
    {ok, #{
        <<"ao-result">> => <<"body">>,
        <<"body">> => hb_maps:get(<<"ao-peer">>, Req, <<"unknown">>, Opts),
        <<"content-type">> => <<"text/plain">>,
        <<"x-content-type-options">> => <<"nosniff">>
    }}.

%% @doc Return the host information for the node. Sets the `host' key in the
%% node message if it is not already set.
-spec node(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, binary()} | {error, _}.
node(_, _, Opts) ->
    case ensure_host(Opts) of
        {ok, NewOpts} ->
            {ok, hb_opts:get(node_host, <<"unknown">>, NewOpts)};
        Error ->
            Error
    end.

%% @doc Return the node message ensuring that the host is set. If it is not, we
%% attempt to find the host information from the specified bootstrap node.
ensure_host(Opts) ->
    case hb_opts:get(node_host, <<"unknown">>, Opts) of
        <<"unknown">> ->
            case bootstrap_node_echo(Opts) of
                {ok, Host} ->
                    % Set the host information in the persisted node message.
                    hb_http_server:update_opts(
                        fun(NodeMsg) ->
                            hb_http_server:set_opts(
                                NodeMsg#{ <<"node-host">> => Host }
                            )
                        end,
                        Opts
                    ),
                    {ok, Opts#{ <<"node-host">> => Host }};
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
            <<"priv-wallet">> => ar_wallet:new()
        }),
    PeerNode =
        hb_http_server:start_node(#{
            <<"port">> => Port = rand:uniform(40000) + 10000,
            <<"priv-wallet">> => ar_wallet:new(),
            <<"host-bootstrap-node">> => BoostrapNode,
            <<"http-client">> => httpc
        }),
    NodeMsg = hb_http_server:get_opts(),
    ?event({nodes, {peer, PeerNode}, {bootstrap, BoostrapNode}}),
    {ok, ReceivedPeerHost} = hb_http:get(PeerNode, <<"/~whois@1.0/node">>, #{}),
    ?event({find_self_test, ReceivedPeerHost}),
    ?assertEqual(<<"127.0.0.1:", (hb_util:bin(Port))/binary>>, ReceivedPeerHost),
    % Only `node-host' changes in the node message.
    ?assertEqual(
        NodeMsg,
        maps:remove(<<"node-host">>, hb_http_server:get_opts())
    ).
