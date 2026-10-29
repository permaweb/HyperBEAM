%%% @doc Bounded certificate exchange and signed DNS-01 publication for TLS.
%%% Peer URLs and certificate trust come only from the node's TLS configuration.
%%% Peers supply public chains, never keys or trusted generation numbers.
-module(dev_tls_peer).
-export([best/4, publish/4]).
-include("include/hb.hrl").

%% @doc Keep the valid chain with the greatest signed expiry, including our own.
best(Current, TLS, Wallet, Opts) ->
    lists:foldl(fun(Peer, Best) ->
        try
            {ok, 200, _, PEM} = request(Peer, <<"/~tls@1.0/certificate">>,
                <<"GET">>, #{}, Opts),
            {ok, Chain} = hb_tls:certificate_chain(PEM),
            ok = hb_tls:validate(Wallet, Chain, TLS),
            case Best =:= [] orelse hb_tls:certificate_expiry(Chain)
                    > hb_tls:certificate_expiry(Best) of
                true -> Chain;
                false -> Best
            end
        catch _:_ -> Best
        end
    end, Current, maps:get(<<"peers">>, TLS, [])).

%% @doc Publish a fixed-expiry DNS record to every configured authority.
%% The same signed record can be retried but cannot acquire a later expiry.
publish(Action, TLS, Wallet, Opts) ->
    ACME = maps:get(<<"acme">>, TLS, #{}),
    Peers = maps:get(<<"dns-peers">>, ACME, []),
    Request = hb_message:commit(Action#{
        <<"path">> => <<"/~tls@1.0/challenge">>,
        <<"method">> => <<"POST">>,
        <<"type">> => <<"TLS-DNS-Challenge">>
    }, Opts#{ <<"priv-wallet">> => Wallet }),
    Results = [request(Peer, <<"/~tls@1.0/challenge">>,
        <<"POST">>, Request, Opts) || Peer <- Peers],
    case lists:all(fun
            ({ok, 200, _, _}) -> true;
            (_) -> false
        end, Results) of
        true -> ok;
        false ->
            ?event(tls, {dns_publication_failed, [case R of
                {ok, Status, _, _} -> Status;
                {error, Reason} -> Reason
            end || R <- Results]}),
            {error, 'tls-dns-publication-failed'}
    end.

%% @doc Use the normal message codec with a bounded, non-redirecting transport.
request(Peer, Path, Method, Message, Opts) ->
    #{scheme := Scheme, host := _} = URI = uri_string:parse(Peer),
    true = lists:member(Scheme, [<<"https">>, <<"http">>]),
    false = maps:is_key(userinfo, URI) orelse maps:is_key(query, URI)
        orelse maps:is_key(fragment, URI),
    true = lists:member(maps:get(path, URI, <<>>), [<<>>, <<"/">>]),
    {Headers, Body} = case Method of
        <<"GET">> -> {#{ <<"accept-bundle">> => <<"false">> }, <<>>};
        <<"POST">> ->
            {#{ <<"content-type">> => hb_http:media_type(<<"flat@1.0">>),
                <<"accept-bundle">> => <<"false">> },
                hb_http:encode(<<"flat@1.0">>, Message, true, Opts)}
    end,
    hb_http_client:request(#{
        peer => Peer, path => Path, method => Method,
        headers => Headers, body => Body, limit => 65536
    }, #{
        <<"http-client">> => gun,
        <<"protocol">> => http1,
        <<"http-retry">> => 0,
        <<"http-client-connect-timeout">> => 3000,
        <<"http-client-send-timeout">> => 5000
    }).
