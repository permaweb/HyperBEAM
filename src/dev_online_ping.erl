%%--------------------------------------------------------------------
%% HyperBEAM Device: online-ping@1.0
%% Public API:  /~online-ping@1.0/ping-once
%% Internals:   ping_once/3 (maps from the hyphenated path)
%%--------------------------------------------------------------------
-module(dev_online_ping).

-export([info/1, info/2, ping_once/3]).

%%====================================================================
%% Public metadata
%%====================================================================

%% Minimal top-level info used by HB for discovery
info(_) ->
    #{
        <<"name">>    => <<"online-ping">>,
        <<"version">> => <<"1.0">>,
        %% Publicly exposed methods (hyphenated)
        exports       => [info, 'ping-once']
    }.

%% Rich info by key (arity 2, not 3)
info(<<"name">>,    _) -> <<"online-ping">>;
info(<<"version">>, _) -> <<"1.0">>;
info(<<"exports">>, _) -> [info, 'ping-once'];

%% Public paths the router exposes.
%% Keep the hyphenated key and explicitly bind to the internal handler.
info(<<"paths">>,  _) ->
    #{
      <<"ping-once">> => #{
         <<"desc">>   => <<"Send a single HTTP GET ping to the given URL and report status.">>,
         <<"params">> => #{
            <<"url">> => #{
               <<"type">>        => <<"string">>,
               <<"required">>    => true,
               <<"example">>     => <<"https://example.org/ping">>,
               <<"description">> => <<"Full URL to ping via HTTP GET.">>
            }
         },
         %% Explicit mapping from public path -> internal function
         handler => ping_once
      }
    };

%% Optional examples for self-doc UIs
info(<<"examples">>, _) ->
    [
      #{
        <<"title">> => <<"Ping a URL once">>,
        <<"call">>  => #{
           <<"device">> => <<"online-ping@1.0">>,
           <<"method">> => <<"ping-once">>,
           <<"msg">>    => #{ <<"url">> => <<"https://httpbin.org/status/200">> }
        }
      }
    ];

%% Catch-all
info(_, _) -> undefined.

%%====================================================================
%% Public method (internal implementation)
%%====================================================================

%% Msg  := #{ <<"url">> := <<"...">> }
%% Ctx  := runtime context (unused)
%% Opts := optional opts (unused)
ping_once(Msg, _Ctx, _Opts) when is_map(Msg) ->
    case maps:get(<<"url">>, Msg, undefined) of
        undefined ->
            {error, #{
               <<"status">> => 400,
               <<"ok">>      => false,
               <<"error">>   => <<"missing 'url'">>
            }};
        UrlBin when is_binary(UrlBin) ->
            Url = binary_to_list(UrlBin),
            do_http_ping(Url)
    end.

%%====================================================================
%% Helpers
%%====================================================================

do_http_ping(Url) when is_list(Url) ->
    ensure_inets_started(),
    {DurationMicros, Result} =
        timer:tc(fun() ->
            httpc:request(get, {Url, []}, [{timeout, 5000}], [])
        end),
    DurationMs = DurationMicros div 1000,
    case Result of
        {ok, {{_HttpVer, Code, _Reason}, _Headers, _Body}} ->
            Ok = (Code >= 200) andalso (Code =< 299),
            {ok, #{
                <<"status">>      => Code,
                <<"ok">>          => Ok,
                <<"duration_ms">> => DurationMs
            }};
        {error, Reason} ->
            {error, #{
                <<"status">>      => 599,
                <<"ok">>          => false,
                <<"error">>       => iolist_to_binary(io_lib:format("~p", [Reason])),
                <<"duration_ms">> => DurationMs
            }}
    end.

ensure_inets_started() ->
    case application:ensure_all_started(inets) of
        {ok, _}    -> ok;
        {error, _} -> ok;  %% already started or not needed
        _          -> ok
    end.
