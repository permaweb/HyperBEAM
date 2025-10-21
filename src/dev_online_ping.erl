%%--------------------------------------------------------------------
%% HyperBEAM Device: online-ping@1.0
%% Public API:  /~online-ping@1.0/ping-once
%% Internals:   ping_once/3
%%--------------------------------------------------------------------
-module(dev_online_ping).

-export([info/1, info/2, ping_once/3]).

%%====================================================================
%% Public metadata (top-level)
%%====================================================================

%% Some HB code paths only call info/1. We include persistence here too.
info(_) ->
    #{
        <<"name">>        => <<"online-ping">>,
        <<"version">>     => <<"1.0">>,
        %% Public API methods — hyphenated (team rule: no underscores)
        exports           => [info, 'ping-once'],

        %% Persistence hints (binary keys)
        <<"persistence">> => #{ <<"group">> => <<"device:online-ping@1.0">> },
        <<"group">>       => <<"device:online-ping@1.0">>
    }.

%%====================================================================
%% Rich metadata by key (accept BOTH atom and binary keys)
%%====================================================================

%% name / version
info(<<"name">>,    _) -> <<"online-ping">>;
info(name,          _) -> <<"online-ping">>;
info(<<"version">>, _) -> <<"1.0">>;
info(version,       _) -> <<"1.0">>;

%% exports
info(<<"exports">>, _) -> [info, 'ping-once'];
info(exports,       _) -> [info, 'ping-once'];

%% persistence group (both shapes)
info(<<"persistence">>, _) -> #{ <<"group">> => <<"device:online-ping@1.0">> };
info(persistence,       _) -> #{ <<"group">> => <<"device:online-ping@1.0">> };
info(<<"group">>,       _) -> <<"device:online-ping@1.0">>;
info(group,             _) -> <<"device:online-ping@1.0">>;

%% paths — public key is hyphenated; we bind it to the internal function
info(<<"paths">>,  _) ->
    #{
      <<"ping-once">> => #{
         <<"desc">>   => <<"Send a single HTTP GET ping to the given URL and report status.">>,
         <<"params">> => #{
            <<"url">> => #{
               <<"type">>        => <<"string">>,
               <<"required">>    => true,
               <<"example">>     => <<"https://httpbin.org/status/200">>,
               <<"description">> => <<"Full URL to ping via HTTP GET.">>
            }
         },
         handler => ping_once
      }
    };
info(paths,        Ctx) -> info(<<"paths">>, Ctx);

%% examples (optional)
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
info(examples,       Ctx) -> info(<<"examples">>, Ctx);

%% catch-all
info(_, _) -> undefined.

%%====================================================================
%% Method implementation
%%====================================================================

%% Msg  := #{ <<"url">> := <<"...">> }
%% Ctx/Opts kept for interface parity with other devices
ping_once(Msg, _Ctx, _Opts) when is_map(Msg) ->
    case maps:get(<<"url">>, Msg, undefined) of
        undefined ->
            {error, #{
               <<"status">> => 400,
               <<"ok">>     => false,
               <<"error">>  => <<"missing 'url'">>
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
        {error, _} -> ok;  %% already started
        _          -> ok
    end.
