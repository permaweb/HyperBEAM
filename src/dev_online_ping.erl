%%--------------------------------------------------------------------
%% Device: online-ping@1.0
%% Public API:  /~online-ping@1.0/ping-once
%% Pattern:     matches other working devices (info/1 + call/3 + group)
%%--------------------------------------------------------------------
-module(dev_online_ping).

-export([info/1, call/3, ping_once/3]).

%% =========================
%% info/1 — atom-key lookups
%% (ALL clauses contiguous; semicolons between, final period)
%% =========================
info(name)        -> <<"online-ping">>;
info(version)     -> <<"1.0">>;
info(exports)     -> [info, 'ping-once'];
%% Grouping for hb_persistent (atom keys)
info(group)       -> <<"device:online-ping@1.0">>;
info(persistence) -> #{ group => <<"device:online-ping@1.0">> };
%% Public paths for docs/routers that read info(paths)
info(paths) ->
    #{
      <<"ping-once">> => #{
        desc   => <<"Send a single HTTP GET to the given URL and report status.">>,
        params => #{
          url => #{
            type        => <<"string">>,
            required    => true,
            example     => <<"https://httpbin.org/status/200">>,
            description => <<"Full URL to ping via HTTP GET.">>
          }
        }
      }
    };
%% Top-level map (for UIs that call info(_)); keep atom keys consistent
info(_) ->
    #{
      name        => info(name),
      version     => info(version),
      exports     => info(exports),
      group       => info(group),
      persistence => info(persistence),
      paths       => info(paths)
    }.

%% =========================
%% call/3 — public dispatcher
%% =========================
call(<<"ping-once">>, Msg, Ctx) -> ping_once(Msg, Ctx, #{});
call(_, _Msg, _Ctx)             -> {error, not_found}.

%% =========================
%% Implementation
%% =========================
%% Msg = #{ <<"url">> := <<"...">> }
ping_once(Msg, _Ctx, _Opts) when is_map(Msg) ->
    case maps:get(<<"url">>, Msg, undefined) of
        undefined ->
            {error, #{
              <<"status">> => 400,
              <<"ok">>     => false,
              <<"error">>  => <<"missing 'url'">>
            }};
        UrlBin when is_binary(UrlBin) ->
            do_http_ping(binary_to_list(UrlBin))
    end.

%% =========================
%% Helpers
%% =========================
do_http_ping(Url) when is_list(Url) ->
    ensure_inets_started(),
    {Micros, Result} =
        timer:tc(fun() ->
            httpc:request(get, {Url, []}, [{timeout, 5000}], [])
        end),
    Ms = Micros div 1000,
    case Result of
        {ok, {{_V, Code, _Reason}, _Hdrs, _Body}} ->
            Ok = (Code >= 200) andalso (Code =< 299),
            {ok, #{
              <<"status">>      => Code,
              <<"ok">>          => Ok,
              <<"duration_ms">> => Ms
            }};
        {error, Reason} ->
            {error, #{
              <<"status">>      => 599,
              <<"ok">>          => false,
              <<"error">>       => iolist_to_binary(io_lib:format("~p", [Reason])),
              <<"duration_ms">> => Ms
            }}
    end.

ensure_inets_started() ->
    case application:ensure_all_started(inets) of
        {ok, _}    -> ok;
        {error, _} -> ok;  %% already started
        _          -> ok
    end.
