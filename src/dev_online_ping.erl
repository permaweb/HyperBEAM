%%--------------------------------------------------------------------
%% Device: online-ping@1.0
%% Public API:  /~online-ping@1.0/ping-once
%% Pattern:     info/1 (atom keys) + call/3 dispatcher + ping_once/3
%%--------------------------------------------------------------------
-module(dev_online_ping).

-export([info/1, call/3, ping_once/3]).

%% =========================
%% info/1 — atom-key lookups
%% (all clauses contiguous; semicolons between; final period)
%% =========================
info(name)        -> <<"online-ping">>;
info(version)     -> <<"1.0">>;
%% keep public API hyphenated; router uses call/3 to map → ping_once/3
info(exports)     -> [info, 'ping-once'];
%% persistence/group used by hb_persistent
info(group)       -> <<"device:online-ping@1.0">>;
info(persistence) -> #{ group => <<"device:online-ping@1.0">> };
%% router/docs path index (public key stays hyphenated)
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
%% top-level map for UIs that call info(_)
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
%% call/3 — dispatch hyphenated methods
%% =========================
call(<<"ping-once">>, Msg, Ctx) -> ping_once(Msg, Ctx, #{});
call(_, _Msg, _Ctx)             -> {error, not_found}.

%% =========================
%% impl
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
            do_http_get(binary_to_list(UrlBin))
    end.

%% =========================
%% helpers
%% =========================
do_http_get(Url) when is_list(Url) ->
    ensure_inets_started(),
    {Micros, Result} =
        timer:tc(fun() ->
            httpc:request(get, {Url, []}, [{timeout, 5000}], [])
        end),
    Ms = Micros div 1000,
    case Result of
        {ok, {{_Vsn, Code, _Reason}, _Hdrs, _Body}} ->
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
        {error, _} -> ok;
        _          -> ok
    end.
