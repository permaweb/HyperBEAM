%%% @doc A request-hook gate that requires an Odysee credential (a `cookie' or
%%% `authorization' header) on protected write paths (e.g. `publish'). It is
%%% configured as a request hook BEFORE `~auth-hook@1.0' -- as a hook list
%%% `"on": { "request": [ gate, auth-hook ] }' -- so that an unauthenticated
%%% write is rejected with 401 before resolution, while reads and authenticated
%%% writes pass through (the auth hook then signs the latter). The protected
%%% path substring is configurable via the hook base's `protect-path' key
%%% (default `publish').
%%%
%%% Why the gate lives at the hook layer (not in the device): the client
%%% commitment that `~auth-hook@1.0' produces is carried on the request
%%% singleton, NOT on the `Base' a device key handler receives, so an in-device
%%% signer check cannot see it (measured -- see aidocs/015). The gate therefore
%%% checks credential PRESENCE on the request singleton, which both the
%%% in-process hook contract and the real HTTP path expose (over HTTP the cookie
%%% is parsed into `priv/cookie' before the hook runs).
-module(dev_odysee_publish_gate).
-implements(<<"odysee-publish-gate@1.0">>).
-export([request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Path substring that marks a request as a protected (auth-required) write.
-define(DEFAULT_PROTECT_PATH, <<"publish">>).

%% @doc The on-request hook entry point. Rejects a protected-path request that
%% carries no Odysee credential with 401; otherwise passes the hook request
%% through unchanged so the next handler (the auth hook) can run.
request(Base, HookReq, Opts) ->
    Request = hb_maps:get(<<"request">>, HookReq, #{}, Opts),
    case is_protected(Base, Request, Opts) andalso not has_credential(Request, Opts) of
        true ->
            ?event(odysee_gate, {publish_rejected_no_credential}, Opts),
            {error,
                #{
                    <<"status">> => 401,
                    <<"body">> => <<"odysee-publish-requires-auth">>
                }
            };
        false ->
            {ok, HookReq}
    end.

%% @doc A request is protected when its path contains the configured substring.
is_protected(Base, Request, Opts) ->
    Pattern = hb_maps:get(<<"protect-path">>, Base, ?DEFAULT_PROTECT_PATH, Opts),
    case hb_util:bin(hb_maps:get(<<"path">>, Request, <<>>, Opts)) of
        <<>> -> false;
        Path -> binary:match(Path, Pattern) =/= nomatch
    end.

%% @doc An Odysee credential is present as a raw `cookie'/`authorization' header
%% (in-process path) or as the parsed cookie under `priv/cookie' (real HTTP path,
%% where the cookie codec has already reshaped the inbound header).
has_credential(Request, Opts) ->
    has_raw(Request, <<"cookie">>, Opts)
        orelse has_raw(Request, <<"authorization">>, Opts)
        orelse has_priv_cookie(Request, Opts).

has_raw(Request, Key, Opts) ->
    case hb_maps:get(Key, Request, undefined, Opts) of
        Value when is_binary(Value), Value =/= <<>> -> true;
        _ -> false
    end.

has_priv_cookie(Request, Opts) ->
    case hb_private:get(<<"cookie">>, Request, #{}, Opts) of
        Map when is_map(Map), map_size(Map) > 0 -> true;
        _ -> false
    end.
