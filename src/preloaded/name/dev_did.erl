%%% @doc The device of the `did:' URI scheme. A DID has the form
%%% `did:<method>:<id>'. This device does not read any method itself: it is a
%%% proxy (see `hb_uri') for the device of the DID's method,
%%% `did-<method>@1.0'. That device reads `<id>' with its `from' key, and
%%% writes the whole DID with its `to' key.
-module(dev_did).
-export([from/3, to/3]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Loads the resource that a DID identifies, with the device of its
%% method.
-spec from(
    #{ body := binary(), _ => _ },
    #{ type := binary(), _ => _ },
    #{ _ => _ }
) -> {ok, _} | {error, _}.
from(#{ <<"body">> := Body }, Req = #{ <<"type">> := <<"uri">> }, Opts) ->
    hb_uri:proxy_from(<<"did">>, Body, Req, Opts).

%% @doc Writes the DID of a message whose device is `did-<method>@...'.
-spec to(
    #{ device := binary(), _ => _ },
    #{ type := binary(), _ => _ },
    #{ _ => _ }
) -> {ok, binary()} | {error, _}.
to(Msg, Req = #{ <<"type">> := <<"uri">> }, Opts) ->
    hb_uri:proxy_to(<<"did">>, Msg, Req, Opts).

%%% Tests

%% @doc A DID is read by the device of its method: `did:pkh:...' by
%% `did-pkh@1.0', and `did:web:...' by `did-web@1.0', which this node does
%% not have. `did-pkh@1.0' refuses an account that it does not give for any key.
method_device_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    ?assertMatch(
        {ok,
            #{
                <<"device">> := <<"did-pkh@1.0">>,
                <<"body">> :=
                    <<"eip155:1:0xB9C5714089478a327F09197987f16f9E5d936E8a">>
            }
        },
        hb_ao:resolve_many(
            [<<"did:pkh:eip155:1:0xB9C5714089478a327F09197987f16f9E5d936E8a">>],
            Opts
        )
    ),
    ?assertMatch(
        {error, #{ <<"error">> := <<"invalid-account">> }},
        hb_ao:resolve_many(
            [<<"did:pkh:eip155:1:0xb9c5714089478a327f09197987f16f9e5d936e8a">>],
            Opts
        )
    ),
    ?assertMatch(
        {error,
            #{
                <<"error">> := <<"unsupported-uri-scheme">>,
                <<"offender">> := <<"did-web">>
            }
        },
        hb_ao:resolve_many([<<"did:web:example.com">>], Opts)
    ).
