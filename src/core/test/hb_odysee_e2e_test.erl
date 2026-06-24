%%% @doc End-to-end HTTP test for the Odysee upload path. Starts a live node
%%% configured with the `~auth-hook@1.0' request hook (secret-provider
%%% `~odysee-auth@1.0') and drives a `publish' over the wire, so an inbound
%%% `cookie' header travels the full hb_http path (through the `~cookie@1.0'
%%% codec into priv/cookie before the hook runs) -- exercising
%%% `dev_odysee_auth's priv/cookie branch and the write loop end-to-end.
%%%
%%% What this proves over real HTTP: a `publish' POST returns a `content-id' and
%%% the stored HB-native object reads back by that id with its bytes intact.
%%%
%%% What is intentionally NOT asserted here (proven in-process instead, and/or
%%% entangled with HTTP infrastructure rather than the Odysee code -- see
%%% aidocs/015):
%%%   - Attributing a request signature to the cookie: the hb_http client signs
%%%     outbound requests itself, which confounds a non-operator-signer check.
%%%     Cookie -> committed / no-cookie -> pass-through is proven in-process by
%%%     `hb_odysee_auth_test'.
%%%   - Cross-request signer determinism over HTTP: depends on `~secret@1.0'
%%%     wallet persistence (node config). Secret determinism is proven in-process.
%%%   - Re-verifying the wire-re-encoded read-back: a codec round-trip concern;
%%%     committed-object verification is proven in-process by
%%%     `hb_odysee_device_test'.
%%%   - Gating publish on a client signature: must live at the hook/`~meta@1.0'
%%%     layer, not in the device (the client commitment is carried on the
%%%     request singleton, not the device handler's `Base').
%%%
%%% Offline: ephemeral port, real wallet, fresh volatile store, no network.
-module(hb_odysee_e2e_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc The `on/request' hook wiring `~odysee-auth@1.0' as the secret-provider of
%% `~auth-hook@1.0', gated on the presence of a `cookie' or `authorization' key.
hook() ->
    #{
        <<"device">> => <<"auth-hook@1.0">>,
        <<"path">> => <<"request">>,
        <<"when">> => #{ <<"keys">> => [<<"cookie">>, <<"authorization">>] },
        <<"secret-provider">> =>
            #{
                <<"device">> => <<"odysee-auth@1.0">>,
                <<"access-control">> => #{ <<"device">> => <<"odysee-auth@1.0">> }
            }
    }.

publish_over_http_round_trips_test() ->
    ServerWallet = ar_wallet:new(),
    Node =
        hb_http_server:start_node(#{
            <<"port">> => 0,
            <<"priv-wallet">> => ServerWallet,
            <<"on">> => #{ <<"request">> => hook() },
            <<"store">> =>
                [hb_test_utils:test_store(hb_store_volatile, <<"odysee-e2e">>)]
        }),
    Cookie = <<"auth_token=odysee-publisher">>,
    Body = <<"hello odysee native upload">>,
    % Publish over HTTP, carrying an Odysee cookie (travels through the cookie
    % codec into priv/cookie before the hook runs).
    {ok, PubResp} =
        hb_http:post(
            Node,
            <<"/~odysee@1.0/publish">>,
            #{ <<"body">> => Body,
               <<"content-type">> => <<"text/plain">>,
               <<"cookie">> => Cookie },
            #{}
        ),
    ?event({publish_over_http, {resp, PubResp}}),
    ?assertEqual(200, hb_maps:get(<<"status">>, PubResp, undefined, #{})),
    ContentID = hb_maps:get(<<"content-id">>, PubResp, undefined, #{}),
    ?assert(?IS_ID(ContentID)),
    % Read the published object back by its content id over HTTP; the bytes and
    % the HB-native provenance marker round-trip.
    {ok, ReadBack} = hb_http:get(Node, <<"/", ContentID/binary>>, #{}),
    Loaded = hb_cache:ensure_all_loaded(ReadBack, #{}),
    ?assertEqual(Body, hb_maps:get(<<"body">>, Loaded, undefined, #{})),
    ?assertEqual(
        <<"hb-native-signed">>,
        hb_maps:get(<<"provenance">>, Loaded, undefined, #{})
    ).

%% @doc The hook-layer publish gate over real HTTP: with the `~odysee-publish-gate@1.0'
%% request hook ahead of the auth hook, a `publish' POST WITHOUT an Odysee cookie
%% is rejected 401 before resolution, while a POST WITH a cookie passes the gate,
%% is signed by the auth hook, and succeeds with a content-id. This is the
%% correct layer for "uploads require auth" (credential presence on the request
%% singleton, not an in-device signer check).
publish_gate_requires_cookie_over_http_test() ->
    ServerWallet = ar_wallet:new(),
    Node =
        hb_http_server:start_node(#{
            <<"port">> => 0,
            <<"priv-wallet">> => ServerWallet,
            <<"on">> => #{ <<"request">> => [gate(), hook()] },
            <<"store">> =>
                [hb_test_utils:test_store(hb_store_volatile, <<"odysee-e2e-gate">>)]
        }),
    PublishMsg =
        fun(Extra) ->
            maps:merge(
                #{ <<"body">> => <<"gated bytes">>,
                   <<"content-type">> => <<"text/plain">> },
                Extra
            )
        end,
    % No cookie -> the gate rejects before the upload runs.
    NoCookie =
        hb_http:post(Node, <<"/~odysee@1.0/publish">>, PublishMsg(#{}), #{}),
    ?event({publish_gate_no_cookie, {resp, NoCookie}}),
    ?assertMatch({error, #{ <<"status">> := 401 }}, NoCookie),
    % With a cookie -> the gate passes and the upload succeeds.
    {ok, WithCookie} =
        hb_http:post(
            Node,
            <<"/~odysee@1.0/publish">>,
            PublishMsg(#{ <<"cookie">> => <<"auth_token=odysee-gated-publisher">> }),
            #{}
        ),
    ?assertEqual(200, hb_maps:get(<<"status">>, WithCookie, undefined, #{})),
    ?assert(?IS_ID(hb_maps:get(<<"content-id">>, WithCookie, undefined, #{}))).

%% @doc The publish-gate request hook, wired ahead of the auth hook.
gate() ->
    #{ <<"device">> => <<"odysee-publish-gate@1.0">>, <<"path">> => <<"request">> }.
