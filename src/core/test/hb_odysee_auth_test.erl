%%% @doc Offline tests for the `~odysee-auth@1.0' secret-provider device
%%% (`dev_odysee_auth'), wired as the secret-provider of `~auth-hook@1.0'.
%%%
%%% The tests assert that:
%%% <ol>
%%%   <li>A request carrying a known Odysee cookie becomes committed after the
%%%       hook runs.</li>
%%%   <li>The same cookie twice yields the identical derived key/secret
%%%       (determinism).</li>
%%%   <li>A request with no cookie is left uncommitted (pass-through).</li>
%%% </ol>
%%%
%%% All tests are offline: they use real wallets (`ar_wallet:new'), bind to an
%%% ephemeral port (`port => 0'), and never reach the network. The device is
%%% resolved by name from the build-signed preloaded store.
%%%
%%% The committed/uncommitted assertions drive the genuine `~auth-hook@1.0'
%%% handler in-process (the same `request'/`body' hook contract that
%%% `dev_meta:resolve_hook' constructs), so the raw `cookie' header reaches the
%%% provider's `generate' key as `~auth-hook@1.0' delivers it -- before the HTTP
%%% layer reshapes inbound cookies into the message's private element. A live
%%% node is started so that the `~secret@1.0' device the hook calls to mint and
%%% reuse wallets can read and persist node options.
-module(hb_odysee_auth_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc The secret-provider hook base message wiring `~odysee-auth@1.0' as both
%% the secret-provider and the `~secret@1.0' access-control device, gated on the
%% presence of the `cookie' or `authorization' key.
hook_base() ->
    #{
        <<"device">> => <<"auth-hook@1.0">>,
        <<"path">> => <<"request">>,
        <<"when">> => #{
            <<"keys">> => [<<"cookie">>, <<"authorization">>]
        },
        <<"secret-provider">> =>
            #{
                <<"device">> => <<"odysee-auth@1.0">>,
                <<"access-control">> =>
                    #{ <<"device">> => <<"odysee-auth@1.0">> }
            }
    }.

%% @doc Start a live node and return its server id, so that the `~secret@1.0'
%% device the hook calls can read/persist node options. The server id is the
%% operator wallet's address (see `hb_http_server:new_server'), which lets us
%% recover the live options from the Cowboy environment on each hook run.
start_live_node(ServerWallet) ->
    _Node = hb_http_server:start_node(#{
        <<"port">> => 0,
        <<"priv-wallet">> => ServerWallet
    }),
    ServerID = hb_util:human_id(ar_wallet:to_address(ServerWallet)),
    hb_http_server:set_proc_server_id(ServerID),
    ServerID.

%% @doc The current server-bound options for the live node.
server_opts(ServerID) ->
    hb_http_server:get_opts(#{ <<"http-server">> => ServerID }).

%% @doc Run the `~auth-hook@1.0' request hook over a request, mirroring the
%% `request'/`body' shape that `dev_meta:resolve_hook' builds. Options are read
%% fresh from the live node so that a wallet minted by `~secret@1.0' on a prior
%% run is visible (and therefore reused) on the next.
run_hook(Request, ServerID) ->
    hb_ao:resolve(
        hook_base(),
        #{
            <<"path">> => <<"request">>,
            <<"request">> => Request,
            <<"body">> => [Request]
        },
        server_opts(ServerID)
    ).

%% @doc A request carrying a known Odysee cookie becomes committed after the
%% hook runs, and the same cookie yields the identical signing wallet across
%% requests (determinism observed end-to-end through the hook + `~secret@1.0').
cookie_commits_request_test() ->
    ServerWallet = ar_wallet:new(),
    ServerID = start_live_node(ServerWallet),
    ServerAddress = hb_util:human_id(ar_wallet:to_address(ServerWallet)),
    Cookie = <<"auth_token=odysee-session-known">>,
    Request =
        #{
            <<"path">> => <<"hello">>,
            <<"body">> => <<"Test data">>,
            <<"cookie">> => Cookie
        },
    {ok, #{ <<"request">> := Signed }} = run_hook(Request, ServerID),
    Signers = client_signers(Signed, ServerAddress, server_opts(ServerID)),
    ?event({cookie_commits, {found_signers, Signers}}),
    ?assertEqual(1, length(Signers)),
    [Signer] = Signers,
    % A second request with the same cookie must be signed by the same address,
    % demonstrating the determinism of the derived secret end-to-end.
    Request2 = Request#{ <<"body">> => <<"Test data 2">> },
    {ok, #{ <<"request">> := Signed2 }} = run_hook(Request2, ServerID),
    ?assertEqual(
        [Signer],
        client_signers(Signed2, ServerAddress, server_opts(ServerID))
    ).

%% @doc The same cookie always derives the identical secret/key. Asserted by
%% invoking the device's `generate' key directly through AO-Core, twice, and
%% comparing the returned keys.
deterministic_secret_test() ->
    Provider = #{ <<"device">> => <<"odysee-auth@1.0">> },
    Cookie = <<"auth_token=odysee-session-deterministic">>,
    Req = #{ <<"path">> => <<"generate">>, <<"cookie">> => Cookie },
    {ok, Key1} = hb_ao:resolve(Provider, Req, #{}),
    {ok, Key2} = hb_ao:resolve(Provider, Req, #{}),
    ?event({deterministic_secret, {key1, Key1}, {key2, Key2}}),
    ?assert(is_binary(Key1)),
    ?assertEqual(Key1, Key2),
    % A different cookie must derive a different key.
    {ok, OtherKey} =
        hb_ao:resolve(
            Provider,
            #{
                <<"path">> => <<"generate">>,
                <<"cookie">> => <<"auth_token=different-session">>
            },
            #{}
        ),
    ?assertNotEqual(Key1, OtherKey).

%% @doc The `authorization' header is used as a fallback source for the Odysee
%% token when no `cookie' is present, and it too is derived deterministically.
authorization_fallback_test() ->
    Provider = #{ <<"device">> => <<"odysee-auth@1.0">> },
    Req =
        #{
            <<"path">> => <<"generate">>,
            <<"authorization">> => <<"Bearer odysee-token-abc">>
        },
    {ok, Key1} = hb_ao:resolve(Provider, Req, #{}),
    {ok, Key2} = hb_ao:resolve(Provider, Req, #{}),
    ?assert(is_binary(Key1)),
    ?assertEqual(Key1, Key2).

%% @doc The token delivered as a parsed cookie under `priv/cookie' (exactly as
%% the HTTP layer leaves it after running an inbound `cookie' header through the
%% `~cookie@1.0' codec, before the request hook runs) derives the SAME secret/key
%% as the equivalent raw `cookie' header. This proves cross-path determinism: the
%% same Odysee session yields the same node-hosted wallet whether the token
%% arrived in-process (raw header) or over real HTTP (parsed into `priv/cookie').
priv_cookie_matches_raw_cookie_test() ->
    Provider = #{ <<"device">> => <<"odysee-auth@1.0">> },
    Cookie = <<"auth_token=odysee-session-crosspath">>,
    % The raw-header path: the token arrives as the `cookie' header verbatim.
    {ok, RawKey} =
        hb_ao:resolve(
            Provider,
            #{ <<"path">> => <<"generate">>, <<"cookie">> => Cookie },
            #{}
        ),
    % The real-HTTP path: the `~cookie@1.0' codec has already parsed the inbound
    % `cookie' header, stripped the raw key, and stored the parsed token under
    % `priv/cookie'. We reproduce that exact post-parse shape directly (the
    % probed server shape: `priv => #{ <<"cookie">> => #{ <<"auth_token">> => ...}}'),
    % with no raw `cookie' key present.
    Reshaped =
        hb_private:set(
            #{ <<"path">> => <<"generate">> },
            <<"cookie">>,
            #{ <<"auth_token">> => <<"odysee-session-crosspath">> },
            #{}
        ),
    ?assertEqual(undefined, maps:get(<<"cookie">>, Reshaped, undefined)),
    {ok, PrivKey} = hb_ao:resolve(Provider, Reshaped, #{}),
    ?event({priv_cookie_crosspath, {raw_key, RawKey}, {priv_key, PrivKey}}),
    ?assert(is_binary(PrivKey)),
    ?assertEqual(RawKey, PrivKey),
    % A different session delivered via `priv/cookie' must derive a different key.
    OtherReshaped =
        hb_private:set(
            #{ <<"path">> => <<"generate">> },
            <<"cookie">>,
            #{ <<"auth_token">> => <<"odysee-session-other">> },
            #{}
        ),
    {ok, OtherKey} = hb_ao:resolve(Provider, OtherReshaped, #{}),
    ?assertNotEqual(PrivKey, OtherKey).

%% @doc A request with no cookie (and no authorization header) is left
%% uncommitted: the `when' condition does not match, so the hook passes the
%% request through unchanged and adds no client signature.
no_cookie_passthrough_test() ->
    ServerWallet = ar_wallet:new(),
    ServerID = start_live_node(ServerWallet),
    ServerAddress = hb_util:human_id(ar_wallet:to_address(ServerWallet)),
    Request =
        #{
            <<"path">> => <<"hello">>,
            <<"body">> => <<"Test data">>
        },
    {ok, Result} = run_hook(Request, ServerID),
    % The hook is a pass-through: the request body is returned unchanged and no
    % client wallet has signed it.
    Opts = server_opts(ServerID),
    Signed = hb_maps:get(<<"request">>, Result, Request, Opts),
    ?assertEqual(
        [],
        client_signers(Signed, ServerAddress, Opts)
    ).

%%% Test helpers

%% @doc Return the signers of a message, excluding the node's own operator
%% wallet (which is not relevant to whether the client request was committed by
%% the hook).
client_signers(Msg, ServerAddress, Opts) ->
    [
        Signer
    ||
        Signer <- hb_message:signers(Msg, Opts),
        Signer =/= ServerAddress
    ].
