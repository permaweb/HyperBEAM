%%% @doc Implements the access-control interface of the `~cookie@1.0' device,
%%% as used by `~secret@1.0', as well as the `generator' interface type for
%%% the `~auth-hook@1.0' device. A `commit' binds a secret to the base message
%%% by recording the secret's committer (its hash) and setting the secret in
%%% the caller's cookie. A `verify' checks that the caller presents a secret
%%% that hashes to the recorded committer. The node keeps only the
%%% committer, so it holds neither the secret nor a commitment that it
%%% cannot verify itself.
%%% See the [cookie codec](dev_cookie.html) documentation for more details.
-module(dev_cookie_auth).
-device_libraries([lib_secret]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
-export([commit/3, verify/3]).
-export([generate/3, finalize/3]).

%% @doc Generate a new secret (if no `committer' specified), and use it as the
%% key for the `httpsig@1.0' commitment. If a `committer' is given, we search 
%% for it in the cookie message instead of generating a new secret. See the
%% module documentation of `dev_cookie' for more details on its scheme.
-spec generate(
    #{ _ => _ },
    #{ committer => binary(), generator => _, _ => _ },
    map()
) -> term().
generate(Base, Request, Opts) ->
    {WithCookie, Secrets} =
        case find_secrets(Request, Opts) of
            [] ->
                {ok, GeneratedSecret} = generate_secret(Base, Request, Opts),
                {ok, Updated} = store_secret(GeneratedSecret, Request, Opts),
                {Updated, [GeneratedSecret]};
            FoundSecrets ->
                {Request, FoundSecrets}
        end,
    ?event({normalized_cookies_found, {priv_secrets, Secrets}}),
    {
        ok,
        WithCookie#{
            <<"secret">> => Secrets
        }
    }.

%% @doc Finalize an `on-request' hook by adding the cookie to the chain of 
%% messages. The inbound request has the same structure as a normal request
%% hook: The message sequence is the body of the request, and the request is
%% the request message.
-spec finalize(
    #{ _ => _ },
    #{ request := #{ _ => _ }, body := list(), _ => _ },
    map()
) -> term().
finalize(Base, Request, Opts) ->
    ?event(debug_auth, {finalize, {base, Base}, {request, Request}}),
    maybe
        {ok, SignedMsg} ?= hb_maps:find(<<"request">>, Request, Opts),
        {ok, MessageSequence} ?= hb_maps:find(<<"body">>, Request, Opts),
        % Cookie auth adds set-cookie to response
        {ok, #{ <<"set-cookie">> := SetCookie }} =
            dev_cookie:to(
                SignedMsg,
                #{ <<"format">> => <<"set-cookie">> },
                Opts
            ),
        {
            ok,
            MessageSequence ++
                [#{ <<"path">> => <<"set">>, <<"set-cookie">> => SetCookie }]
        }
    else error ->
        {error, no_request}
    end.

%% @doc Bind a secret to the base message and set it in the caller's cookie.
%% If no `committer' is given in the request, a new secret is generated;
%% otherwise the secret for that committer is found in the request's cookie.
%% See the
%% module documentation of `dev_cookie' for more details on its scheme.
-spec commit(
    #{ _ => _ },
    #{ secret => binary(), committer => binary(), generator => _, _ => _ },
    map()
) -> term().
commit(Base, Request, RawOpts) when ?IS_LINK(Request) ->
    Opts = dev_cookie:opts(RawOpts),
    commit(Base, hb_cache:ensure_loaded(Request, Opts), Opts);
commit(Base, #{ <<"secret">> := Secret }, RawOpts) ->
    Opts = dev_cookie:opts(RawOpts),
    bind_secret(hb_cache:ensure_loaded(Secret, Opts), Base, Opts);
commit(Base, Request, RawOpts) ->
    Opts = dev_cookie:opts(RawOpts),
    % Find or generate the secret to bind.
    SecretRes =
        case find_secret(Request, Opts) of
            {ok, RawSecret} ->
                {ok, RawSecret};
            {error, no_secret} ->
                generate_secret(Base, Request, Opts);
            {error, not_found} ->
                throw({error, <<"Necessary cookie not found in request.">>})
        end,
    case SecretRes of
        {ok, Secret} -> bind_secret(Secret, Base, Opts);
        {error, Err} -> {error, Err}
    end.

%% @doc Record the committer of the secret (its hash) in the base message and
%% set the secret in the caller's cookie. The node keeps only the committer,
%% from which the secret cannot be recovered, while `verify' requires the
%% caller to present the secret itself. Other devices may call this function
%% directly to bind a secret of their own.
bind_secret(Secret, Base, Opts) ->
    store_secret(
        Secret,
        Base#{ <<"committer">> => lib_secret:to_committer(Secret) },
        Opts
    ).

%% @doc Update the nonces for a given secret.
store_secret(Secret, Msg, Opts) ->
    CookieAddr = lib_secret:to_committer(Secret),
    % Create the cookie parameters, using the name as the key and the secret as
    % the value. The secret is a credential: page scripts may not read its
    % cookie, and browsers send it only over HTTPS, from this site's pages.
    {ok, Cookies} = dev_cookie:extract(Msg, #{}, Opts),
    NewCookies =
        Cookies#{
            secret_ref(CookieAddr) =>
                #{
                    <<"value">> => Secret,
                    <<"attributes">> =>
                        #{
                            <<"path">> => <<"/">>,
                            <<"samesite">> => <<"strict">>
                        },
                    <<"flags">> => [<<"httponly">>, <<"secure">>]
                }
        },
    {ok, WithCookie} = dev_cookie:store(Msg, NewCookies, Opts),
    {ok, WithCookie}.

%% @doc The cookie name of a committer's secret: a key, so it is lowercase.
secret_ref(Committer) ->
    hb_util:lowercase_key(<<"secret-", Committer/binary>>).

%% @doc Verify that the caller has the secret bound to the base message: the
%% secret must hash to the `committer' recorded by `commit'. The secret is taken
%% from the `secret' key of the request if present, and from the caller's cookie
%% for the committer otherwise.
-spec verify(
    #{ committer => binary(), _ => _ },
    #{ secret => binary(), _ => _ },
    map()
) -> {ok, boolean()} | {error, not_found}.
verify(Base, ReqLink, RawOpts) when ?IS_LINK(ReqLink) ->
    Opts = dev_cookie:opts(RawOpts),
    verify(Base, hb_cache:ensure_loaded(ReqLink, Opts), Opts);
verify(Base, Req = #{ <<"secret">> := Secret }, RawOpts) ->
    Opts = dev_cookie:opts(RawOpts),
    ?event({verify_with_explicit_key, {priv_base, Base}, {priv_request, Req}}),
    verify_secret(
        hb_cache:ensure_loaded(Secret, Opts),
        Req,
        hb_maps:get(<<"committer">>, Base, undefined, Opts),
        Opts
    );
verify(Base, Request, RawOpts) ->
    Opts = dev_cookie:opts(RawOpts),
    ?event({verify_finding_key, {priv_base, Base}, {priv_request, Request}}),
    maybe
        {ok, Committer} ?= hb_maps:find(<<"committer">>, Base, Opts),
        {ok, Secret} ?= find_secret(Committer, Request, Opts),
        verify_secret(
            Secret,
            hb_cache:ensure_loaded(Request, Opts),
            Committer,
            Opts
        )
    else
        error -> {error, not_found};
        {error, Err} -> {error, Err}
    end.

%% @doc Check that a secret hashes to the given committer and that the
%% commitment's `committer', if present, is the same committer.
verify_secret(Secret, Commitment, Committer, Opts) ->
    {ok,
        lib_secret:to_committer(Secret) =:= Committer andalso
            hb_committer:matches_commitment(Committer, Commitment, Opts)}.

%% @doc Generate a new secret key for the given request. The user may specify
%% a generator function in the request, which will be executed to generate the
%% secret key. If no generator is specified, the default generator is used.
%% A `generator` may be either a path or full message. If no path is present in
%% a generator message, the `generate` path is assumed.
generate_secret(_Base, Request, Opts) ->
    case hb_maps:get(<<"generator">>, Request, undefined, Opts) of
        undefined ->
            % If no generator is specified, use the default generator.
            case hb_opts:get(cookie_default_generator, <<"random">>, Opts) of
                <<"random">> ->
                    default_generator(Opts);
                Provider ->
                    execute_generator(Request#{<<"path">> => Provider}, Opts)
        end;
        Provider ->
            % Execute the user's generator function.
            execute_generator(Request#{<<"path">> => Provider}, Opts)
    end.

%% @doc Generate a new secret key using the default generator.
default_generator(_Opts) ->
    {ok, hb_util:encode(crypto:strong_rand_bytes(64))}.

%% @doc Execute a generator function. See `generate_secret/3' for more details.
execute_generator(GeneratorPath, Opts) when is_binary(GeneratorPath) ->
    hb_ao:resolve(GeneratorPath, Opts);
execute_generator(Generator, Opts) ->
    Path = hb_maps:get(<<"path">>, Generator, <<"generate">>, Opts),
    hb_ao:resolve(Generator#{ <<"path">> => Path }, Opts).

%% @doc Find all secrets in the cookie of a message.
find_secrets(Request, Opts) ->
    maybe
        {ok, Cookie} ?= dev_cookie:extract(Request, #{}, Opts),
        [
            hb_maps:get(SecretRef, Cookie, secret_unavailable, Opts)
        ||
            SecretRef = <<"secret-", _/binary>> <- hb_maps:keys(Cookie)
        ]
    else error -> []
    end.

%% @doc Find the secret key for the given committer, if it exists in the cookie.
find_secret(Request, Opts) ->
    maybe
        {ok, Committer} ?= hb_maps:find(<<"committer">>, Request, Opts),
        find_secret(Committer, Request, Opts)
    else error -> {error, no_secret}
    end.
find_secret(Committer, Request, Opts) ->
    maybe
        {ok, Cookie} ?= dev_cookie:extract(Request, #{}, Opts),
        {ok, _Secret} ?= hb_maps:find(secret_ref(Committer), Cookie, Opts)
    else error -> {error, not_found}
    end.

%%% Tests

%% @doc Bind a secret to a message with `commit', then `verify' the bound
%% message against the caller's cookie, a wrong secret, and no cookie at all.
commit_verify_test() ->
    Base =
        #{
            <<"device">> => <<"cookie@1.0">>,
            <<"test-key">> => <<"test-value">>
        },
    {ok, Bound} = hb_ao:resolve(Base, #{ <<"path">> => <<"commit">> }, #{}),
    ?event({bound_msg, Bound}),
    % The bound message records the committer and carries no commitment.
    #{ <<"committer">> := Committer } = Bound,
    ?assertEqual([], hb_message:signers(Bound, #{})),
    % Page scripts, other sites and plain HTTP do not get the secret's cookie.
    {ok, #{ <<"set-cookie">> := [SetCookie] }} =
        dev_cookie:to(Bound, #{ <<"format">> => <<"set-cookie">> }, #{}),
    ?assertMatch(
        [_, <<"path=/; samesite=strict; httponly; secure">>],
        binary:split(SetCookie, <<"\"; ">>)
    ),
    Stored = hb_private:reset(Bound),
    VerifyReq = apply_cookie(#{ <<"path">> => <<"verify">> }, Bound, #{}),
    ?assertEqual({ok, true}, hb_ao:resolve(Stored, VerifyReq, #{})),
    % A wrong secret under the same cookie name is refused.
    {ok, WrongReq} =
        dev_cookie:store(
            #{ <<"path">> => <<"verify">> },
            #{
                secret_ref(Committer) =>
                    hb_util:encode(crypto:strong_rand_bytes(64))
            },
            #{}
        ),
    ?assertEqual({ok, false}, hb_ao:resolve(Stored, WrongReq, #{})),
    % A request without the cookie is refused.
    ?assertEqual(
        {error, not_found},
        hb_ao:resolve(Stored, #{ <<"path">> => <<"verify">> }, #{})
    ).

%% @doc Set keys in a cookie and verify that they can be parsed into a message.
http_set_get_cookies_test() ->
    Node = hb_http_server:start_node(#{}),
    {ok, SetRes} =
        hb_http:get(
            Node,
            <<"/~cookie@1.0/store?k1=v1&k2=v2">>,
            #{}
        ),
    ?event(debug_cookie, {set_cookie_test, {set_res, SetRes}}),
    ?assertMatch(#{ <<"priv">> := #{ <<"cookie">> := _ } }, SetRes),
    Req = apply_cookie(#{ <<"path">> => <<"/~cookie@1.0/extract">> }, SetRes, #{}),
    {ok, Res} = hb_http:get(Node, Req, #{}),
    ?assertMatch(#{ <<"k1">> := <<"v1">>, <<"k2">> := <<"v2">> }, Res),
    ok.

%%% Test Helpers

%% @doc Takes the cookies from the `GenerateResponse' and applies them to the
%% `Target' message.
apply_cookie(NextReq, GenerateResponse, Opts) ->
    {ok, Cookie} = dev_cookie:extract(GenerateResponse, #{}, Opts),
    {ok, NextWithParsedCookie} = dev_cookie:store(NextReq, Cookie, Opts),
    {ok, NextWithCookie} =
        dev_cookie:to(
            NextWithParsedCookie,
            #{ <<"format">> => <<"cookie">> },
            Opts
        ),
    NextWithCookie.
