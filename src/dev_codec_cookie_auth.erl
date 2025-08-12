%%% @doc Implements the `message@1.0' commitment interface for the `~cookie@1.0',
%%% as well as the `generator' interface type for the `~auth-hook@1.0' device.
%%% See the [cookie codec](dev_codec_cookie.html) documentation for more details.
-module(dev_codec_cookie_auth).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
-export([commit/3, verify/3]).
-export([generate/3, finalize/3]).

%% @doc Generate a new secret (if no `committer' specified), and use it as the
%% key for the `httpsig@1.0' commitment. If a `committer' is given, we search 
%% for it in the cookie message instead of generating a new secret. See the
%% module documentation of `dev_codec_cookie' for more details on its scheme.
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
    ?event({normalized_cookies_found, {secrets, Secrets}}),
    {
        ok,
        WithCookie#{
            <<"secret">> => Secrets
        }
    }.

%% @doc Finalize an `on-request' hook by adding the cookie to the chain of 
%% messages. The inbound request has the same structure as a normal `~hook@1.0'
%% on-request hook: The message sequence is the body of the request, and the
%% request is the request message.
finalize(Base, Request, Opts) ->
    ?event(debug_auth, {finalize, {base, Base}, {request, Request}}),
    maybe
        {ok, SignedMsg} ?= hb_maps:find(<<"request">>, Request, Opts),
        {ok, MessageSequence} ?= hb_maps:find(<<"body">>, Request, Opts),
        % Cookie auth adds set-cookie to response
        {ok, #{ <<"set-cookie">> := SetCookie }} =
            dev_codec_cookie:to(
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

%% @doc Generate a new secret (if no `committer' specified), and use it as the
%% key for the `httpsig@1.0' commitment. If a `committer' is given, we search 
%% for it in the cookie message instead of generating a new secret. See the
%% module documentation of `dev_codec_cookie' for more details on its scheme.
commit(Base, Request, RawOpts) when ?IS_LINK(Request) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    commit(Base, hb_cache:ensure_loaded(Request, Opts), Opts);
commit(Base, Req = #{ <<"secret">> := Secret }, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    commit(hb_cache:ensure_loaded(Secret, Opts), Base, Req, Opts);
commit(Base, Request, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    % Calculate the key to use for the commitment.
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
        {ok, Secret} -> commit(Secret, Base, Request, Opts);
        {error, Err} -> {error, Err}
    end.

%% @doc Given the secret key, commit the message and set the cookie. This 
%% function may be used by other devices via a direct module call, in order to
%% commit a message and set the given secret key in the cookie.
commit(Secret, Base, Request, Opts) ->
    {ok, CommittedMsg} =
        dev_codec_httpsig_proxy:commit(
            <<"cookie@1.0">>,
            Secret,
            Base,
            Request,
            Opts
        ),
    store_secret(Secret, CommittedMsg, Opts).

%% @doc Update the nonces for a given secret.
store_secret(Secret, Msg, Opts) ->
    CookieAddr = dev_codec_httpsig_keyid:secret_key_to_committer(Secret),
    % Create the cookie parameters, using the name as the key and the secret as
    % the value.
    {ok, Cookies} = dev_codec_cookie:extract(Msg, #{}, Opts),
    NewCookies = Cookies#{ <<"secret-", CookieAddr/binary>> => Secret },
    {ok, WithCookie} = dev_codec_cookie:store(Msg, NewCookies, Opts),
    {ok, WithCookie}.

%% @doc Verify the HMAC commitment with the key being the secret from the 
%% request cookies. We find the appropriate cookie from the cookie message by
%% the committer ID given in the request message.
verify(Base, ReqLink, RawOpts) when ?IS_LINK(ReqLink) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    verify(Base, hb_cache:ensure_loaded(ReqLink, Opts), Opts);
verify(Base, Req = #{ <<"secret">> := Secret }, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    ?event({verify_with_explicit_key, {base, Base}, {request, Req}}),
    dev_codec_httpsig_proxy:verify(
        hb_util:decode(Secret),
        Base,
        Req,
        Opts
    );
verify(Base, Request, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    ?event({verify_finding_key, {base, Base}, {request, Request}}),
    case find_secret(Request, Opts) of
        {ok, Secret} ->
            dev_codec_httpsig_proxy:verify(
                hb_util:decode(Secret),
                Base,
                Request,
                Opts
            );
        {error, Err} ->
            {error, Err}
    end.

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
        {ok, Cookie} ?= dev_codec_cookie:extract(Request, #{}, Opts),
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
        {ok, Cookie} ?= dev_codec_cookie:extract(Request, #{}, Opts),
        {ok, _Secret} ?= hb_maps:find(<<"secret-", Committer/binary>>, Cookie, Opts)
    else error -> {error, not_found}
    end.

%%% Tests

%% @doc Call the cookie codec's `commit' and `verify' functions directly.
directly_invoke_commit_verify_test() ->
    Base = #{ <<"test-key">> => <<"test-value">> },
    CommittedMsg =
        hb_message:commit(
            Base,
            #{},
            #{
                <<"commitment-device">> => <<"cookie@1.0">>
            }
        ),
    ?event({committed_msg, CommittedMsg}),
    ?assertEqual(1, length(hb_message:signers(CommittedMsg, #{}))),
    VerifyReq =
        apply_cookie(
            CommittedMsg#{
                <<"committers">> => hb_message:signers(CommittedMsg, #{})
            },
            CommittedMsg,
            #{}
        ),
    VerifyReqWithoutComms = hb_maps:without([<<"commitments">>], VerifyReq, #{}),
    ?event({verify_req_without_comms, VerifyReqWithoutComms}),
    ?assert(hb_message:verify(CommittedMsg, VerifyReqWithoutComms, #{})),
    ok.

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
    ?assertMatch(#{ <<"set-cookie">> := _ }, SetRes),
    Req = apply_cookie(#{ <<"path">> => <<"/~cookie@1.0/extract">> }, SetRes, #{}),
    {ok, Res} = hb_http:get(Node, Req, #{}),
    ?assertMatch(#{ <<"k1">> := <<"v1">>, <<"k2">> := <<"v2">> }, Res),
    ok.

%%% Test Helpers

%% @doc Takes the cookies from the `GenerateResponse' and applies them to the
%% `Target' message.
apply_cookie(NextReq, GenerateResponse, Opts) ->
    {ok, Cookie} = dev_codec_cookie:extract(GenerateResponse, #{}, Opts),
    {ok, NextWithParsedCookie} = dev_codec_cookie:store(NextReq, Cookie, Opts),
    {ok, NextWithCookie} =
        dev_codec_cookie:to(
            NextWithParsedCookie,
            #{ <<"format">> => <<"cookie">> },
            Opts
        ),
    NextWithCookie.