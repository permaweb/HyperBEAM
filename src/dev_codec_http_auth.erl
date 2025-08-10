%%% @doc Implements a two-step authentication process for HTTP requests, using
%%% the `Basic' authentication scheme. This device is a viable implementation
%%% of the `generator' interface type employed by `~auth-hook@1.0', as well as
%%% the `~message@1.0' commitment scheme interface.
%%%
%%% `http-auth@1.0`'s `commit' and `verify' keys proxy to the `~httpsig@1.0'
%%% secret key HMAC commitment scheme, utilizing a secret key derived from the
%%% user's authentication information. Callers may also utilize the `generate'
%%% key directly to derive entropy from HTTP Authorization headers provided by
%%% the user. If no Authorization header is provided, the `generate' key will
%%% return a `401 Unauthorized` response, which triggers a recipient's browser
%%% to prompt the user for authentication details and resend the request.
%%% 
%%% The `generate' key derives secrets for it's users by calling PBKDF2 with
%%% the user's authentication information. The parameters for the PBKDF2
%%% algorithm are configurable, and can be specified in the request message:
%%% 
%%% <pre>
%%%   salt:       The salt to use for the PBKDF2 algorithm. Defaults to
%%%               `sha256("constant:ao")'.
%%%   iterations: The number of iterations to use for the PBKDF2 algorithm.
%%%               Defaults to `1,200,000'.
%%%   alg:        The hashing algorithm to use with PBKDF2. Defaults to
%%%               `sha256'.
%%%   key-length: The length of the key to derive from PBKDF2. Defaults to
%%%               `64'.
%%% </pre>
%%% 
%%% The default iteration count was chosen at two times the recommendation of
%%% OWASP in 2023 (600,000), and executes at a run rate of ~5-10 key derivations 
%%% per second on modern CPU hardware. Additionally, the default salt was chosen
%%% such that it is a public constant (needed in order for reproducibility 
%%% between nodes), and hashed in order to provide additional entropy, in 
%%% alignment with RFC 8018, Section 4.1.
-module(dev_codec_http_auth).
-export([commit/3, verify/3]).
-export([generate/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The default salt to use for the PBKDF2 algorithm. This value must be
%% global across all nodes that intend to have a shared keyspace, although in
%% instances where this is not possible, users may specify non-standard salts
%% to the `/generate' path with the `salt' request key.
-define(DEFAULT_SALT, <<"constant:ao">>).

%% @doc Generate or extract a new secret and commit to the message with the
%% `~httpsig@1.0/commit?type=hmac-sha256&scheme=secret' commitment mechanism.
commit(Base, Req, Opts) ->
    case generate(Base, Req, Opts) of
        {ok, Key} ->
            {ok, CommitRes} =
                dev_codec_httpsig_proxy:commit(
                    <<"http-auth@1.0">>,
                    Key,
                    Base,
                    Req,
                    Opts
                ),
            ?event({commit_result, CommitRes}),
            {ok, CommitRes};
        {error, Err} ->
            {error, Err}
    end.

%% @doc Verify a given `Base' message with a derived `Key' using the
%% `~httpsig@1.0' secret key HMAC commitment scheme.
verify(Base, RawReq, Opts) ->
    ?event({verify_invoked, {base, Base}, {req, RawReq}}),
    {ok, Key} = generate(Base, RawReq, Opts),
    ?event({verify_found_key, {key, Key}, {base, Base}, {req, RawReq}}),
    {ok, VerifyRes} =
        dev_codec_httpsig_proxy:verify(
            Key,
            Base,
            RawReq,
            Opts
        ),
    ?event({verify_result, VerifyRes}),
    {ok, VerifyRes}.

%% @doc Collect authentication information from the client. If the `raw' flag
%% is set to `true', return the raw authentication information. Otherwise,
%% derive a key from the authentication information and return it.
generate(_Msg, ReqLink, Opts) when ?IS_LINK(ReqLink) ->
    generate(_Msg, hb_cache:ensure_loaded(ReqLink, Opts), Opts);
generate(_Msg, #{ <<"secret">> := Secret }, _Opts) ->
    {ok, Secret};
generate(_Msg, Req, Opts) ->
    case hb_maps:get(<<"authorization">>, Req, undefined, Opts) of
        <<"Basic ", Auth/binary>> ->
            Decoded = base64:decode(Auth),
            ?event(key_gen, {generated_key, {auth, Auth}, {decoded, Decoded}}),
            case hb_maps:get(<<"raw">>, Req, false, Opts) of
                true -> {ok, Decoded};
                false -> derive_key(Decoded, Req, Opts)
            end;
        undefined ->
            {error,
                #{
                    <<"status">> => 401,
                    <<"www-authenticate">> => <<"Basic">>,
                    <<"details">> => <<"No authorization header provided.">>
                }
            };
        Unrecognized ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"details">> =>
                        <<"Unrecognized authorization header: ", Unrecognized/binary>>
                }
            }
    end.

%% @doc Derive a key from the authentication information using the PBKDF2
%% algorithm and user specified parameters.
derive_key(Decoded, Req, Opts) ->
    Alg = hb_util:atom(hb_maps:get(<<"alg">>, Req, <<"sha256">>, Opts)),
    Salt =
        hb_maps:get(
            <<"salt">>,
            Req,
            hb_crypto:sha256(?DEFAULT_SALT),
            Opts
        ),
    Iterations = hb_maps:get(<<"iterations">>, Req, 2 * 600_000, Opts),
    KeyLength = hb_maps:get(<<"key-length">>, Req, 64, Opts),
    ?event(key_gen,
        {derive_key,
            {alg, Alg},
            {salt, Salt},
            {iterations, Iterations},
            {key_length, KeyLength}
        }
    ),
    case hb_crypto:pbkdf2(Alg, Decoded, Salt, Iterations, KeyLength) of
        {ok, Key} ->
            EncodedKey = hb_util:encode(Key),
            {ok, EncodedKey};
        {error, Err} ->
            ?event(key_gen,
                {pbkdf2_error,
                    {alg, Alg},
                    {salt, Salt},
                    {iterations, Iterations},
                    {key_length, KeyLength},
                    {error, Err}
                }
            ),
            {error,
                #{
                    <<"status">> => 500,
                    <<"details">> => <<"Failed to derive key.">>
                }
            }
    end.

%%% Tests

benchmark_pbkdf2_test() ->
    Key = crypto:strong_rand_bytes(32),
    Iterations = 2 * 600_000,
    KeyLength = 32,
    Derivations = 
        hb_test_utils:benchmark(
            fun() ->
                hb_crypto:pbkdf2(sha256, Key, <<"salt">>, Iterations, KeyLength)
            end
        ),
    hb_test_utils:benchmark_print(
        <<"Derived">>,
        <<"keys (1.2m iterations each)">>,
        Derivations
    ).