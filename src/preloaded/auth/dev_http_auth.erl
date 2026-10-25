%%% @doc Implements a two-step authentication process for HTTP requests, using
%%% the `Basic' authentication scheme. This device is a viable implementation
%%% of the `generator' interface type employed by `~auth-hook@1.0', as well as
%%% the access-control interface used by `~secret@1.0'.
%%%
%%% `http-auth@1.0`'s `commit' key records the committer (the hash) of a secret
%%% key derived from the user's authentication information in the base message,
%%% and its `verify' key checks that the key derived from a request hashes to
%%% that committer. Callers may also use the `generate'
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
-module(dev_http_auth).
-device_libraries([lib_secret]).
-export([commit/3, verify/3]).
-export([generate/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The default salt to use for the PBKDF2 algorithm. This value must be
%% global across all nodes that intend to have a shared keyspace, although in
%% instances where this is not possible, users may specify non-standard salts
%% to the `/generate' path with the `salt' request key.
-define(DEFAULT_SALT, <<"constant:ao">>).

%% @doc The largest PBKDF2 iteration count and key length a request may demand,
%% bounding the work one unauthenticated request can cause. Nodes may override
%% them with the `http_auth_max_iterations' and `http_auth_max_key_length'
%% options.
-define(MAX_ITERATIONS, 5_000_000).
-define(MAX_KEY_LENGTH, 256).

%% @doc Generate or extract a secret and bind it to the base message by
%% recording its committer.
-spec commit(
    #{ _ => _ },
    #{
        secret => binary(),
        authorization => binary(),
        raw => boolean(),
        alg => atom(),
        salt => binary(),
        iterations => integer(),
        'key-length' => integer(),
        _ => _
    },
    #{ _ => _ }
) -> {ok, #{ _ => _ }} | {error, _}.
commit(Base, Req, Opts) ->
    case generate(Base, Req, Opts) of
        {ok, Key} ->
            Bound =
                Base#{
                    <<"committer">> => lib_secret:to_committer(Key)
                },
            ?event({commit_result, Bound}),
            {ok, Bound};
        {error, Err} ->
            {error, Err}
    end.

%% @doc Verify that the key derived from the request's authentication
%% information hashes to the committer recorded in the base message.
-spec verify(
    #{ _ => _ },
    #{
        secret => binary(),
        authorization => binary(),
        raw => boolean(),
        alg => atom(),
        salt => binary(),
        iterations => integer(),
        'key-length' => integer(),
        _ => _
    },
    #{ _ => _ }
) -> {ok, boolean()} | {error, #{ status := integer(), _ => _ }}.
verify(Base, RawReq, Opts) ->
    ?event({verify_invoked, {priv_base, Base}, {priv_req, RawReq}}),
    case generate(Base, RawReq, Opts) of
        {ok, Key} ->
            ?event({verify_found_key, {priv_key, Key}, {priv_base, Base}}),
            Committer = hb_maps:get(<<"committer">>, Base, undefined, Opts),
            Req = hb_cache:ensure_loaded(RawReq, Opts),
            VerifyRes =
                lib_secret:to_committer(Key) =:= Committer andalso
                    hb_committer:matches_commitment(Committer, Req, Opts),
            ?event({verify_result, VerifyRes}),
            {ok, VerifyRes};
        {error, Err} ->
            {error, Err}
    end.

%% @doc Collect authentication information from the client. If the `raw' flag
%% is set to `true', return the raw authentication information. Otherwise,
%% derive a key from the authentication information and return it.
-spec generate(
    #{ _ => _ },
    #{
        secret => binary(),
        authorization => binary(),
        raw => boolean(),
        alg => atom(),
        salt => binary(),
        iterations => integer(),
        'key-length' => integer(),
        _ => _
    },
    #{ _ => _ }
) -> {ok, binary()} | {error, #{ status := integer(), _ => _ }}.
generate(_Msg, ReqLink, Opts) when ?IS_LINK(ReqLink) ->
    generate(_Msg, hb_cache:ensure_loaded(ReqLink, Opts), Opts);
generate(_Msg, #{ <<"secret">> := Secret }, _Opts) ->
    {ok, Secret};
generate(_Msg, Req, Opts) ->
    case hb_maps:get(<<"authorization">>, Req, undefined, Opts) of
        <<"Basic ", Auth/binary>> ->
            Decoded = base64:decode(Auth),
            ?event(key_gen, {generated_key, {priv_auth, Auth}, {priv_decoded, Decoded}}),
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
    MaxIterations = hb_opts:get(http_auth_max_iterations, ?MAX_ITERATIONS, Opts),
    MaxKeyLength = hb_opts:get(http_auth_max_key_length, ?MAX_KEY_LENGTH, Opts),
    case Iterations =< MaxIterations andalso KeyLength =< MaxKeyLength of
        false ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"details">> =>
                        <<"PBKDF2 iterations or key-length above node maxima.">>
                }
            };
        true ->
            derive_key(Alg, Decoded, Salt, Iterations, KeyLength)
    end.

%% @doc Run PBKDF2 over the authentication information with the given
%% parameters and return the encoded key.
derive_key(Alg, Decoded, Salt, Iterations, KeyLength) ->
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

%% @doc A request whose PBKDF2 parameters exceed the node's maxima is refused
%% rather than run, bounding the work one request can demand.
excessive_pbkdf2_parameters_rejected_test() ->
    Auth = <<"Basic ", (base64:encode(<<"user:pass">>))/binary>>,
    Req =
        #{
            <<"path">> => <<"generate">>,
            <<"authorization">> => Auth,
            <<"iterations">> => 2000
        },
    ?assertMatch(
        {error, #{ <<"status">> := 400 }},
        hb_ao:resolve(
            #{ <<"device">> => <<"http-auth@1.0">> },
            Req,
            #{ <<"http-auth-max-iterations">> => 1000 }
        )
    ).

benchmark_pbkdf2_test() ->
    Key = crypto:strong_rand_bytes(32),
    Iterations = 2 * 600_000,
    KeyLength = 32,
    Derivations = 
        hb_test_utils:benchmark(
            fun() ->
                hb_crypto:pbkdf2(sha256, Key, <<"salt">>, Iterations, KeyLength)
            end,
            0.5
        ),
    hb_test_utils:benchmark_print(
        <<"Derived">>,
        <<"keys (1.2m iterations each)">>,
        Derivations
    ).
