%%% @doc An Odysee-cookie secret-provider for the `~auth-hook@1.0' device. This
%%% device implements the `generator' interface type employed by `~auth-hook@1.0'
%%% (see [the auth hook](dev_auth_hook.html)), as well as the `~message@1.0'
%%% commitment scheme interface (`commit'/`verify') used by `~secret@1.0' to
%%% manage the wallet that is bound to a derived secret.
%%%
%%% The device derives a DETERMINISTIC secret from a user's Odysee session token,
%%% read from the `cookie' header (falling back to the `authorization' header).
%%% The same token always yields the same secret, such that requests carrying the
%%% same Odysee session are consistently signed by the same node-hosted wallet.
%%% This mirrors the `~http-auth@1.0' device's PBKDF2 derivation, but sources its
%%% entropy from the Odysee cookie rather than HTTP Basic credentials.
%%%
%%% The `generate' key reads the token and derives a key from it using PBKDF2.
%%% The parameters for the PBKDF2 algorithm are configurable via the request
%%% message:
%%%
%%% <pre>
%%%   salt:       The salt to use for the PBKDF2 algorithm. Defaults to
%%%               `sha256("constant:odysee")'.
%%%   iterations: The number of iterations to use for the PBKDF2 algorithm.
%%%               Defaults to `1,200,000'.
%%%   alg:        The hashing algorithm to use with PBKDF2. Defaults to
%%%               `sha256'.
%%%   key-length: The length of the key to derive from PBKDF2. Defaults to
%%%               `64'.
%%% </pre>
%%%
%%% If no Odysee token is present in either the `cookie' or `authorization'
%%% header, the `generate' key returns an error so the `~auth-hook@1.0' device
%%% leaves the request uncommitted (pass-through).
-module(dev_odysee_auth).
-implements(<<"odysee-auth@1.0">>).
-export([commit/3, verify/3]).
-export([generate/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The default salt to use for the PBKDF2 algorithm. As with
%% `~http-auth@1.0', this value must be global across all nodes that intend to
%% have a shared keyspace. It is a hashed public constant, in alignment with
%% RFC 8018, Section 4.1.
-define(DEFAULT_SALT, <<"constant:odysee">>).

%% @doc Generate or extract a new secret and commit to the message with the
%% `~httpsig@1.0/proxy-commit?type=hmac-sha256&scheme=secret' commitment
%% mechanism.
commit(Base, Req, Opts) ->
    case generate(Base, Req, Opts) of
        {ok, Key} ->
            {ok, CommitRes} =
                hb_ao:resolve(
                    #{ <<"device">> => <<"httpsig@1.0">> },
                    Req#{
                        <<"path">> => <<"proxy-commit">>,
                        <<"commitment-device">> => <<"odysee-auth@1.0">>,
                        <<"secret">> => Key,
                        <<"message">> => Base
                    },
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
    ?event({verify_invoked, {priv_base, Base}, {priv_req, RawReq}}),
    {ok, Key} = generate(Base, RawReq, Opts),
    ?event({verify_found_key, {priv_key, Key}, {priv_base, Base}, {priv_req, RawReq}}),
    {ok, VerifyRes} =
        hb_ao:resolve(
            #{ <<"device">> => <<"httpsig@1.0">> },
            RawReq#{
                <<"path">> => <<"proxy-verify">>,
                <<"secret">> => Key,
                <<"message">> => Base
            },
            Opts
        ),
    ?event({verify_result, VerifyRes}),
    {ok, VerifyRes}.

%% @doc Derive a deterministic secret from the user's Odysee token. The token is
%% read from the `cookie' header, falling back to the `authorization' header. If
%% the `secret' key is already present in the request (as set by
%% `~auth-hook@1.0' after a prior generation), it is returned directly. If no
%% token is present, an error is returned so that the hook leaves the request
%% uncommitted.
generate(_Msg, ReqLink, Opts) when ?IS_LINK(ReqLink) ->
    generate(_Msg, hb_cache:ensure_loaded(ReqLink, Opts), Opts);
generate(_Msg, #{ <<"secret">> := Secret }, _Opts) ->
    {ok, Secret};
generate(_Msg, Req, Opts) ->
    case token(Req, Opts) of
        {ok, Token} ->
            ?event(key_gen, {generating_key, {priv_token, Token}}),
            derive_key(Token, Req, Opts);
        {error, no_token} ->
            {error,
                #{
                    <<"status">> => 401,
                    <<"details">> =>
                        <<"No Odysee token provided in cookie or authorization "
                            "header.">>
                }
            }
    end.

%% @doc Read the Odysee token from the request. We first look in the `cookie'
%% header, falling back to the `authorization' header. Either source is used
%% verbatim as the password for the PBKDF2 derivation, so that the same token
%% always yields the same secret.
token(Req, Opts) ->
    case hb_maps:get(<<"cookie">>, Req, undefined, Opts) of
        Cookie when is_binary(Cookie), Cookie =/= <<>> ->
            {ok, Cookie};
        _ ->
            case hb_maps:get(<<"authorization">>, Req, undefined, Opts) of
                Auth when is_binary(Auth), Auth =/= <<>> ->
                    {ok, Auth};
                _ ->
                    {error, no_token}
            end
    end.

%% @doc Derive a key from the Odysee token using the PBKDF2 algorithm and user
%% specified parameters, mirroring `~http-auth@1.0'.
derive_key(Token, Req, Opts) ->
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
    case hb_crypto:pbkdf2(Alg, Token, Salt, Iterations, KeyLength) of
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
