%%% @doc An Odysee-cookie secret-provider for the `~auth-hook@1.0' device. This
%%% device implements the `generator' interface type employed by `~auth-hook@1.0'
%%% (see [the auth hook](dev_auth_hook.html)), as well as the `~message@1.0'
%%% commitment scheme interface (`commit'/`verify') used by `~secret@1.0' to
%%% manage the wallet that is bound to a derived secret.
%%%
%%% The device derives a DETERMINISTIC secret from a user's Odysee session token.
%%% The token is sourced from whichever of the following is present, in order:
%%% the raw `cookie' header, the raw `authorization' header, or the parsed cookie
%%% map that the HTTP layer stores under `priv/cookie'. The last source is the one
%%% that arrives over real HTTP: `hb_http' converts an inbound `cookie' header
%%% through the `~cookie@1.0' codec (`from'), stripping the raw key and storing
%%% the parsed cookie under `priv/cookie' BEFORE the `~auth-hook@1.0' request hook
%%% runs. Both the raw header and the parsed map are canonicalised to the SAME
%%% deterministic token string, so the same Odysee session yields the same secret
%%% regardless of which path delivered it.
%%%
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
%%% If no Odysee token is present in the `cookie' header, the `authorization'
%%% header, or the parsed `priv/cookie' map, the `generate' key returns an error
%%% so the `~auth-hook@1.0' device leaves the request uncommitted (pass-through).
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

%% @doc Read the Odysee token from the request. Sources are tried in order: the
%% raw `cookie' header, the raw `authorization' header, then the parsed cookie
%% map stored under `priv/cookie' by the HTTP layer. The raw `cookie' header and
%% the parsed `priv/cookie' map are both canonicalised through `canonical_token/2'
%% to the SAME deterministic token string, so a given Odysee session yields the
%% same derived secret whether it arrived in-process (raw header) or over real
%% HTTP (parsed into `priv/cookie' before the hook ran). The `authorization'
%% header is used verbatim, as it is never reshaped by the cookie codec.
token(Req, Opts) ->
    case hb_maps:get(<<"cookie">>, Req, undefined, Opts) of
        Cookie when is_binary(Cookie), Cookie =/= <<>> ->
            canonical_token(Cookie, Opts);
        _ ->
            case hb_maps:get(<<"authorization">>, Req, undefined, Opts) of
                Auth when is_binary(Auth), Auth =/= <<>> ->
                    {ok, Auth};
                _ ->
                    priv_cookie_token(Req, Opts)
            end
    end.

%% @doc Read the parsed cookie map that the HTTP layer stores under
%% `priv/cookie'. This is the form the token takes over real HTTP: `hb_http'
%% runs the inbound `cookie' header through the `~cookie@1.0' codec before the
%% request hook executes, leaving a parsed map (e.g.
%% `#{ <<"auth_token">> => <<...>> }') rather than the raw header. We canonicalise
%% that map to the same token string the raw-cookie path produces.
priv_cookie_token(Req, Opts) ->
    case hb_private:get(<<"cookie">>, Req, #{}, Opts) of
        ParsedCookie when is_map(ParsedCookie), map_size(ParsedCookie) > 0 ->
            {ok, canonical_cookie(ParsedCookie, Opts)};
        _ ->
            {error, no_token}
    end.

%% @doc Canonicalise a raw `cookie' header binary to a deterministic token. We
%% parse the `key=value; key2=value2' header into the same map form the HTTP layer
%% stores under `priv/cookie' (the `~cookie@1.0' codec's `from_cookie' parse:
%% split on `;', then on the first `=', trimming and URL-decoding values), then
%% serialise it canonically. This makes the raw-header path and the parsed
%% `priv/cookie' path converge on an identical token for the same Odysee session.
%% The parse is done inline with core utilities rather than via the `~cookie@1.0'
%% device, as device-to-device source calls are not available from within the
%% build-signed preloaded device context.
canonical_token(Cookie, Opts) ->
    case parse_cookie_header(Cookie) of
        ParsedCookie when map_size(ParsedCookie) > 0 ->
            {ok, canonical_cookie(ParsedCookie, Opts)};
        _ ->
            {ok, Cookie}
    end.

%% @doc Parse a raw `cookie' header binary into a key-value map, mirroring the
%% `~cookie@1.0' codec's `from_cookie' parse so that a header and its parsed
%% `priv/cookie' form canonicalise identically. Malformed pairs (no `=') are
%% skipped, leaving the raw header to be used verbatim if nothing parses.
parse_cookie_header(Cookie) ->
    lists:foldl(
        fun(Pair, Acc) ->
            case binary:split(Pair, <<"=">>) of
                [RawKey, RawValue] ->
                    Key = trim(RawKey),
                    Value = hb_escape:decode(trim(RawValue)),
                    Acc#{ Key => Value };
                _ ->
                    Acc
            end
        end,
        #{},
        binary:split(Cookie, <<";">>, [global])
    ).

%% @doc Trim leading and trailing ASCII whitespace from a binary.
trim(Bin) ->
    hb_util:bin(string:trim(hb_util:list(Bin))).

%% @doc Serialise a parsed cookie map to a single deterministic token string. The
%% map's entries are sorted by key and rendered as `key=value' pairs joined by
%% `;', so the same set of cookie entries always produces byte-identical output
%% regardless of map ordering. A cookie value may itself be a map (when it carried
%% `set-cookie' attributes/flags), in which case we take its `value' field, so the
%% token depends only on the session value and not on transport attributes.
canonical_cookie(ParsedCookie, Opts) ->
    Pairs = lists:keysort(1, hb_maps:to_list(ParsedCookie, Opts)),
    Components =
        [
            <<(hb_util:bin(Key))/binary, "=", (cookie_value(Value))/binary>>
        ||
            {Key, Value} <- Pairs
        ],
    hb_util:bin(
        lists:join(<<";">>, Components)
    ).

%% @doc Extract the canonical value of a single parsed cookie entry. When the
%% entry is a map (carrying `set-cookie' attributes/flags), only its `value' field
%% contributes to the token; a bare binary value is used as-is.
cookie_value(Value) when is_map(Value) ->
    hb_util:bin(hb_maps:get(<<"value">>, Value, <<>>));
cookie_value(Value) ->
    hb_util:bin(Value).

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
