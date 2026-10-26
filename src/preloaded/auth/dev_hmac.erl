%%% @doc A device that implements the `hmac:' URI scheme. Its URIs name the
%%% committers of HMAC-SHA256 keys, in one of two forms:
%%% <pre>
%%%     hmac:constant:VALUE   The key is VALUE, which is known to everyone.
%%%     hmac:secret:ID        The key is derived from a secret. The ID of the
%%%                           secret is given by
%%%                           lib_secret:to_committer(Secret).
%%% </pre>
%%%
%%% The key of a secret is the SHA-256 hash of `hmac:secret:' followed by the
%%% secret. We do not use the secret itself as the key: HMAC hashes any key
%%% that is longer than its block size (RFC 2104), so the published ID of a
%%% secret over 64 bytes would be a working key.
%%%
%%% The device's `from' and `to' keys read and write the URI of a committer,
%%% when called with `type: uri'. The `commit' key returns the HMAC of the
%%% request's `body' as its `signature', and `verify' checks a `signature'
%%% against the `body'. The secret of an `hmac:secret:' committer is read from
%%% its `priv' message.
-module(dev_hmac).
-device_libraries([lib_secret]).
-export([from/3, to/3, commit/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Read the committer of an `hmac:' URI. `hb_uri' gives the rest of the
%% URI as the `body' of the message, which is already the committer.
-spec from(
    #{ body := binary(), _ => _ },
    #{ type := binary(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ body := binary(), _ => _ }}.
from(Committer, #{ <<"type">> := <<"uri">> }, _Opts) ->
    {ok, Committer}.

%% @doc Write the `hmac:' URI of a committer.
-spec to(
    #{ body := binary(), _ => _ },
    #{ type := binary(), _ => _ },
    #{ _ => _ }
) -> {ok, binary()} | {error, _}.
to(#{ <<"body">> := Body }, #{ <<"type">> := <<"uri">> }, _Opts) ->
    {ok, <<"hmac:", Body/binary>>}.

%% @doc Sign the request's `body' with the committer's key, returning the
%% HMAC-SHA256 as the `signature'.
-spec commit(
    #{ body := binary(), priv => #{ secret => binary() }, _ => _ },
    #{ body := binary(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ signature := binary() }} | {error, not_found}.
commit(Committer, #{ <<"body">> := Base }, Opts) ->
    maybe
        {ok, Key} ?= key(Committer, Opts),
        {ok, #{ <<"signature">> => crypto:mac(hmac, sha256, Key, Base) }}
    end.

%% @doc Verify that a `signature' is the HMAC-SHA256 of the request's `body'
%% under the committer's key. The comparison is made in constant time.
-spec verify(
    #{ body := binary(), priv => #{ secret => binary() }, _ => _ },
    #{ body := binary(), signature := binary(), _ => _ },
    #{ _ => _ }
) -> {ok, boolean()}.
verify(Committer, Req = #{ <<"body">> := Base, <<"signature">> := Sig }, Opts) ->
    case key(Committer, Opts) of
        {ok, Key} ->
            HMac = crypto:mac(hmac, sha256, Key, Base),
            Expected =
                case hb_maps:get(<<"body">>, Committer, undefined, Opts) of
                    <<"secret:", ID/binary>> -> <<"hmac:secret:", ID/binary>>;
                    _ -> undefined
                end,
            {
                ok,
                byte_size(Sig) == byte_size(HMac)
                    andalso crypto:hash_equals(HMac, Sig)
                    andalso hb_committer:matches_commitment(Expected, Req, Opts)
            };
        {error, not_found} -> {ok, false}
    end.

%% @doc Find the key of a committer. The secret of an `hmac:secret:' committer
%% is read from its `priv', and must hash to the ID in the committer's `body'.
key(#{ <<"body">> := <<"constant:", Value/binary>> }, _Opts) ->
    {ok, Value};
key(Committer = #{ <<"body">> := <<"secret:", ID/binary>> }, Opts) ->
    Secret = hb_private:get(<<"secret">>, Committer, not_found, Opts),
    case is_binary(Secret) andalso lib_secret:to_committer(Secret) of
        ID -> {ok, hb_crypto:sha256(<<"hmac:secret:", Secret/binary>>)};
        _ -> {error, not_found}
    end;
key(_Committer, _Opts) ->
    {error, not_found}.

%%% Tests

%% @doc Read a committer from an `hmac:' URI, and write it back to the same
%% URI.
uri_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    {ok, Committer} = hb_uri:from(?UNSIGNED_COMMITTER, Opts),
    ?assertEqual(
        {ok, ?UNSIGNED_COMMITTER},
        hb_uri:to(Committer, #{ <<"scheme">> => <<"hmac">> }, Opts)
    ).

%% @doc Ensure that a secret over 64 bytes does not sign under the key of its
%% published ID, and that its signatures verify only with the secret.
secret_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Secret = binary:copy(<<"s">>, 100),
    ID = lib_secret:to_committer(Secret),
    Body = <<"signature base">>,
    {ok, Committer} = hb_uri:from(<<"hmac:secret:", ID/binary>>, Opts),
    Call =
        fun(Holder, Req) ->
            hb_ao:raw(
                Committer#{ <<"priv">> => #{ <<"secret">> => Holder } },
                Req#{ <<"body">> => Body },
                Opts
            )
        end,
    {ok, #{ <<"signature">> := Sig }} =
        Call(Secret, #{ <<"path">> => <<"commit">> }),
    % The decoded ID of the secret must not give the same HMAC.
    ?assertNotEqual(Sig, crypto:mac(hmac, sha256, hb_util:decode(ID), Body)),
    Verify = #{ <<"path">> => <<"verify">>, <<"signature">> => Sig },
    ?assertEqual({ok, true}, Call(Secret, Verify)),
    ?assertEqual({ok, false}, Call(<<"other">>, Verify)).
