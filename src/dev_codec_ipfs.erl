%%% @doc `~ipfs@1.0': a codec and commitment device whose commitment IDs are
%%% IPFS CIDv1s over a message's `body'. In codec mode, encodes TABMs to
%%% deterministic dag-cbor and back, routed through `~structured@1.0' the
%%% same way `dev_codec_json' and `dev_codec_flat' do. The `body''s CID is
%%% produced by `dev_codec_ipfs_cid:encode/3'; `hb_cache' then links the
%%% CID to the message's uncommitted ID automatically.
-module(dev_codec_ipfs).
-export([info/1, commit/3, verify/3, content_type/1]).
-export([to/3, from/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE_NAME, <<"ipfs@1.0">>).
%% Native commitment types combine the multihash and the CID multicodec so
%% they slot into the wire `alg' field as `ipfs@1.0/<type>' via
%% `dev_codec_httpsig_siginfo:commitment_to_alg/2' — no custom RFC 9421
%% metadata parameters.
-define(DEFAULT_TYPE, <<"sha2-256-raw">>).
-define(COMMITTED_KEYS, [<<"body">>]).
-define(IS_NATIVE_TYPE(T),
    (T =:= <<"sha2-256-raw">> orelse T =:= <<"sha2-256-dag-cbor">>)).

%% @doc Restrict AO-Core resolution to the codec/commitment surface.
info(_) ->
    #{ exports => [commit, verify, content_type, to, from] }.

%% @doc Return the IPLD MIME type for a commitment's native `type'.
content_type(#{ <<"type">> := <<"sha2-256-dag-cbor">> }) ->
    {ok, <<"application/vnd.ipld.dag-cbor">>};
content_type(_) ->
    {ok, <<"application/vnd.ipld.raw">>}.

%% @doc Attach a CIDv1 commitment over `body'. `type: unsigned' is the
%% generic caller knob — translate it to the codec's native type. Any other
%% commit type (signed, rsa-pss, etc.) delegates to `~httpsig@1.0', the
%% composition pattern used by `dev_codec_flat' and `dev_codec_json'.
commit(Msg, Req = #{ <<"type">> := <<"unsigned">> }, Opts) ->
    Native = hb_maps:get(<<"hash-alg">>, Req, ?DEFAULT_TYPE, Opts),
    Req1 = hb_maps:without([<<"hash-alg">>], Req, Opts),
    commit(Msg, Req1#{ <<"type">> => Native }, Opts);
commit(Msg, #{ <<"type">> := Type }, Opts) when ?IS_NATIVE_TYPE(Type) ->
    Body = hb_maps:get(<<"body">>, Msg, <<>>, Opts),
    Multicodec = multicodec_of(Type),
    CID = dev_codec_ipfs_cid:encode(Multicodec, sha2_256, Body),
    Commitment = #{
        <<"commitment-device">> => ?DEVICE_NAME,
        <<"type">>              => Type,
        <<"committed">>         => ?COMMITTED_KEYS,
        %% Carrying the raw sha-256 digest as `signature' keeps the
        %% commitment on the httpsig wire (see
        %% `dev_codec_httpsig_siginfo''s signature filter). No `keyid' —
        %% content-addressed commitments need no key material. RFC 9421
        %% §1.4.2.3 permits keyid's absence.
        <<"signature">>         => hb_util:encode(crypto:hash(sha256, Body))
    },
    Existing = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    ?event(ipfs, {commit, {cid, CID}, {type, Type}, {size, byte_size(Body)}}),
    {ok, Msg#{ <<"commitments">> => Existing#{ CID => Commitment } }};
commit(_Msg, #{ <<"type">> := <<"sha2-256-", _/binary>> = Type }, _Opts) ->
    {error, {unsupported_type, Type}};
commit(Msg, Req, Opts) ->
    dev_codec_httpsig:commit(Msg, Req, Opts).

%% @doc Verify an `~ipfs@1.0' commitment by recomputing the CID from `body'
%% under the declared native type and checking it keys the commitments map.
verify(Base, #{ <<"type">> := Type }, Opts) when ?IS_NATIVE_TYPE(Type) ->
    Body = hb_maps:get(<<"body">>, Base, <<>>, Opts),
    Comms = hb_maps:get(<<"commitments">>, Base, #{}, Opts),
    Expected = dev_codec_ipfs_cid:encode(multicodec_of(Type), sha2_256, Body),
    Res = hb_maps:is_key(Expected, Comms, Opts),
    ?event(ipfs, {verify, {type, Type}, {expected, Expected}, {result, Res}}),
    {ok, Res};
verify(Base, Req, Opts) ->
    dev_codec_httpsig:verify(Base, Req, Opts).

%% @doc Resolve a native `type' to its CID multicodec name.
multicodec_of(<<"sha2-256-raw">>)      -> <<"raw">>;
multicodec_of(<<"sha2-256-dag-cbor">>) -> <<"dag-cbor">>.

%% @doc Serialize a TABM to deterministic dag-cbor bytes. Routes through
%% `~structured@1.0' to recover native types, resolves links (dag-cbor is
%% self-contained), strips `priv', and walks the result into the IPLD
%% intermediate form that `dev_codec_ipfs_cbor:encode/1' consumes.
to(Bin, _Req, _Opts) when is_binary(Bin) ->
    %% Bare binaries encode as text strings (or byte strings if not UTF-8)
    %% so that `to' / `from' is a roundtrip.
    try {ok, dev_codec_ipfs_cbor:encode(Bin)}
    catch throw:{dag_cbor_encode, {invalid_utf8, _}} ->
        {ok, dev_codec_ipfs_cbor:encode({bytes, Bin})}
    end;
to(Msg, _Req, Opts) when is_map(Msg) ->
    try
        Structured =
            hb_message:convert(
                hb_private:reset(Msg),
                <<"structured@1.0">>,
                tabm,
                Opts
            ),
        Loaded = hb_cache:ensure_all_loaded(Structured, Opts),
        Clean = hb_maps:without([<<"priv">>], Loaded, Opts),
        {ok, dev_codec_ipfs_cbor:encode(structured_to_ipld(Clean))}
    catch throw:{dag_cbor_encode, Reason} ->
        ?event(warning, {ipfs_to_failed, Reason}),
        {error, {dag_cbor_encode, Reason}}
    end.

%% @doc Walk a structured HyperBEAM value into the IPLD intermediate form.
%% Atoms outside `null/true/false' have no IPLD representation and throw.
structured_to_ipld(null)  -> null;
structured_to_ipld(true)  -> true;
structured_to_ipld(false) -> false;
structured_to_ipld(A) when is_atom(A) ->
    throw({dag_cbor_encode, {unsupported_atom, A}});
structured_to_ipld(N) when is_integer(N); is_float(N) -> N;
structured_to_ipld(B) when is_binary(B) -> B;
structured_to_ipld(L) when is_list(L) ->
    [ structured_to_ipld(V) || V <- L ];
structured_to_ipld(M) when is_map(M) ->
    maps:from_list(
        [
            {assert_binary_key(K), structured_to_ipld(V)}
        ||
            {K, V} <- maps:to_list(M)
        ]
    );
structured_to_ipld(V) ->
    throw({dag_cbor_encode, {unsupported_value, V}}).

assert_binary_key(K) when is_binary(K) -> K;
assert_binary_key(K) -> throw({dag_cbor_encode, {non_binary_map_key, K}}).

%% @doc Parse dag-cbor bytes into a TABM. Pre-decoded maps pass through
%% unchanged, matching the `dev_codec_json' / `dev_codec_flat' discipline.
from(Map, _Req, _Opts) when is_map(Map) ->
    {ok, Map};
from(Bin, Req, Opts) when is_binary(Bin) ->
    case dev_codec_ipfs_cbor:decode(Bin) of
        {ok, Ipld} ->
            case ipld_to_structured(Ipld) of
                M when is_map(M) -> dev_codec_structured:from(M, Req, Opts);
                Other            -> {ok, Other}
            end;
        {error, Reason} ->
            ?event(warning, {ipfs_from_failed, Reason}),
            {error, {dag_cbor_decode, Reason}}
    end.

%% @doc Walk the IPLD intermediate form into a rich-typed HyperBEAM value.
%% `{bytes, B}' flattens to a binary; `{link, CID}' flattens to the CID
%% string — a link-aware mapping through `hb_link' is future work.
ipld_to_structured(null)  -> null;
ipld_to_structured(true)  -> true;
ipld_to_structured(false) -> false;
ipld_to_structured(N) when is_integer(N); is_float(N) -> N;
ipld_to_structured(B) when is_binary(B) -> B;
ipld_to_structured({bytes, B})          -> B;
ipld_to_structured({link, CID})         -> CID;
ipld_to_structured(L) when is_list(L) ->
    [ ipld_to_structured(V) || V <- L ];
ipld_to_structured(M) when is_map(M) ->
    maps:map(fun(_K, V) -> ipld_to_structured(V) end, M).

%%% Tests. Integration-level tests live in `dev_codec_ipfs_test'.

commit_unsigned_raw_attaches_cid_test() ->
    {ok, Committed} =
        commit(
            #{ <<"body">> => <<"hello world">> },
            #{ <<"type">> => <<"unsigned">> },
            #{}
        ),
    [CID] = maps:keys(maps:get(<<"commitments">>, Committed)),
    ?assertEqual(
        <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
        CID
    ),
    Commitment = maps:get(CID, maps:get(<<"commitments">>, Committed)),
    ?assertEqual(?DEVICE_NAME, maps:get(<<"commitment-device">>, Commitment)),
    ?assertEqual(<<"sha2-256-raw">>, maps:get(<<"type">>, Commitment)),
    ?assertEqual([<<"body">>], maps:get(<<"committed">>, Commitment)),
    %% The commitment carries `signature' (= raw digest) so it survives the
    %% httpsig wire; `keyid', `committer', `hash-alg', and `multicodec' are
    %% deliberately absent — content-addressed commitments need no key, and
    %% the multihash + multicodec are already encoded in `type'.
    ?assertMatch(#{<<"signature">> := _}, Commitment),
    ?assertNot(maps:is_key(<<"keyid">>, Commitment)),
    ?assertNot(maps:is_key(<<"committer">>, Commitment)),
    ?assertNot(maps:is_key(<<"hash-alg">>, Commitment)),
    ?assertNot(maps:is_key(<<"multicodec">>, Commitment)).

commit_unsigned_dag_cbor_test() ->
    {ok, Committed} =
        commit(
            #{ <<"body">> => <<16#a0>> },
            #{
                <<"type">>     => <<"unsigned">>,
                <<"hash-alg">> => <<"sha2-256-dag-cbor">>
            },
            #{}
        ),
    [CID] = maps:keys(maps:get(<<"commitments">>, Committed)),
    ?assertEqual(
        <<"bafyreigbtj4x7ip5legnfznufuopl4sg4knzc2cof6duas4b3q2fy6swua">>,
        CID
    ).

commit_native_type_test() ->
    {ok, Committed} =
        commit(
            #{ <<"body">> => <<"hello world">> },
            #{ <<"type">> => <<"sha2-256-raw">> },
            #{}
        ),
    [CID] = maps:keys(maps:get(<<"commitments">>, Committed)),
    ?assertEqual(
        <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
        CID
    ).

commit_preserves_existing_commitments_test() ->
    Msg = #{
        <<"body">>        => <<"hello world">>,
        <<"commitments">> => #{ <<"other">> => #{ <<"kind">> => <<"x">> } }
    },
    {ok, Committed} = commit(Msg, #{ <<"type">> => <<"unsigned">> }, #{}),
    ?assertEqual(2, maps:size(maps:get(<<"commitments">>, Committed))).

commit_signed_delegates_to_httpsig_test() ->
    {ok, Signed} =
        commit(
            #{ <<"body">> => <<"x">> },
            #{ <<"type">> => <<"signed">> },
            #{ priv_wallet => ar_wallet:new() }
        ),
    [{_CID, C}|_] = maps:to_list(maps:get(<<"commitments">>, Signed)),
    ?assertEqual(<<"httpsig@1.0">>, maps:get(<<"commitment-device">>, C)).

commit_rejects_unsupported_ipfs_type_test() ->
    ?assertMatch(
        {error, {unsupported_type, <<"sha2-256-dag-pb">>}},
        commit(
            #{ <<"body">> => <<"x">> },
            #{
                <<"type">>     => <<"unsigned">>,
                <<"hash-alg">> => <<"sha2-256-dag-pb">>
            },
            #{}
        )
    ).

verify_ok_for_intact_body_test() ->
    {ok, Committed} =
        commit(
            #{ <<"body">> => <<"hello world">> },
            #{ <<"type">> => <<"unsigned">> },
            #{}
        ),
    [{_CID, C}] = maps:to_list(maps:get(<<"commitments">>, Committed)),
    ?assertEqual({ok, true}, verify(Committed, C, #{})).

verify_fails_for_tampered_body_test() ->
    {ok, Committed} =
        commit(
            #{ <<"body">> => <<"hello world">> },
            #{ <<"type">> => <<"unsigned">> },
            #{}
        ),
    [{_CID, C}] = maps:to_list(maps:get(<<"commitments">>, Committed)),
    ?assertEqual(
        {ok, false},
        verify(Committed#{ <<"body">> => <<"hello earth">> }, C, #{})
    ).

verify_fails_when_hash_alg_mismatches_test() ->
    {ok, Committed} =
        commit(
            #{ <<"body">> => <<"hello world">> },
            #{ <<"type">> => <<"unsigned">> },
            #{}
        ),
    [{_CID, C}] = maps:to_list(maps:get(<<"commitments">>, Committed)),
    ?assertEqual(
        {ok, false},
        verify(Committed, C#{ <<"type">> => <<"sha2-256-dag-cbor">> }, #{})
    ).
