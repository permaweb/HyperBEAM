-module(httpsig_stream_sign_test).
-include_lib("eunit/include/eunit.hrl").

header_only_stream_signature_test() ->
    %% Simulate header-mode streaming: sign metadata only (no content-digest/body)
    Msg = #{
        <<"status">> => 200,
        <<"ao-result">> => <<"body">>,
        <<"content-type">> => <<"video/mp4">>
    },
    CommitSpec = #{
        <<"commitment-device">> => <<"httpsig@1.0">>,
        <<"committed">> => [<<"ao-result">>, <<"status">>]
    },
    Signed = hb_message:commit(Msg, #{ priv_wallet => hb:wallet() }, CommitSpec),
    {ok, #{ <<"headers">> := Headers }} =
        dev_codec_httpsig:serialize(Signed, #{ <<"format">> => <<"components">> }, #{}),
    ?assert(maps:is_key(<<"signature">>, Headers)),
    ?assert(maps:is_key(<<"signature-input">>, Headers)),
    %% In header-only mode there is no digest
    ?assertNot(maps:is_key(<<"content-digest">>, Headers)).

%% Trailer-mode streaming removed: no trailer signature tests remain.

full_body_signature_has_digest_test() ->
    %% Full GET (no range): body present, encoder moves digest into headers
    Msg = #{
        <<"status">> => 200,
        <<"ao-result">> => <<"body">>,
        <<"body">> => <<"abc">>,
        <<"content-type">> => <<"application/octet-stream">>
    },
    Signed = hb_message:commit(Msg, #{ priv_wallet => hb:wallet() }, <<"httpsig@1.0">>),
    {ok, #{ <<"headers">> := Headers, <<"body">> := _Body }} =
        dev_codec_httpsig:serialize(Signed, #{ <<"format">> => <<"components">> }, #{}),
    ?assert(maps:is_key(<<"signature">>, Headers)),
    ?assert(maps:is_key(<<"signature-input">>, Headers)),
    ?assert(maps:is_key(<<"content-digest">>, Headers)).

range_partial_signature_test() ->
    %% Range 206: partial body signed (digest must be present)
    Partial = <<"1234567890">>,
    Msg = #{
        <<"status">> => 206,
        <<"ao-result">> => <<"body">>,
        <<"body">> => Partial,
        <<"content-type">> => <<"application/octet-stream">>,
        <<"content-range">> => <<"bytes 0-9/10">>
    },
    Signed = hb_message:commit(Msg, #{ priv_wallet => hb:wallet() }, <<"httpsig@1.0">>),
    {ok, #{ <<"headers">> := Headers }} =
        dev_codec_httpsig:serialize(Signed, #{ <<"format">> => <<"components">> }, #{}),
    ?assertEqual(<<"bytes 0-9/10">>, maps:get(<<"content-range">>, Headers)),
    ?assert(maps:is_key(<<"signature">>, Headers)),
    ?assert(maps:is_key(<<"signature-input">>, Headers)),
    ?assert(maps:is_key(<<"content-digest">>, Headers)).
