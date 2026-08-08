%%% @doc Deterministic admission vectors for full Arweave block validation.
%%%
%%% A live, full account transition is provided by
%%% `dev_arweave_sync_test_vectors:live_account_transition/0'. It hydrates a
%%% recent checkpoint because public peers prune historical wallet lists.
-module(dev_arweave_block_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc A wide range carries the standard 10,800-step suffix without being
%% rejected merely because the parent is farther away.
wide_step_range_test() ->
    Output = <<0:256>>,
    Steps = lists:duplicate(10800, Output),
    Prev =
        #block{
            nonce_limiter_info =
                #nonce_limiter_info{
                    global_step_number = 1,
                    output = Output
                }
        },
    Next =
        #block{
            nonce_limiter_info =
                #nonce_limiter_info{
                    global_step_number = 10802,
                    prev_output = Output,
                    steps = Steps
                }
        },
    ?assertEqual(ok, dev_arweave_block:check_step_number(Next, Prev)),
    NextInfo = Next#block.nonce_limiter_info,
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-step-number">> }},
        dev_arweave_block:check_step_number(
            Next#block{
                nonce_limiter_info =
                    NextInfo#nonce_limiter_info{ steps = tl(Steps) }
            },
            Prev
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-step-number">> }},
        dev_arweave_block:check_step_number(
            Next#block{
                nonce_limiter_info =
                    NextInfo#nonce_limiter_info{ steps = [Output | Steps] }
            },
            Prev
        )
    ).

%% @doc A VDF answer is accepted only when it carries the atom `true'.
vdf_fails_closed_test() ->
    Refused =
        fun(Answer) ->
            case dev_arweave_block:holds(
                maps:get(<<"valid">>, Answer, false) =:= true,
                <<"invalid-vdf-chain">>,
                <<"detail">>
            ) of
                ok -> accepted;
                {error, Error} -> maps:get(<<"message">>, Error)
            end
        end,
    ?assertEqual(accepted, Refused(#{ <<"valid">> => true })),
    ?assertEqual(
        <<"invalid-vdf-chain">>,
        Refused(#{ <<"valid">> => false })
    ),
    ?assertEqual(
        <<"invalid-vdf-chain">>,
        Refused(#{ <<"valid">> => <<"true">> })
    ),
    ?assertEqual(<<"invalid-vdf-chain">>, Refused(#{})).

%% @doc Malformed wire bytes are reported as a codec error rather than
%% escaping as an exception.
rejects_corrupt_binary_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    {error, Error} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-block@2.9">>,
                <<"body">> => <<0:512>>
            },
            <<"from-binary">>,
            Opts
        ),
    ?assertEqual(
        <<"invalid-block-encoding">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).
