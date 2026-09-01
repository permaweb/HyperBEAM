%%% @doc A device that verifies responses from remote cache nodes and invokes
%%% the configured hook for admitted responses.
-module(dev_cache_admissibility).
-export([expected_response/3]).
-implements(<<"cache-admissibility@1.0">>).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Verify that a remote cache response contains the requested message and
%% is cryptographically valid. `Base' carries the expected id and hook config;
%% `Req' is the remote response.
expected_response(Base, Req, Opts) ->
    case hb_maps:find(<<"requested-key">>, Base, Opts) of
        {ok, Expected} ->
            Admissible = check_response_matches_expected(Req, Expected, Opts),
            ?event(debug_admissible,
                {expected_response, {expected, Expected}, {admissible, Admissible}}),
            case Admissible of
                true -> fire_admissible_response_hook(Base, Opts);
                _ -> ok
            end,
            {ok, Admissible};
        error ->
            ?event(error, {config_key_not_found, <<"expected">>, Base}),
            {ok, false}
    end.

%% @doc Fire the `admissible-response' hook when a peer read is admitted. Spawns
%% by default so the read returns without waiting for the downstream handler;
%% set `<<"admissible-response-hook-async">>=> false' to run it synchronously.
fire_admissible_response_hook(Base, Opts) ->
    % Relay + handler-device loading resolve against the node default store (local
    % cache + gateway). Drop the request's remote-node store, or those reads
    % re-enter the multi_read fan-out. (Gateway and remote-node are both `remote'
    % scope, so hb_store:scope can't separate them , dropping the override is the way.)
    Local = maps:without([<<"store">>], Opts),
    HookOpts =
        Local#{
            <<"on">> => hb_maps:get(<<"on">>, Base, #{}, Opts),
            <<"commit-hook-response">> =>
                hb_opts:get(<<"commit-hook-response">>, false, Base),
            <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
        },
    Run =
        fun() ->
            hb_hook:on(
                <<"admissible-response">>,
                #{ <<"body">> => admissible_response_body(Base, HookOpts) },
                HookOpts
            )
        end,
    case hb_opts:get(<<"admissible-response-hook-async">>, true, Opts) of
        false -> Run();
        _ ->
            spawn(
                fun() ->
                    try Run()
                    catch C:R:S ->
                        ?event(debug_admissible,
                            {admissible_response_hook_async_error,
                                {class, C}, {reason, R}, {stacktrace, {trace, S}}})
                    end
                end
            )
    end.

%% @doc Build the message supplied to the admitted-response hook.
admissible_response_body(Base, Opts) ->
    Ref = hb_maps:get(<<"http-reference">>, Base, <<>>, Opts),
    Body =
        #{
            <<"reference">> => Ref,
            <<"status-class">> => <<"success">>,
            <<"event">> => <<"is-admissible">>
        },
    case hb_opts:get(commit_hook_response, false, Opts) of
        true ->
            hb_message:commit(
                Body,
                Opts#{ <<"priv-wallet">> => hb_opts:get(priv_wallet, hb:wallet(), Opts) },
                #{ <<"type">> => <<"signed">> }
            );
        _ -> Body
    end.

%% @doc Verify that a remote cache response matches the expected id. For an
%% id-based read, the expected id must be among the response's commitment ids and
%% that commitment must verify. For a path-based read, verify all
%% committer-attributed commitments on the response.
check_response_matches_expected(Response, Expected, Opts) ->
    case hb_maps:get(<<"commitments">>, Response, not_found, Opts) of
        not_found ->
            false;
        _ ->
            CommitmentIDs =
                hb_maps:keys(
                    hb_maps:get(<<"commitments">>, Response, #{}, Opts),
                    Opts
                ),
            MembershipOk =
                (not ?IS_ID(Expected))
                    orelse lists:member(Expected, CommitmentIDs),
            VerifyReq =
                case ?IS_ID(Expected) of
                    true -> #{ <<"commitment-ids">> => [Expected] };
                    false -> #{ <<"committers">> => <<"all">> }
                end,
            MembershipOk
                andalso hb_message:verify(Response, VerifyReq, Opts)
    end.

%%% Tests

-ifdef(TEST).

missing_expected_test() ->
    ?assertEqual(
        {ok, false},
        hb_ao:resolve(
            #{ <<"device">> => <<"cache-admissibility@1.0">> },
            #{ <<"path">> => <<"expected-response">> },
            #{}
        )
    ).

%% @doc A response with the expected commitment id but modified committed data
%% is rejected.
tampered_response_test() ->
    SignOpts = #{ <<"priv-wallet">> => ar_wallet:new() },
    Response = hb_message:commit(#{ <<"test">> => <<"value">> }, SignOpts),
    Expected = hb_message:id(Response, signed, SignOpts),
    Base =
        #{
            <<"device">> => <<"cache-admissibility@1.0">>,
            <<"requested-key">> => Expected
        },
    Tampered =
        Response#{
            <<"test">> => <<"tampered">>,
            <<"path">> => <<"expected-response">>
        },
    ?assertEqual({ok, false}, hb_ao:resolve(Base, Tampered, #{})).

%% @doc Build an admitted response and a hook that reports its invocation to
%% the calling test process.
hook_test_messages(Ref) ->
    Parent = self(),
    SignOpts = #{ <<"priv-wallet">> => ar_wallet:new() },
    Response = hb_message:commit(#{ <<"test">> => <<"value">> }, SignOpts),
    Expected = hb_message:id(Response, signed, SignOpts),
    Handler =
        #{
            <<"device">> =>
                #{
                    notify =>
                        fun(_Base, Req, _Opts) ->
                            Parent ! {Ref, Req},
                            {ok, Req}
                        end
                },
            <<"path">> => <<"notify">>
        },
    Base =
        #{
            <<"device">> => <<"cache-admissibility@1.0">>,
            <<"requested-key">> => Expected,
            <<"on">> => #{ <<"admissible-response">> => Handler }
        },
    {Base, Response#{ <<"path">> => <<"expected-response">> }}.

%% @doc The synchronous option runs the admitted-response hook before the
%% predicate resolution returns.
synchronous_hook_test() ->
    Ref = make_ref(),
    {Base, Req} = hook_test_messages(Ref),
    ?assertEqual(
        {ok, true},
        hb_ao:resolve(
            Base,
            Req,
            #{ <<"admissible-response-hook-async">> => false }
        )
    ),
    receive
        {Ref, HookReq} ->
            Body = hb_maps:get(<<"body">>, HookReq, #{}),
            ?assertEqual(
                <<"is-admissible">>,
                hb_maps:get(<<"event">>, Body, undefined)
            )
    after 0 ->
        ?assert(false)
    end.

%% @doc The configured hook response is signed and verifies before delivery.
signed_hook_response_test() ->
    Ref = make_ref(),
    {Base, Req} = hook_test_messages(Ref),
    SignedBase = Base#{ <<"commit-hook-response">> => true },
    ?assertEqual(
        {ok, true},
        hb_ao:resolve(
            SignedBase,
            Req,
            #{ <<"admissible-response-hook-async">> => false }
        )
    ),
    receive
        {Ref, HookReq} ->
            Body = hb_maps:get(<<"body">>, HookReq, #{}),
            ?assert(hb_message:verify(Body, all, #{}))
    after 0 ->
        ?assert(false)
    end.

%% @doc The default asynchronous mode eventually invokes the admitted-response
%% hook without changing the predicate result.
asynchronous_hook_test() ->
    Ref = make_ref(),
    {Base, Req} = hook_test_messages(Ref),
    ?assertEqual({ok, true}, hb_ao:resolve(Base, Req, #{})),
    receive
        {Ref, HookReq} ->
            Body = hb_maps:get(<<"body">>, HookReq, #{}),
            ?assertEqual(
                <<"is-admissible">>,
                hb_maps:get(<<"event">>, Body, undefined)
            )
    after 1000 ->
        ?assert(false)
    end.

-endif.
