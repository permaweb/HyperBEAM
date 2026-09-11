-module(dev_test).
-implements(<<"test-device@1.0">>).
-export([info/3]).
-export([info/1, test_func/1, compute/3, init/3, restore/3, snapshot/3, mul/2]).
-export([mangle/3, update_state/3, increment_counter/3, delay/3, append/3]).
-export([index/3, postprocess/3, load/3]).
-export([vary_projection/3, vary_wildcard/3, vary_unspecified/3]).
-export([vary_overlay/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% A simple test device for AO-Core, so that we can test the functionality that
%%% depends on using Erlang's module system.
%%% 
%%% NOTE: This device is labelled `test-device/1.0' to avoid conflicts with
%%% other testing functionality -- care should equally be taken to avoid
%%% using the `test' key in other settings.


%% @doc Exports a default_handler function that can be used to test the
%% handler resolution mechanism.
info(_) ->
	#{
        <<"default">> => <<"message@1.0">>,
		handlers => #{
			<<"info">> => fun info/3,
			<<"update_state">> => fun update_state/3,
			<<"increment_counter">> => fun increment_counter/3
		}
	}.

%% @doc Exports a default_handler function that can be used to test the
%% handler resolution mechanism.
-spec info(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ status := integer(), body := #{ _ => _ } }}.
info(_Base, _Req, _Opts) ->
	InfoBody = #{
		<<"description">> => <<"Test device for testing the AO-Core framework">>,
		<<"version">> => <<"1.0">>,
		<<"paths">> => #{
			<<"info">> => <<"Get device info">>,
			<<"test_func">> => <<"Test function">>,
			<<"compute">> => <<"Compute function">>,
			<<"init">> => <<"Initialize function">>,
			<<"restore">> => <<"Restore function">>,
			<<"mul">> => <<"Multiply function">>,
			<<"snapshot">> => <<"Snapshot function">>,
			<<"response">> => <<"Response function">>,
			<<"append">> => <<"Append a test value">>,
			<<"update_state">> => <<"Update state function">>
		}
	},
	{ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc Example index handler.
-spec index(#{ name => binary(), _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ body := binary(), 'content-type' := binary(), _ => _ }}.
index(Msg, _Req, Opts) ->
    Name = hb_ao:get(<<"name">>, Msg, <<"turtles">>, Opts),
    {ok,
        #{
            <<"content-type">> => <<"text/html">>,
            <<"body">> => <<"i like ", Name/binary, "!">>
        }
    }.

%% @doc Return a message with the device set to this module.
-spec load(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ device := binary(), _ => _ }}.
load(Base, _, _Opts) ->
    {ok, Base#{ <<"device">> => <<"test-device@1.0">> }}.

test_func(_) ->
	{ok, <<"GOOD FUNCTION">>}.

%% @doc Example implementation of a `compute' handler. Makes a running list of
%% the slots that have been computed in the state message and places the new
%% slot number in the results key.
-spec compute(#{ 'already-seen' => [integer()], _ => _ }, #{ slot := integer() }, #{ _ => _ }) ->
    {ok, #{ 'already-seen' := [integer()], results := #{ 'assignment-slot' := integer() }, _ => _ }}.
compute(Base, Req, Opts) ->
    AssignmentSlot = hb_ao:get(<<"slot">>, Req, Opts),
    Seen = hb_ao:get(<<"already-seen">>, Base, Opts),
    ?event({compute_called, {base, Base}, {req, Req}, {opts, Opts}}),
    {ok,
        hb_ao:set(
            Base,
            #{
                <<"random-key">> => <<"random-value">>,
                <<"results">> =>
                    #{ <<"assignment-slot">> => AssignmentSlot },
                <<"already-seen">> => [AssignmentSlot | Seen]
            },
            Opts
        )
    }.

%% @doc Example `init/3' handler. Sets the `Already-Seen' key to an empty list.
-spec init(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ 'already-seen' := list(), _ => _ }}.
init(Msg, _Req, Opts) ->
    ?event({init_called_on_dev_test, Msg}),
    {ok, hb_ao:set(Msg, #{ <<"already-seen">> => [] }, Opts)}.

%% @doc Example `restore/3' handler. Sets the hidden key `Test/Started' to the
%% value of `Current-Slot' and checks whether the `Already-Seen' key is valid.
-spec restore(#{ 'already-seen' => list(), _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | {error, binary()}.
restore(Msg, _Req, Opts) ->
    ?event({restore_called_on_dev_test, Msg}),
    case hb_ao:get(<<"already-seen">>, Msg, Opts) of
        not_found ->
            ?event({restore_not_found, Msg}),
            {error, <<"No viable state to restore.">>};
        AlreadySeen ->
            ?event({restore_found, AlreadySeen}),
            {ok,
                hb_private:set(
                    Msg,
                    #{ <<"test-key/started-state">> => AlreadySeen },
                    Opts
                )
            }
    end.

%% @doc Example implementation of an `imported' function for a WASM
%% executor.
mul(Base, Req) ->
    ?event(mul_called),
    State = hb_ao:get(<<"state">>, Base, #{ <<"hashpath">> => ignore }),
    [Arg1, Arg2] = hb_ao:get(<<"args">>, Req, #{ <<"hashpath">> => ignore }),
    ?event({mul_called, {state, State}, {args, [Arg1, Arg2]}}),
    {ok, #{ <<"state">> => State, <<"results">> => [Arg1 * Arg2] }}.

%% @doc Do nothing when asked to snapshot.
-spec snapshot(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) -> {ok, #{}}.
snapshot(Base, Req, _Opts) ->
    ?event({snapshot_called, {base, Base}, {req, Req}}),
    {ok, #{}}.

%% @doc Append a test binary to the `result' key.
append(Base = #{ <<"pass">> := 3 }, _Req, _Opts) ->
    {ok, Base};
append(Base, Req, Opts) ->
    Existing = hb_maps:get(<<"result">>, Base, <<>>, Opts),
    Prefix = hb_maps:get(<<"append-prefix">>, Base, <<>>, Opts),
    Bin = hb_maps:get(<<"bin">>, Req, <<>>, Opts),
    {ok, Base#{ <<"result">> => <<Existing/binary, Prefix/binary, Bin/binary>> }}.

%% @doc Set the `postprocessor-called' key to true in the HTTP server.
-spec postprocess(#{ _ => _ }, #{ body := _, _ => _ }, #{ _ => _ }) ->
    {ok, _}.
postprocess(_Msg, #{ <<"body">> := Msgs }, Opts) ->
    ?event({postprocess_called, Opts}),
    hb_http_server:set_opts(Opts#{ <<"postprocessor-called">> => true }),
    {ok, Msgs}.

%% @doc Find a test worker's PID and send it an update message.
-spec update_state(#{ _ => _ }, #{ 'test-id' => _, _ => _ }, #{ _ => _ }) ->
    {ok, ok} | {error, binary()}.
update_state(_Msg, Req, _Opts) ->
    case hb_ao:get(<<"test-id">>, Req) of
        not_found ->
            {error, <<"No test ID found in message.">>};
        ID ->
            LookupResult = hb_name:lookup({<<"test">>, ID}),
            case LookupResult of
                undefined ->
                    {error, <<"No test worker found.">>};
                Pid ->
                    Pid ! {update, Req},
                    {ok, Pid}
            end
    end.

%% @doc Find a test worker's PID and send it an increment message.
-spec increment_counter(#{ _ => _ }, #{ 'test-id' => _, _ => _ }, #{ _ => _ }) ->
    {ok, ok} | {error, binary()}.
increment_counter(_Base, Req, _Opts) ->
    case hb_ao:get(<<"test-id">>, Req) of
        not_found ->
            {error, <<"No test ID found in message.">>};
        ID ->
            LookupResult = hb_name:lookup({<<"test">>, ID}),
            case LookupResult of
                undefined ->
                    {error, <<"No test worker found for increment.">>};
                Pid when is_pid(Pid) ->
                    Pid ! {increment},
				    {ok, Pid};
                _ -> % Handle case where registered value isn't a PID
                    {error, <<"Invalid registration found for test worker.">>}
            end
    end.

%% @doc Does nothing, just sleeps `Req/duration or 750' ms and returns the 
%% appropriate form in order to be used as a hook.
-spec delay(#{ _ => _ }, #{ duration => integer(), result => _, body => _, _ => _ }, #{ _ => _ }) ->
    {ok, _}.
delay(Base, Req, Opts) ->
    Duration =
        hb_ao:get_first(
            [
                {Base, <<"duration">>},
                {Req, <<"duration">>}
            ],
            750,
            Opts
        ),
    ?event(delay, {delay, {sleeping, Duration}}),
    timer:sleep(Duration),
    ?event({delay, waking}),
    Return =
        case hb_ao:get(<<"return">>, Base, Opts) of
            not_found ->
                hb_ao:get(<<"body">>, Req, #{ <<"result">> => <<"slept">> }, Opts);
            ReturnMsgs ->
                ReturnMsgs
        end,
    ?event(delay, {returning, Return}),
    {ok, Return}.

%% @doc Mangle the message by setting the first committed key to a random value.
%% We do not update the message's commitments to reflect the new value, such that
%% the message will be invalid after execution.
%% 
%% Caution: This function is not safe to use in production, as it may cause
%% state inconsistencies.
-spec mangle(#{ commitments => #{ _ => _ }, _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | {error, binary()}.
mangle(Base, _Req, Opts) ->
    case hb_opts:get(mode, prod, Opts) of
        prod -> {error, <<"`mangle' unavailable in `prod` mode.">>};
        debug ->
            ?no_prod("`mangle' is not safe to use in production."),
            case hb_message:committed(Base, #{ <<"commitment-ids">> => <<"all">> }, Opts) of
                [] ->
                    {error, <<"No committed keys to mangle found on base message.">>};
                [FirstKey|_] ->
                    MangleReference = hb_util:human_id(crypto:strong_rand_bytes(32)),
                    {
                        ok,
                        Base#{ FirstKey => <<"mangled-", MangleReference/binary>> }
                    }
            end
    end.

%% @doc Return the inputs selected by the function's schema.
-spec vary_projection(
    #{ required := integer(), optional => binary(), deep := #{ slot := integer() } },
    #{ path := binary(), deep_request := #{ slot := integer() } },
    #{ _ => _ }
) -> {ok, #{ base := #{ _ => _ }, request := #{ _ => _ } }}.
vary_projection(Base, Req, _Opts) ->
    {ok, #{ <<"base">> => Base, <<"request">> => Req }}.

%% @doc Return schema-selected inputs while retaining wildcard keys.
-spec vary_wildcard(
    #{ required := integer(), child => #{ slot := integer(), commitments => _ },
        _ => _ },
    #{ path := binary(), optional => integer(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ base := #{ _ => _ }, request := #{ _ => _ } }}.
vary_wildcard(Base, Req, _Opts) ->
    {ok, #{ <<"base">> => Base, <<"request">> => Req }}.

%% @doc Return the inputs without declaring a Vary schema.
vary_unspecified(Base, Req, _Opts) ->
    {ok, #{ <<"base">> => Base, <<"request">> => Req }}.

%% @doc Increment a counter in a projection of the base, returning a patch
%% that the resolver lays over the whole base.
-spec vary_overlay(#{ counter := integer() }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ '...' := base, counter := integer() }}.
vary_overlay(Base = #{ <<"counter">> := Counter }, _Req, _Opts) ->
    {ok, Base#{ <<"counter">> => Counter + 1 }}.

%%% Tests

%% @doc Tests the resolution of a default function.
device_with_function_key_module_test() ->
	Msg =
		#{
			<<"device">> => <<"test-device@1.0">>
		},
	?assertEqual(
		{ok, <<"GOOD FUNCTION">>},
		hb_ao:resolve(Msg, test_func, #{})
	).

compute_test() ->
    Msg0 = #{ <<"device">> => <<"test-device@1.0">> },
    {ok, Base} = hb_ao:resolve(Msg0, init, #{}),
    Req =
        hb_ao:set(
            #{ <<"path">> => <<"compute">> },
            #{
                <<"slot">> => 1,
                <<"body/number">> => 1337
            },
            #{}
        ),
    {ok, Res} = hb_ao:resolve(Base, Req, #{}),
    ?assertEqual(1, hb_ao:get(<<"results/assignment-slot">>, Res, #{})),
    Msg4 =
        hb_ao:set(
            #{ <<"path">> => <<"compute">> },
            #{
                <<"slot">> => 2,
                <<"body/number">> => 9001
            },
            #{}
        ),
    {ok, Msg5} = hb_ao:resolve(Res, Msg4, #{}),
    ?assertEqual(2, hb_ao:get(<<"results/assignment-slot">>, Msg5, #{})),
    ?assertEqual([2, 1], hb_ao:get(<<"already-seen">>, Msg5, #{})).

restore_test() ->
    Base = #{ <<"device">> => <<"test-device@1.0">>, <<"already-seen">> => [1] },
    {ok, Res} = hb_ao:resolve(Base, <<"restore">>, #{}),
    ?assertEqual([1], hb_private:get(<<"test-key/started-state">>, Res, #{})).

%% @doc Varying either argument preserves signatures only for unchanged content.
vary_commitments_test_() ->
    Cases =
        [
            {"unchanged", inline, 7, <<"vary-wildcard">>},
            {"loaded", lazy, 7, <<"vary-wildcard">>},
            {"coerced", inline, <<"007">>, <<"vary-wildcard">>},
            {"loaded and coerced", lazy, <<"007">>, <<"vary-wildcard">>},
            {"no schema", lazy, 7, <<"vary-unspecified">>}
        ],
    [
        {binary_to_list(Which) ++ " " ++ Name,
            fun() -> vary_commitments(Which, Mode, Value, Function) end}
    ||
        Which <- [<<"base">>, <<"request">>],
        {Name, Mode, Value, Function} <- Cases
    ].

%% @doc Check signatures and the original cache entry after varying and caching.
vary_commitments(Which, Mode, Value, Function) ->
    Wallet = ar_wallet:new(),
    Signer = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Opts = (vary_opts())#{ <<"priv-wallet">> => Wallet },
    Key =
        case Which of
            <<"base">> -> <<"required">>;
            <<"request">> -> <<"optional">>
        end,
    Signed =
        hb_message:commit(
            #{
                <<"device">> => <<"test-device@1.0">>,
                <<"path">> => Function,
                Key => Value
            },
            Opts,
            <<"httpsig@1.0">>
        ),
    SignedID = hb_message:id(Signed, [Signer], Opts),
    {ok, _} = hb_cache:write(Signed, Opts),
    {ok, Lazy} = hb_cache:read(SignedID, Opts),
    ?assertMatch({link, _, _}, maps:get(Key, Lazy)),
    ?assert(hb_message:verify(Lazy, [Signer], Opts)),
    Input =
        case Mode of
            inline -> Signed;
            lazy -> Lazy
        end,
    {Base, Request} =
        case Which of
            <<"base">> -> {Input, Function};
            <<"request">> ->
                {#{ <<"device">> => <<"test-device@1.0">>, <<"required">> => 7 }, Input}
        end,
    {ok, Res} = hb_ao:resolve(Base, Request, Opts),
    Varied = hb_maps:get(Which, Res, not_found, Opts),
    case Function of
        <<"vary-unspecified">> ->
            ?assertEqual(maps:get(Key, Lazy), maps:get(Key, Varied));
        _ -> ?assertEqual(7, maps:get(Key, Varied))
    end,
    case Value of
        7 ->
            ?assert(lists:member(Signer, hb_message:signers(Varied, Opts))),
            ?assertEqual(SignedID, hb_message:id(Varied, [Signer], Opts)),
            ?assert(hb_message:verify(Varied, [Signer], Opts));
        _ -> ?assertNot(hb_maps:is_key(<<"commitments">>, Varied, Opts))
    end,
    {ok, VariedID} = hb_cache:write(Varied, Opts),
    {ok, CachedVaried} = hb_cache:read(VariedID, Opts),
    ?assertEqual(7, hb_maps:get(Key, CachedVaried, not_found, Opts)),
    {ok, CachedOriginal} = hb_cache:read(SignedID, Opts),
    ?assertEqual(Value, hb_maps:get(Key, CachedOriginal, not_found, Opts)),
    ?assert(lists:member(Signer, hb_message:signers(CachedOriginal, Opts))),
    ?assertEqual(SignedID, hb_message:id(CachedOriginal, [Signer], Opts)),
    ?assert(hb_message:verify(CachedOriginal, [Signer], Opts)).

%% @doc Only changes to signed content invalidate parent or child commitments.
vary_child_commitments_test_() ->
    Child = #{ <<"slot">> => 7 },
    Coerced = #{ <<"slot">> => <<"007">> },
    Projected = #{ <<"slot">> => 7, <<"noise">> => 8 },
    Cases =
        [
            {"unchanged parent and child", 7, Child, inline, {true, true}},
            {"loaded child", 7, Child, lazy, {true, true}},
            {"coerced child", 7, Coerced, inline, {false, false}},
            {"loaded and coerced child", 7, Coerced, lazy, {false, false}},
            {"projected child", 7, Projected, inline, {false, false}},
            {"coerced parent preserves child", <<"007">>, Child, inline, {true, false}}
        ],
    [
        {Description,
            fun() -> vary_child_commitments(Required, Body, Mode, Keep) end}
    ||
        {Description, Required, Body, Mode, Keep} <- Cases
    ].

%% @doc Check each signed cache entry as well as the varied parent and child.
vary_child_commitments(Required, Body, Mode, {KeepChild, KeepParent}) ->
    Wallet = ar_wallet:new(),
    Signer = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Opts = (vary_opts())#{ <<"priv-wallet">> => Wallet },
    Child = hb_message:commit(Body, Opts, <<"httpsig@1.0">>),
    ChildID = hb_message:id(Child, [Signer], Opts),
    Parent =
        hb_message:commit(
            #{
                <<"device">> => <<"test-device@1.0">>,
                <<"required">> => Required,
                <<"child">> => Child
            },
            Opts,
            <<"httpsig@1.0">>
        ),
    ParentID = hb_message:id(Parent, [Signer], Opts),
    {ok, _} = hb_cache:write(Parent, Opts),
    {ok, Lazy} = hb_cache:read(ParentID, Opts),
    ?assertMatch({link, _, _}, maps:get(<<"child">>, Lazy)),
    LazyChild = hb_maps:get(<<"child">>, Lazy, not_found, Opts),
    ?assertMatch({link, _, _}, maps:get(<<"slot">>, LazyChild)),
    ?assert(hb_message:verify(Lazy, [Signer], Opts)),
    Original = hb_cache:ensure_all_loaded(Lazy, Opts),
    % Keep the parent's scalar inline so only its child needs loading.
    Input =
        case Mode of
            inline -> Parent;
            lazy -> Lazy#{ <<"required">> => Required }
        end,
    {ok, Res} = hb_ao:resolve(Input, <<"vary-wildcard">>, Opts),
    Varied = hb_maps:get(<<"base">>, Res, not_found, Opts),
    VariedChild = maps:get(<<"child">>, Varied),
    ?assertEqual(7, maps:get(<<"required">>, Varied)),
    ?assertEqual(#{ <<"slot">> => 7 }, hb_message:uncommitted(VariedChild, Opts)),
    {ok, _} = hb_cache:write(Varied, Opts),
    lists:foreach(
        fun({Message, ID, Keep, Expected}) ->
            {ok, Cached} = hb_cache:read(ID, Opts),
            ?assertEqual(Expected, hb_cache:ensure_all_loaded(Cached, Opts)),
            ?assert(lists:member(Signer, hb_message:signers(Cached, Opts))),
            ?assert(hb_message:verify(Cached, [Signer], Opts)),
            case Keep of
                true ->
                    ?assert(lists:member(Signer, hb_message:signers(Message, Opts))),
                    ?assertEqual(ID, hb_message:id(Message, [Signer], Opts)),
                    ?assert(hb_message:verify(Message, [Signer], Opts));
                false -> ?assertNot(hb_maps:is_key(<<"commitments">>, Message, Opts))
            end
        end,
        [
            {VariedChild, ChildID, KeepChild, maps:get(<<"child">>, Original)},
            {Varied, ParentID, KeepParent, Original}
        ]
    ).

vary_unspecified_function_is_identity_test() ->
    Opts = vary_opts(),
    {ok, Path} = hb_cache:write(<<"kept">>, Opts),
    Base =
        #{
            <<"device">> => <<"test-device@1.0">>,
            <<"noise">> => {link, Path, #{}}
        },
    {ok, Res} = hb_ao:resolve(Base, <<"vary-unspecified">>, Opts),
    ?assertEqual(
        Base,
        hb_private:reset(maps:get(<<"base">>, Res))
    ).

vary_overlay_patches_unvaried_base_test() ->
    Opts = vary_opts(),
    {ok, Res} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"test-device@1.0">>,
                <<"counter">> => <<"1">>,
                <<"noise">> => <<"kept">>
            },
            <<"vary-overlay">>,
            Opts
        ),
    ?assertEqual(2, hb_ao:get(<<"counter">>, Res, Opts)),
    ?assertEqual(<<"kept">>, hb_ao:get(<<"noise">>, Res, Opts)).

vary_projection_uses_projected_cache_key_test() ->
    Store = hb_test_utils:test_store(),
    Opts =
        #{
            <<"store">> => Store,
            <<"attested-store">> => hb_test_utils:test_store(),
            <<"cache-control">> => [<<"always">>],
            <<"spawn-worker">> => false
        },
    Base =
        #{
            <<"device">> => <<"test-device@1.0">>,
            <<"required">> => <<"7">>,
            <<"deep">> => #{ <<"slot">> => <<"8">> },
            <<"noise">> => <<"first">>
        },
    Req =
        #{
            <<"path">> => <<"vary-projection">>,
            <<"deep-request">> => #{ <<"slot">> => <<"9">> }
        },
    {ok, First} = hb_ao:resolve(Base, Req, Opts),
    {ok, Second} =
        hb_ao:resolve(
            Base#{ <<"noise">> => <<"second">> },
            Req,
            Opts#{ <<"cache-control">> => [<<"only-if-cached">>] }
        ),
    ?assertEqual(7, hb_ao:get(<<"base/required">>, Second, Opts)),
    ?assertEqual(hb_path:hashpath(First, Opts), hb_path:hashpath(Second, Opts)),
    ?assertEqual(8, hb_ao:get(<<"base/deep/slot">>, Second, Opts)),
    ?assertEqual(9, hb_ao:get(<<"request/deep-request/slot">>, Second, Opts)).

vary_opts() ->
    Store = hb_test_utils:test_store(),
    hb_store:reset(Store),
    #{
        <<"store">> => Store,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"spawn-worker">> => false
    }.
