-module(dev_test).
-implements(<<"test-device@1.0">>).
-export([info/3]).
-export([info/1, test_func/1, compute/3, init/3, restore/3, snapshot/3, mul/2]).
-export([mangle/3, update_state/3, increment_counter/3, delay/3, append/3]).
-export([index/3, postprocess/3, load/3]).
-export([varied/3, varied_request/3, compute_nested/3, compute_all/3]).
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
-spec info(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ status := integer(), body := #{ _ => _ } }}.
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

-spec varied(#{ x := integer() }, #{}, _) -> {ok, #{ x := integer(), _ => base }}.
varied(#{ <<"x">> := X }, _Req, _Opts) ->
    {ok, #{ <<"x">> => X + 1 }}.

-spec varied_request(#{}, #{ x := integer() }, _) -> {ok, #{ y := integer(), _ => request }}.
varied_request(_Base, #{ <<"x">> := X }, _Opts) ->
    {ok, #{ <<"y">> => X + 1 }}.

%% @doc Example implementation of a `compute' handler. Makes a running list of
%% the slots that have been computed in the state message and places the new
%% slot number in the results key.
-spec compute(#{ 'already-seen' => [integer()], _ => _ }, #{ slot := integer() }, #{ _ => _ }) ->
    {ok, #{ 'already-seen' := [integer()], results := #{ 'assignment-slot' := integer() }, _ => _ }}.
compute(Base, Req, Opts) ->
    AssignmentSlot = maps:get(<<"slot">>, Req),
    Seen = maps:get(<<"already-seen">>, Base, []),
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

-spec compute_nested(
    #{ 'already-seen' => [integer()], _ => _ },
    #{ outer := #{ slot := integer() } },
    #{ _ => _ }
) -> {ok, #{ 'already-seen' := [integer()], results := #{ 'assignment-slot' := integer() }, _ => _ }}.
compute_nested(Base, Req, Opts) ->
        AssignmentSlot = maps:get(<<"slot">>, maps:get(<<"outer">>, Req)),
        Seen = maps:get(<<"already-seen">>, Base, []),
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

-spec compute_all(#{ a => integer(), _ => _ }, #{ slot := integer(), _ => _ }, #{ _ => _ }) ->
    {ok, #{ all := binary(), _ => _ }}.
compute_all(Base, Req, Opts) ->
    {ok, Base#{ <<"all">> => <<"done">> }}.

-spec compute_all_nested(
    #{ nested := #{ a := integer() }, _ => _ },
    #{ slot := integer(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ nested := #{ all := binary() }, _ => _ }}.
compute_all_nested(Base, Req, Opts) ->
    {ok, Base#{ <<"nested">> => #{ <<"all">> => <<"done">> } }}.
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

varied_overlay_cache_test() ->
    Store = hb_test_utils:test_store(),
    Opts =
        #{
            store => [Store],
            priv_wallet => hb:wallet(),
            cache_control => [<<"always">>]
        },
    Req = #{ <<"path">> => <<"varied">> },
    Base1 =
        #{
            <<"device">> => <<"test-device@1.0">>,
            <<"x">> => <<"1">>,
            <<"keep">> => <<"first">>
        },
    {ok, Res1} = hb_ao:resolve(Base1, Req, Opts),
    ?assertEqual(2, maps:get(<<"x">>, Res1)),
    % `keep' is inherited from the base, which the result now extends via `...',
    % so it resolves through the extension-aware accessor rather than directly.
    ?assertEqual(<<"first">>, hb_maps:get(<<"keep">>, Res1, undefined, Opts)),
    Base2 = Base1#{ <<"keep">> => <<"second">> },
    {ok, Res2} =
        hb_ao:resolve(
            Base2,
            Req,
            Opts#{ cache_control => [<<"only-if-cached">>] }
        ),
    ?assertEqual(2, maps:get(<<"x">>, Res2)),
    ?assertEqual(<<"second">>, hb_maps:get(<<"keep">>, Res2, undefined, Opts)).

varied_request_overlay_hashpath_test() ->
    Opts =
        #{
            store => [hb_test_utils:test_store()],
            priv_wallet => hb:wallet()
        },
    Base = #{ <<"device">> => <<"test-device@1.0">> },
    Req =
        #{
            <<"path">> => <<"varied-request">>,
            <<"x">> => <<"1">>,
            <<"keep">> => <<"request">>
        },
    {ok, Res} = hb_ao:resolve(Base, Req, Opts),
    ?assertEqual(2, maps:get(<<"y">>, Res)),
    % `keep' is inherited from the request, which the result now extends via `...'.
    ?assertEqual(<<"request">>, hb_maps:get(<<"keep">>, Res, undefined, Opts)),
    VariedBase = hb_message:normalize_commitments(Base, Opts, fast),
    ?assertEqual(
        hb_path:hashpath(VariedBase, Req, Opts),
        hb_path:hashpath(Res, Opts)
    ).

restore_test() ->
    Base = #{ <<"device">> => <<"test-device@1.0">>, <<"already-seen">> => [1] },
    {ok, Res} = hb_ao:resolve(Base, <<"restore">>, #{}),
    ?assertEqual([1], hb_private:get(<<"test-key/started-state">>, Res, #{})).
