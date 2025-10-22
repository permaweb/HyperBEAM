%%% @doc A device that contains a stack of other devices, and manages their
%%% execution. It can run in two modes: fold (the default), and map.
%%% 
%%% In fold mode, it runs upon input messages in the order of their keys. A
%%% stack maintains and passes forward a state (expressed as a message) as it
%%% progresses through devices.
%%%
%%% For example, a stack of devices as follows:
%%% <pre>
%%% Device -> Stack
%%% Device-Stack/1/Name -> Add-One-Device
%%% Device-Stack/2/Name -> Add-Two-Device
%%% </pre>
%%% 
%%% When called with the message:
%%% <pre>
%%% #{ Path = "FuncName", binary => `<<"0">>' }
%%% </pre>
%%% 
%%% Will produce the output:
%%% <pre>
%%% #{ Path = "FuncName", binary => `<<"3">>' }
%%% {ok, #{ bin => `<<"3">>' }}
%%% </pre>
%%% 
%%% In map mode, the stack will run over all the devices in the stack, and
%%% combine their results into a single message. Each of the devices'
%%% output values have a key that is the device's name in the `Device-Stack'
%%% (its number if the stack is a list).
%%% 
%%% You can switch between fold and map modes by setting the `Mode' key in the
%%% `Req' to either `Fold' or `Map', or set it globally for the stack by
%%% setting the `Mode' key in the `Base' message. The key in `Req' takes
%%% precedence over the key in `Base'.
%%%
%%% The key that is called upon the device stack is the same key that is used
%%% upon the devices that are contained within it. For example, in the above
%%% scenario we resolve FuncName on the stack, leading FuncName to be called on
%%% Add-One-Device and Add-Two-Device.
%%%
%%% A device stack responds to special statuses upon responses as follows:
%%%
%%%     `skip': Skips the rest of the device stack for the current pass.
%%% 
%%%     `pass': Causes the stack to increment its pass number and re-execute
%%%             the stack from the first device, maintaining the state 
%%%             accumulated so far. Only available in fold mode.
%%%
%%% In all cases, the device stack will return the accumulated state to the
%%% caller as the result of the call to the stack.
%%%
%%% The dev_stack adds additional metadata to the message in order to track
%%% the state of its execution as it progresses through devices. These keys
%%% are as follows:
%%%
%%%     `Stack-Pass': The number of times the stack has reset and re-executed
%%%     from the first device for the current message.
%%%
%%%     `Input-Prefix': The prefix that the device should use for its outputs
%%%     and inputs.
%%%
%%%     `Output-Prefix': The device that was previously executed.
%%%
%%% All counters used by the stack are initialized to 1.
%%%
%%% Additionally, as implemented in HyperBEAM, the device stack will honor a
%%% number of options that are passed to it as keys in the message. Each of
%%% these options is also passed through to the devices contained within the
%%% stack during execution. These options include:
%%%
%%%     `Error-Strategy': Determines how the stack handles errors from devices.
%%%     See `maybe_error/5' for more information.
%%% 
%%%     `Allow-Multipass': Determines whether the stack is allowed to automatically
%%%     re-execute from the first device when the `pass' tag is returned. See
%%%     `maybe_pass/3' for more information.
%%%
%%% Under-the-hood, dev_stack uses a `default' handler to resolve all calls to
%%% devices, aside `set/2' which it calls itself to mutate the message's `device'
%%% key in order to change which device is currently being executed. This method
%%% allows dev_stack to ensure that the message's HashPath is always correct,
%%% even as it delegates calls to other devices. An example flow for a `dev_stack'
%%% execution is as follows:
%%% <pre>
%%% 	/Base/AlicesExcitingKey ->
%%% 		dev_stack:execute ->
%%% 			/Base/Set?device=/Device-Stack/1 ->
%%% 			/Req/AlicesExcitingKey ->
%%% 			/Res/Set?device=/Device-Stack/2 ->
%%% 			/Msg4/AlicesExcitingKey
%%% 			... ->
%%% 			/MsgN/Set?device=[This-Device] ->
%%% 		returns {ok, /MsgN+1} ->
%%% 	/MsgN+1
%%% </pre>
%%%
%%% In this example, the `device' key is mutated a number of times, but the
%%% resulting HashPath remains correct and verifiable.
-module(dev_stack).
-export([info/2, router/4, prefix/3, input_prefix/3, output_prefix/3]).
%%% Test exports
-export([generate_append_device/1]).
-include_lib("eunit/include/eunit.hrl").

-include("include/hb.hrl").

info(Msg, Opts) ->
    hb_maps:merge(
        #{
            handler => fun router/4,
            excludes => [<<"set">>, <<"keys">>]
        },
        case hb_maps:get(<<"stack-keys">>, Msg, not_found, Opts) of
            not_found -> #{};
            StackKeys -> #{ exports => StackKeys }
        end
    ).

%% @doc Return the default prefix for the stack.
prefix(Base, _Req, Opts) ->
    hb_ao:get(<<"output-prefix">>, {as, dev_message, Base}, <<"">>, Opts).

%% @doc Return the input prefix for the stack.
input_prefix(Base, _Req, Opts) ->
    hb_ao:get(<<"input-prefix">>, {as, dev_message, Base}, <<"">>, Opts).

%% @doc Return the output prefix for the stack.
output_prefix(Base, _Req, Opts) ->
    hb_ao:get(<<"output-prefix">>, {as, dev_message, Base}, <<"">>, Opts).

%% @doc The device stack key router. Sends the request to `resolve_stack',
%% except for `set/2' which is handled by the default implementation in
%% `dev_message'.
router(<<"keys">>, Base, Request, Opts) ->
	?event({keys_called, {base, Base}, {req, Request}}),
	dev_message:keys(Base, Opts);
router(Key, Base, Request, Opts) ->
    case hb_path:matches(Key, <<"transform">>) of
        true -> transformer_message(Base, Opts);
        false -> router(Base, Request, Opts)
    end.
router(Base, Request, Opts) ->
	?event({router_called, {base, Base}, {req, Request}}),
    Mode =
        case hb_ao:get(<<"mode">>, Request, not_found, Opts) of
            not_found ->
                hb_ao:get(
                    <<"mode">>,
                    {as, dev_message, Base},
                    <<"Fold">>,
                    Opts
                );
            ReqMode -> ReqMode
        end,
    case Mode of
        <<"Fold">> -> resolve_fold(Base, Request, Opts);
        <<"Map">> -> resolve_map(Base, Request, Opts)
    end.

%% @doc Return a message which, when given a key, will transform the message
%% such that the device named `Key' from the `Device-Stack' key in the message
%% takes the place of the original `Device' key. This allows users to call
%% a single device from the stack:
%%
%% 	/Base/Transform/DeviceName/keyInDevice ->
%% 		keyInDevice executed on DeviceName against Base.
transformer_message(Base, Opts) ->
	?event({creating_transformer, {for, Base}}),
    BaseInfo = info(Base, Opts),
	{ok, 
		Base#{
			<<"device">> => #{
				info =>
					fun() ->
                        hb_maps:merge(
                            BaseInfo,
                            #{
                                handler =>
                                    fun(Key, MsgX1) ->
                                        transform(MsgX1, Key, Opts)
                                    end
                            },
							Opts
                        )
					end,
				<<"type">> => <<"stack-transformer">>
			}
		}
	}.

%% @doc Return Base, transformed such that the device named `Key' from the
%% `Device-Stack' key in the message takes the place of the original `Device'
%% key. This transformation allows dev_stack to correctly track the HashPath
%% of the message as it delegates execution to devices contained within it.
transform(Base, Key, Opts) ->
	% Get the device stack message from Base.
    ?event({transforming_stack, {key, Key}, {base, Base}, {opts, Opts}}),
	case hb_ao:get(<<"device-stack">>, {as, dev_message, Base}, Opts) of
        not_found -> throw({error, no_valid_device_stack});
        StackMsg ->
			% Find the requested key in the device stack.
            % TODO: Should we use `as dev_message` here? After the first transform
            % of a fold (for example), the message is no longer a stack, so its 
            % `GET' behavior may be different.
            NormKey = hb_ao:normalize_key(Key),
			case hb_ao:resolve(StackMsg, #{ <<"path">> => NormKey }, Opts) of
				{ok, DevMsg} ->
					% Set the:
					% - Device key to the device we found.
					% - Device-Stack-Previous key to the device we are replacing.
                    % - The prefixes for the device.
                    % - The prior prefixes for later restoration.
					?event({activating_device, DevMsg}),
					dev_message:set(
                        Base,
						#{
							<<"device">> => DevMsg,
                            <<"device-key">> => Key,
                            <<"input-prefix">> =>
                                hb_ao:get(
                                    [<<"input-prefixes">>, Key],
                                    {as, dev_message, Base},
                                    undefined,
                                    Opts
                                ),
                            <<"output-prefix">> =>
                                hb_ao:get(
                                    [<<"output-prefixes">>, Key],
                                    {as, dev_message, Base},
                                    undefined,
                                    Opts
                                ),
                            <<"previous-device">> =>
                                hb_ao:get(
                                    <<"device">>,
                                    {as, dev_message, Base},
                                    Opts
                                ),
                            <<"previous-input-prefix">> =>
                                hb_ao:get(
                                    <<"input-prefix">>,
                                    {as, dev_message, Base},
                                    undefined,
                                    Opts
                                ),
                            <<"previous-output-prefix">> =>
                                hb_ao:get(
                                    <<"output-prefix">>,
                                    {as, dev_message, Base},
                                    undefined,
                                    Opts
                                )
						},
                        Opts
                    );
				_ ->
					?event({no_device_key, Key, {stack, StackMsg}}),
					not_found
			end
	end.

%% @doc The main device stack execution engine. See the moduledoc for more
%% information.
resolve_fold(Base, Request, Opts) ->
	{ok, InitDevMsg} = dev_message:get(<<"device">>, Base, Opts),
    StartingPassValue =
        hb_ao:get(<<"pass">>, {as, dev_message, Base}, unset, Opts),
    PreparedMessage = hb_ao:set(Base, <<"pass">>, 1, Opts),
    case resolve_fold(PreparedMessage, Request, 1, Opts) of
        {ok, Raw} when not is_map(Raw) ->
            {ok, Raw};
        {ok, Result} ->
            dev_message:set(
                Result,
                #{
                    <<"device">> => InitDevMsg,
                    <<"input-prefix">> =>
                        hb_ao:get(
                            <<"previous-input-prefix">>,
                            {as, dev_message, Result},
                            undefined,
                            Opts
                        ),
                    <<"output-prefix">> =>
                        hb_ao:get(
                            <<"previous-output-prefix">>,
                            {as, dev_message, Result},
                            undefined,
                            Opts
                        ),
                    <<"device-key">> => unset,
                    <<"device-stack-previous">> => unset,
                    <<"pass">> => StartingPassValue
                },
                Opts
            );
        Else ->
            Else
    end.
resolve_fold(Base, Request, DevNum, Opts) ->
	case transform(Base, DevNum, Opts) of
		{ok, Result} ->
			?event({stack_execute, DevNum, {base, Result}, {req, Request}}),
			case hb_ao:resolve(Result, Request, Opts) of
				{ok, Message4} when is_map(Message4) ->
					?event({result, ok, DevNum, Message4}),
					resolve_fold(Message4, Request, DevNum + 1, Opts);
                {error, not_found} ->
                    ?event({skipping_device, not_found, DevNum, Result}),
                    resolve_fold(Result, Request, DevNum + 1, Opts);
                {ok, RawResult} ->
                    ?event({returning_raw_result, RawResult}),
                    {ok, RawResult};
				{skip, Message4} when is_map(Message4) ->
					?event({result, skip, DevNum, Message4}),
					{ok, Message4};
				{pass, Message4} when is_map(Message4) ->
                    ?event({result, pass, {dev, DevNum}, Message4}),
                    resolve_fold(
                        increment_pass(Message4, Opts),
                        Request,
                        1,
                        Opts
                    );
				{error, Info} ->
					?event({result, error, {dev, DevNum}, Info}),
					maybe_error(Base, Request, DevNum, Info, Opts);
				Unexpected ->
					?event({result, unexpected, {dev, DevNum}, Unexpected}),
					maybe_error(
						Base,
						Request,
						DevNum,
						{unexpected_result, Unexpected},
						Opts
					)
			end;
		not_found ->
			?event({execution_complete, DevNum, Base}),
			{ok, Base}
	end.

%% @doc Map over the devices in the stack, accumulating the output in a single
%% message of keys and values, where keys are the same as the keys in the
%% original message (typically a number).
resolve_map(Base, Request, Opts) ->
    ?event({resolving_map, {base, Base}, {req, Request}}),
    DevKeys =
        hb_ao:get(
            <<"device-stack">>,
            {as, dev_message, Base},
            Opts
        ),
    Res = {ok,
        hb_maps:filtermap(
            fun(Key, _Dev) ->
                {ok, OrigWithDev} = transform(Base, Key, Opts),
                case hb_ao:resolve(OrigWithDev, Request, Opts) of
                    {ok, Value} -> {true, Value};
                    _ -> false
                end
            end,
            hb_maps:without(?AO_CORE_KEYS, hb_ao:normalize_keys(DevKeys, Opts), Opts),
			Opts
        )
    },
    Res.

%% @doc Helper to increment the pass number.
increment_pass(Message, Opts) ->
    hb_ao:set(
        Message,
        #{ <<"pass">> => hb_ao:get(<<"pass">>, {as, dev_message, Message}, 1, Opts) + 1 },
        Opts
    ).

maybe_error(Base, Request, DevNum, Info, Opts) ->
    case hb_opts:get(error_strategy, throw, Opts) of
        stop ->
			{error, {stack_call_failed, Base, Request, DevNum, Info}};
        throw ->
			erlang:raise(
                error,
                {device_failed,
                    {dev_num, DevNum},
                    {base, Base},
                    {req, Request},
                    {info, Info}
                },
                []
            )
    end.

%%% Tests

generate_append_device(Separator) ->
	generate_append_device(Separator, ok).
generate_append_device(Separator, Status) ->
	#{
		append =>
			fun(M1 = #{ <<"pass">> := 3 }, _) ->
                % Stop after 3 passes.
                {ok, M1};
			   (M1 = #{ <<"result">> := Existing }, #{ <<"bin">> := New }) ->
				?event({appending, {existing, Existing}, {new, New}}),
				{Status, M1#{ <<"result">> =>
					<< Existing/binary, Separator/binary, New/binary>>
				}}
			end
	}.

%% @doc Test that the transform function can be called correctly internally
%% by other functions in the module.
transform_internal_call_device_test() ->
	AppendDev = generate_append_device(<<"_">>),
	Base =
		#{
			<<"device">> => <<"stack@1.0">>,
			<<"device-stack">> =>
				#{
					<<"1">> => AppendDev,
					<<"2">> => <<"message@1.0">>
				}
		},
	?assertMatch(
		<<"message@1.0">>,
		hb_ao:get(
			<<"device">>,
			element(2, transform(Base, <<"2">>, #{}))
		)
	).

%% @doc Ensure we can generate a transformer message that can be called to
%% return a version of base with only that device attached.
transform_external_call_device_test() ->
	Base = #{
		<<"device">> => <<"stack@1.0">>,
		<<"device-stack">> =>
			#{
				<<"make-cool">> =>
					#{
						info =>
							fun() ->
								#{
									handler =>
										fun(<<"keys">>, MsgX1) ->
                                            ?event({test_dev_keys_called, MsgX1}),
											{ok, hb_maps:keys(MsgX1, #{})};
										(Key, MsgX1) ->
											{ok, Value} =
												dev_message:get(Key, MsgX1, #{}),
											dev_message:set(
												MsgX1,
												#{ Key =>
													<< Value/binary, "-Cool">>
												},
												#{}
											)
										end
								}
							end,
						<<"suffix">> => <<"-Cool">>
					}
			},
		<<"value">> => <<"Super">>
	},
	?assertMatch(
		{ok, #{ <<"value">> := <<"Super-Cool">> }},
		hb_ao:resolve(Base, #{
			<<"path">> => <<"/transform/make-cool/value">>
		}, #{})
	).

example_device_for_stack_test() ->
	% Test the example device that we use for later stack tests, such that
	% we know that an error later is actually from the stack, and not from
	% the example device.
	?assertMatch(
		{ok, #{ <<"result">> := <<"1_2">> }},
		hb_ao:resolve(
			#{ <<"device">> => generate_append_device(<<"_">>), <<"result">> => <<"1">> },
			#{ <<"path">> => <<"append">>, <<"bin">> => <<"2">> },
			#{}
		)
	).

simple_stack_execute_test() ->
	Msg = #{
		<<"device">> => <<"stack@1.0">>,
		<<"device-stack">> =>
			#{
				<<"1">> => generate_append_device(<<"!D1!">>),
				<<"2">> => generate_append_device(<<"_D2_">>)
			},
		<<"result">> => <<"INIT">>
	},
	?event({stack_executing, test, {explicit, Msg}}),
	?assertMatch(
		{ok, #{ <<"result">> := <<"INIT!D1!2_D2_2">> }},
		hb_ao:resolve(Msg, #{ <<"path">> => <<"append">>, <<"bin">> => <<"2">> }, #{})
	).

many_devices_test() ->
	Msg = #{
		<<"device">> => <<"stack@1.0">>,
		<<"device-stack">> =>
			#{
				<<"1">> => generate_append_device(<<"+D1">>),
				<<"2">> => generate_append_device(<<"+D2">>),
				<<"3">> => generate_append_device(<<"+D3">>),
				<<"4">> => generate_append_device(<<"+D4">>),
				<<"5">> => generate_append_device(<<"+D5">>),
				<<"6">> => generate_append_device(<<"+D6">>),
				<<"7">> => generate_append_device(<<"+D7">>),
				<<"8">> => generate_append_device(<<"+D8">>)
			},
		<<"result">> => <<"INIT">>
	},
	?assertMatch(
		{ok,
			#{
				<<"result">> :=
					<<"INIT+D12+D22+D32+D42+D52+D62+D72+D82">>
			}
		},
		hb_ao:resolve(Msg, #{ <<"path">> => <<"append">>, <<"bin">> => <<"2">> }, #{})
	).

benchmark_test() ->
    BenchTime = 0.3,
	Msg = #{
		<<"device">> => <<"stack@1.0">>,
		<<"device-stack">> =>
			#{
				<<"1">> => generate_append_device(<<"+D1">>),
				<<"2">> => generate_append_device(<<"+D2">>),
				<<"3">> => generate_append_device(<<"+D3">>),
				<<"4">> => generate_append_device(<<"+D4">>),
				<<"5">> => generate_append_device(<<"+D5">>)
			},
		<<"result">> => <<"INIT">>
	},
    Iterations =
        hb_test_utils:benchmark(
            fun() ->
                hb_ao:resolve(Msg,
                    #{
                        <<"path">> => <<"append">>,
                        <<"bin">> => <<"2">>
                    },
                    #{}
                ),
                {count, 5}
            end,
            BenchTime
        ),
    hb_test_utils:benchmark_print(
        <<"Stack:">>,
        <<"resolutions">>,
        Iterations,
        BenchTime
    ),
    ?assert(Iterations >= 10).


test_prefix_msg() ->
    Dev = #{
        prefix_set =>
            fun(M1, M2, Opts) ->
                In = input_prefix(M1, M2, Opts),
                Out = output_prefix(M1, M2, Opts),
                Key = hb_ao:get(<<"key">>, M2, Opts),
                Value = hb_ao:get(<<In/binary, "/", Key/binary>>, M2, Opts),
                ?event({setting, {inp, In}, {outp, Out}, {key, Key}, {value, Value}}),
                {ok, hb_ao:set(
                    M1,
                    <<Out/binary, "/", Key/binary>>,
                    Value,
                    Opts
                )}
            end
    },
    #{
        <<"device">> => <<"stack@1.0">>,
        <<"device-stack">> => #{ <<"1">> => Dev, <<"2">> => Dev }
    }.

no_prefix_test() ->
    Req =
        #{
            <<"path">> => <<"prefix_set">>,
            <<"key">> => <<"example">>,
            <<"example">> => 1
        },
    {ok, Ex1Res} = hb_ao:resolve(test_prefix_msg(), Req, #{}),
    ?event({ex1, Ex1Res}),
    ?assertMatch(1, hb_ao:get(<<"example">>, Ex1Res, #{})).

output_prefix_test() ->
    Base =
        (test_prefix_msg())#{
            <<"output-prefixes">> => #{ <<"1">> => <<"out1/">>, <<"2">> => <<"out2/">> }
        },
    Req =
        #{
            <<"path">> => <<"prefix_set">>,
            <<"key">> => <<"example">>,
            <<"example">> => 1
        },
    {ok, Ex2Res} = hb_ao:resolve(Base, Req, #{}),
    ?assertMatch(1,
        hb_ao:get(<<"out1/example">>, {as, dev_message, Ex2Res}, #{})),
    ?assertMatch(1,
        hb_ao:get(<<"out2/example">>, {as, dev_message, Ex2Res}, #{})).

input_and_output_prefixes_test() ->
    Base =
        (test_prefix_msg())#{
            <<"input-prefixes">> => #{ 1 => <<"in1/">>, 2 => <<"in2/">> },
            <<"output-prefixes">> => #{ 1 => <<"out1/">>, 2 => <<"out2/">> }
        },
    Req =
        #{
            <<"path">> => <<"prefix_set">>,
            <<"key">> => <<"example">>,
            <<"in1">> => #{ <<"example">> => 1 },
            <<"in2">> => #{ <<"example">> => 2 }
        },
    {ok, Res} = hb_ao:resolve(Base, Req, #{}),
    ?assertMatch(1,
        hb_ao:get(<<"out1/example">>, {as, dev_message, Res}, #{})),
    ?assertMatch(2,
        hb_ao:get(<<"out2/example">>, {as, dev_message, Res}, #{})).

input_output_prefixes_passthrough_test() ->
    Base =
        (test_prefix_msg())#{
            <<"output-prefix">> => <<"combined-out/">>,
            <<"input-prefix">> => <<"combined-in/">>
        },
    Req =
        #{
            <<"path">> => <<"prefix_set">>,
            <<"key">> => <<"example">>,
            <<"combined-in">> => #{ <<"example">> => 1 }
        },
    {ok, Ex2Res} = hb_ao:resolve(Base, Req, #{}),
    ?assertMatch(1,
        hb_ao:get(
            <<"combined-out/example">>,
            {as, dev_message, Ex2Res},
            #{}
        )
    ).

reinvocation_test() ->
	Msg = #{
		<<"device">> => <<"stack@1.0">>,
		<<"device-stack">> =>
			#{
				<<"1">> => generate_append_device(<<"+D1">>),
				<<"2">> => generate_append_device(<<"+D2">>)
			},
		<<"result">> => <<"INIT">>
	},
	Res1 = hb_ao:resolve(Msg, #{ <<"path">> => <<"append">>, <<"bin">> => <<"2">> }, #{}),
	?assertMatch(
		{ok, #{ <<"result">> := <<"INIT+D12+D22">> }},
		Res1
	),
	{ok, Req} = Res1,
	Res2 = hb_ao:resolve(Req, #{ <<"path">> => <<"append">>, <<"bin">> => <<"3">> }, #{}),
	?assertMatch(
		{ok, #{ <<"result">> := <<"INIT+D12+D22+D13+D23">> }},
		Res2
	).

skip_test() ->
	Base = #{
		<<"device">> => <<"stack@1.0">>,
		<<"device-stack">> =>
			#{
				<<"1">> => generate_append_device(<<"+D1">>, skip),
				<<"2">> => generate_append_device(<<"+D2">>)
			},
		<<"result">> => <<"INIT">>
	},
	?assertMatch(
		{ok, #{ <<"result">> := <<"INIT+D12">> }},
		hb_ao:resolve(
			Base,
			#{ <<"path">> => <<"append">>, <<"bin">> => <<"2">> },
            #{}
		)
	).

pass_test() ->
    % The append device will return `ok' after 2 passes, so this test
    % recursively calls the device by forcing its response to be `pass'
    % until that happens.
	Msg = #{
		<<"device">> => <<"stack@1.0">>,
		<<"device-stack">> =>
			#{
				<<"1">> => generate_append_device(<<"+D1">>, pass)
			},
		<<"result">> => <<"INIT">>
	},
	?assertMatch(
		{ok, #{ <<"result">> := <<"INIT+D1_+D1_">> }},
		hb_ao:resolve(Msg, #{ <<"path">> => <<"append">>, <<"bin">> => <<"_">> }, #{})
	).

not_found_test() ->
    % Ensure that devices not exposing a key are safely skipped.
	Msg = #{
		<<"device">> => <<"stack@1.0">>,
		<<"device-stack">> =>
			#{
				<<"1">> => generate_append_device(<<"+D1">>),
				<<"2">> =>
                    (generate_append_device(<<"+D2">>))#{
                        <<"special">> =>
                            fun(M1) ->
                                {ok, M1#{ <<"output">> => 1337 }}
                            end
                    }
			},
		<<"result">> => <<"INIT">>
	},
    {ok, Res} = hb_ao:resolve(Msg, #{ <<"path">> => <<"append">>, <<"bin">> => <<"_">> }, #{}),
    ?assertMatch(
		#{ <<"result">> := <<"INIT+D1_+D2_">> },
		Res
	),
    ?event({ex3, Res}),
    ?assertEqual(1337, hb_ao:get(<<"special/output">>, Res, #{})).

simple_map_test() ->
    Msg = #{
        <<"device">> => <<"stack@1.0">>,
        <<"device-stack">> =>
            #{
                <<"1">> => generate_append_device(<<"+D1">>),
                <<"2">> => generate_append_device(<<"+D2">>)
            },
        <<"result">> => <<"INIT">>
    },
    {ok, Res} =
        hb_ao:resolve(
            Msg,
            #{ <<"path">> => <<"append">>, <<"mode">> => <<"Map">>, <<"bin">> => <<"/">> },
            #{}
        ),
    ?assertMatch(<<"INIT+D1/">>, hb_ao:get(<<"1/result">>, Res, #{})),
    ?assertMatch(<<"INIT+D2/">>, hb_ao:get(<<"2/result">>, Res, #{})).