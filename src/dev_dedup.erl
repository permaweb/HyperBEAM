%%% @doc A device that deduplicates messages send to a process.
%%% Only runs on the first pass of the `compute' key call if executed
%%% in a stack. Currently the device stores its list of already seen 
%%% items in memory, but at some point it will likely make sense to 
%%% drop them in the cache.
-module(dev_dedup).
-export([info/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

info(_M1) ->
    #{
        handler => fun handle/4
    }.

%% @doc Forward the keys function to the message device, handle all others
%% with deduplication. We only act on the first pass.
handle(<<"keys">>, M1, _M2, _Opts) ->
    dev_message:keys(M1);
handle(<<"set">>, M1, M2, Opts) ->
    dev_message:set(M1, M2, Opts);
handle(Key, M1, M2, Opts) ->
    ?event({dedup_handle, {key, Key}, {msg1, M1}, {msg2, M2}}),
    case hb_converge:get(<<"pass">>, {as, dev_message, M1}, 1, Opts) of
        1 ->
            Msg2ID = hb_message:id(M2, all),
            Dedup = hb_converge:get(<<"dedup">>, {as, dev_message, M1}, [], Opts),
            ?event({dedup_checking, {existing, Dedup}}),
            case lists:member(Msg2ID, Dedup) of
                true ->
                    ?event({already_seen, Msg2ID}),
                    {skip, M1};
                false ->
                    ?event({not_seen, Msg2ID}),
                    M3 = hb_converge:set(
                        M1,
                        #{ <<"dedup">> => [Msg2ID|Dedup] }
                    ),
                    ?event({dedup_updated, M3}),
                    {ok, M3}
            end;
        Pass ->
            ?event({multipass_detected, skipping_dedup, {pass, Pass}}),
            {ok, M1}
    end.

%%% Tests

dedup_test() ->
    hb:init(),
    % Create a stack with a dedup device and 2 devices that will append to a
    % `Result' key.
	Msg = #{
		<<"device">> => <<"Stack@1.0">>,
		<<"device-stack">> =>
			#{
				<<"1">> => <<"Dedup@1.0">>,
				<<"2">> => dev_stack:generate_append_device(<<"+D2">>),
				<<"3">> => dev_stack:generate_append_device(<<"+D3">>)
			},
		<<"result">> => <<"INIT">>
	},
    % Send the same message twice, with the same binary.
    {ok, Msg2} = hb_converge:resolve(Msg,
        #{ <<"path">> => <<"append">>, <<"bin">> => <<"_">> }, #{}),
    {ok, Msg3} = hb_converge:resolve(Msg2,
        #{ <<"path">> => <<"append">>, <<"bin">> => <<"_">> }, #{}),
    % Send the same message twice, with another binary.
    {ok, Msg4} = hb_converge:resolve(Msg3,
        #{ <<"path">> => <<"append">>, <<"bin">> => <<"/">> }, #{}),
    {ok, Msg5} = hb_converge:resolve(Msg4,
        #{ <<"path">> => <<"append">>, <<"bin">> => <<"/">> }, #{}),
    % Ensure that downstream devices have only seen each message once.
    ?assertMatch(
		#{ <<"result">> := <<"INIT+D2_+D3_+D2/+D3/">> },
		Msg5
	).

dedup_with_multipass_test() ->
    % Create a stack with a dedup device and 2 devices that will append to a
    % `Result' key and a `Multipass' device that will repeat the message for 
    % an additional pass. We want to ensure that Multipass is not hindered by
    % the dedup device.
	Msg = #{
		<<"device">> => <<"Stack@1.0">>,
		<<"device-stack">> =>
			#{
				<<"1">> => <<"Dedup@1.0">>,
				<<"2">> => dev_stack:generate_append_device(<<"+D2">>),
				<<"3">> => dev_stack:generate_append_device(<<"+D3">>),
                <<"4">> => <<"Multipass@1.0">>
			},
		<<"result">> => <<"INIT">>,
        <<"passes">> => 2
	},
    % Send the same message twice, with the same binary.
    {ok, Msg2} = hb_converge:resolve(Msg, #{ <<"path">> => <<"append">>, <<"bin">> => <<"_">> }, #{}),
    {ok, Msg3} = hb_converge:resolve(Msg2, #{ <<"path">> => <<"append">>, <<"bin">> => <<"_">> }, #{}),
    % Send the same message twice, with another binary.
    {ok, Msg4} = hb_converge:resolve(Msg3, #{ <<"path">> => <<"append">>, <<"bin">> => <<"/">> }, #{}),
    {ok, Msg5} = hb_converge:resolve(Msg4, #{ <<"path">> => <<"append">>, <<"bin">> => <<"/">> }, #{}),
    % Ensure that downstream devices have only seen each message once.
    ?assertMatch(
		#{ <<"result">> := <<"INIT+D2_+D3_+D2_+D3_+D2/+D3/+D2/+D3/">> },
		Msg5
	).