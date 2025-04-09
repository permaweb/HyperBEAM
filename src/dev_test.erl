-module(dev_test).
-export([info/1, test_func/1, compute/3, init/3, restore/3, snapshot/3, mul/2]).
-export([postprocess/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% A simple test device for AO-Core, so that we can test the functionality that
%%% depends on using Erlang's module system.
%%% 
%%% NOTE: This device is labelled `Test-Device/1.0` to avoid conflicts with
%%% other testing functionality -- care should equally be taken to avoid
%%% using the `test` key in other settings.

-doc """
Exports a default_handler function that can be used to test the
handler resolution mechanism.
""".
info(_) ->
	#{
        <<"default">> => dev_message
	}.

test_func(_) ->
	{ok, <<"GOOD_FUNCTION">>}.

-doc """
Example implementation of a `compute` handler. Makes a running list of
the slots that have been computed in the state message and places the new
slot number in the results key.
""".
compute(Msg1, Msg2, Opts) ->
    AssignmentSlot = hb_ao:get(<<"slot">>, Msg2, Opts),
    Seen = hb_ao:get(<<"already-seen">>, Msg1, Opts),
    ?event({compute_called, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    {ok,
        hb_ao:set(
            Msg1,
            #{
                <<"random-key">> => <<"random-value">>,
                <<"results">> =>
                    #{ <<"assignment-slot">> => AssignmentSlot },
                <<"already-seen">> => [AssignmentSlot | Seen]
            },
            Opts
        )
    }.

-doc """
Example `init/3` handler. Sets the `Already-Seen` key to an empty list.
""".
init(Msg, _Msg2, Opts) ->
    ?event({init_called_on_dev_test, Msg}),
    {ok, hb_ao:set(Msg, #{ <<"already-seen">> => [] }, Opts)}.

-doc """
Example `restore/3` handler. Sets the hidden key `Test/Started` to the
value of `Current-Slot` and checks whether the `Already-Seen` key is valid.
""".
restore(Msg, _Msg2, Opts) ->
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

-doc """
Example implementation of an `imported` function for a WASM
executor.
""".
mul(Msg1, Msg2) ->
    ?event(mul_called),
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    [Arg1, Arg2] = hb_ao:get(<<"args">>, Msg2, #{ hashpath => ignore }),
    ?event({mul_called, {state, State}, {args, [Arg1, Arg2]}}),
    {ok, #{ <<"state">> => State, <<"results">> => [Arg1 * Arg2] }}.

-doc """
Do nothing when asked to snapshot.
""".
snapshot(_Msg1, _Msg2, _Opts) ->
    {ok, #{}}.

-doc """
Set the `postprocessor-called` key to true in the HTTP server.
""".
postprocess(_Msg, #{ <<"body">> := Msgs }, Opts) ->
    ?event({postprocess_called, Opts}),
    hb_http_server:set_opts(Opts#{ <<"postprocessor-called">> => true }),
    {ok, Msgs}.

%%% Tests

-doc """
Tests the resolution of a default function.
""".
device_with_function_key_module_test() ->
	Msg =
		#{
			<<"device">> => <<"Test-Device@1.0">>
		},
	?assertEqual(
		{ok, <<"GOOD_FUNCTION">>},
		hb_ao:resolve(Msg, test_func, #{})
	).

compute_test() ->
    Msg0 = #{ <<"device">> => <<"Test-Device@1.0">> },
    {ok, Msg1} = hb_ao:resolve(Msg0, init, #{}),
    Msg2 =
        hb_ao:set(
            #{ <<"path">> => <<"compute">> },
            #{
                <<"slot">> => 1,
                <<"body/number">> => 1337
            },
            #{}
        ),
    {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, #{}),
    ?assertEqual(1, hb_ao:get(<<"results/assignment-slot">>, Msg3, #{})),
    Msg4 =
        hb_ao:set(
            #{ <<"path">> => <<"compute">> },
            #{
                <<"slot">> => 2,
                <<"body/number">> => 9001
            },
            #{}
        ),
    {ok, Msg5} = hb_ao:resolve(Msg3, Msg4, #{}),
    ?assertEqual(2, hb_ao:get(<<"results/assignment-slot">>, Msg5, #{})),
    ?assertEqual([2, 1], hb_ao:get(<<"already-seen">>, Msg5, #{})).

restore_test() ->
    Msg1 = #{ <<"device">> => <<"Test-Device@1.0">>, <<"already-seen">> => [1] },
    {ok, Msg3} = hb_ao:resolve(Msg1, <<"restore">>, #{}),
    ?assertEqual([1], hb_private:get(<<"test-key/started-state">>, Msg3, #{})).
