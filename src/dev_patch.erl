-module(dev_patch).
-moduledoc """
A device that finds `PATCH' requests in the `results/outbox'
of its message, and applies them to it. This can be useful for processes
whose computation would like to manipulate data outside of the `results' key
of its message.
""".
-export([init/3, compute/3, normalize/3, snapshot/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

-doc """
Default process device hooks.
""".
init(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
normalize(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
snapshot(Msg1, _Msg2, _Opts) -> {ok, Msg1}.

-doc """
Find `PATCH' requests in the `results/outbox' of the message, and apply
them to the state.
""".
compute(Msg1, Msg2, Opts) ->
    % Find the input keys.
    PatchFrom = hb_ao:get_first(
        [
            {Msg2, <<"patch-from">>},
            {Msg1, <<"patch-from">>}
        ],
        <<"/results/outbox">>,
        Opts
    ),
    PatchTo = hb_ao:get_first(
        [
            {Msg2, <<"patch-to">>},
            {Msg1, <<"patch-to">>}
        ],
        <<"/">>,
        Opts
    ),
    ?event({patch_from, PatchFrom}),
    ?event({patch_to, PatchTo}),
    % Get the outbox from the message.
    Outbox = hb_ao:get(PatchFrom, Msg1, #{}, Opts),
    % Find all messages with the PATCH request.
    Patches =
        maps:filter(
            fun(_, Msg) ->
                (hb_ao:get(<<"method">>, Msg, Opts) == <<"PATCH">>) orelse
                    (hb_ao:get(<<"device">>, Msg, Opts) == <<"patch@1.0">>)
            end,
            Outbox
        ),
    OutboxWithoutPatches = maps:without(maps:keys(Patches), Outbox),
    % Remove the outbox from the message.
    Msg1WithoutOutbox = hb_ao:set(Msg1, PatchFrom, should_never_happen, Opts),
    % Set the new outbox.
    Msg1WithNewOutbox = hb_ao:set(Msg1WithoutOutbox, PatchFrom, OutboxWithoutPatches, Opts),
    % Find the state to apply the patches to.
    % Apply the patches to the state.
    PatchedSubmessage =
        maps:fold(
            fun(_, Patch, MsgN) ->
                ?event({patching, {patch, Patch}, {before, MsgN}}),
                Res = hb_ao:set(
                    MsgN,
                    maps:without([<<"method">>], Patch),
                    Opts
                ),
                ?event({patched, {'after', Res}}),
                Res
            end,
            case PatchTo of
                not_found -> Msg1WithNewOutbox;
                PatchTo -> hb_ao:get(PatchTo, Msg1WithNewOutbox, Opts)
            end,
            Patches
        ),
    PatchedState =
        case PatchTo of
            <<"/">> -> PatchedSubmessage;
            _ -> hb_ao:set(Msg1WithNewOutbox, PatchTo, PatchedSubmessage, Opts)
        end,
    % Return the patched message and the source, less the patches.
    ?event({patch_result, PatchedState}),
    {ok, PatchedState}.

%%% Tests

uninitialized_patch_test() ->
    InitState = #{
        <<"device">> => <<"patch@1.0">>,
        <<"results">> => #{
            <<"outbox">> => #{
                <<"1">> => #{
                    <<"method">> => <<"PATCH">>,
                    <<"prices">> => #{
                        <<"apple">> => 100,
                        <<"banana">> => 200
                    }
                },
                <<"2">> => #{
                    <<"method">> => <<"GET">>,
                    <<"prices">> => #{
                        <<"apple">> => 1000
                    }
                }
            }
        },
        <<"other-message">> => <<"other-value">>,
        <<"patch-to">> => <<"/">>,
        <<"patch-from">> => <<"/results/outbox">>
    },
    {ok, ResolvedState} =
        hb_ao:resolve(
            InitState,
            <<"compute">>,
            #{}
        ),
    ?event({resolved_state, ResolvedState}),
    ?assertEqual(
        100,
        hb_ao:get(<<"prices/apple">>, ResolvedState, #{})
    ),
    ?assertMatch(
        not_found,
        hb_ao:get(<<"results/outbox/1">>, ResolvedState, #{})
    ).

patch_to_submessage_test() ->
    InitState = #{
        <<"device">> => <<"patch@1.0">>,
        <<"results">> => #{
            <<"outbox">> => #{
                <<"1">> =>
                    hb_message:commit(#{
                        <<"method">> => <<"PATCH">>,
                        <<"prices">> => #{
                            <<"apple">> => 100,
                            <<"banana">> => 200
                        }
                    },
                    hb:wallet()
                )
            }
        },
        <<"state">> => #{
            <<"prices">> => #{
                <<"apple">> => 1000
            }
        },
        <<"other-message">> => <<"other-value">>,
        <<"patch-to">> => <<"/state">>,
        <<"patch-from">> => <<"/results/outbox">>
    },
    {ok, ResolvedState} =
        hb_ao:resolve(
            InitState,
            <<"compute">>,
            #{}
        ),
    ?event({resolved_state, ResolvedState}),
    ?assertEqual(
        100,
        hb_ao:get(<<"state/prices/apple">>, ResolvedState, #{})
    ).