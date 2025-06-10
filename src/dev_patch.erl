%%% @doc A device that can be used to reorganize a message: Moving data from
%%% one path inside it to another. This device's function runs in two modes:
%%%
%%% 1. When using `all' to move all data at the path given in `from' to the
%%%    path given in `to'.
%%% 2. When using `patches' to move all submessages in the source to the target,
%%%    _if_ they have a `method' key of `PATCH' or a `device' key of `patch@1.0'.
%%%
%%% Source and destination paths may be prepended by `base:` or `req:` keys to
%%% indicate that they are relative to either of the message's that the
%%% computation is being performed on.
%%%
%%% The search order for finding the source and destination keys is as follows,
%%% where `X` is either `from' or `to`:
%%%
%%% 1. The `patch-X' key of the execution message.
%%% 2. The `X' key of the execution message.
%%% 3. The `patch-X' key of the request message.
%%% 4. The `X' key of the request message.
%%%
%%% Additionally, this device implements the standard computation device keys,
%%% allowing it to be used as an element of an execution stack pipeline, etc.
-module(dev_patch).
-export([all/3, patches/3]).
%%% `execution-device` standard hooks:
-export([init/3, compute/3, normalize/3, snapshot/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% @doc Necessary hooks for compliance with the `execution-device' standard.
init(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
normalize(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
snapshot(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
compute(Msg1, Msg2, Opts) -> patches(Msg1, Msg2, Opts).

%% @doc Get the value found at the `patch-from' key of the message, or the
%% `from' key if the former is not present. Remove it from the message and set
%% the new source to the value found.
all(Msg1, Msg2, Opts) ->
    move(all, Msg1, Msg2, Opts).

%% @doc Find relevant `PATCH' messages in the given source key of the execution
%% and request messages, and apply them to the given destination key of the
%% request.
patches(Msg1, Msg2, Opts) ->
    move(patches, Msg1, Msg2, Opts).

%% @doc Unified executor for the `all' and `patches' modes.
move(Mode, Msg1, Msg2, Opts) ->
    maybe
        % Find the input paths.
        % For `from' we parse the path to see if it is relative to the request
        % or the base message. This is not needed for `to' because it is
        % always relative to the request.
        RawPatchFrom =
            hb_ao:get_first(
                [
                    {Msg2, <<"patch-from">>},
                    {Msg1, <<"patch-from">>},
                    {Msg2, <<"from">>},
                    {Msg1, <<"from">>}
                ],
                <<"/">>,
                Opts
            ),
        {FromMsg, PatchFromParts} =
            case hb_path:term_to_path_parts(RawPatchFrom) of
                [BinKey|RestKeys] ->
                    case binary:split(BinKey, <<":">>) of
                        [<<"base">>, RestKey] ->
                            {Msg1, [RestKey|RestKeys]};
                        [<<"req">>, RestKey] ->
                            {Msg2, [RestKey|RestKeys]};
                        _ ->
                            {Msg1, RawPatchFrom}
                    end;
                _ ->
                    {Msg1, RawPatchFrom}
            end,
        ?event({patch_from_parts, {explicit, PatchFromParts}}),
        PatchFrom =
            case hb_path:to_binary(PatchFromParts) of
                <<"">> -> <<"/">>;
                Path -> Path
            end,
        ?event({patch_from, PatchFrom}),
        PatchTo =
            hb_ao:get_first(
                [
                    {Msg2, <<"patch-to">>},
                    {Msg1, <<"patch-to">>},
                    {Msg2, <<"to">>},
                    {Msg1, <<"to">>}
                ],
                <<"/">>,
                Opts
            ),
        ?event({patch_from, PatchFrom}),
        ?event({patch_to, PatchTo}),
        % Get the source of the patches from the message. Makes the `maybe'
        % statement return `{error, not_found}' if the source is not found.
        {ok, Source} ?= hb_ao:resolve(FromMsg, PatchFrom, Opts),
        % Find all messages with the PATCH request.
        {ToWrite, NewSourceValue} =
            case Mode of
                patches ->
                    maps:fold(
                        fun(Key, Msg, {PatchAcc, NewSourceAcc}) ->
                            Method = hb_ao:get(<<"method">>, Msg, Opts)
                                == <<"PATCH">>,
                            Device = hb_ao:get(<<"device">>, Msg, Opts)
                                == <<"patch@1.0">>,
                            if Method orelse Device ->
                                {PatchAcc#{Key => Msg}, NewSourceAcc};
                            true ->
                                {PatchAcc, NewSourceAcc#{ Key => Msg }}
                            end
                        end,
                        {#{}, #{}},
                        Source
                    );
                all ->
                    {Source, unset}
            end,
        ?event({source_data, ToWrite}),
        ?event({new_data_for_source_path, NewSourceValue}),
        % Remove the source from the message and set the new source.
        FromMsgWithoutSource =
            hb_ao:set(
                FromMsg,
                PatchFrom,
                <<"patch-error">>,
                Opts
            ),
        FromMsgWithNewSource =
            hb_ao:set(
                FromMsgWithoutSource,
                #{ PatchFrom => NewSourceValue },
                Opts
            ),
        % If the `mode` is `patches`, we need to remove the `method` key from
        % them, if present.
        ToWriteMod =
            case Mode of
                all -> ToWrite;
                patches ->
                    maps:fold(
                        fun(_, Patch, MsgN) ->
                            ?event({patching, {patch, Patch}, {before, MsgN}}),
                            Res =
                                hb_ao:set(
                                    MsgN,
                                    maps:without([<<"method">>], Patch),
                                    Opts
                                ),
                            ?event({patched, {'after', Res}}),
                            Res
                        end,
                        #{},
                        ToWrite
                    )
            end,
        % Find the target to apply the patches to, and apply them.
        PatchedResult =
            hb_ao:set(
                FromMsgWithNewSource,
                PatchTo,
                ToWriteMod,
                Opts
            ),
        % Return the patched message and the source, less the patches.
        ?event({patch_result, PatchedResult}),
        {ok, PatchedResult}
    end.

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

all_mode_test() ->
    InitState = #{
        <<"device">> => <<"patch@1.0">>,
        <<"input">> => #{
            <<"zones">> => #{
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
                        <<"orange">> => 300
                    }
                }
            }
        },
        <<"state">> => #{
            <<"prices">> => #{
                <<"apple">> => 1000
            }
        }
    },
    {ok, ResolvedState} =
        hb_ao:resolve(
            InitState,
            #{
                <<"path">> => <<"all">>,
                <<"patch-to">> => <<"/state">>,
                <<"patch-from">> => <<"/input/zones">>
            },
            #{}
        ),
    ?event({resolved_state, ResolvedState}),
    ?assertEqual(
        100,
        hb_ao:get(<<"state/1/prices/apple">>, ResolvedState, #{})
    ),
    ?assertEqual(
        300,
        hb_ao:get(<<"state/2/prices/orange">>, ResolvedState, #{})
    ),
    ?assertEqual(
        not_found,
        hb_ao:get(<<"input/zones">>, ResolvedState, #{})
    ).

req_prefix_test() ->
    BaseMsg = #{
        <<"device">> => <<"patch@1.0">>,
        <<"state">> => #{
            <<"prices">> => #{
                <<"apple">> => 1000
            }
        }
    },
    ReqMsg = #{
        <<"path">> => <<"all">>,
        <<"patch-from">> => <<"req:/results/outbox/1">>,
        <<"patch-to">> => <<"/state">>,
        <<"results">> => #{
            <<"outbox">> => #{
                <<"1">> => #{
                    <<"method">> => <<"PATCH">>,
                    <<"prices">> => #{
                        <<"apple">> => 100,
                        <<"banana">> => 200
                    }
                }
            }
        }
    },
    {ok, ResolvedState} = hb_ao:resolve(BaseMsg, ReqMsg, #{}),
    ?event({resolved_state, ResolvedState}),
    ?assertEqual(
        100,
        hb_ao:get(<<"state/prices/apple">>, ResolvedState, #{})
    ),
    ?assertEqual(
        200,
        hb_ao:get(<<"state/prices/banana">>, ResolvedState, #{})
    ),
    ?assertEqual(
        not_found,
        hb_ao:get(<<"results/outbox/1">>, ResolvedState, #{})
    ).