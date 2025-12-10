-module(dev_trie_prop).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

model_test() ->
    Res =
        hb_prop:state_machine(
            #{
                state => #{ <<"device">> => <<"trie@1.0">>, <<"a">> => 1 },
                model_state => #{ <<"device">> => <<"message@1.0">>, <<"a">> => 1 },
                request_gen => requests(),
                properties => properties(),
                next => fun next/4,
                runs => 10,
                seq_len => 10,
                opts => #{}
            }
        ),
    case Res of
        {failure, InitialState, ResSequence} ->
            ?event(
                properties,
                {failure,
                    {initial_state, InitialState},
                    {sequence, ResSequence}
                },
                #{ debug_print_truncate => infinity }
            ),
            error(failure);
        ok -> ok
    end.

requests() ->
    [
        fun(S, Opts) -> request(Action, S, Opts) end
    ||
        Action <- [set, get]
    ].
request(set, _S, _Opts) ->
    #{
        <<"path">> => <<"set">>,
        hb_prop:key() => hb_prop:any()
    };
request(get, S, Opts) ->
    #{
        <<"path">> => hb_prop:pick(hb_ao:keys(S, Opts) -- [<<"device">>])
    }.

properties() ->
    [
        fun verify_set/6,
        fun verify_size/4,
        fun verify_commitments/4
    ].

verify_set(_O1, _O2, Req = #{ <<"path">> := <<"set">> }, New1, New2, Opts) ->
    ?event({verify, retrievability}),
    [Key] = hb_maps:keys(Req, Opts) -- [<<"path">>],
    hb_ao:resolve(New1, Key, Opts) == hb_ao:resolve(New2, Key, Opts).

verify_size(Old, #{ <<"path">> := <<"set">> }, New, Opts) ->
    NumNewKeys = length(hb_ao:keys(New, Opts)),
    NumOldKeys = length(hb_ao:keys(Old, Opts)),
    ?event({verify, size, {new_count, NumNewKeys}, {old_count, NumOldKeys}}),
    (NumNewKeys == NumOldKeys) orelse (NumNewKeys == NumOldKeys + 1).

verify_commitments(_, #{ <<"path">> := <<"set">> }, New, Opts) ->
    ?event({verify, commitments}),
    hb_message:verify(New, all, Opts).

next(_OldS, #{ <<"path">> := <<"set">> }, NewS, _Opts) -> NewS;
next(OldS, _, _NewS, _Opts) -> OldS.