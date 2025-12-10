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
    ?event(
        {retrievability,
            {request, Req},
            {new_state, New1},
            {new_model_state, New2}
        }
    ),
    [Key] = hb_maps:keys(Req, Opts) -- [<<"path">>],
    hb_ao:resolve(New1, Key, Opts) == hb_ao:resolve(New2, Key, Opts).

verify_size(Old, #{ <<"path">> := <<"set">> }, New, Opts) ->
    ?event(
        {size,
            {old_state, Old},
            {new_state, New}
        }
    ),
    NumNewKeys = length(hb_ao:keys(New, Opts)),
    NumOldKeys = length(hb_ao:keys(Old, Opts)),
    (NumNewKeys == NumOldKeys) orelse (NumNewKeys == NumOldKeys + 1).

verify_commitments(_, #{ <<"path">> := <<"set">> }, New, Opts) ->
    ?event(
        {commitments,
            {new_state, New}
        }
    ),
    hb_message:verify(New, all, Opts).

next(_OldS, #{ <<"path">> := <<"set">> }, NewS, _Opts) -> NewS;
next(OldS, _, _NewS, _Opts) -> OldS.