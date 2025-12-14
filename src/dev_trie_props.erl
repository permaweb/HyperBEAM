-module(dev_trie_props).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

model_test() ->
    ok = hb_invariant:state_machine(
        #{
            states => [#{ <<"device">> => <<"trie@1.0">>, <<"a">> => 1 }],
            models => [#{ <<"device">> => <<"message@1.0">>, <<"a">> => 1 }],
            requests => requests(),
            properties => properties(),
            next => fun next/4,
            runs => 10,
            length => 100,
            opts => #{}
        }
    ).

requests() ->
    [
        fun(S, Opts) -> request(Action, S, Opts) end
    ||
        Action <- [get, set, reset]
    ].
request(set, _S, _Opts) ->
    #{
        <<"path">> => <<"set">>,
        hb_invariant:key() => hb_invariant:any()
    };
request(get, S, Opts) ->
    ?event({generating_request, {get, S}}),
    #{
        <<"path">> => hb_invariant:pick(hb_ao:keys(S, Opts) -- [<<"device">>])
    };
request(reset, S, Opts) ->
    ResetKey = hb_invariant:pick(hb_ao:keys(S, Opts) -- [<<"device">>]),
    #{
        <<"path">> => <<"set">>,
        ResetKey => hb_invariant:any()
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