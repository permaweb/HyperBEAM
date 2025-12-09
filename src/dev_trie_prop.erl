-module(dev_trie_prop).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

model_test() ->
    hb_prop:state_machine_with_model(
        #{ <<"device">> => <<"trie@1.0">> },
        #{ <<"device">> => <<"message@1.0">> },
        requests(),
        properties()
    ).

requests() ->
    [
        fun(S, Opts) -> request(Action, S, Opts) end
    ||
        Action <- [set, get]
    ].
request(set, S, Opts) ->
    #{
        <<"path">> => <<"set">>,
        hb_prop:key() => hb_prop:any()
    };
request(get, S, Opts) ->
    #{
        <<"path">> => hb_prop:pick(hb_ao:keys(S, Opts))
    }.

properties() ->
    [
        fun verify_set/6,
        fun verify_size/4,
        fun verify_commitments/4
    ].

verify_set(_O1, _O2, Req = #{ <<"path">> := <<"set">> }, New1, New2, Opts) ->
    {ok, Key} = hb_maps:find(<<"key">>, Req, Opts),
    hb_ao:resolve(New1, Key, Opts) == hb_ao:resolve(New2, Key, Opts).

verify_size(Old, #{ <<"path">> := <<"set">> }, New, Opts) ->
    NumNewKeys = length(hb_ao:keys(New, Opts)),
    NumOldKeys = length(hb_ao:keys(Old, Opts)),
    (NumNewKeys == NumOldKeys) orelse (NumNewKeys == NumOldKeys + 1).

verify_commitments(_, #{ <<"path">> := <<"set">> }, New, Opts) ->
    hb_message:verify(New, all, Opts).