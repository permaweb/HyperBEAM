%%% @doc An invariant-based test suite for the `~trie@1.0' device. This suite
%%% utilizes comparison of `get' and `set' requests against the default AO-Core
%%% `~message@1.0' device as a reference implementation of the message interface.
-module(dev_trie_props).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Test the `~trie@1.0' device against the default AO-Core `~message@1.0'
%% device as a reference implementation of the message interface.
model_test() ->
    ok = hb_invariant:state_machine(
        #{
            <<"states">> => [#{ <<"device">> => <<"trie@1.0">>, <<"a">> => 1 }],
            <<"models">> => [#{ <<"device">> => <<"message@1.0">>, <<"a">> => 1 }],
            <<"requests">> => requests(),
            <<"properties">> => properties(),
            <<"next">> => fun next/4,
            <<"runs">> => 10,
            <<"length">> => 100,
            <<"opts">> => #{}
        }
    ).

%% @doc Generate a list of request messages for the `~trie@1.0' and `~message@1.0'
%% devices. Calls to `set' are split into two kinds: one that adds new keys to
%% the `Base', and another that resets an existing key to a new value.
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

%% @doc Generate a list of properties to enforce after each `set' or `get'
%% request.
properties() ->
    [
        fun verify_get/6,
        fun verify_set/6,
        fun verify_size/4,
        fun verify_commitments/4
    ].

%% @doc Verify that the `Result' of `get' request is always the same between the
%% primary and model executions.
verify_get(_O1, _O2, Req = #{ <<"path">> := <<"get">> }, New1, New2, _Opts) ->
    (New1 == New2) orelse
        {inconsistent_get_result, {req, Req}, {res1, New1}, {res2, New2}}.

%% @doc Verify that both of the resulting states return the same value for a key
%% that was set in a request. Only executes on requests with `path: set' (see
%% the property semantics of `hb_invariant' for more details).
verify_set(_O1, _O2, Req = #{ <<"path">> := <<"set">> }, New1, New2, Opts) ->
    ?event({verify, retrievability}),
    [Key] = hb_maps:keys(Req, Opts) -- [<<"path">>],
    (hb_ao:resolve(New1, Key, Opts) == hb_ao:resolve(New2, Key, Opts)) orelse
        {set_value_not_retrievable_consistently,
            {req, Req},
            {res1, New1},
            {res2, New2},
            {key, Key}
        }.

%% @doc Verify that a `set' request did not result in more than one new key being
%% added to the `Base'. Similarly, verify that a `set' never results in a decrease
%% in the number of keys in the `Base'.
verify_size(Old, #{ <<"path">> := <<"set">> }, New, Opts) ->
    NumNewKeys = length(hb_ao:keys(New, Opts)),
    NumOldKeys = length(hb_ao:keys(Old, Opts)),
    ?event({verify, size, {new_count, NumNewKeys}, {old_count, NumOldKeys}}),
    ((NumNewKeys == NumOldKeys) orelse (NumNewKeys == NumOldKeys + 1)) orelse
        {invalid_set_size, {old, NumOldKeys}, {new, NumNewKeys}}.

%% @doc Verify that the `Result' after a `set' is always a well-committed, valid
%% message.
verify_commitments(_, #{ <<"path">> := <<"set">> }, New, Opts) ->
    ?event({verify, commitments}),
    hb_message:verify(New, all, Opts) orelse
        {invalid_commitment_after_set, {res, New}}.

%% @doc If the request was for a `set' operation, return the new state as it
%% was given. Otherwise, in the case of a `get' discard the resulting value and
%% utilize the original state again for the next request.
next(_OldS, #{ <<"path">> := <<"set">> }, NewS, _Opts) -> NewS;
next(OldS, _, _NewS, _Opts) -> OldS.
