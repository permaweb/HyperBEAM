%%% @doc A battery of test vectors exercising the `bundle' / `hint-device'
%%% machinery of the `message@1.0' device across a three-level message tree.
%%%
%%% The tree is built bottom-up; each level is a committed (signed) message
%%% holding the level below it as a sub-message:
%%%
%%% <pre>
%%%     L1 (root) -- l2 --> L2 (middle) -- l3 --> L3 (leaf) -- inner --> #{}
%%% </pre>
%%%
%%% Each level is committed with its own `bundle' choice -- `true', `false'
%%% or `none' (committed with no `bundle' flag at all). The flag decides
%%% whether that level's sub-message is held inline (loaded) or as a link
%%% (offloaded) in the level's signed TABM form:
%%%
%%%   - L1's flag controls `l2', L2's flag controls `l3', and L3's flag
%%%     controls L3's plain sub-map `inner'.
%%%
%%% `none' is observably identical to `false': committing with no flag
%%% offloads children exactly as `false' does.
%%%
%%% For every 3x3x3 permutation of build flags the suite checks:
%%%
%%%   - verify/3 with no forced bundle: the reliable path -- every level
%%%     verifies in the state it was committed in.
%%%   - verify/3 with a forced bundle (`true'|`false'): the edge case -- a
%%%     `bundle' on the verify request is harmless. verify builds its
%%%     source spec like commit/3 (mirroring the request `bundle' but also
%%%     setting `hint-device'), so the per-node hints override the forced
%%%     value and the tree still verifies. Tested for completeness.
%%%   - id/3: the root's id equals its sole commitment's key.
%%%   - convert/4: the tree round-trips through the `ans104@1.0' codec --
%%%     the standard structured<->codec path -- and still verifies at every
%%%     level. A `bundle' on the request is per-node-overridden, so the
%%%     committed shape survives the round-trip.
-module(dev_message_bundle_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Fresh, isolated options for a single vector: a new wallet and a new
%% in-memory store, so vectors cannot interfere with one another.
fresh_opts() ->
    #{
        <<"priv-wallet">> => hb:wallet(),
        <<"store">> => hb_test_utils:test_store()
    }.

%% @doc Commit a message with the `ans104@1.0' codec. `Bundle' is `true',
%% `false', or `none' to commit with no `bundle' flag at all.
commit(Msg, none, Opts) ->
    hb_message:commit(Msg, Opts, #{ <<"device">> => <<"ans104@1.0">> });
commit(Msg, Bundle, Opts) ->
    hb_message:commit(
        Msg,
        Opts,
        #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => Bundle }
    ).

%% @doc Build a signed three-level tree with the given per-level flags.
build_tree(B1, B2, B3, Opts) ->
    L3 =
        commit(
            #{
                <<"l3-tag">> => <<"l3-value">>,
                <<"inner">> => #{ <<"deep">> => <<"deep-value">> }
            },
            B3,
            Opts
        ),
    L2 = commit(#{ <<"l2-tag">> => <<"l2-value">>, <<"l3">> => L3 }, B2, Opts),
    commit(#{ <<"l1-tag">> => <<"l1-value">>, <<"l2">> => L2 }, B1, Opts).

%%% Test vector generator.

%% @doc The {API, RequestBundle} operations run against every tree shape.
operations() ->
    [
        {verify, none},
        {verify, true},
        {verify, false},
        {id, none},
        {convert, none},
        {convert, true},
        {convert, false}
    ].

%% @doc Generate the full grid: 3x3x3 tree shapes x the operation list.
bundle_vectors_test_() ->
    {timeout, 240,
        [
            {
                test_label(B1, B2, B3, Api, ReqBundle),
                fun() -> run(B1, B2, B3, Api, ReqBundle) end
            }
        ||
            B1 <- [true, false, none],
            B2 <- [true, false, none],
            B3 <- [true, false, none],
            {Api, ReqBundle} <- operations()
        ]
    }.

test_label(B1, B2, B3, Api, ReqBundle) ->
    lists:flatten(
        io_lib:format(
            "L1=~p L2=~p L3=~p ~p req-bundle=~p",
            [B1, B2, B3, Api, ReqBundle]
        )
    ).

%% @doc Build the tree and exercise the chosen API.
run(B1, B2, B3, Api, ReqBundle) ->
    Opts = fresh_opts(),
    Tree = build_tree(B1, B2, B3, Opts),
    % Every freshly built tree must verify via the reliable per-node path,
    % whatever per-level bundle permutation it was signed with.
    ?assert(hb_message:verify(Tree, all, Opts)),
    exercise(Api, ReqBundle, B1, B2, B3, Tree, Opts).

%%% Per-API exercises.

%% `verify': verification always uses the per-node path -- each subtree is
%% checked in the bundle state it was committed in. A `bundle' on the
%% request is mirrored as commit/3 does, but `hint-device' is set too, so
%% the per-node hints override it. A validly-built tree therefore always
%% verifies at every level, with or without a forced request bundle.
exercise(verify, ReqBundle, _B1, _B2, _B3, Tree, Opts) ->
    Spec = verify_spec(ReqBundle),
    ?assert(hb_message:verify(Tree, Spec, Opts)),
    L2 = hb_maps:get(<<"l2">>, Tree, undefined, Opts),
    ?assert(hb_message:verify(L2, Spec, Opts)),
    L3 = hb_maps:get(<<"l3">>, L2, undefined, Opts),
    ?assert(hb_message:verify(L3, Spec, Opts));

%% `id': the root was committed exactly once, so `id/3' with `all'
%% committers accumulates to that single commitment -- the id must equal
%% the key under which it is stored in the root's commitments map.
exercise(id, _ReqBundle, _B1, _B2, _B3, Tree, Opts) ->
    Id = hb_message:id(Tree, all, Opts),
    Commitments = hb_maps:get(<<"commitments">>, Tree, #{}, Opts),
    ?assertEqual([Id], maps:keys(Commitments));

%% `convert': round-trip the tree through the `ans104@1.0' codec -- the
%% standard structured<->codec path. Each subtree converts in the state its
%% own commitment dictates (per-node), so a `bundle' flag on the request is
%% overridden and the committed shape is preserved. The round-tripped tree
%% must therefore still verify at every level.
exercise(convert, ReqBundle, _B1, _B2, _B3, Tree, Opts) ->
    Encoded = hb_message:convert(Tree, convert_target(ReqBundle), Opts),
    Restored =
        hb_message:convert(
            Encoded,
            <<"structured@1.0">>,
            <<"ans104@1.0">>,
            Opts
        ),
    ?assert(hb_message:verify(Restored, all, Opts)),
    L2 = hb_maps:get(<<"l2">>, Restored, undefined, Opts),
    ?assert(hb_message:verify(L2, all, Opts)),
    L3 = hb_maps:get(<<"l3">>, L2, undefined, Opts),
    ?assert(hb_message:verify(L3, all, Opts)).

%% @doc The verify spec for a request-bundle value: `all' committers, plus
%% the forced `bundle' flag when one is given.
verify_spec(none) ->
    all;
verify_spec(ReqBundle) ->
    #{ <<"committers">> => <<"all">>, <<"bundle">> => ReqBundle }.

%% @doc The convert target for a request-bundle value: the bare `ans104@1.0'
%% codec, plus a forced `bundle' flag when one is given.
convert_target(none) ->
    <<"ans104@1.0">>;
convert_target(ReqBundle) ->
    #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => ReqBundle }.
