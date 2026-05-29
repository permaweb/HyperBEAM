%%% @doc A battery of test vectors exercising the `bundle' / `hint-device'
%%% machinery of the `message@1.0' device across a three-level message tree.
%%%
%%% The tree is built bottom-up; each level holds the level below it as a
%%% sub-message:
%%%
%%% <pre>
%%%     L1 (root) --> L2 (middle) --> L3 (leaf) --> #{}
%%% </pre>
%%%
%%% Each level is built with one of four choices:
%%%
%%%   - `bundle_true'  -- committed (`ans104@1.0') with `bundle' => true
%%%   - `bundle_false' -- committed with `bundle' => false
%%%   - `no_bundle'    -- committed with no `bundle' flag
%%%   - `uncommitted'  -- not committed at all; a plain, unsigned map
%%% 
%%% For every 4x4x4 permutation of build choices the suite checks:
%%%
%%%   - verify/3: every level verifies in the state it was committed in.
%%%   - id/3: the root's id equals its sole commitment's key (or, for an
%%%     uncommitted root, the content-addressed unsigned id).
%%%   - convert/4 with target `bundle' none/true/false: the tree
%%%     round-trips through the `ans104@1.0' codec -- the standard
%%%     structured<->codec path -- and still verifies at every level. A
%%%     `bundle' on the conversion target applies only to the root; nested
%%%     subtrees follow their own commitments via `hint-device', so the
%%%     committed shape survives the round-trip.
-module(dev_message_bundle_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

fresh_opts() ->
    #{
        <<"priv-wallet">> => hb:wallet(),
        <<"store">> => hb_test_utils:test_store()
    }.

%% @doc Build one tree level from its build choice: commit the message with
%% the `ans104@1.0' codec (with `bundle' => true, `bundle' => false, or no
%% `bundle' flag), or -- for `uncommitted' -- leave it as a plain map.
build_level(Msg, uncommitted, _Opts) ->
    Msg;
build_level(Msg, no_bundle, Opts) ->
    hb_message:commit(Msg, Opts, #{ <<"device">> => <<"ans104@1.0">> });
build_level(Msg, bundle_true, Opts) ->
    hb_message:commit(
        Msg,
        Opts,
        #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true }
    );
build_level(Msg, bundle_false, Opts) ->
    hb_message:commit(
        Msg,
        Opts,
        #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => false }
    ).

%% @doc Build a three-level tree with the given per-level build choices.
build_tree(B1, B2, B3, Opts) ->
    L3 =
        build_level(
            #{
                <<"l3-tag">> => <<"l3-value">>,
                <<"inner">> => #{ <<"deep">> => <<"deep-value">> }
            },
            B3,
            Opts
        ),
    L2 =
        build_level(#{ <<"l2-tag">> => <<"l2-value">>, <<"l3">> => L3 }, B2, Opts),
    build_level(#{ <<"l1-tag">> => <<"l1-value">>, <<"l2">> => L2 }, B1, Opts).

%%% Test vector generator.

%% @doc The {API, RequestBundle} operations run against every tree shape.
operations() ->
    [
        {verify, none},
        {id, none},
        {convert, none},
        {convert, true},
        {convert, false}
    ].

%% @doc The per-level build choices a tree level can take.
build_choices() ->
    [bundle_true, bundle_false, no_bundle, uncommitted].

%% @doc Generate the full grid: 4x4x4 tree shapes x the operation list.
bundle_vectors_test_() ->
    {timeout, 600,
        [
            {
                test_label(B1, B2, B3, Api, ReqBundle),
                fun() -> run(B1, B2, B3, Api, ReqBundle) end
            }
        ||
            B1 <- build_choices(),
            B2 <- build_choices(),
            B3 <- build_choices(),
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

%% @doc Build the tree and run_test the chosen API.
run(B1, B2, B3, Api, ReqBundle) ->
    Opts = fresh_opts(),
    Tree = build_tree(B1, B2, B3, Opts),
    ?event(debug_test, {tree,
        {label, test_label(B1, B2, B3, Api, ReqBundle)},
        {built, Tree}}),
    % Every freshly built tree must verify via the reliable per-node path,
    % whatever per-level bundle permutation it was signed with.
    ?assert(hb_message:verify(Tree, all, Opts)),
    run_test(Api, ReqBundle, B1, B2, B3, Tree, Opts).

%%% Per-API run_tests.

%% `verify': every level of a validly-built tree verifies. The bundle
%% state each subtree was committed in is reproduced per-node via
%% `hint-device', so the verify request carries no `bundle'. (`run/3'
%% already verifies the root, so this only adds the nested levels.)
run_test(verify, _ReqBundle, _B1, _B2, _B3, Tree, Opts) ->
    L2 = hb_maps:get(<<"l2">>, Tree, undefined, Opts),
    ?assert(hb_message:verify(L2, all, Opts)),
    L3 = hb_maps:get(<<"l3">>, L2, undefined, Opts),
    ?assert(hb_message:verify(L3, all, Opts));

%% `id':
%%   - committed root: `id/3' with `all' committers accumulates to the
%%     single commitment -- the id must equal the key under which it is
%%     stored in the root's commitments map.
%%   - uncommitted root: there are no commitments, so `id/3' falls back to
%%     the (content-addressed) unsigned id -- `all' committers must give
%%     the same result as the bare unsigned-id call.
run_test(id, _ReqBundle, uncommitted, _B2, _B3, Tree, Opts) ->
    ?assertEqual(
        hb_message:id(Tree, none, Opts),
        hb_message:id(Tree, all, Opts)
    );
run_test(id, _ReqBundle, _B1, _B2, _B3, Tree, Opts) ->
    Id = hb_message:id(Tree, all, Opts),
    Commitments = hb_maps:get(<<"commitments">>, Tree, #{}, Opts),
    ?assertEqual([Id], maps:keys(Commitments));

%% `convert': round-trip the tree through the `ans104@1.0' codec. Each subtree 
%% converts in the state its own commitment dictates (per-node) via
%% `hint-device', so the `bundle' on the conversion target applies only to the
%% root and the committed shape is preserved.
run_test(convert, ReqBundle, _B1, _B2, _B3, Tree, Opts) ->
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

%% @doc The convert target for a request-bundle value: the bare `ans104@1.0'
%% codec, plus a forced `bundle' flag when one is given.
convert_target(none) ->
    <<"ans104@1.0">>;
convert_target(ReqBundle) ->
    #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => ReqBundle }.
