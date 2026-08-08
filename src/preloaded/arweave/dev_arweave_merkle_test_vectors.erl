%%% @doc Test vectors for the `~arweave-merkle@1.0' device.
-module(dev_arweave_merkle_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Tests.

%% @doc A tree of known chunks validates every one of its own paths, and
%% reports the byte range each covers.
validates_every_path_of_a_tree_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Leaves = [ {crypto:strong_rand_bytes(32), N * 262144} || N <- lists:seq(1, 8) ],
    {RootID, Tree} = ar_merkle:generate_tree(Leaves),
    lists:foreach(
        fun({ID, Offset}) ->
            Path = ar_merkle:generate_path(RootID, Offset - 1, Tree),
            {ok, Res} =
                hb_ao:resolve(
                    #{
                        <<"device">> => <<"arweave-merkle@2.9">>,
                        <<"root">> => hb_util:encode(RootID),
                        <<"proof">> => hb_util:encode(Path),
                        <<"offset">> => Offset - 1,
                        <<"size">> => 8 * 262144
                    },
                    <<"validate">>,
                    Opts
                ),
            % Assert per key rather than on the whole message: the resolver
            % attaches its own `priv'/hashpath bookkeeping to every result.
            ?assertEqual(hb_util:encode(ID), hb_maps:get(<<"leaf">>, Res, not_found, Opts)),
            ?assertEqual(Offset - 262144, hb_maps:get(<<"start-offset">>, Res, not_found, Opts)),
            ?assertEqual(Offset, hb_maps:get(<<"end-offset">>, Res, not_found, Opts))
        end,
        Leaves
    ).

%% @doc A path that does not belong to the root is rejected, and rejected with
%% the specific error the caller can branch on -- not merely `false'.
reject_foreign_path_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Leaves = [ {crypto:strong_rand_bytes(32), N * 262144} || N <- lists:seq(1, 4) ],
    {RootID, Tree} = ar_merkle:generate_tree(Leaves),
    Path = ar_merkle:generate_path(RootID, 262143, Tree),
    OtherRoot = crypto:strong_rand_bytes(32),
    {error, Error} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-merkle@2.9">>,
                <<"root">> => hb_util:encode(OtherRoot),
                <<"proof">> => hb_util:encode(Path),
                <<"offset">> => 262143,
                <<"size">> => 4 * 262144
            },
            <<"validate">>,
            Opts
        ),
    ?assertEqual(<<"invalid-merkle-path">>, hb_maps:get(<<"message">>, Error, not_found, Opts)).

%% @doc The note recorded in a path is the leaf's end offset. This is the
%% behaviour `dev_arweave_offset' already depends upon.
note_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Leaves = [ {crypto:strong_rand_bytes(32), N * 262144} || N <- lists:seq(1, 4) ],
    {RootID, Tree} = ar_merkle:generate_tree(Leaves),
    Path = ar_merkle:generate_path(RootID, 524287, Tree),
    {ok, Res} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-merkle@2.9">>,
                <<"proof">> => hb_util:encode(Path)
            },
            <<"note">>,
            Opts
        ),
    ?assertEqual(524288, hb_maps:get(<<"note">>, Res, not_found, Opts)).

%% @doc A proof that is not valid base64URL is reported as such, rather than
%% escaping the device as an exception.
%%
%% `hb_util:safe_decode/1' rejects on length -- an input whose size is not a
%% whole number of base64 quanta -- but is tolerant of out-of-alphabet
%% characters, which it decodes to arbitrary bytes. So a corrupted proof
%% usually surfaces as `invalid-merkle-path' rather than `invalid-base64';
%% either way it is a clean rejection, which is what matters here.
reject_malformed_proof_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    {error, Error} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-merkle@2.9">>,
                <<"root">> => hb_util:encode(crypto:strong_rand_bytes(32)),
                <<"proof">> => <<"abcde">>,
                <<"offset">> => 0,
                <<"size">> => 262144
            },
            <<"validate">>,
            Opts
        ),
    ?assertEqual(
        <<"invalid-base64">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc The root a path records is the root of the tree it was generated from.
%% Both node shapes are exercised: a tree of one leaf yields a path whose first
%% node is the leaf, a tree of several yields one whose first node is a branch,
%% and `ar_merkle' reconstitutes the root differently from each.
extract_root_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    lists:foreach(
        fun(Count) ->
            Leaves =
                [
                    {crypto:strong_rand_bytes(32), N * 262144}
                ||
                    N <- lists:seq(1, Count)
                ],
            {RootID, Tree} = ar_merkle:generate_tree(Leaves),
            Path = ar_merkle:generate_path(RootID, 262143, Tree),
            {ok, Res} =
                hb_ao:resolve(
                    #{
                        <<"device">> => <<"arweave-merkle@2.9">>,
                        <<"proof">> => hb_util:encode(Path)
                    },
                    <<"extract-root">>,
                    Opts
                ),
            ?assertEqual(
                hb_util:encode(RootID),
                hb_maps:get(<<"root">>, Res, not_found, Opts)
            )
        end,
        [1, 4]
    ).

%% @doc A proof too short to hold a node carries no root. `ar_merkle' answers
%% `{error, invalid_proof}' for it, which must reach the caller as a rejection
%% rather than as a `badarg' out of the encoder.
reject_rootless_proof_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    {error, Error} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-merkle@2.9">>,
                <<"proof">> => hb_util:encode(crypto:strong_rand_bytes(16))
            },
            <<"extract-root">>,
            Opts
        ),
    ?assertEqual(
        <<"invalid-merkle-proof">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc An unknown ruleset is refused rather than coerced into an atom.
reject_unknown_ruleset_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Leaves = [ {crypto:strong_rand_bytes(32), N * 262144} || N <- lists:seq(1, 4) ],
    {RootID, Tree} = ar_merkle:generate_tree(Leaves),
    Path = ar_merkle:generate_path(RootID, 262143, Tree),
    ?assertMatch(
        {error, #{ <<"message">> := <<"unknown-ruleset">> }},
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-merkle@2.9">>,
                <<"root">> => hb_util:encode(RootID),
                <<"proof">> => hb_util:encode(Path),
                <<"offset">> => 262143,
                <<"size">> => 4 * 262144,
                <<"ruleset">> => <<"made-up">>
            },
            <<"validate">>,
            Opts
        )
    ).
