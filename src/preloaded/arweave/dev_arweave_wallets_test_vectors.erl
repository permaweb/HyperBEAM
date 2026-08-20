%%% @doc Deterministic test vectors for Arweave wallet-list transitions.
-module(dev_arweave_wallets_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(DEVICE, <<"arweave-wallets@2.9">>).

root_cannot_shape_a_store_path_test() ->
    ?assertThrow(
        {unsafe_tree_path, _},
        dev_arweave_wallets:tree_path(<<"../../../secret">>)
    ),
    ?assertThrow(
        {unsafe_tree_path, _},
        dev_arweave_wallets:tree_path(<<"a/b">>)
    ),
    Good = hb_util:encode(crypto:strong_rand_bytes(48)),
    ?assertMatch(
        <<"~arweave-wallets@2.9/trees/", _/binary>>,
        dev_arweave_wallets:tree_path(Good)
    ).

%% @doc Page order does not affect the committed Patricia root.
page_order_test() ->
    Opts = opts(),
    Accounts = accounts(),
    {Left, Right} = lists:split(length(Accounts) div 2, Accounts),
    Forwards = ingest_pages([Left, Right], Opts),
    Backwards = ingest_pages([Right, Left], Opts),
    Root = resolved_root(Forwards, Opts),
    ?assertEqual(Root, resolved_root(Backwards, Opts)),
    ?assertNotEqual(
        Root,
        resolved_root(ingest_pages([Left, tl(Right)], Opts), Opts)
    ).

%% @doc Empty and short addresses are valid account keys and survive a cold
%% store round trip.
short_address_page_test() ->
    Opts = opts(),
    Accounts =
        [
            {<<>>, {876060014779297, <<>>}},
            {<<177>>, {30000, <<>>}},
            {<<105, 189>>, {20, <<>>}}
            | accounts()
        ],
    State = ingest_pages([Accounts], Opts),
    Root = resolved_root(State, Opts),
    {ok, Cold} = hb_cache:read(dev_arweave_wallets:tree_path(Root), Opts),
    ?assertEqual(Root, resolved_root(Cold, Opts)),
    {ok, Account} =
        hb_ao:resolve(
            Cold,
            #{ <<"path">> => <<"get">>, <<"address">> => hb_util:encode(<<>>) },
            Opts
        ),
    ?assertEqual(
        876060014779297,
        hb_maps:get(<<"balance">>, Account, not_found, Opts)
    ).

%% @doc Sparse updates change the root, enforce the expected root, and retain
%% enough ancestry for a reorg rollback.
apply_and_rollback_test() ->
    Opts = opts(),
    State = ingest_pages([accounts()], Opts),
    [{Address, {Balance, _}} | _] = accounts(),
    InitialRoot = resolved_root(State, Opts),
    {ok, One} = apply_diff(State, credit(Address, Balance + 1), [], Opts),
    OneRoot = resolved_root(One, Opts),
    ?assertNotEqual(InitialRoot, OneRoot),
    ?assertMatch(
        {ok, _},
        apply_diff(State, credit(Address, Balance + 1), OneRoot, Opts)
    ),
    {error, WrongRoot} =
        apply_diff(State, credit(Address, Balance + 2), OneRoot, Opts),
    ?assertEqual(
        <<"invalid-wallet-list-root">>,
        hb_maps:get(<<"message">>, WrongRoot, not_found, Opts)
    ),
    {ok, Two} = apply_diff(One, credit(Address, Balance + 2), [], Opts),
    {ok, Back} =
        hb_ao:resolve(
            Two,
            #{ <<"path">> => <<"rollback">>, <<"depth">> => 2 },
            Opts
        ),
    ?assertEqual(InitialRoot, resolved_root(Back, Opts)).

%% @doc An account state records the version it was derived from twice: as the
%% root a block header and a peer both name a tree by, and as an AO-Core link a
%% caller can traverse without knowing this device's store layout.
account_state_names_its_previous_version_test() ->
    Opts = opts(),
    State = ingest_pages([accounts()], Opts),
    [{Address, {Balance, _}} | _] = accounts(),
    InitialRoot = resolved_root(State, Opts),
    {ok, One} = apply_diff(State, credit(Address, Balance + 1), [], Opts),
    ?assertEqual(
        InitialRoot,
        hb_maps:get(<<"previous-root">>, One, not_found, Opts)
    ),
    ?assertEqual(
        InitialRoot,
        hb_maps:get(
            <<"root">>,
            hb_maps:get(<<"previous">>, One, not_found, Opts),
            not_found,
            Opts
        )
    ),
    % The first state of a tree was derived from nothing, and says so by
    % carrying no root rather than an empty one.
    ?assertEqual(
        not_found,
        hb_maps:get(<<"previous-root">>, State, not_found, Opts)
    ),
    ?assertEqual(
        not_found,
        hb_maps:get(<<"previous">>, State, not_found, Opts)
    ).

reject_malformed_page_test() ->
    Opts = opts(),
    ?assertEqual(
        <<"invalid-wallet-list-page">>,
        page_error(<<"not a term">>, Opts)
    ),
    ?assertEqual(
        <<"invalid-account">>,
        page_error(
            term_to_binary(
                #{ next_cursor => last, wallets => [{<<1:256>>, {-1, <<>>}}] }
            ),
            Opts
        )
    ).

accounts() ->
    [
        {
            crypto:hash(sha256, <<"account-", N:16>>),
            {N * 1000, crypto:hash(sha256, <<"last-tx-", N:16>>)}
        }
     || N <- lists:seq(1, 64)
    ].

ingest_pages(Pages, Opts) ->
    lists:foldl(
        fun({Accounts, Cursor}, State) ->
            {ok, Result} =
                hb_ao:resolve(
                    State,
                    #{
                        <<"path">> => <<"page">>,
                        <<"body">> =>
                            term_to_binary(
                                #{ next_cursor => Cursor, wallets => Accounts }
                            )
                    },
                    Opts
                ),
            hb_maps:get(<<"accounts">>, Result, not_found, Opts)
        end,
        #{ <<"device">> => ?DEVICE },
        cursored(Pages)
    ).

cursored([Last]) -> [{Last, last}];
cursored([Page | Pages]) ->
    [{Page, crypto:strong_rand_bytes(32)} | cursored(Pages)].

resolved_root(State, Opts) ->
    {ok, Result} = hb_ao:resolve(State, <<"root">>, Opts),
    hb_maps:get(<<"root">>, Result, not_found, Opts).

credit(Address, Balance) ->
    #{
        hb_util:encode(Address) =>
            #{
                <<"balance">> => Balance,
                <<"last-tx">> => hb_util:encode(<<>>)
            }
    }.

apply_diff(State, Diff, Expected, Opts) ->
    hb_ao:resolve(
        State,
        #{
            <<"path">> => <<"apply">>,
            <<"diff">> => Diff,
            <<"expected-root">> => Expected
        },
        Opts
    ).

page_error(Body, Opts) ->
    {error, Error} =
        hb_ao:resolve(
            #{ <<"device">> => ?DEVICE },
            #{ <<"path">> => <<"page">>, <<"body">> => Body },
            Opts
        ),
    hb_maps:get(<<"message">>, Error, not_found, Opts).

opts() -> #{ <<"store">> => [hb_test_utils:test_store()] }.
