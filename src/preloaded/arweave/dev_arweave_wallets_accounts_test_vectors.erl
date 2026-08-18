%%% @doc Test vectors for Arweave account-tree semantics.
-module(dev_arweave_wallets_accounts_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The pair and quadruple account forms have distinct native preimages.
account_form_test() ->
    ?assertEqual({12345, <<3:256>>}, account_from_fields(12345, 1, true)),
    ?assertEqual(
        {12345, <<3:256>>, 2, true},
        account_from_fields(12345, 2, true)
    ),
    ?assertEqual(
        {12345, <<3:256>>, 1, false},
        account_from_fields(12345, 1, false)
    ),
    ?assertNotEqual(
        account_root({12345, <<3:256>>}),
        account_root({12345, <<3:256>>, 1, true})
    ).

%% @doc AO-Core account messages round-trip without changing native arity.
account_round_trip_test() ->
    lists:foreach(
        fun(Account) ->
            ?assertEqual(
                Account,
                lib_arweave_accounts:account(
                    lib_arweave_accounts:account_message(Account),
                    #{}
                )
            )
        end,
        [
            {12345, <<3:256>>},
            {0, <<>>},
            {12345, <<3:256>>, 2, true},
            {10218569660, <<7:256>>, 1, false}
        ]
    ),
    ?assertEqual(
        {12345, <<3:256>>},
        lib_arweave_accounts:account(
            lib_arweave_accounts:account_message(
                {12345, <<3:256>>, 1, true}
            ),
            #{}
        )
    ).

%% @doc Native root identity is independent of insertion order.
insertion_order_test() ->
    Accounts = accounts(200),
    Root = root(Accounts),
    ?assertEqual(Root, root(lists:reverse(Accounts))),
    ?assertEqual(Root, root(shuffle(Accounts))),
    [{Address, {Balance, LastTX}} | Rest] = Accounts,
    ?assertNotEqual(Root, root([{Address, {Balance + 1, LastTX}} | Rest])).

%% @doc Hashing memoises nodes and reports only invalidated Patricia nodes.
root_update_test() ->
    Tree = lib_arweave_accounts:insert_all(accounts(40), new()),
    {Root, Memoised, Update} = lib_arweave_accounts:root_update(Tree),
    ?assertNotEqual(<<>>, Root),
    ?assert(map_size(Update) > 0),
    {Root, Memoised2, #{}} = lib_arweave_accounts:root_update(Memoised),
    [{Address, {Balance, LastTX}} | _] = accounts(40),
    Changed =
        lib_arweave_accounts:insert(
            Address,
            {Balance + 1, LastTX},
            Memoised2
        ),
    {ChangedRoot, _ChangedTree, ChangedUpdate} =
        lib_arweave_accounts:root_update(Changed),
    ?assertNotEqual(Root, ChangedRoot),
    ?assert(map_size(ChangedUpdate) < map_size(Update)).

%% @doc The public boundary accepts every canonical mainnet account key.
canonical_address_test() ->
    lists:foreach(
        fun(Address) ->
            ?assertEqual(
                Address,
                lib_arweave_accounts:address(hb_util:encode(Address))
            )
        end,
        [<<>>, <<1>>, <<1:200>>, <<1:256>>]
    ).

%%% Test helpers.

new() -> lib_arweave_accounts:new().

root(Accounts) ->
    {Root, _Tree} =
        lib_arweave_accounts:root(
            lib_arweave_accounts:insert_all(Accounts, new())
        ),
    Root.

accounts(Count) ->
    [
        {
            crypto:hash(sha256, <<"account-", N:16>>),
            {N * 1000, crypto:hash(sha256, <<"last-tx-", N:16>>)}
        }
    || N <- lists:seq(1, Count)
    ].

account_from_fields(Balance, Denomination, MiningPermission) ->
    lib_arweave_accounts:account(
        #{
            <<"balance">> => Balance,
            <<"last-tx">> => hb_util:encode(<<3:256>>),
            <<"denomination">> => Denomination,
            <<"mining-permission">> => MiningPermission
        },
        #{}
    ).

account_root(Account) ->
    root([{<<1:256>>, Account}]).

shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- List])].
