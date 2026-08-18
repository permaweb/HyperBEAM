%%% @doc Cross-component persistence vectors for Arweave's AO-native consensus
%%% structures. These tests exercise devices only through `hb_ao' and use
%%% isolated LMDB stores.
-module(dev_arweave_native_storage_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(WALLETS, <<"arweave-wallets@2.9">>).
-define(BLOCK_INDEX, <<"arweave-block-index@2.9">>).

%% @doc Both linked structures remain usable in a fresh OS VM with no inherited
%% BEAM heap, private execution memo or store handle.
cold_os_vm_restart_test() ->
    Opts = test_opts(<<"native-cold-vm">>),
    Accounts = wallet_accounts(320),
    Entries = index_entries(260),
    Wallets = wallet_state(Accounts, Opts),
    Index = index_state(Entries, Opts),
    WalletID = state_id(Wallets, Opts),
    IndexID = state_id(Index, Opts),
    {ok, _} = hb_cache:read(WalletID, Opts),
    {ok, _} = hb_cache:read(IndexID, Opts),
    sync_store(store(Opts)),
    {ok, Peer, _Node} =
        peer:start_link(
            #{ connection => standard_io, args => peer_args() }
        ),
    try
        Result = cold_peer_result(
            Peer,
            WalletID,
            IndexID,
            hb_util:encode(element(1, hd(Accounts))),
            length(Entries) - 1,
            Opts
        ),
        ?assertEqual(expected_cold_result(Accounts, Entries), Result)
    after
        peer:stop(Peer)
    end.

%% @doc Resolve both persisted states using only modules loaded in a fresh VM.
cold_peer_result(Peer, WalletID, IndexID, Address, Height, Opts) ->
    lists:foreach(
        fun(App) ->
            {ok, _} = peer_call(
                Peer,
                application,
                ensure_all_started,
                [App]
            )
        end,
        [crypto, public_key, inets, ssl]
    ),
    ok = peer_call(Peer, hb, init, []),
    {ok, Wallets} = peer_call(Peer, hb_cache, read, [WalletID, Opts]),
    {ok, Index} = peer_call(Peer, hb_cache, read, [IndexID, Opts]),
    {
        clean_result(
            peer_call(Peer, hb_ao, resolve, [Wallets, <<"root">>, Opts]),
            Opts
        ),
        clean_result(
            peer_call(
                Peer,
                hb_ao,
                resolve,
                [
                    Wallets,
                    #{ <<"path">> => <<"get">>, <<"address">> => Address },
                    Opts
                ]
            ),
            Opts
        ),
        clean_result(
            peer_call(Peer, hb_ao, resolve, [Index, <<"root">>, Opts]),
            Opts
        ),
        clean_result(
            peer_call(
                Peer,
                hb_ao,
                resolve,
                [
                    Index,
                    #{ <<"path">> => <<"at">>, <<"height">> => Height },
                    Opts
                ]
            ),
            Opts
        )
    }.

peer_call(Peer, Module, Function, Arguments) ->
    peer:call(Peer, Module, Function, Arguments, 60_000).

%% @doc Publishing a containing block links already-durable components without
%% recursively indexing them, while transaction and Arweave aliases still work.
no_match_publication_test() ->
    PublishOpts = match_opts(<<"native-publication-match">>),
    Wallets = wallet_state(wallet_accounts(12), PublishOpts),
    Index = index_state(index_entries(130), PublishOpts),
    ?assertEqual(#{ keys => 0, bytes => 0 }, match_stats(store(PublishOpts))),
    {Record, TX, Block0, Hash} = publication_fixture(PublishOpts),
    Block = Block0#{
        <<"accounts">> => Wallets,
        <<"block-index">> => Index
    },
    {ok, _BlockID} = lib_arweave_sync:publish(
        Block,
        Hash,
        [TX],
        PublishOpts
    ),
    ArweaveID = hb_util:encode(Record#tx.id),
    {ok, Placement} = hb_cache:read(
        lib_arweave_paths:placement(ArweaveID),
        PublishOpts
    ),
    ?assertEqual(
        ArweaveID,
        hb_maps:get(<<"id">>, Placement, not_found, PublishOpts)
    ),
    ?assertMatch({ok, _}, hb_cache:read(ArweaveID, PublishOpts)),
    {ok, PublishedBlock} = hb_cache:read(Hash, PublishOpts),
    UncommittedBlock = hb_message:uncommitted(PublishedBlock),
    AccountsLink = maps:get(<<"accounts">>, UncommittedBlock),
    IndexLink = maps:get(<<"block-index">>, UncommittedBlock),
    ?assert(?IS_LINK(AccountsLink)),
    ?assert(?IS_LINK(IndexLink)),
    ?assertEqual(
        state_id(Wallets, PublishOpts),
        link_target(AccountsLink, PublishOpts)
    ),
    ?assertEqual(
        state_id(Index, PublishOpts),
        link_target(IndexLink, PublishOpts)
    ),
    PublishedMatch = match_stats(store(PublishOpts)),
    ?assert(maps:get(keys, PublishedMatch) > 0),
    ControlOpts = match_opts(<<"native-transaction-match-control">>),
    {ok, _ControlID} = hb_cache:write(TX, ControlOpts),
    ?assertEqual(match_stats(store(ControlOpts)), PublishedMatch),
    {ok, TXMatches} = hb_cache:match(
        #{
            <<"anchor">> =>
                hb_maps:get(<<"anchor">>, TX, not_found, PublishOpts)
        },
        PublishOpts
    ),
    ?assert(lists:member(ArweaveID, TXMatches)),
    lists:foreach(
        fun(Device) ->
            ?assertEqual(
                {error, not_found},
                hb_cache:match(#{ <<"device">> => Device }, PublishOpts)
            )
        end,
        [?WALLETS, ?BLOCK_INDEX, <<"arweave-block@2.9">>]
    ).

%% @doc Build a canonical wallet state through its public AO interface.
wallet_state(Accounts, Opts) ->
    Accumulator =
        hb_util:ok(
            hb_ao:resolve(
                #{ <<"device">> => ?WALLETS },
                #{
                    <<"path">> => <<"insert">>,
                    <<"accounts">> => wallet_message(Accounts)
                },
                Opts
            )
        ),
    hb_util:ok(
        hb_ao:resolve(
            Accumulator,
            #{ <<"path">> => <<"finalize">> },
            Opts
        )
    ).

%% @doc Build a canonical block index through its public AO interface.
index_state(Entries, Opts) ->
    hb_util:ok(
        hb_ao:resolve(
            #{ <<"device">> => ?BLOCK_INDEX },
            #{
                <<"path">> => <<"append">>,
                <<"start-height">> => 0,
                <<"entries">> =>
                    hb_util:list_to_numbered_message(
                        [index_entry_message(Entry) || Entry <- Entries]
                    )
            },
            Opts
        )
    ).

wallet_accounts(Count) ->
    [wallet_account(N) || N <- lists:seq(1, Count)].

wallet_account(N) when N rem 3 == 0 ->
    {
        crypto:hash(sha256, <<"account-", N:64>>),
        {
            N * 1000,
            crypto:hash(sha256, <<"last-tx-", N:64>>),
            2,
            false
        }
    };
wallet_account(N) ->
    {
        crypto:hash(sha256, <<"account-", N:64>>),
        {N * 1000, crypto:hash(sha256, <<"last-tx-", N:64>>)}
    }.

index_entries(Count) ->
    [index_entry(N) || N <- lists:seq(1, Count)].

index_entry(N) ->
    Suffix = integer_to_binary(N),
    {
        crypto:hash(sha384, <<"block-", Suffix/binary>>),
        N * 262144,
        crypto:hash(sha256, <<"tx-root-", Suffix/binary>>)
    }.

wallet_message(Accounts) ->
    maps:from_list(
        [
            {
                hb_util:encode(Address),
                lib_arweave_accounts:account_message(Account)
            }
        || {Address, Account} <- Accounts
        ]
    ).

index_entry_message({Hash, WeaveSize, TXRoot}) ->
    #{
        <<"indep-hash">> => hb_util:encode(Hash),
        <<"weave-size">> => WeaveSize,
        <<"tx-root">> => hb_util:encode(TXRoot)
    }.

wallet_root(Accounts) ->
    Tree = lib_arweave_accounts:insert_all(
        Accounts,
        lib_arweave_accounts:new()
    ),
    {Root, _Memoised} = lib_arweave_accounts:root(Tree),
    Root.

index_root(Entries) ->
    hb_util:encode(
        ar_unbalanced_merkle:block_index_to_merkle_root(
            lists:reverse(Entries)
        )
    ).

%% @doc Construct one real signed transaction, placement and containing block.
publication_fixture(Opts) ->
    Record =
        ar_tx:sign(
            #tx{
                format = 2,
                anchor = crypto:hash(sha256, <<"publication-anchor">>),
                reward = 1_000_000_000_000,
                data_size = 262144,
                data_root = crypto:hash(sha256, <<"publication-data">>)
            },
            ar_wallet:new()
        ),
    TX = hb_message:convert(
        Record,
        <<"structured@1.0">>,
        <<"tx@1.0">>,
        Opts
    ),
    ID = hb_util:encode(Record#tx.id),
    Hash = hb_util:encode(crypto:hash(sha384, <<"publication-block">>)),
    Placement =
        #{
            <<"id">> => ID,
            <<"block">> => Hash,
            <<"height">> => 1,
            <<"position">> => 0,
            <<"data-root">> => hb_util:encode(Record#tx.data_root),
            <<"data-size">> => Record#tx.data_size,
            <<"start-offset">> => 1_000_000,
            <<"transaction">> => to_link(ID)
        },
    Block =
        #{
            <<"device">> => <<"arweave-block@2.9">>,
            <<"indep-hash">> => Hash,
            <<"height">> => 1,
            <<"previous-block">> =>
                hb_util:encode(crypto:hash(sha384, <<"publication-parent">>)),
            <<"transactions">> => [Placement]
        },
    {Record, TX, Block, Hash}.

%% @doc Count only physical reverse-match entries in the real LMDB.
match_stats(Store) ->
    sync_store(Store),
    #{ <<"db">> := DB } = hb_store:find(Store),
    {ok, Stats} = elmdb:fold(DB, fun count_match/3, #{ keys => 0, bytes => 0 }),
    Stats.

count_match(<<"~match@1.0", _/binary>> = Key, Value, Stats) ->
    Stats#{
        keys := maps:get(keys, Stats) + 1,
        bytes := maps:get(bytes, Stats) + byte_size(Key) + byte_size(Value)
    };
count_match(_Key, _Value, Stats) ->
    Stats.

sync_store(Store) ->
    #{ <<"env">> := Env, <<"db">> := DB } = hb_store:find(Store),
    ok = elmdb:flush(DB),
    ok = elmdb:env_sync(Env),
    0 = elmdb:overlay_count(DB),
    ok.

test_opts(Tag) ->
    Store0 = hb_test_utils:test_store(hb_store_lmdb, Tag),
    Name = list_to_binary(
        filename:absname(hb_util:list(maps:get(<<"name">>, Store0)))
    ),
    #{ <<"store">> => [Store0#{ <<"name">> => Name }] }.

match_opts(Tag) ->
    Opts = test_opts(Tag),
    Opts#{ <<"match-index">> => hb_opts:get(store, [], Opts) }.

store(Opts) ->
    hd(hb_opts:get(store, [], Opts)).

state_id(State, Opts) ->
    hb_message:id(
        hb_private:reset(State),
        none,
        Opts#{ <<"linkify-mode">> => discard }
    ).

to_link(ID) ->
    {link, ID, #{ <<"type">> => <<"link">>, <<"lazy">> => false }}.

link_target({link, ID, LinkOpts}, Opts) ->
    case maps:get(<<"lazy">>, LinkOpts, false) of
        true ->
            {ok, Target} = hb_cache:read(ID, Opts),
            Target;
        false ->
            ID
    end.

clean_result({ok, Result}, Opts) ->
    {ok, hb_maps:without([<<"priv">>], Result, Opts)};
clean_result(Result, _Opts) -> Result.

expected_cold_result(Accounts, Entries) ->
    {_Address, Account} = hd(Accounts),
    Entry = lists:last(Entries),
    {
        {ok, #{ <<"root">> => wallet_root(Accounts) }},
        {ok, lib_arweave_accounts:account_message(Account)},
        {ok, #{ <<"root">> => index_root(Entries) }},
        {ok, index_entry_message(Entry)}
    }.

peer_args() ->
    lists:append([["-pa", Path] || Path <- code:get_path()]).
