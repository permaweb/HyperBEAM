%%% @doc Deterministic transaction admission vectors for Arweave 2.9.
-module(dev_arweave_tx_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_HEIGHT, 1974871).
-define(TEST_REWARD, 1_000_000_000_000).
%% A fee of 31 digits, one more than fork 2.6 permits at denomination 1.
-define(OVERSIZED_REWARD, 1_000_000_000_000_000_000_000_000_000_000).

%% @doc A fee with more decimal digits than the height permits is refused. The
%% limit is on the digits, not the value, so the transaction is well-formed in
%% every other respect and only this rule can refuse it.
oversized_field_test() ->
    Opts = opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    Signed =
        fun(Reward) ->
            sign(
                #tx{ format = 2, anchor = Anchor, reward = Reward },
                Wallet,
                Opts
            )
        end,
    ?assertEqual({ok, true}, verify_result(Signed(?TEST_REWARD), Opts)),
    ?assertEqual(
        {error, <<"invalid-field-size">>},
        verify_result(Signed(?OVERSIZED_REWARD), Opts)
    ).

%% @doc A transfer must name a recipient. From fork 2.4 a transaction moving
%% winstons needs a 32 byte target; only a targetless upload, which moves
%% nothing, may leave it empty.
invalid_target_length_test() ->
    Opts = opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    Signed =
        fun(Target) ->
            sign(
                #tx{
                    format = 2,
                    anchor = Anchor,
                    target = Target,
                    quantity = 1,
                    reward = ?TEST_REWARD
                },
                Wallet,
                Opts
            )
        end,
    ?assertEqual(
        {ok, true},
        verify_result(Signed(crypto:strong_rand_bytes(32)), Opts)
    ),
    ?assertEqual(
        {error, <<"invalid-target-length">>},
        verify_result(Signed(<<>>), Opts)
    ).

self_targeted_test() ->
    Opts = opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    Signed =
        fun(Target) ->
            sign(
                #tx{
                    format = 2,
                    anchor = Anchor,
                    target = Target,
                    reward = ?TEST_REWARD
                },
                Wallet,
                Opts
            )
        end,
    ?assertEqual(
        {ok, true},
        verify_result(Signed(crypto:strong_rand_bytes(32)), Opts)
    ),
    ?assertEqual(
        {error, <<"self-targeted-transaction">>},
        verify_result(Signed(ar_wallet:to_address(Wallet)), Opts)
    ).

negative_data_size_test() ->
    Opts = opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    DataRoot = crypto:strong_rand_bytes(32),
    Signed =
        fun(DataSize) ->
            sign(
                #tx{
                    format = 2,
                    anchor = Anchor,
                    data_size = DataSize,
                    data_root = DataRoot,
                    reward = ?TEST_REWARD
                },
                Wallet,
                Opts
            )
        end,
    ?assertEqual({ok, true}, verify_result(Signed(262144), Opts)),
    ?assertEqual(
        {error, <<"negative-data-size">>},
        verify_result(Signed(-262144), Opts)
    ).

malleable_v1_test() ->
    Opts = opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    Signed =
        fun(Target) ->
            sign(
                #tx{
                    format = 1,
                    anchor = Anchor,
                    target = Target,
                    quantity = 0,
                    reward = ?TEST_REWARD
                },
                Wallet,
                Opts
            )
        end,
    ?assertEqual({ok, true}, verify_result(Signed(<<>>), Opts)),
    ?assertEqual(
        {error, <<"malleable-transaction">>},
        verify_result(Signed(crypto:strong_rand_bytes(32)), Opts)
    ).

%% @doc The record a transaction message converts to names the same sender the
%% message does.
%%
%% `tx@1.0' derives its committer from the owner bytes unconditionally, and the
%% record's cached owner address has to agree, or the two are not the same
%% transaction. They part company only for an owner of 512 zero bytes, where
%% `ar_tx:get_owner_address/1' answers the atom `not_set' -- that being the
%% `#tx{}' default, which upstream reads as "no owner set". Such a transaction
%% cannot verify, because zero is no RSA modulus; what it must not do is put an
%% atom where every consumer expects an address.
record_and_message_name_one_sender_test() ->
    Opts = opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    TX = sign(#tx{ format = 2, anchor = Anchor, reward = ?TEST_REWARD },
        Wallet, Opts),
    ?assertEqual(committer(TX, Opts), (lib_arweave_tx:to_tx(TX, Opts))#tx.owner_address),
    % An owner of 512 zero bytes: still one sender, and still an address.
    Ownerless = with_owner(TX, ?DEFAULT_OWNER, Opts),
    Record = lib_arweave_tx:to_tx(Ownerless, Opts),
    ?assertEqual(committer(Ownerless, Opts), Record#tx.owner_address),
    ?assert(is_binary(Record#tx.owner_address)),
    ?assertEqual(not_set, ar_tx:get_owner_address(Record#tx{ owner_address = not_set })),
    % And it is refused, by the check that asks whether the signature verifies.
    ?assertEqual(
        {error, <<"invalid-signature">>},
        verify_result(Ownerless, Opts)
    ).

%% @doc The address the message's commitment names as its committer.
committer(TX, Opts) ->
    {ok, _, Commitment} = hb_message:commitment(#{}, TX, Opts),
    hb_util:decode(hb_maps:get(<<"committer">>, Commitment, <<>>, Opts)).

%% @doc Restate a transaction's owner, leaving its signature and identifier
%% alone. The owner lives in the commitment, so that is where it is replaced.
with_owner(TX, Owner, Opts) ->
    {ok, ID, Commitment} = hb_message:commitment(#{}, TX, Opts),
    Restated =
        Commitment#{
            <<"keyid">> => <<"publickey:", (hb_util:encode(Owner))/binary>>,
            <<"committer">> =>
                hb_util:encode(ar_wallet:to_address(Owner, ?RSA_KEY_TYPE))
        },
    TX#{ <<"commitments">> => #{ ID => Restated } }.

sign(TX, Wallet, Opts) ->
    lib_arweave_tx:from_tx(ar_tx:sign(TX, Wallet), Opts).

wallets(TX, Opts) ->
    Record = lib_arweave_tx:to_tx(TX, Opts),
    Addresses =
        case Record#tx.target of
            <<>> -> [Record#tx.owner_address];
            Target -> [Record#tx.owner_address, Target]
        end,
    maps:from_list(
        [
            {
                hb_util:encode(Address),
                #{
                    <<"balance">> => 1_000_000_000_000_000_000,
                    <<"last-tx">> => hb_maps:get(<<"anchor">>, TX, <<>>, Opts),
                    <<"denomination">> => 1,
                    <<"mining-permission">> => true
                }
            }
         || Address <- Addresses
        ]
    ).

verify_result(TX, Opts) ->
    Req =
        #{
            <<"path">> => <<"verify">>,
            <<"height">> => ?TEST_HEIGHT,
            <<"wallets">> => wallets(TX, Opts),
            <<"price-per-gib-minute">> => 4897,
            <<"kryder-plus-rate-multiplier">> => 1,
            <<"block-denomination">> => 1
        },
    Base = TX#{ <<"device">> => <<"arweave-tx@2.9">> },
    case hb_ao:resolve(Base, Req, Opts) of
        {ok, Result} ->
            {ok, hb_maps:get(<<"valid">>, Result, not_found, Opts)};
        {error, Error} ->
            {error, hb_maps:get(<<"message">>, Error, not_found, Opts)}
    end.

opts() -> #{ <<"store">> => [hb_test_utils:test_store()] }.

