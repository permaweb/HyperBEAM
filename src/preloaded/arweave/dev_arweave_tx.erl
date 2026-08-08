%%% @doc An AO-Core interface to Arweave layer-one transactions: whether one
%%% is valid at a given height, the identifiers derived from it, and the codec
%%% between its message form and the JSON Arweave serves.
%%%
%%% `verify/3' answers twelve independent questions -- format, denomination,
%%% self-targeting, field sizes, target length, data size, data root,
%%% signature, anchor, balance, fee and malleability -- and names the one that
%%% failed. A single boolean would be cheaper to write and useless to a caller
%%% deciding whether a peer is malicious, a block is invalid, or a wallet is
%%% simply broke, and it would make the mutation tests unable to tell a live
%%% check from a dead one.
%%%
%%% Signatures are RSA-PSS or, from fork 2.9, ECDSA over secp256k1. An ECDSA
%%% transaction carries no public key: the key is recovered from the signature,
%%% and the preimage it signs omits the owner for that reason. All of that
%%% lives in the vendored `ar_tx'/`ar_wallet'; this device chooses the checks
%%% and reports which one failed.
%%%
%%% `height' throughout is the height of the block the transaction is included
%%% in. Upstream's validators take the *previous* height instead
%%% (`ar_node_utils:validate_block(txs, ...)' passes `Height - 1'), so the
%%% conversion happens here, once, rather than at every call site.
-module(dev_arweave_tx).
-implements(<<"arweave-tx@2.9">>).
-device_libraries([lib_arweave_tx]).
-export([info/1, verify/3, id/3, chunk_id/3, weave_size_increase/3, tx_root/3]).
-export([from_json/3, to_json/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Export only the transaction operations, leaving message manipulation
%% to `message@1.0'.
info(_Base) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>]
    }.

%% @doc Validate a transaction for inclusion in a block at `height'. The
%% checks run in the order a cheap one can spare an expensive one, and the
%% first to fail names itself.
%%
%% `wallets' is an account message keyed by base64URL address, as
%% `~arweave-wallets@2.9' returns them. Only the sender's and the recipient's
%% accounts are read from it, so passing the whole account tree costs two
%% lookups rather than a load.
%%
%% NOT CHECKED HERE, deliberately: the replay family. Upstream's block
%% validator additionally rejects a transaction whose identifier is already on
%% the weave (`ar_tx_replay_pool:tx_already_in_weave/2'), already in the
%% mempool (`tx_already_in_mempool/2'), or whose anchor is the identifier of a
%% mempool transaction (`last_tx_in_mempool/2'). All three are questions about
%% a set of *other* transactions, not about this one, and a mempool is a
%% declared non-goal of this subsystem -- so a caller assembling or validating
%% a block must supply that rule itself. `~arweave-block@2.9/check-txs' does,
%% by folding `ar_tx_replay_pool:verify_block_txs/1' over the block rather than
%% this key. Everything upstream's `ar_tx:verify/3' answers about a single
%% transaction is answered below.
verify(Base, Req, Opts) ->
    TX = lib_arweave_tx:to_tx(Base, Opts),
    Height = hb_util:int(required(<<"height">>, Base, Req, Opts)),
    Denomination =
        hb_util:int(get_first(<<"block-denomination">>, Base, Req, 1, Opts)),
    Redenomination =
        hb_util:int(
            get_first(<<"block-redenomination-height">>, Base, Req, 0, Opts)
        ),
    Accounts = accounts(TX, required(<<"wallets">>, Base, Req, Opts), Opts),
    Anchors = anchors(get_first(<<"block-anchors">>, Base, Req, [], Opts), Opts),
    Price = hb_util:int(required(<<"price-per-gib-minute">>, Base, Req, Opts)),
    Kryder =
        hb_util:int(
            get_first(<<"kryder-plus-rate-multiplier">>, Base, Req, 1, Opts)
        ),
    first_failure(
        [
            {
                <<"invalid-format">>,
                <<"The format and signature type are not valid at this height.">>,
                fun() -> valid_format(TX, Height) end
            },
            {
                <<"invalid-denomination">>,
                <<"The denomination is not one the block accepts.">>,
                fun() ->
                    ar_tx:verify_denomination(
                        TX,
                        Denomination,
                        Height - 1,
                        Redenomination
                    )
                end
            },
            {
                <<"self-targeted-transaction">>,
                <<"The sender and the recipient are the same address.">>,
                fun() -> not_self_targeted(TX) end
            },
            {
                <<"invalid-field-size">>,
                <<"A field is longer than the format permits at this height.">>,
                fun() -> valid_field_sizes(TX, Height, Denomination) end
            },
            {
                <<"invalid-target-length">>,
                <<"The target is not an address, and the transaction is not a "
                    "targetless upload.">>,
                fun() -> ar_tx:verify_target_length(TX, Height - 1) end
            },
            {
                <<"negative-data-size">>,
                <<"The data size is negative.">>,
                fun() -> valid_data_size(TX) end
            },
            {
                <<"invalid-data-root">>,
                <<"The data root and data size do not describe the data.">>,
                fun() -> valid_data_root(TX) end
            },
            {
                <<"invalid-signature">>,
                <<"The signature does not verify, or the id is not its hash.">>,
                fun() -> ar_tx:verify_tx_id(TX#tx.id, TX) end
            },
            {
                <<"invalid-anchor">>,
                <<"The anchor is neither the sender's last transaction nor a "
                    "recent block.">>,
                fun() -> valid_anchor(TX, Accounts, Anchors) end
            },
            {
                <<"insufficient-balance">>,
                <<"The sender cannot cover the quantity and the fee.">>,
                fun() -> sufficient_balance(TX, Accounts, Denomination) end
            },
            {
                <<"invalid-fee">>,
                <<"The fee is below the minimum for the data size at this "
                    "height.">>,
                fun() ->
                    sufficient_fee(TX, Height, Accounts, Price, Kryder, Denomination)
                end
            },
            {
                <<"malleable-transaction">>,
                <<"The format 1 signature preimage does not determine the "
                    "transaction.">>,
                fun() ->
                    not_malleable(TX, Height, Accounts, Price, Kryder, Denomination)
                end
            }
        ]
    ).

%% @doc Compute the transaction's identifier from its signature. Recomputed
%% rather than read back from the `id' field, which is what makes it worth
%% asking for.
id(Base, _Req, Opts) ->
    TX = ar_tx:reset_ids(lib_arweave_tx:to_tx(Base, Opts)),
    {ok, #{ <<"id">> => hb_util:encode(TX#tx.id) }}.

%% @doc Compute the identifier of a data chunk: its SHA-256 hash. This is the
%% leaf of the Merkle tree a transaction's `data-root' commits to.
chunk_id(Base, Req, Opts) ->
    {ok,
        #{
            <<"chunk-id">> =>
                hb_util:encode(
                    ar_tx:generate_chunk_id(decoded(<<"chunk">>, Base, Req, Opts))
                )
        }
    }.

%% @doc The number of bytes the weave grows by when a transaction of
%% `data-size' bytes is included at `height'. From fork 2.5 the weave is
%% padded to whole chunks, so it is not the data size.
weave_size_increase(Base, Req, Opts) ->
    DataSize = hb_util:int(required(<<"data-size">>, Base, Req, Opts)),
    Height = hb_util:int(required(<<"height">>, Base, Req, Opts)),
    {ok,
        #{
            <<"weave-size-increase">> =>
                ar_tx:get_weave_size_increase(DataSize, Height)
        }
    }.

%% @doc Compute the Merkle root over a block's transactions, which is the
%% block header's `tx-root'. The tree is built over each transaction's data
%% root at its weave offset, with a padding leaf wherever a transaction is
%% padded to a whole chunk, so it cannot be derived from the identifiers a
%% block header carries -- the bodies are needed.
tx_root(Base, Req, Opts) ->
    Height = hb_util:int(required(<<"height">>, Base, Req, Opts)),
    TXs =
        [
            lib_arweave_tx:to_tx(hb_cache:ensure_loaded(TX, Opts), Opts)
        ||
            TX <-
                hb_util:message_to_ordered_list(
                    required(<<"txs">>, Base, Req, Opts),
                    Opts
                )
        ],
    {ok,
        #{
            <<"tx-root">> =>
                hb_util:encode(ar_block:generate_tx_root_for_block(TXs, Height))
        }
    }.

%% @doc Parse the JSON form Arweave serves at `/tx/<id>'.
from_json(Base, Req, Opts) ->
    {ok,
        lib_arweave_tx:from_tx(
            lib_arweave_tx:from_json_struct(
                hb_json:decode(required(<<"body">>, Base, Req, Opts))
            ),
            Opts
        )
    }.

%% @doc Render a transaction message in the JSON form Arweave serves.
to_json(Base, _Req, Opts) ->
    {ok,
        #{
            <<"body">> =>
                hb_json:encode(
                    lib_arweave_tx:to_json_struct(
                        lib_arweave_tx:to_tx(Base, Opts),
                        Opts
                    )
                )
        }
    }.

%%% Internal functions.

%% @doc Run checks in order, returning the first failure by name. Each check
%% is a thunk so that a cheap check that fails spares the expensive ones after
%% it -- signature verification and fee pricing are both far dearer than the
%% field checks that precede them.
first_failure([]) ->
    {ok, #{ <<"valid">> => true }};
first_failure([{Message, Detail, Check} | Checks]) ->
    case Check() of
        true -> first_failure(Checks);
        false -> {error, error_message(Message, Detail)}
    end.

%% @doc Whether the format exists and its signature type is permitted at the
%% height. Format 2 arrived with fork 2.0 and ECDSA signatures with fork 2.9;
%% both gates read the previous height, as the vendored validators do.
valid_format(#tx{ format = 1, signature_type = ?RSA_KEY_TYPE }, _Height) ->
    true;
valid_format(#tx{ format = 2, signature_type = ?RSA_KEY_TYPE }, Height) ->
    (Height - 1) >= ar_fork:height_2_0();
valid_format(#tx{ format = 2, signature_type = ?ECDSA_KEY_TYPE }, Height) ->
    (Height - 1) >= ar_fork:height_2_9();
valid_format(_TX, _Height) ->
    false.

%% @doc Whether the recipient is somebody other than the sender. A transfer to
%% oneself moves nothing and burns the fee, and the balance check cannot object
%% to it -- the debit and the credit land on the same account -- so it is a
%% rule of its own.
not_self_targeted(TX) ->
    TX#tx.owner_address =/= TX#tx.target.

%% @doc Whether every field is within the byte size its format permits. The two
%% formats differ in which fields exist and how long an anchor may be, so the
%% limits are per-format. The format check above has already run, so only 1 and
%% 2 reach here.
valid_field_sizes(#tx{ format = 1 } = TX, Height, Denomination) ->
    ar_tx:tx_field_size_limit_v1(TX, Height - 1, Denomination);
valid_field_sizes(#tx{ format = 2 } = TX, Height, Denomination) ->
    ar_tx:tx_field_size_limit_v2(TX, Height - 1, Denomination).

%% @doc Whether the data size is a number of bytes. Asked separately from the
%% root below because that check pairs an empty size with an empty root, and a
%% negative size is not empty: a transaction claiming a negative number of
%% bytes under a real root agrees with itself and passes it.
valid_data_size(TX) ->
    TX#tx.data_size >= 0.

%% @doc Whether the data root and data size describe the data. A format 2
%% transaction usually carries no data -- it is uploaded in chunks later --
%% and then only the agreement between an empty root and a zero size can be
%% checked.
valid_data_root(#tx{ data = <<>> } = TX) ->
    (TX#tx.data_size == 0) == (TX#tx.data_root == <<>>);
valid_data_root(#tx{ format = 1 } = TX) ->
    TX#tx.data_size == byte_size(TX#tx.data)
        andalso TX#tx.data_root == ar_tx:data_root(legacy, TX#tx.data);
valid_data_root(TX) ->
    TX#tx.data_size == byte_size(TX#tx.data)
        andalso TX#tx.data_root == ar_tx:data_root(TX#tx.data).

%% @doc Whether the anchor is either the sender's last transaction or one of
%% the recent block hashes. The first form chains a wallet's transactions; the
%% second lets a wallet post several at once, bounded by how far back the
%% anchor may reach.
valid_anchor(TX, Accounts, Anchors) ->
    case maps:get(TX#tx.owner_address, Accounts, not_found) of
        {_Balance, LastTX} when LastTX == TX#tx.anchor ->
            true;
        {_Balance, LastTX, _Denomination, _MiningPermission}
                when LastTX == TX#tx.anchor ->
            true;
        _ ->
            lists:member(TX#tx.anchor, Anchors)
    end.

%% @doc Whether the sender can cover the transfer and the fee. Applying the
%% transaction and checking the result is how the vendored code expresses it,
%% and it is the only form that gets redenomination right.
sufficient_balance(TX, Accounts, Denomination) ->
    TX#tx.quantity >= 0
        andalso maps:is_key(TX#tx.owner_address, Accounts)
        andalso
            case
                maps:get(
                    TX#tx.owner_address,
                    ar_node_utils:apply_tx(Accounts, Denomination, TX)
                )
            of
                {Balance, _LastTX} -> Balance >= 0;
                {Balance, _LastTX, _Denomination, _MiningPermission} -> Balance >= 0
            end.

%% @doc Whether the fee meets the minimum for the bytes the transaction adds
%% to the weave. Transferring to an address with no account costs extra, which
%% is why the accounts are part of the price.
sufficient_fee(TX, Height, Accounts, Price, Kryder, Denomination) ->
    Minimum =
        ar_tx:get_tx_fee(
            {
                ar_tx:get_weave_size_increase(TX, Height),
                Price,
                Kryder,
                TX#tx.target,
                Accounts,
                Height
            }
        ),
    ar_pricing:redenominate(TX#tx.reward, TX#tx.denomination, Denomination)
        >= Minimum.

%% @doc Whether the transaction's signature preimage determines it. A format 1
%% preimage concatenates fields without delimiters, so a transaction can be
%% edited into a different one that the same signature still covers -- a target
%% moved into the quantity, a fee shortened by a digit. Format 2 hashes its
%% fields as a structure and a preimage with an explicit denomination is a
%% structure too, so neither is at risk and upstream asks the question only of
%% the rest.
not_malleable(#tx{ format = 1 } = TX, Height, Accounts, Price, Kryder, Denomination) ->
    ar_tx:verify_malleability(
        {TX, Price, Kryder, Denomination, Height - 1, Accounts}
    );
not_malleable(_TX, _Height, _Accounts, _Price, _Kryder, _Denomination) ->
    true.

%% @doc Read the sender's and the recipient's accounts out of the wallets
%% message, in the shape the vendored account functions take. Reading two
%% addresses rather than folding the message is what keeps the account tree
%% unloaded.
accounts(TX, Wallets, Opts) ->
    maps:from_list(
        lists:filtermap(
            fun(Address) ->
                case hb_maps:get(hb_util:encode(Address), Wallets, not_found, Opts) of
                    not_found -> false;
                    Account -> {true, {Address, account(Account, Opts)}}
                end
            end,
            addresses(TX)
        )
    ).

addresses(#tx{ owner_address = From, target = <<>> }) -> [From];
addresses(#tx{ owner_address = From, target = To }) -> [From, To].

account(Account, Opts) ->
    {
        hb_util:int(hb_maps:get(<<"balance">>, Account, 0, Opts)),
        decoded_value(hb_maps:get(<<"last-tx">>, Account, <<>>, Opts)),
        hb_util:int(hb_maps:get(<<"denomination">>, Account, 1, Opts)),
        hb_util:bool(hb_maps:get(<<"mining-permission">>, Account, true, Opts))
    }.

%% @doc Decode the recent block hashes a transaction may anchor to.
anchors(Anchors, Opts) ->
    [
        decoded_value(Anchor)
    ||
        Anchor <- hb_util:message_to_ordered_list(Anchors, Opts)
    ].

%% @doc Read a key from the request, falling back to the base message.
get_first(Key, Base, Req, Default, Opts) ->
    hb_ao:get_first([{Req, Key}, {Base, Key}], Default, Opts).

%% @doc Read a key that has no meaningful default.
required(Key, Base, Req, Opts) ->
    case get_first(Key, Base, Req, not_found, Opts) of
        not_found -> throw({missing_key, Key});
        Value -> Value
    end.

%% @doc Read a required base64URL key, decoded.
decoded(Key, Base, Req, Opts) ->
    decoded_value(required(Key, Base, Req, Opts)).

%% @doc Decode a base64URL value that arrived from a peer. `hb_util:decode/1'
%% is unchecked and would turn a malformed value into plausible bytes.
decoded_value(Value) ->
    case hb_util:safe_decode(Value) of
        {ok, Decoded} -> Decoded;
        {error, _} -> throw({invalid_base64, Value})
    end.

%% @doc Build the standard error body.
error_message(Message, Detail) ->
    #{
        <<"status">> => 422,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.

%%% Tests. The golden vectors are real mainnet blocks and the full body of
%%% every transaction they reference, held under `test/fixtures/arweave'.

-define(FIXTURES, "test/fixtures/arweave").
%%% A post-2.9 mainnet block with 26 transactions.
-define(TEST_HEIGHT, 1974871).
%%% A fee comfortably above the minimum at `?TEST_HEIGHT' for a transaction
%%% that adds no data to the weave, so that a transaction signed here reaches
%%% the check it is written for rather than stopping at the fee.
-define(TEST_REWARD, 1_000_000_000_000).

test_opts() ->
    #{ <<"store">> => [hb_test_utils:test_store()] }.

test_block(Height) ->
    {ok, Bin} =
        file:read_file(
            ?FIXTURES ++ "/block-" ++ integer_to_list(Height) ++ ".json"
        ),
    hb_json:decode(Bin).

%% @doc Every transaction of a fixture block, as messages, in block order.
test_txs(Height, Opts) ->
    [
        test_tx(Height, ID, Opts)
    ||
        ID <- hb_maps:get(<<"txs">>, test_block(Height), [], Opts)
    ].

test_tx(Height, ID, Opts) ->
    {ok, Bin} =
        file:read_file(
            ?FIXTURES ++ "/txs-" ++ integer_to_list(Height) ++ "/"
                ++ binary_to_list(ID) ++ ".json"
        ),
    hb_util:ok(
        test_resolve(
            #{ <<"device">> => <<"arweave-tx@2.9">> },
            #{ <<"path">> => <<"from-json">>, <<"body">> => Bin },
            Opts
        )
    ).

%% @doc Add the device and drop the private hashpath the resolver records, so
%% that a converted transaction can be handed back to the device and compared.
test_message(TX, Opts) ->
    hb_maps:without(
        [<<"priv">>],
        TX#{ <<"device">> => <<"arweave-tx@2.9">> },
        Opts
    ).

test_resolve(Base, Req, Opts) ->
    case hb_ao:resolve(Base, Req, Opts) of
        {ok, Result} -> {ok, hb_maps:without([<<"priv">>], Result, Opts)};
        Other -> Other
    end.

%% @doc An account message for a transaction's sender, funded and anchored so
%% that every check but the one under test passes.
%% The recipient is funded too. A transfer to an address with no account
%% carries a surcharge, so a wallets message holding only the sender would
%% price every transfer above what its sender actually paid.
test_wallets(TX, Opts) ->
    Record = lib_arweave_tx:to_tx(TX, Opts),
    maps:from_list(
        [
            {hb_util:encode(Address), test_account(TX, Opts)}
        ||
            Address <- [Record#tx.owner_address | test_target(Record)]
        ]
    ).

test_target(#tx{ target = <<>> }) -> [];
test_target(#tx{ target = Target }) -> [Target].

test_account(TX, Opts) ->
    #{
        <<"balance">> => 1_000_000_000_000_000_000,
        <<"last-tx">> => hb_maps:get(<<"last-tx">>, TX, not_found, Opts),
        <<"denomination">> => 1,
        <<"mining-permission">> => true
    }.

%% @doc The address of a transaction's owner, via the bridge rather than a
%% second implementation of the hash.
test_sender(TX, Opts) ->
    Record = lib_arweave_tx:to_tx(TX, Opts),
    hb_util:encode(Record#tx.owner_address).

%% @doc Sign a transaction with a fresh key and render it in message form.
%%
%% Mutating a fixture transaction breaks its signature, so such a mutant only
%% ever proves that *something* rejects it. The field-shape checks are worth
%% having precisely because a signer is free to sign a malformed transaction:
%% the fields below are all covered by the signature, so upstream rejects
%% these on their own terms and a signature check never would. That is the
%% divergence the checks close, and only a genuinely signed subject shows it.
test_signed(TX, Wallet, Opts) ->
    lib_arweave_tx:from_tx(ar_tx:sign(TX, Wallet), Opts).

%% @doc The first transaction of the fixture block that transfers to another
%% address, which is the only shape the recipient's account is priced into.
test_transfer(Opts) ->
    hd(
        [
            TX
        ||
            TX <- test_txs(?TEST_HEIGHT, Opts),
            hb_maps:get(<<"target">>, TX, <<>>, Opts) =/= <<>>
        ]
    ).

%%% Codec and identifier tests.

%% @doc Every transaction of a mainnet block parses, and its recomputed
%% identifier is the one the block referenced. The identifier is the hash of
%% the signature, so this is also a signature-shape check across 26 real
%% transactions.
id_test() ->
    Opts = test_opts(),
    IDs = hb_maps:get(<<"txs">>, test_block(?TEST_HEIGHT), [], Opts),
    lists:foreach(
        fun({ID, TX}) ->
            ?assertEqual(
                {ok, #{ <<"id">> => ID }},
                test_resolve(test_message(TX, Opts), <<"id">>, Opts)
            )
        end,
        lists:zip(IDs, test_txs(?TEST_HEIGHT, Opts))
    ).

%% @doc The JSON codec round-trips a real transaction byte for byte, once the
%% key order JSON does not fix is put aside.
json_round_trip_test() ->
    Opts = test_opts(),
    lists:foreach(
        fun(TX) ->
            {ok, #{ <<"body">> := JSON }} =
                test_resolve(test_message(TX, Opts), <<"to-json">>, Opts),
            {ok, Reparsed} =
                test_resolve(
                    #{ <<"device">> => <<"arweave-tx@2.9">> },
                    #{ <<"path">> => <<"from-json">>, <<"body">> => JSON },
                    Opts
                ),
            ?assertEqual(TX, Reparsed)
        end,
        test_txs(?TEST_HEIGHT, Opts)
    ).

%% @doc A chunk's identifier is its SHA-256 hash.
chunk_id_test() ->
    Opts = test_opts(),
    Chunk = crypto:strong_rand_bytes(262144),
    ?assertEqual(
        {ok, #{ <<"chunk-id">> => hb_util:encode(crypto:hash(sha256, Chunk)) }},
        test_resolve(
            #{ <<"device">> => <<"arweave-tx@2.9">> },
            #{ <<"path">> => <<"chunk-id">>, <<"chunk">> => hb_util:encode(Chunk) },
            Opts
        )
    ).

%% @doc The weave grows by whole chunks, not by the data size.
weave_size_increase_test() ->
    Opts = test_opts(),
    lists:foreach(
        fun({DataSize, Expected}) ->
            ?assertEqual(
                {ok, #{ <<"weave-size-increase">> => Expected }},
                test_resolve(
                    #{ <<"device">> => <<"arweave-tx@2.9">> },
                    #{
                        <<"path">> => <<"weave-size-increase">>,
                        <<"data-size">> => DataSize,
                        <<"height">> => ?TEST_HEIGHT
                    },
                    Opts
                )
            )
        end,
        [{0, 0}, {1, 262144}, {262144, 262144}, {262145, 524288}]
    ).

%%% Golden vector: the tx root of real mainnet blocks.

%% @doc The tx root recomputed from the transaction bodies is the one in the
%% block header. This is the check that needs the bodies -- a block carries
%% only identifiers -- and it covers a block with no transactions, a small
%% block and the largest in the fixture set.
tx_root_test() ->
    Opts = test_opts(),
    lists:foreach(
        fun(Height) ->
            ?assertEqual(
                {ok,
                    #{
                        <<"tx-root">> =>
                            hb_maps:get(
                                <<"tx_root">>,
                                test_block(Height),
                                not_found,
                                Opts
                            )
                    }
                },
                test_resolve(
                    #{ <<"device">> => <<"arweave-tx@2.9">> },
                    #{
                        <<"path">> => <<"tx-root">>,
                        <<"txs">> => test_txs(Height, Opts),
                        <<"height">> => Height
                    },
                    Opts
                )
            )
        end,
        [1974876, 1974240, ?TEST_HEIGHT, 1974880]
    ).

%%% Mutation tests. Each mutates exactly the field its check guards and
%%% asserts the error that check produces. A mutant that still verifies means
%%% the check is not doing anything.

%% @doc Resolve `verify' and reduce the result to its error `message'.
verify_result(TX, Extra, Opts) ->
    Req =
        hb_maps:merge(
            #{
                <<"path">> => <<"verify">>,
                <<"height">> => ?TEST_HEIGHT,
                <<"wallets">> => test_wallets(TX, Opts),
                <<"price-per-gib-minute">> =>
                    hb_util:int(
                        hb_maps:get(
                            <<"price_per_gib_minute">>,
                            test_block(?TEST_HEIGHT),
                            not_found,
                            Opts
                        )
                    ),
                <<"kryder-plus-rate-multiplier">> =>
                    hb_util:int(
                        hb_maps:get(
                            <<"kryder_plus_rate_multiplier">>,
                            test_block(?TEST_HEIGHT),
                            not_found,
                            Opts
                        )
                    ),
                <<"block-denomination">> =>
                    hb_util:int(
                        hb_maps:get(
                            <<"denomination">>,
                            test_block(?TEST_HEIGHT),
                            not_found,
                            Opts
                        )
                    )
            },
            Extra,
            Opts
        ),
    case hb_ao:resolve(test_message(TX, Opts), Req, Opts) of
        {ok, Result} -> {ok, hb_maps:get(<<"valid">>, Result, not_found, Opts)};
        {error, Error} -> {error, hb_maps:get(<<"message">>, Error, not_found, Opts)}
    end.

%% @doc The first transaction of the fixture block, which every mutation test
%% starts from.
test_subject(Opts) ->
    hd(test_txs(?TEST_HEIGHT, Opts)).

%% @doc Every transaction of a real mainnet block verifies against a funded
%% account: 26 signatures, 26 fees priced at the block's own
%% `price_per_gib_minute', 26 data roots. The fee in particular is a real
%% number set by a real miner, so a fee formula that is wrong in either
%% direction fails here.
verify_test() ->
    Opts = test_opts(),
    lists:foreach(
        fun(TX) -> ?assertEqual({ok, true}, verify_result(TX, #{}, Opts)) end,
        test_txs(?TEST_HEIGHT, Opts)
    ).

%% @doc A signature that is genuine, but not this owner's. The identifier is
%% recomputed over it, so the transaction is internally consistent: the id is
%% the hash of the signature it carries, and the signature is a real RSA-PSS
%% signature over this transaction's own signing preimage. Only the public-key
%% verification can tell that another key made it.
%%
%% This is the mutant that isolates `ar_tx:verify_signature/1'. The three
%% conjuncts of `ar_tx:verify_tx_id/2' -- `ExpectedID == ID',
%% `verify_signature/1' and `verify_hash/1' -- share one error message, and
%% `verify_hash/1' is `ID == sha256(signature)'. So every mutation that touches
%% the signature bytes or the identifier breaks `verify_hash/1' too, and the
%% RSA verification could be deleted outright without any of them noticing.
%% Re-signing with a foreign key leaves the id and the hash self-consistent and
%% moves nothing else.
foreign_signature_test() ->
    Opts = test_opts(),
    Wallet = ar_wallet:new(),
    {ForeignKey, _ForeignPub} = ar_wallet:new(),
    Signed =
        test_signed(
            #tx{
                format = 2,
                anchor = crypto:strong_rand_bytes(32),
                reward = ?TEST_REWARD
            },
            Wallet,
            Opts
        ),
    % The control: the same transaction, signed by the key its owner names.
    ?assertEqual({ok, true}, verify_result(Signed, #{}, Opts)),
    Record = lib_arweave_tx:to_tx(Signed, Opts),
    Forged =
        Record#tx{
            signature =
                ar_wallet:sign(
                    ForeignKey,
                    ar_tx:generate_signature_data_segment(Record)
                )
        },
    Foreign =
        lib_arweave_tx:from_tx(
            Forged#tx{ id = ar_tx:generate_id(Forged, signed) },
            Opts
        ),
    ?assertEqual(
        hb_maps:get(<<"owner">>, Signed, not_found, Opts),
        hb_maps:get(<<"owner">>, Foreign, not_found, Opts)
    ),
    ?assertEqual({error, <<"invalid-signature">>},
        verify_result(Foreign, #{}, Opts)).

%% @doc Flipping one bit of the signature breaks it. The identifier is the
%% hash of the signature, so this is the mutation an attacker cannot repair
%% without the key.
tampered_signature_test() ->
    Opts = test_opts(),
    TX = test_subject(Opts),
    ?assertEqual(
        {error, <<"invalid-signature">>},
        verify_result(
            TX#{
                <<"signature">> =>
                    hb_util:encode(
                        flip_bit(
                            decoded_value(
                                hb_maps:get(<<"signature">>, TX, not_found, Opts)
                            )
                        )
                    )
            },
            #{},
            Opts
        )
    ).

%% @doc Replacing the identifier with another valid identifier breaks the
%% transaction: the identifier must be the hash of this signature.
tampered_id_test() ->
    Opts = test_opts(),
    TX = test_subject(Opts),
    ?assertEqual(
        {error, <<"invalid-signature">>},
        verify_result(
            TX#{ <<"id">> => hb_util:encode(crypto:strong_rand_bytes(32)) },
            #{},
            Opts
        )
    ).

%% @doc Changing a signed field breaks the signature, even though every field
%% is still well formed on its own.
tampered_target_test() ->
    Opts = test_opts(),
    TX = test_subject(Opts),
    ?assertEqual(
        {error, <<"invalid-signature">>},
        verify_result(
            TX#{ <<"target">> => hb_util:encode(crypto:strong_rand_bytes(32)) },
            #{},
            Opts
        )
    ).

%% @doc A data size that does not agree with an empty data root is rejected
%% before the signature is even looked at.
mismatched_data_root_test() ->
    Opts = test_opts(),
    TX = test_subject(Opts),
    ?assertEqual(
        {error, <<"invalid-data-root">>},
        verify_result(TX#{ <<"data-root">> => <<>> }, #{}, Opts)
    ).

%% @doc An anchor that is neither the sender's last transaction nor a recent
%% block is rejected. This is what stops a transaction being replayed.
invalid_anchor_test() ->
    Opts = test_opts(),
    TX = test_subject(Opts),
    Wallets = test_wallets(TX, Opts),
    ?assertEqual(
        {error, <<"invalid-anchor">>},
        verify_result(
            TX,
            #{
                <<"wallets">> =>
                    hb_maps:map(
                        fun(_Address, Account) ->
                            Account#{
                                <<"last-tx">> =>
                                    hb_util:encode(crypto:strong_rand_bytes(32))
                            }
                        end,
                        Wallets,
                        Opts
                    )
            },
            Opts
        )
    ).

%% @doc An anchor the sender's account does not know is still valid when it
%% names a recent block, which is how a wallet posts several transactions at
%% once.
block_anchor_test() ->
    Opts = test_opts(),
    TX = test_subject(Opts),
    Wallets = test_wallets(TX, Opts),
    ?assertEqual(
        {ok, true},
        verify_result(
            TX,
            #{
                <<"wallets">> =>
                    hb_maps:map(
                        fun(_Address, Account) ->
                            Account#{
                                <<"last-tx">> =>
                                    hb_util:encode(crypto:strong_rand_bytes(32))
                            }
                        end,
                        Wallets,
                        Opts
                    ),
                <<"block-anchors">> =>
                    [hb_maps:get(<<"last-tx">>, TX, not_found, Opts)]
            },
            Opts
        )
    ).

%% @doc A sender who cannot cover the fee is rejected.
insufficient_balance_test() ->
    Opts = test_opts(),
    TX = test_subject(Opts),
    ?assertEqual(
        {error, <<"insufficient-balance">>},
        verify_result(
            TX,
            #{
                <<"wallets">> =>
                    hb_maps:map(
                        fun(_Address, Account) -> Account#{ <<"balance">> => 1 } end,
                        test_wallets(TX, Opts),
                        Opts
                    )
            },
            Opts
        )
    ).

%% @doc A sender with no account at all is rejected, rather than treated as
%% having an unlimited balance. The anchor is supplied as a recent block so
%% that the transaction reaches the balance check at all.
unknown_sender_test() ->
    Opts = test_opts(),
    TX = test_subject(Opts),
    ?assertEqual(
        {error, <<"insufficient-balance">>},
        verify_result(
            TX,
            #{
                <<"wallets">> => #{},
                <<"block-anchors">> =>
                    [hb_maps:get(<<"last-tx">>, TX, not_found, Opts)]
            },
            Opts
        )
    ).

%% @doc A fee below the minimum for the data size is rejected. Lowering the
%% fee also breaks the signature, so the fee check is exercised by raising the
%% price of a byte instead.
insufficient_fee_test() ->
    Opts = test_opts(),
    ?assertEqual(
        {error, <<"invalid-fee">>},
        verify_result(
            test_subject(Opts),
            #{ <<"price-per-gib-minute">> => 1_000_000_000_000 },
            Opts
        )
    ).

%% @doc A transfer to an address that has no account costs a surcharge. Every
%% transaction in the fixture block pays its minimum to the winston, so taking
%% the recipient's account away is on its own enough to put the fee below what
%% is required.
new_account_fee_test() ->
    Opts = test_opts(),
    TX = test_transfer(Opts),
    ?assertEqual({ok, true}, verify_result(TX, #{}, Opts)),
    ?assertEqual(
        {error, <<"invalid-fee">>},
        verify_result(
            TX,
            #{
                <<"wallets">> =>
                    hb_maps:with(
                        [test_sender(TX, Opts)],
                        test_wallets(TX, Opts),
                        Opts
                    )
            },
            Opts
        )
    ).

%% @doc A transaction of an unknown format is rejected.
invalid_format_test() ->
    Opts = test_opts(),
    TX = test_subject(Opts),
    ?assertEqual(
        {error, <<"invalid-format">>},
        verify_result(TX#{ <<"format">> => 3 }, #{}, Opts)
    ).

%% @doc An ECDSA transaction is not valid before fork 2.9, whatever else is
%% true of it.
ecdsa_before_fork_test() ->
    Opts = test_opts(),
    TX = test_subject(Opts),
    ?assertEqual(
        {error, <<"invalid-format">>},
        verify_result(
            TX#{ <<"signature-type">> => <<"ecdsa-secp256k1">> },
            #{ <<"height">> => ar_fork:height_2_9() - 1 },
            Opts
        )
    ).

%% @doc A denomination the block does not accept is rejected. Two ways to be
%% unacceptable: naming none at all once a redenomination is scheduled, where
%% an unqualified fee is ambiguous between the old unit and the new one, and
%% naming one above the block's own.
%%
%% The first needs no mutation at all. The subject is an unaltered mainnet
%% transaction, signature and identifier intact, and the only thing that
%% changes is the block it is offered to -- which is what makes it the sharpest
%% case in this file: nothing else in `verify' can tell the two blocks apart.
invalid_denomination_test() ->
    Opts = test_opts(),
    TX = test_subject(Opts),
    ?assertEqual(
        {error, <<"invalid-denomination">>},
        verify_result(
            TX,
            #{ <<"block-redenomination-height">> => ?TEST_HEIGHT },
            Opts
        )
    ),
    ?assertEqual(
        {error, <<"invalid-denomination">>},
        verify_result(TX#{ <<"denomination">> => 2 }, #{}, Opts)
    ).

%% @doc A field longer than its format permits is rejected, signature intact.
%% The anchor is the field whose limit moved -- 32 bytes before fork 1.8, 48
%% after -- and it is covered by the signature, so a signer is free to produce
%% a 64 byte one and no signature check will ever object to it.
oversized_field_test() ->
    Opts = test_opts(),
    Wallet = ar_wallet:new(),
    Signed =
        fun(Anchor) ->
            test_signed(
                #tx{ format = 2, anchor = Anchor, reward = ?TEST_REWARD },
                Wallet,
                Opts
            )
        end,
    ?assertEqual(
        {ok, true},
        verify_result(Signed(crypto:strong_rand_bytes(32)), #{}, Opts)
    ),
    ?assertEqual(
        {error, <<"invalid-field-size">>},
        verify_result(Signed(crypto:strong_rand_bytes(64)), #{}, Opts)
    ).

%% @doc A target that is not an address is rejected, signature intact. From
%% fork 2.4 a target is either exactly an address or absent, so 33 bytes is
%% neither -- and, like the anchor, it is signed, so only this check catches it.
invalid_target_length_test() ->
    Opts = test_opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    Signed =
        fun(Target) ->
            test_signed(
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
        verify_result(Signed(crypto:strong_rand_bytes(32)), #{}, Opts)
    ),
    ?assertEqual(
        {error, <<"invalid-target-length">>},
        verify_result(Signed(crypto:strong_rand_bytes(33)), #{}, Opts)
    ).

%% @doc A transaction whose recipient is its own sender is rejected, signature
%% intact. Nothing else in `verify' objects to it: the target is a well formed
%% address, and the balance check nets the debit and the credit against the one
%% account and finds it solvent.
self_targeted_test() ->
    Opts = test_opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    Signed =
        fun(Target) ->
            test_signed(
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
        verify_result(Signed(crypto:strong_rand_bytes(32)), #{}, Opts)
    ),
    ?assertEqual(
        {error, <<"self-targeted-transaction">>},
        verify_result(Signed(ar_wallet:to_address(Wallet)), #{}, Opts)
    ).

%% @doc A negative data size is rejected, signature intact. The data root check
%% that follows it cannot see this one: it asks whether an empty size and an
%% empty root agree, and minus five bytes under a real root is a pair of
%% non-empties like any honest upload. The control differs only in the sign.
negative_data_size_test() ->
    Opts = test_opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    DataRoot = crypto:strong_rand_bytes(32),
    Signed =
        fun(DataSize) ->
            test_signed(
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
    ?assertEqual({ok, true}, verify_result(Signed(262144), #{}, Opts)),
    ?assertEqual(
        {error, <<"negative-data-size">>},
        verify_result(Signed(-262144), #{}, Opts)
    ).

%% @doc A format 1 transaction whose preimage does not determine it is
%% rejected, even though every field is well formed and the signature is
%% genuine. The preimage concatenates the target and the quantity with no
%% delimiter, so a transaction that transfers nothing to an address reads
%% equally as one that transfers to nobody with the address absorbed into the
%% amount -- and one signature covers both readings.
%%
%% Format 1 is legacy and no block in the fixture set contains one, so the
%% subject is signed here. The transaction with no target is the control, and
%% it verifies, so the rejection below is the malleability check and not the
%% pipeline refusing a synthetic transaction.
malleable_v1_test() ->
    Opts = test_opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    Signed =
        fun(Target) ->
            test_signed(
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
    ?assertEqual({ok, true}, verify_result(Signed(<<>>), #{}, Opts)),
    ?assertEqual(
        {error, <<"malleable-transaction">>},
        verify_result(Signed(crypto:strong_rand_bytes(32)), #{}, Opts)
    ).

%% @doc Flip the lowest bit of a binary's last byte.
flip_bit(Bin) ->
    Size = byte_size(Bin) - 1,
    << Head:Size/binary, Byte:8 >> = Bin,
    << Head/binary, (Byte bxor 1):8 >>.
