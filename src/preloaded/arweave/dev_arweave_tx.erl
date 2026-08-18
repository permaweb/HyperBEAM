%%% @doc An AO-Core interface to Arweave layer-one transactions: whether one
%%% is valid at a given height and the identifiers derived from it.
%%%
%%% That message form is `tx@1.0', HyperBEAM's own committed transaction
%%% message, and it is the only one -- so a transaction this device admits is
%%% the same message the cache indexes and a query returns. `lib_arweave_tx'
%%% is the boundary between it and the `#tx{}' record the vendored rules work
%%% on.
%%%
%%% `verify/3' answers twelve independent questions -- format, denomination,
%%% self-targeting, field sizes, target length, data size, data root,
%%% signature, anchor, balance, fee and malleability -- and names the one that
%%% failed. A single boolean would not tell a caller whether the peer response,
%%% block, or wallet state caused the rejection.
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
-include("include/hb.hrl").

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
%% leaf of the Merkle tree a transaction's `data_root' commits to.
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
        not_found -> throw({'missing-key', Key});
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
        {error, _} -> throw({'invalid-base64', Value})
    end.

%% @doc Build the standard error body.
error_message(Message, Detail) ->
    #{
        <<"status">> => 422,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.
