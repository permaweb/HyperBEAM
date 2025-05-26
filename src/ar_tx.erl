%%% @doc The module with utilities for transaction creation, signing, and verification.
-module(ar_tx).

-export([sign/2, verify/1, verify_tx_id/2]).
-export([enforce_valid_tx/1, enforce_valid_unsigned_tx/1, enforce_valid_signed_tx/1]).
-export([json_struct_to_tx/1, tx_to_json_struct/1]).

%% Used for testing
-export([new/0]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% @doc Cryptographically sign (claim ownership of) a transaction.
sign(TX, {PrivKey, PubKey = {KeyType, Owner}}) ->
    TX2 = TX#tx{ owner = Owner, signature_type = KeyType,
            owner_address = ar_wallet:to_address(Owner, KeyType) },
    SignatureDataSegment = generate_signature_data_segment(TX2),
    sign(TX2, PrivKey, PubKey, SignatureDataSegment).

sign(TX, PrivKey, PubKey = {KeyType, Owner}) ->
    TX2 = TX#tx{ owner = Owner, signature_type = KeyType,
            owner_address = ar_wallet:to_address(Owner, KeyType) },
    SignatureDataSegment = generate_signature_data_segment(TX2),
    sign(TX2, PrivKey, PubKey, SignatureDataSegment).

%% @doc Cryptographically sign (claim ownership of) a v1 transaction.
%% Used in tests and by the handler of the POST /unsigned_tx endpoint, which is
%% disabled by default.
sign_v1(TX, {PrivKey, PubKey = {_, Owner}}) ->
    sign(TX, PrivKey, PubKey, signature_data_segment_v1(TX#tx{ owner = Owner })).

sign_v1(TX, PrivKey, PubKey = {_, Owner}) ->
    sign(TX, PrivKey, PubKey, signature_data_segment_v1(TX#tx{ owner = Owner })).


%% @doc Verify whether a transaction is valid.
%% 
%% Checks that are missing:
%% - format 2 unsupported pre-2.0
%% - valid ECDSA signature post-2.9
%% - verify_denomination
%% - is_tx_fee_sufficient
%% - tx_field_size_limit_v1/v2
%% - check_last_tx
%% - validate_overspend
%% - verify_malleabilitiy
%% - verify_target_length
verify(TX) ->
    From = ar_wallet:to_address(TX#tx.owner, TX#tx.signature_type),
    Checks = [
        {"tx_format_not_supported", TX#tx.format == 1 orelse TX#tx.format == 2},
        {"invalid_signature_type", {?RSA_SIGN_ALG, 65537} == TX#tx.signature_type},
        {"quantity_negative", TX#tx.quantity >= 0},
        {"same_owner_as_target", (From =/= TX#tx.target)},
        {"tx_id_not_valid", verify_hash(TX)},
        {"tx_signature_not_valid", verify_signature(TX)}
    ] ++ verify_v1(TX) ++ verify_v2(TX),
    collect_validation_results(TX#tx.id, Checks).

%% @doc Verify the given transaction actually has the given identifier.
verify_tx_id(ExpectedID, #tx{ id = ID } = TX) ->
    ExpectedID == ID andalso verify_signature(TX) andalso verify_hash(TX).

%%%===================================================================
%%% Private functions.
%%%===================================================================

%% @doc Verify the transaction's signature.
verify_signature(TX = #tx{ signature_type = SigType }) ->
    case generate_signature_data_segment(TX) of
        error ->
            false;
        SignatureDataSegment ->
            ar_wallet:verify({SigType, TX#tx.owner}, SignatureDataSegment, TX#tx.signature)
    end.

%% @doc Generate the data segment to be signed for a given TX.
generate_signature_data_segment(#tx{ format = 2 } = TX) ->
    signature_data_segment_v2(TX);
generate_signature_data_segment(#tx{ format = 1 } = TX) ->
    signature_data_segment_v1(TX);
generate_signature_data_segment(_) ->
    error.

%% @doc Generate the data segment to be signed for a given v2 TX.
signature_data_segment_v2(TX) ->
    true = enforce_valid_tx(TX),
    List = [
        << (integer_to_binary(TX#tx.format))/binary >>,
        << (TX#tx.owner)/binary >>,
        << (TX#tx.target)/binary >>,
        << (list_to_binary(integer_to_list(TX#tx.quantity)))/binary >>,
        << (list_to_binary(integer_to_list(TX#tx.reward)))/binary >>,
        << (TX#tx.last_tx)/binary >>,
        tags_to_list(TX#tx.tags),
        << (integer_to_binary(TX#tx.data_size))/binary >>,
        << (TX#tx.data_root)/binary >>
    ],
    List2 =
        case TX#tx.denomination > 0 of
            true ->
                [<< (integer_to_binary(TX#tx.denomination))/binary >> | List];
            false ->
                List
        end,
    ar_deep_hash:hash(List2).

%% @doc Generate the data segment to be signed for a given v1 TX.
signature_data_segment_v1(TX) ->
    true = enforce_valid_tx(TX),
    <<
        (TX#tx.owner)/binary,
        (TX#tx.target)/binary,
        (TX#tx.data)/binary,
        (list_to_binary(integer_to_list(TX#tx.quantity)))/binary,
        (list_to_binary(integer_to_list(TX#tx.reward)))/binary,
        (TX#tx.last_tx)/binary,
        (tags_to_binary(TX#tx.tags))/binary
    >>.

tags_to_list(Tags) ->
    [[Name, Value] || {Name, Value} <- Tags].

%% @doc Convert a transactions key-value tags to binary a format.
tags_to_binary(Tags) ->
    list_to_binary(
        lists:foldr(
            fun({Name, Value}, Acc) ->
                [Name, Value | Acc]
            end,
            [],
            Tags
        )
    ).

verify_v1(#tx{ format = 1 }) ->
    [];
verify_v1(_) ->
    [].

verify_v2(#tx{ format = 2 } = TX) ->
    [
        {"tx_data_size_negative", TX#tx.data_size >= 0},
        {"tx_data_size_data_root_mismatch", (TX#tx.data_size == 0) == (TX#tx.data_root == <<>>)}
    ];
verify_v2(_) ->
    [].

sign(TX, PrivKey, {KeyType, Owner}, SignatureDataSegment) ->
    true = enforce_valid_unsigned_tx(TX),
    NewTX = TX#tx{ owner = Owner, signature_type = KeyType,
            owner_address = ar_wallet:to_address(Owner, KeyType) },
    Sig = ar_wallet:sign(PrivKey, SignatureDataSegment),
    ID = crypto:hash(?HASH_ALG, <<Sig/binary>>),
    NewTX#tx{ id = ID, signature = Sig }.

%% @doc Verify that the transaction's ID is a hash of its signature.
verify_hash(#tx{ signature = Sig, id = ID }) ->
    ID == crypto:hash(sha256, << Sig/binary >>).

collect_validation_results(_TXID, Checks) ->
    KeepFailed = fun
        ({_, true}) -> false;
        ({ErrorCode, false}) -> {true, ErrorCode}
    end,
    case lists:filtermap(KeepFailed, Checks) of
        [] -> true;
        FailedChecks -> 
            ?event({tx_validation_failed, FailedChecks}),
            false
    end.

json_struct_to_tx(TXStruct) ->
    Tags =
        case hb_util:find_value(<<"tags">>, TXStruct) of
            undefined ->
                [];
            Xs ->
                Xs
        end,
    Data = hb_util:decode(hb_util:find_value(<<"data">>, TXStruct)),
    Format =
        case hb_util:find_value(<<"format">>, TXStruct) of
            undefined ->
                1;
            N when is_integer(N) ->
                N;
            N when is_binary(N) ->
                binary_to_integer(N)
        end,
    Denomination =
        case hb_util:find_value(<<"denomination">>, TXStruct) of
            undefined ->
                0;
            EncodedDenomination ->
                MaybeDenomination = binary_to_integer(EncodedDenomination),
                true = MaybeDenomination > 0,
                MaybeDenomination
        end,
    TXID = hb_util:decode(hb_util:find_value(<<"id">>, TXStruct)),
    32 = byte_size(TXID),
    Owner = hb_util:decode(hb_util:find_value(<<"owner">>, TXStruct)),
    Sig = hb_util:decode(hb_util:find_value(<<"signature">>, TXStruct)),
    SigType = set_sig_type_from_pub_key(Owner),
    ?RSA_KEY_TYPE = SigType,
    TX = #tx{
        format = Format,
        id = TXID,
        last_tx = hb_util:decode(hb_util:find_value(<<"last_tx">>, TXStruct)),
        owner = Owner,
        owner_address = ar_wallet:to_address(Owner, SigType),
        tags = [{hb_util:decode(Name), hb_util:decode(Value)}
                %% Only the elements matching this pattern are included in the list.
                || {[{<<"name">>, Name}, {<<"value">>, Value}]} <- Tags],
        target = hb_util:decode(hb_util:find_value(<<"target">>, TXStruct)),
        quantity = binary_to_integer(hb_util:find_value(<<"quantity">>, TXStruct)),
        data = Data,
        reward = binary_to_integer(hb_util:find_value(<<"reward">>, TXStruct)),
        signature = Sig,
        signature_type = SigType,
        data_size = binary_to_integer(hb_util:find_value(<<"data_size">>, TXStruct)),
        data_root =
            case hb_util:find_value(<<"data_root">>, TXStruct) of
                undefined -> <<>>;
                DR -> hb_util:decode(DR)
            end,
        denomination = Denomination
    },
    true = enforce_valid_signed_tx(TX),
    TX.

set_sig_type_from_pub_key(Owner) ->
    case Owner of
        <<>> ->
            ?ECDSA_KEY_TYPE;
        _ ->
            ?RSA_KEY_TYPE
    end.
tx_to_json_struct(
    #tx{
        id = ID,
        format = Format,
        last_tx = Last,
        owner = Owner,
        tags = Tags,
        target = Target,
        quantity = Quantity,
        data = Data,
        reward = Reward,
        signature = Sig,
        data_size = DataSize,
        data_root = DataRoot,
        denomination = Denomination
    } = TX) ->
    true = enforce_valid_signed_tx(TX),
    Fields = [
        {<<"format">>,
            case Format of
                undefined ->
                    1;
                _ ->
                    Format
            end},
        {<<"id">>, hb_util:encode(ID)},
        {<<"last_tx">>, hb_util:encode(Last)},
        {<<"owner">>, hb_util:encode(Owner)},
        {<<"tags">>,
            lists:map(
                fun({Name, Value}) ->
                    {
                        [
                            {<<"name">>, hb_util:encode(Name)},
                            {<<"value">>, hb_util:encode(Value)}
                        ]
                    }
                end,
                Tags
            )
        },
        {<<"target">>, hb_util:encode(Target)},
        {<<"quantity">>, integer_to_binary(Quantity)},
        {<<"data">>, hb_util:encode(Data)},
        {<<"data_size">>, integer_to_binary(DataSize)},
        {<<"data_tree">>, []},
        {<<"data_root">>, hb_util:encode(DataRoot)},
        {<<"reward">>, integer_to_binary(Reward)},
        {<<"signature">>, hb_util:encode(Sig)}
    ],
    Fields2 =
        case Denomination > 0 of
            true ->
                Fields ++ [{<<"denomination">>, integer_to_binary(Denomination)}];
            false ->
                Fields
        end,
    hb_maps:from_list(Fields2).

%% @doc Split the tx data into chunks and compute the Merkle tree from them.
%% Used to compute the Merkle roots of v1 transactions' data and to compute
%% Merkle proofs for v2 transactions when their data is uploaded without proofs.
generate_chunk_tree(TX) ->
    generate_chunk_tree(TX,
        sized_chunks_to_sized_chunk_ids(
            chunks_to_size_tagged_chunks(
                chunk_binary(?DATA_CHUNK_SIZE, TX#tx.data)
            )
        )
    ).

generate_chunk_tree(TX, ChunkIDSizes) ->
    {Root, Tree} = ar_merkle:generate_tree(ChunkIDSizes),
    TX#tx{ data_tree = Tree, data_root = Root }.

%% @doc Generate a chunk ID used to construct the Merkle tree from the tx data chunks.
generate_chunk_id(Chunk) ->
    crypto:hash(sha256, Chunk).

%% @doc Split the binary into chunks. Used for computing the Merkle roots of
%% v1 transactions' data and computing Merkle proofs for v2 transactions' when
%% their data is uploaded without proofs.
chunk_binary(ChunkSize, Bin) when byte_size(Bin) < ChunkSize ->
    [Bin];
chunk_binary(ChunkSize, Bin) ->
    <<ChunkBin:ChunkSize/binary, Rest/binary>> = Bin,
    [ChunkBin | chunk_binary(ChunkSize, Rest)].

%% @doc Assign a byte offset to every chunk in the list.
chunks_to_size_tagged_chunks(Chunks) ->
    lists:reverse(
        element(
            2,
            lists:foldl(
                fun(Chunk, {Pos, List}) ->
                    End = Pos + byte_size(Chunk),
                    {End, [{Chunk, End} | List]}
                end,
                {0, []},
                Chunks
            )
        )
    ).

%% @doc Convert a list of chunk, byte offset tuples to
%% the list of chunk ID, byte offset tuples.
sized_chunks_to_sized_chunk_ids(SizedChunks) ->
    [{generate_chunk_id(Chunk), Size} || {Chunk, Size} <- SizedChunks].

%% @doc Verifies that the given transaction is a minimally valid signed or
%% unsigned transaction.
%% 
%% In particular:
%% 1. Values are of the correct type and size.
%% 2. In some cases where a limited number of values are allowed for a field, those are
%%    checked as well (e.g. format is 1 or 2)
%% 3. Unsupported fields are set to their default values.
%% 
%% When support is added for new fields (e.g. when we add support for ECDSA signatures),
%% this function will have to be updated.
enforce_valid_tx(TX) ->
    hb_util:ok_or_throw(TX,
        hb_util:check_type(TX, tx),
        {invalid_tx, TX}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_value(TX#tx.format, [1, 2]),
        {invalid_field, format, TX#tx.format}
    ),
    % id will be set when the transaction is signed and can be empty before then
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.id, [0, 32]),
        {invalid_field, id, TX#tx.id}
    ),
    % Arweave L1 #tx doesn't support unsigned_id
    hb_util:ok_or_throw(TX,
        hb_util:check_value(TX#tx.unsigned_id, [?DEFAULT_ID]),
        {invalid_field, unsigned_id, TX#tx.unsigned_id}
    ),
    % last_tx can't be empty
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.last_tx, [32]),
        {invalid_field, last_tx, TX#tx.last_tx}
    ),
    % owner can't be empty
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.owner, [byte_size(?DEFAULT_OWNER)]),
        {invalid_field, owner, TX#tx.owner}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.target, [0, 32]),
        {invalid_field, target, TX#tx.target}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_type(TX#tx.quantity, integer),
        {invalid_field, quantity, TX#tx.quantity}
    ),
    hb_util:ok_or_throw(
        TX,
        hb_util:check_type(TX#tx.data, binary),
        {invalid_field, data, TX#tx.data}
    ),
    % Arweave L1 #tx doesn't support manifest
    hb_util:ok_or_throw(TX,
        hb_util:check_value(TX#tx.manifest, [undefined]),
        {invalid_field, manifest, TX#tx.manifest}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_type(TX#tx.data_size, integer),
        {invalid_field, data_size, TX#tx.data_size}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.data_root, [0, 32]),
        {invalid_field, data_root, TX#tx.data_root}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.signature, [0, 65, byte_size(?DEFAULT_SIG)]),
        {invalid_field, signature, TX#tx.signature}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_type(TX#tx.reward, integer),
        {invalid_field, reward, TX#tx.reward}
    ),
    % Arweave L1 #tx doesn't support denomination changes yet.
    % Refresh from arweave source to add support.
    hb_util:ok_or_throw(TX,
        hb_util:check_value(TX#tx.denomination, [0]),
        {invalid_field, denomination, TX#tx.denomination}
    ),
    % Arweave L1 #tx only supports RSA signatures for now
    hb_util:ok_or_throw(TX,
        hb_util:check_value(TX#tx.signature_type, [?RSA_KEY_TYPE]),
        {invalid_field, signature_type, TX#tx.signature_type}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_type(TX#tx.tags, list),
        {invalid_field, tags, TX#tx.tags}
    ),
    lists:foreach(
        fun({Name, Value}) ->
            hb_util:ok_or_throw(TX,
                hb_util:check_type(Name, binary),
                {invalid_field, tag_name, Name}
            ),
            hb_util:ok_or_throw(TX,
                hb_util:check_size(Name, {range, 0, ?MAX_TAG_NAME_SIZE}),
                {invalid_field, tag_name, Name}
            ),
            hb_util:ok_or_throw(TX,
                hb_util:check_type(Value, binary),
                {invalid_field, tag_value, Value}
            ),
            hb_util:ok_or_throw(TX,
                hb_util:check_size(Value, {range, 0, ?MAX_TAG_VALUE_SIZE}),
                {invalid_field, tag_value, Value}
            );
            (InvalidTagForm) ->
                throw({invalid_field, tag, InvalidTagForm})
        end,
        TX#tx.tags
    ),
    true.

%% @doc Like enforce_valid_tx/1, but restricts also verifies that the transaction has not
%% been signed.
enforce_valid_unsigned_tx(TX) ->
    true = enforce_valid_tx(TX),
    hb_util:ok_or_throw(TX,
        hb_util:check_value(TX#tx.id, [?DEFAULT_ID]),
        {invalid_field, id, TX#tx.id}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_value(TX#tx.signature, [?DEFAULT_SIG]),
        {invalid_field, signature, TX#tx.signature}
    ),
    true.

%% @doc Like enforce_valid_tx/1, but also verifies that the transaction has been signed.
%% 
%% Note: the validity of the signature is *not* checked.
enforce_valid_signed_tx(TX) ->
    true = enforce_valid_tx(TX),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.id, [32]),
        {invalid_field, id, TX#tx.id}
    ),
    hb_util:ok_or_throw(TX,
        not hb_util:check_value(TX#tx.id, [?DEFAULT_ID]),
        {invalid_field, id, TX#tx.id}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.signature, [65, byte_size(?DEFAULT_SIG)]),
        {invalid_field, signature, TX#tx.signature}
    ),
    hb_util:ok_or_throw(TX,
        not hb_util:check_value(TX#tx.signature, [?DEFAULT_SIG]),
        {invalid_field, signature, TX#tx.signature}
    ),
    true.

%%%===================================================================
%%% Tests.
%%%===================================================================

%% @doc A helper for preparing transactions for signing. Used in tests.
%% Should be moved to a testing module.
new() ->
    new(<<>>).
new(Data) ->
    TX = #tx{
        format = 1,
        last_tx = crypto:strong_rand_bytes(32),
        owner = crypto:strong_rand_bytes(512),
        data = Data,
        data_size = byte_size(Data)
    },
    true = enforce_valid_unsigned_tx(TX),
    TX.

%%===================================================================
%% signing tests
%%===================================================================

signing_test_() ->
    [
        {timeout, 10, fun test_sign_tx/0},
        {timeout, 10, fun test_sign_and_verify_chunked/0},
        {timeout, 10, fun test_forge/0}
    ].

sign_tx_v1(TX, {Priv, Pub}) ->
    sign_v1(generate_chunk_tree(TX), Priv, Pub).

sign_tx_v2(TX, {Priv, Pub}) ->
    sign(generate_chunk_tree(TX), Priv, Pub).

test_sign_tx() ->
    NewTX = (new(<<"TEST DATA">>))#tx{ reward = ?AR(1) },
    {Priv, Pub} = Wallet = ar_wallet:new(),

    ValidTXs = [
        sign_v1(NewTX, Priv, Pub),
        sign(generate_chunk_tree(NewTX#tx{ format = 2 }), Priv, Pub)
    ],
    lists:foreach(
        fun(TX) ->
            ?assert(verify(TX), TX#tx.format)
        end,
        ValidTXs
    ),
    InvalidTXs = [
        { tx_format_not_supported,
            (sign_tx_v1(NewTX, Wallet))#tx{ format = 3 } },
        { tx_format_not_supported,
            (sign_tx_v2(NewTX#tx{ format = 2 }, Wallet))#tx{ format = 3 } },
        { quantity_negative,
            sign_tx_v1(NewTX#tx{ quantity = -1 }, Wallet) },
        { quantity_negative,
            sign_tx_v2(NewTX#tx{ quantity = -1, format = 2 }, Wallet) },
        { same_owner_as_target,
            sign_tx_v1(NewTX#tx{ target = ar_wallet:to_address(Wallet) }, Wallet) },
        { same_owner_as_target,
            sign_tx_v2(NewTX#tx{ target = ar_wallet:to_address(Wallet), format = 2 }, Wallet) },
        { tx_id_not_valid,
            (sign_tx_v1(NewTX, Wallet))#tx{ id = crypto:strong_rand_bytes(32) } },
        { tx_id_not_valid,
            (sign_tx_v2(NewTX#tx{ format = 2 }, Wallet))#tx{ id = crypto:strong_rand_bytes(32) } },
        { tx_signature_not_valid,
            (sign_tx_v1(NewTX, Wallet))#tx{ signature = crypto:strong_rand_bytes(512) } },
        { tx_signature_not_valid,
            (sign_tx_v2(NewTX#tx{ format = 2 }, Wallet))#tx{ signature = crypto:strong_rand_bytes(512) } },
        { tx_data_size_negative,
            sign_tx_v2(NewTX#tx{ data_size = -1, format = 2 }, Wallet) },
        { tx_data_size_data_root_mismatch, 
            sign((generate_chunk_tree(NewTX#tx{ format = 2 }))#tx{ data_root = <<>> }, Priv, Pub) }
    ],
    lists:foreach(
        fun({Message, TX}) ->
            ?assert(not verify(TX), 
                lists:flatten(
                    io_lib:format("Format ~p: ~s", [TX#tx.format, Message])
                )
            )
        end,
        InvalidTXs
    ).

test_sign_and_verify_chunked() ->
    TXData = crypto:strong_rand_bytes(trunc(?DATA_CHUNK_SIZE * 5.5)),
    {Priv, Pub} = ar_wallet:new(),
    UnsignedTX =
        generate_chunk_tree(
            (new())#tx{
                format = 2,
                data = TXData,
                data_size = byte_size(TXData),
                reward = ?AR(100)
            }
        ),
    SignedTX = sign(UnsignedTX#tx{ data = <<>> }, Priv, Pub),
    ?assert(verify(SignedTX)).

%% Ensure that a forged transaction does not pass verification.
test_forge() ->
    NewTX = (new(<<"TEST DATA">>))#tx{ reward = ?AR(10) },
    {Priv, Pub} = ar_wallet:new(),
    InvalidSignTX = (sign_v1(NewTX, Priv, Pub))#tx{
        data = <<"FAKE DATA">>
    },
    ?assert(not verify(InvalidSignTX)).

%%===================================================================
%% chunk tree tests
%%===================================================================

generate_and_validate_chunk_tree_test_() ->
    [
        fun test_generate_and_validate_even_chunk_tree/0,
        fun test_generate_and_validate_uneven_chunk_tree/0
    ].

test_generate_and_validate_even_chunk_tree() ->
    Data = crypto:strong_rand_bytes(?DATA_CHUNK_SIZE * 7),
    lists:map(
        fun(ChallengeLocation) ->
            generate_chunk_tree_and_validate_path(Data, ChallengeLocation)
        end,
        [
            0, 1, 10, ?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE + 1, 2 * ?DATA_CHUNK_SIZE - 1,
            7 * ?DATA_CHUNK_SIZE - 1
        ]
    ).

test_generate_and_validate_uneven_chunk_tree() ->
    Data = crypto:strong_rand_bytes(?DATA_CHUNK_SIZE * 4 + 10),
    lists:map(
        fun(ChallengeLocation) ->
            generate_chunk_tree_and_validate_path(Data, ChallengeLocation)
        end,
        [
            0, 1, 10, ?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE + 1, 2 * ?DATA_CHUNK_SIZE - 1,
            4 * ?DATA_CHUNK_SIZE + 9
        ]
    ).

generate_chunk_tree_and_validate_path(Data, ChallengeLocation) ->
    ChunkStart = hb_util:floor_int(ChallengeLocation, ?DATA_CHUNK_SIZE),
    Chunk = binary:part(Data, ChunkStart, min(?DATA_CHUNK_SIZE, byte_size(Data) - ChunkStart)),
    #tx{ data_root = DataRoot, data_tree = DataTree } =
        generate_chunk_tree(
            #tx{
                data = Data,
                data_size = byte_size(Data)
            }
        ),
    DataPath =
        ar_merkle:generate_path(
            DataRoot,
            ChallengeLocation,
            DataTree
        ),
    RealChunkID = generate_chunk_id(Chunk),
    {PathChunkID, StartOffset, EndOffset} =
        ar_merkle:validate_path(DataRoot, ChallengeLocation, byte_size(Data), DataPath),
    {PathChunkID, StartOffset, EndOffset} =
        ar_merkle:validate_path(DataRoot, ChallengeLocation, byte_size(Data), DataPath, strict_data_split_ruleset),
    {PathChunkID, StartOffset, EndOffset} =
        ar_merkle:validate_path(DataRoot, ChallengeLocation, byte_size(Data),
                DataPath, strict_borders_ruleset),
    ?assertEqual(RealChunkID, PathChunkID),
    ?assert(ChallengeLocation >= StartOffset),
    ?assert(ChallengeLocation < EndOffset).

%%===================================================================
%% json_struct_to_tx tests
%%===================================================================

json_struct_to_tx_test_() ->
    [
        fun test_json_struct_to_tx_happy/0,
        fun test_json_struct_to_tx_failure/0
    ].

test_json_struct_to_tx_happy() ->
    ID          = crypto:strong_rand_bytes(32),
    Owner       = crypto:strong_rand_bytes(512),
    LastTX      = crypto:strong_rand_bytes(32),
    DataRoot    = crypto:strong_rand_bytes(32),
    BaseStruct = #{
        <<"format">>      => 1,
        <<"id">>          => hb_util:encode(ID),
        <<"last_tx">>     => hb_util:encode(LastTX),
        <<"owner">>       => hb_util:encode(Owner),
        <<"signature">>   => hb_util:encode(crypto:strong_rand_bytes(512)),
        <<"quantity">>    => <<"100">>,
        <<"reward">>      => <<"10">>,
        <<"data_size">>   => <<"0">>,
        <<"data">>        => hb_util:encode(<<>>),
        <<"target">>      => <<>>,
        <<"tags">>        => [],
        <<"data_root">>   => hb_util:encode(DataRoot)
    },

    TargetBin = crypto:strong_rand_bytes(32),
    TargetB64 = hb_util:encode(TargetBin),

    Variants = [
        %% baseline – ensure empty tag list maintained
        {BaseStruct, fun(TX) ->
            ?assertEqual(1, TX#tx.format),
            ?assertEqual([], TX#tx.tags)
        end},
        {BaseStruct#{ <<"format">> => 2 }, fun(TX) -> ?assertEqual(2, TX#tx.format) end},
        {BaseStruct#{ <<"format">> => <<"2">> }, fun(TX) -> ?assertEqual(2, TX#tx.format) end},
        {BaseStruct#{ <<"quantity">> => <<"1234">> }, fun(TX) -> ?assertEqual(1234, TX#tx.quantity) end},
        {BaseStruct#{ <<"reward">> => <<"567">> }, fun(TX) -> ?assertEqual(567, TX#tx.reward) end},
        {BaseStruct#{ <<"tags">> => [{[
                {<<"name">>, hb_util:encode(<<"tagname">>)},
                {<<"value">>, hb_util:encode(<<"tagval">>)}
            ]}] },
            fun(TX) -> ?assertEqual([{<<"tagname">>, <<"tagval">>}], TX#tx.tags) end},
        {BaseStruct#{ <<"target">> => TargetB64 },
            fun(TX) -> ?assertEqual(TargetBin, TX#tx.target)end},
        {BaseStruct#{ <<"data_root">> => hb_util:encode(DataRoot) },
            fun(TX) -> ?assertEqual(DataRoot, TX#tx.data_root) end},
        {BaseStruct#{ <<"data_size">> => <<"250">> },
            fun(TX) -> ?assertEqual(250, TX#tx.data_size) end}
    ],

    lists:foreach(
        fun({Struct, AssertFun}) ->
            Parsed = json_struct_to_tx(Struct),
            RoundTripStruct = tx_to_json_struct(Parsed),
            ?event({round_trip, {struct, Struct}, {round_trip_struct, RoundTripStruct}}),
            ?assertEqual(
                maps:without([<<"data_tree">>, <<"format">>], Struct),
                maps:without([<<"data_tree">>, <<"format">>], RoundTripStruct)
            ),
            %% constant fields always true across variants
            ?assert(is_record(Parsed, tx)),
            ?assertEqual(ID, Parsed#tx.id),
            ?assertEqual(Owner, Parsed#tx.owner),
            ?assertEqual(ar_wallet:to_address(Owner), Parsed#tx.owner_address),
            ?assertEqual(LastTX, Parsed#tx.last_tx),
            ?assertEqual(?RSA_KEY_TYPE, Parsed#tx.signature_type),
            %% run variant-specific assertion
            AssertFun(Parsed)
        end,
        Variants
    ).

test_json_struct_to_tx_failure() ->
    BaseStruct = #{
        <<"id">>         => hb_util:encode(crypto:strong_rand_bytes(32)),
        <<"last_tx">>    => hb_util:encode(crypto:strong_rand_bytes(32)),
        <<"owner">>      => hb_util:encode(crypto:strong_rand_bytes(512)),
        <<"signature">>  => hb_util:encode(<<>>),
        <<"quantity">>   => <<"0">>,
        <<"reward">>     => <<"0">>,
        <<"data_size">>  => <<"0">>,
        <<"data">>       => hb_util:encode(<<>>),
        <<"target">>     => <<>>,
        <<"tags">>       => [],
        <<"data_root">>  => hb_util:encode(<<>>)
    },

    BadIDBin  = crypto:strong_rand_bytes(31),
    InvalidB64 = <<"!!not_base64!!">>,

    %% invalid tag variants
    BadTagName   = [ {[{<<"name">>, InvalidB64}, {<<"value">>, hb_util:encode(<<"val">>)}]} ],
    BadTagValue  = [ {[{<<"name">>, hb_util:encode(<<"key">>)}, {<<"value">>, InvalidB64}]} ],

    FailureCases = [
        {invalid_format, BaseStruct#{ <<"format">> => <<"abc">> }, badarg},
        {denomination_zero, BaseStruct#{ <<"denomination">> => <<"0">> }, {badmatch, false}},
        {denomination_one, BaseStruct#{ <<"denomination">> => <<"1">> }, {invalid_field,denomination,1}},
        {id_wrong_size, BaseStruct#{ <<"id">> => hb_util:encode(BadIDBin) }, {badmatch, 31}},
        {quantity_nonnumeric, BaseStruct#{ <<"quantity">> => <<"notanumber">> }, badarg},
        {reward_nonnumeric, BaseStruct#{ <<"reward">> => <<"xyz">> }, badarg},
        {data_size_nonnumeric, BaseStruct#{ <<"data_size">> => <<"abc">> }, badarg},
        {quantity_missing, maps:remove(<<"quantity">>, BaseStruct), badarg},
        {id_invalid_b64, BaseStruct#{ <<"id">> => InvalidB64 }, badarg},
        {owner_invalid_b64, BaseStruct#{ <<"owner">> => InvalidB64 }, badarg},
        {last_tx_invalid_b64, BaseStruct#{ <<"last_tx">> => InvalidB64 }, badarg},
        {signature_invalid_b64, BaseStruct#{ <<"signature">> => InvalidB64 }, badarg},
        {data_invalid_b64, BaseStruct#{ <<"data">> => InvalidB64 }, badarg},
        {data_root_invalid_b64, BaseStruct#{ <<"data_root">> => InvalidB64 }, badarg},
        {tag_name_invalid_b64, BaseStruct#{ <<"tags">> => BadTagName }, badarg},
        {tag_value_invalid_b64, BaseStruct#{ <<"tags">> => BadTagValue }, badarg},
        {target_invalid_b64, BaseStruct#{ <<"target">> => InvalidB64 }, badarg},
        {invalid_signature_type, BaseStruct#{ <<"owner">> => <<>> }, {badmatch, {ecdsa,secp256k1}}}
        ],

    lists:foreach(
        fun({Label, Struct, Reason}) ->
            hb_test_utils:assert_throws(
                fun json_struct_to_tx/1,
                [Struct],
                Reason,
                Label
            )
        end,
        FailureCases
    ).

%%===================================================================
%% tx_to_json_struct tests
%%===================================================================

tx_to_json_struct_test_() ->
    [
        fun test_tx_to_json_struct_happy/0,
        fun test_tx_to_json_struct_failure/0
    ].

test_tx_to_json_struct_happy() ->
    Owner = crypto:strong_rand_bytes(512),

    BaseTX = (new())#tx{
        id             = crypto:strong_rand_bytes(32),
        last_tx        = crypto:strong_rand_bytes(32),
        owner          = Owner,
        signature_type = ?RSA_KEY_TYPE, 
        owner_address  = ar_wallet:to_address(Owner, ?RSA_KEY_TYPE), %% Not in JSON
        tags           = [],
        target         = <<>>,
        quantity       = 0,
        data           = <<>>,
        data_size      = 0,
        data_root      = <<>>,
        reward         = 0,
        signature      = crypto:strong_rand_bytes(512), %% RSA signature size
        format         = 1,
        denomination   = 0
    },

    SpecificTarget = crypto:strong_rand_bytes(32),
    SpecificDataRoot = crypto:strong_rand_bytes(32),
    SpecificData = crypto:strong_rand_bytes(64),

    Variants = [
        %% {Label, ModifiedTX, ExpectedCheckerFun}
        {baseline, BaseTX},
        {format_2, BaseTX#tx{format = 2}},
        {with_tags, BaseTX#tx{tags = [{<<"tag1">>, <<"val1">>}, {<<"TagTwo">>, <<"ValueTwo">>}]}},
        {with_target, BaseTX#tx{target = SpecificTarget}},
        {with_data_root, BaseTX#tx{data_root = SpecificDataRoot}},
        {with_data, BaseTX#tx{data = SpecificData, data_size = byte_size(SpecificData)}},
        {with_quantity_and_reward, BaseTX#tx{quantity = 12345, reward = 6789}}
    ],

    lists:foreach(
        fun({Label, TXVariant}) ->
            JsonStruct = tx_to_json_struct(TXVariant),
            RoundtripTX = json_struct_to_tx(JsonStruct),
            ?assertEqual(TXVariant, RoundtripTX),
            ?assert(is_map(JsonStruct), Label),
            %% Assert common fields
            ExpectedFormat = case TXVariant#tx.format of
                undefined -> 1;
                _ -> TXVariant#tx.format
            end,
            ?assertEqual(ExpectedFormat, maps:get(<<"format">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.id), maps:get(<<"id">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.last_tx), maps:get(<<"last_tx">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.owner), maps:get(<<"owner">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.target), maps:get(<<"target">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.signature), maps:get(<<"signature">>, JsonStruct), Label),
            ?assertEqual(integer_to_binary(TXVariant#tx.quantity), maps:get(<<"quantity">>, JsonStruct), Label),
            ?assertEqual(integer_to_binary(TXVariant#tx.reward), maps:get(<<"reward">>, JsonStruct), Label),
            ?assertEqual(integer_to_binary(TXVariant#tx.data_size), maps:get(<<"data_size">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.data), maps:get(<<"data">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.data_root), maps:get(<<"data_root">>, JsonStruct), Label),
            ?assertEqual(integer_to_binary(TXVariant#tx.denomination),
                maps:get(<<"denomination">>, JsonStruct, integer_to_binary(0)), Label),
            ?assertEqual([], maps:get(<<"data_tree">>, JsonStruct), Label), % Always empty list

            ExpectedJsonTags = lists:map(
                fun({Name, Value}) -> 
                    {[{<<"name">>, hb_util:encode(Name)}, {<<"value">>, hb_util:encode(Value)}]}
                end,
                TXVariant#tx.tags
            ),
            ?assertEqual(ExpectedJsonTags, maps:get(<<"tags">>, JsonStruct), Label),

            %% Assert no extra keys are present
            BaseExpectedKeys = [
                <<"format">>, <<"id">>, <<"last_tx">>, <<"owner">>, <<"tags">>, <<"target">>,
                <<"quantity">>, <<"data">>, <<"data_size">>, <<"data_tree">>,
                <<"data_root">>, <<"reward">>, <<"signature">>
            ],
            ExpectedKeys =
                case TXVariant#tx.denomination > 0 of
                    true  -> lists:sort([<<"denomination">> | BaseExpectedKeys]);
                    false -> lists:sort(BaseExpectedKeys)
                end,
            ActualKeys = lists:sort(maps:keys(JsonStruct)),
            ?assertEqual(ExpectedKeys, ActualKeys,
                {extra_or_missing_keys, Label, ExpectedKeys, ActualKeys})
        end,
        Variants
    ).

test_tx_to_json_struct_failure() ->
    % Just run a single test to confirm that enforce_valid_tx/1 is called. All other
    % validations are performed by enforce_valid_tx/1 and tested in the
    % enforce_valid_tx_test_() group.
    TX = (new())#tx{ id = not_a_binary },
    Reason = {invalid_field, id, not_a_binary},

    hb_test_utils:assert_throws(
        fun tx_to_json_struct/1,
        [TX],
        Reason,
        "tx_to_json_struct/1 failed to enforce_valid_tx"
    ).


%%===================================================================
%% enforce_valid_tx tests
%%===================================================================

enforce_valid_tx_test_() ->
    [
        fun test_enforce_valid_tx_happy/0,
        fun test_enforce_valid_tx_failure/0,
        fun test_enforce_valid_unsigned_tx/0,
        fun test_enforce_valid_signed_tx/0
    ].

test_enforce_valid_tx_happy() ->
    BaseTX = new(),
    ?assert(enforce_valid_tx(BaseTX), "Base valid TX"),

    ValidTXs = [
        {format_2, BaseTX#tx{format = 2}},
        {id_32_bytes, BaseTX#tx{id = crypto:strong_rand_bytes(32)}},
        {sig_65_bytes, BaseTX#tx{signature = crypto:strong_rand_bytes(65)}},
        {sig_default_size, BaseTX#tx{signature = crypto:strong_rand_bytes(byte_size(?DEFAULT_SIG))}},
        {target_32_bytes, BaseTX#tx{target = crypto:strong_rand_bytes(32)}},
        {data_root_32_bytes, BaseTX#tx{data_root = crypto:strong_rand_bytes(32)}},
        {simple_tag, BaseTX#tx{tags = [{<<"name">>, <<"value">>}]}},
        {empty_tag_name_value, BaseTX#tx{tags = [{<<>>, <<>>}]}},
        {max_len_tag_name, BaseTX#tx{tags = [{crypto:strong_rand_bytes(?MAX_TAG_NAME_SIZE), <<"val">>}]}},
        {max_len_tag_value, BaseTX#tx{tags = [{<<"key">>, crypto:strong_rand_bytes(?MAX_TAG_VALUE_SIZE)}]}}
    ],

    lists:foreach(
        fun({Label, TX}) ->
            ?assert(enforce_valid_tx(TX), Label)
        end,
        ValidTXs
    ).

test_enforce_valid_tx_failure() ->
    BaseTX = new(),

    InvalidUnsignedID = list_to_binary(lists:duplicate(32, $a)), % Guaranteed not ?DEFAULT_ID
    BadID31 = crypto:strong_rand_bytes(31),
    BadID33 = crypto:strong_rand_bytes(33),
    BadOwnerSize = crypto:strong_rand_bytes(byte_size(?DEFAULT_OWNER) - 1),
    TooLongTagName = crypto:strong_rand_bytes(?MAX_TAG_NAME_SIZE + 1),
    TooLongTagValue = crypto:strong_rand_bytes(?MAX_TAG_VALUE_SIZE + 1),

    SigInvalidSize1 = crypto:strong_rand_bytes(1),
    SigInvalidSize64 = crypto:strong_rand_bytes(64),
    SigInvalidSize66 = crypto:strong_rand_bytes(66),
    SigInvalidSize511 = crypto:strong_rand_bytes(511),
    SigTooLong513 = crypto:strong_rand_bytes(byte_size(?DEFAULT_SIG)+1),

    FailureCases = [
        {not_a_tx_record, not_a_tx_record_atom, {invalid_tx, not_a_tx_record_atom}},
        {invalid_format_0, BaseTX#tx{format = 0}, {invalid_field, format, 0}},
        {invalid_format_3, BaseTX#tx{format = 3}, {invalid_field, format, 3}},
        {invalid_format_atom, BaseTX#tx{format = an_atom}, {invalid_field, format, an_atom}},
        {id_too_short_31, BaseTX#tx{id = BadID31}, {invalid_field, id, BadID31}},
        {id_too_long_33, BaseTX#tx{id = BadID33}, {invalid_field, id, BadID33}},
        {unsigned_id_invalid_val, BaseTX#tx{unsigned_id = InvalidUnsignedID}, {invalid_field, unsigned_id, InvalidUnsignedID}},
        {last_tx_empty, BaseTX#tx{last_tx = <<>>}, {invalid_field, last_tx, <<>>}},
        {last_tx_too_short_31, BaseTX#tx{last_tx = BadID31}, {invalid_field, last_tx, BadID31}},
        {last_tx_too_long_33, BaseTX#tx{last_tx = BadID33}, {invalid_field, last_tx, BadID33}},
        {owner_wrong_size, BaseTX#tx{owner = BadOwnerSize}, {invalid_field, owner, BadOwnerSize}},
        {target_too_short_31, BaseTX#tx{target = BadID31}, {invalid_field, target, BadID31}},
        {target_too_long_33, BaseTX#tx{target = BadID33}, {invalid_field, target, BadID33}},
        {quantity_not_integer, BaseTX#tx{quantity = <<"100">>}, {invalid_field, quantity, <<"100">>}},
        {data_not_binary, BaseTX#tx{data = an_atom}, {invalid_field, data, an_atom}},
        {manifest_not_undefined, BaseTX#tx{manifest = <<>>}, {invalid_field, manifest, <<>>}},
        {data_size_not_integer, BaseTX#tx{data_size = an_atom}, {invalid_field, data_size, an_atom}},
        {data_root_too_short_31, BaseTX#tx{data_root = BadID31}, {invalid_field, data_root, BadID31}},
        {data_root_too_long_33, BaseTX#tx{data_root = BadID33}, {invalid_field, data_root, BadID33}},
        {signature_invalid_size_1, BaseTX#tx{signature = SigInvalidSize1}, {invalid_field, signature, SigInvalidSize1}},
        {signature_invalid_size_64, BaseTX#tx{signature = SigInvalidSize64}, {invalid_field, signature, SigInvalidSize64}},
        {signature_invalid_size_66, BaseTX#tx{signature = SigInvalidSize66}, {invalid_field, signature, SigInvalidSize66}},
        {signature_invalid_size_511, BaseTX#tx{signature = SigInvalidSize511}, {invalid_field, signature, SigInvalidSize511}},
        {signature_too_long_513, BaseTX#tx{signature = SigTooLong513}, {invalid_field, signature, SigTooLong513}},
        {reward_not_integer, BaseTX#tx{reward = 1.0}, {invalid_field, reward, 1.0}},
        {denomination_not_zero, BaseTX#tx{denomination = 1}, {invalid_field, denomination, 1}},
        {signature_type_not_rsa, BaseTX#tx{signature_type = ?ECDSA_KEY_TYPE}, {invalid_field, signature_type, ?ECDSA_KEY_TYPE}},
        {tags_not_list, BaseTX#tx{tags = #{}}, {invalid_field, tags, #{}}},
        {tag_name_not_binary, BaseTX#tx{tags = [{not_binary, <<"val">>}]}, {invalid_field, tag_name, not_binary}},
        {tag_name_too_long, BaseTX#tx{tags = [{TooLongTagName, <<"val">>}]}, {invalid_field, tag_name, TooLongTagName}},
        {tag_value_not_binary, BaseTX#tx{tags = [{<<"key">>, not_binary}]}, {invalid_field, tag_value, not_binary}},
        {tag_value_too_long, BaseTX#tx{tags = [{<<"key">>, TooLongTagValue}]}, {invalid_field, tag_value, TooLongTagValue}},
        {invalid_tag_form_atom, BaseTX#tx{tags = [not_a_tuple]}, {invalid_field, tag, not_a_tuple}},
        {invalid_tag_form_list, BaseTX#tx{tags = [[<<"name">>, <<"value">>]]}, {invalid_field, tag, [<<"name">>, <<"value">>]} }
    ],

    lists:foreach(
        fun({Label, BadTX, ExpectedThrow}) ->
            ?assertThrow(ExpectedThrow, enforce_valid_tx(BadTX), Label)
        end,
        FailureCases
    ).

test_enforce_valid_unsigned_tx() ->
    BaseTX = (new())#tx{ id = ?DEFAULT_ID, signature = ?DEFAULT_SIG },
    ?assert(enforce_valid_unsigned_tx(BaseTX), "Base valid unsigned TX"),

    ID = crypto:strong_rand_bytes(32),
    Signature = crypto:strong_rand_bytes(512),

    FailureCases = [
        {id_is_set, BaseTX#tx{id = ID}, {invalid_field, id, ID}},
        {signature_is_set, BaseTX#tx{signature = Signature}, {invalid_field, signature, Signature}}
    ],

    lists:foreach(
        fun({Label, BadTX, ExpectedThrow}) ->
            ?assertThrow(ExpectedThrow, enforce_valid_unsigned_tx(BadTX), Label)
        end,
        FailureCases
    ).

test_enforce_valid_signed_tx() ->
    BaseTX = (new())#tx{ 
        id = crypto:strong_rand_bytes(32),
        signature = crypto:strong_rand_bytes(512) },
    ?assert(enforce_valid_signed_tx(BaseTX), "Base valid signed TX"),

    FailureCases = [
        {id_is_default, BaseTX#tx{id = ?DEFAULT_ID}, {invalid_field, id, ?DEFAULT_ID}},
        {id_not_set, BaseTX#tx{id = <<>>}, {invalid_field, id, <<>>}},
        {signature_is_default, BaseTX#tx{signature = ?DEFAULT_SIG}, {invalid_field, signature, ?DEFAULT_SIG}},
        {signature_not_set, BaseTX#tx{signature = <<>>}, {invalid_field, signature, <<>>}}
    ],

    lists:foreach(
        fun({Label, BadTX, ExpectedThrow}) ->
            ?assertThrow(ExpectedThrow, enforce_valid_signed_tx(BadTX), Label)
        end,
        FailureCases
    ).