%%% @doc The module with utilities for transaction creation, signing, and verification.
-module(ar_tx).

-export([sign/2, verify/1, verify_tx_id/2]).
-export([json_struct_to_tx/1, tx_to_json_struct/1]).

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
	case TX#tx.denomination > 0 of
		true ->
			ar_deep_hash:hash([
				<< (integer_to_binary(TX#tx.denomination))/binary >>,
				<< (TX#tx.owner)/binary >>,
				<< (TX#tx.target)/binary >>,
				<< (list_to_binary(integer_to_list(TX#tx.quantity)))/binary >>,
				<< (list_to_binary(integer_to_list(TX#tx.reward)))/binary >>,
				<< (TX#tx.last_tx)/binary >>,
				tags_to_list(TX#tx.tags)
			]);
		false ->
			<<
				(TX#tx.owner)/binary,
				(TX#tx.target)/binary,
				(TX#tx.data)/binary,
				(list_to_binary(integer_to_list(TX#tx.quantity)))/binary,
				(list_to_binary(integer_to_list(TX#tx.reward)))/binary,
				(TX#tx.last_tx)/binary,
				(tags_to_binary(TX#tx.tags))/binary
			>>
	end.

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
    #tx{
        format = Format,
        id = TXID,
        last_tx = hb_util:decode(hb_util:find_value(<<"last_tx">>, TXStruct)),
        owner = hb_util:decode(hb_util:find_value(<<"owner">>, TXStruct)),
        tags = [{hb_util:decode(Name), hb_util:decode(Value)}
                %% Only the elements matching this pattern are included in the list.
                || {[{<<"name">>, Name}, {<<"value">>, Value}]} <- Tags],
        target = hb_util:find_value(<<"target">>, TXStruct),
        quantity = binary_to_integer(hb_util:find_value(<<"quantity">>, TXStruct)),
        data = Data,
        reward = binary_to_integer(hb_util:find_value(<<"reward">>, TXStruct)),
        signature = hb_util:decode(hb_util:find_value(<<"signature">>, TXStruct)),
        data_size = binary_to_integer(hb_util:find_value(<<"data_size">>, TXStruct)),
        data_root =
            case hb_util:find_value(<<"data_root">>, TXStruct) of
                undefined -> <<>>;
                DR -> hb_util:decode(DR)
            end,
        denomination = Denomination
    }.

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
    }) ->
    Fields = [
        {format,
            case Format of
                undefined ->
                    1;
                _ ->
                    Format
            end},
        {id, hb_util:encode(ID)},
        {last_tx, hb_util:encode(Last)},
        {owner, hb_util:encode(Owner)},
        {tags,
            lists:map(
                fun({Name, Value}) ->
                    {
                        [
                            {name, hb_util:encode(Name)},
                            {value, hb_util:encode(Value)}
                        ]
                    }
                end,
                Tags
            )
        },
        {target, hb_util:encode(Target)},
        {quantity, integer_to_binary(Quantity)},
        {data, hb_util:encode(Data)},
        {data_size, integer_to_binary(DataSize)},
        {data_tree, []},
        {data_root, hb_util:encode(DataRoot)},
        {reward, integer_to_binary(Reward)},
        {signature, hb_util:encode(Sig)}
    ],
    Fields2 =
        case Denomination > 0 of
            true ->
                Fields ++ [{denomination, integer_to_binary(Denomination)}];
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

%%%===================================================================
%%% Tests.
%%%===================================================================

%% @doc A helper for preparing transactions for signing. Used in tests.
%% Should be moved to a testing module.
new(Data, Reward) ->
	#tx{
		format = 1,
		id = crypto:strong_rand_bytes(32),
		data = Data,
		reward = Reward,
		data_size = byte_size(Data)
	}.

sign_tx_test_() ->
	{timeout, 30, fun test_sign_tx/0}.

sign_tx_v1(TX, {Priv, Pub}) ->
	sign_v1(generate_chunk_tree(TX), Priv, Pub).

sign_tx_v2(TX, {Priv, Pub}) ->
	sign(generate_chunk_tree(TX), Priv, Pub).

test_sign_tx() ->
	NewTX = new(<<"TEST DATA">>, ?AR(1)),
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
		{ "tx_format_not_supported",
			(sign_tx_v1(NewTX, Wallet))#tx{ format = 3 } },
		{ "tx_format_not_supported",
			(sign_tx_v2(NewTX#tx{ format = 2 }, Wallet))#tx{ format = 3 } },
		{ "quantity_negative",
			sign_tx_v1(NewTX#tx{ quantity = -1 }, Wallet) },
		{ "quantity_negative",
			sign_tx_v2(NewTX#tx{ quantity = -1, format = 2 }, Wallet) },
		{ "same_owner_as_target",
			sign_tx_v1(NewTX#tx{ target = ar_wallet:to_address(Wallet) }, Wallet) },
		{ "same_owner_as_target",
			sign_tx_v2(NewTX#tx{ target = ar_wallet:to_address(Wallet), format = 2 }, Wallet) },
		{ "tx_id_not_valid",
			(sign_tx_v1(NewTX, Wallet))#tx{ id = crypto:strong_rand_bytes(32) } },
		{ "tx_id_not_valid",
			(sign_tx_v2(NewTX#tx{ format = 2 }, Wallet))#tx{ id = crypto:strong_rand_bytes(32) } },
		{ "tx_signature_not_valid",
			(sign_tx_v1(NewTX, Wallet))#tx{ signature = crypto:strong_rand_bytes(64) } },
		{ "tx_signature_not_valid",
			(sign_tx_v2(NewTX#tx{ format = 2 }, Wallet))#tx{ signature = crypto:strong_rand_bytes(64) } },
		{ "tx_data_size_negative",
			sign_tx_v2(NewTX#tx{ data_size = -1, format = 2 }, Wallet) },
		{ "tx_data_size_data_root_mismatch", 
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

sign_and_verify_chunked_test_() ->
	{timeout, 60, fun test_sign_and_verify_chunked/0}.

test_sign_and_verify_chunked() ->
	TXData = crypto:strong_rand_bytes(trunc(?DATA_CHUNK_SIZE * 5.5)),
	{Priv, Pub} = ar_wallet:new(),
	UnsignedTX =
		generate_chunk_tree(
			#tx{
				format = 2,
				data = TXData,
				data_size = byte_size(TXData),
				reward = ?AR(100)
			}
		),
	SignedTX = sign(UnsignedTX#tx{ data = <<>> }, Priv, Pub),
	?assert(verify(SignedTX)).

%% Ensure that a forged transaction does not pass verification.

forge_test_() ->
	{timeout, 30, fun test_forge/0}.

test_forge() ->
	NewTX = new(<<"TEST DATA">>, ?AR(10)),
	{Priv, Pub} = ar_wallet:new(),
	InvalidSignTX = (sign_v1(NewTX, Priv, Pub))#tx{
		data = <<"FAKE DATA">>
	},
	?assert(not verify(InvalidSignTX)).

generate_and_validate_even_chunk_tree_test() ->
	Data = crypto:strong_rand_bytes(?DATA_CHUNK_SIZE * 7),
	lists:map(
		fun(ChallengeLocation) ->
			test_generate_chunk_tree_and_validate_path(Data, ChallengeLocation)
		end,
		[
			0, 1, 10, ?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE + 1, 2 * ?DATA_CHUNK_SIZE - 1,
			7 * ?DATA_CHUNK_SIZE - 1
		]
	).

generate_and_validate_uneven_chunk_tree_test() ->
	Data = crypto:strong_rand_bytes(?DATA_CHUNK_SIZE * 4 + 10),
	lists:map(
		fun(ChallengeLocation) ->
			test_generate_chunk_tree_and_validate_path(Data, ChallengeLocation)
		end,
		[
			0, 1, 10, ?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE + 1, 2 * ?DATA_CHUNK_SIZE - 1,
			4 * ?DATA_CHUNK_SIZE + 9
		]
	).

test_generate_chunk_tree_and_validate_path(Data, ChallengeLocation) ->
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
		ar_merkle:validate_path(DataRoot, ChallengeLocation, byte_size(Data),
				DataPath, strict_data_split_ruleset),
	{PathChunkID, StartOffset, EndOffset} =
		ar_merkle:validate_path(DataRoot, ChallengeLocation, byte_size(Data),
				DataPath, strict_borders_ruleset),
	?assertEqual(RealChunkID, PathChunkID),
	?assert(ChallengeLocation >= StartOffset),
	?assert(ChallengeLocation < EndOffset).

