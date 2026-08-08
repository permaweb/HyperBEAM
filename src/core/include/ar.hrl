-ifndef(AR_HRL).
-define(AR_HRL, true).

%% Maximum size of a single data chunk, in bytes.
-define(DATA_CHUNK_SIZE, (256 * 1024)).

%% The size of data chunk hashes, in bytes.
-define(CHUNK_ID_HASH_SIZE, 32).

-define(NOTE_SIZE, 32).

-define(DEFAULT_SIG, << 0:4096 >>).
-define(DEFAULT_ID, << 0:256 >>).
-define(DEFAULT_OWNER, << 0:4096 >>).
-define(DEFAULT_DATA, <<>>).
-define(DEFAULT_ANCHOR, <<>>).
-define(DEFAULT_TARGET, <<>>).
-define(DEFAULT_DATA_ROOT, <<>>).
-define(DEFAULT_DATA_SIZE, 0).
-define(DEFAULT_QUANTITY, 0).
-define(DEFAULT_REWARD, 0).

-define(MAX_TAG_COUNT, 128).
-define(MAX_TAG_NAME_SIZE, 1024).
-define(MAX_TAG_VALUE_SIZE, 3072).

%% Winstons per AR.
-define(WINSTON_PER_AR, 1000000000000).

%% A macro to convert AR into Winstons.
-define(AR(AR), (?WINSTON_PER_AR * AR)).

%% @doc A transaction.
-record(tx, {
    %% 1 or 2 or ans104.
    format = ans104,
    %% The transaction identifier.
    id = ?DEFAULT_ID,
    unsigned_id = ?DEFAULT_ID,
    %% Either the identifier of the previous transaction from
    %% the same wallet or the identifier of one of the
    %% last ?MAX_TX_ANCHOR_DEPTH blocks.
    anchor = ?DEFAULT_ANCHOR,
    %% The public key the transaction is signed with.
    owner =	?DEFAULT_OWNER,
    %% The owner address. Used as a cache to avoid recomputing it, not serialized.
    owner_address = not_set,
    %% A list of arbitrary key-value pairs. Keys and values are binaries.
    tags = [],
    %% The address of the recipient, if any. The SHA2-256 hash of the public key.
    target = ?DEFAULT_TARGET,
    %% The amount of Winstons to send to the recipient, if any.
    quantity = ?DEFAULT_QUANTITY,
    %% The data to upload, if any. For v2 transactions, the field is optional - a fee
    %% is charged based on the "data_size" field, data itself may be uploaded any time
    %% later in chunks.
    data = ?DEFAULT_DATA,
    manifest = undefined,
    %% Size in bytes of the transaction data.
    data_size = ?DEFAULT_DATA_SIZE,
    %% Deprecated. Not used, not gossiped.
    data_tree = [],
    %% The Merkle root of the Merkle tree of data chunks.
    data_root = ?DEFAULT_DATA_ROOT,
    %% The signature.
    signature = ?DEFAULT_SIG,
    %% The fee in Winstons.
    reward = ?DEFAULT_REWARD,

    %% The code for the denomination of AR in base units.
    %%
    %% 1 corresponds to the original denomination of 1^12 base units.
    %% Every time the available supply falls below ?REDENOMINATION_THRESHOLD,
    %% the denomination is multiplied by 1000, the code is incremented.
    %%
    %% 0 is the default denomination code. It is treated as the denomination code of the
    %% current block. We do NOT default to 1 because we want to distinguish between the
    %% transactions with the explicitly assigned denomination (the denomination then becomes
    %% a part of the signature preimage) and transactions signed the way they were signed
    %% before the upgrade. The motivation is to keep supporting legacy client libraries after
    %% redenominations and at the same time protect users from an attack where
    %% a post-redenomination transaction is included in a pre-redenomination block. The attack
    %% is prevented by forbidding inclusion of transactions with denomination=0 in the 100
    %% blocks preceding the redenomination block.
    %%
    %% Transaction denomination code must not exceed the block's denomination code.
    denomination = 0,

    %% The type of signature this transaction was signed with. A system field,
    %% not used by the protocol yet.
    signature_type = {rsa, 65537}
}).

%% The hashing algorithm used to calculate wallet addresses.
-define(HASH_ALG, sha256).

-define(RSA_SIGN_ALG, rsa).
-define(RSA_SIGN_TYPE, <<"rsa-pss-sha256">>).
-define(RSA_PRIV_KEY_SZ, 4096).
-define(RSA_KEY_TYPE, {?RSA_SIGN_ALG, 65537}).

-define(ECDSA_SIGN_ALG, ecdsa).
-define(ECDSA_SIGN_TYPE, <<"ecdsa-secp256k1-sha256">>).
-define(ECDSA_TYPE_BYTE, <<2>>).
-define(ECDSA_KEY_TYPE, {?ECDSA_SIGN_ALG, secp256k1}).

-define(EDDSA_SIGN_ALG, eddsa).
-define(EDDSA_SIGN_TYPE, <<"ed25519-sha512">>).
-define(EDDSA_TYPE_BYTE, <<3>>).
-define(EDDSA_KEY_TYPE, {?EDDSA_SIGN_ALG, ed25519}).

-define(SOLANA_SIGN_ALG, solana).
-define(SOLANA_SIGN_TYPE, <<"solana">>).
-define(SOLANA_TYPE_BYTE, <<4>>).
-define(SOLANA_KEY_TYPE, solana).

-define(ETHEREUM_SIGN_ALG, ethereum).
-define(ETHEREUM_SIGN_TYPE, <<"ethereum">>).
-define(ETHEREUM_TYPE_BYTE, <<3>>).
-define(ETHEREUM_KEY_TYPE, ethereum).

-define(TYPED_ETHEREUM_SIGN_ALG, typed_ethereum).
-define(TYPED_ETHEREUM_SIGN_TYPE, <<"typed_ethereum">>).
-define(TYPED_ETHEREUM_TYPE_BYTE, <<7>>).
-define(TYPED_ETHEREUM_KEY_TYPE, typed_ethereum).

%% The default key type used by transactions that do not specify a signature type.
-define(DEFAULT_KEY_TYPE, ?RSA_KEY_TYPE).

-define(BUNDLE_TAGS, [
    {<<"bundle-format">>, <<"binary">>},
    {<<"bundle-version">>, <<"2.0.0">>}
]).

-define(BUNDLE_KEYS, [
    <<"bundle-format">>, <<"bundle-version">>, <<"bundle-map">>]).

%% The threshold was determined on the mainnet at the 2.5 fork block. The chunks
%% submitted after the threshold must adhere to stricter validation rules.
%% This offset is about half way through partition 8.
%%
%% VENDOR: upstream's home for this constant is `ar_consensus.hrl', which
%% defines it under the same `-ifndef'. This copy exists because HyperBEAM code
%% predating the vendored tree includes only `ar.hrl'. Guarding both means
%% whichever header a module includes first supplies the value instead of this
%% one silently winning on include order -- which matters, because this is the
%% offset that selects a chunk's merkle validation ruleset. Keep the two in
%% sync when upgrading; `ar_consensus.hrl' is authoritative.
-ifndef(STRICT_DATA_SPLIT_THRESHOLD).
-define(STRICT_DATA_SPLIT_THRESHOLD, 30_607_159_107_830).
-endif.

%%%===================================================================
%%% VENDOR: block-consensus constants and records, copied verbatim from
%%% ArweaveTeam/arweave @ 50e47de (release 2.9.6-alpha1),
%%% apps/arweave/include/ar.hrl. Upstream's text, order, comments and
%%% tab indentation are preserved so the section stays diffable against
%%% upstream. The upstream `#tx{}' record is deliberately NOT vendored:
%%% HyperBEAM's own `#tx{}' above is the one used throughout the codec
%%% stack and it has diverged from upstream (`anchor' where upstream
%%% spells `last_tx', `format = ans104' rather than `1', and the extra
%%% `unsigned_id' / `owner_address' / `manifest' fields).
%%%===================================================================

%% The mainnet name. Does not change at the hard forks.
-ifndef(NETWORK_NAME).
	-ifdef(AR_TEST).
		-define(NETWORK_NAME, "arweave.localtest").
	-else.
		-define(NETWORK_NAME, "arweave.N.1").
	-endif.
-endif.

%% When a request is received without specifing the X-Network header, this network name
%% is assumed.
-ifndef(DEFAULT_NETWORK_NAME).
	-define(DEFAULT_NETWORK_NAME, "arweave.N.1").
-endif.

%% The current release number of the arweave client software.
%% @deprecated Not used apart from being included in the /info response.
-define(CLIENT_VERSION, 5).

%% The current build number -- incremented for every release.
-define(RELEASE_NUMBER, 92).

-ifdef(FORKS_RESET).
-define(FORK_1_6, 0).
-else.
%%% FORK INDEX
%%% @deprecated Fork heights from 1.7 on are defined in the ar_fork module.
-define(FORK_1_6, 95000).
-endif.

-define(DEEP_HASH_ALG, sha384).

-define(MERKLE_HASH_ALG, sha384).

-define(RSA_BLOCK_SIG_SIZE, 512).
-define(ECDSA_PUB_KEY_SIZE, 33).
-define(ECDSA_SIG_SIZE, 65).

%% The difficulty a new weave is started with.
-define(DEFAULT_DIFF, 6).

-ifndef(TARGET_BLOCK_TIME).
-define(TARGET_BLOCK_TIME, 120).
-endif.

-ifndef(RETARGET_BLOCKS).
-define(RETARGET_BLOCKS, 10).
-endif.

%% We only do retarget if the time it took to mine ?RETARGET_BLOCKS is more than
%% 1.1 times bigger or smaller than ?TARGET_BLOCK_TIME * ?RETARGET_BLOCKS. Was used before
%% the fork 2.5 where we got rid of the floating point calculations.
-define(RETARGET_TOLERANCE, 0.1).

-define(JOIN_CLOCK_TOLERANCE, 15).

-define(MAX_BLOCK_PROPAGATION_TIME, 60).

-define(CLOCK_DRIFT_MAX, 5).

%% The total supply of tokens in the Genesis block.
-define(GENESIS_TOKENS, 55000000).

%% The number of bytes in a gibibyte.
-define(KiB, (1024)).
-define(MiB, (1024 * ?KiB)).
-define(GiB, (1024 * ?MiB)).
-define(TiB, (1024 * ?GiB)).

%% How far into the past or future the block can be in order to be accepted for
%% processing.
-ifdef(AR_TEST).
-define(STORE_BLOCKS_BEHIND_CURRENT, 10).
-else.
-define(STORE_BLOCKS_BEHIND_CURRENT, 50).
-endif.

%% The maximum lag when fork recovery (chain reorganisation) is performed.
-ifdef(AR_TEST).
-define(CHECKPOINT_DEPTH, 4).
-else.
-define(CHECKPOINT_DEPTH, 18).
-endif.

%% The maximum allowed size in bytes for the data field of
%% a format=1 transaction.
-define(TX_DATA_SIZE_LIMIT, 10 * ?MiB).

%% The maximum allowed size in bytes for the combined data fields of
%% the format=1 transactions included in a block. Must be greater than
%% or equal to ?TX_DATA_SIZE_LIMIT.
-define(BLOCK_TX_DATA_SIZE_LIMIT, ?TX_DATA_SIZE_LIMIT).

%% The maximum number of transactions (both format=1 and format=2) in a block.
-ifdef(AR_TEST).
-define(BLOCK_TX_COUNT_LIMIT, 10).
-else.
-define(BLOCK_TX_COUNT_LIMIT, 1000).
-endif.

%% The base transaction size the transaction fee must pay for.
-define(TX_SIZE_BASE, 3210).

%% Default TCP port.
-define(DEFAULT_HTTP_IFACE_PORT, 1984).

%% The adjustment of difficutly going from SHA-384 to RandomX.
-define(RANDOMX_DIFF_ADJUSTMENT, (-14)).

%% Max allowed difficulty multiplication and division factors, before the fork 2.4.
-define(DIFF_ADJUSTMENT_DOWN_LIMIT, 2).
-define(DIFF_ADJUSTMENT_UP_LIMIT, 4).

%% The maximum allowed packing difficulty.
-define(MAX_PACKING_DIFFICULTY, 32).

%% The number of sub-chunks in a compositely packed chunk.
%% The composite packing with the packing difficulty 1 matches approximately the non-composite
%% 2.6 packing in terms of computational costs.
-define(COMPOSITE_PACKING_SUB_CHUNK_COUNT, 32).

%% The size of a unit sub-chunk in the compositely packed chunk.
-define(COMPOSITE_PACKING_SUB_CHUNK_SIZE,
		(?DATA_CHUNK_SIZE div ?COMPOSITE_PACKING_SUB_CHUNK_COUNT)).

%% The number of RandomX rounds used for a single iteration of packing of a single sub-chunk
%% during the composite packing.
-define(COMPOSITE_PACKING_ROUND_COUNT, 10).

%% Maximum size of a `data_path`, in bytes.
-define(MAX_PATH_SIZE, (256 * 1024)).

%% The speed in chunks/s of moving the fork 2.5 packing threshold.
-ifdef(AR_TEST).
-define(PACKING_2_5_THRESHOLD_CHUNKS_PER_SECOND, 1).
-else.
-define(PACKING_2_5_THRESHOLD_CHUNKS_PER_SECOND, 10).
-endif.

%% The data_root of the system "padding" nodes inserted in the transaction Merkle trees
%% since the 2.5 fork block. User transactions cannot set <<>> for data_root unless
%% data_size == 0. The motivation is to place all chunks including those
%% smaller than 256 KiB into the 256 KiB buckets on the weave, to even out their chances to be
%% picked as recall chunks and therefore equally incentivize the storage.
-define(PADDING_NODE_DATA_ROOT, <<>>).

-ifndef(INITIAL_VDF_DIFFICULTY).
-define(INITIAL_VDF_DIFFICULTY, 600_000).
-endif.

%% @doc A chunk with the proofs of its presence in the weave at a particular offset.
-record(poa, {
	%% DEPRECATED. Not used since the fork 2.4.
	option = 1,
	%% The path through the Merkle tree of transactions' "data_root"s.
	%% Proofs the inclusion of the "data_root" in the corresponding "tx_root"
	%% under the particular offset.
	tx_path = <<>>,
	%% The path through the Merkle tree of the identifiers of the chunks
	%% of the corresponding transaction. Proofs the inclusion of the chunk
	%% in the corresponding "data_root" under a particular offset.
	data_path = <<>>,
	%% When packing difficulty is 0 chunk stores a full ?DATA_CHUNK_SIZE-sized packed chunk.
	%% When packing difficulty >= 1, chunk stores a ?COMPOSITE_PACKING_SUB_CHUNK_SIZE-sized
	%% packed sub-chunk.
	chunk = <<>>,
	%% When packing difficulty is 0 unpacked_chunk is <<>>.
	%% When packing difficulty >= 1, unpacked_chunk stores a full 0-padded
	%% ?DATA_CHUNK_SIZE-sized unpacked chunk.
	unpacked_chunk = <<>>
}).

%% @doc The information which simplifies validation of the nonce limiting procedures.
-record(nonce_limiter_info, {
	%% The output of the latest step - the source of the entropy for the mining nonces.
	output = <<>>,
	%% The output of the latest step of the previous block.
	prev_output = <<>>,
	%% The hash of the latest block mined below the current reset line.
	seed = <<>>,
	%% The hash of the latest block mined below the future reset line.
	next_seed = <<>>,
	%% The weave size of the latest block mined below the current reset line.
	partition_upper_bound = 0,
	%% The weave size of the latest block mined below the future reset line.
	next_partition_upper_bound = 0,
	%% The global sequence number of the nonce limiter step at which the block was found.
	global_step_number = 1,
	%% ?VDF_CHECKPOINT_COUNT_IN_STEP checkpoints from the most recent step in the nonce
	%% limiter process.
	last_step_checkpoints = [],
	%% A list of the output of each step of the nonce limiting process. Note: each step
	%% has ?VDF_CHECKPOINT_COUNT_IN_STEP checkpoints, the last of which is that step's output.
	steps = [],

	%% The fields added at the fork 2.7

	%% The number of SHA2-256 iterations in a single VDF checkpoint. The protocol aims to keep the
	%% checkoint calculation time to around 40ms by varying this paramter. Note: there are
	%% 25 checkpoints in a single VDF step - so the protocol aims to keep the step calculation at
	%% 1 second by varying this parameter.
	vdf_difficulty = ?INITIAL_VDF_DIFFICULTY,
	%% The VDF difficulty scheduled for to be applied after the next VDF reset line.
	next_vdf_difficulty = ?INITIAL_VDF_DIFFICULTY
}).

%% @doc A block (txs is a list of tx records) or a block shadow (txs is a list of
%% transaction identifiers).
-record(block, {
	%% The nonce chosen to solve the mining problem.
	nonce,
	%% `indep_hash` of the previous block in the weave.
	previous_block = <<>>,
	%% POSIX time of block discovery.
	timestamp,
	%% POSIX time of the last difficulty retarget.
	last_retarget,
	%% Mining difficulty, the number `hash` must be greater than.
	diff,
	height = 0,
	%% Mining solution hash.
	hash = <<>>,
	%% The block identifier.
	indep_hash,
	%% The list of transaction identifiers or transactions (tx records).
	txs = [],
	%% The Merkle root of the tree of Merkle roots of block's transactions' data.
	tx_root = <<>>,
	%% The Merkle tree of Merkle roots of block's transactions' data. Used internally,
	%% not gossiped.
	tx_tree = [],
	%% Deprecated. Not used, not gossiped.
	hash_list = unset,
	%% The Merkle root of the block index - the list of
	%% {`indep_hash`, `weave_size`, `tx_root`} triplets describing the past blocks
	%% excluding this one.
	hash_list_merkle = <<>>,
	%% The root hash of the Merkle Patricia Tree containing all wallet (account) balances and
	%% the identifiers of the last transactions posted by them, if any
	wallet_list,
	%% The mining address. Before the fork 2.6, either the atom 'unclaimed' or
	%% a SHA2-256 hash of the RSA PSS public key. In 2.6, 'unclaimed' is not supported.
    reward_addr = unclaimed,
	%% Miner-specified tags (a list of strings) to store with the block.
    tags = [],
	%% The number of Winston in the endowment pool.
	reward_pool,
	%% The total number of bytes whose storage is incentivized.
	weave_size,
	%% The total number of bytes added to the storage incentivization by this block.
	block_size,
	%% The sum of the average number of hashes computed by the network to produce the past
	%% blocks including this one.
	cumulative_diff,
	%% The list of {{`tx_id`, `data_root`}, `offset`} pairs. Used internally, not gossiped.
	size_tagged_txs = unset,
	%% The first proof of access.
	poa = #poa{},
	%% The estimated USD to AR conversion rate used in the pricing calculations.
	%% A tuple {Dividend, Divisor}.
	%% Used until the transition to the new fee calculation method is complete.
	usd_to_ar_rate,
	%% The estimated USD to AR conversion rate scheduled to be used a bit later, used to
	%% compute the necessary fee for the currently signed txs. A tuple {Dividend, Divisor}.
	%% Used until the transition to the new fee calculation method is complete.
	scheduled_usd_to_ar_rate,
	%% The offset on the weave separting the data which has to be packed for mining after the
	%% fork 2.5 from the data which does not have to be packed yet. It is set to the
	%% weave_size of the 50th previous block at the hard fork block and moves down at a speed
	%% of ?PACKING_2_5_THRESHOLD_CHUNKS_PER_SECOND chunks/s. The motivation behind the
	%% threshold is a smooth transition to the new algorithm - big miners who might not want
	%% to adopt the new algorithm are still incentivized to upgrade and stay in the network
	%% for some time.
	packing_2_5_threshold,
	%% The offset on the weave separating the data which has to be split according to the
	%% stricter rules introduced in the fork 2.5 from the historical data. The new rules
	%% require all chunk sizes to be 256 KiB excluding the last or the only chunks of the
	%% corresponding transactions and the second last chunks of their transactions where they
	%% exceed 256 KiB in size when combined with the following (last) chunk. Furthermore, the
	%% new chunks may not be smaller than their Merkle proofs unless they are the last chunks.
	%% The motivation is to be able to put all chunks into 256 KiB buckets. It makes all
	%% chunks equally attractive because they have equal chances of being chosen as recall
	%% chunks. Moreover, every chunk costs the same in terms of storage and computation
	%% expenditure when packed (smaller chunks are simply padded before packing).
	strict_data_split_threshold,
	%% Used internally by tests.
	account_tree,

	%%
	%% The fields below were added at the fork 2.6.
	%%

	%% A part of the solution hash preimage. Used for the initial solution validation
	%% without a data chunk.
	hash_preimage = <<>>,
	%% The absolute recall offset.
	recall_byte,
	%% The total amount of winston the miner receives for this block.
	reward = 0,
	%% The solution hash of the previous block.
	previous_solution_hash = <<>>,
	%% The sequence number of the mining partition where the block was found.
	partition_number,
	%% The nonce limiter information.
	nonce_limiter_info = #nonce_limiter_info{},
	%% The second proof of access (empty when the solution was found with only one chunk).
	poa2 = #poa{},
	%% The absolute second recall offset.
	recall_byte2,
	%% The block signature.
	signature = <<>>,
	%% {KeyType, PubKey} - the public key the block was signed with.
	%% The only supported KeyType is currently {rsa, 65537}.
	reward_key,
	%% The estimated number of Winstons it costs the network to store one gibibyte
	%% for one minute.
	price_per_gib_minute = 0,
	%% The updated estimation of the number of Winstons it costs the network to store
	%% one gibibyte for one minute.
	scheduled_price_per_gib_minute = 0,
	%% The recursive hash of the network hash rates, block rewards, mining addresses,
	%% and denominations.
	%% Note that the length of the reward history has increased from
	%% ?LEGACY_REWARD_HISTORY_BLOCKS to ?REWARD_HISTORY_BLOCKS in 2.8.
	%% Before 2.8 every new hash was computed over the latest ?REWARD_HISTORY_BLOCKS.
	%% After 2.8 the new hash is computed from the new history element and the previous hash.
	reward_history_hash,
	%% The network hash rates, block rewards, and mining addresses from the latest
	%% ?REWARD_HISTORY_BLOCKS + ar_block:get_consensus_window_size() blocks. Used internally, not gossiped.
	reward_history = [],
	%% The total number of Winston emitted when the endowment was not sufficient
	%% to compensate mining.
	debt_supply = 0,
	%% An additional multiplier for the transaction fees doubled every time the
	%% endowment pool becomes empty.
	kryder_plus_rate_multiplier = 1,
	%% A lock controlling the updates of kryder_plus_rate_multiplier. It is set to 1
	%% after the update and back to 0 when the endowment pool is bigger than
	%% ?RESET_KRYDER_PLUS_LATCH_THRESHOLD (redenominated according to the denomination
	%% used at the time).
	kryder_plus_rate_multiplier_latch = 0,
	%% The code for the denomination of AR in base units.
	%% 1 is the default which corresponds to the original denomination of 1^12 base units.
	%% Every time the available supply falls below ?REDENOMINATION_THRESHOLD,
	%% the denomination is multiplied by 1000, the code is incremented.
	%% Transaction denomination code must not exceed the block's denomination code.
	denomination = 1,
	%% The biggest known redenomination height (0 means there were no redenominations yet).
	redenomination_height = 0,
	%% The proof of signing the same block several times or extending two equal forks.
	double_signing_proof,
	%% The cumulative difficulty of the previous block.
	previous_cumulative_diff = 0,

	%%
	%% The fields below were added at the fork 2.7 (note that 2.6.8 was a hard fork too).
	%%

	%% The merkle trees of the data written after this weave offset may be constructed
	%% in a way where some subtrees are "rebased", i.e., their offsets start from 0 as if
	%% they were the leftmost subtree of the entire tree. The merkle paths for the chunks
	%% belonging to the subtrees will include a 32-byte 0-sequence preceding the pivot to
	%% the corresponding subtree. The rebases allow for flexible combination of data before
	%% registering it on the weave, extremely useful e.g., for the bundling services.
	merkle_rebase_support_threshold,
	%% The SHA2-256 of the packed chunk.
	chunk_hash,
	%% The SHA2-256 of the packed chunk2, when present.
	chunk2_hash,

	%% The hashes of the history of block times (in seconds), VDF times (in steps),
	%% and solution types (one-chunk vs two-chunk) of the latest
	%% ?BLOCK_TIME_HISTORY_BLOCKS blocks.
	block_time_history_hash,
	%% The block times (in seconds), VDF times (in steps), and solution types (one-chunk vs
	%% two-chunk) of the latest ?BLOCK_TIME_HISTORY_BLOCKS blocks.
	%% Used internally, not gossiped.
	block_time_history = [], % {block_interval, vdf_interval, chunk_count}

	%%
	%% The fields below were added at the fork 2.8.
	%%

	%% The packing difficulty of the replica the block was mined with.
	%% Applies to both poa1 and poa2.
	%%
	%% Packing difficulty 0 denotes the usual pre-2.8 packing scheme.
	%% Packing difficulty 1 refers to the new composite packing of approximately the same
	%% computational cost as the difficulty 0 packing. Packing difficulty 2 is the composite
	%% packing where each sub-chunk is hashed twice as many times. The maximum allowed
	%% value is 32.
	%%
	%% When packing_difficulty >= 1, both poa1 and poa2 contain the unpacked chunks.
	%% The values of the "chunk" fields are now 8192-byte packed sub-chunks.
	%%
	%% If the block is associated with the new replication format (replica_format=1,)
	%% the packing difficulty is constant and determines the number of nonces
	%% (also, sub-chunks) in the recall range and their mining difficulty, in line with
	%% the chosen computational difficulty of the entropy computation.
	packing_difficulty = 0,
	%% The SHA2-256 of the unpacked 0-padded (if less than 256 KiB) chunk.
	%% undefined when packing_difficulty == 0, has a value otherwise.
	unpacked_chunk_hash,
	%% The SHA2-256 of the unpacked 0-padded (if less than 256 KiB) chunk2.
	%% undefined when packing_difficulty == 0 or recall_byte2 == undefined,
	%% has a value otherwise.
	unpacked_chunk2_hash,

	%% The replica format 0 is the inefficient "packing" where every chunk is packed
	%% independently. The replica format 1 is new the blazing fast replication format.
	replica_format = 0,

	%% Used internally, not gossiped. Convenient for validating potentially non-unique
	%% merkle proofs assigned to the different signatures of the same solution
	%% (see validate_poa_against_cached_poa in ar_block_pre_validator.erl).
	poa_cache,
	%% Used internally, not gossiped. Convenient for validating potentially non-unique
	%% merkle proofs assigned to the different signatures of the same solution
	%% (see validate_poa_against_cached_poa in ar_block_pre_validator.erl).
	poa2_cache,

	%% Used internally, not gossiped.
	receive_timestamp
}).

%% A macro to return whether a term is a block record.
-define(IS_BLOCK(X), (is_record(X, block))).

%% Convert a v2.0 block index into an old style block hash list.
-define(BI_TO_BHL(BI), ([BH || {BH, _, _} <- BI])).

%% Pattern matches on ok-tuple and returns the value.
-define(OK(Tuple), begin (case (Tuple) of {ok, SuccessValue} -> (SuccessValue) end) end).

%% Use a standard way of logging.
%% For more details see https://erlang.org/doc/man/logger.html#macros.
-include_lib("kernel/include/logger.hrl").


%% @doc The data_path field will only be not_found if the chunk record is corrupt/invalid.
%% This can happen if the chunk entry exists in the chunks_index but not in the chunk_data_db.
%% In this case:
%% - not_set means that a field has not been queried yet.
%% - not_found means that the field has been queried but could not be found.
-record(chunk_metadata, {
	chunk_data_key = not_set :: not_set | binary(),
	tx_root = not_set :: not_set | binary(),
	tx_path = not_set :: not_set | binary(),
	data_root = not_set :: not_set | binary(),
	data_path = not_set :: not_set | not_found | binary(),
	chunk_size = not_set :: not_set | non_neg_integer()
}).


-endif.
