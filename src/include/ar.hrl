%% Maximum size of a single data chunk, in bytes.
-define(DATA_CHUNK_SIZE, (256 * 1024)).

%% The size of data chunk hashes, in bytes.
-define(CHUNK_ID_HASH_SIZE, 32).

-define(NOTE_SIZE, 32).

-define(DEFAULT_SIG, << 0:4096 >>).
-define(DEFAULT_ID, << 0:256 >>).
-define(DEFAULT_OWNER, << 0:4096 >>).
-define(DEFAULT_DATA, <<>>).

-define(MAX_TAG_NAME_SIZE, 3072).
-define(MAX_TAG_VALUE_SIZE, 3072).

-define(RSA_SIGN_ALG, rsa).
-define(RSA_PRIV_KEY_SZ, 4096).

-define(ECDSA_SIGN_ALG, ecdsa).
-define(ECDSA_TYPE_BYTE, <<2>>).

-define(EDDSA_SIGN_ALG, eddsa).
-define(EDDSA_TYPE_BYTE, <<3>>).

%% The default key type used by transactions that do not specify a signature type.
-define(RSA_KEY_TYPE, {?RSA_SIGN_ALG, 65537}).
-define(ECDSA_KEY_TYPE, {?ECDSA_SIGN_ALG, secp256k1}).
-define(DEFAULT_KEY_TYPE, ?RSA_KEY_TYPE).

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
    last_tx = <<>>,
    %% The public key the transaction is signed with.
    owner =	?DEFAULT_OWNER,
	%% The owner address. Used as a cache to avoid recomputing it, not serialized.
	owner_address = not_set,
    %% A list of arbitrary key-value pairs. Keys and values are binaries.
    tags = [],
    %% The address of the recipient, if any. The SHA2-256 hash of the public key.
    target = <<>>,
    %% The amount of Winstons to send to the recipient, if any.
    quantity = 0,
    %% The data to upload, if any. For v2 transactions, the field is optional - a fee
    %% is charged based on the "data_size" field, data itself may be uploaded any time
    %% later in chunks.
    data = ?DEFAULT_DATA,
    %% Not serialized.
    manifest = undefined,
    %% Size in bytes of the transaction data.
    data_size = 0,
    %% Deprecated. Not used, not gossiped.
    data_tree = [],
    %% The Merkle root of the Merkle tree of data chunks.
    data_root = <<>>,
    %% The signature.
    signature = ?DEFAULT_SIG,
    %% The fee in Winstons.
    reward = 0,

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
    signature_type = ?RSA_KEY_TYPE
}).


