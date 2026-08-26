-ifndef(AR_CHUNK_STORAGE_HRL).
-define(AR_CHUNK_STORAGE_HRL, true).

-define(OFFSET_SIZE, 3). % Sufficient to represent a number up to 256 * 1024 (?DATA_CHUNK_SIZE).
-define(OFFSET_BIT_SIZE, (?OFFSET_SIZE * 8)).

-define(CHUNK_DIR, "chunk_storage").

%% VENDOR: upstream keeps this in `ar.hrl'. It names the catch-all storage
%% module an Arweave node writes directly under its data directory, which an
%% import reads alongside the configured modules.
-define(DEFAULT_MODULE, "default").

%% VENDOR: upstream keeps this in `arweave_config.hrl' as the default of the
%% `chunk_storage_file_size' node option. HyperBEAM reads the same value from
%% the `arweave-chunk-group-size' node option and passes it in explicitly, so
%% the constant lives here as that option's default.
-define(CHUNK_GROUP_SIZE, (256 * 1024 * 8000)). % 2 GiB.

-endif.
