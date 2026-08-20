%%% VENDOR: upstream also reads the node's configured module list out of
%%% `arweave_config' (`get_by_id/1', `get_range/1', `get/2', `get_all/1,2',
%%% `has_any/1', `has_range/2', `is_repack_in_place/1') and mints prometheus
%%% label atoms from an ETS counter (`label/1', `address_label/2',
%%% `packing_label/1'). Neither survives the port: HyperBEAM reads its module
%%% list from the node message and passes it in, and it labels nothing. What is
%%% kept is the part on-disk compatibility depends on -- how a module is named
%%% and which offsets it covers.
-module(ar_storage_module).

-export([get_overlap/1, id/1, module_address/1, module_packing_difficulty/1,
		module_range/1, module_range/2, get_packing/1, get_all/2]).

-include("include/ar.hrl").
-include("include/ar_chunk_storage.hrl").
-include("include/ar_consensus.hrl").

%% The overlap makes sure a 100 MiB recall range can always be fetched
%% from a single storage module.
-define(OVERLAP, (?LEGACY_RECALL_RANGE_SIZE)).

-define(REPLICA_2_9_OVERLAP, (262144 * 10)).

-type storage_module() :: {integer(), integer(), {atom(), binary()}}
						| {integer(), integer(), {atom(), binary(), integer()}}.

-export_type([storage_module/0]).

%%%===================================================================
%%% Public interface.
%%%===================================================================

get_overlap({replica_2_9, _Addr}) ->
	?REPLICA_2_9_OVERLAP;
get_overlap(_Packing) ->
	?OVERLAP.

%% @doc Return the storage module identifier.
%%
%% VENDOR: `ar_util:encode/1' upstream; HyperBEAM's own base64url encoder is
%% the same function under a different name.
id(?DEFAULT_MODULE) -> ?DEFAULT_MODULE;
id({BucketSize, Bucket, Packing}) ->
	PackingString =
		case Packing of
			{spora_2_6, Addr} ->
				hb_util:encode(Addr);
			{composite, Addr, PackingDiff} ->
				<< (hb_util:encode(Addr))/binary, ".",
						(integer_to_binary(PackingDiff))/binary >>;
			{replica_2_9, Addr} ->
				<< (hb_util:encode(Addr))/binary, ".replica.2.9" >>;
			_ ->
				atom_to_list(Packing)
		end,
	id(BucketSize, Bucket, PackingString).

-spec module_address(storage_module()) -> binary() | undefined.
module_address({_, _, {spora_2_6, Addr}}) ->
	Addr;
module_address({_, _, {composite, Addr, _PackingDifficulty}}) ->
	Addr;
module_address({_, _, {replica_2_9, Addr}}) ->
	Addr;
module_address(_StorageModule) ->
	undefined.

-spec module_packing_difficulty(storage_module()) -> integer().
module_packing_difficulty({_, _, {composite, _Addr, PackingDifficulty}}) ->
	true = PackingDifficulty /= ?REPLICA_2_9_PACKING_DIFFICULTY,
	PackingDifficulty;
module_packing_difficulty({_, _, {replica_2_9, _Addr}}) ->
	?REPLICA_2_9_PACKING_DIFFICULTY;
module_packing_difficulty(_StorageModule) ->
	0.

-spec module_range(storage_module()) -> {non_neg_integer(), non_neg_integer()}.
module_range(Module) ->
	{_BucketSize, _Bucket, Packing} = Module,
	module_range(Module, get_overlap(Packing)).

module_range(Module, Overlap) ->
	{BucketSize, Bucket, _Packing} = Module,
	{BucketSize * Bucket, (Bucket + 1) * BucketSize + Overlap}.

%% @doc Return the packing configured for the given module.
get_packing(?DEFAULT_MODULE) ->
	unpacked;
get_packing({_BucketSize, _Bucket, Packing}) ->
	Packing.

%% @doc Return every module of the given list covering the given offset.
get_all(Offset, StorageModules) ->
	get_all2(Offset, StorageModules, []).

%%%===================================================================
%%% Private functions.
%%%===================================================================

id(BucketSize, Bucket, PackingString) ->
	case BucketSize == ar_block:partition_size() of
		true ->
			binary_to_list(iolist_to_binary(io_lib:format("storage_module_~B_~s",
					[Bucket, PackingString])));
		false ->
			binary_to_list(iolist_to_binary(io_lib:format("storage_module_~B_~B_~s",
					[BucketSize, Bucket, PackingString])))
	end.

get_all2(Offset, [{BucketSize, Bucket, Packing} = StorageModule | StorageModules],
		FoundModules) ->
	case Offset =< BucketSize * Bucket
			orelse Offset > BucketSize * (Bucket + 1) + get_overlap(Packing) of
		true ->
			get_all2(Offset, StorageModules, FoundModules);
		false ->
			get_all2(Offset, StorageModules, [StorageModule | FoundModules])
	end;
get_all2(_Offset, [], FoundModules) ->
	FoundModules.
