-module(ao_cache).
-export([
	read_output/3,
	write/2,
	write_output/4,
	write_assignment/2,
	outputs/2,
	assignments/2,
	latest/2, latest/3, latest/4,
	read/2, read/3, read/4,
	lookup/2, lookup/3,
	read_assignment/3
]).
-include("src/include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_DIR, "test-cache").

% -define(TEST_STORE, {ao_fs_store, #{ dir => ?TEST_DIR }}).
-define(TEST_STORE, {ao_rocksdb_store, #{dir => ?TEST_DIR}}).

-define(COMPUTE_CACHE_DIR, "computed").
-define(ASSIGNMENTS_DIR, "assignments").

%%% A cache of AO messages and compute results.
%%%
%%% In AO, every message is a combinator: The message itself represents a
%%% 'processor' that can be applied to a new message, yielding a result.
%%% As a consequence, a simple way of understanding AO's computation model is to
%%% think of it as a dictionary: Every message is a key, yielding its computed value.
%%%
%%% Each message itself can be raw data with an associated header (containing metadata),
%%% or a bundle of other messages (its children). These children are expressed as
%%% either maps or list of other messages.
%%%
%%% We store each of the messages in a cache on disk. The cache is a simple
%%% wrapper that allows us to look up either the direct key (a message's ID --
%%% either signed or unsigned) or a 'subpath'. We also store a cache of the linkages
%%% between messages as symlinks. In the backend, we store each message as either a
%%% directory -- if it contains further data items inside -- or as a file, if it is
%%% a simple value.
%%%
%%% The file structure of the store is as follows:
%%%
%%% Root: ?DEFAULT_DATA_DIR
%%% Messages: ?DEFAULT_DATA_DIR/messages
%%% Computed outputs: ?DEFAULT_DATA_DIR/computed
%%%
%%% Outputs by process: ?DEFAULT_DATA_DIR/computed/ProcessID
%%% Outputs by slot on process: ?DEFAULT_DATA_DIR/computed/ProcessID/slot/[n]
%%% Outputs by message on process: ?DEFAULT_DATA_DIR/computed/ProcessID/MessageID[/Subpath]
%%%
%%% Outputs are stored as symlinks to the actual file or directory containing the message.
%%% Messages that are composite are represented as directories containing their childen
%%% (by ID and by subpath), as well as their base message stored at `.base`.

latest(Store, ProcID) ->
	latest(Store, ProcID, undefined).
latest(Store, ProcID, Limit) ->
	latest(Store, ProcID, Limit, []).
latest(Store, ProcID, Limit, Path) ->
	case outputs(Store, ProcID) of
		[] ->
			not_found;
		AllOutputSlots ->
			Slot =
				first_slot_with_path(
					Store,
					ProcID,
					lists:reverse(lists:sort(AllOutputSlots)),
					Limit,
					Path
				),
			ResolvedPath =
				ar_util:remove_common(
					ao_store:resolve(
						Store,
						ao_store:path(Store, ["computed", fmt_id(ProcID), "slot", integer_to_list(Slot)])
					),
					"messages"
				),
			?c({resolved_path, ResolvedPath}),
			Msg = read(Store, ResolvedPath),
			?c(got_message),
			{Slot, Msg}
	end.

first_slot_with_path(_Store, _ProcID, [], _Limit, _Path) ->
	not_found;
first_slot_with_path(Store, ProcID, [AfterLimit | Rest], Limit, Path) when AfterLimit > Limit ->
	first_slot_with_path(Store, ProcID, Rest, Limit, Path);
first_slot_with_path(Store, ProcID, [LatestSlot | Rest], Limit, Path) ->
	?c({trying_slot, LatestSlot, Path}),
	RawPath =
		build_path(
			["computed", process, "slot", slot] ++ Path,
			#{slot => integer_to_list(LatestSlot), process => fmt_id(ProcID)}
		),
	ResolvedPath = ao_store:resolve(Store, RawPath),
	case ao_store:type(Store, ResolvedPath) of
		not_found -> first_slot_with_path(Store, ProcID, Rest, Limit, Path);
		_ -> LatestSlot
	end.

build_path(PathList, Map) ->
	lists:map(
		fun
			(Ref) when is_atom(Ref) -> maps:get(Ref, Map);
			(Other) -> Other
		end,
		PathList
	).

outputs(Store, ProcID) ->
	slots(Store, [?COMPUTE_CACHE_DIR, fmt_id(ProcID), "slot"]).

assignments(Store, ProcID) ->
	slots(Store, [?ASSIGNMENTS_DIR, fmt_id(ProcID)]).

slots(Store, Path) ->
	SlotDir = ao_store:path(Store, Path),
	case ao_store:list(Store, SlotDir) of
		{ok, Names} -> [list_to_integer(Name) || Name <- Names];
		{error, _} -> []
	end.

%% Write a full message to the cache.
write_output(Store, ProcID, Slot, Item) ->
	% Write the message into the main cache
	ok = write(Store, Item),
	UnsignedID = fmt_id(Item, unsigned),
	SignedID = fmt_id(Item, signed),
	% Create symlinks from the message on the process and the slot on the process
	% to the underlying data.
	RawMessagePath = ao_store:path(Store, ["messages", UnsignedID]),
	ProcMessagePath = ao_store:path(Store, ["computed", fmt_id(ProcID), UnsignedID]),
	ProcSlotPath = ao_store:path(Store, ["computed", fmt_id(ProcID), "slot", integer_to_list(Slot)]),
	ao_store:make_link(Store, RawMessagePath, ProcMessagePath),
	ao_store:make_link(Store, RawMessagePath, ProcSlotPath),
	if
		SignedID =/= UnsignedID ->
			ok = ao_store:make_link(
				Store,
				RawMessagePath,
				ao_store:path(Store, ["computed", fmt_id(ProcID), SignedID])
			);
		true ->
			already_exists
	end,
	ok.

%% Write a full message to the cache.
write_assignment(Store, Assignment) ->
	% Write the message into the main cache
	{_, ProcID} = lists:keyfind(<<"Process">>, 1, Assignment#tx.tags),
	{_, Slot} = lists:keyfind(<<"Slot">>, 1, Assignment#tx.tags),
	ok = write(Store, Assignment),
	UnsignedID = fmt_id(Assignment, unsigned),
	% Create symlinks from the message on the process and the
	% slot on the process to the underlying data.
	RawMessagePath =
		ao_store:path(Store, [
			"messages",
			UnsignedID
		]),
	AssignmentPath =
		ao_store:path(Store, [
			"assignments",
			fmt_id(ProcID),
			binary_to_list(Slot)
		]),
	ao_store:make_link(Store, RawMessagePath, AssignmentPath),
	ok.

write(Store, Item) ->
	write(Store, ao_store:path(Store, ["messages"]), Item).

write(Store, Path, Item) when not is_record(Item, tx) ->
	write(Store, Path, ar_bundles:normalize(Item));
write(Store, Path, Item) when Item#tx.id == ?DEFAULT_ID ->
	write(Store, Path, ar_bundles:normalize(Item));
write(Store, Path, Item) ->
	case ar_bundles:type(Item) of
		binary ->
			% The item is a raw binary. Write it into the store and make a
			% link for the signed ID if it is different.
			UnsignedID = ar_bundles:id(Item, unsigned),
			SignedID = ar_bundles:id(Item, signed),
			UnsignedPath = ao_store:path(Store, [Path, fmt_id(UnsignedID)]),
			ok = ao_store:write(Store, UnsignedPath, ar_bundles:serialize(Item)),
			if
				SignedID =/= UnsignedID ->
					SignedPath = ao_store:path(Store, [Path, fmt_id(SignedID)]),
					ao_store:make_link(Store, UnsignedPath, SignedPath);
				true ->
					link_unnecessary
			end;
		CompositeType ->
			write_composite(Store, Path, CompositeType, Item)
	end,
	ok.

write_composite(Store, Path, map, Item) ->
	% The item is a map. We should:
	% 1. Write the header item into the store without its children.
	% 2. Write each child into the store.
	% 3. Make links from the keys in the map to the corresponding messages.
	% This process will recurse as necessary to write grandchild messages.
	UnsignedHeaderID = ar_bundles:id(Item, unsigned),
	ok = ao_store:make_group(Store, Dir = ao_store:path(Store, [Path, fmt_id(UnsignedHeaderID)])),
	SignedHeaderID = ar_bundles:id(Item, signed),
	ok = ao_store:make_link(
		Store,
		ao_store:path(Store, [Path, fmt_id(UnsignedHeaderID)]),
		ao_store:path(Store, [Path, fmt_id(SignedHeaderID)])
	),
	ok = ao_store:write(
		Store,
		ao_store:path(Store, [Path, fmt_id(UnsignedHeaderID), "item"]),
		ar_bundles:serialize(Item#tx{data = #{<<"manifest">> => ar_bundles:manifest_item(Item)}})
	),

	maps:map(
		fun(Key, Subitem) ->
			% Note: _Not_ relative to the Path! All messages are stored at the
			% same root of the store.
			ok = write(Store, Subitem),
			ao_store:make_link(
				Store,
				ao_store:path(Store, [Path, fmt_id(Subitem)]),
				ao_store:path(Store, [Dir, Key])
			)
		end,
		ar_bundles:map(Item)
	),
	ok;
write_composite(Store, Path, list, Item) ->
	write_composite(Store, Path, map, ar_bundles:normalize(Item)).

read_output(Store, ProcID, undefined) ->
	element(2, latest(Store, ProcID));
read_output(Store, ProcID, Slot) when is_integer(Slot) ->
	read_output(Store, fmt_id(ProcID), ["slot", integer_to_list(Slot)]);
read_output(Store, ProcID, MessageID) when is_binary(MessageID) andalso byte_size(MessageID) == 32 ->
	read_output(Store, fmt_id(ProcID), fmt_id(MessageID));
read_output(Store, ProcID, SlotBin) when is_binary(SlotBin) ->
	read_output(Store, ProcID, ["slot", binary_to_list(SlotBin)]);
read_output(Store, ProcID, Input) ->
	?c({reading_computed_result, Input}),
	ResolvedPath =
		ar_util:remove_common(
			ar_util:remove_common(
				?c(
					ao_store:resolve(
						Store,
						ao_store:path(Store, [?COMPUTE_CACHE_DIR, fmt_id(ProcID), Input])
					)
				),
				?COMPUTE_CACHE_DIR
			),
			"messages"
		),
	?c({resolved_path, ResolvedPath}),
	case ao_store:type(Store, ["messages", ResolvedPath]) of
		not_found ->
			?c(not_found);
		_Type ->
			read(Store, ResolvedPath)
	end.

read_assignment(Store, _ProcID, AssignmentID) when is_binary(AssignmentID) ->
	read(Store, AssignmentID);
read_assignment(Store, ProcID, Slot) ->
	ResolvedPath =
		ar_util:remove_common(
			ao_store:resolve(
				Store,
				ao_store:path(Store, [
					"assignments",
					fmt_id(ProcID),
					integer_to_list(Slot)
				])
			),
			"messages"
		),
	?c({resolved_path, ResolvedPath}),
	read(Store, ResolvedPath).

lookup(Store, ID) ->
	lookup(Store, ID, "").
lookup(Store, ID, Subpath) ->
	lookup(Store, ID, Subpath, "messages").
lookup(Store, ID, Subpath, DirBase) ->
	ResolvedPath =
		ar_util:remove_common(
			ao_store:resolve(
				Store,
				ao_store:path(Store, [DirBase, fmt_id(ID), Subpath])
			),
			"messages"
		),
	?c({resolved_path, ResolvedPath}),
	read(Store, ResolvedPath, DirBase, DirBase).

read(Store, ID) ->
	read(Store, ID, "messages").
read(Store, ID, DirBase) ->
	read(Store, ID, DirBase, all).
read(Store, ID, DirBase, Subpath) ->
	?c({reading_message, ID, DirBase, Subpath}),
	MessagePath = ao_store:path(Store, [DirBase, fmt_id(ID)]),
	case ao_store:type(Store, MessagePath) of
		composite ->
			case Subpath of
				all ->
					% The message is a bundle and we want the whole item.
					% Read the root and reconstruct it.
					RootPath = ao_store:path(Store, [MessagePath, "item"]),
					Root = read_simple_message(Store, RootPath),
					% The bundle is a map of its children by ID. Reconstruct
					% the bundle by reading each child.
					Root#tx{
						data = maps:map(
							fun(_, Key) -> read(Store, Key) end,
							ar_bundles:parse_manifest(
								maps:get(<<"manifest">>, Root#tx.data)
							)
						)
					};
				_ ->
					% Subpath is specified, so we look for a child message.
					Direct = ao_store:path(Store, [MessagePath, Subpath]),
					case ao_store:type(Store, Direct) of
						not_found ->
							% We can't find the in one hop, so find greatest common
							% child and recurse.
							case best_common_prefix(MessagePath, Subpath) of
								{_, Subpath} -> not_found;
								{NextID, RemainingSubpath} -> read(Store, NextID, "", RemainingSubpath)
							end;
						_ ->
							% The subpath is a direct value. Read it.
							read(Store, Direct, "", "")
					end
			end;
		simple ->
			read_simple_message(Store, MessagePath);
		not_found ->
			not_found
	end.

read_simple_message(Store, Path) ->
	{ok, Bin} = ao_store:read(Store, Path),
	ar_bundles:deserialize(Bin).

best_common_prefix(Base, Subpath) ->
	Children = ao_keyval:children(fmt_id(Base)),
	{LongestCommon, BestChild} = lists:foldl(
		fun(Child, {BestSharedBytes, BestSharedPath}) ->
			Common = binary:longest_common_prefix(Child, Subpath),
			if
				byte_size(Common) > BestSharedBytes -> {byte_size(Common), Common};
				true -> {BestSharedBytes, BestSharedPath}
			end
		end,
		{0, <<>>},
		Children
	),
	{BestChild, binary:part(Subpath, {byte_size(Subpath), -byte_size(LongestCommon)})}.

fmt_id(ID) -> fmt_id(ID, unsigned).
fmt_id(ID, Type) when is_record(ID, tx) -> fmt_id(ar_bundles:id(ID, Type));
fmt_id(ID, _) when is_list(ID) andalso length(ID) == 43 -> ID;
fmt_id(ID, _) when is_binary(ID) andalso byte_size(ID) == 43 -> ID;
fmt_id(ID, _Type) when is_binary(ID) andalso byte_size(ID) == 32 ->
	binary_to_list(ar_util:id(ID));
fmt_id(ID, _Type) ->
	?c({warning, invalid_id_type, ID, _Type}),
	ID.

%%% Tests

%% Helpers
test_cache() ->
	ao_store:start(?TEST_STORE),
	ao_store:reset(?TEST_STORE),
	?TEST_STORE.

create_unsigned_tx(Data) ->
	ar_bundles:normalize(#tx{format = ans104, data = Data}).

%% Helper function to create signed #tx items.
create_signed_tx(Data) ->
	ar_bundles:sign_item(create_unsigned_tx(Data), ar_wallet:new()).

%% Test path resolution dynamics.
simple_path_resolution_test() ->
	TestStore = test_cache(),
	ao_store:start(TestStore),
	ao_store:reset(?TEST_STORE),
	ao_store:write(TestStore, <<"test-file">>, <<"test-data">>),
	ao_store:make_link(TestStore, "test-file", "test-link"),
	?assertEqual({ok, <<"test-data">>}, ao_store:read(TestStore, <<"test-link">>)).

resursive_path_resolution_test() ->
	ao_store:write(TestStore = test_cache(), "test-file", <<"test-data">>),
	ao_store:make_link(TestStore, "test-file", "test-link"),
	ao_store:make_link(TestStore, "test-link", "test-link2"),
	?assertEqual({ok, <<"test-data">>}, ao_store:read(TestStore, "test-link2")).

% For now it's broken in rocksdb_store, as it supposed to write a file under a subfolder
% hierarchical_path_resolution_test() ->
%     TestStore = test_cache(),
%     ao_store:make_group(TestStore, "test-dir1"),
%     ao_store:write(TestStore, ["test-dir1", "test-file"], <<"test-data">>),
%     ao_store:make_link(TestStore, ["test-dir1"], "test-link"),
%     ?assertEqual({ok, <<"test-data">>}, ao_store:read(TestStore, ["test-link", "test-file"])).

%% Test storing and retrieving a simple unsigned item
store_simple_unsigned_item_test() ->
	Item = create_unsigned_tx(<<"Simple unsigned data item">>),
	%% Write the simple unsigned item
	ok = write(TestStore = test_cache(), Item),
	%% Read the item back
	RetrievedItem = read(TestStore, Item),
	?assertEqual(Item, RetrievedItem).

%% Test storing and retrieving a simple signed item
simple_signed_item_test() ->
	Item = create_signed_tx(<<"Simple signed data item">>),
	%% Write the simple signed item
	ok = write(TestStore = test_cache(), Item),
	%% Read the item back
	RetrievedItemUnsigned = read(TestStore, ar_bundles:id(Item, unsigned)),
	RetrievedItemSigned = read(TestStore, ar_bundles:id(Item, signed)),
	%% Assert that the retrieved item matches the original and verifies
	?assertEqual(Item, RetrievedItemUnsigned),
	?assertEqual(Item, RetrievedItemSigned),
	?assertEqual(true, ar_bundles:verify_item(RetrievedItemSigned)).

%% Test storing and retrieving a composite unsigned item
composite_unsigned_item_test() ->
	ItemData = #{
		<<"key1">> => create_unsigned_tx(<<"value1">>),
		<<"key2">> => create_unsigned_tx(<<"value2">>)
	},
	Item = ar_bundles:deserialize(create_unsigned_tx(ItemData)),
	ok = write(TestStore = test_cache(), Item),

	% This is a root item...
	RetrievedItem = read(TestStore, Item#tx.id),
	?assertEqual(
		ar_bundles:id((maps:get(<<"key1">>, Item#tx.data)), unsigned),
		ar_bundles:id((maps:get(<<"key1">>, RetrievedItem#tx.data)), unsigned)
	),
	?assertEqual(
		ar_bundles:id((maps:get(<<"key2">>, Item#tx.data)), unsigned),
		ar_bundles:id((maps:get(<<"key2">>, RetrievedItem#tx.data)), unsigned)
	),
	?assertEqual(
		ar_bundles:id(Item, unsigned),
		ar_bundles:id(RetrievedItem, unsigned)
	).

%% Test storing and retrieving a composite signed item
composite_signed_item_test() ->
	ItemData = #{
		<<"key1">> => create_signed_tx(<<"value1">>),
		<<"key2">> => create_signed_tx(<<"value2">>)
	},
	Item = ar_bundles:deserialize(create_signed_tx(ItemData)),
	ok = write(TestStore = test_cache(), Item),

	% RetrievedItem = read(TestStore, Item#tx.id),
	RetrievedItem = read(TestStore, ar_bundles:id(Item, unsigned)),
	?assertEqual(
		ar_bundles:id((maps:get(<<"key1">>, Item#tx.data)), unsigned),
		ar_bundles:id((maps:get(<<"key1">>, RetrievedItem#tx.data)), unsigned)
	),
	?assertEqual(
		ar_bundles:id((maps:get(<<"key2">>, Item#tx.data)), signed),
		ar_bundles:id((maps:get(<<"key2">>, RetrievedItem#tx.data)), signed)
	),
	?assertEqual(ar_bundles:id(Item, unsigned), ar_bundles:id(RetrievedItem, unsigned)),
	%?assertEqual(ar_bundles:id(Item, signed), ar_bundles:id(RetrievedItem, signed)),
	?assertEqual(true, ar_bundles:verify_item(Item)),
	?assertEqual(true, ar_bundles:verify_item(RetrievedItem)).

%% Test deeply nested item storage and retrieval
deeply_nested_item_test() ->
	%% Create nested data
	DeepValueTx = create_signed_tx(<<"deep_value">>),
	Level3Tx = create_unsigned_tx(#{<<"level3_key">> => DeepValueTx}),
	Level2Tx = create_signed_tx(#{<<"level2_key">> => Level3Tx}),
	Level1Tx = create_unsigned_tx(#{<<"level1_key">> => Level2Tx}),
	%% Write the nested item
	ok = write(TestStore = test_cache(), Level1Tx),
	%% Read the deep value back using subpath
	RetrievedItem = lookup(TestStore, Level1Tx#tx.id, ["level1_key", "level2_key", "level3_key"]),
	?assertEqual(<<"deep_value">>, RetrievedItem#tx.data),
	% %% Assert that the retrieved item matches the original deep value
	?assertEqual(
		ar_bundles:id(DeepValueTx, unsigned),
		ar_bundles:id(RetrievedItem, unsigned)
	).

write_and_read_output_test() ->
	Store = test_cache(),
	Proc = create_signed_tx(#{<<"test-item">> => create_unsigned_tx(<<"test-body-data">>)}),
	Item1 = create_signed_tx(<<"Simple signed output #1">>),
	Item2 = create_signed_tx(<<"Simple signed output #2">>),
	ok = write_output(Store, Proc#tx.id, 0, Item1),
	ok = write_output(Store, Proc#tx.id, 1, Item2),

	?assertEqual(Item1, read(Store, Item1#tx.id)),
	?assertEqual(Item2, read(Store, Item2#tx.id)),
	?assertEqual(Item2, read_output(Store, fmt_id(Proc#tx.id), 1)),
	?assertEqual(Item1, read_output(Store, fmt_id(Proc#tx.id), Item1#tx.id)).

% latest_output_retrieval_test_broken() ->
%     Store = test_cache(),
%     Proc = create_signed_tx(#{ <<"test-item">> => create_unsigned_tx(<<"test-body-data">>) }),
%     Item1 = create_signed_tx(<<"Simple signed output #1">>),
%     Item2 = create_signed_tx(<<"Simple signed output #2">>),
%     ok = write_output(Store, Proc#tx.id, 0, Item1),
%     ok = write_output(Store, Proc#tx.id, 1, Item2),
%     ?assertEqual(Item2, latest(Store, Proc#tx.id)),
%     % TODO: Validate that this is the correct item -- is the 'limit' inclusive or exclusive?
%     ?assertEqual(Item1, latest(Store, Proc#tx.id, 1)).
