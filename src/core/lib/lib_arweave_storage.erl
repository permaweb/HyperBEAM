%%% @doc The storage modules an Arweave node holds the weave in.
%%%
%%% A storage module is a range of the weave held in one packing format, on
%%% disk, in the layout the Arweave node writes: a directory named for the
%%% range and the packing, holding fixed-size chunk files under
%%% `chunk_storage'. An operator points this node at a data directory an
%%% Arweave node already filled and it reads what is there, because the layout
%%% is the vendored arithmetic in `ar_chunk_storage' and `ar_storage_module'
%%% rather than anything this node invented.
%%%
%%% This module owns the configuration: which directory the modules live
%%% under, how large their chunk files are, and how a module's directory name
%%% is read back into the `{BucketSize, Bucket, Packing}' triple the vendored
%%% code names a module by.
-module(lib_arweave_storage).
-export([modules/1, module/2, discovered/1, id/1, range/1]).
-export([packing/1, packing_label/1, address/1]).
-export([data_dir/1, chunk_group_size/1, module_path/2, chunk_dir/2]).
-export([parse_id/1]).
-include("include/hb.hrl").
-include("include/ar_chunk_storage.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The storage modules this node is configured to hold, oldest option
%% spelling first. Each is the `{BucketSize, Bucket, Packing}' triple the
%% vendored code names a module by.
modules(Opts) ->
    [
        module(Entry, Opts)
    ||
        Entry <-
            hb_util:message_to_ordered_list(
                hb_opts:get(<<"arweave-storage-modules">>, [], Opts),
                Opts
            )
    ].

%% @doc Read one storage module from its configuration entry. The bucket size
%% defaults to the partition size, which is the size a miner's modules take.
module(Entry, Opts) ->
    BucketSize =
        hb_util:int(
            hb_maps:get(
                <<"bucket-size">>, Entry, ar_block:partition_size(), Opts)
        ),
    Bucket = hb_util:int(hb_maps:get(<<"bucket">>, Entry, 0, Opts)),
    {BucketSize, Bucket, entry_packing(Entry, Opts)}.

%% @doc The storage modules a data directory already holds, by reading the
%% names under `storage_modules'. Directories that do not spell a module --
%% an Arweave node's own `rocksdb', a cursor file -- are passed over.
discovered(Opts) ->
    Dir = filename:join(data_dir(Opts), "storage_modules"),
    Names =
        case file:list_dir(Dir) of
            {ok, Listed} -> lists:sort(Listed);
            {error, _Reason} -> []
        end,
    [
        Module
    ||
        Name <- Names,
        (Module = parse_id(Name)) /= not_found
    ].

%% @doc The identifier a module's directory is named by. This is the vendored
%% spelling, so a directory an Arweave node created is found under it.
id(Module) ->
    ar_storage_module:id(Module).

%% @doc The offsets a module is responsible for, including the overlap that
%% lets a whole recall range be read from one module.
range(Module) ->
    ar_storage_module:module_range(Module).

%% @doc The packing a module holds its chunks in.
packing(Module) ->
    ar_storage_module:get_packing(Module).

%% @doc Name a packing on the wire.
packing_label({spora_2_6, _Addr}) -> <<"spora-2-6">>;
packing_label({composite, _Addr, _Difficulty}) -> <<"composite">>;
packing_label({replica_2_9, _Addr}) -> <<"replica-2-9">>;
packing_label(unpacked) -> <<"unpacked">>;
packing_label(unpacked_padded) -> <<"unpacked-padded">>.

%% @doc The address a module's packing is bound to, or `undefined' for a
%% packing that is bound to none.
address(Module) ->
    ar_storage_module:module_address(Module).

%% @doc The directory the storage modules live under. An operator points this
%% at the data directory of the Arweave node whose data they are.
data_dir(Opts) ->
    hb_util:list(hb_opts:get(<<"arweave-data-dir">>, <<"arweave-data">>, Opts)).

%% @doc How many bytes of chunk data one chunk file holds. An Arweave node that
%% was run with a different value wrote different files, so this is read rather
%% than assumed.
chunk_group_size(Opts) ->
    hb_util:int(
        hb_opts:get(<<"arweave-chunk-group-size">>, ?CHUNK_GROUP_SIZE, Opts)).

%% @doc The directory one module occupies.
module_path(Module, Opts) ->
    ar_chunk_storage:get_storage_module_path(data_dir(Opts), id(Module)).

%% @doc The directory one module's chunk files occupy.
chunk_dir(Module, Opts) ->
    ar_chunk_storage:get_chunk_storage_path(data_dir(Opts), id(Module)).

%% @doc Read a module out of the identifier its directory is named by. The
%% inverse of `ar_storage_module:id/1', for finding the modules a data
%% directory already holds.
parse_id(StoreID) ->
    case string:split(hb_util:list(StoreID), "storage_module_") of
        ["", Rest] -> parse_fields(string:split(Rest, "_"));
        _ -> not_found
    end.

%%% Internal functions.

%% @doc Read the packing of a configuration entry. A module that names no
%% packing is unpacked, which is the only packing the indexer reads.
entry_packing(Entry, Opts) ->
    case hb_maps:get(<<"packing">>, Entry, <<"unpacked">>, Opts) of
        <<"unpacked">> ->
            unpacked;
        <<"replica-2-9">> ->
            {replica_2_9, entry_address(Entry, Opts)};
        <<"spora-2-6">> ->
            {spora_2_6, entry_address(Entry, Opts)};
        <<"composite">> ->
            {composite,
                entry_address(Entry, Opts),
                hb_util:int(
                    hb_maps:get(<<"packing-difficulty">>, Entry, 1, Opts))
            };
        Format ->
            throw({'unsupported-packing', Format})
    end.

%% @doc The address a module's packing is bound to.
entry_address(Entry, Opts) ->
    case hb_maps:get(<<"address">>, Entry, not_found, Opts) of
        not_found ->
            ar_wallet:to_address(hb_opts:get(priv_wallet, hb:wallet(), Opts));
        Address -> hb_util:native_id(Address)
    end.

%% @doc Read the fields of an identifier: a bucket and a packing, or a bucket
%% size, a bucket and a packing. A module whose bucket size is the partition
%% size names only its bucket, which is the form a miner's modules take.
%%
%% The fields are taken from the left one at a time rather than by splitting on
%% every separator, because the packing carries one: an address is base64url,
%% and about half of all addresses contain an underscore.
parse_fields([Bucket, Rest]) ->
    parse_rest(Bucket, string:split(Rest, "_"));
parse_fields(_Fields) ->
    not_found.

%% @doc Decide which of the two forms an identifier is in. The second field is a
%% bucket only when it is an integer and something follows it; otherwise the
%% whole of the rest is the packing, underscores and all.
parse_rest(Bucket, [Second, Third]) ->
    parse_rest(Bucket, Second, Third, catch hb_util:int(Second));
parse_rest(Bucket, [Packing]) ->
    parsed(ar_block:partition_size(), Bucket, Packing).

parse_rest(BucketSize, Bucket, Packing, Integer) when is_integer(Integer) ->
    parsed(BucketSize, Bucket, Packing);
parse_rest(Bucket, Second, Third, _NotInteger) ->
    parsed(ar_block:partition_size(), Bucket, Second ++ "_" ++ Third).

%% @doc Build the module a set of parsed fields names, or `not_found' for a
%% bucket that is not a number or a packing this node has no name for.
parsed(BucketSize, Bucket, Packing) ->
    case catch hb_util:int(Bucket) of
        Number when is_integer(Number) ->
            parsed_packing(BucketSize, Number, parse_packing(Packing));
        _ ->
            not_found
    end.

parsed_packing(_BucketSize, _Bucket, not_found) ->
    not_found;
parsed_packing(BucketSize, Bucket, Packing) ->
    {hb_util:int(BucketSize), Bucket, Packing}.

%% @doc Read the packing an identifier names. The address is base64url, as it
%% is everywhere else this subsystem spells one.
parse_packing("unpacked") ->
    unpacked;
parse_packing(Packing) ->
    case string:split(Packing, ".", all) of
        [Address, "replica", "2", "9"] ->
            {replica_2_9, hb_util:native_id(hb_util:bin(Address))};
        [Address, Difficulty] ->
            {composite,
                hb_util:native_id(hb_util:bin(Address)),
                hb_util:int(Difficulty)
            };
        [Address] ->
            {spora_2_6, hb_util:native_id(hb_util:bin(Address))};
        _ ->
            not_found
    end.

%%% Tests.

%% @doc Every identifier an Arweave node writes reads back as the module it
%% names, and names itself again.
%%
%% The addresses are the case the naive parse gets wrong: an address is
%% base64url, and about half of all addresses contain the same underscore the
%% identifier separates its fields with. The first is the identifier a real
%% Arweave node wrote for a bucket of the mainnet weave.
parse_id_round_trip_test() ->
    lists:foreach(
        fun(ID) ->
            Module = parse_id(ID),
            ?assertNotEqual(not_found, Module),
            ?assertEqual(ID, id(Module))
        end,
        [
            "storage_module_14063501312_27020_"
                "uaV-x-DGYWgKxGs5AsbOY9GDPWTFOz63Q9kPTal0ePA.replica.2.9",
            "storage_module_27020_"
                "f9RjWid39W__dibIykicpypIsjNh8Y9VkCVnWaeOLzg.replica.2.9",
            "storage_module_14063501312_27020_unpacked",
            "storage_module_3_unpacked",
            "storage_module_5_"
                "f9RjWid39W__dibIykicpypIsjNh8Y9VkCVnWaeOLzg.7",
            "storage_module_5_"
                "f9RjWid39W__dibIykicpypIsjNh8Y9VkCVnWaeOLzg"
        ]
    ).

%% @doc A directory that is not a storage module, or one whose bucket is not a
%% number, names no module -- rather than half of one.
parse_id_refuses_test() ->
    ?assertEqual(not_found, parse_id("not_a_module")),
    ?assertEqual(not_found, parse_id("storage_module_x_unpacked")),
    ?assertEqual(not_found, parse_id("rocksdb")),
    ?assertEqual(not_found, parse_id("storage_module_")).

%% @doc A module of the partition size names only its bucket; any other size
%% names both. The two forms are what tells an operator's directory apart from
%% a miner's.
id_forms_test() ->
    Address = hb_util:native_id(<<"f9RjWid39W__dibIykicpypIsjNh8Y9VkCVnWaeOLzg">>),
    ?assertEqual(
        "storage_module_7_f9RjWid39W__dibIykicpypIsjNh8Y9VkCVnWaeOLzg.replica.2.9",
        id({ar_block:partition_size(), 7, {replica_2_9, Address}})
    ),
    ?assertEqual(
        "storage_module_1048576_7_unpacked",
        id({1048576, 7, unpacked})
    ).
