-module(hb_volume).
-moduledoc """
Module for managing physical disks and volumes, providing operations
for partitioning, formatting, mounting, and managing encrypted volumes.
""".
-export([list_partitions/0, create_partition/2]).
-export([format_disk/2, mount_disk/4, change_node_store/2]).
-export([check_for_device/1, check_lmdb_exists/1]).
-export([copy_lmdb_store/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-doc """
List available partitions in the system.
@returns {ok, Map} where Map contains the partition information,
         or {error, Reason} if the operation fails.
""".
-spec list_partitions() -> {ok, map()} | {error, binary()}.
list_partitions() ->
    ?event(disk, {list_partitions, start}),
    
    % Get the partition information using fdisk -l
    case os:cmd("sudo fdisk -l") of
        [] ->
            % Empty output indicates an error
            Reason = <<"Failed to list partitions: no output">>,
            ?event(disk, {list_partitions, error, Reason}),
            {error, Reason};
        Output ->
            ?event(disk, {list_partitions, complete}),
            
            % Split output into lines
            Lines = string:split(Output, "\n", all),
            
            % Process the output to group information by disk
            {_, DiskData} = lists:foldl(
                fun process_disk_line/2,
                {undefined, []},
                Lines
            ),
            
            % Process each disk's data to extract all information
            DiskObjects = lists:filtermap(
                fun(DiskEntry) ->
                    Device = maps:get(<<"device">>, DiskEntry),
                    DiskLines = lists:reverse(maps:get(<<"data">>, DiskEntry)),
                    DiskInfo = parse_disk_info(Device, DiskLines),
                    {true, DiskInfo}
                end,
                DiskData
            ),
            
            % Return the partition information
            {ok, #{
                <<"status">> => 200,
                <<"content-type">> => <<"application/json">>,
                <<"body">> => hb_json:encode(#{<<"disks">> => DiskObjects})
            }}
    end.

%%% Helper functions for list_partitions
% Process a line of fdisk output to group by disk
process_disk_line(Line, {CurrentDisk, Acc}) ->
    % Match for a new disk entry
    DiskPattern = "^Disk (/dev/(?!ram)\\S+):",
    case re:run(Line, DiskPattern, [{capture, [1], binary}]) of
        {match, [Device]} ->
            % Start a new disk entry
            NewDisk = #{
                <<"device">> => Device,
                <<"data">> => [Line]
            },
            {NewDisk, [NewDisk | Acc]};
        _ when CurrentDisk =:= undefined ->
            % Not a disk line and no current disk
            {undefined, Acc};
        _ ->
            % Add line to current disk's data
            CurrentData = maps:get(<<"data">>, CurrentDisk),
            UpdatedDisk = CurrentDisk#{
                <<"data">> => [Line | CurrentData]
            },
            % Update the list with the modified disk entry
            UpdatedAcc = [UpdatedDisk | lists:delete(CurrentDisk, Acc)],
            {UpdatedDisk, UpdatedAcc}
    end.

% Parse detailed disk information from fdisk output lines
parse_disk_info(Device, Lines) ->
    % Initialize with device ID
    DiskInfo = #{<<"device">> => Device},
    
    % Process each line to extract information
    lists:foldl(
        fun parse_disk_line/2,
        DiskInfo,
        Lines
    ).

% Parse a single line of disk information
parse_disk_line(Line, Info) ->
    % Extract disk size and bytes
    SizePattern = "^Disk .+: ([0-9.]+ [KMGT]iB), ([0-9]+) bytes, ([0-9]+) sectors",
    case re:run(Line, SizePattern, [{capture, [1, 2, 3], binary}]) of
        {match, [Size, Bytes, Sectors]} ->
            Info#{
                <<"size">> => Size,
                <<"bytes">> => binary_to_integer(Bytes),
                <<"sectors">> => binary_to_integer(Sectors)
            };
        _ -> 
            parse_disk_model_line(Line, Info)
    end.

% Parse disk model information
parse_disk_model_line(Line, Info) ->
    % Extract disk model
    ModelPattern = "^Disk model: (.+)\\s*$",
    case re:run(Line, ModelPattern, [{capture, [1], binary}]) of
        {match, [Model]} ->
            Info#{<<"model">> => string:trim(Model)};
        _ ->
            parse_disk_units_line(Line, Info)
    end.

% Parse disk units information
parse_disk_units_line(Line, Info) ->
    % Extract units information
    UnitsPattern = "^Units: (.+)$",
    case re:run(Line, UnitsPattern, [{capture, [1], binary}]) of
        {match, [Units]} ->
            Info#{<<"units">> => Units};
        _ ->
            parse_sector_size_line(Line, Info)
    end.

% Parse sector size information
parse_sector_size_line(Line, Info) ->
    % Extract sector size
    SectorPattern = "^Sector size \\(logical/physical\\): ([^/]+)/(.+)$",
    case re:run(Line, SectorPattern, [{capture, [1, 2], binary}]) of
        {match, [LogicalSize, PhysicalSize]} ->
            Info#{
                <<"sector_size">> => #{
                    <<"logical">> => string:trim(LogicalSize),
                    <<"physical">> => string:trim(PhysicalSize)
                }
            };
        _ ->
            parse_io_size_line(Line, Info)
    end.

% Parse I/O size information
parse_io_size_line(Line, Info) ->
    % Extract I/O size
    IOPattern = "^I/O size \\(minimum/optimal\\): ([^/]+)/(.+)$",
    case re:run(Line, IOPattern, [{capture, [1, 2], binary}]) of
        {match, [MinSize, OptSize]} ->
            Info#{
                <<"io_size">> => #{
                    <<"minimum">> => string:trim(MinSize),
                    <<"optimal">> => string:trim(OptSize)
                }
            };
        _ -> 
            Info
    end.

-doc """
Create a partition on a disk device.
@param Device The path to the device, e.g. "/dev/sdb".
@param PartType The partition type to create, defaults to "ext4".
@returns {ok, Map} on success where Map includes status and partition information,
         or {error, Reason} if the operation fails.
""".
-spec create_partition(Device :: binary(), PartType :: binary()) ->
    {ok, map()} | {error, binary()}.
create_partition(undefined, _PartType) ->
    {error, <<"Device path not specified">>};
create_partition(Device, PartType) ->
    ?event(disk, {create_partition, start}),
    ?event(disk, {create_partition, device, Device}),
    ?event(disk, {create_partition, part_type, PartType}),
    % Create a GPT partition table
    DeviceStr = binary_to_list(Device),
    MklabelCmd = "sudo parted " ++ DeviceStr ++ " mklabel gpt",
    case safe_exec(MklabelCmd) of
        {ok, _Result} ->
            create_actual_partition(Device, PartType);
        {error, ErrorMsg} ->
            ?event(disk, {create_partition, error, ErrorMsg}),
            {error, ErrorMsg}
    end.

% Create the actual partition after making the GPT label
create_actual_partition(Device, PartType) ->
    DeviceStr = binary_to_list(Device),
    PartTypeStr = binary_to_list(PartType),
    % Build the parted command to create the partition
    MkpartCmd = "sudo parted -a optimal " ++ DeviceStr ++ 
               " mkpart primary " ++ PartTypeStr ++ " 0% 100%",
    case safe_exec(MkpartCmd) of
        {ok, _Result} ->
            get_partition_info(Device);
        {error, ErrorMsg} ->
            ?event(disk, {create_partition, error, ErrorMsg}),
            {error, ErrorMsg}
    end.

% Get the partition information after creating a partition
get_partition_info(Device) ->
    DeviceStr = binary_to_list(Device),
    % Print partition information
    PrintCmd = "sudo parted " ++ DeviceStr ++ " print",
    PartitionInfo = os:cmd(PrintCmd),
    ?event(disk, {create_partition, complete}),
    {ok, #{
        <<"status">> => 200,
        <<"message">> => <<"Partition created successfully.">>,
        <<"device_path">> => Device,
        <<"partition_info">> => list_to_binary(PartitionInfo)
    }}.

-doc """
Format a disk or partition with LUKS encryption.
@param Partition The path to the partition, e.g. "/dev/sdc1".
@param EncKey The encryption key to use for LUKS.
@returns {ok, Map} on success where Map includes the status and confirmation message,
         or {error, Reason} if the operation fails.
""".
-spec format_disk(Partition :: binary(), EncKey :: binary()) ->
    {ok, map()} | {error, binary()}.
format_disk(undefined, _EncKey) ->
    {error, <<"Partition path not specified">>};
format_disk(_Partition, undefined) ->
    {error, <<"Encryption key not specified">>};
format_disk(Partition, EncKey) ->
    ?event(disk, {format, start}),
    ?event(disk, {format, partition, Partition}),
    PartitionStr = binary_to_list(Partition),
    with_secure_key_file(EncKey, fun(KeyFile) ->
        FormatCmd = "sudo cryptsetup luksFormat --batch-mode --key-file " ++ 
                    KeyFile ++ " " ++ PartitionStr,
        case safe_exec(FormatCmd, ["failed"]) of
            {ok, _Result} ->
                ?event(disk, {format, complete}),
                {ok, #{
                    <<"status">> => 200,
                    <<"message">> => 
                        <<"Partition formatted with LUKS encryption successfully.">>
                }};
            {error, ErrorMsg} ->
                ?event(disk, {format, error, ErrorMsg}),
                {error, ErrorMsg}
        end
    end).

-doc """
Mount a LUKS-encrypted disk.
@param Partition The path to the partition, e.g. "/dev/sdc1".
@param EncKey The encryption key for LUKS.
@param MountPoint The directory where the disk should be mounted.
@param VolumeName The name to use for the decrypted LUKS volume.
@returns {ok, Map} on success where Map includes the status and confirmation message,
         or {error, Reason} if the operation fails.
""".
-spec mount_disk(
    Partition :: binary(),
    EncKey :: binary(),
    MountPoint :: binary(),
    VolumeName :: binary()
) -> {ok, map()} | {error, binary()}.
mount_disk(undefined, _EncKey, _MountPoint, _VolumeName) ->
    {error, <<"Partition path not specified">>};
mount_disk(_Partition, undefined, _MountPoint, _VolumeName) ->
    {error, <<"Encryption key not specified">>};
mount_disk(_Partition, _EncKey, undefined, _VolumeName) ->
    {error, <<"Mount point not specified">>};
mount_disk(Partition, EncKey, MountPoint, VolumeName) ->
    ?event(disk, {mount, start}),
    ?event(disk, {mount, partition, Partition}),
    ?event(disk, {mount, mount_point, MountPoint}),
    ?event(disk, {mount, volume_name, VolumeName}),
    PartitionStr = binary_to_list(Partition),
    VolumeNameStr = binary_to_list(VolumeName),
    with_secure_key_file(EncKey, fun(KeyFile) ->
        OpenCmd = "sudo cryptsetup luksOpen --key-file " ++ KeyFile ++ " " ++ 
                   PartitionStr ++ " " ++ VolumeNameStr,
        case safe_exec(OpenCmd, ["failed"]) of
            {ok, _Result} ->
                mount_opened_volume(Partition, MountPoint, VolumeName);
            {error, ErrorMsg} ->
                ?event(disk, {mount, error, ErrorMsg}),
                {error, ErrorMsg}
        end
    end).

% Mount an already opened LUKS volume
mount_opened_volume(Partition, MountPoint, VolumeName) ->
    % Create mount point if it doesn't exist
    MountPointStr = binary_to_list(MountPoint),
    os:cmd("sudo mkdir -p " ++ MountPointStr),
    % Mount the unlocked LUKS volume
    VolumeNameStr = binary_to_list(VolumeName),
    MountCmd = "sudo mount /dev/mapper/" ++ VolumeNameStr ++ " " ++ 
                MountPointStr,
    case safe_exec(MountCmd, ["failed"]) of
        {ok, _Result} ->
            create_mount_info(Partition, MountPoint, VolumeName);
        {error, ErrorMsg} ->
            % Close the LUKS volume if mounting failed
            os:cmd("sudo cryptsetup luksClose " ++ VolumeNameStr),
            ?event(disk, {mount, error, ErrorMsg}),
            {error, ErrorMsg}
    end.

% Create mount info response
create_mount_info(Partition, MountPoint, VolumeName) ->
    ?event(disk, {mount, complete}),
    {ok, #{
        <<"status">> => 200,
        <<"message">> => <<"Encrypted partition mounted successfully.">>,
        <<"mount_point">> => MountPoint,
        <<"mount_info">> => #{
            partition => Partition,
            mount_point => MountPoint,
            volume_name => VolumeName
        }
    }}.

-doc """
Change the node's data store location to the mounted encrypted disk.
@param StorePath The new path for the store directory.
@param CurrentStore The current store configuration.
@returns {ok, Map} on success where Map includes the status and confirmation message,
         or {error, Reason} if the operation fails.
""".
-spec change_node_store(StorePath :: binary(), CurrentStore :: list()) ->
    {ok, map()} | {error, binary()}.
change_node_store(undefined, _CurrentStore) ->
    {error, <<"Store path not specified">>};
change_node_store(StorePath, CurrentStore) ->
    ?event(disk, {change_store, start}),
    ?event(disk, {change_store, store_path, StorePath}),
    % Create the store directory if it doesn't exist
    StorePathStr = binary_to_list(StorePath),
    os:cmd("sudo mkdir -p " ++ StorePathStr),
    % Update the store configuration with the new path
    NewStore = update_store_config(CurrentStore, StorePath),
    % Return the result
    ?event(disk, {change_store, complete}),
    {ok, #{
        <<"status">> => 200,
        <<"message">> => <<"Node store updated to use encrypted disk.">>,
        <<"store_path">> => StorePath,
        <<"store">> => NewStore
    }}.

%%% Helper functions
%% Execute system command with error checking
safe_exec(Command) ->
    safe_exec(Command, ["Error", "failed"]).

safe_exec(Command, ErrorKeywords) ->
    Result = os:cmd(Command),
    case check_command_errors(Result, ErrorKeywords) of
        ok -> {ok, Result};
        error -> {error, list_to_binary(Result)}
    end.

%% Check if command result contains error indicators
check_command_errors(Result, Keywords) ->
    case lists:any(fun(Keyword) -> 
        string:find(Result, Keyword) =/= nomatch 
    end, Keywords) of
        true -> error;
        false -> ok
    end.

%% Secure key file management with automatic cleanup
with_secure_key_file(EncKey, Fun) ->
    % Ensure tmp directory exists
    os:cmd("sudo mkdir -p /root/tmp"),
    KeyFile = "/root/tmp/luks_key_" ++ os:getpid(),
    try
        % Write key to temporary file
        file:write_file(KeyFile, EncKey, [raw]),
        % Execute function with key file path
        Result = Fun(KeyFile),
        % Always clean up the key file
        os:cmd("sudo shred -u " ++ KeyFile),
        Result
    catch
        Class:Reason:Stacktrace ->
            % Ensure cleanup even if function fails
            os:cmd("sudo shred -u " ++ KeyFile),
            erlang:raise(Class, Reason, Stacktrace)
    end.

% Update the store configuration with a new base path
-spec update_store_config(StoreConfig :: term(), NewPath :: binary()) -> term().
update_store_config(StoreConfig, NewPath) when is_list(StoreConfig) ->
    % For a list, update each element
    [update_store_config(Item, NewPath) || Item <- StoreConfig];
update_store_config(#{<<"store-module">> := Module} = StoreConfig, NewPath) 
  when is_map(StoreConfig) ->
    % Handle various store module types differently
    case Module of
        hb_store_fs ->
            % For filesystem store, prefix the existing path with the new path
            ExistingPath = maps:get(<<"name">>, StoreConfig, <<"">>),
            NewName = <<NewPath/binary, "/", ExistingPath/binary>>,
            ?event(debug_volume, {fs, StoreConfig, NewPath, NewName}),
            StoreConfig#{<<"name">> => NewName};
        hb_store_lmdb ->
            update_lmdb_store_config(StoreConfig, NewPath);
        hb_store_rocksdb ->
            StoreConfig;
        hb_store_gateway ->
            % For gateway store, recursively update nested store configurations
            NestedStore = maps:get(<<"store">>, StoreConfig, []),
            StoreConfig#{
                <<"store">> => update_store_config(NestedStore, NewPath)
            };
        _ ->
            % For any other store type, update the prefix
            % StoreConfig#{<<"name">> => NewPath}
            ?event(debug_volume, {other, StoreConfig, NewPath}),
            StoreConfig
    end;
update_store_config({Type, _OldPath, Opts}, NewPath) ->
    % For tuple format with options
    {Type, NewPath, Opts};
update_store_config({Type, _OldPath}, NewPath) ->
    % For tuple format without options
    {Type, NewPath};
update_store_config(StoreConfig, _NewPath) ->
    % Return unchanged for any other format
    StoreConfig.

%% Handle LMDB store migration to new encrypted mount location
update_lmdb_store_config(StoreConfig, NewPath) ->
    ExistingPath = maps:get(<<"name">>, StoreConfig, <<"">>),
    NewName = <<NewPath/binary, "/", ExistingPath/binary>>,
    ?event(debug_volume, {migrate_start, ExistingPath, NewName}),
    safe_stop_lmdb_store(StoreConfig),
    FinalConfig = handle_lmdb_migration(StoreConfig, ExistingPath, NewName),
    safe_start_lmdb_store(FinalConfig),
    FinalConfig.

%% Safely stop LMDB store with error handling
safe_stop_lmdb_store(StoreConfig) ->
    ?event(debug_volume, {stopping_current_store, StoreConfig}),
    try 
        hb_store_lmdb:stop(StoreConfig)
    catch 
        error:StopReason ->
            ?event(debug_volume, {stop_error, StopReason})
    end.

%% Handle migration destination logic
handle_lmdb_migration(StoreConfig, ExistingPath, NewName) ->
    case check_lmdb_exists(NewName) of
        true ->
            use_existing_lmdb_store(StoreConfig, NewName);
        false ->
            copy_lmdb_store_data(StoreConfig, ExistingPath, NewName)
    end.

%% Use existing LMDB store at new location
use_existing_lmdb_store(StoreConfig, NewName) ->
    ?event(debug_volume, {using_existing_store, NewName}),
    StoreConfig#{<<"name">> => NewName}.

%% Copy LMDB store data to new location with fallback
copy_lmdb_store_data(StoreConfig, ExistingPath, NewName) ->
    ?event(debug_volume, {copying_store, ExistingPath, NewName}),
    case copy_lmdb_store(ExistingPath, NewName) of
        ok ->
            ?event(debug_volume, {copy_success, NewName}),
            StoreConfig#{<<"name">> => NewName};
        {error, Reason} ->
            ?event(debug_volume, {copy_error, Reason}),
            ?event(debug_volume, {fallback_to_original, ExistingPath}),
            StoreConfig
    end.

%% Safely start LMDB store
safe_start_lmdb_store(StoreConfig) ->
    NewName = maps:get(<<"name">>, StoreConfig),
    ?event(debug_volume, {starting_new_store, NewName}),
    hb_store_lmdb:start(StoreConfig).

-doc """
Check if a device exists on the system.
@param Device The path to the device to check (binary).
@returns true if the device exists, false otherwise.
""".
-spec check_for_device(Device :: binary()) -> boolean().
check_for_device(Device) ->
    Command = io_lib:format(
        "ls -l ~s 2>/dev/null || echo 'not_found'", 
        [binary_to_list(Device)]
    ),
	?event(disk, {check_for_device, command, Command}),
    Result = os:cmd(Command),
    string:find(Result, "not_found") =:= nomatch.

%% @doc Check if an LMDB database exists at the given path.
%%
%% LMDB databases consist of at least a data.mdb file and optionally a lock.mdb file.
%% This function checks for the presence of these files to determine if a valid
%% LMDB database exists at the specified location.
%%
%% @param Path Binary path to check for LMDB database
%% @returns true if LMDB database exists, false otherwise
-spec check_lmdb_exists(Path :: binary()) -> boolean().
check_lmdb_exists(Path) ->
    PathStr = binary_to_list(Path),
    DataFile = PathStr ++ "/data.mdb",    
    % Check if the directory exists first
    case filelib:is_dir(PathStr) of
        false -> 
            false;
        true ->
            % Check if data.mdb exists (required for LMDB)
            % lock.mdb is optional and might not exist when DB is not in use
            filelib:is_regular(DataFile)
    end.

%% @doc Copy an LMDB database from source to destination.
%%
%% This function performs a safe copy of an LMDB database by copying all
%% the database files (data.mdb, lock.mdb if present) from the source
%% directory to the destination directory. It ensures the destination
%% directory exists before copying.
%%
%% @param SourcePath Binary path to source LMDB database
%% @param DestPath Binary path to destination location
%% @returns ok on success, {error, Reason} on failure
-spec copy_lmdb_store(
    SourcePath :: binary(), 
    DestPath :: binary()
) -> ok | {error, term()}.
copy_lmdb_store(SourcePath, DestPath) ->
    SourceStr = binary_to_list(SourcePath),
    DestStr = binary_to_list(DestPath),
    ?event(debug_volume, {start, SourceStr, DestStr}),
    % Check if source LMDB database exists
    case check_lmdb_exists(SourcePath) of
        false ->
            ?event(debug_volume, {source_not_found, SourceStr}),
            {error, <<"Source LMDB database not found">>};
        true ->
            % Ensure destination directory exists
            ok = filelib:ensure_dir(DestStr ++ "/"),
            
            % Copy the LMDB database files
            case copy_lmdb_files(SourceStr, DestStr) of
                ok ->
                    ?event(debug_volume, {success}),
                    ok;
                {error, Reason} ->
                    ?event(debug_volume, {error, Reason}),
                    {error, Reason}
            end
    end.

%% @doc Helper function to copy LMDB database files.
%%
%% This function copies the actual LMDB files (data.mdb and lock.mdb if present)
%% from source to destination using system commands for reliability.
%%
%% @param SourceDir String path to source directory
%% @param DestDir String path to destination directory
%% @returns ok on success, {error, Reason} on failure
-spec copy_lmdb_files(
    SourceDir :: string(), 
    DestDir :: string()
) -> ok | {error, term()}.
copy_lmdb_files(SourceDir, DestDir) ->
    % Use rsync for reliable copying of LMDB files
    % --archive preserves permissions and timestamps
    % --sparse handles sparse files efficiently
    CopyCommand = io_lib:format(
        "rsync -av --sparse '~s/' '~s/'", 
        [SourceDir, DestDir]
    ),
    ?event(debug_volume, {copy_lmdb_files, CopyCommand}),
    case os:cmd(CopyCommand) of
        Result ->
            % Check if rsync completed successfully by looking for error indicators
            case {
                    string:find(Result, "rsync error"), 
                    string:find(Result, "failed")
                } of
                {nomatch, nomatch} ->
                    ok;
                _ ->
                    {error, list_to_binary(Result)}
            end
    end.

%%% Unit Tests
%% Test helper function error checking
check_command_errors_test() ->
    % Test successful case - no errors
    ?assertEqual(ok, check_command_errors("Success: operation completed", ["Error", "failed"])),
    % Test error detection
    ?assertEqual(error, check_command_errors("Error: something went wrong", ["Error", "failed"])),
    ?assertEqual(error, check_command_errors("Operation failed", ["Error", "failed"])),
    % Test case sensitivity
    ?assertEqual(ok, check_command_errors("error (lowercase)", ["Error", "failed"])),
    % Test multiple keywords
    ?assertEqual(error, check_command_errors("Command failed with Error", ["Error", "failed"])).

%% Test LMDB existence checking
check_lmdb_exists_test() ->
    % Create temporary test directories
    TestDir = "/tmp/hb_volume_test_" ++ integer_to_list(erlang:system_time()),
    TestDirBin = list_to_binary(TestDir),
    % Clean up function
    Cleanup = fun() -> os:cmd("rm -rf " ++ TestDir) end,
    try
        % Test non-existent directory
        ?assertEqual(false, check_lmdb_exists(TestDirBin)),
        % Create directory but no LMDB files
        ok = filelib:ensure_dir(TestDir ++ "/"),
        ?assertEqual(false, check_lmdb_exists(TestDirBin)),
        % Create data.mdb file
        DataFile = TestDir ++ "/data.mdb",
        file:write_file(DataFile, <<"test data">>, [raw]),
        ?assertEqual(true, check_lmdb_exists(TestDirBin)),
        % Clean up
        Cleanup()
    after
        Cleanup()
    end.

%% Test store configuration updates for different types
update_store_config_test() ->
    % Test filesystem store
    FSStore = #{
        <<"store-module">> => hb_store_fs,
        <<"name">> => <<"cache">>
    },
    NewPath = <<"/encrypted/mount">>,
    Updated = update_store_config(FSStore, NewPath),
    Expected = FSStore#{<<"name">> => <<"/encrypted/mount/cache">>},
    ?assertEqual(Expected, Updated),
    % Test list of stores
    StoreList = [FSStore, #{<<"store-module">> => hb_store_gateway}],
    UpdatedList = update_store_config(StoreList, NewPath),
    ?assertEqual(2, length(UpdatedList)),
    % Test tuple format
    TupleStore = {fs, <<"old_path">>, []},
    UpdatedTuple = update_store_config(TupleStore, NewPath),
    ?assertEqual({fs, NewPath, []}, UpdatedTuple).

%% Test secure key file management
with_secure_key_file_test() ->
    TestKey = <<"test_encryption_key_123">>,
    % Create a safe test version that doesn't use /root/tmp
    TestWithSecureKeyFile = fun(EncKey, Fun) ->
        % Use /tmp instead of /root/tmp for testing
        TmpDir = "/tmp",
        KeyFile = TmpDir ++ "/test_luks_key_" ++ os:getpid(),
        try
            % Write key to temporary file
            file:write_file(KeyFile, EncKey, [raw]),
            % Execute function with key file path
            Result = Fun(KeyFile),
            % Clean up the key file
            file:delete(KeyFile),
            Result
        catch
            Class:Reason:Stacktrace ->
                % Ensure cleanup even if function fails
                file:delete(KeyFile),
                erlang:raise(Class, Reason, Stacktrace)
        end
    end,
    % Test successful execution
    Result = TestWithSecureKeyFile(TestKey, fun(KeyFile) ->
        % Verify key file was created and contains the key
        ?assert(filelib:is_regular(KeyFile)),
        {ok, FileContent} = file:read_file(KeyFile),
        ?assertEqual(TestKey, FileContent),
        {ok, <<"success">>}
    end),
    ?assertEqual({ok, <<"success">>}, Result),
    % Test exception handling and cleanup
    TestException = fun() ->
        TestWithSecureKeyFile(TestKey, fun(KeyFile) ->
            ?assert(filelib:is_regular(KeyFile)),
            error(test_error)
        end)
    end,
    ?assertError(test_error, TestException()).

%% Test device checking with mocked commands
check_for_device_test() ->
    % This test would need mocking of os:cmd to be fully testable
    % For now, test with /dev/null which should always exist
    ?assertEqual(true, check_for_device(<<"/dev/null">>)),
    % Test non-existent device
    ?assertEqual(false, check_for_device(<<"/dev/nonexistent_device_123">>)).

%% Test safe command execution with mocked results
safe_exec_mock_test() ->
    % We can't easily mock os:cmd, but we can test the error checking logic
    % This is covered by check_command_errors_test above
    % Test with default error keywords
    TestResult1 = check_command_errors("Operation completed successfully", ["Error", "failed"]),
    ?assertEqual(ok, TestResult1),
    TestResult2 = check_command_errors("Error: disk not found", ["Error", "failed"]),
    ?assertEqual(error, TestResult2). 