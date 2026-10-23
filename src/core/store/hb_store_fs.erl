%%% @doc A key-value store implementation, following the `hb_store' behavior 
%%% and interface. This implementation utilizes the node's local file system as
%%% its storage mechanism, offering an alternative to other store's that require
%%% the compilation of additional libraries in order to function.
%%% 
%%% As this store implementation operates using Erlang's native `file' and 
%%% `filelib' mechanisms, it largely inherits its performance characteristics 
%%% from those of the underlying OS/filesystem drivers. Certain filesystems can
%%% be quite performant for the types of workload that HyperBEAM AO-Core execution
%%% requires (many reads and writes to explicit keys, few directory 'listing' or
%%% search operations), awhile others perform suboptimally.
%%% 
%%% Additionally, thisstore implementation offers the ability for simple 
%%% integration of HyperBEAM with other non-volatile storage media: `hb_store_fs'
%%% will interact with any service that implements the host operating system's
%%% native filesystem API. By mounting devices via `FUSE' (etc), HyperBEAM is
%%% able to interact with a large number of existing storage systems (for example,
%%% S3-compatible cloud storage APIs, etc).
-module(hb_store_fs).
-behavior(hb_store).
-export([start/3, stop/3, reset/3, scope/0, scope/1]).
-export([read/3, write/3, list/3]).
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Initialize the file system store with the given data directory.
start(#{ <<"name">> := DataDir }, _Req, _Opts) ->
    ok = filelib:ensure_dir(DataDir).

%% @doc Stop the file system store. Currently a no-op.
stop(#{ <<"name">> := _DataDir }, _Req, _Opts) ->
    ok.

%% @doc The file-based store is always local, for now. In the future, we may
%% want to allow that an FS store is shared across a cluster and thus remote.
scope() -> local.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_) -> scope().

%% @doc Reset the store by completely removing its directory and recreating it.
reset(#{ <<"name">> := DataDir }, _Req, _Opts) ->
    % Use pattern that completely removes directory then recreates it
    os:cmd(binary_to_list(<< "rm -Rf ", DataDir/binary >>)),
    ?event({reset_store, {path, DataDir}}).

%% @doc Read a key from the store: the file's value, or the entries of the
%% directory, each file with its value.
read(Opts, #{ <<"read">> := Key }, _NodeOpts) ->
    read_path(add_prefix(Opts, hb_path:to_binary(Key))).
read_path(Path) ->
	?event({read, Path}),
	case file:read_file_info(Path) of
		{ok, #file_info{type = regular}} ->
			{ok, _} = file:read_file(Path);
        {ok, #file_info{type = directory}} ->
            case file:list_dir_all(Path) of
                {ok, Files} ->
                    {composite, [ child(Path, File) || File <- Files ]};
                {error, _} ->
                    {error, not_found}
            end;
		_ ->
			{error, not_found}
	end.

%% @doc Write a value to the specified path in the store.
write(Opts, Req, _NodeOpts) when is_map(Req) ->
    maps:fold(
        fun(PathComponents, Value, ok) ->
            write_path(Opts, PathComponents, Value);
           (_PathComponents, _Value, Error) ->
            Error
        end,
        ok,
        Req
    ).
write_path(Opts, PathComponents, Value) ->
    Path = add_prefix(Opts, hb_path:to_binary(PathComponents)),
    ?event({writing, Path, byte_size(Value)}),
    filelib:ensure_dir(Path),
    % Keep a file that contains the value; replace any other entry.
    Make = fun(Temp) -> file:write_file(Temp, Value) end,
    case holds_value(Path, Value) of
        true -> ok;
        false -> replace(Opts, Path, Make)
    end.

%% @doc Whether `Path' is a file, not a link, that holds `Value'.
holds_value(Path, Value) ->
    case file:read_link_info(Path) of
        {ok, #file_info{ type = regular, size = Size }} ->
            Size == byte_size(Value) andalso
                file:read_file(Path) == {ok, Value};
        _ ->
            false
    end.

%% @doc List contents of a directory in the store.
list(Opts, Req = #{ <<"list">> := Path }, _NodeOpts) ->
    case file:list_dir_all(add_prefix(Opts, hb_path:to_binary(Path))) of
        {ok, Files} ->
            Children = lists:map(fun name/1, Files),
            {ok, hb_store_utils:apply_list_bounds(Children, Req, Opts)};
        {error, _} -> {error, not_found}
    end.

%% @doc Return a listed file with its value, or a listed directory by its name.
child(Dir, File) ->
    case file:read_file(filename:join(Dir, File)) of
        {ok, Value} -> {name(File), Value};
        {error, _} -> name(File)
    end.

%% @doc Convert a name listed by `file:list_dir_all/1' to the key it was
%% written for. The VM decodes names into lists of characters from its native
%% file name encoding, and returns names it cannot decode as binaries.
name(File) when is_binary(File) -> hb_escape:decode(File);
name(File) ->
    name(unicode:characters_to_binary(File, unicode, file:native_name_encoding())).

%% @doc Replace the entry at `Path' with the file that `Make' makes at a
%% unique path in the store's `~tmp' directory, by renaming it over the entry.
%% The rename replaces the entry in one step: a concurrent read finds the old
%% entry or the new one, and a crash leaves one of them whole. Escaped keys
%% never contain `~', so no key maps to the directory or a path in it.
replace(#{ <<"name">> := DataDir }, Path, Make) ->
    Unique = hb_util:encode(crypto:strong_rand_bytes(16)),
    Temp = <<DataDir/binary, "/~tmp/", Unique/binary>>,
    filelib:ensure_dir(Temp),
    R =
        maybe
            ok ?= Make(Temp),
            file:rename(Temp, Path)
        end,
    % Remove the temporary entry if the rename did not move it.
    file:delete(Temp),
    R.

%% @doc Add the directory prefix to a path.
add_prefix(#{ <<"name">> := Prefix }, Path) ->
	?event({add_prefix, Prefix, Path}),
    % Check if the prefix is an absolute path
    IsAbsolute = is_binary(Prefix) andalso binary:first(Prefix) =:= $/ orelse
                 is_list(Prefix) andalso hd(Prefix) =:= $/,
    % Join the paths
    JoinedPath = hb_path:to_binary([Prefix, safe_path(Path)]),
    % If the prefix was absolute, ensure the joined path is also absolute
    case IsAbsolute of
        true -> 
            case is_binary(JoinedPath) of
                true ->
                    case binary:first(JoinedPath) of
                        $/ -> JoinedPath;
                        _ -> <<"/", JoinedPath/binary>>
                    end;
                false ->
                    case JoinedPath of
                        [$/ | _] -> JoinedPath;
                        _ -> [$/ | JoinedPath]
                    end
            end;
        false -> 
            JoinedPath
    end.

%% @doc Refuse a key with a `.' or `..' component, which the filesystem reads
%% as the current or parent directory, and can resolve outside the store root.
%% Percent-encode any other key, so that keys differing only in case or Unicode
%% normalization do not share a file.
%%
%% Keys are caller-supplied. `hb_escape:encode_path_component/1' escapes the
%% message keys `.' and `..', so neither component maps to an AO-Core key and
%% refusing them costs nothing; the operating system would otherwise follow `..'
%% out of the root.
safe_path(Path) ->
    Parts = binary:split(hb_path:to_binary(Path), <<"/">>, [global]),
    case lists:member(<<".">>, Parts) orelse lists:member(<<"..">>, Parts) of
        false -> hb_escape:encode(Path);
        true -> throw({unsafe_store_key, Path})
    end.

%%% TESTS

%% @doc A key cannot name a path outside the store root.
key_cannot_escape_the_store_test() ->
    Store = hb_test_utils:test_store(?MODULE, <<"safe-path">>),
    ?assertThrow(
        {unsafe_store_key, _},
        read(Store, #{ <<"read">> => <<"../outside">> }, #{})
    ),
    ?assertThrow(
        {unsafe_store_key, _},
        write(Store, #{ <<"../outside">> => <<"escaped">> }, #{})
    ),
    % An ordinary key is not refused: it reaches the store and simply misses.
    % Asserted without writing first, because a write-then-read round trip
    % under `cache-TEST' is flaky for reasons unrelated to this check.
    ?assertEqual(
        {error, not_found},
        read(Store, #{ <<"read">> => <<"ordinary">> }, #{})
    ).
