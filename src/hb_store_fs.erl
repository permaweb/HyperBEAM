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
-export([type/3, read/3, write/3, list/3]).
-export([group/3, link/3, resolve/3]).
-include_lib("kernel/include/file.hrl").
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

%% @doc Read a key from the store, following symlinks as needed.
read(Opts, #{ <<"read">> := Key }, _NodeOpts) ->
    case resolve_path(Opts, Key) of
        {ok, ResolvedPath} ->
            read_path(add_prefix(Opts, ResolvedPath));
        {error, _} = Error ->
            Error
    end.
read_path(Path) ->
	?event({read, Path}),
	case file:read_file_info(Path) of
		{ok, #file_info{type = regular}} ->
			{ok, _} = file:read_file(Path);
        {ok, #file_info{type = directory}} ->
            case file:list_dir(Path) of
                {ok, Files} ->
                    {composite, lists:map(fun hb_util:bin/1, Files)};
                {error, _} ->
                    {error, not_found}
            end;
		_ ->
			case file:read_link(Path) of
				{ok, Link} ->
					?event({link_found, Path, Link}),
					read_path(Link);
				_ ->
					{error, not_found}
			end
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
    ok = file:write_file(Path, Value),
    ok.

%% @doc List contents of a directory in the store.
list(Opts, #{ <<"list">> := Path }, _NodeOpts) ->
    case file:list_dir(add_prefix(Opts, hb_path:to_binary(Path))) of
        {ok, Files} -> {ok, lists:map(fun hb_util:bin/1, Files)};
        {error, _} -> {error, not_found}
    end.

%% @doc Replace links in a path successively, returning the final path.
%% Each element of the path is resolved in turn, with the result of each
%% resolution becoming the prefix for the next resolution. This allows 
%% paths to resolve across many links. For example, a structure as follows:
%%
%%    /a/b/c: "Not the right data"
%%    /a/b -> /a/alt-b
%%    /a/alt-b/c: "Correct data"
%%
%% will resolve "a/b/c" to "Correct data".
resolve(Opts, #{ <<"resolve">> := RawPath }, _NodeOpts) ->
    resolve_path(Opts, RawPath).
resolve_path(Opts, RawPath) ->
    try
        Res = resolve_path(
            Opts,
            <<>>,
            hb_path:term_to_path_parts(hb_path:to_binary(RawPath), Opts)
        ),
        ?event({resolved, RawPath, Res}),
        {ok, Res}
    catch
        throw:{error, _} = Error ->
            Error
    end.
resolve_path(_, CurrPath, []) ->
    hb_path:to_binary(CurrPath);
resolve_path(Opts, CurrPath, [Next|Rest]) ->
    PathPart = hb_path:to_binary([CurrPath, Next]),
    ?event(
        {resolving,
            {accumulated_path, CurrPath},
            {next_segment, Next},
            {generated_partial_path_to_test, PathPart}
        }
    ),
    case file:read_link(add_prefix(Opts, PathPart)) of
        {ok, RawLink} ->
            Link = remove_prefix(Opts, RawLink),
            resolve_path(Opts, Link, Rest);
        {error, enoent} ->
            throw({error, not_found});
        _ ->
            resolve_path(Opts, PathPart, Rest)
    end.

%% @doc Determine the type of a key in the store.
type(Opts, #{ <<"type">> := Key }, _NodeOpts) ->
    type_path(add_prefix(Opts, hb_path:to_binary(Key))).
type_path(Path) ->
    ?event({type, Path}),
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} -> {ok, composite};
        {ok, #file_info{type = regular}} -> {ok, simple};
        _ ->
            case file:read_link(Path) of
                {ok, Link} ->
                    type_path(Link);
                _ ->
                    {error, not_found}
            end
    end.

%% @doc Create a directory (group) in the store.
group(Opts = #{ <<"name">> := _DataDir }, #{ <<"group">> := Path }, _NodeOpts) ->
    P = add_prefix(Opts, hb_path:to_binary(Path)),
    ?event({making_group, P}),
    % We need to ensure that the parent directory exists, so that we can
    % make the group.
    filelib:ensure_dir(P),
   case file:make_dir(P) of
        ok -> ok;
        {error, eexist} -> ok
    end.

%% @doc Create a symlink, handling the case where the link would point to itself.
link(Opts, Req, _NodeOpts) when is_map(Req) ->
    maps:fold(
        fun(New, Existing, ok) ->
            link_path(Opts, Existing, New);
           (_New, _Existing, Error) ->
            Error
        end,
        ok,
        Req
    ).
link_path(_, Link, Link) ->
    ok;
link_path(Opts, Existing, New) ->
    ExistingPath = hb_path:to_binary(Existing),
    NewPath = hb_path:to_binary(New),
    ?event({symlink,
		add_prefix(Opts, ExistingPath),
		P2 = add_prefix(Opts, NewPath)}),
    filelib:ensure_dir(P2),
    case file:make_symlink(add_prefix(Opts, ExistingPath), N = add_prefix(Opts, NewPath)) of
        ok -> ok;
        {error, eexist} ->
            file:delete(N),
            R = file:make_symlink(add_prefix(Opts, ExistingPath), N),
            ?event(debug_fs,
                {symlink_recreated,
                    {existing, ExistingPath},
                    {new, NewPath},
                    {result, R}
                }
            ),
            case R of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Add the directory prefix to a path.
add_prefix(#{ <<"name">> := Prefix }, Path) ->
	?event({add_prefix, Prefix, Path}),
    % Check if the prefix is an absolute path
    IsAbsolute = is_binary(Prefix) andalso binary:first(Prefix) =:= $/ orelse
                 is_list(Prefix) andalso hd(Prefix) =:= $/,
    % Join the paths
    JoinedPath = hb_path:to_binary([Prefix, Path]),
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

%% @doc Remove the directory prefix from a path.
remove_prefix(#{ <<"name">> := Prefix }, Path) ->
    hb_util:remove_common(Path, Prefix).
