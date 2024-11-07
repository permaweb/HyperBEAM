-module(ao_fs_store).
-behavior(ao_store).
-export([start/1, stop/1, reset/1]).
-export([type/2, read/2, write/3, list/2]).
-export([path/2, add_path/3, join/1]).
-export([make_group/2, make_link/3, resolve/2]).
-include_lib("kernel/include/file.hrl").
-include("include/ao.hrl").

%%% A key-value store abstraction, such that the underlying implementation
%%% can be swapped out easily. The default implementation is a file-based
%%% store.

start(#{dir := DataDir}) ->
	ok = filelib:ensure_dir(DataDir).

stop(#{dir := _DataDir}) ->
	ok.

reset(#{dir := DataDir}) ->
	os:cmd("rm -Rf " ++ DataDir).

%% @doc Read a key from the store, following symlinks as needed.
read(Opts = #{dir := DataDir}, Key) ->
	case read(join([DataDir, Key])) of
		not_found ->
			case resolve(Opts, Key) of
				Key -> not_found;
				ResolvedPath -> read(ResolvedPath)
			end;
		Result ->
			Result
	end.
read(Path) ->
	?c({read, Path}),
	case file:read_file_info(Path) of
		{ok, #file_info{type = regular}} ->
			{ok, _} = file:read_file(Path);
		_ ->
			case file:read_link(Path) of
				{ok, Link} ->
					read(Link);
				_ ->
					not_found
			end
	end.

write(#{dir := DataDir}, PathComponents, Value) ->
	Path = join([DataDir, PathComponents]),
	?c({writing, Path, byte_size(Value)}),
	filelib:ensure_dir(Path),
	ok = file:write_file(Path, Value).

list(#{dir := DataDir}, Path) ->
	%?c({listing, join([DataDir, Path])}),
	file:list_dir(join([DataDir, Path])).

%% @doc Replace links in a path with the target of the link.
resolve(Opts = #{dir := DataDir}, RawPath) ->
	LinkedPathWithDataDir = resolve(Opts, "", filename:split(join(RawPath))),
	NewPath = ar_util:remove_common(LinkedPathWithDataDir, DataDir),
	NewPath.
resolve(#{dir := DataDir}, CurrPath, []) ->
	join([DataDir, CurrPath]);
resolve(Opts = #{dir := DataDir}, CurrPath, [Next | Rest]) ->
	PathPart = join([CurrPath, Next]),
	case file:read_link(join([DataDir, PathPart])) of
		{ok, Link} ->
			resolve(Opts#{dir := Link}, "", Rest);
		_ ->
			resolve(Opts, PathPart, Rest)
	end.

type(#{dir := DataDir}, Key) ->
	type(join([DataDir, Key])).
type(Path) ->
	case file:read_file_info(Joint = join(Path)) of
		{ok, #file_info{type = directory}} ->
			composite;
		{ok, #file_info{type = regular}} ->
			simple;
		_ ->
			case file:read_link(Joint) of
				{ok, Link} ->
					type(Link);
				_ ->
					not_found
			end
	end.

make_group(#{dir := DataDir}, Path) ->
	P = join([DataDir, Path]),
	%?c({mkdir, P}),
	ok = filelib:ensure_dir(P).

make_link(_, Link, Link) ->
	ok;
make_link(#{dir := DataDir}, Existing, New) ->
	?c({symlink, join([DataDir, Existing]), P2 = join([DataDir, New])}),
	filelib:ensure_dir(P2),
	file:make_symlink(
		join([DataDir, Existing]),
		join([DataDir, New])
	).

%% @doc Create a path from a list of path components.
path(#{dir := _DataDir}, Path) ->
	Path.

%% @doc Add two path components together.
add_path(#{dir := _DataDir}, Path1, Path2) ->
	Path1 ++ Path2.

join([]) -> [];
join(Path) when is_binary(Path) -> Path;
join(["" | Xs]) -> join(Xs);
join(FN = [X | _Xs]) when is_integer(X) -> FN;
join([X | Xs]) -> filename:join(join(X), join(Xs)).
