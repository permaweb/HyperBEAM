# hb_store_fs

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_store_fs.erl)

A key-value store implementation, following the `hb_store` behavior 
and interface. This implementation utilizes the node's local file system as
its storage mechanism, offering an alternative to other store's that require
the compilation of additional libraries in order to function.
As this store implementation operates using Erlang's native `file` and 
`filelib` mechanisms, it largely inherits its performance characteristics 
from those of the underlying OS/filesystem drivers. Certain filesystems can
be quite performant for the types of workload that HyperBEAM AO-Core execution
requires (many reads and writes to explicit keys, few directory 'listing' or
search operations), awhile others perform suboptimally.
Additionally, thisstore implementation offers the ability for simple 
integration of HyperBEAM with other non-volatile storage media: `hb_store_fs`
will interact with any service that implements the host operating system's
native filesystem API. By mounting devices via `FUSE` (etc), HyperBEAM is
able to interact with a large number of existing storage systems (for example,
S3-compatible cloud storage APIs, etc).

---

## Exported Functions

- `list/2`
- `make_group/2`
- `make_link/3`
- `read/2`
- `reset/1`
- `resolve/2`
- `scope/0`
- `scope/1`
- `start/1`
- `stop/1`
- `type/2`
- `write/3`

---

### start

A key-value store implementation, following the `hb_store` behavior 
Initialize the file system store with the given data directory.

```erlang
start(#{ <<"name">> := DataDir }) ->
    ok = filelib:ensure_dir(DataDir).
```

### stop

Stop the file system store. Currently a no-op.

```erlang
stop(#{ <<"name">> := _DataDir }) ->
    ok.
```

### scope

The file-based store is always local, for now. In the future, we may

```erlang
scope() -> local.
```

### scope

The file-based store is always local, for now. In the future, we may

```erlang
scope(#{ <<"scope">> := Scope }) -> Scope;
```

### scope

The file-based store is always local, for now. In the future, we may
Reset the store by completely removing its directory and recreating it.

```erlang
scope(_) -> scope().
```

### reset

The file-based store is always local, for now. In the future, we may
Reset the store by completely removing its directory and recreating it.

```erlang
reset(#{ <<"name">> := DataDir }) ->
    % Use pattern that completely removes directory then recreates it
    os:cmd(binary_to_list(<< "rm -Rf ", DataDir/binary >>)),
    ?event({reset_store, {path, DataDir}}).
```

### read

Read a key from the store, following symlinks as needed.

```erlang
read(Opts, Key) ->
    read(add_prefix(Opts, resolve(Opts, Key))).
```

### read

```erlang
read(Path) ->
	?event({read, Path}),
	case file:read_file_info(Path) of
		{ok, #file_info{type = regular}} ->
			{ok, _} = file:read_file(Path);
		_ ->
			case file:read_link(Path) of
				{ok, Link} ->
					?event({link_found, Path, Link}),
					read(Link);
				_ ->
					not_found
			end
	end.
```

### write

Write a value to the specified path in the store.

```erlang
write(Opts, PathComponents, Value) ->
    Path = add_prefix(Opts, PathComponents),
    ?event({writing, Path, byte_size(Value)}),
    filelib:ensure_dir(Path),
    ok = file:write_file(Path, Value).
```

### list

List contents of a directory in the store.

```erlang
list(Opts, Path) ->
    case file:list_dir(add_prefix(Opts, Path)) of
        {ok, Files} -> {ok, lists:map(fun hb_util:bin/1, Files)};
        {error, _} -> not_found
    end.
```

### resolve

Replace links in a path successively, returning the final path.

```erlang
resolve(Opts, RawPath) ->
    Res = resolve(Opts, "", hb_path:term_to_path_parts(hb_store:join(RawPath), Opts)),
    ?event({resolved, RawPath, Res}),
    Res.
```

### resolve

```erlang
resolve(_, CurrPath, []) ->
    hb_store:join(CurrPath);
```

### resolve

```erlang
resolve(Opts, CurrPath, [Next|Rest]) ->
    PathPart = hb_store:join([CurrPath, Next]),
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
            resolve(Opts, Link, Rest);
        {error, enoent} ->
            not_found;
        _ ->
            resolve(Opts, PathPart, Rest)
    end.
```

### type

Determine the type of a key in the store.

```erlang
type(Opts, Key) ->
    type(add_prefix(Opts, Key)).
```

### type

```erlang
type(Path) ->
    ?event({type, Path}),
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} -> composite;
        {ok, #file_info{type = regular}} -> simple;
        _ ->
            case file:read_link(Path) of
                {ok, Link} ->
                    type(Link);
                _ ->
                    not_found
            end
    end.
```

### make_group

Create a directory (group) in the store.

```erlang
make_group(Opts = #{ <<"name">> := _DataDir }, Path) ->
    P = add_prefix(Opts, Path),
    ?event({making_group, P}),
    % We need to ensure that the parent directory exists, so that we can
    % make the group.
```

### make_link

Create a symlink, handling the case where the link would point to itself.

```erlang
make_link(_, Link, Link) -> ok;
```

### make_link

Create a symlink, handling the case where the link would point to itself.

```erlang
make_link(Opts, Existing, New) ->
    ?event({symlink,
		add_prefix(Opts, Existing),
		P2 = add_prefix(Opts, New)}),
    filelib:ensure_dir(P2),
    case file:make_symlink(add_prefix(Opts, Existing), N = add_prefix(Opts, New)) of
        ok -> ok;
        {error, eexist} ->
            file:delete(N),
            R = file:make_symlink(add_prefix(Opts, Existing), N),
            ?event(debug_fs,
                {symlink_recreated,
                    {existing, Existing},
                    {new, New},
                    {result, R}
                }
            ),
            R
    end.
```

### add_prefix

Add the directory prefix to a path.

```erlang
add_prefix(#{ <<"name">> := Prefix }, Path) ->
	?event({add_prefix, Prefix, Path}),
    % Check if the prefix is an absolute path
    IsAbsolute = is_binary(Prefix) andalso binary:first(Prefix) =:= $/ orelse
                 is_list(Prefix) andalso hd(Prefix) =:= $/,
    % Join the paths
    JoinedPath = hb_store:join([Prefix, Path]),
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
```

### remove_prefix

Remove the directory prefix from a path.

```erlang
remove_prefix(#{ <<"name">> := Prefix }, Path) ->
```

---

*Generated from [hb_store_fs.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_store_fs.erl)*
