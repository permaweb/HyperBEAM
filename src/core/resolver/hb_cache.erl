%%% @doc A cache of AO-Core protocol messages and compute results.
%%%
%%% HyperBEAM stores all paths in key value stores, abstracted by the `hb_store'
%%% module. Each store has its own storage backend, but each works with simple
%%% key-value pairs. A reference from one path to another is written as a
%%% value: `ao:<ID>' refers to a message, and a `ni:' name refers to a binary.
%%% The cache follows these references itself when it reads a path.
%%%
%%% There are three layers to HyperBEAMs internal data representation on-disk:
%%%
%%% 1. The raw binary data, written to the store under its `ni:' name, which
%%%    contains the hash of the content. Storing binary paths in this way
%%%    effectively deduplicates the data.
%%% 2. Messages, whose keys are written under the ID of the message without
%%%    its commitments (its unsigned ID). Each key is written at `<ID>/<key>'.
%%%    The keys of a nested message are written under the key's path, and the
%%%    keys of each commitment under `<ID>/commitments/<CommitmentID>'. A
%%%    binary of 60 bytes or more is written under its `ni:' name, and the key
%%%    contains the name. Each of the message's other IDs, such as its signed
%%%    ID, contains `ao:' and the unsigned ID.
%%% 3. Computations, stored separately in `attested-store'. The path
%%%    `VBaseID/VReqID' holds `ao:<VResID>', or the `ni:' name of a binary
%%%    result. Portable hashpath receipts describe the execution and its
%%%    witnesses independently of these lookup entries.
%%%
%%% For example, a signed message with a short key `a' and a 64-byte key `b'
%%% is written as follows, where `U' is its unsigned ID and `S' its signed ID:
%%% <pre>
%%%     U/a                                  => 1
%%%     U/b                                  => ni:///sha-256;fOEA...
%%%     ni:%2f%2f%2fsha-256;fOEA...          => xxxxxxxx... (the 64 bytes)
%%%     U/commitments/S/commitment-device    => httpsig@1.0
%%%     U/commitments/S/type                 => rsa-pss-sha512
%%%     U/commitments/S/signature            => ni:///sha-256;HVZf...
%%%     ...
%%%     S                                    => ao:U
%%% </pre>
%%%
%%% Before writing a message to the store, we convert it to Type-Annotated
%%% Binary Messages (TABMs), such that each of the keys in the message is
%%% either a map or a direct binary.
%%% 
%%% Nested keys are lazily loaded from the stores, such that large deeply
%%% nested messages where only a small part of the data is actually used are
%%% not loaded into memory unnecessarily. In order to ensure that a message is
%%% loaded from the cache after a `read', we can use the `ensure_loaded/1' and
%%% `ensure_all_loaded/1' functions. Ensure loaded will load the exact value
%%% that has been requested, while ensure all loaded will load the entire 
%%% structure of the message into memory.
%%% 
%%% Lazily loadable `links' are expressed as a tuple of the following form:
%%% `{link, ID, LinkOpts}', where `ID' is the path to the data in the store,
%%% and `LinkOpts' is a map of suggested options to use when loading the data.
%%% In particular, this module ensures to stash the `store' option in `LinkOpts',
%%% such that the `read' function can use the correct store without having to
%%% search unnecessarily. By providing an `Opts' argument to `ensure_loaded' or
%%% `ensure_all_loaded', the caller can specify additional options to use when
%%% loading the data -- overriding the suggested options in the link.
-module(hb_cache).
-export([read_all_commitments/2]).
-export([ensure_loaded/1, ensure_loaded/2, ensure_all_loaded/1, ensure_all_loaded/2]).
-export([read/2, read_resolved/3, write/2, link/3, encode_value/1]).
-export([match/2, list/2, list_numbered/2]).
-export([test_unsigned/1, test_signed/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(DIRECT_VALUE_LENGTH, 60).
-define(MAX_KEY_LENGTH, 256).
-define(MAX_REDIRECTS, 32).

%% @doc Ensure that a value is loaded from the cache if it is an ID or a link.
%% If it is not loadable we raise an error. If the value is a message, we will
%% load only the first `layer' of it: Representing all nested messages inside 
%% the result as links. If the value has an associated `type' key in the extra
%% options, we apply it to the read value, 'lazily' recreating a `structured@1.0'
%% form.
ensure_loaded(Msg) ->
    ensure_loaded(Msg, #{}).
ensure_loaded(Msg, Opts) ->
    ensure_loaded([], Msg, Opts).
ensure_loaded(Ref, {Status, Msg}, Opts) when Status == ok; Status == error ->
    {Status, ensure_loaded(Ref, Msg, Opts)};
ensure_loaded(Ref, {link, ID, LinkOpts = #{ <<"execution-input">> := true }}, Opts) ->
    hb_ao:execution_input(
        ensure_loaded(Ref,
            {link, ID, maps:remove(<<"execution-input">>, LinkOpts)}, Opts),
        Opts
    );
ensure_loaded(Ref, Link = {link, ID, LinkOpts = #{ <<"lazy">> := true }}, RawOpts) ->
    % If the user provided their own options, we merge them and _overwrite_
    % the options that are already set in the link. We read the value from the
    % stores in the link first, then from the caller's stores.
    Opts = link_opts(LinkOpts, RawOpts),
    Stores =
        lists:uniq(
            lists:flatten([
                hb_opts:get(store, [], Opts),
                hb_opts:get(store, [], RawOpts)
            ])
        ),
    Commitment = hb_opts:get(commitment, undefined, Opts),
    CacheReadResult =
        case Commitment of
            true ->
                do_read_commitment(ID, Opts);
            _ ->
                read(ID, Opts#{ <<"store">> => Stores })
        end,
    case CacheReadResult of
        {ok, LoadedMsg} ->
            ?event_debug(debug_caching,
                {lazy_loaded,
                    {link, ID},
                    {msg, LoadedMsg},
                    {link_opts, LinkOpts}
                }
            ),
            case maps:get(<<"type">>, LinkOpts, undefined) of
                % A value that is a link is loaded too, except in a commitment,
                % which is read as it is stored.
                undefined when Commitment =/= true ->
                    case hb_link:decode(LoadedMsg) of
                        {link, URI, _} ->
                            ensure_loaded(
                                Ref,
                                {link, URI, #{ <<"store">> => Stores }},
                                RawOpts
                            );
                        Value -> Value
                    end;
                undefined -> LoadedMsg;
                Type -> structured_decode_value(Type, LoadedMsg, Opts)
            end;
        {error, not_found} ->
            report_ensure_loaded_not_found(Ref, Link, Opts);
        {error, Reason} -> erlang:error(Reason)
    end;
ensure_loaded(Ref, Link = {link, URI, LinkOpts}, RawOpts) ->
    % The value is a link, so we read the message at its URI.
    Opts = link_opts(LinkOpts, RawOpts),
    case read(URI, Opts) of
        {ok, Msg} -> Msg;
        {error, not_found} -> report_ensure_loaded_not_found(Ref, Link, Opts);
        {error, Reason} -> erlang:error(Reason)
    end;
ensure_loaded(_Ref, Msg, _Opts) when not ?IS_LINK(Msg) ->
    Msg.

%% @doc Merge the options of a link over the caller's. A link with stores
%% is read from them first, and then from the caller's stores.
link_opts(LinkOpts, RawOpts) ->
    Opts = hb_util:deep_merge(RawOpts, LinkOpts, RawOpts),
    case hb_opts:get(store, not_found, LinkOpts#{ <<"only">> => local }) of
        not_found ->
            hb_store:scope(
                Opts,
                hb_opts:get(scope, hb_opts:get(scope, local, RawOpts), LinkOpts)
            );
        Store ->
            Opts#{
                <<"store">> =>
                    lists:uniq(
                        lists:flatten([Store, hb_opts:get(store, [], RawOpts)])
                    )
            }
    end.

%% @doc Report that a value was not found in the cache. If a key is provided,
%% we report that the key was not found, otherwise we report that the link was
%% not found.
report_ensure_loaded_not_found(Ref, Lk, Opts) ->
    ?event(link_error, {link_not_resolvable, {ref, Ref}, {link, Lk}, {opts, Opts}}),
    throw(
        {necessary_message_not_found,
            hb_path:to_binary(lists:reverse(Ref)),
            hb_link:format_unresolved(Lk, Opts, 0)
        }
    ).

%% @doc Ensure that all of the components of a message (whether a map, list,
%% or immediate value) are recursively fully loaded from the stores into memory.
%% This is a catch-all function that is useful in situations where ensuring a
%% message contains no links is important, but it carries potentially extreme
%% performance costs.
ensure_all_loaded(Msg) ->
    ensure_all_loaded(Msg, #{}).
ensure_all_loaded(Msg, Opts) ->
    ensure_all_loaded([], Msg, Opts).
ensure_all_loaded(Ref, Link, Opts) when ?IS_LINK(Link) ->
    ensure_all_loaded(Ref, ensure_loaded(Ref, Link, Opts), Opts);
ensure_all_loaded(Ref, Msg, Opts) when is_map(Msg) ->
    maps:map(fun(K, V) -> ensure_all_loaded([K|Ref], V, Opts) end, Msg);
ensure_all_loaded(Ref, Msg, Opts) when is_list(Msg) ->
    lists:map(
        fun({N, V}) -> ensure_all_loaded([N|Ref], V, Opts) end,
        hb_util:number(Msg)
    );
ensure_all_loaded(Ref, Msg, Opts) ->
    ensure_loaded(Ref, Msg, Opts).

%% @doc List all items in a directory, assuming they are numbered.
list_numbered(Path, Opts) ->
    SlotDir = hb_path:to_binary(Path),
    [ hb_util:int(Name) || Name <- list(SlotDir, Opts) ].

%% @doc List all items under a given path.
list(Path, Opts) when is_map(Opts) and not is_map_key(<<"store-module">>, Opts) ->
    case hb_opts:get(store, no_viable_store, Opts) of
        not_found -> [];
        Store ->
            list(Path, Store, Opts)
    end;
list(Path, Store) ->
    list(Path, Store, #{}).
list(Path, Store, Opts) ->
    PathBin = hb_path:to_binary(Path),
    % List the path in the local stores first. The other stores are only asked
    % if the local stores have no keys for it.
    case list(PathBin, hb_store:scope(Store, local), Store, Opts) of
        [] -> list(PathBin, remote_stores(Store), Store, Opts);
        Names -> Names
    end.
list(PathBin, Stores, AllStores, Opts) ->
    case hb_store:list(Stores, PathBin, Opts) of
        {ok, Names} -> Names;
        _ ->
            % If the path has a link, we list the keys of the link's target.
            case lookup(PathBin, PathBin, Stores, Opts, 0) of
                {ok, _Target, Resolved, _} when Resolved =/= PathBin ->
                    list(Resolved, AllStores, Opts);
                _ -> []
            end
    end.

%% @doc Match a template message against the cache, returning a list of IDs
%% that match the template. We match on the binary representation of values,
%% rather than their types explicitly, such that 'AO-Types' keys that are
%% only partial matches do not cause the match to fail. If the `match_index' key
%% is set, indicating the presence and usage of the `~match@1.0` device, we use
%% it to find the matching messages. This lowers the complexity class of the
%% match to `O(keys * log(cache_size))` instead of `O(cache_size)`.
match(MatchSpec, Opts) ->
    ReadMode = hb_opts:get(cache_read_mode, normal, Opts),
    NormalizedSpec = normalize_match_spec(MatchSpec, ReadMode, Opts),
    case (ReadMode == raw) orelse
        (hb_opts:get(match_index, false, Opts) == false)
    of
        true -> store_match(NormalizedSpec, Opts);
        false ->
            case
                hb_ao:raw(
                    <<"match@1.0">>,
                    <<"all">>,
                    NormalizedSpec,
                    #{},
                    Opts
                )
            of
                {ok, Matches} when length(Matches) > 0 -> {ok, Matches};
                _ -> {error, not_found}
            end
    end.

%% @doc Normalize match values for store reverse-index lookups.
normalize_match_spec(MatchSpec, raw, Opts) ->
    maps:without([<<"ao-types">>], hb_ao:normalize_keys(MatchSpec, Opts));
normalize_match_spec(MatchSpec, _ReadMode, Opts) ->
    Spec = hb_message:convert(MatchSpec, tabm, <<"structured@1.0">>, Opts),
    maps:without([<<"ao-types">>], hb_ao:normalize_keys(Spec, Opts)).

%% @doc Match using the store's reverse index.
store_match(NormalizedSpec, Opts) ->
    ConvertedMatchSpec =
        maps:from_list([
            {
                hb_escape:encode_path_component(Key),
                store_match_value(Value, Opts)
            }
        ||
            {Key, Value} <- maps:to_list(NormalizedSpec)
        ]),
    case hb_store:match(
        hb_opts:get(store, no_viable_store, Opts),
        ConvertedMatchSpec,
        Opts
    ) of
        {ok, Matches} ->
            case lists:filtermap(fun match_id/1, Matches) of
                [] -> {error, not_found};
                IDs -> {ok, IDs}
            end;
        _ -> {error, not_found}
    end.

%% @doc Find the message ID in a path that a store match returns. A top-level
%% key is itself the ID, and a path under `<ID>/commitments/' gives the ID of
%% the commitment. Any other path does not name a message.
match_id(Path) ->
    case binary:split(Path, <<"/">>, [global]) of
        [ID] -> {true, ID};
        [_ID, <<"commitments">>, CommitmentID] -> {true, CommitmentID};
        _ -> false
    end.

%% @doc Generate the path at which a binary value should be stored: its `ni:'
%% name, as one path component.
generate_binary_path(Bin, Opts) ->
    hb_escape:encode_path_component(?NI_URI(hb_path:hashpath(Bin, Opts))).

%% @doc Write a message to the cache. For raw binaries, we write the data under
%% its `ni:' name, which has the SHA2-256 hash of the data. For messages, we
%% write each key under the unsigned ID of the message (see `key_ops/3'). We
%% then write `ao:' and the unsigned ID at each of the message's other IDs,
%% such that any of the committed or uncommitted IDs can be read, and once in
%% memory all of the commitments are available. Commitments over different
%% keys are written as separate messages, and the ID of the one with the most
%% keys is returned (see `write_split/3').
write(RawMsg, Opts) when is_map(RawMsg) ->
    write_split(prepare_write(RawMsg, Opts), RawMsg, Opts);
write(List, Opts) when is_list(List) ->
    write(hb_message:convert(List, tabm, <<"structured@1.0">>, Opts), Opts);
write(Bin, Opts) when is_binary(Bin) ->
    do_write_message(Bin, hb_opts:get(store, no_viable_store, Opts), Opts).

%% @doc Write a prepared message once for each set of keys that its
%% commitments cover, and return the result of the set with the most keys. If
%% there is more than one set and more than one signed commitment, write at
%% their combined ID the path that reads the message with all of them (see
%% `commitments_path/1').
write_split(Msg, RawMsg, Opts) ->
    Sets = split_commitments(Msg, Opts),
    Result = lists:last([ write_prepared(Set, RawMsg, Opts) || Set <- Sets ]),
    case length(Sets) > 1 andalso calculate_all_ids(Msg, undefined, Opts) of
        {SignedIDs, _, AllID} when length(SignedIDs) > 1 ->
            hb_store:write(
                hb_opts:get(store, no_viable_store, Opts),
                #{ AllID => commitments_path(SignedIDs) },
                Opts
            );
        _ -> ok
    end,
    Result.

%% @doc Return the message once for each set of keys its commitments cover,
%% with only those keys and the commitments over them, the set with the most
%% keys last, or the message itself if all of its commitments cover the same
%% keys. A list has no commitments, so it is returned as it is.
split_commitments(List, _Opts) when is_list(List) -> [List];
split_commitments(Msg, Opts) ->
    Groups =
        maps:groups_from_list(
            fun({_, Commitment}) ->
                Committed = hb_maps:get(<<"committed">>, Commitment, [], Opts),
                Keys =
                    lists:usort(
                        hb_util:message_to_ordered_list(
                            ensure_all_loaded(Committed, Opts),
                            Opts
                        )
                    ),
                {length(Keys), Keys}
            end,
            hb_maps:to_list(hb_maps:get(<<"commitments">>, Msg, #{}, Opts), Opts)
        ),
    case map_size(Groups) of
        N when N < 2 -> [Msg];
        _ ->
            [
                (hb_maps:with([<<"commitments">> | Keys], Msg, Opts))#{
                    <<"commitments">> => maps:from_list(Comms)
                }
            ||
                {{_, Keys}, Comms} <- lists:sort(maps:to_list(Groups))
            ]
    end.

%% @doc Write a prepared message, with private keys from `RawMsg' for the hook.
write_prepared(Msg, RawMsg, Opts) ->
    hb_message:paranoid_verify(cache_write, Msg, Opts),
    % Conversion writes the children that it links only in `offload' mode.
    Linked =
        case hb_opts:get(<<"linkify-mode">>, offload, Opts) of
            offload -> link_prepared(Msg, Opts);
            _ -> Msg
        end,
    TABM = hb_message:convert(Linked, tabm, <<"structured@1.0">>, Opts),
    ?event_debug(debug_cache, {writing_full_message, {msg, TABM}}),
    try
        % The message's private keys are not written, but the `cache-write'
        % hook receives them.
        do_write_message(
            hb_private:set_priv(TABM, hb_private:from_message(RawMsg)),
            hb_opts:get(store, no_viable_store, Opts),
            Opts
        )
    catch
        Type:Reason:Stacktrace ->
            ?event(error,
                {cache_write_error,
                    {type, Type},
                    {reason, Reason},
                    {stacktrace, {trace, Stacktrace}}
                },
                Opts
            ),
            erlang:raise(Type, Reason, Stacktrace)
    end.

%% @doc Write each child that conversion links with `write_split/3', as
%% `prepare_write/2' has already prepared it, and replace it with a link to its
%% `all' ID, from the unsigned ID that the write gives. Values under private
%% keys are not written. Conversion in `offload' mode writes the children it
%% loads from links with `write/2', which prepares them first.
link_prepared(List, Opts) when is_list(List) ->
    [ link_prepared(true, Child, Opts) || Child <- List ];
link_prepared(Msg, Opts) ->
    maps:map(
        fun(<<"commitments">>, Child) -> Child;
           (<<"ao-types">>, Child) -> Child;
           (Key, Child) ->
                link_prepared(not hb_private:is_private(Key), Child, Opts)
        end,
        Msg
    ).
link_prepared(true, Child, Opts) when is_map(Child) orelse is_list(Child) ->
    {ok, UncommittedID} = write_split(Child, Child, Opts),
    {_, _, ID} = calculate_all_ids(Child, UncommittedID, Opts),
    {link, <<"ao:", ID/binary>>, #{}};
link_prepared(_Prepared, Child, _Opts) -> Child.

%% @doc Prepare a message to be written: prepare each child, except those under
%% private keys, and each commitment message, then remove the commitments that
%% do not verify. If commitments remain and do not commit every key that is not
%% private, keep only the keys that one of them commits, and again remove the
%% commitments that do not verify over those keys. A message with an empty key
%% is refused.
prepare_write(Msg, Opts) when is_map(Msg) ->
    Deep = maps:map(
        fun
            % An empty key has its parent's store path, so we refuse it.
            (Key, _Value) when Key =:= <<>> ->
                throw({invalid_key, Key});
           (<<"commitments">>, Value) ->
                prepare_write(ensure_all_loaded(Value, Opts), Opts);
           (Key, Value) ->
                case hb_private:is_private(Key) of
                    true -> Value;
                    false -> prepare_write(Value, Opts)
                end
        end,
        Msg
    ),
    Settings = #{ <<"verify">> => all, <<"add-unsigned">> => false,
        <<"depth">> => shallow },
    Verified = hb_message:normalize_commitments(Deep, Opts, Settings),
    {ok, Committed} = hb_message:with_only_committed(Verified, any, Opts),
    case lists:all(fun hb_private:is_private/1,
            maps:keys(Verified) -- maps:keys(Committed)) of
        true -> Verified;
        false -> hb_message:normalize_commitments(Committed, Opts, Settings)
    end;
prepare_write(Values, Opts) when is_list(Values) ->
    [ prepare_write(Value, Opts) || Value <- Values ];
prepare_write(Value, _Opts) -> Value.

do_write_message(Bin, Store, Opts) when is_binary(Bin) ->
    % Write the binary in the store at its calculated content-hash.
    % Return the path.
    Path = generate_binary_path(Bin, Opts),
    hb_store:write(Store, #{ Path => Bin }, Opts),
    %lists:map(fun(ID) -> hb_store:make_link(Store, Path, ID) end, AllIDs),
    {ok, Path};
do_write_message(List, Store, Opts) when is_list(List) ->
    do_write_message(
        hb_message:convert(List, tabm, <<"structured@1.0">>, Opts),
        Store,
        Opts
    );
do_write_message(Msg, Store, Opts) when is_map(Msg) ->
    {ok, UncommittedID, Ops} = write_message_ops(Msg, Opts),
    % Check the length of every key before we write any of them. A store that
    % fails to write one key would leave the message stored without it.
    lists:foreach(
        fun({write, Path, _Value}) -> check_key_length(Path);
           (_) -> ok
        end,
        Ops
    ),
    run_write_ops(Store, Ops, Opts),
    {ok, UncommittedID}.

%% @doc Throw ``{'key-too-long', Key}'' for a store key over `?MAX_KEY_LENGTH'
%% bytes: `hb_store_fs' cannot write a file name over 255 bytes, nor
%% `hb_store_lmdb' a key over 511 bytes.
check_key_length(Key) when byte_size(Key) > ?MAX_KEY_LENGTH ->
    throw({'key-too-long', Key});
check_key_length(_Key) ->
    ok.

write_message_ops(Msg, Opts) when is_map(Msg) ->
    ?event_debug(debug_cache, {writing_message, Msg}),
    UncommittedID =
        hb_message:id(Msg, none, Opts#{ <<"linkify-mode">> => discard }),
    {SignedIDs, UnsignedIDs, AllID} = calculate_all_ids(Msg, UncommittedID, Opts),
    ?event_debug(debug_cache,
        {writing_message,
            {id, UncommittedID},
            {signed_ids, SignedIDs},
            {unsigned_ids, UnsignedIDs},
            {all_id, AllID},
            {original, Msg}
        }
    ),
    KeyOps = key_ops(UncommittedID, maps:without([<<"priv">>], Msg), Opts),
    % Create an operation to write `ao:' and the unsigned ID at each of the
    % known alternative IDs for the message (signed IDs, and _other_ non-root
    % IDs). The combined 'all' ID of several signed commitments does not
    % identify any one of them, so we write an `ao://' path at it that reads
    % the message with all of them (see `commitments_path/1').
    Ops =
        lists:foldl(
            fun(AltID, Acc) ->
                ?event_debug(debug_cache,
                    {linking_commitment,
                        {uncommitted_id, UncommittedID},
                        {committed_id, AltID}
                    }
                ),
                [{write, AltID, <<"ao:", UncommittedID/binary>>} | Acc]
            end,
            [
                {write, AllID, commitments_path(SignedIDs)}
            ||
                length(SignedIDs) > 1
            ] ++ [{index_hook, AllID, SignedIDs, UnsignedIDs, Msg} | KeyOps],
            lists:uniq(SignedIDs ++ UnsignedIDs) -- [UncommittedID]
        ),
    {ok, UncommittedID, lists:reverse(Ops)}.

%% @doc The `ao://' path written at the combined ID of a message with several
%% signed commitments: `with-commitments' of their IDs, which `message@1.0'
%% resolves on the empty message to the message with them.
commitments_path(IDs) ->
    iolist_to_binary([<<"ao://with-commitments=">> | lists:join(<<",">>, IDs)]).

%% @doc Read the `ao://' path that is stored at an ID. A `with-commitments'
%% path (see `is_commitments_path/1') is resolved by `ao@1.0' in the local
%% stores only, giving the message with those commitments. In `raw' mode we
%% return the path itself, because resolving it needs `message@1.0'. Any other
%% `ao://' value at an ID is not found.
read_commitments_path(AOPath, Opts) ->
    case hb_opts:get(cache_read_mode, normal, Opts) of
        raw -> {ok, AOPath};
        _ ->
            case is_commitments_path(AOPath) of
                true -> hb_uri:from(AOPath, hb_store:scope(Opts, local));
                false -> {error, not_found}
            end
    end.

%% @doc Whether a value at an ID is `ao://with-commitments=' followed by only
%% base64url characters and commas: a path that resolves `with-commitments' on
%% the empty message, and nothing else.
is_commitments_path(<<"ao://with-commitments=", List/binary>>) ->
    re:run(List, <<"\\A[A-Za-z0-9_,-]+\\z">>, [{capture, none}]) =:= match;
is_commitments_path(_Value) ->
    false.

%% @doc Generate the write operations for a message's keys under a path. A
%% message with no keys is written as an empty `ao-types' key, so that the
%% path can still be read.
key_ops(Path, Msg, Opts) ->
    case maps:fold(
        fun(Key, Value, Acc) -> write_key_ops(Path, Key, Value, Opts, Acc) end,
        [],
        Msg
    ) of
        [] -> [{write, <<Path/binary, "/ao-types">>, <<>>}];
        KeyOps -> KeyOps
    end.

%% @doc Add the write operations for one key of a message to the accumulator.
%% The keys of each commitment are written under `<Path>/commitments/<ID>',
%% and a list is written as a message. Any other value is written at the key's
%% path by `value_ops/4'.
write_key_ops(Path, <<"commitments">>, RawCommitments, Opts, Acc) ->
    Commitments = prepare_commitments(RawCommitments, Opts),
    ?event({writing_commitments, {path, Path}, {commitments, Commitments}}),
    maps:fold(
        fun(CommID, Commitment, InnerAcc) ->
            prepend_reversed(
                key_ops(
                    <<Path/binary, "/commitments/", CommID/binary>>,
                    Commitment,
                    Opts
                ),
                InnerAcc
            )
        end,
        Acc,
        Commitments
    );
write_key_ops(Path, Key, Value, Opts, Acc) when is_list(Value) ->
    write_key_ops(
        Path,
        Key,
        hb_message:convert(Value, tabm, <<"structured@1.0">>, Opts),
        Opts,
        Acc
    );
write_key_ops(Path, Key, Value, Opts, Acc) ->
    value_ops(
        <<Path/binary, "/", (hb_escape:encode_path_component(Key))/binary>>,
        Value,
        Opts,
        Acc
    ).

%% @doc Add the write operations for a value at a key's path. A nested message
%% has its keys written under the path. A binary of `?DIRECT_VALUE_LENGTH'
%% bytes or more is written under its `ni:' name, and the path has the name.
%% A shorter binary is written at the path itself, encoded by `encode_value/1'.
value_ops(KeyPath, Value, Opts, Acc) when is_map(Value) ->
    prepend_reversed(key_ops(KeyPath, Value, Opts), Acc);
value_ops(KeyPath, Value, Opts, Acc) ->
    case is_immediate_value(Value) of
        true -> [{write, KeyPath, encode_value(Value)} | Acc];
        false ->
            Name = ?NI_URI(hb_path:hashpath(Value, Opts)),
            [
                {write, KeyPath, Name},
                {write, hb_escape:encode_path_component(Name), Value}
                | Acc
            ]
    end.

%% @doc Return whether a value is a binary that is short enough to be written
%% at its key's path. Longer binaries are written under their `ni:' name.
is_immediate_value(Value) ->
    is_binary(Value) andalso byte_size(Value) < ?DIRECT_VALUE_LENGTH.

%% @doc Encode a binary to be written at a key's path. Most binaries are
%% written unchanged. A binary that starts with `ni:' or `data:' would be read
%% back as a `ni:' name or as an encoded value, so we write it as a `data:,'
%% URI of the binary, escaped by `hb_escape:encode/1'. The gateway and remote
%% node stores encode the binaries that they return in the same way.
encode_value(Value = <<"ni:", _/binary>>) -> data_uri(Value);
encode_value(Value = <<"data:", _/binary>>) -> data_uri(Value);
encode_value(Value) -> Value.

data_uri(Value) -> <<"data:,", (hb_escape:encode(Value))/binary>>.

%% @doc Decode a value read at a key's path. A `data:,' URI is decoded to the
%% binary that it holds, and one that is not percent-encoded is refused. Any
%% other value is returned as it is.
decode_value(<<"data:,", Value/binary>>) ->
    case re:run(Value, <<"%(?![0-9a-fA-F]{2})">>, [{capture, none}]) of
        match -> throw({'invalid-data-uri', Value});
        nomatch -> hb_escape:decode(Value)
    end;
decode_value(Value) -> Value.

store_match_value(Bin, Opts) when is_binary(Bin) ->
    case is_immediate_value(Bin) of
        true -> encode_value(Bin);
        false -> ?NI_URI(hb_path:hashpath(Bin, Opts))
    end;
store_match_value(Map, Opts) when is_map(Map) ->
    <<"ao:",
        (hb_message:id(
            Map,
            all,
            Opts#{ <<"linkify-mode">> => discard }
        ))/binary
    >>;
store_match_value(List, Opts) when is_list(List) ->
    case io_lib:printable_unicode_list(List) of
        true ->
            store_match_value(iolist_to_binary(List), Opts);
        false ->
            store_match_value(
                hb_message:convert(List, tabm, <<"structured@1.0">>, Opts),
                Opts
            )
    end;
store_match_value(Other, Opts) ->
    store_match_value(hb_path:to_binary(Other), Opts).

prepend_reversed(Ops, Acc) ->
    lists:foldl(fun(Op, InnerAcc) -> [Op | InnerAcc] end, Acc, Ops).

run_write_ops(Store, Ops, Opts) ->
    run_write_ops(Store, Ops, Opts, []).
run_write_ops(Store, [], Opts, Pending) ->
    flush_write_ops(Store, Pending, Opts);
run_write_ops(
        Store,
        [{index_hook, AllID, SignedIDs, UnsignedIDs, Msg} | Rest],
        Opts,
        Pending
) ->
    flush_write_ops(Store, Pending, Opts),
    {ok, _} =
        hb_hook:on(
            <<"cache-write">>,
            #{
                <<"body">> => Msg,
                <<"all-id">> => AllID,
                <<"signed-ids">> => SignedIDs,
                <<"unsigned-ids">> => UnsignedIDs,
                <<"priv">> => #{ <<"hook-caller">> => <<"kernel">> }
            },
            Opts
        ),
    run_write_ops(Store, Rest, Opts, []);
run_write_ops(Store, [Op | Rest], Opts, Pending) ->
    run_write_ops(Store, Rest, Opts, [Op | Pending]).

flush_write_ops(_Store, [], _Opts) ->
    ok;
flush_write_ops(Store, Pending, Opts) ->
    apply_write_ops(Store, lists:reverse(Pending), Opts).

%% @doc Apply a list of write operations in one store `write'.
apply_write_ops(Store, Ops, Opts) ->
    hb_store:write(
        Store,
        maps:from_list([{Path, Value} || {write, Path, Value} <- Ops]),
        Opts
    ),
    ok.

%% @doc The `structured@1.0` encoder does not typically encode `commitments`,
%% subsequently, when we encounter a commitments message we prepare its contents
%% separately, then write each to the store. A commitment does not contain
%% links, so we convert it with `bundle' set, keeping its nested values in its
%% TABM.
prepare_commitments(RawCommitments, Opts) ->
    Commitments = ensure_all_loaded(RawCommitments, Opts),
    maps:map(
        fun(_, StructuredCommitment) ->
            hb_message:convert(
                StructuredCommitment,
                tabm,
                #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
                Opts
            )
        end,
        Commitments
    ).

%% @doc Calculate the IDs for a message.
calculate_all_ids(Bin, _UncommittedID, _Opts) when is_binary(Bin) -> [];
calculate_all_ids(List, UncommittedID, _Opts) when is_list(List) ->
    % A list has no commitments, so return its unsigned ID as its `all' ID.
    {[], [UncommittedID], UncommittedID};
calculate_all_ids(Msg, UncommittedID, Opts) ->
    CommIDs = 
        hb_maps:keys(
            Comms = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
            Opts
        ),
    {SignedIDs, UnsignedIDs} =
        lists:partition(
            fun(ID) ->
                Comm = hb_maps:get(ID, Comms, #{}, Opts),
                is_map_key(<<"committer">>, Comm)
            end,
            CommIDs
        ),
    ?event_debug({calculating_ids, {msg, Msg}, {comm_ids, CommIDs}}),
    case length(CommIDs) of
        0 ->
            % With no commitments the `all' id is the uncommitted id we already
            % computed; skip re-serializing the message to recompute it.
            {[], [UncommittedID], UncommittedID};
        _ ->
            AllID =
                hb_message:id(
                    Msg,
                    all,
                    Opts#{ <<"linkify-mode">> => discard }
                ),
            {SignedIDs, UnsignedIDs, AllID}
    end.

%% @doc Read the message at a path. Returns in `structured@1.0' format: Either
%% a richly typed map or a direct binary. A URI is read by the device of its
%% scheme (`hb_uri'), except a `ni:' name, under which the stores contain a
%% binary (`write/2'), and `ao:<ID>', which is read as the ID: `ao@1.0' reads
%% it so, and a link then loads without a resolution. If `cache-read-mode' is
%% `raw', composite reads return lazy links without decoding `ao-types'. We
%% follow the `ao:' and `ni:' references that the cache writes (see
%% `lookup/5'). An ID that contains an `ao://' path is read by
%% `read_commitments_path/2'.
%% A literal value read by its path below a message, such as `<ID>/count',
%% is returned in the encoding that the store holds: an integer `1337' reads
%% as `<<"1337">>'. The types of a message's keys are stored in its
%% `ao-types', so they are applied only when the whole message is read.
read(<<"ni:", _/binary>> = Name, Opts) ->
    do_read(Name, Opts);
read(ID, Opts) when ?IS_ID(ID) ->
    % An ID has no scheme, so it is read without looking for one.
    do_read(ID, Opts);
read(<<"ao:", ID/binary>>, Opts) when ?IS_ID(ID) ->
    do_read(ID, Opts);
read(Path, Opts) ->
    case hb_uri:scheme(Path) of
        {error, not_found} -> do_read(Path, Opts);
        _ -> hb_uri:from(Path, Opts)
    end.
do_read(Path, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    case {
        store_read(Path, Store, Opts),
        hb_opts:get(cache_read_mode, normal, Opts)
    } of
        {{ok, Res}, raw} ->
            {ok, Res};
        {{ok, Res}, _} ->
            hb_message:paranoid_verify(cache_read, Res, Opts),
            {ok, Res};
        {Other, _} ->
            Other
    end.

%% @doc Read a commitment from the store. The values in a commitment are not
%% links, so they are returned as they are stored.
do_read_commitment(Path, Opts) ->
    store_read(
        Path,
        hb_opts:get(store, no_viable_store, Opts),
        Opts#{ <<"commitment">> => true }
    ).

%% @doc Load all of the commitments for a message into memory.
read_all_commitments(Msg, Opts) ->
    LocalOpts = hb_store:scope(Opts, local),
    UncommittedID = hb_message:id(Msg, none, Opts#{ <<"linkify-mode">> => discard }),
    CurrentCommitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    AlreadyLoaded = hb_maps:keys(CurrentCommitments, Opts),
    CommitmentsPath = hb_path:to_binary([UncommittedID, <<"commitments">>]),
    FoundCommitments =
        case hb_store:list(CommitmentsPath, LocalOpts) of
            {ok, CommitmentIDs} ->
                lists:filtermap(
                    fun(CommitmentID) ->
                        ShouldLoad = not lists:member(CommitmentID, AlreadyLoaded),
                        ResolvedCommPath = hb_path:to_binary([CommitmentsPath, CommitmentID]),
                        case
                            ShouldLoad andalso
                                do_read_commitment(ResolvedCommPath, LocalOpts)
                        of
                            {ok, Commitment} ->
                                {
                                    true,
                                    {
                                        CommitmentID,
                                        ensure_all_loaded(
                                            Commitment,
                                            Opts#{ <<"commitment">> => true }
                                        )
                                    }
                                };
                            _ ->
                                false
                        end
                    end,
                    CommitmentIDs
                );
            _ ->
                []
    end,
    NewCommitments =
        hb_maps:merge(
            CurrentCommitments,
            maps:from_list(FoundCommitments)
        ),
    Msg#{ <<"commitments">> => NewCommitments }.

%% @doc Read a path from a store, returning its value, or the message of the
%% keys under it. The references that the cache writes as values are followed
%% by `lookup/5'.
store_read(Path, Store, Opts) ->
    store_read(Path, Path, Store, Opts).
store_read(_Target, _Path, no_viable_store, _) ->
    {error, not_found};
store_read(Target, <<"ni:///", _/binary>> = Name, Store, Opts) ->
    % A `ni:' name is stored as one escaped path component.
    store_read(Target, hb_escape:encode_path_component(Name), Store, Opts);
store_read(Target, Path, Store, Opts) ->
    case lookup(Target, hb_path:to_binary(Path), Store, Opts, 0) of
        {ok, _ReadTarget, <<"ni:", _/binary>>, {ok, Bin}} ->
            % The value under a `ni:' name is the binary itself, so we do not
            % decode it.
            {ok, Bin};
        {ok, _ReadTarget, ID, {ok, AOPath = <<"ao://", _/binary>>}}
                when ?IS_ID(ID) ->
            read_commitments_path(AOPath, Opts);
        {ok, _ReadTarget, _ResolvedFullPath, {ok, Value}} ->
            {ok, decode_value(Value)};
        {ok, ReadTarget, ResolvedFullPath, {composite, Children}} ->
            ?event_debug({reading_composite, ResolvedFullPath}),
            {ok,
                read_message(
                    ReadTarget,
                    ResolvedFullPath,
                    Children,
                    Store,
                    Opts
                )
            };
        Error ->
            Error
    end.

%% @doc Read a path, following the references that the cache writes as
%% values. A `ni:' name is followed to the binary stored under it, and
%% `ao:<ID>' to the message with that ID. The `ao:' values of `path' and
%% `hashpath' keys, and those in a commitment, are not followed. We read from
%% the local stores first. If they do not have the path, but its first part is
%% an ID that has `ao:<ID>' in them, we read the path under that ID instead.
%% Only then do we ask the other stores for the path. A chain of more than
%% `?MAX_REDIRECTS' references is refused. Returns the ID that selects the
%% commitment of a message, the path read, and the store's result for it.
lookup(_Target, _Path, _Store, _Opts, Depth) when Depth > ?MAX_REDIRECTS ->
    {error, 'too-many-links'};
lookup(Target, Path = <<"ni:", _/binary>>, Store, Opts, _Depth) ->
    Result =
        case hb_store:read(hb_store:scope(Store, local), Path, Opts) of
            {ok, Bin} -> {ok, Bin};
            _ -> hb_store:read(remote_stores(Store), Path, Opts)
        end,
    lookup_result(Target, Path, Result);
lookup(Target, Path, Store, Opts, Depth) ->
    Local = hb_store:scope(Store, local),
    case hb_store:read(Local, Path, Opts) of
        {ok, _} = Found ->
            follow(Target, Path, Found, Store, Opts, Depth);
        {composite, _} = Found ->
            follow(Target, Path, Found, Store, Opts, Depth);
        _ ->
            case alias(Path, Local, Opts) of
                {ok, Aliased} ->
                    lookup(Target, Aliased, Store, Opts, Depth + 1);
                {error, not_found} ->
                    follow(
                        Target,
                        Path,
                        hb_store:read(remote_stores(Store), Path, Opts),
                        Store,
                        Opts,
                        Depth
                    )
            end
    end.

%% @doc Return the stores in a list that are not local, in their order.
remote_stores(Store) ->
    hb_store:filter(Store, fun(Scope, _) -> Scope =/= local end).

%% @doc Follow a value read at a path. A `ni:' name is followed to the binary
%% stored under it, and `ao:<ID>' to the message with that ID, if
%% `follows_links/2' allows it. Any other value is returned for the path.
follow(Target, _Path, {ok, Name = <<"ni:", _/binary>>}, Store, Opts, Depth) ->
    lookup(
        Target,
        hb_escape:encode_path_component(Name),
        Store,
        Opts,
        Depth + 1
    );
follow(Target, Path, {ok, Link = <<"ao:", ID/binary>>}, Store, Opts, Depth) ->
    case ?IS_ID(ID) andalso follows_links(Path, Opts) of
        true -> lookup(link_target(Target, ID), ID, Store, Opts, Depth + 1);
        false -> {ok, Target, Path, {ok, Link}}
    end;
follow(Target, Path, Result, _Store, _Opts, _Depth) ->
    lookup_result(Target, Path, Result).

%% @doc Return the store's answer for the path that a lookup ends at. Errors
%% are returned unchanged.
lookup_result(Target, Path, Result = {ok, _}) ->
    {ok, Target, Path, Result};
lookup_result(Target, Path, Result = {composite, _}) ->
    {ok, Target, Path, Result};
lookup_result(_Target, _Path, Error) ->
    Error.

%% @doc Return whether an `ao:' value at a path should be followed as a link.
%% It is not followed in a commitment, in a value that is read as it is stored
%% (the `commitment' option), or as the value of `path' or `hashpath'.
follows_links(Path, Opts) ->
    hb_opts:get(commitment, undefined, Opts) =/= true andalso
        not lists:member(
            lists:last(binary:split(Path, <<"/">>, [global])),
            ?AO_CORE_KEYS
        ).

%% @doc Choose the ID that selects the commitment of a message read through a
%% link. If the path that was asked for is an ID, we use it. Otherwise, we use
%% the ID that the link names.
link_target(Target, ID) ->
    case ?IS_ID(Target) of
        true -> Target;
        false -> ID
    end.

%% @doc Rewrite a path whose first part is an ID that holds `ao:<ID>', such
%% that the rest of the path is read under the ID in that value. An ID with
%% keys below it is a message, so it is not an alias. We read an ID for its
%% `ao:' value only if it has no keys.
alias(Path, Store, Opts) ->
    case binary:split(Path, <<"/">>) of
        [First, Rest] when ?IS_ID(First) ->
            OneKey = #{ <<"list">> => First, <<"limit">> => 1 },
            case hb_store:list(Store, OneKey, Opts) of
                {ok, [_ | _]} -> {error, not_found};
                _ -> alias_path(hb_store:read(Store, First, Opts), Rest)
            end;
        _ ->
            {error, not_found}
    end.

%% @doc Join the rest of a path to the ID of an `ao:<ID>' value that was read.
alias_path({ok, <<"ao:", ID/binary>>}, Rest) when ?IS_ID(ID) ->
    {ok, <<ID/binary, "/", Rest/binary>>};
alias_path(_Read, _Rest) ->
    {error, not_found}.

%% @doc Read the message under a path from the store's listing of its
%% children.
read_message(Target, ResolvedFullPath, Children, Store, Opts) ->
    Subpaths =
        [
            hb_util:bin(child_name(Child))
        ||
            Child <- Children
        ],
    ?event(
        {listed,
            {original_path, Target},
            {subpaths, {explicit, Subpaths}}
        }
    ),
    Msg =
        case hb_opts:get(cache_read_mode, normal, Opts) of
            raw ->
                maps:from_list(
                    [
                        {
                            hb_escape:decode_path_component(Subpath),
                            {link,
                                hb_path:to_binary([ResolvedFullPath, Subpath]),
                                #{ <<"lazy">> => true, <<"store">> => Store }
                            }
                        }
                    ||
                        Subpath <- Subpaths,
                        Subpath =/= <<"ao-types">>
                    ]
                );
            _ ->
                Values =
                    maps:from_list([
                        {hb_util:bin(Subpath), Value}
                    ||
                        {Subpath, Value} <- Children
                    ]),
                prepare_typed_values(
                    Target,
                    ResolvedFullPath,
                    Subpaths,
                    Values,
                    Store,
                    Opts
                )
        end,
    ?event(
        {completed_read,
            {resolved_path, ResolvedFullPath},
            {explicit, Msg}
        }
    ),
    Msg.

%% @doc Normalize a composite child entry to its subpath name, dropping any
%% inline value the read supplied alongside it.
child_name({Subpath, _Value}) -> Subpath;
child_name(Subpath) -> Subpath.

%% @doc Prepare child values using `ao-types' where present. Immediate values
%% are returned directly; linked values remain lazy.
prepare_typed_values(Target, RootPath, Subpaths, Values, Store, Opts) ->
    {ok, Implicit, Types} = read_ao_types(RootPath, Subpaths, Values, Store, Opts),
    Res =
        maps:from_list(lists:filtermap(
            fun(<<"ao-types">>) -> false;
                (<<"commitments">>) ->
                    % If the message has the commitment that the target ID
                    % names, we read it into memory. Otherwise, the message is
                    % read without commitments.
                    CommPath =
                        hb_path:to_binary(
                            [RootPath, <<"commitments">>, Target]),
                    ?event(read_commitment,
                        {reading_commitment,
                            {target, Target},
                            {root_path, RootPath},
                            {commitments_path, CommPath}
                        }
                    ),
                    % If the message is read by its unsigned ID (`Target' is
                    % `RootPath'), we read the commitment of that ID from the
                    % local stores only.
                    CommOpts =
                        case Target of
                            RootPath -> hb_store:scope(Opts, local);
                            _ -> Opts
                        end,
                    case do_read_commitment(CommPath, CommOpts) of
                        {ok, Commitment} ->
                            LoadedCommitment =
                                ensure_all_loaded(
                                    Commitment,
                                    Opts#{ <<"commitment">> => true }
                                ),
                            ?event(read_commitment,
                                {found_target_commitment,
                                    {path, CommPath},
                                    {commitment, LoadedCommitment}
                                }
                            ),
                            % We have commitments, so we read each commitment
                            % into memory, and return it as part of the message.
                            {
                                true,
                                {
                                    <<"commitments">>,
                                    #{ Target => LoadedCommitment }
                                }
                            };
                        _ ->
                            false
                    end;
                (Subpath) ->
                    ?event(
                        {returning_link,
                            {subpath, Subpath}
                        }
                    ),
                    SubkeyPath = hb_path:to_binary([RootPath, Subpath]),
                    Key = hb_escape:decode_path_component(Subpath),
                    % We return the value of the subpath, or a resolvable link
                    % to it, leaving the key as-is.
                    LinkOpts =
                        (case Types of
                            #{ Key := Type } ->
                                % We have an `ao-types' entry for the
                                % subpath, so the link carries its type
                                % and resolves lazily to the final value.
                                #{
                                    <<"type">> => Type,
                                    <<"lazy">> => true
                                };
                            _ ->
                                % No `ao-types' entry: the subpath is a
                                % literal value, resolved lazily.
                                #{
                                    <<"lazy">> => true
                                }
                        end)#{ <<"store">> => Store },
                    PreparedValue =
                        case maps:get(Subpath, Values, none) of
                            Name = <<"ni:", _/binary>> ->
                                {link, Name, LinkOpts};
                            ImmediateValue when is_binary(ImmediateValue) ->
                                apply_type_to_immediate(
                                    decode_value(ImmediateValue),
                                    LinkOpts,
                                    Opts
                                );
                            _ ->
                                {link, SubkeyPath, LinkOpts}
                        end,
                    {true, {Key, key_value(Key, PreparedValue, Store, Opts)}}
                end,
            Subpaths
        )),
    Merged = maps:merge(Res, Implicit),
    % Convert the message to an ordered list if the ao-types indicate that it
    % should be so.
    case hb_maps:get(<<".">>, Types, undefined, Opts) of
        <<"list">> ->
            hb_util:message_to_ordered_list(Merged, Opts);
        _ ->
            case hb_opts:get(lazy_loading, true, Opts) of
                true -> Merged;
                false -> ensure_all_loaded(Merged, Opts)
            end
    end.

%% @doc Prepare the value of a key in a message that was read from a store. If
%% we are reading a commitment, the value is returned unchanged. The values of
%% `path' and `hashpath' are AO-Core syntax, so we load them immediately and
%% return them as they are stored. Any other value that starts with `ao:' is
%% returned as a link that is loaded from the same store as the message.
key_value(Key, Value, Store, Opts) ->
    case hb_opts:get(commitment, undefined, Opts) of
        true -> Value;
        _ ->
            case lists:member(Key, ?AO_CORE_KEYS) of
                true -> ensure_loaded(Value, Opts#{ <<"commitment">> => true });
                false when ?IS_LINK(Value) -> Value;
                false ->
                    case hb_link:decode(Value) of
                        {link, URI, _} ->
                            {link, URI, #{ <<"store">> => Store }};
                        Literal -> Literal
                    end
            end
    end.

apply_type_to_immediate(Value, #{ <<"type">> := Type }, Opts) ->
    structured_decode_value(Type, Value, Opts);
apply_type_to_immediate(Value, _LinkOpts, _Opts) ->
    Value.

%% @doc Read and parse the ao-types for a given path if it is in the supplied
%% list of subpaths, returning a map of keys and their types. Composite reads
%% already return small inline `ao-types' values, so reuse them when present.
%% An `ao-types' that does not parse is returned as a key of the message, and
%% gives no types.
read_ao_types(Path, Subpaths, Values, Store, Opts) ->
    ?event_debug({reading_ao_types, {path, Path}, {subpaths, {explicit, Subpaths}}}),
    case lists:member(<<"ao-types">>, Subpaths) of
        true ->
            TypesPath = hb_path:to_binary([Path, <<"ao-types">>]),
            TypesBin =
                case Values of
                    #{ <<"ao-types">> := <<"ni:", _/binary>> } ->
                        hb_util:ok(store_read(TypesPath, Store, Opts));
                    #{ <<"ao-types">> := Direct } when is_binary(Direct) ->
                        decode_value(Direct);
                    _ ->
                        hb_util:ok(store_read(TypesPath, Store, Opts))
                end,
            case structured_decode_types(TypesBin, Opts) of
                {ok, Types} ->
                    ?event_debug({parsed_ao_types, {types, Types}}),
                    {ok, types_to_implicit(Types), Types};
                {error, _} ->
                    {ok, #{ <<"ao-types">> => TypesBin }, #{}}
            end;
        false ->
            ?event_debug({no_ao_types_key_found, {path, Path}, {subpaths, Subpaths}}),
            {ok, #{}, #{}}
    end.

%% @doc Convert a map of ao-types to an implicit map of types.
types_to_implicit(Types) ->
    maps:filtermap(
        fun(_K, <<"empty-message">>) -> {true, #{}};
           (_K, <<"empty-list">>) -> {true, []};
           (_K, <<"empty-binary">>) -> {true, <<>>};
           (_, _) -> false
        end,
        Types
    ).

%% @doc Decode the `ao-types' field through the structured device. An empty
%% value is the `ao-types' of a message with no keys, and gives no types.
structured_decode_types(<<>>, _Opts) -> {ok, #{}};
structured_decode_types(Types, Opts) ->
    hb_ao:raw(
        <<"structured@1.0">>,
        <<"decode-types">>,
        Types,
        #{},
        Opts
    ).

%% @doc Decode a value of the given type through the structured device.
structured_decode_value(Type, Value, Opts) ->
    hb_util:ok(
        hb_ao:raw(
            <<"structured@1.0">>,
            <<"decode-value">>,
            Value,
            #{ <<"type">> => Type },
            Opts
        )
    ).


%% @doc Read the result of a computation, using heuristics. The supported
%% heuristics are as follows:
%% 1. If the base message is an ID, we try to determine if the message has an
%% explicit device. If it does not, we can simply read the key and return it if
%% it exists, as this is the behavior of `message@1.0'.
%% 2. If the base message is loaded (a map), we determine if it has an explicit,
%% non-direct data access device. If it does, we simply read the key from the
%% message and return it if it exists.
%% 3. If the message has an explicit device, we attempt to read the hashpath to
%% see if it has already been computed.
%% A key is read as `message@1.0' reads it: lowercased once, then exactly.
read_resolved(BaseMsg, Key, Opts) when is_binary(Key) ->
    read_resolved(BaseMsg, #{ <<"path">> => Key }, Opts);
read_resolved({link, ID, LinkOpts}, Req, Opts) ->
    read_resolved(ID, Req, maps:merge(LinkOpts, Opts));
read_resolved(BaseMsgID, Req = #{ <<"path">> := Key }, Opts) when ?IS_ID(BaseMsgID) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    NormKey = hb_util:lowercase_key(hb_ao:normalize_key(Key, Opts)),
    case hb_device:is_direct_key_access(BaseMsgID, Req, Opts, Store) of
        unknown -> miss;
        false ->
            ?event_debug(read_cached,
                {found_non_message_device,
                    {key, NormKey}
                }
            ),
            read_computation(BaseMsgID, Req, Opts);
        true ->
            % Either the message does not exist in the store, or there is no
            % explicit device in the message. If the message exists this implies
            % that the default (`message@1.0`) device will be used to execute
            % the key. Subsequently, we can simply read the key and return it if
            % it exists.
            ?event_debug(read_cached,
                {skipping_execution_store_lookup,
                    {base_msg, BaseMsgID},
                    {key, NormKey}
                }
            ),
            KeyPath =
                hb_path:to_binary(
                    [BaseMsgID, hb_escape:encode_path_component(NormKey)]
                ),
            hashpath_read_result(read(KeyPath, Opts))
    end;
read_resolved(BaseMsg, Req = #{ <<"path">> := Key }, Opts) when is_map(BaseMsg) ->
    % The base message is loaded, so we determine if it has an explicit device
    % and perform a direct lookup if it does not.
    NormKey = hb_util:lowercase_key(hb_ao:normalize_key(Key, Opts)),
    case hb_device:is_direct_key_access(BaseMsg, Req, Opts) of
        false -> read_computation(BaseMsg, Req, Opts);
        true ->
            ?event_debug(read_cached,
                {skip_execution_memory_lookup,
                    {path, NormKey}
                }
            ),
            {hit,
                read_in_memory_key(
                    hb_ao:execution_input(BaseMsg, Opts), NormKey, Opts)}
    end;
read_resolved(Base, Req, Opts) ->
    read_computation(Base, Req, Opts).

%% @doc Return a key from an in-memory message, returning the same form as
%% a store read (`{Status, Value}').
read_in_memory_key(BaseMsg, NormKey, Opts) ->
    case hb_maps:find(NormKey, BaseMsg, Opts) of
        error ->
            ?event_debug(read_cached, {key_not_found, {key, NormKey}}),
            {error, not_found};
        {ok, Value} ->
            ?event_debug(read_cached, {key_found, {key, NormKey}}),
            {ok, Value}
    end.

%% @doc Read the output of a prior computation, given BaseMsg and Req.
read_computation(BaseMsgID, ReqID, Opts) when ?IS_ID(BaseMsgID) and ?IS_ID(ReqID) ->
    ?event_debug({cache_lookup, {base, BaseMsgID}, {req, ReqID}, {opts, Opts}}),
    Key = <<BaseMsgID/binary, "/", ReqID/binary>>,
    case hb_store:read(hb_opts:get(attested_store, [], Opts), Key, Opts) of
        {ok, Result = <<"ao:", _/binary>>} ->
            hashpath_read_result(read(Result, Opts));
        {ok, Result = <<"ni:", _/binary>>} ->
            hashpath_read_result(read(Result, Opts));
        _ -> miss
    end;
read_computation(BaseMsgID, Req, Opts) when ?IS_ID(BaseMsgID) and is_map(Req) ->
    read_computation(BaseMsgID, hb_message:id(Req, all, Opts), Opts);
read_computation(BaseMsg, Req, Opts) when is_map(BaseMsg) and is_map(Req) ->
    read_computation(hb_message:id(BaseMsg, all, Opts), Req, Opts);
read_computation(_, _, _) -> miss.

hashpath_read_result({ok, Msg}) -> {hit, {ok, Msg}};
hashpath_read_result({error, not_found}) -> miss;
hashpath_read_result(Other) -> {hit, Other}.

%% @doc Make a link from a path to an existing message or binary. For the ID
%% of a message, we write `ao:<ID>' at the path. For the `ni:' name of a
%% binary, as `write/2' returns it, we write the name.
%% Note: Argument order is `link(Src, Dst, Opts)'.
link(Existing, New, Opts) ->
    NewPath = hb_path:to_binary(New),
    check_key_length(NewPath),
    hb_store:write(
        hb_opts:get(store, no_viable_store, Opts),
        #{ NewPath => link_value(Existing) },
        Opts
    ).

link_value(Name = <<"ni:", _/binary>>) -> hb_escape:decode_path_component(Name);
link_value(ID) when ?IS_ID(ID) -> <<"ao:", ID/binary>>.

%%% Tests

test_unsigned(Data) ->
    #{
        <<"base-test-key">> => <<"base-test-value">>,
        <<"other-test-key">> => Data
    }.

%% Helper function to create signed #tx items.
test_signed(Data) -> test_signed(Data, #{ <<"priv-wallet">> => ar_wallet:new() }).
test_signed(Data, Opts) ->
    hb_message:commit(test_unsigned(Data), Opts).

test_store_binary(Store) ->
    Bin = <<"Simple unsigned data item">>,
    ?event_debug(debug_store_test, {store, Store}),
    Opts = #{ <<"store">> => Store },
    {ok, ID} = write(Bin, Opts),
    {ok, RetrievedBin} = read(ID, Opts),
    ?assertEqual(Bin, RetrievedBin),
    % A binary is stored under its `ni:' name, which reads it back.
    Name = ?NI_URI(hb_path:hashpath(Bin, Opts)),
    ?assertEqual({ok, Bin}, read(Name, Opts)),
    % Reading a path that is linked to the name returns the binary.
    ok = link(Name, <<"named">>, Opts),
    ?assertEqual({ok, Bin}, read(<<"named">>, Opts)),
    % A binary whose bytes are a store marker reads back as those bytes.
    {ok, Marker} = write(<<"group">>, Opts),
    ?assertEqual({ok, <<"group">>}, read(Marker, Opts)),
    % A key with a value of 60 bytes or more contains the value's `ni:' name.
    Long = binary:copy(<<"x">>, 60),
    {ok, LongID} = write(#{ <<"long">> => Long }, Opts),
    ?assertEqual(
        {ok, ?NI_URI(hb_path:hashpath(Long, Opts))},
        hb_store:read(Store, <<LongID/binary, "/long">>, Opts)
    ).

test_store_unsigned_empty_message(Store) ->
    ?event_debug(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Item = #{},
    Opts = #{ <<"store">> => Store },
    {ok, Path} = write(Item, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?event(
        {retrieved_item,
            {path, {string, Path}},
            {expected, Item},
            {got, RetrievedItem}
        }
    ),
    MatchRes = hb_message:match(Item, RetrievedItem, strict, Opts),
    ?event_debug({match_result, MatchRes}),
    ?assert(MatchRes).

test_store_unsigned_nested_empty_message(Store) ->
    ?event_debug(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Item =
        #{ <<"layer1">> =>
            #{ <<"layer2">> =>
                #{ <<"layer3">> =>
                    #{ <<"a">> => <<"b">>}
                },
                <<"layer3b">> => #{ <<"c">> => <<"d">>},
                <<"layer3c">> => #{}
            }
        },
    Opts = #{ <<"store">> => Store },
    % The write computes one HMAC, the unsigned ID, of each of the 6 messages.
    erlang:trace_pattern({crypto, mac, 4}, true, [call_time]),
    erlang:trace(self(), true, [call]),
    {ok, Path} = write(Item, Opts),
    {call_time, [{_, HMACs, _, _}]} =
        erlang:trace_info({crypto, mac, 4}, call_time),
    erlang:trace_pattern({crypto, mac, 4}, false, [call_time]),
    ?assertEqual(6, HMACs),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Item, RetrievedItem, strict, Opts)).

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_unsigned_message(Store) ->
    Item = test_unsigned(<<"Simple unsigned data item">>),
    ?event_debug(debug_store_test, {store, Store}),
    Opts = #{ <<"store">> => Store },
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    ID = hb_util:human_id(hb_ao:get(id, Item)),
    {ok, RetrievedItem} = read(ID, Opts),
    ?assert(hb_message:match(Item, RetrievedItem, strict, Opts)),
    ok.

test_store_ans104_message(Store) ->
    ?event_debug(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    Item = #{ <<"type">> => <<"ANS104">>, <<"content">> => <<"Hello, world!">> },
    Committed = hb_message:commit(Item, #{ <<"priv-wallet">> => hb:wallet() }),
    {ok, _Path} = write(Committed, Opts),
    CommittedID = hb_util:human_id(hb_message:id(Committed, all)),
    UncommittedID = hb_util:human_id(hb_message:id(Committed, none)),
    ?event_debug({test_message_ids, {uncommitted, UncommittedID}, {committed, CommittedID}}),
    {ok, RetrievedItem} = read(CommittedID, Opts),
    {ok, RetrievedItemU} = read(UncommittedID, Opts),
    ?assert(hb_message:match(Committed, RetrievedItem, strict, Opts)),
    ?assert(hb_message:match(Committed, RetrievedItemU, strict, Opts)),
    ok.

%% @doc A signed message whose committed key changed after it was signed is
%% written without its signature: the signed ID is never linked to content
%% that the signature does not cover, while the message as signed is.
test_store_modified_signed_message(Store) ->
    Opts = #{ <<"store">> => Store, <<"priv-wallet">> => ar_wallet:new() },
    hb_store:reset(Store),
    Signed =
        hb_message:commit(
            #{ <<"path">> => <<"/~meta@1.0/info">>, <<"x">> => <<"1">> },
            Opts
        ),
    SignedID = hb_message:id(Signed, signed, Opts),
    Changed = Signed#{ <<"x">> => <<"2">> },
    Unsigned = hb_message:normalize_commitments(hb_message:uncommitted(Changed), Opts),
    Mixed = Unsigned#{
        <<"commitments">> => maps:merge(
            maps:get(<<"commitments">>, Unsigned),
            maps:get(<<"commitments">>, Signed))
    },
    InMetadata = Unsigned#{
        <<"commitments">> => maps:map(
            fun(_, Comm) -> Comm#{ <<"child">> => Mixed } end,
            maps:get(<<"commitments">>, Unsigned))
    },
    lists:foreach(fun(Msg) ->
        {ok, _} = write(Msg, Opts),
        ?assertEqual({error, not_found}, read(SignedID, Opts))
    end, [Changed, Mixed, #{ <<"child">> => Mixed },
        #{ <<"children">> => [Mixed] }, InMetadata]),
    {ok, _} = write(hb_message:normalize_commitments(Signed, Opts), Opts),
    {ok, Stored} = read(SignedID, Opts),
    Loaded = ensure_all_loaded(Stored, Opts),
    ?assertEqual(<<"1">>, hb_maps:get(<<"x">>, Loaded, not_found, Opts)),
    ?assert(hb_message:verify(Loaded, all, Opts)),
    ok.

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_signed_message(Store) ->
    ?event_debug(debug_store_test, {store, Store}),
    Opts = #{ <<"store">> => Store },
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Address = hb_test_utils:committer(Wallet),
    Item = test_signed(<<"Simple signed data item">>, #{ <<"priv-wallet">> => Wallet }),
    ?event_debug({writing_test_message, Item}),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    % %% Read the item back
    % UID = hb_message:id(Item, none, Opts),
    % {ok, RetrievedItemUnsig} = read(UID, Opts),
    % ?event_debug({retreived_unsigned_message, {expected, Item}, {got, RetrievedItemUnsig}}),
    % MatchRes = hb_message:match(Item, RetrievedItemUnsig, strict, Opts),
    % ?event_debug({match_result, MatchRes}),
    % ?assert(MatchRes),
    CommittedID = hb_message:id(Item, [Address], Opts),
    % The signed ID holds `ao:' and the unsigned ID.
    UID = hb_message:id(Item, none, Opts),
    ?assertEqual(
        {ok, <<"ao:", UID/binary>>},
        hb_store:read(Store, CommittedID, Opts)
    ),
    % A path under the signed ID is read under the unsigned ID in the local
    % stores before a remote store is asked for it.
    Remote = (hb_test_utils:test_store(hb_store_fs, <<"remote">>))#{
        <<"scope">> => remote
    },
    KeyPath = <<CommittedID/binary, "/base-test-key">>,
    ok = hb_store:write(Remote, #{ KeyPath => <<"remote">> }, Opts),
    ?assertEqual(
        {ok, <<"base-test-value">>},
        read(KeyPath, Opts#{ <<"store">> => [Store, Remote] })
    ),
    % A read of the unsigned ID asks no remote store for its commitment.
    UnsignedCommitment = <<UID/binary, "/commitments/", UID/binary>>,
    ok = hb_store:write(Remote, #{ UnsignedCommitment => <<"remote">> }, Opts),
    ?assertNotMatch(
        {ok, #{ <<"commitments">> := _ }},
        read(UID, Opts#{ <<"store">> => [Store, Remote] })
    ),
    {ok, RetrievedItemSigned} = read(CommittedID, Opts),
    ?event_debug({retrieved_signed_message, {expected, Item}, {got, RetrievedItemSigned}}),
    MatchResSigned = 
        hb_message:match(
            Item,
            hb_message:normalize_commitments(RetrievedItemSigned, Opts),
            strict,
            Opts
        ),
    ?event_debug({match_result_signed, MatchResSigned}),
    ?assert(MatchResSigned),
    ok.

%% @doc A message with an empty key is refused: the write throws, and the store
%% holds nothing at the message's ID or at the path of any other key.
test_empty_key_refused(Store) ->
    ?event_debug(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    Msg = #{ <<>> => <<"v">> },
    ID = hb_message:id(Msg, unsigned, Opts),
    ?assertThrow({invalid_key, <<>>}, write(Msg, Opts)),
    ?assertEqual({error, not_found}, read(ID, Opts)),
    Nested = #{ <<"a">> => #{ <<>> => <<"v">> } },
    NestedID = hb_message:id(Nested, unsigned, Opts),
    ?assertThrow({invalid_key, <<>>}, write(Nested, Opts)),
    ?assertEqual({error, not_found}, read(NestedID, Opts)),
    Beside = #{ <<"a">> => <<"1">>, <<>> => <<"v">> },
    BesideID = hb_message:id(Beside, unsigned, Opts),
    ?assertThrow({invalid_key, <<>>}, write(Beside, Opts)),
    ?assertEqual({error, not_found}, read(BesideID, Opts)),
    ?assertEqual({error, not_found}, read(<<BesideID/binary, "/a">>, Opts)).

%% @doc Test deeply nested item storage and retrieval
test_deeply_nested_complex_message(Store) ->
    ?event_debug(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Opts = #{ <<"store">> => Store, <<"priv-wallet">> => Wallet },
    %% Create nested data
    Level3SignedSubmessage = test_signed([1,2,3], Opts),
    Outer =
        hb_message:commit(
            #{
                <<"level1">> =>
                    InnerSigned = hb_message:commit(
                        #{
                            <<"level2">> =>
                                #{
                                    <<"level3">> => Level3SignedSubmessage,
                                    <<"e">> => <<"f">>,
                                    <<"z">> => [1,2,3]
                                },
                            <<"c">> => <<"d">>,
                            <<"g">> => [<<"h">>, <<"i">>],
                            <<"j">> => 1337
                        },
                        Opts
                    ),
                <<"a">> => <<"b">>
            },
            Opts
        ),
    UID = hb_message:id(Outer, none, Opts),
    ?event_debug({string, <<"================================================">>}),
    CommittedID = hb_message:id(Outer, signed, Opts),
    ?event_debug({string, <<"================================================">>}),
    ?event_debug({test_message_ids, {uncommitted, UID}, {committed, CommittedID}}),
    %% Write the nested item
    {ok, _} = write(Outer, Opts),
    %% Read the deep value back using subpath
	OuterID = hb_util:human_id(UID),
    {ok, OuterMsg} = read(OuterID, Opts),
	EnsuredLoadedOuter = hb_cache:ensure_all_loaded(OuterMsg, Opts),
    ?event_debug({deep_message, {explicit, EnsuredLoadedOuter}}),
    %% Assert that the retrieved item matches the original deep value
    ?assertEqual(
        [1,2,3],
        hb_ao:get(
            <<"level1/level2/level3/other-test-key">>,
            EnsuredLoadedOuter,
            Opts
        )
    ),
    ?event(
        {deep_message_match,
            {read, EnsuredLoadedOuter},
            {write, Level3SignedSubmessage}
        }
    ),
    ?event_debug({reading_committed_outer, {id, CommittedID}, {expect, Outer}}),
    {ok, CommittedMsg} = read(hb_util:human_id(CommittedID), Opts),
	EnsuredLoadedCommitted = hb_cache:ensure_all_loaded(CommittedMsg, Opts),
	?assertEqual(
        [1,2,3],
        hb_ao:get(
            <<"level1/level2/level3/other-test-key">>,
            EnsuredLoadedCommitted,
            Opts
        )
    ).

test_message_with_list(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    Msg = test_unsigned([<<"a">>, <<"b">>, <<"c">>]),
    ?event_debug({writing_message, Msg}),
    {ok, Path} = write(Msg, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Msg, RetrievedItem, strict, Opts)).

test_match_message(Store) when map_get(<<"store-module">>, Store) =/= hb_store_lmdb ->
    skip;
test_match_message(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store, <<"match-index">> => false },
    % Write two messages that match the template, and a third that does not.
    {ok, ID1} = hb_cache:write(#{ <<"x">> => <<"1">> }, Opts),
    {ok, ID2} = hb_cache:write(#{ <<"y">> => <<"2">>, <<"z">> => <<"3">> }, Opts),
    {ok, ID2b} = hb_cache:write(#{ <<"x">> => <<"4">>, <<"z">> => <<"3">> }, Opts),
    {ok, ID3} = hb_cache:write(#{ <<"z">> => <<"5">>, <<"c">> => <<"d">> }, Opts),
    % Match the template, and ensure that we get two matches.
    {ok, MatchedItems} = match(#{ <<"z">> => <<"3">> }, Opts),
    ?assertEqual(2, length(MatchedItems)),
    ?assert(
        lists:all(
            fun(ID) ->
                {ok, Msg} = read(ID, Opts),
                hb_maps:get(<<"z">>, Msg, Opts) =:= <<"3">> andalso
                    lists:member(ID, [ID2, ID2b])
            end,
            MatchedItems
        )
    ),
    {ok, MatchedItems2} = match(#{ <<"x">> => <<"4">> }, Opts),
    ?assertEqual(1, length(MatchedItems2)),
    ?assertEqual([ID2b], MatchedItems2),
    % A match on the keys of a commitment returns the commitment's ID.
    Wallet = ar_wallet:new(),
    Committer =
        hb_committer:uri(hb_util:human_id(ar_wallet:to_address(Wallet))),
    Signed = hb_message:commit(#{ <<"w">> => <<"1">> }, Opts#{
        <<"priv-wallet">> => Wallet
    }),
    {ok, _} = hb_cache:write(Signed, Opts),
    ?assertEqual(
        {ok, [hb_message:id(Signed, signed, Opts)]},
        match(#{ <<"committer">> => Committer }, Opts)
    ).

test_match_linked_message(Store) when map_get(<<"store-module">>, Store) =/= hb_store_lmdb ->
    skip;
test_match_linked_message(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store, <<"match-index">> => false },
    Msg = #{ <<"a">> => Inner = #{ <<"b">> => <<"c">>, <<"d">> => <<"e">> } },
    {ok, _ID} = write(Msg, Opts),
    {ok, [MatchedID]} = match(#{ <<"b">> => <<"c">> }, Opts),
    {ok, Read1} = read(MatchedID, Opts),
    ?assertEqual(
        #{ <<"b">> => <<"c">>, <<"d">> => <<"e">> },
        hb_cache:ensure_all_loaded(Read1, Opts)
    ),
    {ok, [MatchedID2]} = match(#{ <<"a">> => Inner }, Opts),
    {ok, Read2} = read(MatchedID2, Opts),
    ?assertEqual(
        #{ <<"a">> => Inner },
        ensure_all_loaded(Read2, Opts)
    ).

test_match_typed_message(Store) when map_get(<<"store-module">>, Store) =/= hb_store_lmdb ->
    skip;
test_match_typed_message(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store, <<"match-index">> => false },
    % Add some messages that should not match the template, as well as the main
    % message that should match the template.
    write(#{ <<"atom-value">> => atom, <<"wrong">> => <<"wrong">> }, Opts),
    write(#{ <<"integer-value">> => 1337, <<"wrong">> => <<"wrong-2">> }, Opts),
    Msg =
        #{
            <<"int-key">> => 1337,
            <<"other-key">> => <<"other-test-value">>,
            <<"atom-key">> => atom
        },
    {ok, _ID} = write(Msg, Opts),
    {ok, [MatchedID]} = match(#{ <<"int-key">> => 1337 }, Opts),
    {ok, Read1} = read(MatchedID, Opts),
    ?assertEqual(
        Msg,
        ensure_all_loaded(Read1, Opts)
    ),
    {ok, [MatchedID2]} = match(#{ <<"atom-key">> => atom }, Opts),
    {ok, Read2} = read(MatchedID2, Opts),
    ?assertEqual(
        Msg,
        ensure_all_loaded(Read2, Opts)
    ).

test_raw_match_read(Store) when map_get(<<"store-module">>, Store) =/= hb_store_lmdb ->
    skip;
test_raw_match_read(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    Msg = #{
        <<"content-type">> => <<"application/beam">>,
        <<"implements-device">> => <<"test-spec">>,
        <<"body">> => <<"beam-bytes">>
    },
    {ok, ID} = write(Msg, Opts),
    RawOpts = Opts#{
        <<"cache-read-mode">> => raw,
        <<"match-index">> => [Store]
    },
    ?assertEqual(
        {ok, [ID]},
        match(#{ <<"implements-device">> => <<"test-spec">> }, RawOpts)
    ),
    {ok, RawMsg} = read(ID, RawOpts),
    ?assertEqual(
        <<"beam-bytes">>,
        hb_maps:get(<<"body">>, RawMsg, undefined, RawOpts)
    ).

%% @doc Literal path separators and escapes remain distinct from nested keys.
test_literal_keys(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store, <<"match-index">> => false },
    Binaries = #{
        <<"a/b">> => <<"literal slash">>,
        <<"a%2fb">> => <<"literal escape">>,
        <<"a%252fb">> => <<"repeated escape">>,
        <<"%25/">> => <<"adjacent escapes">>,
        <<"/leading//trailing/">> => binary:copy(<<"large value">>, 10)
    },
    Msg = Binaries#{
        <<"a">> => #{ <<"b">> => <<"nested">> },
        <<"typed%2fkey">> => 42,
        <<"empty%2flist">> => [],
        <<"nested/map">> => #{ <<"inner/key%25">> => [1, 2, 3] }
    },
    {ok, ID} = write(Msg, Opts),
    {ok, Read} = read(ID, Opts),
    ?assertEqual(Msg, ensure_all_loaded(Read, Opts)),
    RawOpts = Opts#{ <<"cache-read-mode">> => raw },
    {ok, Raw} = read(ID, RawOpts),
    maps:foreach(
        fun(Key, Value) ->
            ?assertEqual(Value, hb_maps:get(Key, Raw, RawOpts)),
            ?assertEqual({hit, {ok, Value}}, read_resolved(ID, Key, Opts)),
            case map_get(<<"store-module">>, Store) of
                hb_store_lmdb ->
                    ?assertEqual({ok, [ID]}, match(#{ Key => Value }, Opts)),
                    ?assertEqual({ok, [ID]}, match(#{ Key => Value }, RawOpts));
                _ ->
                    ok
            end
        end,
        Binaries
    ).

%% @doc ANS-104 commitments still verify after caching literal slash tags.
test_signed_literal_keys(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    Signed =
        hb_message:commit(
            #{
                <<"@kyvejs/protocol">> => <<"1.4.2">>,
                <<"@kyvejs/tendermint-bsync">> => <<"1.2.11">>,
                <<"@kyvejs%2fprotocol">> => <<"distinct tag">>,
                <<"body">> => <<"Signed data">>
            },
            #{ <<"priv-wallet">> => ar_wallet:new(ethereum) },
            #{
                <<"commitment-device">> => <<"ans104@1.0">>,
                <<"type">> => <<"ethereum">>
            }
        ),
    ?assert(hb_message:verify(Signed, all, Opts)),
    ID = hb_message:id(Signed, signed, Opts),
    {ok, _} = write(Signed, Opts),
    {ok, Read} = read(ID, Opts),
    Loaded = ensure_all_loaded(Read, Opts),
    ?assertEqual(Signed, Loaded),
    ?assert(hb_message:verify(Loaded, all, Opts)),
    ?assertEqual(ID, hb_message:id(Loaded, signed, Opts)),
    ?assert(is_map(hb_message:convert(Loaded, <<"httpsig@1.0">>, Opts))).

%% @doc Ensure that a value of the form `ao:<ID>' is read back as a link to the
%% message with that ID. A `hashpath' value is read back as it was written, and
%% a key that ends in `+link' is stored like any other key.
test_link_value(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    {ok, OtherID} = write(#{ <<"target">> => <<"value">> }, Opts),
    Hashpath = <<"ao://", OtherID/binary, "/target">>,
    {ok, ID} =
        write(
            #{
                <<"k">> => <<"ao:", OtherID/binary>>,
                <<"hashpath">> => Hashpath,
                <<"k+link">> => OtherID
            },
            Opts
        ),
    {ok, Read} = read(ID, Opts),
    ?assertEqual(
        #{
            <<"k">> => #{ <<"target">> => <<"value">> },
            <<"hashpath">> => Hashpath,
            <<"k+link">> => OtherID
        },
        ensure_all_loaded(Read, Opts)
    ),
    % A chain of more than 32 links is refused.
    [A, B] = [hb_util:human_id(crypto:strong_rand_bytes(32)) || _ <- [a, b]],
    ok =
        hb_store:write(
            Store,
            #{ A => <<"ao:", B/binary>>, B => <<"ao:", A/binary>> },
            Opts
        ),
    ?assertEqual({error, 'too-many-links'}, read(A, Opts)).

test_immediate_marker_values(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store, <<"match-index">> => false },
    Msg = #{
        <<"groupish">> => <<"group">>,
        <<"linkish">> => <<"link:literal">>,
        <<"rawish">> => <<"raw:literal">>,
        <<"niish">> => <<"ni:literal">>
    },
    {ok, ID} = write(Msg, Opts),
    {ok, Read} = read(ID, Opts),
    ?assertEqual(<<"group">>, hb_maps:get(<<"groupish">>, Read, Opts)),
    ?assertEqual(<<"link:literal">>, hb_maps:get(<<"linkish">>, Read, Opts)),
    ?assertEqual(<<"raw:literal">>, hb_maps:get(<<"rawish">>, Read, Opts)),
    % A short value that starts with `ni:' is written as a `data:' URI.
    ?assertEqual(<<"ni:literal">>, hb_maps:get(<<"niish">>, Read, Opts)),
    ?assertEqual(
        {ok, <<"data:,ni%3aliteral">>},
        hb_store:read(Store, <<ID/binary, "/niish">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"link:literal">>},
        hb_store:read(Store, <<ID/binary, "/linkish">>, Opts)
    ),
    % A `data:' value that is not percent-encoded is refused.
    Bad = <<ID/binary, "/bad">>,
    ok = hb_store:write(Store, #{ Bad => <<"data:,%zz">> }, Opts),
    ?assertThrow({'invalid-data-uri', _}, read(Bad, Opts)),
    case map_get(<<"store-module">>, Store) of
        hb_store_lmdb ->
            ?assertEqual({ok, [ID]}, match(#{ <<"rawish">> => <<"raw:literal">> }, Opts));
        _ ->
            ok
    end.

%% @doc The `cache-write' hook receives each message the cache writes with its
%% private keys, which are not written, under the IDs it is written at.
test_cache_write_hook(Store) ->
    hb_store:reset(Store),
    Self = self(),
    Opts = #{
        <<"store">> => Store,
        <<"priv-wallet">> => ar_wallet:new(),
        <<"on">> => #{
            <<"cache-write">> => [
                #{
                    <<"device">> => #{
                        cache_write =>
                            fun(_, Req, _) -> Self ! Req, {ok, Req} end
                    }
                },
                #{ <<"device">> => <<"match@1.0">>, <<"path">> => <<"index">> }
            ]
        }
    },
    Committed = hb_message:commit(#{ <<"a">> => <<"b">> }, Opts),
    Msg = hb_private:set(Committed, <<"offset">>, 1, Opts),
    {ok, ID} = write(Msg, Opts),
    Req = receive R = #{ <<"body">> := #{ <<"a">> := _ } } -> R end,
    #{ <<"body">> := Body, <<"signed-ids">> := IDs } = Req,
    ?assertEqual(<<"kernel">>, hb_private:get(<<"hook-caller">>, Req, Opts)),
    ?assertEqual(1, hb_private:get(<<"offset">>, Body, Opts)),
    ?assertEqual([hb_message:id(Msg, signed, Opts)], IDs),
    ?assertEqual({ok, IDs}, match(#{ <<"a">> => <<"b">> }, Opts)),
    {ok, Keys} = hb_store:list(Store, ID, Opts),
    ?assertNot(lists:member(<<"priv">>, Keys)).

cache_suite_test_() ->
    hb_store:generate_test_suite([
        {"store unsigned empty message",
            fun test_store_unsigned_empty_message/1},
        {"store binary", fun test_store_binary/1},
        {"store unsigned nested empty message",
            fun test_store_unsigned_nested_empty_message/1},
        {"store simple unsigned message", fun test_store_simple_unsigned_message/1},
        {"store simple signed message", fun test_store_simple_signed_message/1},
        {"store modified signed message",
            fun test_store_modified_signed_message/1},
        {"empty key refused", fun test_empty_key_refused/1},
        {"deeply nested complex message", fun test_deeply_nested_complex_message/1},
        {"message with list", fun test_message_with_list/1},
        {"match message", fun test_match_message/1},
        {"match linked message", fun test_match_linked_message/1},
        {"match typed message", fun test_match_typed_message/1},
        {"raw match read", fun test_raw_match_read/1},
        {"literal keys", fun test_literal_keys/1},
        {"link value", fun test_link_value/1},
        {"signed literal keys", fun test_signed_literal_keys/1},
        {"immediate marker values", fun test_immediate_marker_values/1},
        {"cache-write hook", fun test_cache_write_hook/1}
    ]).

%% @doc Test that message whose device is `#{}' cannot be written. If it were to
%% be written, it would cause an infinite loop.
test_device_map_cannot_be_written_test() ->
    try
        Opts = #{ <<"store">> => StoreOpts =
            [#{ <<"store-module">> => hb_store_fs, <<"name">> => <<"cache-TEST">> }] },
        hb_store:reset(StoreOpts),
        Danger = #{ <<"device">> => #{}},
        write(Danger, Opts),
        ?assert(false)
    catch
        _:_:_ -> ?assert(true)
    end.

%% @doc Cache writes are best-effort with respect to the configured store
%% list: a list whose only entry rejects write-class operations (e.g. a
%% read-only store) must still yield `{ok, ID}', not crash. Exercises both
%% the binary path and the map path, so every store write in
%% `do_write_message' is covered.
write_with_only_read_only_store_test() ->
    ReadOnlyStore = #{
        <<"store-module">> => hb_store_volatile,
        <<"name">> => <<"cache-readonly">>,
        <<"access">> => [<<"read">>]
    },
    Opts = #{ <<"store">> => [ReadOnlyStore] },
    ?assertMatch({ok, _}, write(<<"some-binary-payload">>, Opts)),
    ?assertMatch({ok, _}, write(#{ <<"hello">> => <<"world">> }, Opts)).

%% @doc Nested links retain their source store outside the caller's store chain.
isolated_nested_store_test() ->
    Store = hb_test_utils:test_store(hb_store_volatile, <<"isolated-nested">>),
    ok = hb_store:start(Store),
    Opts = #{ <<"store">> => [Store] },
    Msg = #{ <<"child">> => #{ <<"value">> => <<"nested">> } },
    {ok, ID} = write(Msg, Opts),
    {ok, Lazy} = read(ID, Opts),
    ?assertEqual(Msg, ensure_all_loaded(Lazy, #{ <<"store">> => [] })),
    ok = hb_store:stop(Store).

%% @doc A message changed after normalization is written under the ID of its
%% content, not the ID of its stale unsigned commitment.
write_changed_normalized_message_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Msg = hb_message:normalize_commitments(#{ <<"a">> => 1 }, Opts),
    {ok, ID} = write(Msg, Opts),
    {ok, ChangedID} = write(Msg#{ <<"a">> => 2 }, Opts),
    ?assertEqual(hb_message:id(#{ <<"a">> => 2 }, all, Opts), ChangedID),
    {ok, Read} = read(ID, Opts),
    ?assertEqual(1, hb_maps:get(<<"a">>, Read, undefined, Opts)).

%% @doc Projecting mixed commitments cannot retain an ID covering a removed
%% field, either at the root or in a child of another message.
projected_commitments_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ar_wallet:new() },
    Signed = hb_message:commit(#{ <<"a">> => <<"1">> }, Opts),
    Mixed = hb_message:normalize_commitments(Signed#{ <<"extra">> => <<"2">> }, Opts),
    FullID = hb_message:id(Mixed, unsigned, Opts),
    lists:foreach(fun(Msg) ->
        {ok, ID} = write(Msg, Opts),
        {ok, Stored} = read(ID, Opts),
        Loaded = ensure_all_loaded(Stored, Opts),
        ?assert(hb_message:paranoid_verify(Loaded, Opts#{ <<"paranoid-verify">> => true })),
        ?assertEqual(ID, hb_message:id(hb_message:uncommitted(Loaded, Opts), all, Opts))
    end, [Mixed, #{ <<"child">> => Mixed }, #{ <<"children">> => [Mixed] }]),
    {ok, Kept} = read(hb_message:id(Signed, all, Opts), Opts),
    ?assertEqual(hb_message:signers(Signed, Opts), hb_message:signers(Kept, Opts)),
    % Commitments over different keys are written separately.
    {ok, Full} = read(FullID, Opts),
    ?assertEqual(hb_message:uncommitted(Mixed, Opts),
        hb_message:uncommitted(ensure_all_loaded(Full, Opts), Opts)),
    ?assert(hb_message:verify(Full, #{ <<"ids">> => [FullID] }, Opts)).

%% @doc Run a specific test with a given store module.
run_test() ->
    Store = hb_test_utils:test_store(),
    test_match_typed_message(Store).
