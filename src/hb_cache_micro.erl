%%% @doc A minimized implementation of the `hb_ao' cache, matching with the
%%% `hb_ao_micro' module. Supports `write' and `read' operations, with minimal
%%% type-tagging and untagging support.
-module(hb_cache_micro).
-export([resolve/2, read/2, write/2, link/3]).
-include("include/hb.hrl").

-define(DEFAULT_SCOPE, local).

%% @doc Resolve a link or a raw path to a simple prefix (either to a raw binary
%% value or a collection of hashpaths representing a message).
resolve(Path, Opts) when not ?IS_LINK(Path) -> resolve({link, Path, #{}}, Opts);
resolve(L = {link, ID, _LinkOpts}, Opts) when ?IS_ID(ID) -> {ok, L};
resolve({link, Path, LinkOpts}, Opts) ->
    ?event({resolving, Path}),
    case hb_store:read(scoped_store(Opts), Path) of
        {ok, ID} when ?IS_ID(ID) ->
            ?event({resolved, {path, Path}, {result, ID}}),
            {ok, {link, ID, LinkOpts}};
        {ok, NextPath} when is_binary(NextPath) ->
            ?event({resolve_recursing, {path, Path}, {next_path, NextPath}}),
            resolve({link, NextPath, LinkOpts}, Opts);
        {ok, Msg} when is_map(Msg) ->
            ?event(
                warning,
                {unexpectedly_received_full_message, {path, Path}, {message, Msg}},
                Opts
            ),
            {ok, Msg};
        not_found ->
            ?event({failed_resolve, Path}),
            not_found
    end.

%% @doc Read a value from the cache, if it exists. If the path is a valid ID, we
%% read the value from the store. If that fails, we try to list the path and
%% return a message containing known keys and links to their values. For raw
%% paths, we attempt to read and untype their values.
read({link, LinkedID, LinkOpts}, Opts) ->
    read(LinkedID, merge_opts(Opts, LinkOpts));
read(ID, Opts) when ?IS_ID(ID) ->
    ?event({reading_id, {id, ID}}),
    Store = scoped_store(Opts),
    case hb_store:read(Store, ID) of
        {ok, Binary} ->
            ?event({successfully_read_id, {id, ID}, {binary, Binary}}),
            {ok, untype(Binary)};
        _ ->
            case hb_store:list(Store, ID) of
                {ok, Paths} ->
                    ?event({successfully_listed_id, {id, ID}, {paths, Paths}}),
                    {ok,
                        #{
                            Path =>
                                {
                                    link,
                                    <<ID/binary, "/", Path/binary>>,
                                    #{ store => Store }
                                }
                        ||
                            Path <- Paths
                        }
                    };
                _ ->
                    ?event({id_read_failed, {id, ID}}),
                    not_found
            end
    end;
read(Path, Opts) when is_binary(Path) ->
    case resolve(Path, Opts) of
        {ok, Resolved} -> read(Resolved, Opts);
        not_found -> not_found
    end;
read(LoadedMessage, Opts) when is_map(LoadedMessage) ->
    {ok, LoadedMessage}.

%% @doc Link a hashpath to another in the cache.
link(Existing, New, Opts) ->
    case hb_store:make_link(scoped_store(Opts), Existing, New) of
        ok -> ok;
        _ -> error
    end.

%% @doc Write a (possibly deep) message to the cache, if caching is enabled.
%% Returns `{ok, ID}' on success, or `skipped' if caching is disabled.
write(Message, Opts) ->
    case hb_opts:get(cache, true, Opts) of
        false -> skipped;
        true -> do_write(Message, Opts)
    end.

%% @doc Recursively write message keys to the stores, passing the ID of values
%% to link back to the parent.
do_write(Message, Opts) when is_map(Message) ->
    BinaryMessage =
        maps:map(
            fun(Key, Value) ->
                {ok, InnerID} = do_write(Value, Opts),
                ?event(
                    {wrote_nested_value_to_cache,
                        {id, InnerID},
                        {key, Key},
                        {value, Value}
                    }
                ),
                <<InnerID/binary>>
             end,
            Message
        ),
    PrefixID = id(BinaryMessage, Opts),
    ?event({writing_links_to_cache, {id, PrefixID}, {message, Message}}),
    Store = scoped_store(Opts),
    maps:map(
        fun(Key, Value) ->
            hb_store:write(
                Store,
                <<PrefixID/binary, "/", (hb_util:bin(Key))/binary>>,
                Value
            )
        end,
        BinaryMessage
    ),
    ?event({wrote_nested_message_to_cache, {id, PrefixID}}),
    {ok, PrefixID};
do_write({link, ID, _LinkOpts}, _Opts) when ?IS_ID(ID) ->
    {ok, ID};
do_write({link, Path, _LinkOpts}, Opts) when is_binary(Path) ->
    Store = scoped_store(Opts),
    {ok, LinkedID} = hb_store:read(Store, Path),
    {ok, LinkedID};
do_write(Value, Opts) ->
    Binary = type(hb_util:bin(Value), Value),
    ID = id(Binary, Opts),
    Store = scoped_store(Opts),
    ok = hb_store:write(Store, ID, Binary),
    ?event({wrote_explicit_value_to_cache, {id, ID}, {typed_binary, Binary}}),
    {ok, ID}.

%%% Utilities

%% @doc Return the store value from the node options, scoped to the
%% `DEFAULT_SCOPE'.
scoped_store(Opts) ->
    hb_opts:get(store, no_store, hb_store:scope(Opts, ?DEFAULT_SCOPE)).

%% @doc Merge two `Opts`, appending the stores of the second before the first.
%% Supports 
merge_opts(Opts, {link, _, LinkOpts}) -> merge_opts(Opts, LinkOpts);
merge_opts({link, _, LinkOpts}, NewOpts) -> merge_opts(LinkOpts, NewOpts);
merge_opts(Opts = #{ store := Store }, NewOpts) when not is_list(Store) ->
    merge_opts(Opts#{ store := [Store]}, NewOpts);
merge_opts(Opts, NewOpts = #{ store := Store }) when not is_list(Store) ->
    merge_opts(Opts, NewOpts#{ store := [Store]});
merge_opts(Opts, NewOpts) ->
    (maps:merge(Opts, NewOpts))#{
        store => maps:get(store, NewOpts, []) ++ maps:get(store, Opts, [])
    }.

%% @doc Add a type character to an encoded value. Takes the value in its original
%% form and uses that to source the type to annotate the provided encoded form
%% with.
type(EncodedValue, Value) when is_map(Value) -> <<"p:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_binary(Value) -> <<"b:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_integer(Value) -> <<"i:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_float(Value) -> <<"f:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_atom(Value) -> <<"a:", EncodedValue/binary>>.

%% @doc Take a type-tagged binary and return the typed value.
untype(<<"p:", ID/binary>>) -> {link, ID, #{}};
untype(<<"b:", Binary/binary>>) -> Binary;
untype(<<"i:", Binary/binary>>) -> hb_util:int(Binary);
untype(<<"f:", Binary/binary>>) -> hb_util:float(Binary);
untype(<<"a:", Binary/binary>>) -> hb_util:atom(Binary).

%% @doc Generate a simple prefix (message ID) for a flat set of hashpath suffixes
%% (keys) and type-tagged values. This function is deterministic and will return
%% the same prefix for the same message, but is provided only as a sample. It is
%% not a formalized commitment algorithm of the `message@1.0/commit`-compliant
%% algorithm.
id(Binary, _Opts) when is_binary(Binary) ->
    hb_util:human_id(crypto:hash(sha256, Binary));
id(Message, _Opts) when is_map(Message) ->
    hb_util:human_id(
        crypto:hash_final(
            lists:foldl(
                fun(Key, State) ->
                    crypto:hash_update(
                        State,
                        <<
                            (hb_util:bin(Key))/binary,
                            ":",
                            (maps:get(Key, Message))/binary
                        >>
                    )
                end,
                crypto:hash_init(sha256),
                lists:sort(
                    fun(Key1, Key2) -> Key1 < Key2 end,
                    maps:keys(Message)
                )
            )
        )
    ).