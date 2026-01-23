%%% @doc A minimized implementation of the `hb_ao' cache, matching with the
%%% `hb_ao_micro' module. Supports `write' and `read' operations, with minimal
%%% type-tagging and untagging support.
-module(hb_cache_micro).
-export([write/2, read/2, link/3]).
-include("include/hb.hrl").

-define(DEFAULT_SCOPE, local).

%% @doc Return the store value from the node options, scoped to the
%% `DEFAULT_SCOPE'.
scoped_store(Opts) ->
    hb_opts:get(store, no_store, hb_store:scope(Opts, ?DEFAULT_SCOPE)).

%% @doc Link a hashpath to another in the cache.
link(Existing, New, Opts) ->
    Store = scoped_store(Opts),
    case hb_store:make_link(Store, Existing, New) of
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
    BinaryMessage=
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
                InnerID
             end,
            Message
        ),
    ID = id(BinaryMessage, Opts),
    ?event({writing_links_to_cache, {id, ID}, {message, Message}}),
    Store = scoped_store(Opts),
    maps:map(
        fun(Key, Value) ->
            hb_store:write(Store, <<ID/binary, "/", Key/binary>>, Value)
        end,
        BinaryMessage
    ),
    ?event({wrote_nested_message_to_cache, {id, ID}}),
    {ok, ID};
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

%% @doc Add a type character to a linked ID.
type(EncodedValue, Value) when is_map(Value) -> <<"m:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_binary(Value) -> <<"b:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_integer(Value) -> <<"i:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_float(Value) -> <<"f:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_boolean(Value) -> <<"a:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_list(Value) -> <<"l:", EncodedValue/binary>>.

%% @doc Take a type-tagged binary and return the typed value.
untype(<<"m:", ID/binary>>) -> {link, ID, #{}};
untype(<<"b:", Binary/binary>>) -> Binary;
untype(<<"i:", Binary/binary>>) -> hb_util:int(Binary);
untype(<<"f:", Binary/binary>>) -> hb_util:float(Binary);
untype(<<"a:", Binary/binary>>) -> hb_util:atom(Binary);
untype(<<"l:", ID/binary>>) -> {link, ID, #{}}.

%% @doc Read a value from the cache, if it exists. If the path is a valid ID, we
%% read the value from the store. If that fails, we try to list the path and
%% return a message containing known keys and links to their values. For raw
%% paths, we attempt to read and untype their values.
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
read(Path, Opts) ->
    Store = scoped_store(Opts),
    case hb_store:read(Store, Path) of
        {ok, LinkedID} ->
            ?event({found_linked_id, {path, Path}, {linked_id, LinkedID}}),
            read(LinkedID, Opts);
        Res ->
            ?event({read_raw_path_failed, {path, Path}, {result, Res}}),
            not_found
    end.

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
                        <<Key/binary, ":", (maps:get(Key, Message))/binary>>
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