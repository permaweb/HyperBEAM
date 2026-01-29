%%% @doc A minimized implementation of the `hb_ao' cache, matching with the
%%% `hb_ao_micro' module. Supports `write' and `read' operations, with
%%% `~structured@1.0` type-tagging support.
%%% 
%%% The structure of a produced store is as follows:
%%% BaseID/RequestID -> << "path:", [AO-Core Executable Path] >>
%%% ID -> << TypeCharacter, ":", BinaryMatchingID >>.
-module(hb_cache_micro).
-export([resolve/2, read/2, write/2, link/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_SCOPE, local).

%%% Read Operations.

%% @doc Resolve a link or a raw path to a simple prefix (either to a raw binary
%% value or a collection of hashpaths representing a message).
resolve(Path, Opts) when not ?IS_LINK(Path) -> resolve({link, Path, #{}}, Opts);
resolve(L = {link, ID, _LinkOpts}, Opts) when ?IS_ID(ID) -> {ok, L};
resolve({link, Path, LinkOpts}, Opts) ->
    ?event({resolving, Path}),
    case hb_store:read(scoped_store(Opts), Path) of
        {ok, <<"path:", ID/binary>>} when ?IS_ID(ID) ->
            ?event({resolved, {path, Path}, {result, ID}}),
            {ok, {link, ID, LinkOpts}};
        {ok, <<"path:", NextPath/binary>>} when is_binary(NextPath) ->
            ?event({resolve_recursing, {path, Path}, {next_path, NextPath}}),
            resolve({link, NextPath, LinkOpts}, Opts);
        {ok, Result} ->
            ?event(
                warning,
                {unexpectedly_full_result, {path, Path}, {result, Result}},
                Opts
            ),
            {ok, Result};
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

%%% Write Operations.

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
        true ->
            Store = scoped_store(Opts),
            do_write(Message, Store, Opts)
    end.

%% @doc Recursively write message keys to the stores, passing the ID of values
%% to link back to the parent.
do_write(Message, Store, Opts) when is_map(Message) ->
    ?event({do_write_msg, {message, Message}}),
    BinaryMessage =
        maps:map(
            fun(Key, Value) ->
                {ok, InnerID} = do_write(Value, Store, Opts),
                ?event(
                    {wrote_nested_value_to_cache,
                        {id, InnerID},
                        {key, Key},
                        {value, Value}
                    }
                ),
                <<"path:", InnerID/binary>>
             end,
            Message
        ),
    ?event({do_write_messages, {bin_msg, BinaryMessage}, {message, Message}}),
    PrefixID = 
        case maps:get(<<"id">>, Message, not_found) of
            not_found -> 
                GeneratedID = id(BinaryMessage, Opts),
                ?event({no_id_in_message, {generated, GeneratedID}}),
                do_write_id(GeneratedID, Store, Opts),
                do_write_committed(GeneratedID, BinaryMessage, Store, Opts),
                GeneratedID;
            {link, Link, LinkOpts} -> 
                ?event({using_provided_id_link, {link, Link}}),
                Resolved = hb_util:ok(resolve(Link, LinkOpts)),
                ?event({used_provided_id_link, {resolved, Resolved}}),
                Resolved;
            ID -> 
                ?event({using_provided_id, {id, ID}, {untyped_id, untype(ID)}}),
                untype(ID)
        end,
    ?event({prefix_id, PrefixID}),
    ?event({
        writing_links_to_cache, 
            {id, PrefixID},
            {message, Message},
            {bin_msg, BinaryMessage}
        }),
    ?event({wrote_nested_message_to_cache, {id, PrefixID}}),
    {ok, PrefixID};
do_write({link, Path, _LinkOpts}, _Store, _Opts) ->
    {ok, Path};
do_write(List, Store, Opts) when is_list(List) ->
    do_write(
        hb_message:convert(List, tabm, <<"structured@1.0">>, Opts),
        Store,
        Opts
    );
do_write(Value, Store, Opts) ->
    ?event(do_write, {do_write, {value, Value}}),
    Binary = type(hb_util:bin(Value), Value),
    ID = id(Binary, Opts),
    ok = hb_store:write(Store, ID, Binary),
    ?event({wrote_explicit_value_to_cache, {id, ID}, {typed_binary, Binary}}),
    {ok, ID}.
do_write_id(ID, Store, _Opts) ->
    hb_store:write(
        Store,
        <<ID/binary, "/", "id">>,
        ID
    ).
do_write_committed(ID, Message, Store, _Opts) ->
    Committed =
        maps:keys(
            maps:map(
                fun(Key, Value) ->
                    hb_store:write(
                        Store,
                        <<ID/binary, "/", (hb_util:bin(Key))/binary>>,
                        Value
                    ),
                    Key
                end,
                Message
            )
        ),
    CommittedKeys = list_to_binary(lists:join(<<",">>, Committed)),
    EncodedCommittedKeys = type(CommittedKeys, Committed),
    hb_store:write(
        Store,
        <<ID/binary, "/", "committed">>,
        EncodedCommittedKeys
    ),
    ?event({wrote_committed_keys_to_cache, {id, ID}, {committed_keys, CommittedKeys}, {encoded_committed_keys, EncodedCommittedKeys}}).
%%% Utilities
with_only_committed(Message, Opts) ->
    Committed = 
        case maps:get(<<"committed">>, Message, not_found) of
            not_found -> <<"l:">>;
            {link, Link, LinkOpts} -> hb_util:ok(resolve(Link, LinkOpts));
            Comm -> Comm
        end,
    CommittedKeys = untype(Committed),
    maps:filter(
        fun(Key, Value) ->
            lists:member(Key, CommittedKeys)
        end,
        Message
    ).

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
type(EncodedValue, Value) when is_map(Value) -> <<"path:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_binary(Value) -> <<"b:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_integer(Value) -> <<"i:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_float(Value) -> <<"f:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_atom(Value) -> <<"a:", EncodedValue/binary>>;
type(EncodedValue, Value) when is_list(Value) -> <<"l:", EncodedValue/binary>>.

%% @doc Take a type-tagged binary and return the typed value.
untype(<<"path:", ID/binary>>) -> {link, ID, #{}};
untype(<<"b:", Binary/binary>>) -> Binary;
untype(<<"i:", Binary/binary>>) -> hb_util:int(Binary);
untype(<<"f:", Binary/binary>>) -> hb_util:float(Binary);
untype(<<"a:", Binary/binary>>) -> hb_util:atom(Binary);
untype(<<"l:", Binary/binary>>) -> binary:split(Binary, <<",">>, [global]).

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
match(MatchSpec, Opts) ->
    ?event(cache_micro, {matching, {spec, MatchSpec}}),
    Spec = hb_message:convert(MatchSpec, tabm, <<"structured@1.0">>, Opts),
    ConvertedMatchSpec =
        maps:map(
            fun(_, Value) ->
                Value
                % generate_binary_path(Value, Opts)
            end,
            maps:without([<<"ao-types">>], hb_ao:normalize_keys(Spec, Opts))
        ),
    ?event(cache_micro, {matching, {converted_spec, {explicit, ConvertedMatchSpec}}}),
    case hb_store:match(hb_opts:get(store, no_viable_store, Opts), ConvertedMatchSpec) of
        {ok, Matches} -> {ok, Matches};
        _ -> not_found
    end.

%% @doc Generate the path at which a binary value should be stored.
generate_binary_path(Bin, Opts) ->
    Hashpath = hb_path:hashpath(Bin, Opts),
    <<"data/", Hashpath/binary>>.

    
%% TESTS
test_unsigned(Data) ->
    #{
        <<"base-test-key">> => <<"base-test-value">>,
        <<"other-test-key">> => Data
    }.
test_store_unsigned_empty_message(Store) ->
    hb_store:reset(Store),
    Item = #{},
    Opts = #{ store => Store },
    {ok, Path} = write(Item, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Item, RetrievedItem, strict, Opts)).

test_store_binary(Store) ->
    Bin = <<"Simple unsigned data item">>,
    ?event(debug_store_test, {store, Store}),
    Opts = #{ store => Store },
    {ok, ID} = write(Bin, Opts),
    {ok, RetrievedBin} = read(ID, Opts),
    ?assertEqual(Bin, RetrievedBin).

test_store_unsigned_nested_empty_message(Store) ->
    ?event(debug_store_test, {store, Store}),
    % hb_store:reset(Store),
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
    Opts = #{ store => Store },
    {ok, Path} = write(Item, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?event(debug_test, {match, {item, Item}, {retrieved, RetrievedItem}}),
    ?event(debug_test, {only_committed, with_only_committed(RetrievedItem, Opts)}),
    ?assert(hb_message:match(Item, RetrievedItem, strict, Opts)).

test_store_simple_unsigned_message(Store) ->
    Item = test_unsigned(<<"Simple unsigned data item">>),
    ?event(debug_store_test, {item, Item}),
    Opts = #{ store => Store },
    %% Write the simple unsigned item
    {ok, Path} = write(Item, Opts),
    %% Read the item back
    {ok, RetrievedItem} = read(Path, Opts),
    ?event(debug_test, {match, {item, Item}, {retrieved, RetrievedItem}}),
    ?event(debug_test, {only_committed, with_only_committed(RetrievedItem, Opts)}),
    ?assert(hb_message:match(Item, RetrievedItem, strict, Opts)).
cache_suite_test_() ->
    hb_store:generate_test_suite([
        % {"store unsigned empty message",
        %     fun test_store_unsigned_empty_message/1},
        % {"store binary", fun test_store_binary/1},
        % {"store unsigned nested empty message",
        %     fun test_store_unsigned_nested_empty_message/1},
        {"store simple unsigned message", fun test_store_simple_unsigned_message/1}
    ]).
    
run_test() ->
    PreloadedStore = hb_test_utils:test_store(hb_store_preloaded),
    Store = hb_test_utils:test_store(hb_store_lmdb),
    ?event({test_stores, {preloaded, PreloadedStore}, {store, Store}}),
    test_store_simple_unsigned_message([PreloadedStore, Store]).
    
% Test statuses:

% test_store_unsigned_empty_message: Fails fs/lru - difference of what 
% happens when list() returns not_found vs []

% test_store_binary: all pass

% test_store_unsigned_nested_empty_message: built basic ensure_all_loaded,
% we have to call it explicitly to pass the match, where in hb_cache we do not.
% Believe this is correct behavior as hb_cache_micro returns links/paths by default.
% Fs/lru will also fail without empty list behavior change described above.

%

