%%%-----------------------------------------------------------------------------
%%% @doc A process wrapper over rocksdb storage. Replicates functionality of the
%%%      ao_fs_store module.
%%%
%%%      1. All the data is stored in the same rocksdb column family, making the
%%%         structure of the data flat;
%%%      2. The data is stored in the binary format
%%%      3. The concept of link is implemented with a data prefixed with a `link-`
%%%         word, so for example {<<my_key>>, <<"link-my_key2">>} means link to
%%%         my_key2
%%%      4. Read function tries to follow links and extract the data after the
%%%         full traversal
%%%      5. Entry type is identified with the help of the manifest. If entry has
%%%         a manifest it's composite, otherwise it's simple
%%%      6. Read automatically tries to get item entry
%%%      7. Composite items are store with the -item postfix (same as in the FS)
%%% @end
%%%-----------------------------------------------------------------------------
-module(ao_rocksdb_store).

-behaviour(gen_server).

-author("Oleg Tarasenko").

-behaviour(ao_store).

-define(TIMEOUT, 5000).

-include("src/include/ao.hrl").

-define(LINK_PREFIX, <<"link-">>).

% Behaviour based callbacks
-export([start/1, stop/1]).
-export([read/2, write/3]).
-export([list/2]).
-export([reset/1, make_link/3]).
-export([make_group/2]).
-export([type/2]).
-export([add_path/3, path/2]).
-export([resolve/2]).
% Starting/stopping process
-export([start_link/1]).
% Gen server callbacks
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_info/2, handle_call/3]).
-export([code_change/3]).

-type key() :: binary().
-type value() :: binary().

-spec start_link(#{dir := term()}) -> ignore | {ok, pid()}.
start_link(#{dir := _Dir} = RocksDBOpts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, RocksDBOpts, []);
start_link(_Opts) ->
    ignore.

-spec start(#{dir := term()}) -> ignore | {ok, pid()}.
start(Opts) ->
    start_link(Opts).

-spec stop(any()) -> ok.
stop(_Opts) ->
    gen_server:stop(?MODULE).

-spec reset([]) -> ok | no_return().
reset(_Opts) ->
    gen_server:call(?MODULE, reset, ?TIMEOUT).

%%------------------------------------------------------------------------------
%% @doc Read data by the key
%%
%% Recursively follows messages with values defined as <<"link-NewKey">>
%% @end
%%------------------------------------------------------------------------------
-spec read(Opts, Key) -> Result
    when Opts :: map(),
         Key :: key() | list(),
         Result :: {ok, value()} | not_found | {error, {corruption, string()}} | {error, any()}.
read(_Opts, Key) ->
    KeyBin = maybe_convert_to_binary(Key),
    gen_server:call(?MODULE, {read, KeyBin}, ?TIMEOUT).

%%------------------------------------------------------------------------------
%% @doc Write given Key and Value to the database
%%
%% @end
%%------------------------------------------------------------------------------
-spec write(Opts, Key, Value) -> Result
    when Opts :: map(),
         Key :: key() | list(),
         Value :: value() | list(),
         Result :: ok | {error, any()}.
write(_Opts, Key, Value) ->
    KeyBin = maybe_convert_to_binary(Key),
    ValueBin = maybe_convert_to_binary(Value),
    gen_server:call(?MODULE, {write, KeyBin, ValueBin}, ?TIMEOUT).

%%------------------------------------------------------------------------------
%% @doc List key/values stored in the storage so far.
%%      *Note*: This function is slow, and probably should not be used on
%%      production
%% @end
%%------------------------------------------------------------------------------
-spec list(Opts, Path) -> Result
    when Opts :: any(),
         Path :: any(),
         Result :: [{key(), value()}].
list(_Opts, _Path) ->
    % Lists all items under given path. Looks useful for composite items
    gen_server:call(?MODULE, list, ?TIMEOUT).

%% @doc Replace links in a path with the target of the link.
-spec resolve(Opts, Path) -> Result
    when Opts :: any(),
         Path :: binary() | list(),
         Result :: not_found | binary() | [binary() | list()].
resolve(Opts, [BaseId, ResolutionKeys]) ->
    case resolve_chain(Opts, BaseId, ResolutionKeys) of
        not_found ->
            fallback_resolve(Opts, BaseId, ResolutionKeys);
        Result ->
            maybe_remove_messages_prefix(Result)
    end;
resolve(_Opts, Key) when is_binary(Key) ->
    case item_path(Key) of
        not_found ->
            not_found;
        {ok, [LastKey | _]} ->
            maybe_remove_messages_prefix(LastKey)
    end.

item_path(Key) ->
    gen_server:call(?MODULE, {item_path, Key}, ?TIMEOUT).

%% Helper function to resolve a chain of IDs.
resolve_chain(Opts, BaseId, ResolutionKeys) ->
    lists:foldl(fun (_Key, not_found) ->
                        not_found;
                    (Key, Acc) ->
                        case read(Opts, Acc) of
                            not_found ->
                                not_found;
                            {ok, ItemBinary} ->
                                extract_next_id(ItemBinary, Key)
                        end
                end,
                BaseId,
                ResolutionKeys).

%% Helper function to resolve the next ID from an item.
extract_next_id(ItemBinary, ResolutionKey) ->
    Item = ar_bundles:deserialize(ItemBinary),
    Manifest =
        ar_bundles:parse_manifest(
            maps:get(<<"manifest">>, Item#tx.data)),
    NextId = maps:get(list_to_binary(ResolutionKey), Manifest),
    <<"messages-", NextId/binary>>.

%% Fallback resolution if primary resolution fails.
fallback_resolve(Opts, BaseId, ResolutionKeys) ->
    Path = path(Opts, ResolutionKeys),
    case item_path(<<BaseId/binary, "-", Path/binary>>) of
        not_found ->
            not_found;
        {ok, [Key | _]} ->
            maybe_remove_messages_prefix(Key)
    end.

-spec type(Opts, Key) -> Result
    when Opts :: map(),
         Key :: binary(),
         Result :: composite | simple | not_found.
type(Opts, ["messages", Path]) ->
    MessagesPath = <<"messages-", Path/binary>>,
    type(Opts, MessagesPath);
type(Opts, Key) when is_binary(Key) ->
    % The fs adapter looks on the FS. If it's a directory it says 'comosite'
    % composite item is an item which has manifest!
    case read(Opts, Key) of
        not_found ->
            not_found;
        {ok, ItemBinary} ->
            Item = ar_bundles:deserialize(ItemBinary),
            case ar_bundles:manifest(Item) of
                undefined ->
                    simple;
                _Manifest ->
                    composite
            end
    end.

make_group(_Opts, _Path) ->
    % FS just creates a folder....
    % when we write a composite item
    % (the item which consists of multiple items) we do:
    % 1. create folder
    % 2. create manifest file
    % 3. create items themselves
    ok.

-spec make_link(any(), key(), key()) -> ok.
make_link(_, Key1, Key1) ->
    ok;
make_link(Opts, Key1, Key2) when is_list(Key1), is_list(Key2) ->
    Key1Bin = list_to_binary(Key1),
    Key2Bin = list_to_binary(Key2),
    make_link(Opts, Key1Bin, Key2Bin);
make_link(Opts, Key1, Key2) ->
    ok = write(Opts, Key2, <<?LINK_PREFIX/binary, Key1/binary>>).

%% @doc Create a path from a list of path components.
path(Opts, [Part1, Part2, LookupKeys]) ->
    % Special case when we have lookup keys to perform the lookup
    % so we want to make a Binary from Part1 and Part2 and
    Subpath = path(Opts, [Part1, Part2]),
    case is_charlist(LookupKeys) of
        true ->
            path(Opts, [Subpath, LookupKeys]);
        false ->
            [Subpath, LookupKeys]
    end;
path(_Opts, Path) when is_list(Path) ->
    BinList = lists:map(fun convert_to_binary/1, Path),
    Res = lists:join(<<"-">>, BinList),
    erlang:list_to_binary(Res);
path(_Opts, Path) ->
    Path.

%% Flatten and convert items to binary.
convert_to_binary(PathItem) when is_list(PathItem) ->
    case is_charlist(PathItem) of
        true ->
            list_to_binary(PathItem);
        false ->
            path([], PathItem)
    end;
convert_to_binary(PathItem) when is_binary(PathItem) ->
    PathItem.

%% @doc Add two path components together. // is not used
add_path(_Opts, Path1, Path2) ->
    Path1 ++ Path2.

%%%=============================================================================
%%% Gen server callbacks
%%%=============================================================================
init(_Store = #{dir := Dir}) ->
    Options = [{create_if_missing, true}],
    {ok, DBHandle} = rocksdb:open(Dir, Options),
    {ok, #{db_handle => DBHandle, dir => Dir}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_call({write, Key, Value}, _From, State = #{db_handle := DBHandle}) ->
    Result = rocksdb:put(DBHandle, Key, Value, []),
    {reply, Result, State};
handle_call({read, Key}, _From, State = #{db_handle := DBHandle}) ->
    Result =
        case get(DBHandle, Key, [], []) of
            {ok, {_ResolvedPath, Res}} ->
                {ok, Res};
            Other ->
                Other
        end,
    {reply, Result, State};
handle_call({item_path, BaseKey}, _From, State = #{db_handle := DBHandle}) ->
    Result =
        case get(DBHandle, BaseKey, [], []) of
            {ok, {ResolvedPath, _Res}} ->
                {ok, ResolvedPath};
            not_found ->
                not_found
        end,
    {reply, Result, State};
handle_call(reset, _From, State = #{db_handle := DBHandle, dir := Dir}) ->
    ok = rocksdb:close(DBHandle),
    ok = rocksdb:destroy(Dir, []),

    Options = [{create_if_missing, true}],
    {ok, NewHandle} = rocksdb:open(Dir, Options),
    NewState = maps:put(db_handle, NewHandle, State),
    {reply, ok, NewState};
handle_call(list, _From, State = #{db_handle := DBHandle}) ->
    {ok, Iterator} = rocksdb:iterator(DBHandle, []),
    Items = collect(Iterator),
    {reply, Items, State};
handle_call(_Request, _From, State) ->
    {reply, unrecognized_message, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Private
%%%=============================================================================

get(_DBHandle, not_found, _Opts, _Path) ->
    not_found;
get(DBHandle, Key, Opts, Path) ->
    case rocksdb:get(DBHandle, Key, Opts) of
        not_found ->
            % Also try to get complex item if possible
            case contains(Key, <<"-item$">>) of
                false ->
                    get(DBHandle, <<Key/binary, "-item">>, Opts, Path);
                true ->
                    not_found
            end;
        {ok, <<"link-", LinkedKey/binary>>} ->
            % Try following the link
            get(DBHandle, LinkedKey, Opts, [Key | Path]);
        {ok, Result} ->
            {ok, {[Key | Path], Result}}
    end.

is_charlist([Item | _] = _MaybeCharlist) when is_list(Item) ->
    false;
is_charlist([Item | _] = _MaybeCharlist) when Item >= 0, Item =< 255 ->
    true;
is_charlist(_MaybeCharlist) ->
    false.

contains(Subject, Pattern) ->
    case re:run(Subject, Pattern, [{capture, none}, unicode]) of
        match ->
            true;
        nomatch ->
            false
    end.

collect(Iterator) ->
    {ok, Key, Value} = rocksdb:iterator_move(Iterator, <<>>),
    collect(Iterator, [{Key, Value}]).

collect(Iterator, Acc) ->
    case rocksdb:iterator_move(Iterator, next) of
        {ok, Key, Value} ->
            % Continue iterating, accumulating the key-value pair in the list
            collect(Iterator, [{Key, Value} | Acc]);
        {error, invalid_iterator} ->
            % Reached the end of the iterator, return the accumulated list
            lists:reverse(Acc)
    end.

maybe_remove_messages_prefix(<<"messages-", Rest/binary>>) ->
    Rest;
maybe_remove_messages_prefix(Other) ->
    Other.

maybe_convert_to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
maybe_convert_to_binary(Value) when is_binary(Value) ->
    Value.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_or_start_server() ->
    Opts = #{dir => "TEST-data/rocksdb"},
    case start_link(Opts) of
        {ok, Pid} ->
            Pid;
        {error, {already_started, Pid}} ->
            Pid
    end.

utils_test_() ->
    {foreach,
     fun() -> ignored end,
     [{"is_charlist/1 returns true for charlists",
       fun() -> ?assertEqual(true, is_charlist("My Charlist")) end},
      {"is_charlist/2 returns false for regular lists",
       fun() -> ?assertEqual(false, is_charlist(["My Charlist"])) end},
      {"is_charlist/2 returns false for non lists",
       fun() -> ?assertEqual(false, is_charlist(<<"Binary">>)) end},
      {"is_charlist/2 returns false non charlists",
       fun() -> ?assertEqual(false, is_charlist([-1, 2, 12])) end}]}.

path_test_() ->
    {foreach,
     fun() -> ignored_setup end,
     [{"basic path construction",
       fun() ->
          ?assertEqual(<<"messages-MyID">>, path([], [["messages"], "MyID"])),
          ?assertEqual(<<"messages-MyID-item">>, path([], [["messages"], "MyID", "item"])),
          ?assertEqual(<<"messages-MyID">>, path([], ["messages", "MyID"])),
          ?assertEqual(<<"messages-computed-MyID">>, path([], [["messages", "computed"], "MyID"]))
       end},
      {"lookup keys are returned with the base path",
       fun() ->
          Opts = #{},
          Path = ["part1", "part2", ["lookupkey1", "lookupkey2"]],
          Result = path(Opts, Path),

          ?assertEqual([<<"part1-part2">>, ["lookupkey1", "lookupkey2"]], Result)
       end}]}.

write_read_test_() ->
    {foreach,
     fun() ->
        Pid = get_or_start_server(),
        unlink(Pid)
     end,
     fun(_) -> ao_rocksdb_store:reset([]) end,
     [{"can read/write data",
       fun() ->
          ok = write(ignored_options, <<"test_key">>, <<"test_value">>),
          {ok, Value} = read(ignored_options, <<"test_key">>),

          ?assertEqual(<<"test_value">>, Value)
       end},
      {"returns not_found for non existing keys",
       fun() ->
          Value = read(ignored_options, <<"non_existing">>),

          ?assertEqual(not_found, Value)
       end},
      {"follows links",
       fun() ->
          ok = write(ignored_options, <<"test_key2">>, <<"value_under_linked_key">>),
          ok = write(ignored_options, <<"test_key">>, <<"link-test_key2">>),
          {ok, Value} = read(ignored_options, <<"test_key">>),

          ?assertEqual(<<"value_under_linked_key">>, Value)
       end},
      {"automatically extracts items",
       fun() ->
          ok = write(ignored_options, <<"test_key-item">>, <<"item_data">>),
          {ok, Value} = read(ignored_options, <<"test_key">>),

          ?assertEqual(<<"item_data">>, Value)
       end}]}.

api_test_() ->
    {foreach,
     fun() ->
        Pid = get_or_start_server(),
        unlink(Pid)
     end,
     fun(_) -> reset([]) end,
     [{"make_link/3 creates a link to actual data",
       fun() ->
          ok = write(ignored_options, <<"key1">>, <<"test_value">>),
          ok = make_link([], <<"key1">>, <<"key2">>),
          {ok, Value} = read([], <<"key2">>),

          ?assertEqual(<<"test_value">>, Value)
       end},
      {"make_link/3 does not create links if keys are same",
       fun() ->
          ok = make_link([], <<"key1">>, <<"key1">>),
          ?assertEqual(not_found, read(#{}, <<"key1">>))
       end},
      {"reset cleans up the database",
       fun() ->
          ok = write(ignored_options, <<"test_key">>, <<"test_value">>),

          ok = reset([]),
          ?assertEqual(not_found, read(ignored_options, <<"test_key">>))
       end},
      {"item_path/1 can return the full path of a given item (all links)",
       fun() ->
          ok = write(#{}, <<"key">>, <<"link-key1">>),
          ok = write(#{}, <<"key1">>, <<"link-key2">>),
          ok = write(#{}, <<"key2">>, <<"link-key3">>),
          ok = write(#{}, <<"key3">>, <<"final value">>),

          {ok, Path} = item_path(<<"key">>),
          ?assertEqual([<<"key3">>, <<"key2">>, <<"key1">>, <<"key">>], Path)
       end},
      {"resolve/2 returns the final key of the element (after following "
       "links)",
       fun() ->
          ok = write(#{}, <<"key">>, <<"link-messages-key1">>),
          ok = write(#{}, <<"messages-key1">>, <<"Data">>),

          ?assertEqual(<<"key1">>, resolve(#{}, <<"key">>))
       end}]}.

-endif.
