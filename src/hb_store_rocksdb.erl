%%%-----------------------------------------------------------------------------
%%% @doc A process wrapper over rocksdb storage. Replicates functionality of the
%%%      hb_fs_store module.
%%%
%%%     Encodes the item types with the help of prefixes, see `encode_value/2'
%%%     and `decode_value/1'
%%% @end
%%%-----------------------------------------------------------------------------
-module(hb_store_rocksdb).
-behaviour(gen_server).
-behaviour(hb_store).
-export([enabled/0, start/1, start_link/1, stop/1, scope/1]).
-export([read/2, write/3, list/2, reset/1, list/0]).
-export([make_link/3, make_group/2, type/2, add_path/3, path/2, resolve/2]).
-export([init/1, terminate/2, handle_cast/2, handle_info/2, handle_call/3]).
-export([code_change/3]).
-include("include/hb.hrl").

-define(TIMEOUT, 5000).

-type key() :: binary() | list().
-type value() :: binary() | list().

-type value_type() :: link | raw | group.

%% @doc Returns whether the RocksDB store is enabled.
-ifdef(ENABLE_ROCKSDB).


enabled() -> true.


-else.


enabled() -> false.


-endif.

-ifdef(ENABLE_ROCKSDB).
%% @doc Start the RocksDB store.


start_link(#{<<"store-module">> := hb_store_rocksdb, <<"name">> := Dir}) ->
    ?event(rocksdb, {starting, Dir}),
    application:ensure_all_started(rocksdb),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Dir, []);
start_link(Stores) when is_list(Stores) ->
    RocksStores =
        [ Store
          || Store = #{<<"store-module">> := Module} <- Stores,
             Module =:= hb_store_rocksdb ],
    case RocksStores of
        [Store] -> start_link(Store);
        _ -> ignore
    end;
start_link(Store) ->
    ?event(rocksdb, {invalid_store_config, Store}),
    ignore.


-else.


start_link(_Opts) -> ignore.


-endif.


start(Opts = #{<<"store-module">> := hb_store_rocksdb, <<"name">> := _Dir}) ->
    start_link(Opts);
start(Opts) ->
    start_link(Opts).


-spec stop(any()) -> ok.
stop(_Opts) ->
    gen_server:stop(?MODULE).


-spec reset([]) -> ok | no_return().
reset(_Opts) ->
    gen_server:call(?MODULE, reset, ?TIMEOUT).


%% @doc Return scope (local)
scope(_) -> local.


%% @doc Return path
path(_Opts, Path) ->
    hb_store:path(Path).


%% @doc Read data by the key.
%% Recursively follows link messages
-spec read(Opts, Key) -> Result
              when Opts :: map(),
                   Key :: key() | list(),
                   Result :: {ok, value()} | not_found | {error, {corruption, string()}} | {error, any()}.
read(Opts, RawPath) ->
    ?event({read, RawPath}),
    Path = resolve(Opts, RawPath),
    case do_read(Opts, Path) of
        not_found ->
            not_found;
        {error, _Reason} = Err -> Err;
        {ok, {raw, Result}} ->
            {ok, Result};
        {ok, {link, Link}} ->
            ?event({link_found, Path, Link}),
            read(Opts, Link);
        {ok, {group, _Result}} ->
            not_found
    end.


%% @doc Write given Key and Value to the database
-spec write(Opts, Key, Value) -> Result
              when Opts :: map(),
                   Key :: key(),
                   Value :: value(),
                   Result :: ok | {error, any()}.
write(Opts, RawKey, Value) ->
    Key = hb_store:join(RawKey),
    EncodedValue = encode_value(raw, Value),
    ?event({writing, Key, byte_size(EncodedValue)}),
    do_write(Opts, Key, EncodedValue).


%% @doc Returns the full list of items stored under the given path. Where the path
%% child items is relevant to the path of parentItem. (Same as in `hb_store_fs').
-spec list(Opts, Path) -> Result
              when Opts :: any(),
                   Path :: any(),
                   Result :: {ok, [string()]} | {error, term()}.

list(Opts, Path) ->
    case do_read(Opts, Path) of
        not_found -> {error, not_found};
        {error, _Reason} = Err ->
            ?event(rocksdb, {could_not_list_folder, Err}),
            Err;
        {ok, {group, Value}} ->
            {ok, sets:to_list(Value)};
        {ok, {link, LinkedPath}} ->
            list(Opts, LinkedPath);
        Reason ->
            ?event(rocksdb, {could_not_list_folder, Reason}),
            {ok, []}
    end.


%% @doc Replace links in a path with the target of the link.
-spec resolve(Opts, Path) -> Result
              when Opts :: any(),
                   Path :: binary() | list(),
                   Result :: not_found | string().
resolve(Opts, Path) ->
    PathList = hb_path:term_to_path_parts(hb_store:join(Path)),

    ResolvedPath = do_resolve(Opts, "", PathList),
    ResolvedPath.


do_resolve(_Opts, FinalPath, []) ->
    FinalPath;
do_resolve(Opts, CurrentPath, [CurrentPath | Rest]) ->
    do_resolve(Opts, CurrentPath, Rest);
do_resolve(Opts, CurrentPath, [Next | Rest]) ->
    PathPart = hb_store:join([CurrentPath, Next]),
    case do_read(Opts, PathPart) of
        not_found -> do_resolve(Opts, PathPart, Rest);
        {error, _Reason} = Err -> Err;
        {ok, {link, LinkValue}} ->
            do_resolve(Opts, LinkValue, Rest);
        {ok, _OtherType} -> do_resolve(Opts, PathPart, Rest)
    end.


%% @doc Get type of the current item
-spec type(Opts, Key) -> Result
              when Opts :: map(),
                   Key :: binary(),
                   Result :: composite | simple | not_found.

type(Opts, RawKey) ->
    Key = hb_store:join(RawKey),
    case do_read(Opts, Key) of
        not_found -> not_found;
        {ok, {raw, _Item}} -> simple;
        {ok, {link, NewKey}} -> type(Opts, NewKey);
        {ok, {group, _Item}} -> composite
    end.


%% @doc Creates group under the given path.
-spec make_group(Opts, Key) -> Result
              when Opts :: any(),
                   Key :: binary(),
                   Result :: ok | {error, already_added}.
make_group(#{<<"name">> := _DataDir}, Key) ->
    gen_server:call(?MODULE, {make_group, Key}, ?TIMEOUT);
make_group(_Opts, Key) ->
    gen_server:call(?MODULE, {make_group, Key}, ?TIMEOUT).


-spec make_link(any(), key(), key()) -> ok.
make_link(_, Key1, Key1) ->
    ok;

make_link(Opts, Existing, New) ->
    ExistingBin = convert_if_list(Existing),
    NewBin = convert_if_list(New),

    % Create: NewValue -> ExistingBin
    case do_read(Opts, NewBin) of
        not_found ->
            do_write(Opts, NewBin, encode_value(link, ExistingBin));
        _ ->
            ok
    end.


%% @doc Add two path components together. // is not used
add_path(_Opts, Path1, Path2) ->
    Path1 ++ Path2.


%% @doc List all items registered in rocksdb store. Should be used only
%% for testing/debugging, as the underlying operation is doing full traversal
%% on the KV storage, and is slow.
list() ->
    gen_server:call(?MODULE, list, ?TIMEOUT).


%%%=============================================================================
%%% Gen server callbacks
%%%=============================================================================
init(Dir) ->
    filelib:ensure_dir(Dir),
    case open_rockdb(Dir) of
        {ok, DBHandle} ->
            State = #{
                      db_handle => DBHandle,
                      dir => Dir
                     },
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


handle_call(Request, From, #{db_handle := undefined, dir := Dir} = State) ->
    % Re-initialize the DB handle if it's not set.
    {ok, DBHandle} = open_rockdb(Dir),
    handle_call(Request, From, State#{db_handle => DBHandle});
handle_call({do_write, Key, Value}, _From, #{db_handle := DBHandle} = State) ->
    BaseName = filename:basename(Key),
    rocksdb:put(DBHandle, Key, Value, #{}),
    case filename:dirname(Key) of
        <<".">> ->
            ignore;
        BaseDir ->
            ensure_dir(DBHandle, BaseDir),
            {ok, RawDirContent} = rocksdb:get(DBHandle, BaseDir, #{}),
            NewDirContent = maybe_append_key_to_group(BaseName, RawDirContent),
            ok = rocksdb:put(DBHandle, BaseDir, NewDirContent, #{})
    end,
    {reply, ok, State};
handle_call({do_read, Key}, _From, #{db_handle := DBHandle} = State) ->
    Response =
        case rocksdb:get(DBHandle, Key, #{}) of
            {ok, Result} ->
                {Type, Value} = decode_value(Result),
                {ok, {Type, Value}};
            not_found ->
                not_found;
            {error, _Reason} = Err ->
                Err
        end,
    {reply, Response, State};
handle_call(reset, _From, State = #{db_handle := DBHandle, dir := Dir}) ->
    ok = rocksdb:close(DBHandle),
    ok = rocksdb:destroy(DirStr = ensure_list(Dir), []),
    os:cmd(binary_to_list(<<"rm -Rf ", (list_to_binary(DirStr))/binary>>)),
    {reply, ok, State#{db_handle := undefined}};
handle_call(list, _From, State = #{db_handle := DBHandle}) ->
    {ok, Iterator} = rocksdb:iterator(DBHandle, []),
    Items = collect(Iterator),
    {reply, Items, State};
handle_call({make_group, Path}, _From, #{db_handle := DBHandle} = State) ->
    Result = ensure_dir(DBHandle, Path),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, handle_call_unrecognized_message, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%=============================================================================
%%% Private
%%%=============================================================================
%% @doc Write given Key and Value to the database
-spec do_write(Opts, Key, Value) -> Result
              when Opts :: map(),
                   Key :: key(),
                   Value :: value(),
                   Result :: ok | {error, any()}.
do_write(_Opts, Key, Value) ->
    gen_server:call(?MODULE, {do_write, Key, Value}, ?TIMEOUT).


do_read(_Opts, Key) ->
    gen_server:call(?MODULE, {do_read, Key}, ?TIMEOUT).


-spec encode_value(value_type(), binary()) -> binary().
encode_value(link, Value) -> <<1, Value/binary>>;
encode_value(raw, Value) -> <<2, Value/binary>>;
encode_value(group, Value) -> <<3, (term_to_binary(Value))/binary>>.


-spec decode_value(binary()) -> {value_type(), binary()}.
decode_value(<<1, Value/binary>>) -> {link, Value};
decode_value(<<2, Value/binary>>) -> {raw, Value};
decode_value(<<3, Value/binary>>) -> {group, binary_to_term(Value)}.


ensure_dir(DBHandle, BaseDir) ->
    PathParts = hb_path:term_to_path_parts(BaseDir),
    [First | Rest] = PathParts,
    Result = ensure_dir(DBHandle, First, Rest),
    Result.


ensure_dir(DBHandle, CurrentPath, []) ->
    maybe_create_dir(DBHandle, CurrentPath, nil),
    ok;
ensure_dir(DBHandle, CurrentPath, [Next]) ->
    maybe_create_dir(DBHandle, CurrentPath, Next),
    ensure_dir(DBHandle, hb_store:join([CurrentPath, Next]), []);
ensure_dir(DBHandle, CurrentPath, [Next | Rest]) ->
    maybe_create_dir(DBHandle, CurrentPath, Next),
    ensure_dir(DBHandle, hb_store:join([CurrentPath, Next]), Rest).


maybe_create_dir(DBHandle, DirPath, Value) ->
    CurrentValueSet =
        case rocksdb:get(DBHandle, DirPath, #{}) of
            not_found -> sets:new();
            {ok, CurrentValue} ->
                {group, DecodedOldValue} = decode_value(CurrentValue),
                DecodedOldValue
        end,
    NewValueSet =
        case Value of
            nil -> CurrentValueSet;
            _ -> sets:add_element(Value, CurrentValueSet)
        end,
    rocksdb:put(DBHandle, DirPath, encode_value(group, NewValueSet), #{}).


open_rockdb(RawDir) ->
    filelib:ensure_dir(Dir = ensure_list(RawDir)),
    Options = [{create_if_missing, true}],
    rocksdb:open(Dir, Options).


% Helper function to convert lists to binaries
convert_if_list(Value) when is_list(Value) ->
    join(Value);  % Perform the conversion if it's a list
convert_if_list(Value) ->
    Value.  % Leave unchanged if it's not a list


%% @doc Ensure that the given filename is a list, not a binary.
ensure_list(Value) when is_binary(Value) -> binary_to_list(Value);
ensure_list(Value) -> Value.


maybe_convert_to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
maybe_convert_to_binary(Value) when is_binary(Value) ->
    Value.


join(Key) when is_list(Key) ->
    KeyList = hb_store:join(Key),
    maybe_convert_to_binary(KeyList);
join(Key) when is_binary(Key) -> Key.


collect(Iterator) ->
    case rocksdb:iterator_move(Iterator, <<>>) of
        {error, invalid_iterator} -> [];
        {ok, Key, Value} ->
            DecodedValue = decode_value(Value),
            collect(Iterator, [{Key, DecodedValue}])
    end.


collect(Iterator, Acc) ->
    case rocksdb:iterator_move(Iterator, next) of
        {ok, Key, Value} ->
            % Continue iterating, accumulating the key-value pair in the list
            DecodedValue = decode_value(Value),
            collect(Iterator, [{Key, DecodedValue} | Acc]);
        {error, invalid_iterator} ->
            % Reached the end of the iterator, return the accumulated list
            lists:reverse(Acc)
    end.


maybe_append_key_to_group(Key, CurrentDirContents) ->
    case decode_value(CurrentDirContents) of
        {group, GroupSet} ->
            BaseName = filename:basename(Key),
            NewGroupSet = sets:add_element(BaseName, GroupSet),
            encode_value(group, NewGroupSet);
        _ ->
            CurrentDirContents
    end.
%%%=============================================================================
%%% Tests
%%%=============================================================================


-ifdef(ENABLE_ROCKSDB).
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


get_or_start_server() ->
    % Store = lists:keyfind(hb_store_rocksdb2, 1, hb_store:test_stores()),
    Opts = #{
             <<"store-module">> => hb_store_rocksdb,
             <<"name">> => <<"cache-TEST/rocksdb">>
            },
    case start_link(Opts) of
        {ok, Pid} ->
            Pid;
        {error, {already_started, Pid}} ->
            Pid
    end.


write_read_test_() ->
    {foreach,
     fun() ->
             Pid = get_or_start_server(),
             unlink(Pid)
     end,
     fun(_) -> reset([]) end,
     [{"can read/write data",
       fun() ->
               ok = write(#{}, <<"test_key">>, <<"test_value">>),
               {ok, Value} = read(#{}, <<"test_key">>),

               ?assertEqual(<<"test_value">>, Value)
       end},
      {"returns not_found for non existing keys",
       fun() ->
               Value = read(#{}, <<"non_existing">>),
               ?assertEqual(not_found, Value)
       end},
      {"follows links",
       fun() ->
               ok = write(#{}, <<"test_key2">>, <<"value_under_linked_key">>),
               ok = make_link(#{}, <<"test_key2">>, <<"test_key">>),
               {ok, Value} = read(#{}, <<"test_key">>),

               ?assertEqual(<<"value_under_linked_key">>, Value)
       end}]}.


api_test_() ->
    {foreach,
     fun() ->
             Pid = get_or_start_server(),
             unlink(Pid)
     end,
     fun(_) -> reset([]) end,
     [{"write/3 can automatically create folders",
       fun() ->
               ok = write(#{}, <<"messages/key1">>, <<"val1">>),
               ok = write(#{}, <<"messages/key2">>, <<"val2">>),

               {ok, Items} = list(#{}, <<"messages">>),
               ?assertEqual(
                 lists:sort([<<"key1">>, <<"key2">>]),
                 lists:sort(Items)),
               {ok, Item} = read(#{}, <<"messages/key1">>),
               ?assertEqual(<<"val1">>, Item)
       end},
      {"list/2 lists keys under given path",
       fun() ->
               ok = write(#{}, <<"messages/key1">>, <<"val1">>),
               ok = write(#{}, <<"messages/key2">>, <<"val2">>),
               ok = write(#{}, <<"other_path/key3">>, <<"val3">>),
               {ok, Items} = list(#{}, <<"messages">>),
               ?assertEqual(
                 lists:sort([<<"key1">>, <<"key2">>]), lists:sort(Items))
       end},
      {"list/2 when database is empty",
       fun() ->
               ?assertEqual({error, not_found}, list(#{}, <<"process/slot">>))
       end},
      {"make_link/3 creates a link to actual data",
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
      {"type/2 can identify simple items",
       fun() ->
               ok = write(#{}, <<"simple_item">>, <<"test">>),
               ?assertEqual(simple, type(#{}, <<"simple_item">>))
       end},
      {"type/2 returns not_found for non existing keys",
       fun() ->
               ?assertEqual(not_found, type(#{}, <<"random_key">>))
       end},
      {"type/2 resolves links before checking real type of the following item",
       fun() ->
               ok = write(#{}, <<"messages/key1">>, <<"val1">>),
               ok = write(#{}, <<"messages/key2">>, <<"val2">>),

               make_link(#{}, <<"messages">>, <<"CompositeKey">>),
               make_link(#{}, <<"messages/key2">>, <<"SimpleKey">>),
               ?assertEqual(composite, type(#{}, <<"CompositeKey">>)),
               ?assertEqual(simple, type(#{}, <<"SimpleKey">>))
       end},
      {"type/2 treats groups as composite items",
       fun() ->
               make_group(#{}, <<"messages_folder">>),
               ?assertEqual(composite, type(#{}, <<"messages_folder">>))
       end},
      {"resolve/2 resolves raw/groups items",
       fun() ->
               write(#{}, <<"top_level/level1/item1">>, <<"1">>),
               write(#{}, <<"top_level/level1/item2">>, <<"1">>),
               write(#{}, <<"top_level/level1/item3">>, <<"1">>),

               ?assertEqual(
                 <<"top_level/level1/item3">>,
                 resolve(#{}, <<"top_level/level1/item3">>))
       end},
      {"resolve/2 follows links",
       fun() ->
               write(#{}, <<"data/the_data_item">>, <<"the_data">>),
               make_link(#{}, <<"data/the_data_item">>, <<"top_level/level1/item">>),

               ?assertEqual(
                 <<"data/the_data_item">>,
                 resolve(#{}, <<"top_level/level1/item">>))
       end},
      {"make_group/2 creates a folder",
       fun() ->
               ?assertEqual(ok, make_group(#{}, <<"messages">>)),

               ?assertEqual(
                 list(#{}, <<"messages">>),
                 {ok, []})
       end},
      {"make_group/2 does not override folder contents",
       fun() ->
               write(#{}, <<"messages/id">>, <<"1">>),
               write(#{}, <<"messages/commitments">>, <<"2">>),

               ?assertEqual(ok, make_group(#{}, <<"messages">>)),

               ?assertEqual(
                 list(#{}, <<"messages">>),
                 {ok, [<<"id">>, <<"commitments">>]})
       end},
      {"make_group/2 making deep nested groups",
       fun() ->
               make_group(#{}, <<"messages/ids/items">>),
               ?assertEqual(
                 {ok, [<<"ids">>]},
                 list(#{}, <<"messages">>)),
               ?assertEqual(
                 {ok, [<<"items">>]},
                 list(#{}, <<"messages/ids">>)),
               ?assertEqual(
                 {ok, []},
                 list(#{}, <<"messages/ids/items">>))
       end},
      {"write/3 automatically does deep groups",
       fun() ->
               write(#{}, <<"messages/ids/item1">>, <<"1">>),
               write(#{}, <<"messages/ids/item2">>, <<"2">>),
               ?assertEqual(
                 {ok, [<<"ids">>]},
                 list(#{}, <<"messages">>)),
               ?assertEqual(
                 {ok, [<<"item2">>, <<"item1">>]},
                 list(#{}, <<"messages/ids">>)),
               ?assertEqual(read(#{}, <<"messages/ids/item1">>), {ok, <<"1">>}),
               ?assertEqual(read(#{}, <<"messages/ids/item2">>), {ok, <<"2">>})
       end}]}.


-endif.
-endif.
