%%%-----------------------------------------------------------------------------
%%% @doc A process wrapper over rocksdb storage. Replicates functionality of the
%%%      hb_fs_store module.
%%%
%%%      The process is only started (and supervised) when the following
%%%      configuration option is supplied (hb_opts):
%%%      ```
%%%      local_store =>
%%%         [{hb_store_rocksdb, #{prefix => "Test2"}}]
%%%      ```
%%%      The data is stored in two Column Families:
%%%      1. Default - for raw data (e.g. TX records)
%%%      2. Meta - for meta information
%%%         (<<"raw">>/<<"link">>/<<"composite">> or <<"group">>)
%%% @end
%%%-----------------------------------------------------------------------------
-module(hb_store_rocksdb).
-behaviour(gen_server).

-author("Oleg Tarasenko").

-behaviour(hb_store).

-define(TIMEOUT, 5000).

% Behaviour based callbacks
-export([start/1, stop/1]).
-export([read/2, write/3]).
-export([list/2]).
-export([reset/1]).
-export([make_link/3]).
-export([make_group/2]).
-export([type/2]).
-export([add_path/3, path/2]).
-export([resolve/2]).

% Starting process
-export([start_link/1]).
% Gen server callbacks
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_info/2, handle_call/3]).
-export([code_change/3]).

-type key() :: binary() | list().
-type value() :: binary() | list().

% -spec start_link() -> ignore | {ok, pid()}.
start_link([{hb_store_rocksdb, #{ prefix := Dir}}]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Dir, []);
start_link(Opts) ->
    logger:error("Rocksdb store server is not configured.: ~p", [Opts]),
    ignore.

% -spec start(#{dir := term()}) -> ignore | {ok, pid()}.
start(Opts) ->
    start_link(Opts).

-spec stop(any()) -> ok.
stop(_Opts) ->
    gen_server:stop(?MODULE).

-spec reset([]) -> ok | no_return().
reset(_Opts) ->
    gen_server:call(?MODULE, reset, ?TIMEOUT).

%%------------------------------------------------------------------------------
%% @doc Return path
%%
%% Recursively follows link messages
%% @end
%%------------------------------------------------------------------------------
path(_Opts, Path) ->
    Path.

%%------------------------------------------------------------------------------
%% @doc Read data by the key
%%
%% Recursively follows link messages
%% @end
%%------------------------------------------------------------------------------
-spec read(Opts, Key) -> Result when
    Opts :: map(),
    Key :: key() | list(),
    Result :: {ok, value()} | not_found | {error, {corruption, string()}} | {error, any()}.
read(Opts, RawKey) ->
    Key = join(RawKey),
    case meta(Key) of
        not_found ->
            case resolve(Opts, Key) of
                not_found ->
                    not_found;
                ResolvedPath ->
                    gen_server:call(?MODULE, {read, join(ResolvedPath)}, ?TIMEOUT)
            end;
        _Result ->
            gen_server:call(?MODULE, {read, Key}, ?TIMEOUT)
    end.

%%------------------------------------------------------------------------------
%% @doc Write given Key and Value to the database
%%
%% @end
%%------------------------------------------------------------------------------
-spec write(Opts, Key, Value) -> Result when
    Opts :: map(),
    Key :: key(),
    Value :: value(),
    Result :: ok | {error, any()}.
write(_Opts, RawKey, Value) ->
    Key = join(RawKey),
    gen_server:call(?MODULE, {write, Key, Value}, ?TIMEOUT).

%%------------------------------------------------------------------------------
%% @doc Return meta information about the given Key
%% @end
%%------------------------------------------------------------------------------
-spec meta(key()) -> {ok, binary()} | not_found.
meta(Key) ->
    gen_server:call(?MODULE, {meta, join(Key)}, ?TIMEOUT).

%%------------------------------------------------------------------------------
%% @doc List key/values stored in the storage so far.
%%      *Note*: This function is slow, and probably should not be used on
%%      production. Right now it's used for debugging purposes.
%%
%%      This can't work as it works for FS store, especially for large sets
%%      of data.
%% @end
%%------------------------------------------------------------------------------
-spec list(Opts, Path) -> Result when
    Opts :: any(),
    Path :: any(),
    Result :: [string()].
list(_Opts, Path) ->
    Result = gen_server:call(?MODULE, {list, hb_store:join(Path)}, ?TIMEOUT),
    {ok, Result}.

%% @doc Replace links in a path with the target of the link.
-spec resolve(Opts, Path) -> Result when
    Opts :: any(),
    Path :: binary() | list(),
    Result :: not_found | string().
resolve(_Opts, RawKey) ->
    Key = hb_store:join(RawKey),
    Path = filename:split(Key),

    case do_resolve("", Path) of
        not_found -> not_found;
        <<"">> -> "";
        % converting back to list, so hb_cache can remove common prefix
        BinResult -> binary_to_list(BinResult)
    end.

%%------------------------------------------------------------------------------
%% @doc Helper function that is useful when it's required to get a direct data
%%      under the given key, as it is, without following links
%% @end
%%------------------------------------------------------------------------------
read_no_follow(Key) ->
    gen_server:call(?MODULE, {read_no_follow, join(Key)}, ?TIMEOUT).

%%------------------------------------------------------------------------------
%% @doc Get type of the current item
%% @end
%%------------------------------------------------------------------------------
-spec type(Opts, Key) -> Result when
    Opts :: map(),
    Key :: binary(),
    Result :: composite | simple | not_found.

type(_Opts, RawKey) ->
    Key = join(RawKey),
    case meta(Key) of
        not_found ->
            not_found;
        {ok, <<"composite">>} ->
            composite;
        {ok, <<"group">>} ->
            composite;
        {ok, <<"link">>} ->
            simple;
        {ok, _} ->
            simple
    end.

%%------------------------------------------------------------------------------
%% @doc Creates group under the given path
%%      Creates an entry in the database and stored <<"group">> as a type in
%%      the meta family.
%% @end
%%------------------------------------------------------------------------------
make_group(_Opts, Path) ->
    BinPath = join(Path),
    gen_server:call(?MODULE, {make_group, BinPath}, ?TIMEOUT).

-spec make_link(any(), key(), key()) -> ok.
make_link(_, Key1, Key1) ->
    ok;
make_link(Opts, Existing, New) when is_list(Existing), is_list(New) ->
    ExistingBin = join(Existing),
    NewBin = join(New),
    make_link(Opts, ExistingBin, NewBin);
make_link(_Opts, Existing, New) ->
    gen_server:call(?MODULE, {make_link, Existing, New}, ?TIMEOUT).

%% @doc Add two path components together. // is not used
add_path(_Opts, Path1, Path2) ->
    Path1 ++ Path2.

%%%=============================================================================
%%% Gen server callbacks
%%%=============================================================================
init(Dir) ->
    logger:error("Starting the process"),
    {ok, DBHandle, [DefaultH, MetaH]} = open_rockdb(Dir),
    State = #{
        db_handle => DBHandle,
        dir => Dir,
        data_family => DefaultH,
        meta_family => MetaH
    },
    {ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_call({write, Key, Value}, _From, State) ->
    ok = write_meta(State, Key, <<"raw">>, #{}),
    Result = write_data(State, Key, Value, #{}),
    {reply, Result, State};
handle_call({make_group, Key}, _From, State) ->
    ok = write_meta(State, Key, <<"group">>, #{}),
    Result = write_data(State, Key, <<"group">>, #{}),
    {reply, Result, State};
handle_call({make_link, Key1, Key2}, _From, State) ->
    ok = write_meta(State, Key2, <<"link">>, #{}),
    Result = write_data(State, Key2, Key1, #{}),
    {reply, Result, State};
handle_call({read, Key}, _From, DBInfo) ->
    Result = get(DBInfo, Key, #{}),
    {reply, Result, DBInfo};
handle_call({read_no_follow, BaseKey}, _From, State) ->
    Result = get_data(State, BaseKey, #{}),
    {reply, Result, State};
handle_call({meta, Key}, _From, State) ->
    Result = get_meta(State, Key, #{}),
    {reply, Result, State};
handle_call(reset, _From, #{db_handle := DBHandle, dir := Dir}) ->
    ok = rocksdb:close(DBHandle),
    ok = rocksdb:destroy(Dir, []),

    {ok, NewDBHandle, [DefaultH, MetaH]} = open_rockdb(Dir),
    NewState = #{
        db_handle => NewDBHandle,
        dir => Dir,
        data_family => DefaultH,
        meta_family => MetaH
    },

    {reply, ok, NewState};
handle_call({list, Path}, _From, State = #{db_handle := DBHandle}) ->
    {ok, Iterator} = rocksdb:iterator(DBHandle, []),
    Items = collect(Iterator, Path),
    {reply, Items, State};
handle_call(_Request, _From, State) ->
    {reply, handle_call_unrecognized_message, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Private
%%%=============================================================================
open_rockdb(Dir) ->
    ColumnFamilies = [{"default", []}, {"meta", []}],
    Options = [{create_if_missing, true}, {create_missing_column_families, true}],
    rocksdb:open_with_cf(Dir, Options, ColumnFamilies).

get_data(#{data_family := F, db_handle := Handle}, Key, Opts) ->
    rocksdb:get(Handle, F, Key, Opts).

get_meta(#{meta_family := F, db_handle := Handle}, Key, Opts) ->
    rocksdb:get(Handle, F, Key, Opts).

write_meta(DBInfo, Key, Value, Opts) ->
    #{meta_family := ColumnFamily, db_handle := Handle} = DBInfo,
    rocksdb:put(Handle, ColumnFamily, Key, Value, Opts).

write_data(DBInfo, Key, Value, Opts) ->
    #{data_family := ColumnFamily, db_handle := Handle} = DBInfo,
    rocksdb:put(Handle, ColumnFamily, Key, Value, Opts).

% Note: this function is not yet optimized for tail recursion,
% which might be needed if we expect big amount of nested links
get(DBInfo, Key, Opts) ->
    case get_data(DBInfo, Key, Opts) of
        {ok, Value} ->
            case get_meta(DBInfo, Key, Opts) of
                {ok, <<"link">>} ->
                    % Automatically follow the link
                    get(DBInfo, Value, Opts);
                {ok, <<"raw">>} ->
                    {ok, Value};
                _OtherMeta ->
                    {ok, Value}
            end;
        not_found ->
            not_found
    end.

collect(Iterator, Path) ->
    {ok, Key, _Value} = rocksdb:iterator_move(Iterator, <<>>),
    collect(Iterator, Path, maybe_add_key(Key, Path, [])).

collect(Iterator, Path, Acc) ->
    case rocksdb:iterator_move(Iterator, next) of
        {ok, Key, _Value} ->
            % Continue iterating, accumulating the key-value pair in the list
            NewAcc = maybe_add_key(Key, Path, Acc),
            collect(Iterator, Path, NewAcc);
        {error, invalid_iterator} ->
            % Reached the end of the iterator, return the accumulated list
            lists:reverse(Acc)
    end.

maybe_add_key(Key, Prefix, Keys) ->
    case re:split(Key, Prefix, [{return, binary}]) of
        [Key] -> Keys; % The split did not really split anything
        [<<>>, <<"/", Suffix/binary>>] -> [erlang:binary_to_list(Suffix) | Keys];
        [<<>>, Suffix] -> [erlang:binary_to_list(Suffix) | Keys];
        _ -> Keys
    end.


maybe_convert_to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
maybe_convert_to_binary(Value) when is_binary(Value) ->
    Value.

do_resolve(CurrPath, []) ->
    CurrPath;
do_resolve(CurrPath, [LookupKey | Rest]) ->
    LookupPath = hb_store:join([CurrPath, LookupKey]),
    % ?debugFmt("Current lookup path: ~p", [LookupPath]),
    NewCurrentPath =
        case meta(LookupPath) of
            {ok, <<"link">>} ->
                read_no_follow(LookupPath);
            {ok, <<"raw">>} ->
                {ok, LookupPath};
            {ok, <<"group">>} ->
                do_resolve(LookupPath, Rest);
            not_found ->
                do_resolve(LookupPath, Rest)
        end,
    case NewCurrentPath of
        not_found ->
            list_to_binary(CurrPath);
        {ok, Path} ->
            do_resolve(Path, Rest);
        Result ->
            maybe_convert_to_binary(Result)
    end.
%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_or_start_server() ->
    Opts = [{hb_store_rocksdb, #{prefix => "TEST-data/rocksdb"}}],
    case start_link(Opts) of
        {ok, Pid} ->
            Pid;
        {error, {already_started, Pid}} ->
            Pid
    end.

join(Key) when is_list(Key) ->
    KeyList = hb_store:join(Key),
    maybe_convert_to_binary(KeyList);
join(Key) when is_binary(Key) -> Key.

write_read_test_() ->
    {foreach,
        fun() ->
            Pid = get_or_start_server(),
            unlink(Pid)
        end,
        fun(_) -> hb_store_rocksdb:reset([]) end, [
            {"can read/write data", fun() ->
                ok = write(#{}, <<"test_key">>, <<"test_value">>),
                {ok, Value} = read(ignored_options, <<"test_key">>),

                ?assertEqual(<<"test_value">>, Value)
            end},
            {"returns not_found for non existing keys", fun() ->
                Value = read(#{}, <<"non_existing">>),

                ?assertEqual(not_found, Value)
            end},
            {"follows links", fun() ->
                ok = write(#{}, <<"test_key2">>, <<"value_under_linked_key">>),
                ok = make_link(#{}, <<"test_key2">>, <<"test_key">>),
                {ok, Value} = read(#{}, <<"test_key">>),

                ?assertEqual(<<"value_under_linked_key">>, Value)
            end}
        ]}.

api_test_() ->
    {foreach,
        fun() ->
            Pid = get_or_start_server(),
            unlink(Pid)
        end,
        fun(_) -> reset([]) end, [
            {"list/2 lists keys under given path", fun() ->
                ok = write(#{}, <<"messages/key1">>, <<>>),
                ok = write(#{}, <<"messages/key2">>, <<>>),
                ok = write(#{}, <<"other_path/key3">>, <<>>),
                {ok, Items} = list(#{}, <<"messages">>),
                ?assertEqual(["key1", "key2"], Items)
            end},
            {"list/2 resolves given path before listing", fun() ->
                ok = write(#{}, <<"process/slot/1">>, <<>>),
                ok = write(#{}, <<"process/slot/2">>, <<>>),
                ok = write(#{}, <<"messages/key">>, <<>>),
                {ok, Items} = list(#{}, ["process", "slot"]),
                ?assertEqual(["1", "2"], Items)
            end},
            {"make_link/3 creates a link to actual data", fun() ->
                ok = write(ignored_options, <<"key1">>, <<"test_value">>),
                ok = make_link([], <<"key1">>, <<"key2">>),
                {ok, Value} = read([], <<"key2">>),

                ?assertEqual(<<"test_value">>, Value)
            end},
            {"make_group/2 creates a group", fun() ->
                ok = make_group(#{}, <<"folder_path">>),

                {ok, Value} = meta(<<"folder_path">>),
                ?assertEqual(<<"group">>, Value)
            end},
            {"make_link/3 does not create links if keys are same", fun() ->
                ok = make_link([], <<"key1">>, <<"key1">>),
                ?assertEqual(not_found, read(#{}, <<"key1">>))
            end},
            {"reset cleans up the database", fun() ->
                ok = write(ignored_options, <<"test_key">>, <<"test_value">>),

                ok = reset([]),
                ?assertEqual(not_found, read(ignored_options, <<"test_key">>))
            end},
            {
                "type/2 can identify simple items",
                fun() ->
                    ok = write(#{}, <<"simple_item">>, <<"test">>),
                    ?assertEqual(simple, type(#{}, <<"simple_item">>))
                end
            },
            {
                "type/2 returns not_found for non existing keys",
                fun() ->
                    ?assertEqual(not_found, type(#{}, <<"random_key">>))
                end
            },
            {
                "type/2 treats links as simple items",
                fun() ->
                    make_link(#{}, <<"ExistingKey">>, <<"NewKey">>),
                    ?assertEqual(simple, type(#{}, <<"NewKey">>))
                end
            },
            {
                "type/2 treats groups as composite items",
                fun() ->
                    make_group(#{}, <<"messages_folder">>),
                    ?assertEqual(composite, type(#{}, <<"messages_folder">>))
                end
            },
            {
                "resolve/2 resolutions for computed folder",
                fun() ->
                    % ├── computed
                    % │   └── 7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw
                    % │       ├── 76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc -> messages/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc
                    % │       ├── 8ZSzLqadFI0DyaUMEMvEcM9N5zkWqLU2lu7XhVejLGE -> messages/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc
                    % │       ├── LbfBoMI7xNYpBFv1Fsl2FSa8QYA2k9NtbzQTOKlN2TE -> messages/Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg
                    % │       ├── Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg -> messages/Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg
                    % │       ├── slot
                    % │       │   ├── 0 -> messages/Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg
                    % │       │   └── 1 -> messages/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc
                    % ├── messages
                    % │   ├── 76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc [raw]
                    % │   ├── 8ZSzLqadFI0DyaUMEMvEcM9N5zkWqLU2lu7XhVejLGE -> messages/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc
                    % │   ├── LbfBoMI7xNYpBFv1Fsl2FSa8QYA2k9NtbzQTOKlN2TE -> messages/Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg
                    % │   └── Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg [raw]

                    % Resolution examples:
                    % ["computed","7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw", "LbfBoMI7xNYpBFv1Fsl2FSa8QYA2k9NtbzQTOKlN2TE"] -> "messages/Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg"
                    % ["computed","7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw", ["slot", "1"]] -> "messages/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc"

                    % Create raw items in messages
                    write(#{}, <<"messages/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc/item">>, <<"Value">>),
                    write(#{}, <<"messages/Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg/item">>, <<"Value">>),

                    % Create symbolic links in messages
                    make_link(
                        #{},
                        <<"messages/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc">>,
                        <<"messages/8ZSzLqadFI0DyaUMEMvEcM9N5zkWqLU2lu7XhVejLGE">>
                    ),
                    make_link(
                        #{},
                        <<"messages/Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg">>,
                        <<"messages/LbfBoMI7xNYpBFv1Fsl2FSa8QYA2k9NtbzQTOKlN2TE">>
                    ),

                    % Create subdirectory in computed
                    make_group(#{}, <<"computed/7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw">>),

                    % Create symbolic links in computed/7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw
                    make_link(
                        #{},
                        <<"messages/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc">>,
                        <<"computed/7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc">>
                    ),
                    make_link(
                        #{},
                        <<"messages/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc">>,
                        <<"computed/7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw/8ZSzLqadFI0DyaUMEMvEcM9N5zkWqLU2lu7XhVejLGE">>
                    ),
                    make_link(
                        #{},
                        <<"messages/Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg">>,
                        <<"computed/7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw/LbfBoMI7xNYpBFv1Fsl2FSa8QYA2k9NtbzQTOKlN2TE">>
                    ),
                    make_link(
                        #{},
                        <<"messages/Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg">>,
                        <<"computed/7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw/Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg">>
                    ),

                    % Create subdirectory computed/7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw/slot
                    make_group(#{}, <<"computed/7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw/slot">>),

                    % Create symbolic links in computed/7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw/slot
                    make_link(
                        #{},
                        <<"messages/Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg">>,
                        <<"computed/7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw/slot/0">>
                    ),
                    make_link(
                        #{},
                        <<"messages/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc">>,
                        <<"computed/7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw/slot/1">>
                    ),

                    % Test
                    ?assertEqual(
                        "messages/Vsf2Eto5iQ9fghmH5RsUm4b9h0fb_CCYTVTjnHEDGQg",
                        resolve(#{}, [
                            "computed", "7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw", "LbfBoMI7xNYpBFv1Fsl2FSa8QYA2k9NtbzQTOKlN2TE"
                        ])
                    ),

                    ?assertEqual(
                        "messages/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc",
                        resolve(#{}, ["computed", "7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw", ["slot", "1"]])
                    ),

                    ?assertEqual(
                        "messages/76vSvK1yAlcGTPLyP7xEVUG7kiBxQrvTxhQor_KC8Wc",
                        resolve(#{}, ["computed", "7bi8NdEPLJwcD5ADWQ5PIoDlBpBWSw-9N7VXYe25Lvw", "slot", "1"])
                    )
                end
            },
            {"resolving interlinked item paths", fun() ->
                % messages
                % ├── csZNlQe-ehlhmCU8shC3vjhrW2qsaMAsQzs-ALjokOc [raw]
                % ├── -7ZAg8BW_itF-f9y4L0cY0xfz34iZBZ6jlDa9Tb23ME
                % │   ├── item
                % │   └── level1_key -> messages/UsxVZBMaIbe15LPrzqImczl7fhUPdmK3ANhGjuHkxGo
                % ├── UsxVZBMaIbe15LPrzqImczl7fhUPdmK3ANhGjuHkxGo
                % │   ├── item
                % │   └── level2_key -> messages/FGQgh1nQBwqi_kx7wpEIMAT2ltRsbieoZBHaUBK8riE
                % └── FGQgh1nQBwqi_kx7wpEIMAT2ltRsbieoZBHaUBK8riE
                % 	├── item
                % 	└── level3_key -> messages/csZNlQe-ehlhmCU8shC3vjhrW2qsaMAsQzs-ALjokOc
                %

                % resolve("messages", "-7ZAg8BW_itF-f9y4L0cY0xfz34iZBZ6jlDa9Tb23ME", ["level1_key", "level2_key", "level3_key"])
                %      -> "messages/csZNlQe-ehlhmCU8shC3vjhrW2qsaMAsQzs-ALjokOc"

                % Create the raw item in messages
                write(#{}, <<"messages/csZNlQe-ehlhmCU8shC3vjhrW2qsaMAsQzs-ALjokOc/item">>, <<"Value">>),

                % Create the subdirectory under messages
                make_group(#{}, <<"messages/-7ZAg8BW_itF-f9y4L0cY0xfz34iZBZ6jlDa9Tb23ME">>),

                % Add item in the subdirectory
                write(#{}, <<"messages/-7ZAg8BW_itF-f9y4L0cY0xfz34iZBZ6jlDa9Tb23ME/item">>, <<"Value">>),

                % Create symbolic link to level1_key
                make_link(
                    #{},
                    <<"messages/UsxVZBMaIbe15LPrzqImczl7fhUPdmK3ANhGjuHkxGo">>,
                    <<"messages/-7ZAg8BW_itF-f9y4L0cY0xfz34iZBZ6jlDa9Tb23ME/level1_key">>
                ),

                % Create group for messages/UsxVZBMaIbe15LPrzqImczl7fhUPdmK3ANhGjuHkxGo
                make_group(#{}, <<"messages/UsxVZBMaIbe15LPrzqImczl7fhUPdmK3ANhGjuHkxGo">>),

                % Add item in messages/UsxVZBMaIbe15LPrzqImczl7fhUPdmK3ANhGjuHkxGo
                write(#{}, <<"messages/UsxVZBMaIbe15LPrzqImczl7fhUPdmK3ANhGjuHkxGo/item">>, <<"Value">>),

                % Create symbolic link to level2_key
                make_link(
                    #{},
                    <<"messages/FGQgh1nQBwqi_kx7wpEIMAT2ltRsbieoZBHaUBK8riE">>,
                    <<"messages/UsxVZBMaIbe15LPrzqImczl7fhUPdmK3ANhGjuHkxGo/level2_key">>
                ),

                % Create group for messages/FGQgh1nQBwqi_kx7wpEIMAT2ltRsbieoZBHaUBK8riE
                make_group(#{}, <<"messages/FGQgh1nQBwqi_kx7wpEIMAT2ltRsbieoZBHaUBK8riE">>),

                % Add item in messages/FGQgh1nQBwqi_kx7wpEIMAT2ltRsbieoZBHaUBK8riE
                write(#{}, <<"messages/FGQgh1nQBwqi_kx7wpEIMAT2ltRsbieoZBHaUBK8riE/item">>, <<"Value">>),

                % Create symbolic link to level3_key
                make_link(
                    #{},
                    <<"messages/csZNlQe-ehlhmCU8shC3vjhrW2qsaMAsQzs-ALjokOc">>,
                    <<"messages/FGQgh1nQBwqi_kx7wpEIMAT2ltRsbieoZBHaUBK8riE/level3_key">>
                ),

                ?assertEqual(
                    "messages/csZNlQe-ehlhmCU8shC3vjhrW2qsaMAsQzs-ALjokOc",
                    resolve(#{}, [
                        "messages", "-7ZAg8BW_itF-f9y4L0cY0xfz34iZBZ6jlDa9Tb23ME", ["level1_key", "level2_key", "level3_key"]
                    ])
                )
            end}
        ]}.

-endif.