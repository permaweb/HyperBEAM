%%% @doc A module that helps to render given Key graphs into the .dot files
-module(hb_cache_render).

-export([render/0, render/1, render/2]).

% Preparing data for testing
-export([prepare_unsigned_data/1, prepare_signed_data/1, prepare_deeply_nested_complex_message/1]).

-include("src/include/hb.hrl").

-type store_type() :: hb_store_fs | hb_store_rocksdb.
-type key() :: binary().

-define(DIAGRAM_FILENAME, "store_diagram.dot").
-define(SVG_FILENAME, "store_diagram.svg").

render() ->
    case hb_opts:get(store, no_viable_store, #{}) of
        no_viable_store -> no_viable_store;
        {StoreType, _} -> render(StoreType)
    end.

% @doc Render the given Key into svg
-spec render(store_type()) -> ok.
render(StoreType) ->
    Store = get_test_store(StoreType),
    Keys =
        case Store of
            {hb_store_fs, _} ->
                {ok, K} = hb_store:list(Store, "/"),
                K;
            {hb_store_rocksdb, _} ->
                lists:map(fun({Key, _V}) -> Key end, hb_store_rocksdb:list())
        end,
    render(Keys, StoreType).

-spec render([key()], store_type()) -> ok.
render(StartKeys, StoreType) ->
    Store = get_test_store(StoreType),
    os:cmd(["rm ", ?DIAGRAM_FILENAME]),
    {ok, IoDevice} = file:open(?DIAGRAM_FILENAME, [write]),
    ok = file:write(IoDevice, <<"digraph filesystem {\n">>),
    ok = file:write(IoDevice, <<"  node [shape=circle];\n">>),
    lists:foreach(fun(Key) -> render(IoDevice, Store, Key) end, StartKeys),
    ok = file:write(IoDevice, <<"}\n">>),
    file:close(IoDevice),
    os:cmd(["dot -Tsvg ", ?DIAGRAM_FILENAME, " -o ", ?SVG_FILENAME]),
    os:cmd(["open ", ?SVG_FILENAME]),
    ok.

render(IoDevice, Store, Key) ->
    ResolvedPath = hb_store:resolve(Store, Key),
    JoinedPath = hb_store:join(Key),
    IsLink = ResolvedPath /= JoinedPath,
    case hb_store:type(Store, Key) of
        simple ->
            case IsLink of
                false ->
                    % just add the data node
                    add_data(IoDevice, ResolvedPath);
                true ->
                    % Add link (old node) -> add actual data node (with resolved path)
                    add_link(IoDevice, JoinedPath, JoinedPath),
                    add_data(IoDevice, ResolvedPath),
                    insert_arc(IoDevice, JoinedPath, ResolvedPath, "links-to")
                end;
        composite ->
            add_dir(IoDevice, JoinedPath),
            % Composite item also can be a link to another folder
            case IsLink of
                false ->
                    {ok, SubItems} = hb_store:list(Store, Key),
                    lists:foreach(
                        fun(SubItem) ->
                            insert_arc(IoDevice, hb_store:join(Key), hb_store:join([Key, SubItem]), "contains"),
                            render(IoDevice, Store, [Key, SubItem])
                        end,
                        SubItems);
                true ->
                    add_link(IoDevice, JoinedPath, JoinedPath),
                    insert_arc(IoDevice, JoinedPath, ResolvedPath, "links-to"),
                    render(IoDevice, Store, ResolvedPath)
            end;
        no_viable_store ->
            ?event(rocksdb, {no_viable_store, Store}),
            ignore;
        OtherType ->
            ?event(rocksdb, {uknown_type, OtherType}),
            ignore
    end.

get_test_store(hb_store_fs) ->
    {hb_store_fs, #{prefix => "TEST-cache-fs"}};
get_test_store(hb_store_rocksdb) ->
    {hb_store_rocksdb, #{prefix => "TEST-cache-rocksdb"}}.

% Helper functions
add_link(IoDevice, Id, Label) ->
    insert_circle(IoDevice, Id, Label, "green").

add_data(IoDevice, Id) ->
    insert_circle(IoDevice, Id, Id, "blue").

add_dir(IoDevice, Id) ->
    insert_circle(IoDevice, Id, Id, "yellow").

insert_arc(IoDevice, ID1, ID2, Label) ->
    ok = io:format(IoDevice, "  \"~s\" -> \"~s\" [label=\"~s\"];~n", [ID1, ID2, Label]).

insert_circle(IoDevice, ID, Label, Color) ->
    ok = io:format(IoDevice, "  \"~s\" [label=\"~s\", color=~s, style=filled];~n", [ID, Label, Color]).

% Preparing the test data
prepare_unsigned_data(StoreType) ->
    Opts = #{store => get_test_store(StoreType)},
    Item = test_unsigned(#{ <<"key">> => <<"Simple unsigned data item">> }),
    {ok, _Path} = hb_cache:write(Item, Opts).

prepare_signed_data(StoreType) ->
    Opts = #{store => get_test_store(StoreType)},
    Wallet = ar_wallet:new(),
    Item = test_signed(#{ <<"l2-test-key">> => <<"l2-test-value">> }, Wallet),
    %% Write the simple unsigned item
    {ok, _Path} = hb_cache:write(Item, Opts).

prepare_deeply_nested_complex_message(StoreType) ->
    Opts = #{store => get_test_store(StoreType)},
    Wallet = ar_wallet:new(),

    %% Create nested data
    Level3SignedSubmessage = test_signed([1,2,3], Wallet),
    Outer =
        #{
            <<"level1">> =>
                hb_message:attest(
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
                    ar_wallet:new()
                ),
            <<"a">> => <<"b">>
        },
    %% Write the nested item
    {ok, _} = hb_cache:write(Outer, Opts).

test_unsigned(Data) ->
    #{
        <<"base-test-key">> => <<"base-test-value">>,
        <<"data">> => Data
    }.

% test_signed(Data) -> test_signed(Data, ar_wallet:new()).
test_signed(Data, Wallet) ->
    hb_message:attest(test_unsigned(Data), Wallet).