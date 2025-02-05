%%% @doc A module that helps to render given Key graphs into the .dot files
-module(hb_cache_render).

-export([render/1, render/3]).

% Preparing data for testing
-export([prepare_unsigned_data/0, prepare_signed_data/0, prepare_deeply_nested_complex_message/0]).

-include("src/include/hb.hrl").
-include_lib("kernel/include/file.hrl").

% @doc Render the given Key into svg
render("*") ->
    Store = get_test_store(),
    {ok, Keys} = hb_store:list(Store, "/"),
    render(Keys);
render(StartKeys) ->
    Store = get_test_store(),
    os:cmd("rm new_render_diagram.dot"),
    {ok, IoDevice} = file:open("new_render_diagram.dot", [write]),
    ok = file:write(IoDevice, <<"digraph filesystem {\n">>),
    ok = file:write(IoDevice, <<"  node [shape=circle];\n">>),
    lists:foreach(fun(Key) -> render(IoDevice, Store, Key) end, StartKeys),
    ok = file:write(IoDevice, <<"}\n">>),
    file:close(IoDevice),
    os:cmd("dot -Tsvg new_render_diagram.dot -o new_render_diagram.svg"),
    os:cmd("open new_render_diagram.svg"),
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
            ignore;
        _OtherType ->
            ignore
    end.

get_test_store() ->
    hb_opts:get(store, no_viable_store,  #{store => {hb_store_fs,#{prefix => "TEST-cache-fs"}}}).

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
prepare_unsigned_data() ->
    Opts = #{store => {hb_store_fs,#{prefix => "TEST-cache-fs"}}},
    Item = test_unsigned(#{ <<"key">> => <<"Simple unsigned data item">> }),
    {ok, _Path} = hb_cache:write(Item, Opts).

prepare_signed_data() ->
    Opts = #{store => {hb_store_fs,#{prefix => "TEST-cache-fs"}}},
    Wallet = ar_wallet:new(),
    Item = test_signed(#{ <<"l2-test-key">> => <<"l2-test-value">> }, Wallet),
    %% Write the simple unsigned item
    {ok, _Path} = hb_cache:write(Item, Opts).

prepare_deeply_nested_complex_message() ->
    Opts = #{store => {hb_store_fs,#{prefix => "TEST-cache-fs"}}},
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