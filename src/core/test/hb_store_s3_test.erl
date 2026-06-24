%%% @doc Offline tests for `hb_store_s3'. A `hb_mock_server' instance stands in
%%% for the S3 endpoint, backed by an ETS table so that a PUT followed by a GET
%%% round-trips the stored bytes without any live network access.
-module(hb_store_s3_test).
-include_lib("eunit/include/eunit.hrl").

-define(BUCKET, <<"test-bucket">>).

%% @doc Start a mock S3 endpoint backed by an ETS table. The catch-all handler
%% stores PUT bodies under their request path and serves them back on GET,
%% returning 404 for unknown keys.
start_mock_s3() ->
    application:ensure_all_started(inets),
    Table = ets:new(s3_mock, [set, public]),
    Handler = make_handler(Table),
    {ok, Server, Handle} =
        hb_mock_server:start([{"/[...]", s3, Handler}]),
    {Server, Handle, Table}.

stop_mock_s3({_Server, Handle, Table}) ->
    hb_mock_server:stop(Handle),
    ets:delete(Table).

make_handler(Table) ->
    fun(Req) -> handle(Table, Req) end.

handle(Table, Req) ->
    Method = maps:get(<<"method">>, Req),
    Path = maps:get(<<"path">>, Req),
    Body = maps:get(<<"body">>, Req, <<>>),
    case Method of
        <<"PUT">> ->
            ets:insert(Table, {Path, Body}),
            {200, <<>>};
        <<"GET">> ->
            case ets:lookup(Table, Path) of
                [{Path, Stored}] -> {200, Stored};
                [] -> {404, <<>>}
            end
    end.

store(Server) ->
    #{
        <<"store-module">> => hb_store_s3,
        <<"endpoint">> => Server,
        <<"bucket">> => ?BUCKET,
        <<"http-client">> => httpc
    }.

write_read_round_trips_bytes_test() ->
    Mock = {Server, _Handle, _Table} = start_mock_s3(),
    try
        Store = store(Server),
        Key = <<"objects/hello">>,
        Value = <<"s3 payload bytes">>,
        ?assertEqual(
            ok,
            hb_store_s3:write(Store, #{ Key => Value }, #{})
        ),
        ?assertEqual(
            {ok, Value},
            hb_store_s3:read(Store, #{ <<"read">> => Key }, #{})
        )
    after
        stop_mock_s3(Mock)
    end.

read_missing_key_returns_not_found_test() ->
    Mock = {Server, _Handle, _Table} = start_mock_s3(),
    try
        Store = store(Server),
        ?assertEqual(
            {error, not_found},
            hb_store_s3:read(Store, #{ <<"read">> => <<"objects/absent">> }, #{})
        )
    after
        stop_mock_s3(Mock)
    end.

type_reflects_presence_test() ->
    Mock = {Server, _Handle, _Table} = start_mock_s3(),
    try
        Store = store(Server),
        Key = <<"objects/typed">>,
        ?assertEqual(
            {error, not_found},
            hb_store_s3:type(Store, #{ <<"type">> => Key }, #{})
        ),
        ok = hb_store_s3:write(Store, #{ Key => <<"data">> }, #{}),
        ?assertEqual(
            {ok, simple},
            hb_store_s3:type(Store, #{ <<"type">> => Key }, #{})
        )
    after
        stop_mock_s3(Mock)
    end.

resolve_is_identity_test() ->
    ?assertEqual(
        {ok, <<"objects/key">>},
        hb_store_s3:resolve(#{}, #{ <<"resolve">> => <<"objects/key">> }, #{})
    ).

scope_is_remote_test() ->
    ?assertEqual(remote, hb_store_s3:scope()),
    ?assertEqual(remote, hb_store_s3:scope(#{})).
