%%% @doc A simple Arweave-like device backed by a flat byte address space.
%%% Useful for tests that need deterministic `chunk` reads and `append` writes.
-module(dev_arweave_flat).
-export([chunk/3, append/3, reset/3, stats/3, clear_reads/3]).
-include("include/hb.hrl").

-define(DEFAULT_TABLE, hb_arweave_flat_default).

%% @doc Read a byte range from the flat address space.
%% Expects `offset` as 1-based and optional `length`.
chunk(_Base, Request, Opts) ->
    Table = table(Opts),
    Data = get_value(Table, data, <<>>),
    Size = byte_size(Data),
    Offset = hb_util:int(hb_maps:get(<<"offset">>, Request, 1, Opts)),
    StartOffset = Offset - 1,
    MaybeLength =
        case hb_maps:find(<<"length">>, Request, Opts) of
            {ok, ReqLength} -> {ok, hb_util:int(ReqLength)};
            error -> not_set
        end,
    case StartOffset < 0 orelse StartOffset > Size of
        true ->
            {error, not_found};
        false ->
            Length =
                case MaybeLength of
                    not_set -> Size - StartOffset;
                    {ok, GivenLength} -> GivenLength
                end,
            case Length < 0 orelse StartOffset + Length > Size of
                true ->
                    {error, not_found};
                false ->
                    Range = binary:part(Data, StartOffset, Length),
                    prepend_value(Table, reads, {StartOffset, Length}),
                    {ok, Range}
            end
    end.

%% @doc Append bytes to the end of the flat address space.
%% Returns the inserted range in 0-based coordinates.
append(_Base, Request, Opts) ->
    Table = table(Opts),
    Body = hb_util:bin(hb_maps:get(<<"body">>, Request, <<>>, Opts)),
    GapSize = erlang:max(0, hb_util:int(opt(arweave_flat_gap, 0, Opts))),
    Data = get_value(Table, data, <<>>),
    StartOffset = byte_size(Data) + GapSize,
    NewData = <<Data/binary, 0:GapSize/unit:8, Body/binary>>,
    ets:insert(Table, {data, NewData}),
    prepend_value(Table, writes, {StartOffset, byte_size(Body)}),
    {ok, #{
        <<"start-offset">> => StartOffset,
        <<"length">> => byte_size(Body)
    }}.

%% @doc Reset the backing storage and all read/write stats.
reset(_Base, _Request, Opts) ->
    Table = table(Opts),
    ets:insert(Table, [{data, <<>>}, {reads, []}, {writes, []}]),
    ok.

%% @doc Clear read stats without touching stored bytes.
clear_reads(_Base, _Request, Opts) ->
    Table = table(Opts),
    ets:insert(Table, {reads, []}),
    ok.

%% @doc Return size and read/write traces for assertions in tests.
stats(_Base, _Request, Opts) ->
    Table = table(Opts),
    {ok, #{
        <<"size">> => byte_size(get_value(Table, data, <<>>)),
        <<"reads">> => lists:reverse(get_value(Table, reads, [])),
        <<"writes">> => lists:reverse(get_value(Table, writes, []))
    }}.

%% @doc Resolve the ETS table that stores the flat address space.
table(Opts) ->
    case opt(arweave_flat_table, undefined, Opts) of
        undefined -> ensure_default_table();
        Table -> ensure_table(Table)
    end.

%% @doc Ensure the default named table exists.
ensure_default_table() ->
    case ets:info(?DEFAULT_TABLE) of
        undefined ->
            ensure_table(
                ets:new(
                    ?DEFAULT_TABLE,
                    [named_table, public, set]
                )
            );
        _ ->
            ensure_table(?DEFAULT_TABLE)
    end.

%% @doc Ensure required rows exist in a table.
ensure_table(Table) ->
    ensure_row(Table, data, <<>>),
    ensure_row(Table, reads, []),
    ensure_row(Table, writes, []),
    Table.

%% @doc Ensure a row exists with a default value.
ensure_row(Table, Key, Default) ->
    case ets:lookup(Table, Key) of
        [] -> ets:insert(Table, {Key, Default});
        _ -> ok
    end.

%% @doc Prepend a value onto a list row.
prepend_value(Table, Key, Value) ->
    Current = get_value(Table, Key, []),
    ets:insert(Table, {Key, [Value | Current]}).

%% @doc Read a value from the table.
get_value(Table, Key, Default) ->
    case ets:lookup(Table, Key) of
        [{_, Value}] -> Value;
        [] -> Default
    end.

%% @doc Read an option using local overrides first.
opt(Key, Default, Opts) ->
    case maps:find(Key, Opts) of
        {ok, Value} -> Value;
        error -> hb_opts:get(Key, Default, Opts)
    end.
