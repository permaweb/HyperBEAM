-module(hb_data_reader).
-export([metadata/2, read_range/4, fetch_full/3, stream/4, stream_from/5, next_chunk/5]).
-ifdef(TEST).
-export([compute_next_range/3]).
-endif.

-define(DEFAULT_CHUNK_SIZE, 1024 * 1024).

chunk_size(Opts) ->
    case hb_opts:get(stream_chunk_size, ?DEFAULT_CHUNK_SIZE, Opts) of
        Size when is_integer(Size), Size > 0 -> Size;
        _ -> ?DEFAULT_CHUNK_SIZE
    end.

%% @doc Retrieve dataset metadata via HEAD and fall back to Range probing.
metadata(ID, Opts) when is_binary(ID), is_map(Opts) ->
    case head_request(ID, Opts) of
        {ok, Meta} -> {ok, Meta};
        {error, _} -> range_metadata(ID, Opts)
    end.

%% @doc Materialize the bytes defined by a Range header.
read_range(ID, RangeHeader, Meta = #{size := Total}, Opts) when is_binary(RangeHeader) ->
    case hb_http_range:parse(RangeHeader, Total) of
        {ok, {Start, End}} ->
            case range_request(ID, Start, End, Meta, Opts) of
                {ok, RangeInfo} ->
                    {ok, RangeInfo#{ content_type => maps:get(content_type, Meta) }};
                {error, Reason} -> {error, Reason}
            end;
        {error, {range_not_satisfiable, _}} ->
            {error, {range_not_satisfiable, Total}};
        {error, invalid_range} ->
            {error, {invalid_range, Total}}
    end;
read_range(_, _, _, _) -> {error, invalid_arguments}.

%% @doc Fetch the entire body without loading it all at once unless unavoidable.
fetch_full(_ID, #{size := 0, content_type := CType}, _Opts) ->
    {ok, #{ data => <<>>, content_type => CType }};
fetch_full(ID, Meta = #{size := Total}, Opts) when Total > 0 ->
    End = Total - 1,
    case range_request(ID, 0, End, Meta, Opts) of
        {ok, #{body := Body}} ->
            {ok, #{ data => Body, content_type => maps:get(content_type, Meta) }};
        {error, {range_not_supported, _}} ->
            full_get(ID, Meta, Opts);
        {error, Reason} -> {error, Reason}
    end.

%% @doc Stream from the beginning, chunk size governed by configuration.
stream(ID, Meta = #{size := Total}, ChunkFun, Opts) when is_function(ChunkFun, 2) ->
    case Total of
        0 ->
            ChunkFun(<<>>, true),
            {ok, Meta};
        _ ->
            ChunkSize = chunk_size(Opts),
            stream_loop(ID, Meta, 0, ChunkSize, ChunkFun, Opts)
    end.

%% @doc Continue streaming starting from a specific byte offset.
stream_from(ID, Meta, Offset, ChunkFun, Opts) when is_function(ChunkFun, 2) ->
    ChunkSize = chunk_size(Opts),
    stream_loop(ID, Meta, Offset, ChunkSize, ChunkFun, Opts).

%% @doc Retrieve the next chunk without mutating state; used for preflight checks.
next_chunk(_ID, _Meta = #{size := Total}, Offset, _ChunkSize, _Opts) when Offset >= Total ->
    {error, done};
next_chunk(ID, Meta = #{size := Total}, Offset, ChunkSize, Opts) ->
    Normalized = case ChunkSize > 0 of true -> ChunkSize; false -> ?DEFAULT_CHUNK_SIZE end,
    {Start, End, _} = compute_next_range(Offset, Total, Normalized),
    range_request(ID, Start, End, Meta, Opts).

%% Internal helpers --------------------------------------------------------
head_request(ID, Opts) ->
    Req = base_request(ID, <<"HEAD">>),
    case hb_http:request(Req, Opts) of
        {ok, Msg} ->
            with_content_length(Msg,
                fun(Size) ->
                    {ok, #{
                        size => Size,
                        content_type => extract_content_type(Msg, Opts)
                    }}
                end,
                Opts
            );
        {error, _} -> {error, head_failed}
    end.

range_metadata(ID, Opts) ->
    case range_request(ID, 0, 0, #{}, Opts) of
        {ok, #{total := Total, content_type := CType}} ->
            {ok, #{ size => Total, content_type => CType }};
        {error, {range_not_satisfiable, Total}} ->
            {ok, #{ size => Total, content_type => hb_opts:get(range_default_content_type, <<"application/octet-stream">>, Opts) }};
        {error, Reason} -> {error, Reason}
    end.

full_get(ID, Meta, Opts) ->
    Req = base_request(ID, <<"GET">>),
    case hb_http:request(Req, Opts) of
        {ok, Msg} ->
            Body = hb_ao:get(<<"body">>, Msg, <<>>, Opts),
            {ok, #{ data => Body, content_type => maps:get(content_type, Meta) }};
        {error, Reason} -> {error, Reason}
    end.

stream_loop(ID, Meta = #{size := Total}, Offset, ChunkSize, ChunkFun, Opts) ->
    case Offset >= Total of
        true -> {ok, Meta};
        false ->
            {Start, End, IsFinal} = compute_next_range(Offset, Total, ChunkSize),
            case range_request(ID, Start, End, Meta, Opts) of
                {ok, #{body := Chunk, range_end := RangeEnd}} ->
                    ChunkFun(Chunk, IsFinal),
                    case IsFinal of
                        true -> {ok, Meta};
                        false -> stream_loop(ID, Meta, RangeEnd + 1, ChunkSize, ChunkFun, Opts)
                    end;
                {error, Reason} -> {error, Reason}
            end
    end.

range_request(ID, Start, End, Meta, Opts) when Start =< End ->
    RangeValue = range_header(Start, End),
    BaseReq = base_request(ID, <<"GET">>),
    Req = BaseReq#{ <<"range">> => RangeValue },
    case hb_http:request(Req, Opts) of
        {ok, Msg} ->
            handle_range_response(Msg, Start, End, Meta, Opts);
        {error, Msg} ->
            Status = hb_ao:get(<<"status">>, Msg, 0, Opts),
            case Status of
                416 ->
                    Total = range_total_from_header(hb_ao:get(<<"content-range">>, Msg, <<"bytes */0">>, Opts)),
                    {error, {range_not_satisfiable, Total}};
                _ -> {error, {http_error, Status}}
            end
    end;
range_request(_, _, _, _, _) -> {error, invalid_range_request}.

handle_range_response(Msg, Start, End, Meta, Opts) ->
    Status = hb_ao:get(<<"status">>, Msg, 200, Opts),
    Body = hb_ao:get(<<"body">>, Msg, <<>>, Opts),
    CType = extract_content_type(Msg, Opts),
    case Status of
        206 ->
            CR = hb_ao:get(<<"content-range">>, Msg, undefined, Opts),
            case parse_content_range(CR) of
                {ok, RangeStart, RangeEnd, Total} ->
                    ResolvedTotal = resolve_total(Total, Meta, Body, Start, RangeEnd),
                    {ok, #{
                        body => Body,
                        start => RangeStart,
                        range_end => RangeEnd,
                        total => ResolvedTotal,
                        content_type => CType,
                        final => final_flag(RangeEnd, ResolvedTotal)
                    }};
                error ->
                    ActualEnd = Start + erlang:max(byte_size(Body) - 1, 0),
                    ResolvedTotal = resolve_total(undefined, Meta, Body, Start, ActualEnd),
                    {ok, #{
                        body => Body,
                        start => Start,
                        range_end => ActualEnd,
                        total => ResolvedTotal,
                        content_type => CType,
                        final => final_flag(ActualEnd, ResolvedTotal)
                    }}
            end;
        200 ->
            case {Start, byte_size(Body)} of
                {0, Size} when Size >= (End - Start + 1) ->
                    ActualEnd = Start + Size - 1,
                    ResolvedTotal = resolve_total(Size, Meta, Body, Start, ActualEnd),
                    {ok, #{
                        body => Body,
                        start => Start,
                        range_end => ActualEnd,
                        total => ResolvedTotal,
                        content_type => CType,
                        final => final_flag(ActualEnd, ResolvedTotal)
                    }};
                _ ->
                    {error, {range_not_supported, Status}}
            end;
        _ ->
            {error, {http_error, Status}}
    end.

resolve_total(undefined, Meta, Body, Start, RangeEnd) ->
    case maps:get(size, Meta, undefined) of
        undefined -> Start + byte_size(Body);
        Size ->
            case RangeEnd >= Size of
                true -> Size;
                false -> Size
            end
    end;
resolve_total(Value, _Meta, _Body, _Start, _RangeEnd) when is_integer(Value) -> Value.

range_total_from_header(ContentRange) ->
    case parse_content_range(ContentRange) of
        {ok, _S, _E, Total} when is_integer(Total) -> Total;
        {unsatisfied, Total} -> Total;
        _ -> 0
    end.

parse_content_range(undefined) -> error;
parse_content_range(<<>>) -> error;
parse_content_range(<<"bytes ", Rest/binary>>) ->
    case binary:split(Rest, <<"/">>, []) of
        [<<"*">>, TotalBin] ->
            case safe_int(TotalBin) of
                {ok, Total} -> {unsatisfied, Total};
                error -> error
            end;
        [RangePart, TotalBin] ->
            case {safe_int(TotalBin), binary:split(RangePart, <<"-">>, [])} of
                {{ok, Total}, [StartBin, EndBin]} ->
                    case {safe_int(StartBin), safe_int(EndBin)} of
                        {{ok, Start}, {ok, RangeEnd}} -> {ok, Start, RangeEnd, Total};
                        _ -> error
                    end;
                _ -> error
            end;
        _ -> error
    end;
parse_content_range(_) -> error.

range_header(Start, End) when Start =< End ->
    iolist_to_binary([
        <<"bytes=" >>,
        integer_to_binary(Start),
        <<"-" >>,
        integer_to_binary(End)
    ]);
range_header(Start, End) ->
    error({invalid_range_bounds, Start, End}).

with_content_length(Msg, Fun, Opts) ->
    case hb_ao:get(<<"content-length">>, Msg, undefined, Opts) of
        undefined -> {error, missing_content_length};
        Bin ->
            case safe_int(Bin) of
                {ok, Size} -> Fun(Size);
                error -> {error, invalid_content_length}
            end
    end.

extract_content_type(Msg, Opts) ->
    hb_ao:get(
        <<"content-type">>,
        Msg,
        hb_opts:get(range_default_content_type, <<"application/octet-stream">>, Opts),
        Opts
    ).

safe_int(Bin) when is_binary(Bin) ->
    try {ok, binary_to_integer(Bin)} catch _:_ -> error end;
safe_int(Int) when is_integer(Int) -> {ok, Int};
safe_int(_) -> error.

base_request(ID, Method) ->
    #{
        <<"multirequest-responses">> => 1,
        <<"path">> => <<"/raw/", ID/binary>>,
        <<"method">> => Method
    }.

compute_next_range(Offset, Total, ChunkSize) when Offset < Total, ChunkSize > 0 ->
    Start = Offset,
    End = erlang:min(Start + ChunkSize - 1, Total - 1),
    {Start, End, End >= Total - 1};
compute_next_range(_, Total, _) when Total =< 0 -> {0, -1, true};
compute_next_range(_, _, _) -> {0, -1, true}.

final_flag(_RangeEnd, undefined) -> false;
final_flag(RangeEnd, Total) when is_integer(Total) -> RangeEnd >= Total - 1.
