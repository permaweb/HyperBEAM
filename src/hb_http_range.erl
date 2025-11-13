-module(hb_http_range).
-export([parse/2, build_content_range/3, build_unsatisfied_content_range/1]).

%% @doc Parse a single HTTP Range header for a resource of TotalSize bytes.
%% Supports:
%%  - bytes=Start-End
%%  - bytes=Start-
%%  - bytes=-Suffix
%% Returns {ok, {Start, End}} or {error, invalid_range} or {error, {range_not_satisfiable, Total}}.
parse(RangeBin, TotalSize) when is_binary(RangeBin), is_integer(TotalSize), TotalSize >= 0 ->
    case TotalSize of
        0 -> {error, {range_not_satisfiable, 0}};
        _ -> do_parse(normalize(RangeBin), TotalSize)
    end;
parse(_, TotalSize) -> {error, {range_not_satisfiable, max(0, TotalSize)}}.

do_parse(<<"bytes=", Spec/binary>>, Total) ->
    case has_comma(Spec) of
        true -> {error, invalid_range}; % multi-range not supported
        false -> parse_single(Spec, Total)
    end;
do_parse(_, _Total) -> {error, invalid_range}.

parse_single(Spec, Total) ->
    Clean = strip_ws(Spec),
    case Clean of
        <<"-", SuffixBin/binary>> ->
            case safe_int(SuffixBin) of
                {ok, 0} -> {error, {range_not_satisfiable, Total}};
                {ok, N} when N > 0 ->
                    case N >= Total of
                        true -> {ok, {0, Total - 1}};
                        false -> {ok, {Total - N, Total - 1}}
                    end;
                error -> {error, invalid_range}
            end;
        _ ->
            case binary:split(Clean, <<"-">>, [global]) of
                [StartBin, <<>>] ->
                    case safe_int(StartBin) of
                        {ok, Start} when Start < Total -> {ok, {Start, Total - 1}};
                        {ok, _} -> {error, {range_not_satisfiable, Total}};
                        error -> {error, invalid_range}
                    end;
                [StartBin, EndBin] ->
                    case {safe_int(StartBin), safe_int(EndBin)} of
                        {{ok, Start}, {ok, End}} when Start =< End ->
                            case Start < Total of
                                true -> {ok, {Start, min(End, Total - 1)}};
                                false -> {error, {range_not_satisfiable, Total}}
                            end;
                        _ -> {error, invalid_range}
                    end;
                _ -> {error, invalid_range}
            end
    end.

has_comma(Bin) -> binary:match(Bin, <<",">>) =/= nomatch.

strip_ws(Bin) -> binary:replace(Bin, <<" ">>, <<>>, [global]).

normalize(Bin) ->
    %% Lower-case the unit portion only; keep numbers as-is
    Stripped = binary:replace(Bin, <<" ">>, <<>>, [global]),
    case Stripped of
        <<"bytes=", _/binary>> -> Stripped;
        _ ->
            Lower = to_lower(Stripped),
            Lower
    end.

to_lower(<<>>) -> <<>>;
to_lower(<<C, Rest/binary>>) when C >= $A, C =< $Z -> <<(C + 32), (to_lower(Rest))/binary>>;
to_lower(<<C, Rest/binary>>) -> <<C, (to_lower(Rest))/binary>>.

safe_int(<<>>) -> error;
safe_int(B) ->
    try {ok, binary_to_integer(B)}
    catch _:_ -> error end.

build_content_range(Start, End, Total) when is_integer(Start), is_integer(End), is_integer(Total) ->
    <<"bytes ", (integer_to_binary(Start))/binary, "-", (integer_to_binary(End))/binary, "/", (integer_to_binary(Total))/binary>>.

build_unsatisfied_content_range(Total) when is_integer(Total) ->
    <<"bytes */", (integer_to_binary(Total))/binary>>.
