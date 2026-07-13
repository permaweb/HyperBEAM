%%% @doc Hashpaths: Succinct executable claims of AO-Core transitions: Their
%%% results and dependencies, as addressable protocol values.
%%%
%%% Hashpaths abide by the following grammar:
%%%
%%% <pre>
%%%     Hashpath     :: Base "/" Request Variance? Dependencies? Equivalence?
%%%     Base         :: MessageID | Hashpath
%%%     Request      :: MessageID | PathString
%%%     Variance     :: "" | ">" VariedBaseID "+" VariedReqID
%%%     Dependencies :: "" | "@" DependenceMessageID
%%%     Equivalence  :: Normalizer ResultMessageID
%%%     Normalizer   :: "." | "="
%%% </pre>
%%%
%%% A compact form may omit fields when they are derivable: the vary pair is
%%% omitted when it is the identity vary, `Dependencies' when there are none, and
%%% the terminal before a result exists. Segments without explicit vary
%%% syntax are not special: `HP/*=FinalResultID' is an ordinary claim that
%%% resolving `*' at `HP' yields `FinalResultID'.
%%%
%%% Every separator of the syntax (`/', `>', `+', `@', `=', `.') is outside
%%% the base64url alphabet, so the grammar is unambiguous without escaping.
%%% The request position holds an ID when the request is addressed, or a
%%% literal key when it is self-describing (e.g. `*').
-module(hb_hashpath).
%%% Create and parse hashpaths.
-export([format/2, parse/2, context/2]).
%%% Verify hashpath claims.
-export([verify_all/2, verify_part/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Encode a hashpath from an execution context used/returned by `hb_ao:do/1`
%% and its internal stages.
%% 
%% The first stage of format extracts the first of two universal hashpath elements
%% -- the `Base` ID or existing hashpath. We then recurse with this value and the
%% remaining context.
format(Ctx, Opts) ->
    maybe
        {ok, BasePart} ?= format_base(Ctx, Opts),
        {ok, RequestPart} ?= format_request(Ctx, Opts),
        <<
            BasePart/binary, "/", RequestPart/binary,
            (format_varied(Ctx, Opts))/binary,
            (format_dependencies(Ctx, Opts))/binary,
            (format_equivalence(Ctx, Opts))/binary
        >>
    else
        {not_found, Name} ->
            throw({context_not_viable, unavailable_field, Name})
    end.

%% @doc Utilize the `hashpath` of the prior resolution, if it is available,
%% falling back to the `BaseID` if known, and recomputing it only if necessary.
format_base(Ctx = #{ <<"base">> := #{ <<"...">> := PriorHashpath } }, _) ->
    {ok, PriorHashpath};
format_base(Ctx, Opts) ->
    find_id(<<"base">>, Ctx, Opts).

%% @doc General utility for extracting the ID of a message by its name from a
%% context if it is already known, recomputing only if necessary.
find_id(Name, Ctx, _Opts) when is_map_key(<<Name/binary, "-id">>, Ctx) ->
    {ok, maps:get(Name, Ctx)};
find_id(Name, Ctx, Opts) when is_map_key(Name, Ctx) ->
    case hb_opts:get(<<"hashpath">>, enabled, Opts) of
        enabled ->
            {ok, hb_message:id(maps:get(Name, Ctx), signers, Opts)};
        _ ->
            {not_found, Name}
    end;
find_id(Name, _Ctx, _Opts) ->
    {not_found, Name}.

%% @doc Format the varied base and requests, if given, into their hashpath
%% components.
format_varied(Ctx, Opts) ->
    maybe
        {ok, VBase} ?= find_id(<<"varied-base">>, Ctx, Opts),
        {ok, VReq} ?= find_id(<<"varied-request">>, Ctx, Opts),
        <<">", VBase/binary, "+", VReq/binary>>
    else
        {not_found, _} ->
            % Either the base or request is not found, so we omit the varied
            % component of the hashpath.
            <<>>
    end.

%% @doc If the dependencies of a resolution are known, format them into the
%% hashpath depends component. If not, return an empty string. Honors already
%% calculated dependency IDs if provided in the context.
format_dependencies(Ctx, Opts) ->
    case find_id(<<"dependencies">>, Ctx, Opts) of
        {ok, Depends} -> <<"@", Depends/binary>>;
        {not_found, _} -> <<>>
    end.

%% @doc If the result of the execution has already been calculated, format it
%% into the hashpath equivalence component. If not, return an empty string.
format_equivalence(Ctx = #{ <<"varied-result">> := Result }, Opts) ->
    <<(format_normalizer(Ctx, Opts)), Result/binary>>;
format_equivalence(_, _) -> <<>>.

%% @doc Format the normalizer component of the hashpath.
format_normalizer(#{ <<"normalizer">> := base }, _Opts) -> <<"=">>;
format_normalizer(_, _) -> <<".">>.

%% @doc Decode a hashpath into a list of context segments. The first segment will
%% have both a base and a request part, while the latter segments will only have
%% the request part -- the base being inferred from the result of the prior
%% segments.
parse(Hashpath, Opts) when is_binary(Hashpath) ->
    [Base, Req1 | Reqs] = binary:split(Hashpath, <<"/">>, [global]),
    [
        parse_part(Base, Req1, Opts)
    |
        lists:map(
            fun(ReqPart) -> parse_part(undefined, ReqPart, Opts) end,
            Reqs
        )
    ].

%% @doc Parse the last segment of a hashpath into an executable context that 
%% can be additionally executed upon.
context(Hashpath, Opts) ->
    case lists:reverse(binary:split(Hashpath, <<"/">>, [global])) of
        [ LoneBase ] -> #{ <<"base-id">> => LoneBase };
        [ LastReq, Rest ] ->
            ReconstitutedBase = binary:join(lists:reverse(Rest), <<"/">>),
            parse_part(ReconstitutedBase, LastReq, Opts);
        _ -> #{ <<"base-id">> => Hashpath }
    end.

%% @doc Calculate the context for a hashpath segment. If the base is known
%% explicitly, add it to the result from parsing the request part. If not,
%% parse the request part and return as-is.
parse_part(undefined, ReqPart, Opts) ->
    parse_request(ReqPart, Opts);
parse_part(Base, ReqPart, Opts) ->
    (parse_request(ReqPart, Opts, Base))#{ <<"base-id">> => Base }.

%% @doc Parse a single segment of the hashpath into a context segment.
parse_request(Part, Opts) ->
    maybe
        {next, NextDelim, Ctx1, Part2} ?=
            parse_request_id(Part, Opts),
        {ok, NextDelim2, Part3, Ctx2} ?=
            parse_varied(NextDelim, Part2, Ctx1, Opts),
        {ok, NextDelim3, Part4, Ctx3} ?=
            parse_dependencies(NextDelim2, Part3, Ctx2, Opts),
        {ok, Ctx4} ?=
            parse_equivalence(NextDelim3, Part4, Ctx3, Opts),
        {ok, Ctx4}
    end.

%% @doc Parse the request ID part of a hashpath segment. If the request ID is
%% the only part, it is returned as-is and the remainder of the part parsing is
%% skipped.
parse_request_id(Part, Opts) ->
    case next(Part, Opts) of
        {no_match, Part, <<>>} -> {ok, #{ <<"request-id">> => Part } };
        {Sep, ReqID, Part2} -> {next, Sep, Part2, #{ <<"request-id">> => ReqID }}
    end.

%% @doc If the delimiter that starts our segment is `>` we handle the inner
%% segment as a `VariedBase` and `VariedRequest` pair and get the next delimited
%% component. If the delimiter is not `>`, we pass the segment forward as-is.
parse_varied($>, Part, Ctx0, _Opts) ->
    {NextDelim, Next, After} = next([$@, $., $=], Part),
    case binary:split(Next, <<"+">>) of
        [VBase, VReq] ->
            {
                next,
                NextDelim,
                After,
                Ctx0#{ <<"varied-base">> => VBase, <<"varied-request">> => VReq }
            };
        Malformed ->
            {error, {invalid_variance_parts, Malformed}}
    end;
parse_varied(NextDelim, Part, Ctx0, _Opts) ->
    {next, NextDelim, Part, Ctx0}.

%% @doc Parse the dependencies if present. We short-curcuit the parser and 
%% return the context early if we have already hit the end of the string.
parse_dependencies(no_match, <<>>, Ctx, _Opts) -> {ok, Ctx};
parse_dependencies($@, DepID, Ctx0, Opts) ->
    {NextDelim, Next, After} = next([$@, $., $=], Part),
    {next, NextDelim, Next, After, Ctx0#{ <<"dependencies-id">> => DepID }};
parse_dependencies(Delim, Part, Ctx0, _Opts) ->
    {next, Delim, Part, <<>>, Ctx0}.

%% @doc Parse the equivalent relationship if stated in the hashpath.
parse_equivalence(no_match, <<>>, Ctx, _Opts) -> {ok, Ctx};
parse_equivalence($=, ResultID, Ctx, _Opts) ->
    {ok, Ctx#{ <<"normalizer">> => base, <<"varied-result-id">> => ResultID }};
parse_equivalence($., ResultID, Ctx, _Opts) ->
    {ok, Ctx#{ <<"varied-result-id">> => ResultID }}.

%% @doc Utility to split at the next syntax delimiter (e.g. `=`, `.`, `>`, `@`).
%% Returns the syntax element matched, and the rest of the string. Notably, this
%% utility does not break apart `VBase+VReq` pairs. They are treated as a single
%% unit and parsed internally in `parse_varied/4`.
next(S) -> next([$=, $., $>, $@], S).
next(Symbols, S) -> hb_util:split_depth_string_aware_single(Symbols, S).

%% @doc Challenge a complete hashpath, verifying each part's claims.
verify_all(Bin, Opts) when is_binary(Bin) ->
    verify_all(parse(Bin, Opts), Opts);
verify_all([], _Opts) ->
    % We treat an empty hashpath as failing verification.
    false;
verify_all([Init | Parts], Opts) ->
    verify_all(Init, Parts, Opts).

verify_all(_FinalBase, [], _Opts) ->
    % The full hashpath has resolved and we have no more parts to verify. Each
    % passed verification.
    true;
verify_all(State, [Part | Rest], Opts) ->
    % Add the currently computed state to the part's context and verify it.
    case verify_context(Part#{ <<"base">> => State }, Opts) of
        {true, ComputedState} -> verify_all(ComputedState, Rest, Opts);
        false -> false
    end.

%% @doc Verify a single hashpath execution contained inside a larger hashpath
%% sequence.
verify_part(Hashpath, PartNum, Opts) when is_binary(Hashpath) ->
    verify_part(parse(Hashpath, Opts), PartNum, Opts);
verify_part(Hashpath, PartNum, Opts) ->
    maybe
        {ok, Base} ?= load(Hashpath, PartNum, Opts),
        verify_context(Part#{ <<"base">> => State }, Opts)
    end.

%% @doc Verify a full single context, parsed from a binary hashpath. The context
%% must contain a `Base' representation. We remove all of the non-`Base` and
%% `Request` fields, then utilize `hb_ao:do` to re-execute the context. Assuming
%% successful computation, we then verify the `VariedBase` and `VariedRequest`
%% fields against the parsed context, the `DependenciesID` if given, the 
%% `Normalizer` type, and finally the `Result` message itself. If all of these
%% verify, the context is considered valid.
verify_context(Ctx, Opts) ->
    StrippedCtx =
        maps:with(
            [<<"base">>, <<"request">>, <<"base-id">>, <<"request-id">>],
            Ctx#{ <<"opts">> => Opts }
        ),
    maybe
        {ok, ExecutedCtx} = hb_ao:do(StrippedCtx),
        true ?= verify_varied(Ctx, ExecutedCtx, Opts),
        true ?= verify_dependencies(Ctx, ExecutedCtx, Opts),
        true ?= verify_equivalence(Ctx, ExecutedCtx, Opts),
        {true, ExecutedCtx}
    else
        {error, Type} ->
            ?event_debug(
                hashpath_debug,
                {hashpath_verify_context_failed, {type, Type}, {ctx, Ctx}},
                Opts
            ),
            false
    end.

%% @doc If varied `Req` and `Base` statements were present in the hashpath,
%% we verify that they match the executed context.
verify_varied(HPCtx, ExecutedCtx, Opts) ->
    maybe
        {ok, HPVBase} ?= find_id(<<"varied-base">>, HPCtx, Opts),
        {ok, ExecVBase} ?= find_id(<<"varied-base">>, ExecutedCtx, Opts),
        true ?= HPVBase =:= ExecVBase
            orelse {error, <<"Varied `Base`s do not match">>},
        {ok, HPVReq} ?= find_id(<<"varied-request">>, HPCtx, Opts),
        {ok, ExecVReq} ?= find_id(<<"varied-request">>, ExecutedCtx, Opts),
        true ?= HPVReq =:= ExecVReq
            orelse {error, <<"Varied `Request`s do not match">>}
    else
        {not_found, _} ->
            % Skip validation if one or more required components are not 
            % provided.
            true
    end.

%% @doc Verify that the dependencies in the hashpath claim match those in the
%% executed context, if both are present.
verify_dependencies(HPCtx, ExecutedCtx, Opts) ->
    maybe
        {ok, HPDeps} ?= find_id(<<"dependencies">>, HPCtx, Opts),
        {ok, ExecDeps} ?= find_id(<<"dependencies">>, ExecutedCtx, Opts),
        true ?= HPDeps =:= ExecDeps
            orelse {error, <<"Dependencies do not match">>}
    else
        {not_found, <<"dependencies">>} ->
            % Skip verification if dependencies not provided
            true
    end.

%% @doc Verify that the results of the execution match those in the claim.
verify_equivalence(HPCtx, ExecutedCtx, Opts) ->
    maybe
        HPNormalizer = maps:get(<<"normalizer">>, HPCtx, replace),
        ExecNormalizer = maps:get(<<"normalizer">>, ExecutedCtx, replace),
        true ?= HPNormalizer =:= ExecNormalizer
            orelse {error, <<"Normalizers do not match">>},
        {ok, HPResult} ?= find_id(<<"varied-result">>, HPCtx, Opts),
        {ok, ExecResult} ?= find_id(<<"varied-result">>, ExecutedCtx, Opts),
        true ?= HPResult =:= ExecResult
            orelse {error, <<"Results do not match">>}
    end.

%% @doc Load the minimal executable base for a hashpath or a given part number
%% within it.
load(Hashpath, Opts) when is_binary(Hashpath) ->
    load(parse(Hashpath, Opts), Opts);
load(Parts, Opts) ->
    load(Parts, length(Parts), Opts).
load(Hashpath, PartNum, Opts) when is_binary(Hashpath) ->
    load(parse(Hashpath, Opts), PartNum, Opts);
load(Parts, PartNum, Opts) ->
    maybe
        [Init|Rest] ?= load_sequence(Parts, PartNum, Opts),
        {ok, InitState} ?= result_from_context(Init, Opts),
        lists:foldl(
            fun(Ctx, {error, X}) -> {error, X};
               (Ctx, {ok, State}) -> result_from_context(State, Ctx, Opts)
            end,
            {ok, InitState}
        )
    else
        [] ->
            {error, <<"Cannot load empty hashpath.">>};
        {error, X} ->
            {error, <<"Initial state not loadable: ", (hb_util:bin(X))/binary>>}
    end.

%% @doc Find the minimal sequence of hashpath contexts to load/apply such that
%% a valid base can be constructed at position `PartNum`.
load_sequence(Parts, OutOfBounds, Opts) when length(Parts) > OutOfBounds ->
    {
        error,
        <<
            "Hashpath part number `",
            (hb_util:bin(OutOfBounds))/binary,
            "` not found. Hashpath length: ",
            (hb_util:bin(length(Parts))),
            "."
        >>
    };
load_sequence(Parts, PartNum, Opts) ->
    {
        ok, 
        lists:reverse(
            lists:takewhile(
                fun(Ctx) ->
                    case result_from_context(Ctx, Opts) of
                        {error, _} -> true;
                        _ -> false
                    end,
                lists:reverse(lists:sublist(Parts, PartNum))
            )
        )
    }.

%% @doc Extract, if we can, a workable post-exec `message` from a context either
%% via the fully qualified result if possible or layering of the `varied-result`
%% atop the `base` if provided explicitly.
result_from_context(Ctx, Opts) -> result_from_context(undefined, Ctx, Opts).
result_from_context(
    undefined,
    #{ <<"normalizer">> => replace, <<"varied-result">> => VRes },
    _Opts
) ->
    {ok, VRes};
result_from_context(
    S,
    #{ <<"normalizer">> => replace, <<"varied-result">> => VRes },
    _Opts
) ->
    {ok, Vres};
result_from_context(
    undefined,
    Ctx = #{ <<"normalizer">> => base, <<"varied-result">> => VRes },
    _Opts
) ->
    case find_id(<<"base">>, Ctx, Opts) of
        {ok, Base} -> {ok, VRes#{ <<"...">> => S }};
        {not_found, _} ->
            {
                error,
                <<
                    "Context with `base` extension normalizer",
                    " without accessible `base`."
                >>
            }
    end;
result_from_context(
    S,
    #{ <<"normalizer">> => base, <<"varied-result">> => VRes },
    _Opts
) ->
    {ok, VRes#{ <<"...">> => S }};
result_from_context(S, #{ <<"normalizer">> => X }, _Opts) ->
    {error, <<"Unsupported normalizer `", (hb_util:bin(X))/binary, "`.">>};
result_from_context(S, Ctx, Opts) ->
    result_from_context(S, Ctx#{ <<"normalizer">> => replace }, Opts).

%%% Tests

full_form_round_trip_test() ->
    Opts = #{},
    HP =
        <<
            "BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4"
            "/tYRVDkT2X7wjYYVaZWuBBWWzZatEsMoR2NBjcJ8CmZk"
            ">a2Fub25pY2FsLXZhcmllZC1iYXNlLWlkLTAwMDAwMDAw"
            "+a2Fub25pY2FsLXZhcmllZC1yZXEtaWQtMDAwMDAwMDAw"
            "@ZGVwZW5kcy1tZXNzYWdlLWlkLTAwMDAwMDAwMDAwMDAw"
            "=cGF0Y2gtaWQtMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAw"
        >>,
    ?assertEqual(HP, format(parse(HP, Opts), Opts)).

compact_form_round_trip_test() ->
    Opts = #{},
    lists:foreach(
        fun(HP) -> ?assertEqual(HP, format(parse(HP, Opts), Opts)) end,
        [
            <<"BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4">>,
            <<"BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4/balance">>,
            <<"BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4/transfer/balance">>,
            <<"BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4/*"
              "=cGF0Y2gtaWQtMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAw">>
        ]
    ).