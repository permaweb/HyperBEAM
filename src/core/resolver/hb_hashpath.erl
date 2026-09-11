%%% @doc Hashpaths: Succinct executable claims of AO-Core transitions: Their
%%% results and dependencies, as addressable protocol values.
%%%
%%% Hashpaths abide by the following grammar:
%%%
%%% <pre>
%%%     Hashpath     :: "ao://" MessageID Transition*
%%%     Transition   :: "/" Request Variance? Dependencies? Equivalence?
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
-export([format/2, format_request/2, parse/2, context/2]).
-export([generate/8, attach/3, reset/1]).
%%% Load and execute transition contexts.
-export([load/2, load/3]).
%%% Verify hashpath claims.
-export([verify_all/2, verify_part/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Encode a hashpath from an execution context or a sequence of contexts.
%%
%% The first stage of format extracts the first of two universal hashpath elements
%% -- the `Base` ID or existing hashpath. We then recurse with this value and the
%% remaining context.
format([], _Opts) -> <<>>;
format([First | Rest], Opts) ->
    lists:foldl(
        fun(Ctx, Prior) -> format(Ctx#{ <<"base-id">> => Prior }, Opts) end,
        format(First, Opts),
        Rest
    );
format(Ctx = #{ <<"varied-result">> := Result }, Opts)
        when not is_map(Result), not is_binary(Result), not is_list(Result),
             not ?IS_LINK(Result) ->
    % A native scalar has no standalone message ID. Its executable compact
    % address names the value without mistaking it for a pre-result context.
    format(maps:with(
        [<<"base">>, <<"base-id">>, <<"request">>, <<"request-id">>], Ctx), Opts);
format(Ctx, Opts) ->
    maybe
        {ok, Base} ?= format_base(Ctx, Opts),
        BasePart =
            case Base of
                <<"ao://", _/binary>> -> Base;
                _ -> <<"ao://", Base/binary>>
            end,
        case format_request_context(Ctx, Opts) of
            {not_found, <<"request">>} -> BasePart;
            {ok, RequestPart} ->
                <<
                    BasePart/binary, "/", RequestPart/binary,
                    (format_varied(Ctx, Opts))/binary,
                    (format_dependencies(Ctx, Opts))/binary,
                    (format_equivalence(Ctx, Opts))/binary
                >>
        end
    else
        {not_found, Name} ->
            throw({context_not_viable, unavailable_field, Name})
    end.

%% @doc Utilize the `hashpath` of the prior resolution, if it is available,
%% falling back to the `BaseID` if known, and recomputing it only if necessary.
format_base(#{ <<"base-id">> := ID }, _) -> {ok, ID};
format_base(Ctx = #{ <<"base">> := Base }, Opts) ->
    case hb_private:from_message(Base) of
        #{ <<"hashpath">> := HP, <<"hashpath-result">> := ID } ->
            case hb_message:id(Base, all, Opts) of
                ID -> {ok, HP};
                _ -> find_id(<<"base">>, Ctx, Opts)
            end;
        _ -> find_id(<<"base">>, Ctx, Opts)
    end;
format_base(Ctx, Opts) ->
    find_id(<<"base">>, Ctx, Opts).

%% @doc General utility for extracting the ID of a message by its name from a
%% context if it is already known, recomputing only if necessary.
find_id(Name, Ctx, _Opts) when is_map_key(<<Name/binary, "-id">>, Ctx) ->
    {ok, maps:get(<<Name/binary, "-id">>, Ctx)};
find_id(Name, Ctx, Opts) when is_map_key(Name, Ctx) ->
    case hb_cache:ensure_loaded(maps:get(Name, Ctx), Opts) of
        Value when is_map(Value); is_binary(Value); is_list(Value) ->
            {ok, hb_message:id(Value, all, Opts)};
        _ -> {not_found, Name}
    end;
find_id(Name, _Ctx, _Opts) ->
    {not_found, Name}.

%% @doc A request consisting only of a self-describing key can omit its ID.
format_request(Req, Opts) ->
    format_request_context(#{ <<"request">> => Req }, Opts).

%% @doc Use an explicit request ID when supplied by a parsed receipt.
format_request_context(#{ <<"request-id">> := ID }, _Opts) -> {ok, ID};
format_request_context(Ctx = #{ <<"request">> := Req }, Opts) ->
    case hb_private:reset(Req) of
        #{ <<"path">> := Key } = Path when map_size(Path) == 1 ->
            BinKey = hb_ao:normalize_key(Key),
            case binary:match(BinKey, [<<"/">>, <<">">>, <<"+">>,
                    <<"@">>, <<"=">>, <<".">>]) of
                nomatch when byte_size(BinKey) > 0, byte_size(BinKey) =/= 43 ->
                    {ok, BinKey};
                _ -> find_id(<<"request">>, Ctx, Opts)
            end;
        _ -> find_id(<<"request">>, Ctx, Opts)
    end;
format_request_context(Ctx, Opts) -> find_id(<<"request">>, Ctx, Opts).

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
format_equivalence(RawCtx, Opts) ->
    Ctx = equivalence_context(RawCtx, Opts),
    case find_id(<<"varied-result">>, Ctx, Opts) of
        {ok, Result} -> <<(format_normalizer(Ctx, Opts))/binary, Result/binary>>;
        {not_found, _} -> <<>>
    end.

%% @doc Format the normalizer component of the hashpath.
format_normalizer(#{ <<"normalizer">> := base }, _Opts) -> <<"=">>;
format_normalizer(_, _) -> <<".">>.

%% @doc A request overlay replaces the base with the fully overlaid request.
%% Its receipt therefore names that replacement, rather than the generic patch.
equivalence_context(
    Ctx = #{ <<"normalizer">> := request, <<"result">> := Result },
    _Opts
) ->
    maps:remove(<<"varied-result-id">>,
        Ctx#{ <<"normalizer">> => replace, <<"varied-result">> => Result });
equivalence_context(Ctx = #{ <<"normalizer">> := request }, Opts) ->
    {ok, Result} = result_from_context(Ctx, Opts),
    equivalence_context(Ctx#{ <<"result">> => Result }, Opts);
equivalence_context(Ctx, _) -> Ctx.

%% @doc Decode a hashpath into a list of context segments. The first segment will
%% have both a base and a request part, while the latter segments will only have
%% the request part -- the base being inferred from the result of the prior
%% segments.
parse(<<"ao://", Hashpath/binary>>, Opts) ->
    [Base | Reqs] = binary:split(Hashpath, <<"/">>, [global]),
    maybe
        true ?= valid_id(Base) orelse {error, <<"Invalid hashpath base.">>},
        Parts =
            case Reqs of
                [] -> [#{ <<"base-id">> => Base }];
                _ -> parse_parts(Base, Reqs, Opts)
            end,
        true ?= is_list(Parts) orelse Parts,
        true ?= lists:all(fun valid_ids/1, Parts)
            orelse {error, <<"Invalid hashpath field.">>},
        Parts
    end;
parse(_Hashpath, _Opts) -> {error, <<"Invalid hashpath scheme.">>}.

%% @doc Addresses in explicit receipt fields use the base64URL alphabet.
valid_ids(Ctx) ->
    lists:all(
        fun({<<"request-id">>, Key}) ->
                byte_size(Key) > 0 andalso
                    binary:match(Key, [<<"/">>, <<">">>, <<"+">>,
                        <<"@">>, <<"=">>, <<".">>]) =:= nomatch;
           ({_Name, ID}) -> valid_id(ID)
        end,
        maps:to_list(maps:without([<<"normalizer">>], Ctx))
    ).

valid_id(ID) ->
    re:run(ID, <<"\\A[A-Za-z0-9_-]{43}\\z">>, [{capture, none}]) =:= match.

%% @doc Parse a sequence, retaining the base only on its first transition.
parse_parts(_Base, [], _Opts) -> [];
parse_parts(Base, [Req | Rest], Opts) ->
    maybe
        {ok, Ctx} ?= parse_part(Base, Req, Opts),
        Parts = parse_parts(undefined, Rest, Opts),
        true ?= is_list(Parts) orelse Parts,
        [Ctx | Parts]
    end.

%% @doc Parse the last segment of a hashpath into an executable context that
%% can be additionally executed upon.
context(Hashpath, Opts) ->
    case parse(Hashpath, Opts) of
        [Only] -> Only;
        Parts when is_list(Parts) ->
            (lists:last(Parts))#{
                <<"base-id">> => format(lists:droplast(Parts), Opts)
            };
        Error -> Error
    end.

%% @doc Calculate the context for a hashpath segment. If the base is known
%% explicitly, add it to the result from parsing the request part. If not,
%% parse the request part and return as-is.
parse_part(undefined, ReqPart, Opts) ->
    parse_request(ReqPart, Opts);
parse_part(Base, ReqPart, Opts) ->
    maybe
        {ok, Ctx} ?= parse_request(ReqPart, Opts),
        {ok, Ctx#{ <<"base-id">> => Base }}
    end.

%% @doc Parse a single segment of the hashpath into a context segment.
parse_request(Part, Opts) ->
    maybe
        {next, NextDelim, Part2, Ctx1} ?=
            parse_request_id(Part, Opts),
        {next, NextDelim2, Part3, Ctx2} ?=
            parse_varied(NextDelim, Part2, Ctx1, Opts),
        {next, NextDelim3, Part4, Ctx3} ?=
            parse_dependencies(NextDelim2, Part3, Ctx2, Opts),
        {ok, Ctx4} ?=
            parse_equivalence(NextDelim3, Part4, Ctx3, Opts),
        {ok, Ctx4}
    end.

%% @doc Parse the request ID part of a hashpath segment. If the request ID is
%% the only part, it is returned as-is and the remainder of the part parsing is
%% skipped.
parse_request_id(Part, _Opts) ->
    case next(Part) of
        {_, <<>>, _} -> {error, <<"Empty hashpath request.">>};
        {no_match, Part, <<>>} -> {ok, #{ <<"request-id">> => Part } };
        {Sep, ReqID, Part2} -> {next, Sep, Part2, #{ <<"request-id">> => ReqID }}
    end.

%% @doc If the delimiter that starts our segment is `>` we handle the inner
%% segment as a `VariedBase` and `VariedRequest` pair and get the next delimited
%% component. If the delimiter is not `>`, we pass the segment forward as-is.
parse_varied($>, Part, Ctx0, _Opts) ->
    {NextDelim, Next, After} = next([$@, $., $=], Part),
    case binary:split(Next, <<"+">>, [global]) of
        [VBase, VReq] when byte_size(VBase) > 0, byte_size(VReq) > 0 ->
            {
                next,
                NextDelim,
                After,
                Ctx0#{
                    <<"varied-base-id">> => VBase,
                    <<"varied-request-id">> => VReq
                }
            };
        Malformed ->
            {error, {'invalid-variance-parts', Malformed}}
    end;
parse_varied(NextDelim, Part, Ctx0, _Opts) ->
    {next, NextDelim, Part, Ctx0}.

%% @doc Parse the dependencies if present. We short-circuit the parser and
%% return the context early if we have already hit the end of the string.
parse_dependencies(no_match, <<>>, Ctx, _Opts) -> {ok, Ctx};
parse_dependencies($@, Part, Ctx0, _Opts) ->
    {NextDelim, Next, After} = next([$@, $., $=], Part),
    case Next of
        <<>> -> {error, <<"Empty hashpath dependencies.">>};
        _ -> {next, NextDelim, After, Ctx0#{ <<"dependencies-id">> => Next }}
    end;
parse_dependencies(Delim, Part, Ctx0, _Opts) ->
    {next, Delim, Part, Ctx0}.

%% @doc Parse the equivalent relationship if stated in the hashpath.
parse_equivalence(no_match, <<>>, Ctx, _Opts) -> {ok, Ctx};
parse_equivalence(Delim, ResultID, Ctx, _Opts)
        when (Delim == $= orelse Delim == $.), byte_size(ResultID) > 0 ->
    case next([$=, $., $>, $@, $+], ResultID) of
        {no_match, _, <<>>} ->
            ResultCtx = Ctx#{ <<"varied-result-id">> => ResultID },
            case Delim of
                $= -> {ok, ResultCtx#{ <<"normalizer">> => base }};
                $. -> {ok, ResultCtx}
            end;
        _ -> {error, <<"Invalid hashpath result.">>}
    end;
parse_equivalence(_Delim, _Part, _Ctx, _Opts) ->
    {error, <<"Invalid hashpath equivalence.">>}.

%% @doc Utility to split at the next syntax delimiter (e.g. `=`, `.`, `>`, `@`).
%% Returns the syntax element matched, and the rest of the string. Notably, this
%% utility does not break apart `VBase+VReq` pairs. They are treated as a single
%% unit and parsed internally in `parse_varied/4`. Quotes and parentheses are
%% literal key characters, not grouping syntax.
next(S) -> next([$=, $., $>, $@], S).
next(Symbols, S) ->
    case binary:match(S, [<<Symbol>> || Symbol <- Symbols]) of
        nomatch -> {no_match, S, <<>>};
        {Position, 1} ->
            <<Part:Position/binary, Delimiter, Rest/binary>> = S,
            {Delimiter, Part, Rest}
    end.

%% @doc Challenge a complete hashpath, verifying each part's claims.
verify_all(Bin, Opts) when is_binary(Bin) ->
    verify_all(parse(Bin, Opts), Opts);
verify_all([], _Opts) ->
    % We treat an empty hashpath as failing verification.
    false;
verify_all([Init | Parts], Opts) ->
    try
        case load_field(<<"base">>, Init, Opts) of
            {ok, Base} -> verify_all(Base, [Init | Parts], Opts);
            _ -> false
        end
    catch
        _:_ -> false
    end;
verify_all({error, _}, _Opts) -> false.

verify_all(_FinalBase, [], _Opts) ->
    % The full hashpath has resolved and we have no more parts to verify. Each
    % passed verification.
    true;
verify_all(State, [Part | Rest], Opts) ->
    % Add the currently computed state to the part's context and verify it.
    Ctx = Part#{ <<"base">> => State },
    case verify_context(Ctx, Opts) of
        {true, ComputedState} ->
            verify_all(attach(ComputedState, format(Ctx, Opts), Opts), Rest, Opts);
        _ -> false
    end.

%% @doc Verify a single hashpath execution contained inside a larger hashpath
%% sequence.
verify_part(Hashpath, PartNum, Opts) when is_binary(Hashpath) ->
    verify_part(parse(Hashpath, Opts), PartNum, Opts);
verify_part(Parts, PartNum, Opts)
        when is_list(Parts), PartNum > 0, PartNum =< length(Parts) ->
    try
        Part = lists:nth(PartNum, Parts),
        case load_base(Parts, PartNum, Opts) of
            {ok, Base} ->
                case verify_context(Part#{ <<"base">> => Base }, Opts) of
                    {true, _} -> true;
                    _ -> false
                end;
            _ -> false
        end
    catch
        _:_ -> false
    end;
verify_part(_Parts, _PartNum, _Opts) -> false.

%% @doc Verify a full single context, parsed from a binary hashpath. The context
%% must contain a `Base' representation. We remove all of the non-`Base` and
%% `Request` fields, then utilize `hb_ao:resolve` to re-execute the context. Assuming
%% successful computation, we then verify the `VariedBase` and `VariedRequest`
%% fields against the parsed context, the `DependenciesID` if given, the
%% `Normalizer` type, and finally the `Result` message itself. If all of these
%% verify, the context is considered valid. Execution errors are preserved for
%% loading; public verification treats them as failed verification.
verify_context(#{ <<"base">> := Base } = Ctx, Opts)
        when not is_map_key(<<"request">>, Ctx),
             not is_map_key(<<"request-id">>, Ctx) ->
    case verify_commitments(Base, Opts) of
        true -> {true, Base};
        false -> false
    end;
verify_context(Ctx, Opts) ->
    maybe
        {ok, ExecutedCtx} ?= execute(Ctx, Opts),
        true ?= verify_varied(Ctx, ExecutedCtx, Opts),
        true ?= verify_dependencies(Ctx, ExecutedCtx, Opts),
        true ?= verify_equivalence(Ctx, ExecutedCtx, Opts),
        {true,
            case is_context(Ctx) of
                true -> maps:get(<<"base">>, ExecutedCtx);
                false -> maps:get(<<"result">>, ExecutedCtx)
            end
        }
    else
        _Error ->
            ?event_debug(
                hashpath_debug,
                {hashpath_verify_context_failed, {error, _Error}, {ctx, Ctx}},
                Opts
            ),
            case _Error of
                {error, _} -> _Error;
                _ -> false
            end
    end.

%% @doc If varied `Req` and `Base` statements were present in the hashpath,
%% we verify that they match the executed context.
verify_varied(HPCtx, ExecutedCtx, Opts) ->
    verify_id(<<"varied-base">>, HPCtx, ExecutedCtx, Opts) andalso
        verify_id(<<"varied-request">>, HPCtx, ExecutedCtx, Opts).

%% @doc Check each dependency against the input from which Vary selected it.
%% Coercion is determined by the selected device and checked by `verify_varied`.
verify_dependencies(HPCtx, ExecutedCtx, Opts) ->
    case find_id(<<"dependencies">>, HPCtx, Opts) of
        {not_found, _} -> true;
        {ok, ID} ->
            DependencyMsg =
                case find_id(<<"dependencies">>, ExecutedCtx, Opts) of
                    {ok, ID} ->
                        {ok, maps:get(<<"dependencies">>, ExecutedCtx)};
                    _ -> load_field(<<"dependencies">>, HPCtx, Opts)
                end,
            maybe
                {ok, RawDeps} ?= DependencyMsg,
                Deps = public_values(RawDeps),
                true ?= lists:sort(maps:keys(Deps)) =:=
                    [<<"base">>, <<"request">>],
                lists:all(
                    fun(Name) ->
                        verify_origins(
                            maps:get(Name, Deps),
                            maps:get(<<"varied-", Name/binary>>, ExecutedCtx),
                            maps:get(Name, ExecutedCtx),
                            [],
                            Opts
                        )
                    end,
                    [<<"base">>, <<"request">>]
                )
            else
                _ -> false
            end
    end.

%% @doc The dependency message follows the nested shape of the varied inputs.
dependencies(Ctx, Opts) ->
    maps:from_list(
        lists:map(
            fun(Name) ->
                Original = maps:get(Name, Ctx),
                Origin = format(#{ <<"base">> => Original }, Opts),
                {Name, dependency_paths(
                    maps:get(<<"varied-", Name/binary>>, Ctx),
                    Origin
                )}
            end,
            [<<"base">>, <<"request">>]
        )
    ).

%% @doc Vary selects and coerces each field from the addressed input. The
%% dependency tree records the selection without executing member lookups.
dependency_paths(Varied, Origin) when is_map(Varied) ->
    maps:map(
        fun(_Key, Value) -> dependency_paths(Value, Origin) end,
        public_values(Varied)
    );
dependency_paths(_Varied, Origin) ->
    {link, Origin, #{ <<"type">> => <<"link">>, <<"lazy">> => false }}.

%% @doc Read a source member only when the source is itself a message.
source_value(Key, Original, Opts) ->
    case hb_cache:ensure_loaded(Original, Opts) of
        Map when is_map(Map) -> maps:find(Key, Map);
        _ -> error
    end.

%% @doc Message members exclude private state and detached commitments.
%% Commitments bind the input IDs; they are checked when those IDs are loaded.
public_values(Map) ->
    maps:filter(
        fun(Key, _Value) -> not hb_private:is_private(Key) end,
        hb_message:uncommitted(Map)
    ).

%% @doc Check shape and origins before the selected device's deterministic
%% coercions. Loading links does not change the values they represent.
verify_origins(Deps, Varied, Original, Path, Opts) when ?IS_LINK(Deps) ->
    % A link may name a dependency branch or the input selected at a leaf.
    case load(Deps, Opts) of
        {ok, Value} when is_map(Value) ->
            verify_origins(Value, Varied, Original, Path, Opts) orelse
                verify_origin(Value, Original, Path, Opts);
        {ok, Value} -> verify_origin(Value, Original, Path, Opts);
        _ -> false
    end;
verify_origins(RawDeps, Varied, Original, Path, Opts) when is_map(RawDeps) ->
    case hb_cache:ensure_loaded(Varied, Opts) of
        Map when is_map(Map) ->
            Deps = public_values(RawDeps),
            Values = public_values(Map),
            lists:sort(maps:keys(Deps)) =:= lists:sort(maps:keys(Values)) andalso
                verify_commitments(Original, Opts) andalso
                lists:all(
                    fun({Key, Value}) ->
                        Source =
                            case source_value(Key, Original, Opts) of
                                {ok, Member} -> Member;
                                error -> Original
                            end,
                        verify_origins(maps:get(Key, Deps), Value, Source,
                            Path ++ [Key], Opts)
                    end,
                    maps:to_list(Values)
                );
        _ -> false
    end;
verify_origins(Origin, _Varied, Original, Path, Opts) when is_binary(Origin) ->
    case load(Origin, Opts) of
        {ok, Value} -> verify_origin(Value, Original, Path, Opts);
        _ -> false
    end;
verify_origins(_Deps, _Varied, _Original, _Path, _Opts) -> false.

%% @doc Select the dependency's field before comparing its uncoerced value.
%% The roots must be dependency messages, rather than collapsed origin leaves.
verify_origin(_Value, _Original, [], _Opts) -> false;
verify_origin(Input, Original, Path, Opts) ->
    maybe
        {ok, Selected} ?= select_origin(Path, Input, Opts),
        Value = hb_cache:ensure_all_loaded(Selected, Opts),
        true ?= hb_message:paranoid_verify(
            Value, Opts#{ <<"paranoid-verify">> => true }),
        hb_private:reset(Value) =:=
            hb_private:reset(hb_cache:ensure_all_loaded(Original, Opts))
    else
        _ -> false
    end.

%% @doc A compound coercion keeps the source from which its fields were made.
%% Verify each message layer crossed while selecting an origin's member.
select_origin([], Value, _Opts) -> {ok, Value};
select_origin([Key | Rest], Input, Opts) ->
    maybe
        true ?= verify_commitments(Input, Opts),
        Source =
            case source_value(Key, Input, Opts) of
                {ok, Member} -> Member;
                error -> Input
            end,
        select_origin(Rest, Source, Opts)
    end.

%% @doc Verify that the results of the execution match those in the claim.
verify_equivalence(HPCtx, RawExecutedCtx, Opts) ->
    ExecutedCtx = equivalence_context(RawExecutedCtx, Opts),
    case find_id(<<"varied-result">>, HPCtx, Opts) of
        {not_found, _} -> true;
        {ok, _} ->
            maps:get(<<"normalizer">>, HPCtx, replace) =:=
                maps:get(<<"normalizer">>, ExecutedCtx, replace) andalso
                verify_id(<<"varied-result">>, HPCtx, ExecutedCtx, Opts)
    end.

%% @doc An omitted claim is compact; a stated claim must be reproduced.
verify_id(Name, Claim, Executed, Opts) ->
    case find_id(Name, Claim, Opts) of
        {not_found, _} -> true;
        {ok, ID} -> find_id(Name, Executed, Opts) =:= {ok, ID}
    end.

%% @doc Re-execute a claim through the resolver without trusting cached results.
execute(Ctx, Opts) ->
    maybe
        {ok, Base} ?= load_field(<<"base">>, Ctx, Opts),
        {ok, Req} ?= load_request(Ctx, Opts),
        true ?= verify_commitments(Base, Opts),
        true ?= verify_commitments(Req, Opts),
        hb_ao:resolve(
            Base,
            Req,
            (internal_opts(Opts))#{ <<"return-context">> => true }
        )
    end.

%% @doc Load the minimal executable base for a hashpath or a given part number
%% within it.
load(Link = {link, ID, LinkOpts}, Opts) ->
    MergedOpts = hb_store:scope(
        hb_util:deep_merge(Opts, LinkOpts, Opts),
        hb_opts:get(scope, local, LinkOpts)
    ),
    Address =
        case LinkOpts of
            #{ <<"type">> := <<"link">> } ->
                #{ <<"hashpath+link">> := HP } =
                    hb_link:normalize(
                        #{ <<"hashpath">> => Link }, discard, MergedOpts),
                HP;
            _ -> ID
        end,
    case Address of
        _ when ?IS_HASHPATH(Address) -> load(Address, MergedOpts);
        _ when ?IS_ID(Address) -> read_id(Address, MergedOpts);
        _ ->
            case hb_cache:ensure_loaded(Link, MergedOpts) of
                Map when is_map(Map) -> {ok, Map};
                Serialized -> load(Serialized, MergedOpts)
            end
    end;
load(Hashpath, Opts) when is_binary(Hashpath) ->
    load(parse(Hashpath, Opts), Opts);
load(Parts, Opts) when is_list(Parts) ->
    load(Parts, length(Parts), Opts);
load({error, _} = Error, _Opts) -> Error.
load(Hashpath, PartNum, Opts) when is_binary(Hashpath) ->
    load(parse(Hashpath, Opts), PartNum, Opts);
load(Parts, PartNum, Opts)
        when is_list(Parts), is_integer(PartNum),
             PartNum > 0, PartNum =< length(Parts) ->
    Ctx = lists:nth(PartNum, Parts),
    maybe
        {ok, Result} ?= load_context(Parts, PartNum, Ctx, Opts),
        {ok, attach(Result, format(lists:sublist(Parts, PartNum), Opts), Opts)}
    end;
load(_Parts, _PartNum, _Opts) ->
    {error, <<"Hashpath part number out of bounds.">>}.

%% @doc Load a stored replacement or reconstruct the base of an overlay.
load_context(Parts, PartNum, Ctx, Opts) ->
    case is_context(Ctx) of
        true -> load_base(Parts, PartNum, Opts);
        false -> load_result(Parts, PartNum, Ctx, Opts)
    end.

%% @doc An explicit pre-result context presents the prior state as ancestry.
%% A compact request without witnesses still denotes the request's result.
is_context(Ctx) ->
    not maps:is_key(<<"varied-result">>, Ctx) andalso
        not maps:is_key(<<"varied-result-id">>, Ctx) andalso
        (maps:is_key(<<"varied-base-id">>, Ctx) orelse
            maps:is_key(<<"varied-base">>, Ctx) orelse
            maps:is_key(<<"dependencies-id">>, Ctx) orelse
            maps:is_key(<<"dependencies">>, Ctx)).

%% @doc Use an available result witness, or recompute the stated claim.
load_result(Parts, PartNum, Ctx, Opts) ->
    case {maps:get(<<"normalizer">>, Ctx, replace),
            load_field(<<"varied-result">>, Ctx, Opts)} of
        {replace, {ok, Result}} -> {ok, Result};
        {base, {ok, Patch}} ->
            maybe
                {ok, Base} ?= load_base(Parts, PartNum, Opts),
                result_from_context(Ctx#{ <<"base">> => Base,
                    <<"varied-result">> => Patch }, Opts)
            end;
        {_, _} ->
            maybe
                {ok, Base} ?= load_base(Parts, PartNum, Opts),
                case maps:is_key(<<"request">>, Ctx) orelse
                        maps:is_key(<<"request-id">>, Ctx) of
                    false -> {ok, Base};
                    true ->
                        case verify_context(Ctx#{ <<"base">> => Base }, Opts) of
                            {true, Result} -> {ok, Result};
                            {error, _} = Error -> Error;
                            false -> {error, <<"Hashpath claim does not verify.">>}
                        end
                end
            end
    end.

%% @doc A replacement can be loaded without any of the states before it.
load_base([First | _], 1, Opts) -> load_field(<<"base">>, First, Opts);
load_base(Parts, PartNum, Opts) -> load(Parts, PartNum - 1, Opts).

%% @doc Read an addressed context value, or use its supplied in-memory value.
load_field(Name, Ctx, Opts) when is_map_key(Name, Ctx) ->
    Value = hb_cache:ensure_loaded(maps:get(Name, Ctx), Opts),
    case maps:find(<<Name/binary, "-id">>, Ctx) of
        error -> {ok, Value};
        {ok, HP} when ?IS_HASHPATH(HP) ->
            maybe
                {ok, Addressed} ?= load(HP, Opts),
                check_id(Value, hb_message:id(Addressed, all, Opts), Opts)
            end;
        {ok, ID} -> check_id(Value, ID, Opts)
    end;
load_field(Name, Ctx, Opts) ->
    case maps:find(<<Name/binary, "-id">>, Ctx) of
        {ok, HP} when ?IS_HASHPATH(HP) -> load(HP, Opts);
        {ok, ID} -> read_id(ID, Opts);
        error -> {error, {missing, Name}}
    end.

%% @doc A stored value must reproduce its address before it can be a witness.
read_id(ID, Opts) ->
    Read =
        case hb_cache:read(ID, Opts) of
            {error, not_found} -> hb_cache:read(<<"data/", ID/binary>>, Opts);
            Result -> Result
        end,
    maybe
        {ok, Value} ?= Read,
        check_id(Value, ID, Opts)
    end.

%% @doc Check the claimed content address and its attached commitments.
check_id(Value, ID, Opts) ->
    maybe
        true ?= hb_message:id(Value, all, Opts) =:= ID
            orelse {error, <<"Hashpath value does not match its ID.">>},
        true ?= verify_commitments(Value, Opts)
            orelse {error, <<"Hashpath value has invalid commitments.">>},
        {ok, Value}
    end.

%% @doc Check commitments once the value's message layer is loaded.
verify_commitments(Value, Opts) when ?IS_LINK(Value) ->
    verify_commitments(hb_cache:ensure_loaded(Value, Opts), Opts);
verify_commitments(Value, Opts) when is_map(Value) ->
    hb_message:verify(Value, #{ <<"commitment-ids">> => <<"all">> }, Opts);
verify_commitments(_Value, _Opts) -> true.

%% @doc A compact request key denotes the ordinary one-key request message.
load_request(Ctx = #{ <<"request-id">> := Key }, Opts) when byte_size(Key) =/= 43 ->
    Req = #{ <<"path">> => Key },
    case find_id(<<"request">>, maps:remove(<<"request-id">>, Ctx), Opts) of
        {not_found, _} -> {ok, Req};
        {ok, ID} ->
            case hb_message:id(Req, all, Opts) of
                ID -> {ok, Req};
                _ -> {error, <<"Hashpath request does not match its key.">>}
            end
    end;
load_request(Ctx, Opts) -> load_field(<<"request">>, Ctx, Opts).

%% @doc Extract, if we can, a workable post-exec `message` from a context either
%% via the fully qualified result if possible or layering of the `varied-result`
%% atop the `base` if provided explicitly.
result_from_context(Ctx, Opts) ->
    maybe
        {ok, Result} ?= load_field(<<"varied-result">>, Ctx, Opts),
        case maps:get(<<"normalizer">>, Ctx, replace) of
            replace -> {ok, Result};
            Normalizer when Normalizer == base; Normalizer == request ->
                maybe
                    {ok, Original} ?=
                        case Normalizer of
                            base -> load_field(<<"base">>, Ctx, Opts);
                            request -> load_request(Ctx, Opts)
                        end,
                    {ok, hb_ao:set(Original, Result, internal_opts(Opts))}
                end;
            _ -> {error, <<"Unsupported hashpath normalizer.">>}
        end
    end.

%% @doc Generate a receipt from the original and varied execution messages.
%% Return its hashpath and complete context for challenge, preserving witnesses
%% under the execution's cache policy. The resolver supplies the overlaid result.
generate(Base, Req, Res, VariedBase, VariedReq, VariedRes, Overlay, Opts) ->
    Normalizer =
        case Overlay of
            Type when is_map(VariedRes), (Type == base orelse Type == request) ->
                Type;
            _ -> replace
        end,
    Ctx = #{
        <<"base">> => Base,
        <<"request">> => Req,
        <<"result">> => Res,
        <<"varied-base">> => VariedBase,
        <<"varied-request">> => VariedReq,
        <<"varied-result">> => VariedRes,
        <<"normalizer">> => Normalizer
    },
    Completed = Ctx#{ <<"dependencies">> => dependencies(Ctx, Opts) },
    HP = format(Completed, Opts),
    store(HP, Completed, Opts),
    {HP, Completed}.

%% @doc Preserve the witnesses required to port a receipt under the same cache
%% policy as its reusable execution result.
store(HP, Ctx, Opts) ->
    case hb_cache_control:derive_cache_settings([
            maps:get(<<"varied-result">>, Ctx),
            maps:get(<<"varied-request">>, Ctx)
        ], Opts) of
        #{ <<"store">> := true } ->
            lists:foreach(
                fun(Name) -> hb_cache:write(maps:get(Name, Ctx), Opts) end,
                [<<"base">>, <<"request">>, <<"dependencies">>]
            ),
            case maps:get(<<"varied-result">>, Ctx) of
                Patch when is_map(Patch); is_binary(Patch); is_list(Patch) ->
                    hb_cache:write(Patch, Opts);
                _ -> ok
            end,
            case maps:get(<<"normalizer">>, Ctx) of
                request -> hb_cache:write(maps:get(<<"result">>, Ctx), Opts);
                _ -> ok
            end,
            hb_cache:write(HP, Opts);
        _ -> not_caching
    end.

%% @doc Reset both the receipt and its association with the result value.
reset(Result) when is_map(Result) ->
    Result#{
        <<"priv">> =>
        maps:without(
            [<<"hashpath">>, <<"hashpath-result">>],
            hb_private:from_message(Result)
        )
    };
reset(Result) -> Result.

%% @doc Keep a receipt only while it still names the returned state.
attach(Result, HP, Opts) when is_map(Result) ->
    Priv = hb_private:from_message(Result),
    Result#{
        <<"priv">> => Priv#{
            <<"hashpath">> => HP,
            <<"hashpath-result">> => hb_message:id(Result, all, Opts)
        }
    };
attach(Result, _HP, _Opts) -> Result.

%% @doc Internal execution and overlays must not consume cached claims or
%% recursively construct receipts of their own.
internal_opts(Opts) ->
    maps:without(
        [
            <<"return-context">>, <<"force-message">>, <<"resolve-mode">>,
            <<"only">>, <<"prefer">>
        ],
        Opts#{
            <<"hashpath">> => ignore,
            <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
            <<"spawn-worker">> => false,
            <<"await-inprogress">> => false
        }
    ).

%%% Tests

full_form_round_trip_test() ->
    Opts = #{},
    HP =
        <<
            "ao://BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4"
            "/tYRVDkT2X7wjYYVaZWuBBWWzZatEsMoR2NBjcJ8CmZk"
            ">a2Fub25pY2FsLXZhcmllZC1iYXNlLWlkLTAwMDAwMDA"
            "+a2Fub25pY2FsLXZhcmllZC1yZXEtaWQtMDAwMDAwMDA"
            "@ZGVwZW5kcy1tZXNzYWdlLWlkLTAwMDAwMDAwMDAwMDA"
            "=cGF0Y2gtaWQtMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDA"
        >>,
    ?assertEqual(HP, format(parse(HP, Opts), Opts)).

compact_form_round_trip_test() ->
    Opts = #{},
    lists:foreach(
        fun(HP) ->
            ?assert(?IS_HASHPATH(HP)),
            ?assertEqual(HP, format(parse(HP, Opts), Opts))
        end,
        [
            <<"ao://BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4">>,
            <<"ao://BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4/balance">>,
            <<"ao://BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4/transfer/balance">>,
            <<"ao://BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4/*"
              "=cGF0Y2gtaWQtMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDA">>
        ]
    ).

%% @doc Reject incomplete fields, ambiguous separators, and invalid IDs.
malformed_receipt_test() ->
    ID = hb_message:id(#{}, all, #{}),
    lists:foreach(
        fun(HP) ->
            ?assertMatch({error, _}, parse(HP, #{})),
            ?assertMatch({error, _}, context(HP, #{})),
            ?assertMatch({error, _}, load(HP, #{})),
            ?assertNot(verify_all(HP, #{}))
        end,
        [
            <<>>,
            ID,
            <<ID/binary, "/">>,
            <<ID/binary, "/key">>,
            <<"ao://">>,
            <<"ao://ao://", ID/binary, "/key">>,
            <<"ao://", ID/binary, "/">>,
            <<"ao://", ID/binary, "//key">>,
            <<"ao://", ID/binary, "/key+extra">>,
            <<"ao://", ID/binary, "/key>", ID/binary, "+">>,
            <<"ao://", ID/binary, "/key>", ID/binary, "+", ID/binary, ">extra">>,
            <<"ao://", ID/binary, "/key@">>,
            <<"ao://", ID/binary, "/key=not-an-id">>,
            <<"ao://", ID/binary, "\n/key">>,
            <<"ao://", ID/binary, "/key=", ID/binary, "\n">>,
            <<"ao://", ID/binary, "/key=", ID/binary, ".", ID/binary>>
        ]
    ).

%% @doc Supplied context values cannot override their claimed addresses.
supplied_context_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{ <<"a">> => 1, <<"b">> => 2 },
    ?assertEqual({ok, 1}, load([#{
        <<"base">> => Base, <<"request">> => #{ <<"path">> => <<"a">> }
    }], Opts)),
    ?assertEqual({ok, #{ <<"path">> => <<"a">>, <<"value">> => 7 }},
        result_from_context(#{
            <<"request-id">> => <<"a">>, <<"normalizer">> => request,
            <<"varied-result">> => #{ <<"value">> => 7 }
        }, Opts)),
    {ok, ID} = hb_cache:write(Base, Opts),
    WrongBase = [#{ <<"base-id">> => ID, <<"base">> => #{ <<"a">> => 3 } }],
    ?assertNot(verify_all(WrongBase, Opts)),
    ?assertMatch({error, _}, load(WrongBase, Opts)),
    ?assertNot(verify_all([#{
        <<"base-id">> => ID,
        <<"request-id">> => <<"a">>,
        <<"request">> => #{ <<"path">> => <<"b">> }
    }], Opts)).

%% @doc Shared patches have caller-specific receipts and can reconstruct
%% the complete state from independently stored inputs and results.
overlay_receipt_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{
        <<"device">> => <<"test-device@1.0">>,
        <<"counter">> => 1,
        <<"other">> => <<"first">>
    },
    OtherBase = Base#{ <<"other">> => <<"second">> },
    Req = #{ <<"path">> => <<"vary-overlay">> },
    hb_cache:write(OtherBase, Opts),
    {ok, First} = hb_ao:resolve(Base, Req, Opts),
    {ok, Second} = hb_ao:resolve(OtherBase, Req,
        Opts#{ <<"cache-control">> => [<<"only-if-cached">>] }),
    HP1 = hb_path:hashpath(First, Opts),
    HP2 = hb_path:hashpath(Second, Opts),
    Ctx1 = context(HP1, Opts),
    Ctx2 = context(HP2, Opts),
    ?assertEqual(base, maps:get(<<"normalizer">>, Ctx1)),
    ?assertEqual(hb_message:id(Base, all, Opts), maps:get(<<"base-id">>, Ctx1)),
    ?assertNotEqual(HP1, HP2),
    ?assertEqual(maps:get(<<"varied-base-id">>, Ctx1),
        maps:get(<<"varied-base-id">>, Ctx2)),
    ?assertEqual(maps:get(<<"varied-result-id">>, Ctx1),
        maps:get(<<"varied-result-id">>, Ctx2)),
    ?assertEqual(2, maps:get(<<"counter">>, Second)),
    ?assertEqual(<<"second">>, maps:get(<<"other">>, Second)),
    {ok, Loaded} = load(HP2, Opts),
    ?assertEqual(hb_private:reset(Second),
        hb_private:reset(hb_cache:ensure_all_loaded(Loaded, Opts))),
    ?assertEqual(HP2, hb_path:hashpath(Loaded, Opts)),
    ?assert(verify_all(HP2, Opts)),
    ?assert(verify_part(HP2, 1, Opts)),
    {ok, Third} = hb_ao:resolve(Loaded, Req, Opts),
    HP3 = hb_path:hashpath(Third, Opts),
    ?assertEqual(HP2, maps:get(<<"base-id">>, context(HP3, Opts))),
    ?assert(verify_all(HP3, Opts)),
    ?assert(verify_part(HP3, 2, Opts)),
    {ok, Reloaded} = load(HP3, Opts),
    ?assertEqual(3, hb_maps:get(<<"counter">>, Reloaded, undefined, Opts)),
    ?assertEqual(<<"second">>, hb_maps:get(<<"other">>, Reloaded, undefined, Opts)),
    Linked = hb_cache:ensure_loaded({link, HP3, #{}}, Opts),
    ?assertEqual(3, hb_maps:get(<<"counter">>, Linked, undefined, Opts)),
    {ok, Serialized} = hb_cache:write(HP3, Opts),
    {ok, Decoded} = load({link, Serialized, #{}}, Opts),
    ?assertEqual(HP3, hb_path:hashpath(Decoded, Opts)),
    ?assertNot(verify_all(format(Ctx2#{ <<"normalizer">> => replace }, Opts), Opts)),
    ?assertNot(verify_all(format(Ctx2#{ <<"varied-result-id">> =>
        maps:get(<<"varied-base-id">>, Ctx2) }, Opts), Opts)),
    % Port just this transition's witnesses to an independent store.
    PortableOpts = #{ <<"store">> => hb_test_utils:test_store() },
    {ok, Portable} = hb_ao:resolve(OtherBase, Req,
        Opts#{ <<"return-context">> => true }),
    lists:foreach(
        fun(Name) -> hb_cache:write(maps:get(Name, Portable), PortableOpts) end,
        [<<"base">>, <<"request">>, <<"dependencies">>, <<"varied-result">>]
    ),
    {ok, Ported} = load(HP2, PortableOpts),
    ?assertEqual(2, hb_maps:get(<<"counter">>, Ported, undefined, PortableOpts)),
    ?assert(verify_all(HP2, PortableOpts)),
    {ok, Continued} = hb_ao:resolve(Ported, Req, PortableOpts),
    ?assertEqual(3, hb_maps:get(<<"counter">>, Continued, undefined, PortableOpts)).

%% @doc A replacement remains loadable when its predecessor is unavailable;
%% changing a base after resolution must not re-use its old receipt.
replacement_receipt_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>] },
    Base = #{ <<"child">> => #{ <<"value">> => <<"replacement">> } },
    Req = #{ <<"path">> => <<"child">> },
    {ok, Result} = hb_ao:resolve(Base, Req, Opts),
    HP = hb_path:hashpath(Result, Opts),
    hb_cache:write(Result, Opts),
    ?assertEqual({error, not_found},
        hb_cache:read(hb_message:id(Base, all, Opts), Opts)),
    {ok, Loaded} = load(HP, Opts),
    ?assertEqual(<<"replacement">>,
        hb_maps:get(<<"value">>, Loaded, undefined, Opts)),
    Changed = hb_ao:set(Loaded, <<"another">>, #{ <<"value">> => 2 }, Opts),
    {ok, Next} = hb_ao:resolve(Changed, <<"another">>, Opts),
    ?assertEqual(hb_message:id(Changed, all, Opts),
        maps:get(<<"base-id">>, context(hb_path:hashpath(Next, Opts), Opts))),
    ?assertNot(verify_part(HP, 0, Opts)),
    ?assertNot(verify_part(HP, 2, Opts)),
    ?assertMatch({error, _}, load(HP, 0, Opts)).

%% @doc Hashpath verification must not trust a store alias as proof of content.
wrong_content_address_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Claimed = hb_message:id(#{ <<"value">> => <<"claimed">> }, all, Opts),
    {ok, Actual} = hb_cache:write(#{ <<"value">> => <<"actual">> }, Opts),
    hb_cache:link(Actual, Claimed, Opts),
    ?assertNot(verify_all(<<"ao://", Claimed/binary>>, Opts)),
    ?assertMatch({error, _}, load(<<"ao://", Claimed/binary>>, Opts)),
    ?assertMatch({error, _}, load({link, Claimed, #{}}, Opts)).

%% @doc The dependency shape selects the original inputs before coercion.
coerced_dependencies_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{
        <<"device">> => <<"test-device@1.0">>,
        <<"required">> => <<"7">>,
        <<"deep">> => #{ <<"slot">> => <<"8">>, <<"omitted">> => 12 },
        <<"omitted">> => 11
    },
    Req = #{
        <<"path">> => <<"vary-projection">>,
        <<"deep-request">> => #{ <<"slot">> => <<"9">> },
        <<"omitted">> => 10
    },
    {ok, Result} = hb_ao:resolve(Base, Req, Opts),
    HP = hb_path:hashpath(Result, Opts),
    Ctx = context(HP, Opts),
    {ok, Deps} = load_field(<<"dependencies">>, Ctx, Opts),
    BaseID = hb_message:id(Base, all, Opts),
    ReqID = hb_message:id(Req, all, Opts),
    BaseHP = <<"ao://", BaseID/binary>>,
    ReqHP = <<"ao://", ReqID/binary>>,
    ExpectedDeps = #{
        <<"base">> => #{
            <<"device+link">> => BaseHP,
            <<"required+link">> => BaseHP,
            <<"deep">> => #{ <<"slot+link">> => BaseHP }
        },
        <<"request">> => #{
            <<"path+link">> => ReqHP,
            <<"deep-request">> => #{
                <<"slot+link">> => ReqHP
            }
        }
    },
    ?assertEqual(hb_message:id(ExpectedDeps, all, Opts),
        hb_message:id(Deps, all, Opts)),
    ?assertEqual(7, hb_ao:get(<<"base/required">>, Result, Opts)),
    ?assertEqual({ok, <<"7">>}, load(<<BaseHP/binary, "/required">>, Opts)),
    ?assert(verify_all(HP, Opts)),
    WrongDeps = ExpectedDeps#{ <<"base">> =>
        (maps:get(<<"base">>, ExpectedDeps))#{
            <<"required+link">> => <<BaseHP/binary, "/omitted">>
        }
    },
    {ok, WrongID} = hb_cache:write(WrongDeps, Opts),
    ?assertNot(verify_all(
        format(Ctx#{ <<"dependencies-id">> => WrongID }, Opts), Opts)),
    ?assertNot(verify_all(
        format(Ctx#{ <<"varied-base-id">> => BaseID }, Opts), Opts)),
    {ok, CollapsedID} = hb_cache:write(
        #{ <<"base">> => BaseHP, <<"request">> => ReqHP }, Opts),
    ?assertNot(verify_all(
        format(Ctx#{ <<"dependencies-id">> => CollapsedID }, Opts), Opts)).

%% @doc Signed inputs retain their identity through lazy loading. A different
%% signer is a different input, and a forged commitment is not an ID witness.
signed_dependencies_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>],
        <<"priv-wallet">> => ar_wallet:new()
    },
    Base = #{
        <<"device">> => <<"test-device@1.0">>,
        <<"child">> => #{ <<"value">> => <<"signed input">> }
    },
    Signed = hb_message:commit(Base, Opts),
    {ok, Result} = hb_ao:resolve(Signed, <<"vary-unspecified">>, Opts),
    HP = hb_path:hashpath(Result, Opts),
    ?assert(verify_all(HP, Opts)),
    {ok, Loaded} = load(HP, Opts),
    ?assertEqual(hb_message:signers(Signed, Opts),
        hb_message:signers(hb_maps:get(<<"base">>, Loaded, undefined, Opts), Opts)),
    OtherSigned = hb_message:commit(Base,
        Opts#{ <<"priv-wallet">> => ar_wallet:new() }),
    ?assertMatch({error, #{ <<"status">> := 504 }},
        hb_ao:resolve(OtherSigned, <<"vary-unspecified">>,
            Opts#{ <<"cache-control">> => [<<"only-if-cached">>] })),
    Forged = Signed#{ <<"child">> => #{ <<"value">> => <<"forged">> } },
    ID = hb_message:id(Signed, all, Opts),
    ?assertEqual(ID, hb_message:id(Forged, all, Opts)),
    ?assertNot(verify_all([#{ <<"base-id">> => ID, <<"base">> => Forged }], Opts)),
    ForgedOpts = Opts#{ <<"store">> => hb_test_utils:test_store() },
    hb_cache:write(Forged, ForgedOpts),
    ?assertNot(verify_all(<<"ao://", ID/binary>>, ForgedOpts)),
    ?assertMatch({error, _}, load(<<"ao://", ID/binary>>, ForgedOpts)),
    Child = hb_message:commit(#{ <<"value">> => <<"signed child">> }, Opts),
    ForgedChild = Child#{ <<"value">> => <<"forged child">> },
    lists:foreach(
        fun({OriginalValue, ForgedValue}) ->
            Parent = hb_message:commit(Base#{ <<"child">> => OriginalValue }, Opts),
            ForgedParent = Parent#{ <<"child">> => ForgedValue },
            ?assert(hb_message:verify(ForgedParent, all, Opts)),
            {ok, ForgedResult} =
                hb_ao:resolve(ForgedParent, <<"vary-unspecified">>, ForgedOpts),
            ?assertNot(verify_all(
                hb_path:hashpath(ForgedResult, ForgedOpts), ForgedOpts))
        end,
        [{Child, ForgedChild}, {[Child], [ForgedChild]}]
    ).

%% @doc Vary selects local members even when their names are device functions
%% or hashpath syntax. Signature data also remains an ordinary dependency.
selected_origins_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{
        <<"device">> => <<"test-device@1.0">>,
        <<"id">> => <<"local-id">>,
        <<"mangle">> => <<"local-mangle">>,
        <<"a+b">> => <<"literal-plus">>,
        <<"signature">> => hb_util:encode(crypto:strong_rand_bytes(32))
    },
    {ok, Result} = hb_ao:resolve(Base, <<"vary-unspecified">>, Opts),
    HP = hb_path:hashpath(Result, Opts),
    ?assert(verify_all(HP, Opts)).

%% @doc A request overlay is a replacement of the base, preserving only the
%% request's unvaried fields beside the patch. Its receipt must load that value.
request_overlay_receipt_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{ <<"device">> => <<"test-device@1.0">>, <<"base-only">> => true },
    Req = #{
        <<"path">> => <<"vary-request-overlay">>,
        <<"counter">> => <<"1">>,
        <<"request-only">> => true
    },
    {ok, Result} = hb_ao:resolve(Base, Req, Opts),
    ?assertEqual(Req#{ <<"counter">> => 2 }, hb_private:reset(Result)),
    HP = hb_path:hashpath(Result, Opts),
    Ctx = context(HP, Opts),
    ?assertEqual(replace, maps:get(<<"normalizer">>, Ctx, replace)),
    ?assertEqual(hb_message:id(Result, all, Opts),
        maps:get(<<"varied-result-id">>, Ctx)),
    {ok, Loaded} = load(HP, Opts),
    ?assertEqual(hb_private:reset(Result),
        hb_private:reset(hb_cache:ensure_all_loaded(Loaded, Opts))),
    ?assert(verify_all(HP, Opts)).

%% @doc The ancestry of a patch is its pre-result context. Loading it presents
%% the prior state; a compact key request still resolves to that key's value.
pre_result_context_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{ <<"device">> => <<"test-device@1.0">>, <<"counter">> => 1 },
    {ok, Result} = hb_ao:resolve(Base, <<"vary-overlay">>, Opts),
    Ctx = context(hb_path:hashpath(Result, Opts), Opts),
    BeforePatch = format(maps:without(
        [<<"varied-result-id">>, <<"normalizer">>], Ctx), Opts),
    {ok, Prior} = load(BeforePatch, Opts),
    ?assertEqual(1, hb_maps:get(<<"counter">>, Prior, undefined, Opts)),
    ?assertEqual(BeforePatch, hb_path:hashpath(Prior, Opts)),
    ?assertEqual({ok, 1}, load(<<BeforePatch/binary, "/counter">>, Opts)),
    ?assert(verify_all(BeforePatch, Opts)),
    {ok, Next} = hb_ao:resolve(Prior, <<"vary-overlay">>, Opts),
    ?assertEqual(2, hb_maps:get(<<"counter">>, Next, undefined, Opts)),
    ?assert(verify_all(hb_path:hashpath(Next, Opts), Opts)).

%% @doc Binary replacements are content-addressed values, independently of
%% the inputs that produced them. Native scalar keys remain executable paths.
literal_result_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Value = <<"literal replacement">>,
    hb_cache:write(Value, Opts),
    HP = format(#{
        <<"base-id">> => hb_message:id(#{}, all, Opts),
        <<"request">> => #{ <<"path">> => <<"value">> },
        <<"varied-result">> => Value
    }, Opts),
    ?assertEqual({ok, Value}, load(HP, Opts)),
    {ok, Serialized} = hb_cache:write(HP, Opts),
    ?assertEqual({ok, Value}, load({link, Serialized,
        #{ <<"type">> => <<"link">>, <<"lazy">> => true }}, Opts)),
    {ok, BaseID} = hb_cache:write(#{ <<"value">> => 7 }, Opts),
    ?assertEqual({ok, 7}, load(<<"ao://", BaseID/binary, "/value">>, Opts)),
    ?assertEqual({ok, 7}, hb_cache:read(<<"ao://", BaseID/binary, "/value">>, Opts)),
    Link = {link, <<"ao://", BaseID/binary, "/value">>,
        #{ <<"type">> => <<"link">>, <<"lazy">> => false }},
    ?assertEqual({ok, 7}, load(Link, Opts)),
    ?assertEqual(7, hb_cache:ensure_loaded(Link, Opts)),
    {ok, Ctx} = hb_ao:resolve(BaseID, <<"value">>,
        Opts#{ <<"return-context">> => true }),
    ?assertEqual(<<"ao://", BaseID/binary, "/value">>, format(Ctx, Opts)),
    ?assertEqual({ok, 7}, load(format(Ctx, Opts), Opts)).

%% @doc Ordinary store links and hashpath roots have distinct address syntax.
store_path_link_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Child = #{ <<"value">> => 7 },
    {ok, ID} = hb_cache:write(Child, Opts),
    Path = <<(hb_message:id(#{}, all, Opts))/binary, "/child.name">>,
    hb_cache:link(ID, Path, Opts),
    LinkOpts = #{ <<"type">> => <<"link">>, <<"lazy">> => false },
    Loaded = hb_cache:ensure_loaded({link, Path, LinkOpts}, Opts),
    ?assertEqual(hb_message:id(Child, all, Opts),
        hb_message:id(Loaded, all, Opts)),
    HP = <<"ao://", ID/binary>>,
    {ok, Root} = hb_cache:read(HP, Opts),
    ?assertEqual(ID, hb_message:id(Root, all, Opts)),
    {ok, StoredHP} = hb_cache:write(HP, Opts),
    ?assertEqual(
        #{ <<"value+link">> => HP },
        hb_link:normalize(
            #{ <<"value">> => {link, StoredHP, LinkOpts#{ <<"lazy">> => true }} },
            discard,
            Opts
        )
    ).

%% @doc A hashpath link obeys its store scope, including replacement witnesses.
scoped_link_test() ->
    Remote = (hb_test_utils:test_store())#{ <<"scope">> => remote },
    Opts = #{ <<"store">> => [hb_test_utils:test_store(), Remote] },
    Value = <<"remote replacement">>,
    hb_cache:write(Value, Opts#{ <<"store">> => [Remote] }),
    HP = format(#{
        <<"base-id">> => hb_message:id(#{}, all, Opts),
        <<"request">> => #{ <<"path">> => <<"value">> },
        <<"varied-result">> => Value
    }, Opts),
    LinkOpts = #{ <<"type">> => <<"link">>, <<"lazy">> => false },
    ?assertMatch({error, _}, load({link, HP, LinkOpts}, Opts)),
    ?assertThrow({necessary_message_not_found, _, _},
        hb_cache:ensure_loaded({link, HP, LinkOpts}, Opts)),
    RemoteLink = {link, HP, LinkOpts#{ <<"scope">> => remote }},
    ?assertEqual({ok, Value}, load(RemoteLink, Opts)),
    ?assertEqual(Value, hb_cache:ensure_loaded(RemoteLink, Opts)),
    ?assertNot(verify_origins(
        {link, HP, LinkOpts}, Value, Value, [<<"value">>], Opts)),
    ?assert(verify_origins(RemoteLink, Value, Value, [<<"value">>], Opts)).

%% @doc An HTTP receipt preserves the payload's signer and can be challenged,
%% loaded and continued independently of the transport's additional signature.
http_receipt_test() ->
    Wallet = ar_wallet:new(),
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>],
        <<"http-extra-opts">> => #{ <<"cache-control">> => [<<"always">>] },
        <<"priv-wallet">> => Wallet
    },
    Signed = hb_message:commit(#{
        <<"device">> => <<"test-device@1.0">>,
        <<"status">> => 200,
        <<"counter">> => 1,
        <<"value">> => <<"signed child">>
    }, Opts#{ <<"priv-wallet">> => ar_wallet:new() }),
    {ok, BaseID} = hb_cache:write(#{ <<"child">> => Signed }, Opts),
    Node = hb_http_server:start_node(Opts),
    try
        {ok, Reply} = hb_http:get(Node, <<BaseID/binary, "/child">>, Opts),
        ?assertEqual(hb_message:id(Signed, all, Opts),
            hb_message:id(Reply, hb_message:signers(Signed, Opts), Opts)),
        ?assert(hb_message:verify(Reply, all, Opts)),
        ?assert(hb_test_utils:has_committed_keys(Reply, [<<"hashpath">>])),
        HP = hb_maps:get(<<"hashpath">>, Reply, undefined, Opts),
        ?assert(?IS_HASHPATH(HP)),
        ?assert(verify_all(HP, Opts)),
        {ok, LinkedReply} = hb_http:post(Node, <<"/~message@1.0/value">>,
            #{ <<"value">> => {link, HP,
                #{ <<"type">> => <<"link">>, <<"lazy">> => false }} }, Opts),
        ?assertEqual(hb_message:id(Signed, all, Opts),
            hb_message:id(LinkedReply, hb_message:signers(Signed, Opts), Opts)),
        ?assert(hb_message:verify(LinkedReply, all, Opts)),
        ?assert(verify_all(
            hb_maps:get(<<"hashpath">>, LinkedReply, undefined, Opts), Opts)),
        {ok, Loaded} = load(HP, Opts),
        {ok, Next} = hb_ao:resolve(Loaded, <<"vary-overlay">>, Opts),
        ?assertEqual(2, hb_maps:get(<<"counter">>, Next, undefined, Opts)),
        ?assert(verify_all(hb_path:hashpath(Next, Opts), Opts))
    after
        cowboy:stop_listener(hb_util:human_id(ar_wallet:to_address(Wallet)))
    end.

%% @doc A subresolution returns its value to the outer execution, even when
%% that execution is collecting a context for challenge.
subresolution_context_test() ->
    Hook = fun(_Base, HookReq, _Opts) ->
        case maps:get(<<"request">>, HookReq) of
            #{ <<"path">> := <<"outer">> } ->
                {ok, HookReq#{ <<"body">> => {resolve, [
                    maps:get(<<"body">>, HookReq),
                    #{ <<"path">> => <<"child">> }
                ]} }};
            _ -> {ok, HookReq}
        end
    end,
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"on">> => #{ <<"step">> => #{ <<"device">> => #{ step => Hook } } }
    },
    Base = #{ <<"outer">> => #{ <<"child">> => #{ <<"counter">> => 7 } } },
    {ok, Result} = hb_ao:resolve(Base, <<"outer">>, Opts),
    {ok, Ctx} = hb_ao:resolve(Base, <<"outer">>,
        Opts#{ <<"return-context">> => true }),
    ?assertEqual(7, maps:get(<<"counter">>, maps:get(<<"result">>, Ctx))),
    ?assertEqual(hb_path:hashpath(Result, Opts), format(Ctx, Opts)),
    lists:foreach(
        fun(Name) -> hb_cache:write(maps:get(Name, Ctx), Opts) end,
        [<<"base">>, <<"request">>, <<"dependencies">>, <<"varied-result">>]
    ),
    ?assert(verify_all(format(Ctx, Opts), Opts)).

%% @doc Waiting callers receive the shared patch, then apply their own overlay
%% and receipt. The step hook holds real execution until the waiter registers.
awaited_overlay_receipt_test_() ->
    {timeout, 30, fun awaited_overlay_receipt/0}.

awaited_overlay_receipt() ->
    Parent = self(),
    Ref = make_ref(),
    Hook = fun(_Base, HookReq, _Opts) ->
        case maps:get(<<"request">>, HookReq) of
            #{ <<"path">> := <<"vary-overlay">> } ->
                Parent ! {Ref, ready, self()},
                receive
                    Await = {resolve, Waiter, _, _, _} ->
                        self() ! Await,
                        Parent ! {Ref, waiting, Waiter},
                        receive {Ref, continue} -> {ok, HookReq} end
                end;
            _ -> {ok, HookReq}
        end
    end,
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"await-inprogress">> => true,
        <<"on">> => #{ <<"step">> => #{ <<"device">> => #{ step => Hook } } }
    },
    Base = #{
        <<"device">> => <<"test-device@1.0">>,
        <<"counter">> => 1,
        <<"other">> => <<"first">>
    },
    Resolve = fun(Name, Input) ->
        Parent ! {Ref, Name, hb_ao:resolve(Input, <<"vary-overlay">>, Opts)}
    end,
    First = spawn_link(fun() -> Resolve(first, Base) end),
    try
        receive {Ref, ready, First} -> ok after 5000 -> error('leader-timeout') end,
        Second = spawn_link(fun() ->
            Resolve(second, Base#{ <<"other">> => <<"second">> })
        end),
        try
            receive
                {Ref, waiting, Second} -> First ! {Ref, continue}
            after 5000 -> error('waiter-timeout')
            end,
            A = receive {Ref, first, {ok, ResA}} -> ResA
                after 5000 -> error('result-timeout') end,
            B = receive {Ref, second, {ok, ResB}} -> ResB
                after 5000 -> error('result-timeout') end,
            ?assertEqual(<<"second">>, maps:get(<<"other">>, B)),
            ?assertEqual(2, maps:get(<<"counter">>, B)),
            CtxA = context(hb_path:hashpath(A, Opts), Opts),
            CtxB = context(hb_path:hashpath(B, Opts), Opts),
            ?assertNotEqual(maps:get(<<"base-id">>, CtxA),
                maps:get(<<"base-id">>, CtxB)),
            ?assertEqual(maps:get(<<"varied-result-id">>, CtxA),
                maps:get(<<"varied-result-id">>, CtxB))
        after
            unlink(Second),
            exit(Second, kill)
        end
    after
        unlink(First),
        exit(First, kill)
    end.
