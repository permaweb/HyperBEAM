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
%%% A compact form may omit fields before a terminal result is asserted. Once a
%%% result is asserted, the frame must carry the Vary pair and Dependencies
%%% needed for local challenge. Segments without explicit vary syntax are weak
%%% addresses, not complete result assertions.
%%%
%%% Every separator of the syntax (`/', `>', `+', `@', `=', `.') is outside
%%% the base64url alphabet, so the grammar is unambiguous without escaping.
%%% The request position holds an ID when the request is addressed, or a
%%% literal key when it is self-describing (e.g. `*').
-module(hb_hashpath).
%%% Create, parse, and load hashpaths.
-export([format/2, parse/2, context/2, load/2, load/3]).
%%% Reconstruct a result from a resolved or parsed execution context.
-export([result_from_context/2, result_from_context/3]).
-export([with_context_hashpath/3]).
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
format([Ctx = #{ <<"base-id">> := Base }], _Opts)
        when not is_map_key(<<"request-id">>, Ctx),
             not is_map_key(<<"request">>, Ctx) ->
    Base;
format([First | Rest], Opts) ->
    iolist_to_binary(
        [
            format_context(First, Opts),
            [
                <<"/", (format_segment(Part, Opts))/binary>>
            ||
                Part <- Rest
            ]
        ]
    );
format(Ctx, Opts) when is_map(Ctx) ->
    format_context(Ctx, Opts).

format_weak(Ctx, Opts) ->
    format(Ctx, Opts#{ <<"allow-weak-hashpath">> => true }).

format_context(Ctx, Opts) ->
    maybe
        {ok, BasePart} ?= format_base(Ctx, Opts),
        <<BasePart/binary, "/", (format_segment(Ctx, Opts))/binary>>
    else
        {not_found, Name} ->
            throw({context_not_viable, unavailable_field, Name})
    end.

format_segment(Ctx, Opts) ->
    maybe
        {ok, RequestPart} ?= format_request_part(Ctx, Opts),
        ok = verify_format_terminal_witness(Ctx, Opts),
        <<
            RequestPart/binary,
            (format_varied(Ctx, Opts))/binary,
            (format_dependencies(Ctx, Opts))/binary,
            (format_equivalence(Ctx, Opts))/binary
        >>
    else
        {not_found, Name} ->
            throw({context_not_viable, unavailable_field, Name})
    end.

verify_format_terminal_witness(Ctx, Opts) ->
    case allow_weak_hashpath(Opts) orelse not terminal_result_claim(Ctx, Opts) of
        true -> ok;
        false -> require_terminal_fields(
            [
                <<"request">>,
                <<"varied-base">>,
                <<"varied-request">>,
                <<"dependencies">>,
                <<"normalizer">>
            ],
            Ctx
        )
    end.

require_terminal_fields([], _Ctx) ->
    ok;
require_terminal_fields([<<"normalizer">> | Rest], Ctx) ->
    case maps:is_key(<<"normalizer">>, Ctx) of
        true -> require_terminal_fields(Rest, Ctx);
        false -> throw({context_not_viable, unavailable_field, <<"normalizer">>})
    end;
require_terminal_fields([Name | Rest], Ctx) ->
    case has_claim_field(Name, Ctx) of
        true -> require_terminal_fields(Rest, Ctx);
        false -> throw({context_not_viable, unavailable_field, Name})
    end.

%% @doc Utilize the `hashpath` of the prior resolution, if it is available,
%% falling back to the `BaseID` if known, and recomputing it only if necessary.
format_base(Ctx = #{ <<"base">> := Base }, Opts) ->
    case hb_private:from_message(Base) of
        #{ <<"hashpath">> := PriorHashpath } ->
            case hashpath_matches_base(PriorHashpath, Base, Opts) of
                true -> {ok, PriorHashpath};
                false -> find_id(<<"base">>, Ctx, Opts)
            end;
        _ -> find_id(<<"base">>, Ctx, Opts)
    end;
format_base(Ctx, Opts) ->
    find_id(<<"base">>, Ctx, Opts).

hashpath_matches_base(Hashpath, Base, Opts) ->
    case load(Hashpath, Opts) of
        {ok, Loaded} -> same_active_message(Base, Loaded, Opts);
        _ -> false
    end.

same_active_message(Left, Right, Opts) when is_map(Left), is_map(Right) ->
    CompareOpts =
        Opts#{
            <<"hashpath">> => ignore,
            <<"spawn-worker">> => false,
            <<"caching-schema">> => true
        },
    case {deep_public_keys(Left, CompareOpts), deep_public_keys(Right, CompareOpts)} of
        {{ok, Keys}, {ok, Keys}} ->
            case
                all_true(
                    fun(Key) ->
                        same_resolved_key(Key, Left, Right, CompareOpts)
                    end,
                    Keys
                )
            of
                true -> true;
                _ -> false
            end;
        _ ->
            same_result_value(Left, Right, Opts)
    end;
same_active_message(Left, Right, Opts) ->
    same_result_value(Left, Right, Opts).

deep_public_keys(Msg, Opts) ->
    case hb_ao:resolve(Msg, #{ <<"path">> => <<"keys">>, <<"keys">> => <<"deep">> }, Opts) of
        {ok, Keys} -> {ok, lists:sort(Keys)};
        Error -> Error
    end.

same_resolved_key(Key, Left, Right, Opts) ->
    case {hb_ao:resolve(Left, Key, Opts), hb_ao:resolve(Right, Key, Opts)} of
        {{ok, LeftValue}, {ok, RightValue}} ->
            same_public_value(LeftValue, RightValue, Opts)
                orelse {error, <<"Private hashpath does not match base.">>};
        _ ->
            {error, <<"Private hashpath does not match base.">>}
    end.

format_request_part(Ctx, Opts) ->
    find_id(<<"request">>, Ctx, Opts).

%% @doc General utility for extracting the ID of a message by its name from a
%% context if it is already known, recomputing only if necessary.
find_id(Name, Ctx, Opts) when is_map_key(<<Name/binary, "-id">>, Ctx) ->
    ID = maps:get(<<Name/binary, "-id">>, Ctx),
    ok = verify_format_component_id(Name, ID),
    ok = verify_format_value_id(Name, ID, Ctx, Opts),
    {ok, ID};
find_id(Name, Ctx, Opts) when is_map_key(Name, Ctx) ->
    case hb_opts:get(<<"hashpath">>, enabled, Opts) of
        enabled ->
            {ok, value_id(Name, maps:get(Name, Ctx), Opts)};
        _ ->
            {not_found, Name}
    end;
find_id(Name, _Ctx, _Opts) ->
    {not_found, Name}.

verify_format_value_id(Name, ID, Ctx, Opts) ->
    case maps:find(Name, Ctx) of
        error ->
            ok;
        {ok, Value} ->
            case direct_reference_id(Name, Value) of
                {ok, ID} ->
                    ok;
                _ ->
                    case hb_private:from_message(Value) of
                        #{ <<"hashpath">> := ID } ->
                            verify_private_hashpath_id(Name, ID, Value, Opts);
                        _ when ?IS_ID(ID) ->
                            case value_id(Name, Value, Opts) of
                                ID -> ok;
                                _ -> throw({id_mismatch, Name})
                            end;
                        _ ->
                            ok
                    end
            end
    end.

verify_private_hashpath_id(Name, ID, Value, Opts) ->
    case private_hashpath_matches_value(ID, Value, Opts) orelse id_matches_value(Name, ID, Value, Opts) of
        true -> ok;
        false -> throw({id_mismatch, Name})
    end.

private_hashpath_matches_value(ID, Value, Opts) ->
    case read_context_reference(ID, Opts) of
        {ok, Loaded} -> same_result_value(Value, Loaded, Opts);
        _ -> false
    end.

id_matches_value(Name, ID, Value, Opts) when ?IS_ID(ID) ->
    value_id(Name, Value, Opts) =:= ID;
id_matches_value(_Name, _ID, _Value, _Opts) ->
    false.

verify_format_component_id(<<"base">>, ID) when is_binary(ID) ->
    case is_hashpath_component_safe(ID) orelse is_hashpath_reference(ID) of
        true -> ok;
        false -> throw({invalid_hashpath_component_id, <<"base">>})
    end;
verify_format_component_id(<<"base">>, _ID) ->
    ok;
verify_format_component_id(Name, ID) when is_binary(ID) ->
    case is_hashpath_component_safe(ID) of
        true -> ok;
        false -> throw({invalid_hashpath_component_id, Name})
    end;
verify_format_component_id(_Name, _ID) ->
    ok.

is_hashpath_component_safe(Component) when is_binary(Component) ->
    binary:match(Component, [<<"/">>, <<">">>, <<"+">>, <<"@">>, <<"=">>, <<".">>]) =:= nomatch;
is_hashpath_component_safe(_Component) ->
    true.

reject_unsafe_direct_component(Name, Value) ->
    case is_hashpath_component_safe(Value) of
        true -> ok;
        false -> throw({invalid_direct_hashpath_value, Name})
    end.

%% @doc Format the varied base and requests, if given, into their hashpath
%% components.
format_varied(Ctx, Opts) ->
    case {find_id(<<"varied-base">>, Ctx, Opts), find_id(<<"varied-request">>, Ctx, Opts)} of
        {{ok, VBase}, {ok, VReq}} ->
            <<">", VBase/binary, "+", VReq/binary>>;
        {{not_found, _}, {not_found, _}} ->
            <<>>;
        {{not_found, Name}, _} ->
            throw({context_not_viable, unavailable_field, Name});
        {_, {not_found, Name}} ->
            throw({context_not_viable, unavailable_field, Name})
    end.

%% @doc If the dependencies of a resolution are known, format them into the
%% hashpath depends component. If not, return an empty string. Honors already
%% calculated dependency IDs if provided in the context.
format_dependencies(Ctx, Opts) ->
    case find_id(<<"dependencies">>, Ctx, Opts) of
        {ok, Depends} -> <<"@", Depends/binary>>;
        {not_found, Name} ->
            case allow_weak_hashpath(Opts) orelse not terminal_result_claim(Ctx, Opts) of
                true -> <<>>;
                false -> throw({context_not_viable, unavailable_field, Name})
            end
    end.

terminal_result_claim(Ctx, Opts) ->
    case find_result_id(Ctx, Opts) of
        {ok, _} -> true;
        {not_found, _} -> false;
        {error, Normalizer} -> throw({unsupported_normalizer, Normalizer})
    end.

allow_weak_hashpath(Opts) ->
    hb_opts:get(<<"allow-weak-hashpath">>, false, Opts).

%% @doc If the result of the execution has already been calculated, format it
%% into the hashpath equivalence component. If not, return an empty string.
format_equivalence(Ctx, Opts) ->
    case find_result_id(Ctx, Opts) of
        {ok, Result} -> <<(format_normalizer(Ctx, Opts))/binary, Result/binary>>;
        {not_found, _} -> <<>>;
        {error, Normalizer} -> throw({unsupported_normalizer, Normalizer})
    end.

%% @doc Format the normalizer component of the hashpath.
format_normalizer(Ctx, _Opts) ->
    Normalizer = maps:get(<<"normalizer">>, Ctx, replace),
    case normalizer_mode(Normalizer) of
        {extension, _} -> <<"=">>;
        replacement -> <<".">>;
        {error, _} -> throw({unsupported_normalizer, Normalizer})
    end.

%% @doc Decode a hashpath into a list of context segments. The first segment will
%% have both a base and a request part, while the latter segments will only have
%% the request part -- the base being inferred from the result of the prior
%% segments.
parse(Hashpath, Opts) when is_binary(Hashpath) ->
    Parts = binary:split(Hashpath, <<"/">>, [global]),
    case valid_path_parts(Parts) of
        false ->
            {error, {invalid_hashpath_component, empty}};
        true ->
            case Parts of
                [Base] ->
                    [#{ <<"base-id">> => Base }];
                [Base, Req1 | Reqs] ->
                    collect_parsed_parts(
                        [
                            parse_part(Base, Req1, Opts)
                        |
                            lists:map(
                                fun(ReqPart) -> parse_part(undefined, ReqPart, Opts) end,
                                Reqs
                            )
                        ]
                    )
            end
    end.

valid_path_parts(Parts) ->
    Parts =/= [] andalso lists:all(fun(Part) -> Part =/= <<>> end, Parts).

collect_parsed_parts(Parts) ->
    case lists:dropwhile(fun is_map/1, Parts) of
        [] -> Parts;
        [Error | _] -> Error
    end.

%% @doc Parse the last segment of a hashpath into an executable context that 
%% can be additionally executed upon.
context(Hashpath, Opts) ->
    Parts = binary:split(Hashpath, <<"/">>, [global]),
    case valid_path_parts(Parts) of
        false ->
            {error, {invalid_hashpath_component, empty}};
        true ->
            case Parts of
                [LoneBase] ->
                    #{ <<"base-id">> => LoneBase };
                _ ->
                    [LastReq | RevBaseParts] = lists:reverse(Parts),
                    ReconstitutedBase = binary:join(lists:reverse(RevBaseParts), <<"/">>),
                    parse_part(ReconstitutedBase, LastReq, Opts)
            end
    end.

%% @doc Calculate the context for a hashpath segment. If the base is known
%% explicitly, add it to the result from parsing the request part. If not,
%% parse the request part and return as-is.
parse_part(undefined, ReqPart, Opts) ->
    parse_request(ReqPart, Opts);
parse_part(Base, ReqPart, Opts) ->
    case parse_request(ReqPart, Opts) of
        Ctx when is_map(Ctx) -> Ctx#{ <<"base-id">> => Base };
        Error -> Error
    end.

%% @doc Parse a single segment of the hashpath into a context segment.
parse_request(Part, Opts) ->
    maybe
        {next, NextDelim, Ctx1, Part2} ?=
            parse_request_id(Part, Opts),
        {next, NextDelim2, Ctx2, Part3} ?=
            parse_varied(NextDelim, Part2, Ctx1, Opts),
        {next, NextDelim3, Ctx3, Part4} ?=
            parse_dependencies(NextDelim2, Part3, Ctx2, Opts),
        {ok, Ctx4} ?=
            parse_equivalence(NextDelim3, Part4, Ctx3, Opts),
        Ctx4
    end.

%% @doc Parse the request ID part of a hashpath segment. If the request ID is
%% the only part, it is returned as-is and the remainder of the part parsing is
%% skipped.
parse_request_id(Part, _Opts) ->
    case next(Part) of
        {_Sep, <<>>, _Part2} ->
            {error, {invalid_hashpath_component, request}};
        {no_match, ReqID, <<>>} ->
            {next, no_match, #{ <<"request-id">> => ReqID }, <<>>};
        {Sep, ReqID, Part2} ->
            {next, Sep, #{ <<"request-id">> => ReqID }, Part2}
    end.

%% @doc If the delimiter that starts our segment is `>` we handle the inner
%% segment as a `VariedBase` and `VariedRequest` pair and get the next delimited
%% component. If the delimiter is not `>`, we pass the segment forward as-is.
parse_varied($>, Part, Ctx0, _Opts) ->
    {NextDelim, Next, After} = next([$@, $., $=], Part),
    case binary:split(Next, <<"+">>) of
        [VBase, VReq] when VBase =/= <<>>, VReq =/= <<>> ->
            {
                next,
                NextDelim,
                Ctx0#{ <<"varied-base-id">> => VBase, <<"varied-request-id">> => VReq },
                After
            };
        Malformed ->
            {error, {invalid_variance_parts, Malformed}}
    end;
parse_varied(NextDelim, Part, Ctx0, _Opts) ->
    {next, NextDelim, Ctx0, Part}.

%% @doc Parse the dependencies if present. We short-curcuit the parser and 
%% return the context early if we have already hit the end of the string.
parse_dependencies(no_match, <<>>, Ctx, _Opts) ->
    {next, no_match, Ctx, <<>>};
parse_dependencies($@, Part, Ctx0, _Opts) ->
    {NextDelim, DepID, After} = next([$., $=], Part),
    case DepID of
        <<>> -> {error, {invalid_hashpath_component, dependencies}};
        _ -> {next, NextDelim, Ctx0#{ <<"dependencies-id">> => DepID }, After}
    end;
parse_dependencies(Delim, Part, Ctx0, _Opts) ->
    {next, Delim, Ctx0, Part}.

%% @doc Parse the equivalent relationship if stated in the hashpath.
parse_equivalence(no_match, <<>>, Ctx, _Opts) -> {ok, Ctx};
parse_equivalence(_Delim, <<>>, _Ctx, _Opts) ->
    {error, {invalid_hashpath_component, result}};
parse_equivalence($=, ResultID, Ctx, _Opts) ->
    {ok, Ctx#{ <<"normalizer">> => base, <<"varied-result-id">> => ResultID }};
parse_equivalence($., ResultID, Ctx, _Opts) ->
    {ok, Ctx#{ <<"normalizer">> => replace, <<"varied-result-id">> => ResultID }};
parse_equivalence(Delim, Part, _Ctx, _Opts) ->
    {error, {invalid_hashpath_component, Delim, Part}}.

%% @doc Utility to split at the next syntax delimiter (e.g. `=`, `.`, `>`, `@`).
%% Returns the syntax element matched, and the rest of the string. Notably, this
%% utility does not break apart `VBase+VReq` pairs. They are treated as a single
%% unit and parsed internally in `parse_varied/4`.
next(S) -> next([$=, $., $>, $@], S).
next(Symbols, S) -> hb_util:split_depth_string_aware_single(Symbols, S).

%% @doc Challenge a complete hashpath, verifying each part's claims.
verify_all(Bin, Opts) when is_binary(Bin) ->
    verify_all(parse(Bin, Opts), Opts);
verify_all({error, _Reason}, _Opts) ->
    false;
verify_all([], _Opts) ->
    % We treat an empty hashpath as failing verification.
    false;
verify_all(Parts, Opts) ->
    verify_all(undefined, Parts, Opts).

verify_all(_State, [], _Opts) ->
    true;
verify_all(State, [Part | Rest], Opts) ->
    % Add the currently computed state to the part's context and verify it.
    PartWithBase =
        case State of
            undefined -> Part;
            _ -> Part#{ <<"base">> => State }
        end,
    case verify_context(PartWithBase, Opts) of
        {true, ComputedState} -> verify_all(ComputedState, Rest, Opts);
        false -> false
    end.

%% @doc Verify a single hashpath execution contained inside a larger hashpath
%% sequence.
verify_part(Hashpath, PartNum, Opts) when is_binary(Hashpath) ->
    verify_part(parse(Hashpath, Opts), PartNum, Opts);
verify_part({error, _Reason}, _PartNum, _Opts) ->
    false;
verify_part(Parts, PartNum, Opts) when is_list(Parts), PartNum > 0 ->
    maybe
        {Prior, [Part | _]} ?= split_part(Parts, PartNum),
        {ok, PartWithBase} ?= part_with_base(Prior, Part, Opts),
        verify_context(PartWithBase, Opts)
    end.

split_part(Parts, PartNum) when length(Parts) >= PartNum ->
    lists:split(PartNum - 1, Parts);
split_part(_Parts, _PartNum) ->
    {error, part_not_found}.

part_with_base([], Part, Opts) ->
    hb_ao:with([<<"base">>], Part, Opts);
part_with_base(Prior, Part, Opts) ->
    case load(Prior, Opts) of
        {ok, Base} -> {ok, Part#{ <<"base">> => Base }};
        Error -> Error
    end.

%% @doc Verify a full single context, parsed from a binary hashpath. The context
%% must contain a `Base' representation. We remove all of the non-`Base` and
%% `Request` fields, then utilize `hb_ao:do` to re-execute the context. Assuming
%% successful computation, we then verify the `VariedBase` and `VariedRequest`
%% fields against the parsed context, the `DependenciesID` if given, the 
%% `Normalizer` type, and finally the `Result` message itself. If all of these
%% verify, the context is considered valid.
verify_context(Ctx, Opts) ->
    ChallengeOpts = challenge_opts(Opts),
    StrippedCtx =
        maps:with(
            [<<"base">>, <<"request">>, <<"base-id">>, <<"request-id">>, <<"opts">>],
            Ctx#{ <<"opts">> => ChallengeOpts }
        ),
    maybe
        true ?= verify_context_ids(Ctx, ChallengeOpts),
	        true ?= verify_assertion_completeness(Ctx),
	        {ok, ExecutedCtx} ?= hb_ao:do(StrippedCtx),
	        true ?= verify_varied(Ctx, ExecutedCtx, ChallengeOpts),
	        true ?= verify_claim_level(Ctx, ExecutedCtx),
	        true ?= verify_dependencies(Ctx, ExecutedCtx, ChallengeOpts),
	        true ?= verify_equivalence(Ctx, ExecutedCtx, ChallengeOpts),
        {ok, ComputedState} ?= result_from_context(ExecutedCtx, ChallengeOpts),
        {true, ComputedState}
    else
        {error, _Type} ->
            ?event_debug(
                hashpath_debug,
                {hashpath_verify_context_failed, {type, _Type}, {ctx, Ctx}},
                Opts
            ),
            false;
        _Other ->
            ?event_debug(
                hashpath_debug,
                {hashpath_verify_context_failed, {type, _Other}, {ctx, Ctx}},
                Opts
            ),
            false
    end.

challenge_opts(Opts) ->
    Opts#{
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"await-inprogress">> => false,
        <<"spawn-worker">> => false
    }.

verify_context_ids(Ctx, Opts) ->
    maybe
        true ?= verify_value_id(<<"base">>, Ctx, Opts),
        true ?= verify_value_id(<<"request">>, Ctx, Opts),
        true ?= verify_value_id(<<"varied-base">>, Ctx, Opts),
        true ?= verify_value_id(<<"varied-request">>, Ctx, Opts),
        true ?= verify_value_id(<<"dependencies">>, Ctx, Opts),
        true ?= verify_dependency_shape(Ctx, Opts),
        true ?= verify_value_id(<<"varied-result">>, Ctx, Opts),
        true ?= verify_value_id(<<"result">>, Ctx, Opts),
        true
    end.

verify_value_id(Name, Ctx, Opts)
        when is_map_key(Name, Ctx),
             is_map_key(<<Name/binary, "-id">>, Ctx) ->
    ClaimedID = maps:get(<<Name/binary, "-id">>, Ctx),
    ActualID = value_id(Name, maps:get(Name, Ctx), Opts),
    ActualID =:= ClaimedID
        orelse {error, <<"ID mismatch for `", Name/binary, "`.">>};
verify_value_id(_Name, _Ctx, _Opts) ->
    true.

verify_assertion_completeness(Ctx) ->
    case has_result_claim(Ctx) of
        false ->
            true;
        true ->
            case result_assertion_complete(Ctx) of
                true -> true;
                {error, _} = Error -> Error;
                false -> {error, <<"Result assertions must be executable.">>}
            end
    end.

result_assertion_complete(Ctx) ->
    case maps:is_key(<<"normalizer">>, Ctx) of
        false ->
            {error, <<"Result assertions must state a result mode.">>};
        true ->
            has_claim_field(<<"base">>, Ctx)
                andalso has_claim_field(<<"request">>, Ctx)
                andalso has_claim_field(<<"varied-base">>, Ctx)
                andalso has_claim_field(<<"varied-request">>, Ctx)
                andalso has_claim_field(<<"dependencies">>, Ctx)
                andalso valid_claim_level(claim_level(Ctx))
                andalso result_claim_fields_consistent(Ctx)
    end.

result_claim_fields_consistent(Ctx) ->
    case {has_claim_field(<<"varied-base">>, Ctx), has_claim_field(<<"varied-request">>, Ctx)} of
        {true, false} -> false;
        {false, true} -> false;
        _ -> true
    end.

has_result_claim(Ctx) ->
    lists:any(
        fun(Key) -> maps:is_key(Key, Ctx) end,
        [<<"varied-result">>, <<"varied-result-id">>, <<"result">>, <<"result-id">>]
    ).

has_claim_field(Name, Ctx) ->
    maps:is_key(Name, Ctx) orelse maps:is_key(<<Name/binary, "-id">>, Ctx).

verify_claim_level(HPCtx, ExecutedCtx) ->
    case {normalize_claim_level(claim_level(HPCtx)), normalize_claim_level(claim_level(ExecutedCtx))} of
        {unknown, _} ->
            {error, <<"Invalid hashpath claim level.">>};
        {_, unknown} ->
            {error, <<"Invalid executed claim level.">>};
        {Level, Level} ->
            true;
        _ ->
            {error, <<"Hashpath claim level does not match execution.">>}
    end.

claim_level(Ctx) ->
    maps:get(<<"claim-level">>, Ctx, <<"schema-declared">>).

valid_claim_level(Level) ->
    normalize_claim_level(Level) =/= unknown.

normalize_claim_level(<<"schema-declared">>) -> schema_declared;
normalize_claim_level(schema_declared) -> schema_declared;
normalize_claim_level(<<"observed-exact">>) -> observed_exact;
normalize_claim_level(observed_exact) -> observed_exact;
normalize_claim_level(_Level) -> unknown.

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
    case dependency_claimed(HPCtx) of
        false ->
            true;
        true ->
            case dependency_claimed(ExecutedCtx) of
                false ->
                    {error, <<"Dependencies claim not reproduced">>};
                true ->
                    maybe
                        {ok, HPDeps} ?= dependency_value(HPCtx, Opts),
                        true ?= verify_dependency_shape(
                            #{ <<"dependencies">> => HPDeps },
                            Opts
                        ),
                        true ?= verify_dependency_coverage(HPCtx, HPDeps, Opts),
                        true ?= verify_executed_dependencies(HPCtx, HPDeps, ExecutedCtx, Opts),
                        true
                    end
            end
    end.

dependency_claimed(Ctx) ->
    maps:is_key(<<"dependencies">>, Ctx)
        orelse maps:is_key(<<"dependencies-id">>, Ctx).

dependency_value(Ctx, Opts) ->
    case context_value(<<"dependencies">>, Ctx, Opts) of
        {ok, Deps} -> {ok, hb_cache:ensure_all_loaded(Deps, Opts)};
        Other -> Other
    end.

verify_executed_dependencies(_HPCtx, HPDeps, ExecutedCtx, Opts) ->
    maybe
        {ok, ExecDeps} ?= dependency_value(ExecutedCtx, Opts),
        true ?= verify_dependency_shape(
            #{ <<"dependencies">> => ExecDeps },
            Opts
        ),
        true ?= same_dependencies(HPDeps, ExecDeps, Opts)
            orelse {error, <<"Dependencies do not match">>},
        true
    end.

same_dependencies(Left, Right, Opts) ->
    canonical_dependencies(hb_private:reset(hb_cache:ensure_all_loaded(Left, Opts)))
        =:= canonical_dependencies(hb_private:reset(hb_cache:ensure_all_loaded(Right, Opts))).

canonical_dependencies(Deps) when is_map(Deps) ->
    maps:map(fun(_Key, Value) -> canonical_dependency_node(Value) end, Deps).

	canonical_dependency_node(Node) when is_map(Node) ->
	    case normalize_observation_leaf(Node) of
	        {ok, #{ <<"status">> := found, <<"origin">> := Origin } = Leaf}
	                when map_size(Leaf) =:= 2 ->
	            Origin;
	        {ok, Leaf} ->
	            Leaf;
        false ->
            maps:map(fun(_Key, Value) -> canonical_dependency_node(Value) end, Node)
    end;
canonical_dependency_node(Node) ->
    Node.

verify_dependency_shape(#{ <<"dependencies">> := Deps }, _Opts) ->
    case valid_dependencies(Deps) of
        true -> true;
        false -> {error, <<"Invalid Dependencies shape.">>}
    end;
verify_dependency_shape(_Ctx, _Opts) ->
    true.

valid_dependencies(#{ <<"base">> := Base, <<"request">> := Request } = Deps)
        when map_size(Deps) =:= 2, is_map(Base), is_map(Request) ->
    valid_dependency_node(Base) andalso valid_dependency_node(Request);
valid_dependencies(_Deps) ->
    false.

valid_dependency_node(Node) when is_binary(Node) ->
    valid_origin(Node);
valid_dependency_node(Node) when is_map(Node) ->
    valid_observation_leaf(Node)
        orelse lists:all(fun valid_dependency_node/1, maps:values(Node));
valid_dependency_node(_Node) ->
    false.

valid_origin(Origin) when is_binary(Origin), Origin =/= <<>> ->
    case is_hashpath_reference(Origin) of
        true ->
            is_list(parse(Origin, #{}));
        false ->
            is_hashpath_component_safe(Origin)
    end;
valid_origin(_Origin) ->
    false.

valid_default_id(Default) when ?IS_ID(Default) ->
    is_hashpath_component_safe(Default);
valid_default_id(_Default) ->
    false.

valid_observation_leaf(Node) ->
    case normalize_observation_leaf(Node) of
        {ok, _Leaf} -> true;
        false -> false
    end.

normalize_observation_leaf(Node) when is_map(Node) ->
    case {maps:find(<<"status">>, Node), maps:find(<<"origin">>, Node)} of
        {{ok, Status0}, {ok, Origin}} ->
            case {normalize_observation_status(Status0), valid_origin(Origin)} of
                {found, true} ->
                    case {maps:is_key(<<"observed">>, Node), maps:is_key(<<"value">>, Node)} of
                        {false, false} ->
                            observation_leaf_with_keys(Node, [<<"status">>, <<"origin">>]);
                        {true, true} ->
                            observation_leaf_with_keys(
                                Node,
                                [<<"status">>, <<"origin">>, <<"observed">>, <<"value">>]
                            );
                        _ ->
                            false
                    end;
                {Status, true}
                        when Status =:= not_found;
                             Status =:= unset ->
                    observation_leaf_with_keys(
                        Node,
                        [<<"status">>, <<"origin">>, <<"path">>]
                    );
                {defaulted, true} ->
                    case valid_default_id(maps:get(<<"default">>, Node, undefined)) of
                        true ->
                            observation_leaf_with_keys(
                                Node,
                                [<<"status">>, <<"origin">>, <<"path">>, <<"default">>]
                            );
                        false ->
                            false
                    end;
                {error, true} ->
                    observation_leaf_with_keys(
                        Node,
                        [<<"status">>, <<"origin">>, <<"path">>, <<"error">>]
                    );
                _ ->
                    false
            end;
        _ ->
            false
    end;
normalize_observation_leaf(_Node) ->
    false.

observation_leaf_with_keys(Node, Keys) ->
    case lists:sort(maps:keys(Node)) =:= lists:sort(Keys) of
        true ->
            case validate_observation_path(Node) of
                true ->
                    {ok,
                        Node#{
                            <<"status">> =>
                                normalize_observation_status(maps:get(<<"status">>, Node))
                        }};
                false ->
                    false
            end;
        false ->
            false
    end.

validate_observation_path(#{ <<"status">> := Status, <<"path">> := Path }) ->
    normalize_observation_status(Status) =/= found
        andalso observation_path_parts(Path, #{}) =/= invalid;
validate_observation_path(#{ <<"status">> := Status }) ->
    normalize_observation_status(Status) =:= found;
validate_observation_path(#{ <<"path">> := Path }) ->
    observation_path_parts(Path, #{}) =/= invalid;
validate_observation_path(_Leaf) ->
    false.

normalize_observation_status(found) -> found;
normalize_observation_status(<<"found">>) -> found;
normalize_observation_status(not_found) -> not_found;
normalize_observation_status(<<"not_found">>) -> not_found;
normalize_observation_status(unset) -> unset;
normalize_observation_status(<<"unset">>) -> unset;
normalize_observation_status(defaulted) -> defaulted;
normalize_observation_status(<<"defaulted">>) -> defaulted;
normalize_observation_status(error) -> error;
normalize_observation_status(<<"error">>) -> error;
normalize_observation_status(_Status) -> unknown.

verify_dependency_coverage(Ctx, Deps, Opts) ->
    maybe
        {ok, Positive} ?= positive_dependency_checks(Ctx, Deps, Opts),
        {ok, Unset} ?= unset_dependency_checks(Ctx, Deps, Opts),
        true ?= all_true(
            fun({_Source, _Path, _Expected, Leaf}) ->
                dependency_covers_varied_leaf(Leaf)
                    orelse {error, <<"Dependencies do not cover varied inputs.">>}
            end,
            Positive
        ),
        true ?= all_true(
            fun({_Path, Leaf}) ->
                dependency_unset_leaf(Leaf)
                    orelse {error, <<"Dependencies do not cover unset observations.">>}
            end,
            Unset
        ),
        true ?= all_true(
            fun(Check) -> verify_positive_dependency(Check, Opts) end,
            Positive
        ),
        true ?= verify_negative_dependencies(Ctx, Deps, Opts),
        true
    end.

positive_dependency_checks(Ctx, Deps, Opts) ->
    maybe
        {ok, BaseChecks} ?=
            positive_dependency_checks(
                <<"base">>,
                <<"base">>,
                <<"varied-base">>,
                Ctx,
                Deps,
                Opts
            ),
        {ok, ReqChecks} ?=
            positive_dependency_checks(
                <<"request">>,
                <<"request">>,
                <<"varied-request">>,
                Ctx,
                Deps,
                Opts
            ),
        {ok, BaseChecks ++ ReqChecks}
    end.

positive_dependency_checks(Root, SourceName, VariedName, Ctx, Deps, Opts) ->
    case context_value(VariedName, Ctx, Opts) of
        {not_found, _} ->
            {ok, []};
        {ok, Varied} ->
            maybe
                {ok, Source} ?= context_value(SourceName, Ctx, Opts),
                {ok,
                    [
                        {Source, Path, Expected, dependency_value_at(Deps, [Root | Path])}
                    ||
                        {Path, Expected} <- public_leaf_values(hb_private:reset(Varied)),
                        not unset_literal(Expected)
                    ]}
            end;
        Error ->
            Error
    end.

unset_dependency_checks(Ctx, Deps, Opts) ->
    maybe
        {ok, BaseChecks} ?=
            unset_dependency_checks(<<"base">>, <<"varied-base">>, Ctx, Deps, Opts),
        {ok, ReqChecks} ?=
            unset_dependency_checks(<<"request">>, <<"varied-request">>, Ctx, Deps, Opts),
        {ok, BaseChecks ++ ReqChecks}
    end.

unset_dependency_checks(Root, VariedName, Ctx, Deps, Opts) ->
    case context_value(VariedName, Ctx, Opts) of
        {not_found, _} ->
            {ok, []};
        {ok, Varied} ->
            {ok,
                [
                    {Path, dependency_value_at(Deps, [Root | Path])}
                ||
                    {Path, Expected} <- public_leaf_values(hb_private:reset(Varied)),
                    unset_literal(Expected)
                ]};
        Error ->
            Error
    end.

public_leaf_values(Value) when is_map(Value) ->
    lists:flatmap(
        fun(Key) ->
            [
                {[Key | Path], Leaf}
            ||
                {Path, Leaf} <- public_leaf_values(maps:get(Key, Value))
            ]
        end,
        [Key || Key <- maps:keys(Value), not hb_private:is_private(Key)]
    );
public_leaf_values(Value) ->
    [{[], Value}].

dependency_value_at(Value, []) ->
    Value;
dependency_value_at(Value, [Key | Rest]) when is_map(Value) ->
    case maps:find(Key, Value) of
        {ok, Next} -> dependency_value_at(Next, Rest);
        error -> not_found
    end;
dependency_value_at(_Value, _Path) ->
    not_found.

dependency_covers_varied_leaf(Leaf) when is_binary(Leaf) ->
    true;
dependency_covers_varied_leaf(Leaf) when is_map(Leaf) ->
    case normalize_observation_leaf(Leaf) of
        {ok, #{ <<"status">> := found }} -> true;
        _ -> false
    end;
dependency_covers_varied_leaf(_Leaf) ->
    false.

dependency_unset_leaf(Leaf) when is_map(Leaf) ->
    case normalize_observation_leaf(Leaf) of
        {ok, #{ <<"status">> := unset }} -> true;
        _ -> false
    end;
dependency_unset_leaf(_Leaf) ->
    false.

verify_positive_dependency({Source, Path, Expected, Leaf}, Opts) ->
    maybe
        true ?= dependency_covers_varied_leaf(Leaf)
            orelse {error, <<"Dependencies do not cover varied inputs.">>},
        {ok, Origin} ?= dependency_origin(Leaf),
        true ?= verify_positive_origin(Origin, Source, Path, Opts),
        {ok, OriginResult} ?= origin_result(Origin, Source, Opts),
        {ok, Observed} ?= observe_path(Source, Path, Opts),
        true ?= same_result_value(Observed, OriginResult, Opts)
            orelse {error, <<"Dependency origin does not reproduce observed value.">>},
        true ?= verify_projected_dependency(Leaf, Expected, Observed, Opts),
        true
    end.

verify_projected_dependency(Leaf, Expected, Observed, Opts) when is_map(Leaf) ->
    case normalize_observation_leaf(Leaf) of
        {ok,
            #{
                <<"status">> := found,
                <<"observed">> := ClaimedObserved,
                <<"value">> := Projected
            }} ->
            (same_result_value(Expected, Projected, Opts)
                andalso same_result_value(Observed, ClaimedObserved, Opts))
                orelse {error, <<"Dependency projection observation does not reproduce.">>};
        _ ->
            same_public_value(Expected, Observed, Opts)
                orelse {error, <<"Dependency origin does not reproduce varied value.">>}
    end;
verify_projected_dependency(_Leaf, Expected, Observed, Opts) ->
    same_public_value(Expected, Observed, Opts)
        orelse {error, <<"Dependency origin does not reproduce varied value.">>}.

dependency_origin(Origin) when is_binary(Origin) ->
    {ok, Origin};
dependency_origin(Leaf) when is_map(Leaf) ->
    case normalize_observation_leaf(Leaf) of
        {ok, #{ <<"status">> := found, <<"origin">> := Origin }} -> {ok, Origin};
        _ -> {error, <<"Dependency leaf has no origin.">>}
    end;
dependency_origin(_Leaf) ->
    {error, <<"Dependency leaf has no origin.">>}.

verify_negative_dependencies(Ctx, Deps, Opts) ->
    Negative = negative_dependency_checks(Deps),
    case Negative of
        [] ->
            true;
        _ ->
            maybe
                {ok, Base} ?= context_value(<<"base">>, Ctx, Opts),
                {ok, Req} ?= context_value(<<"request">>, Ctx, Opts),
                VBase = optional_context_value(<<"varied-base">>, Ctx, Opts),
                VReq = optional_context_value(<<"varied-request">>, Ctx, Opts),
                true ?= all_true(
                    fun(Check) ->
                        verify_negative_dependency(Check, Base, Req, VBase, VReq, Opts)
                    end,
                    Negative
                ),
                true
            end
    end.

optional_context_value(Name, Ctx, Opts) ->
    case context_value(Name, Ctx, Opts) of
        {ok, Value} -> Value;
        _ -> not_found
    end.

negative_dependency_checks(Deps) ->
    negative_dependency_checks(<<"base">>, [], maps:get(<<"base">>, Deps, #{}))
        ++ negative_dependency_checks(
            <<"request">>,
            [],
            maps:get(<<"request">>, Deps, #{})
        ).

negative_dependency_checks(Root, Path, Node) when is_map(Node) ->
    case normalize_observation_leaf(Node) of
        {ok, #{ <<"status">> := found }} ->
            [];
        {ok, Leaf} ->
            [{Root, Path, Leaf}];
        false ->
            lists:flatmap(
                fun({Key, Child}) ->
                    negative_dependency_checks(Root, Path ++ [Key], Child)
                end,
                maps:to_list(Node)
            )
    end;
negative_dependency_checks(_Root, _Path, _Node) ->
    [].

verify_negative_dependency({Root, Path, Leaf}, Base, Req, VBase, VReq, Opts) ->
    Source =
        case Root of
            <<"base">> -> Base;
            <<"request">> -> Req
        end,
    Varied =
        case Root of
            <<"base">> -> VBase;
            <<"request">> -> VReq
        end,
    Status = maps:get(<<"status">>, Leaf),
    Origin = maps:get(<<"origin">>, Leaf),
    maybe
        true ?= observation_path_matches(Path, maps:get(<<"path">>, Leaf), Opts)
            orelse {error, <<"Dependency observation path does not match leaf path.">>},
        true ?= negative_origin_matches(Origin, Source, Path, Status, Opts)
            orelse {error, <<"Dependency observation origin does not match path.">>},
        true ?= verify_negative_observation(
            Status,
            Source,
            Path,
            Leaf,
            varied_value_at(Varied, Path),
            Opts
        ),
        true
    end.

varied_value_at(Varied, Path) when is_map(Varied) ->
    dependency_value_at(hb_private:reset(Varied), Path);
varied_value_at(_Varied, _Path) ->
    not_found.

observation_path_matches(Expected, Claimed, Opts) ->
    case observation_path_parts(Claimed, Opts) of
        invalid -> false;
        Parts -> Parts =:= Expected
    end.

observation_path_parts(Path, Opts) ->
    try
        case hb_path:term_to_path_parts(Path, Opts) of
            undefined -> [];
            Parts -> Parts
        end
    catch
        _:_ -> invalid
    end.

negative_origin_matches(Origin, Source, Path, unset, Opts) ->
    origin_matches_source_path(Origin, Source, Path, Opts);
negative_origin_matches(Origin, Source, Path, _Status, Opts) ->
    SourceOrigin = origin_ref(Source, Opts),
    Origin =:= SourceOrigin
        orelse
            case origin_address(Origin, Opts) of
                {ok, SourceOrigin} -> true;
                _ -> origin_matches_source_path(Origin, Source, Path, Opts)
            end.

verify_negative_observation(not_found, Source, Path, _Leaf, _Expected, Opts) ->
    case {raw_unset_at_path(Source, Path, Opts), observe_path(Source, Path, Opts)} of
        {false, {error, not_found}} -> true;
        _ -> {error, <<"Dependency not_found observation does not reproduce.">>}
    end;
verify_negative_observation(unset, Source, Path, _Leaf, _Expected, Opts) ->
    case {raw_unset_at_path(Source, Path, Opts), observe_path(Source, Path, Opts)} of
        {true, {error, not_found}} -> true;
        _ -> {error, <<"Dependency unset observation does not reproduce.">>}
    end;
verify_negative_observation(defaulted, Source, Path, Leaf, Expected, Opts) ->
    case observe_path(Source, Path, Opts) of
        {error, not_found} ->
            default_observation_valid(maps:get(<<"default">>, Leaf), Expected, Opts)
                orelse {error, <<"Dependency defaulted value does not match.">>};
        _ -> {error, <<"Dependency defaulted observation does not reproduce.">>}
    end;
verify_negative_observation(error, Source, Path, Leaf, _Expected, Opts) ->
    case observe_path(Source, Path, Opts) of
        {error, Reason} ->
            same_result_value(Reason, maps:get(<<"error">>, Leaf), Opts)
                orelse {error, <<"Dependency error observation does not reproduce.">>};
        _ ->
            {error, <<"Dependency error observation does not reproduce.">>}
    end.

default_observation_valid(DefaultID, not_found, _Opts) ->
    valid_default_id(DefaultID);
default_observation_valid(DefaultID, Expected, Opts) ->
    try
        valid_default_id(DefaultID) andalso value_id(<<"base">>, Expected, Opts) =:= DefaultID
    catch
        _:_ -> false
    end.

verify_positive_origin(Origin, Source, Path, Opts) ->
    maybe
        true ?= origin_matches_source_path(Origin, Source, Path, Opts)
            orelse {error, <<"Dependency origin does not match observed path.">>},
        true
    end.

origin_matches_source_path(Origin, Source, Path, Opts) ->
    Expected = origin_hashpath(Source, Path, Opts),
    Origin =:= Expected
        orelse
            case origin_address(Origin, Opts) of
                {ok, Address} -> Address =:= Expected;
                _ -> false
            end.

origin_hashpath(Source, [], Opts) ->
    origin_ref(Source, Opts);
origin_hashpath(Source, Path, Opts) ->
    {Ref, OriginPath} = origin_location(Source, Path, Opts),
    origin_ref_path(Ref, OriginPath, Opts).

origin_location(Source, [], Opts) ->
    {origin_ref(Source, Opts), []};
origin_location(Source, Path = [Key | _], Opts) ->
    case origin_message(Source, Opts) of
        {ok, Message, Ref} ->
            case hb_maps:find(Key, Message, Opts) of
                {ok, Value} ->
                    origin_location_at(Value, tl(Path), Ref, [Key], Opts);
                error ->
                    case hb_maps:find(<<"...">>, Message, Opts) of
                        {ok, Ancestor} -> origin_location(Ancestor, Path, Opts);
                        error -> {Ref, Path}
                    end
            end;
        error ->
            {origin_ref(Source, Opts), Path}
    end.

origin_location_at(_Value, [], Ref, Prefix, _Opts) ->
    {Ref, Prefix};
origin_location_at(Value, Path = [Key | Rest], Ref, Prefix, Opts) ->
    case origin_message(Value, Opts) of
        {ok, Message, _ValueRef} ->
            case hb_maps:find(Key, Message, Opts) of
                {ok, Child} ->
                    origin_location_at(Child, Rest, Ref, Prefix ++ [Key], Opts);
                error ->
                    case hb_maps:find(<<"...">>, Message, Opts) of
                        {ok, Ancestor} -> origin_location(Ancestor, Path, Opts);
                        error -> {Ref, Prefix ++ Path}
                    end
            end;
        error ->
            {Ref, Prefix ++ Path}
    end.

origin_message(Source, _Opts) when is_map(Source) ->
    {ok, Source, origin_ref(Source, _Opts)};
origin_message(Source, Opts) when ?IS_LINK(Source) ->
    case hb_cache:ensure_loaded(Source, Opts) of
        Msg when is_map(Msg) -> {ok, Msg, origin_ref(Source, Opts)};
        _ -> error
    end;
origin_message(Source, Opts) when is_binary(Source) ->
    case is_hashpath_reference(Source) of
        true ->
            case load(Source, Opts) of
                {ok, Msg} when is_map(Msg) -> {ok, Msg, Source};
                _ -> error
            end;
        false ->
            case hb_cache:read(Source, Opts) of
                {ok, Msg} when is_map(Msg) -> {ok, Msg, Source};
                _ -> error
            end
    end;
origin_message(_Source, _Opts) ->
    error.

origin_ref(Source, _Opts) when is_map(Source) ->
    case hb_private:from_message(Source) of
        #{ <<"hashpath">> := HP } ->
            case hashpath_matches_base(HP, Source, _Opts) of
                true -> HP;
                false -> value_id(<<"base">>, Source, id_only_opts(_Opts))
            end;
        _ -> value_id(<<"base">>, Source, id_only_opts(_Opts))
    end;
origin_ref(Source, _Opts) when is_binary(Source) ->
    Source;
origin_ref(Source, Opts) ->
    value_id(<<"base">>, Source, id_only_opts(Opts)).

id_only_opts(Opts) ->
    Opts#{ <<"caching-schema">> => true }.

origin_ref_path(Ref, [], _Opts) ->
    Ref;
origin_ref_path(Ref, Path, _Opts) ->
    PathBin = hb_path:to_binary(Path),
    <<Ref/binary, "/", PathBin/binary>>.

origin_address(Origin, Opts) ->
    case parse(Origin, Opts) of
        [#{ <<"base-id">> := Base } = Ctx | _Rest] ->
            case maps:find(<<"request-id">>, Ctx) of
                {ok, Req} -> {ok, <<Base/binary, "/", Req/binary>>};
                error -> {ok, Base}
            end;
        _ ->
            {error, <<"Invalid dependency origin.">>}
    end.

origin_result(Origin, Source, Opts) ->
    case parse(Origin, Opts) of
        {error, _} = Error ->
            Error;
        [] ->
            {error, <<"Empty dependency origin.">>};
        Parts ->
            origin_result(undefined, Parts, Source, Opts)
    end.

origin_result(State, [], _Source, _Opts) ->
    {ok, State};
origin_result(State, [_Part | _Rest], _Source, _Opts)
        when State =/= undefined, not is_map(State) ->
    {error, <<"Dependency origin is not verifiable.">>};
origin_result(State, [Part | Rest], Source, Opts) ->
    PartWithBase =
        case State of
            undefined -> seed_origin_base(Part, Source, Opts);
            _ -> Part#{ <<"base">> => State }
        end,
    case verify_context(PartWithBase, Opts) of
        {true, NextState} -> origin_result(NextState, Rest, Source, Opts);
        false -> {error, <<"Dependency origin is not verifiable.">>}
    end.

seed_origin_base(Part = #{ <<"base-id">> := BaseID }, Source, Opts) ->
    case find_origin_source(BaseID, Source, Opts) of
        {ok, MatchedSource} -> Part#{ <<"base">> => MatchedSource };
        error -> Part
    end;
seed_origin_base(Part, _Source, _Opts) ->
    Part.

find_origin_source(Ref, Source, Opts) ->
    case origin_ref_matches(Ref, Source, Opts) of
        true ->
            {ok, Source};
        false ->
            case origin_message(Source, Opts) of
                {ok, Message, _} ->
                    case hb_maps:find(<<"...">>, Message, Opts) of
                        {ok, Ancestor} -> find_origin_source(Ref, Ancestor, Opts);
                        error -> error
                    end;
                error ->
                    error
            end
    end.

origin_ref_matches(Ref, Source, Opts) ->
    origin_ref(Source, Opts) =:= Ref
        orelse value_id(<<"base">>, Source, Opts) =:= Ref.

observe_path(Source, [], _Opts) ->
    {ok, Source};
observe_path(Source, Path, Opts) ->
    hb_ao:resolve(
        Source,
        #{ <<"path">> => hb_path:to_binary(Path) },
        Opts#{
            <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
            <<"hashpath">> => ignore,
            <<"spawn-worker">> => false
        }
    ).

raw_unset_at_path(_Source, [], _Opts) ->
    false;
raw_unset_at_path(Source, [Key | Rest], Opts) ->
    case raw_unset_source(Source, Opts) of
        Message when is_map(Message) ->
            case maps:find(Key, Message) of
                {ok, Value0} ->
                    Value = hb_cache:ensure_loaded(Value0, Opts),
                    case {unset_literal(Value), Rest} of
                        {true, _} -> true;
                        {false, [_ | _]} -> raw_unset_at_path(Value, Rest, Opts);
                        {false, []} -> false
                    end;
                error ->
                    case maps:find(<<"...">>, Message) of
                        {ok, Ancestor} -> raw_unset_at_path(Ancestor, [Key | Rest], Opts);
                        error -> false
                    end
            end;
        _ ->
            false
    end.

raw_unset_source(Source, _Opts) when is_map(Source) ->
    Source;
raw_unset_source(Source, Opts) when ?IS_LINK(Source) ->
    hb_cache:ensure_loaded(Source, Opts);
raw_unset_source(Source, Opts) when is_binary(Source) ->
    case is_hashpath_reference(Source) of
        true ->
            case load(Source, Opts) of
                {ok, Loaded} -> Loaded;
                _ -> Source
            end;
        false ->
            case hb_cache:read(Source, Opts) of
                {ok, Loaded} -> Loaded;
                _ -> Source
            end
    end;
raw_unset_source(Source, _Opts) ->
    Source.

unset_literal(unset) -> true;
unset_literal(<<"unset">>) -> true;
unset_literal(_) -> false.

all_true(_Fun, []) ->
    true;
all_true(Fun, [Item | Rest]) ->
    case Fun(Item) of
        true -> all_true(Fun, Rest);
        Error -> Error
    end.

%% @doc Verify that the results of the execution match those in the claim.
verify_equivalence(HPCtx, ExecutedCtx, Opts) ->
    case has_result_claim(HPCtx) of
        false ->
            true;
        true ->
            maybe
                {ok, HPNormalizer} ?=
                    protocol_normalizer(maps:get(<<"normalizer">>, HPCtx, replace)),
                {ok, ExecNormalizer} ?=
                    protocol_normalizer(maps:get(<<"normalizer">>, ExecutedCtx, replace)),
                true ?= HPNormalizer =:= ExecNormalizer
                    orelse {error, <<"Normalizers do not match">>},
                {ok, HPResult} ?= find_result_id(HPCtx, Opts),
                {ok, ExecResult} ?= find_result_id(ExecutedCtx, Opts),
                true ?= HPResult =:= ExecResult
                    orelse {error, <<"Results do not match">>}
            end
    end.

%% @doc Load the minimal executable base for a hashpath or a given part number
%% within it.
load(Hashpath, Opts) when is_binary(Hashpath) ->
    load(parse(Hashpath, Opts), Opts);
load({error, _Reason} = Error, _Opts) ->
    Error;
load(Parts, Opts) ->
    load(Parts, length(Parts), Opts).
load(Hashpath, PartNum, Opts) when is_binary(Hashpath) ->
    load(parse(Hashpath, Opts), PartNum, Opts);
load({error, _Reason} = Error, _PartNum, _Opts) ->
    Error;
load(Parts, PartNum, Opts) ->
    case load_sequence(Parts, PartNum, Opts) of
        {ok, []} ->
            {error, <<"Cannot load empty hashpath.">>};
        {ok, PrefixParts} ->
            load_parts(PrefixParts, [], undefined, Opts);
        {error, _} = Error ->
            Error
    end.

%% @doc Find the prefix of hashpath contexts to load/apply through `PartNum`.
load_sequence(Parts, OutOfBounds, _Opts)
        when OutOfBounds < 1 orelse OutOfBounds > length(Parts) ->
    {
        error,
        <<
            "Hashpath part number `",
            (hb_util:bin(OutOfBounds))/binary,
            "` not found. Hashpath length: ",
            (hb_util:bin(length(Parts)))/binary,
            "."
        >>
    };
load_sequence(Parts, PartNum, _Opts) ->
    {ok, lists:sublist(Parts, PartNum)}.

load_parts([], _RevPrefix, State, _Opts) ->
    {ok, State};
load_parts([Ctx | Rest], RevPrefix, State, Opts) ->
    maybe
        PriorPrefix = lists:reverse(RevPrefix),
        NextRevPrefix = [Ctx | RevPrefix],
        {ok, NextState0} ?= result_from_context(State, Ctx, Opts),
        NextState1 = with_loaded_parent_ref(NextState0, Ctx, PriorPrefix, Opts),
        NextState = with_loaded_hashpath(NextState1, lists:reverse(NextRevPrefix), Opts),
        load_parts(Rest, NextRevPrefix, NextState, Opts)
    end.

with_loaded_parent_ref(State, Ctx, PriorPrefix, Opts) when is_map(State) ->
    case {normalizer_mode(maps:get(<<"normalizer">>, Ctx, replace)), maps:is_key(<<"...">>, State)} of
        {{extension, _}, true} ->
            case parent_ref(PriorPrefix, Ctx, Opts) of
                {ok, Ref} -> State#{ <<"...">> => Ref };
                _ -> State
            end;
        _ ->
            State
    end;
with_loaded_parent_ref(State, _Ctx, _PriorPrefix, _Opts) ->
    State.

parent_ref([], Ctx, _Opts) when is_map_key(<<"base-id">>, Ctx) ->
    {ok, maps:get(<<"base-id">>, Ctx)};
parent_ref([], #{ <<"base">> := Base }, Opts) when is_map(Base) ->
    case hb_private:from_message(Base) of
        #{ <<"hashpath">> := HP } -> {ok, HP};
        _ ->
            ID = value_id(<<"base">>, Base, Opts),
            try hb_cache:write(Base, Opts)
            catch _:_ -> ok
            end,
            {ok, ID}
    end;
parent_ref([], _Ctx, _Opts) ->
    {not_found, <<"base">>};
parent_ref(PriorPrefix, _Ctx, Opts) ->
    {ok, format_weak(PriorPrefix, Opts)}.

with_loaded_hashpath(State, Prefix, Opts) when is_map(State) ->
    case prefix_hashpath_witnessed(Prefix) of
        true ->
            WithHashpath =
                hb_private:set_priv(
                    State,
                    (hb_private:from_message(State))#{ <<"hashpath">> => format(Prefix, Opts) }
                ),
            try hb_cache:write_hashpath(WithHashpath, Opts)
            catch _:_ -> ok
            end,
            WithHashpath;
        false ->
            State
    end;
with_loaded_hashpath(State, _Prefix, _Opts) ->
    State.

prefix_hashpath_witnessed(Prefix) ->
    prefix_hashpath_witnessed(Prefix, true).

prefix_hashpath_witnessed([], _First) ->
    true;
prefix_hashpath_witnessed([Ctx | Rest], true) ->
    context_hashpath_witnessed(Ctx) andalso prefix_hashpath_witnessed(Rest, false);
prefix_hashpath_witnessed([Ctx | Rest], false) ->
    segment_hashpath_witnessed(Ctx) andalso prefix_hashpath_witnessed(Rest, false).

%% @doc Extract, if we can, a workable post-exec value from a context either via
%% its already accumulated result, by loading a terminal result ID, or by applying
%% an extension patch to the selected parent.
result_from_context(Ctx, Opts) -> result_from_context(undefined, Ctx, Opts).
result_from_context(undefined, Ctx, Opts)
        when is_map_key(<<"base-id">>, Ctx),
             not is_map_key(<<"request-id">>, Ctx),
             not is_map_key(<<"request">>, Ctx) ->
    context_value(<<"base">>, Ctx, Opts);
result_from_context(State, Ctx, Opts) ->
    case normalizer_mode(maps:get(<<"normalizer">>, Ctx, replace)) of
        replacement ->
            result_from_valid_mode(replacement, State, Ctx, Opts);
        {extension, ParentName} ->
            result_from_valid_mode({extension, ParentName}, State, Ctx, Opts);
        {error, Normalizer} ->
            {error, <<"Unsupported normalizer `", (hb_util:bin(Normalizer))/binary, "`.">>}
    end.

with_context_hashpath(Result, _Ctx, _Opts) when not is_map(Result) ->
    Result;
with_context_hashpath(Result, Ctx, Opts) ->
    case hashpath_enabled(Opts) of
        false ->
            Result;
        true ->
            try
                Hashpath = format(Ctx, Opts),
                case context_hashpath_witnessed(Ctx) of
                    true ->
                        cache_context_components(Ctx, Opts),
                        hb_private:set_priv(
                            Result,
                            (hb_private:from_message(Result))#{ <<"hashpath">> => Hashpath }
                        );
                    false ->
                        Result
                end
            catch
                throw:{context_not_viable, unavailable_field, _} -> Result;
                throw:{unsupported_normalizer, _} -> Result
            end
    end.

context_hashpath_witnessed(Ctx) ->
    (not has_result_claim(Ctx))
        orelse (has_claim_field(<<"base">>, Ctx) andalso segment_hashpath_witnessed(Ctx)).

segment_hashpath_witnessed(Ctx) ->
    (not has_result_claim(Ctx))
        orelse
            (
                has_claim_field(<<"request">>, Ctx)
                    andalso has_claim_field(<<"varied-base">>, Ctx)
                    andalso has_claim_field(<<"varied-request">>, Ctx)
                    andalso has_claim_field(<<"dependencies">>, Ctx)
                    andalso maps:is_key(<<"normalizer">>, Ctx)
            ).

cache_context_components(Ctx, Opts) ->
    case lists:member(<<"no-store">>, maps:get(<<"cache-control">>, Opts, [])) of
        true ->
            ok;
        false ->
            lists:foreach(
                fun(Name) -> cache_context_component(Name, Ctx, Opts) end,
                [
                    <<"base">>,
                    <<"request">>,
                    <<"varied-base">>,
                    <<"varied-request">>,
                    <<"dependencies">>,
                    <<"varied-result">>,
                    <<"result">>
                ]
            )
    end.

cache_context_component(Name, Ctx, Opts) ->
    case maps:find(Name, Ctx) of
        {ok, Value} ->
            try cache_context_component_value(Name, Value, Opts)
            catch _:_ -> ok
            end;
        error ->
            ok
    end.

cache_context_component_value(<<"dependencies">>, Value, Opts) ->
    {ok, ID} = hb_cache:write(Value, Opts),
    BundleID = value_id(<<"dependencies">>, Value, Opts),
    link_cache_id(BundleID, ID, Opts);
cache_context_component_value(_Name, Value, Opts) ->
    hb_cache:write(Value, Opts).

link_cache_id(ID, Path, Opts) ->
    case ID =:= Path of
        true ->
            ok;
        false ->
            Store = hb_opts:get(store, no_viable_store, Opts),
            hb_store:link(Store, #{ ID => Path }, Opts),
            ok
    end.

result_from_valid_mode(Mode, State, Ctx, Opts) ->
    case {maps:find(<<"result">>, Ctx), has_varied_result(Ctx)} of
        {{ok, Result}, true} when Mode =:= replacement ->
            case replacement_result_matches(Result, Ctx, Opts) of
                true -> {ok, Result};
                false -> {error, <<"Materialized result does not match varied result.">>};
                no_direct_check ->
                    maybe
                        {ok, Computed} ?= result_from_mode(Mode, State, Ctx, Opts),
                        true ?= same_result_value(Result, Computed, Opts)
                            orelse {error, <<"Materialized result does not match varied result.">>},
                        {ok, Result}
                    end
            end;
        {{ok, Result}, true} ->
            maybe
                {ok, Computed} ?= result_from_mode(Mode, State, Ctx, Opts),
                true ?= same_result_value(Result, Computed, Opts)
                    orelse {error, <<"Materialized result does not match varied result.">>},
                {ok, Result}
            end;
        {{ok, Result}, false} when Mode =:= replacement ->
            {ok, Result};
        {{ok, _Result}, false} ->
            {error, <<"Extension result requires a varied-result patch.">>};
        {error, _} ->
            result_from_mode(Mode, State, Ctx, Opts)
    end.

result_from_mode(replacement, _State, Ctx, Opts) ->
    context_value(<<"varied-result">>, Ctx, Opts);
result_from_mode({extension, ParentName}, State, Ctx, Opts) ->
    maybe
        {ok, Patch} ?= context_value(<<"varied-result">>, Ctx, Opts),
        true ?= is_map(Patch)
            orelse {error, <<"Extension result must be a message patch.">>},
        {ok, Parent} ?= parent_value(State, ParentName, Ctx, Opts),
        true ?= is_map(Parent)
            orelse {error, <<"Extension parent must be a message.">>},
        message_set(Parent, Patch, Opts)
    end.

normalizer_mode(base) -> {extension, <<"base">>};
normalizer_mode(replace) -> replacement;
normalizer_mode(none) -> replacement;
normalizer_mode(undefined) -> replacement;
normalizer_mode(Other) -> {error, Other}.

has_varied_result(Ctx) ->
    maps:is_key(<<"varied-result">>, Ctx)
        orelse maps:is_key(<<"varied-result-id">>, Ctx).

same_public_value(Left, Right, Opts) ->
    hb_private:reset(hb_cache:ensure_all_loaded(Left, Opts))
        =:= hb_private:reset(hb_cache:ensure_all_loaded(Right, Opts)).

same_result_value(Left, Right, Opts) ->
    normalize_result_value(Left, Opts) =:= normalize_result_value(Right, Opts).

normalize_result_value(Value, Opts) ->
    Loaded = hb_cache:ensure_all_loaded(load_result_reference(Value, Opts), Opts),
    Public = hb_private:reset(Loaded),
    case Public of
        Map when is_map(Map) ->
            maps:map(fun(_Key, Inner) -> normalize_result_value(Inner, Opts) end, Map);
        List when is_list(List) ->
            [normalize_result_value(Inner, Opts) || Inner <- List];
        Other ->
            Other
    end.

load_result_reference(Value, Opts) when ?IS_ID(Value) ->
    case hb_cache:read(Value, Opts) of
        {ok, Loaded} -> Loaded;
        _ -> Value
    end;
load_result_reference(Value, Opts) when is_binary(Value) ->
    case binary:match(Value, <<"/">>) of
        nomatch ->
            Value;
        _ ->
            case hb_cache:read(Value, Opts) of
                {ok, Loaded} ->
                    Loaded;
                _ ->
                    case load(Value, Opts) of
                        {ok, Loaded} -> Loaded;
                        _ -> Value
                    end
            end
    end;
load_result_reference(Value, _Opts) ->
    Value.

replacement_result_matches(Result, Ctx, Opts)
        when is_map_key(<<"varied-result">>, Ctx),
             is_map_key(<<"varied-result-id">>, Ctx) ->
    same_result_value(Result, maps:get(<<"varied-result">>, Ctx), Opts)
        andalso value_id(<<"varied-result">>, maps:get(<<"varied-result">>, Ctx), Opts)
            =:= maps:get(<<"varied-result-id">>, Ctx);
replacement_result_matches(Result, Ctx, Opts)
        when is_map_key(<<"varied-result">>, Ctx) ->
    same_result_value(Result, maps:get(<<"varied-result">>, Ctx), Opts);
replacement_result_matches(Result, Ctx, Opts)
        when is_map_key(<<"varied-result-id">>, Ctx) ->
    hb_message:id(Result, all, Opts) =:= maps:get(<<"varied-result-id">>, Ctx);
replacement_result_matches(_Result, _Ctx, _Opts) ->
    no_direct_check.

hashpath_enabled(Opts) ->
    LocalMode =
        case is_map(Opts) of
            true -> maps:get(<<"hashpath">>, Opts, maps:get(hashpath, Opts, undefined));
            false -> undefined
        end,
    Mode =
        case LocalMode of
            undefined -> hb_opts:get(<<"hashpath">>, enabled, Opts);
            _ -> LocalMode
        end,
    Mode =/= ignore.

protocol_normalizer(Normalizer) ->
    case normalizer_mode(Normalizer) of
        {extension, _} -> {ok, base};
        replacement -> {ok, replace};
        {error, _} -> {error, Normalizer}
    end.

find_result_id(Ctx, Opts) ->
    case normalizer_mode(maps:get(<<"normalizer">>, Ctx, replace)) of
        {extension, _} ->
            find_id(<<"varied-result">>, Ctx, Opts);
        replacement ->
            case find_id(<<"varied-result">>, Ctx, Opts) of
                {ok, _} = OK -> OK;
                {not_found, _} -> find_id(<<"result">>, Ctx, Opts)
            end;
        {error, Normalizer} ->
            {error, Normalizer}
    end.

message_set(Parent, Patch, _Opts) ->
    case lists:partition(fun hb_private:is_private/1, maps:keys(Parent)) of
        {[], _} ->
            {ok, Patch#{ <<"...">> => Parent }};
        {PrivKeys, _} ->
            {
                ok,
                (maps:merge(Patch, carried_private_keys(Parent, PrivKeys)))#{
                    <<"...">> => maps:without(PrivKeys, Parent)
                }
            }
    end.

carried_private_keys(Parent, PrivKeys) ->
    lists:foldl(
        fun(Key, Acc) ->
            case carried_private_value(Key, maps:get(Key, Parent)) of
                {keep, Value} -> Acc#{ Key => Value };
                drop -> Acc
            end
        end,
        #{},
        PrivKeys
    ).

carried_private_value(<<"priv">>, Priv) when is_map(Priv) ->
    Clean = maps:without([<<"hashpath">>, hashpath], Priv),
    case map_size(Clean) of
        0 -> drop;
        _ -> {keep, Clean}
    end;
carried_private_value(_Key, Value) ->
    {keep, Value}.

parent_value(undefined, Name, Ctx, Opts) ->
    context_value(Name, Ctx, Opts);
parent_value(State, <<"base">>, _Ctx, _Opts) ->
    {ok, State};
parent_value(_State, Name, Ctx, Opts) ->
    context_value(Name, Ctx, Opts).

context_value(Name, Ctx, Opts)
        when is_map_key(Name, Ctx),
             is_map_key(<<Name/binary, "-id">>, Ctx) ->
    Value = maps:get(Name, Ctx),
    ID = maps:get(<<Name/binary, "-id">>, Ctx),
    case value_id(Name, Value, Opts) of
        ID -> {ok, Value};
        _ -> {error, <<"ID mismatch for `", Name/binary, "`.">>}
    end;
context_value(Name, Ctx, _Opts) when is_map_key(Name, Ctx) ->
    {ok, maps:get(Name, Ctx)};
context_value(Name, Ctx, Opts) when is_map_key(<<Name/binary, "-id">>, Ctx) ->
    ID = maps:get(<<Name/binary, "-id">>, Ctx),
    read_context_reference(ID, Opts);
context_value(Name, _Ctx, _Opts) ->
    {not_found, Name}.

read_context_reference(ID, Opts) ->
    case hb_cache:read(ID, Opts) of
        {ok, _} = OK ->
            OK;
        _ ->
            case is_hashpath_reference(ID) of
                true -> load(ID, Opts);
                false -> {error, not_found}
            end
    end.

value_id(<<"dependencies">>, Value, Opts) ->
    case direct_reference_id(<<"dependencies">>, Value) of
        {ok, ID} ->
            ID;
        not_found ->
            reject_unsafe_direct_component(<<"dependencies">>, Value),
            hb_message:id(
                Value,
                #{ <<"committers">> => <<"none">>, <<"bundle">> => true },
                Opts
            )
    end;
value_id(Name, Value, Opts) ->
    case direct_reference_id(Name, Value) of
        {ok, ID} -> ID;
        not_found ->
            reject_unsafe_direct_component(Name, Value),
            hb_message:id(Value, all, Opts)
    end.

direct_reference_id(<<"base">>, Value) when is_binary(Value) ->
    case {?IS_ID(Value) andalso is_hashpath_component_safe(Value), is_hashpath_reference(Value)} of
        {true, _} -> {ok, Value};
        {_, true} -> {ok, Value};
        _ -> not_found
    end;
direct_reference_id(<<"request">>, Value) when is_binary(Value) ->
    case is_hashpath_component_safe(Value) of
        true -> {ok, Value};
        false -> not_found
    end;
direct_reference_id(_Name, Value) when ?IS_ID(Value) ->
    case is_hashpath_component_safe(Value) of
        true -> {ok, Value};
        false -> not_found
    end;
direct_reference_id(_Name, _Value) ->
    not_found.

is_hashpath_reference(Value) when is_binary(Value) ->
    binary:match(Value, <<"/">>) =/= nomatch;
is_hashpath_reference(_Value) ->
    false.

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
        fun(HP) -> ?assertEqual(HP, format_weak(parse(HP, Opts), Opts)) end,
        [
            <<"BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4">>,
            <<"BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4/balance">>,
            <<"BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4/transfer/balance">>,
            <<"BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4/*"
              "=cGF0Y2gtaWQtMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAw">>
        ]
    ).

invalid_empty_hashpath_components_are_rejected_test() ->
    Opts = #{},
    ?assertMatch({error, _}, parse(<<>>, Opts)),
    ?assertMatch({error, _}, parse(<<"Base/">>, Opts)),
    ?assertMatch({error, _}, parse(<<"Base/Req@">>, Opts)),
    ?assertMatch({error, _}, parse(<<"Base/Req=">>, Opts)),
    ?assertEqual(false, verify_all(<<"Base/Req=">>, Opts)),
    ?assertMatch({error, _}, load(<<"Base/Req=">>, Opts)).

multi_segment_round_trip_preserves_components_test() ->
    Opts = #{},
    HP =
        <<
            "Base/Req1>VBase1+VReq1@Deps1=Patch1"
            "/Req2>VBase2+VReq2@Deps2.Result2"
        >>,
    ?assertEqual(HP, format(parse(HP, Opts), Opts)),
    ?assertEqual(
        [
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req1">>,
                <<"varied-base-id">> => <<"VBase1">>,
                <<"varied-request-id">> => <<"VReq1">>,
                <<"dependencies-id">> => <<"Deps1">>,
                <<"normalizer">> => base,
                <<"varied-result-id">> => <<"Patch1">>
            },
            #{
                <<"request-id">> => <<"Req2">>,
                <<"varied-base-id">> => <<"VBase2">>,
                <<"varied-request-id">> => <<"VReq2">>,
                <<"dependencies-id">> => <<"Deps2">>,
                <<"normalizer">> => replace,
                <<"varied-result-id">> => <<"Result2">>
            }
        ],
        parse(HP, Opts)
    ).

explicit_dependencies_id_formats_depends_component_test() ->
    Opts = #{},
    Ctx = #{
        <<"base-id">> => <<"Base">>,
        <<"request-id">> => <<"Req">>,
        <<"varied-base-id">> => <<"VBase">>,
        <<"varied-request-id">> => <<"VReq">>,
        <<"dependencies-id">> => <<"Deps">>,
        <<"normalizer">> => base,
        <<"varied-result-id">> => <<"Patch">>
    },
    ?assertEqual(<<"Base/Req>VBase+VReq@Deps=Patch">>, format(Ctx, Opts)).

format_uses_direct_reference_values_test() ->
    Opts = #{},
    BaseID = hb_message:id(#{ <<"base">> => true }, all, Opts),
    VBaseID = hb_message:id(#{ <<"varied-base">> => true }, all, Opts),
    VReqID = hb_message:id(#{ <<"varied-request">> => true }, all, Opts),
    DepsID =
        hb_message:id(
            #{ <<"base">> => #{}, <<"request">> => #{} },
            #{ <<"committers">> => <<"none">>, <<"bundle">> => true },
            Opts
        ),
    ResultID = hb_message:id(#{ <<"result">> => true }, all, Opts),
    ?assertEqual(
        <<BaseID/binary, "/balance>", VBaseID/binary, "+", VReqID/binary,
            "@", DepsID/binary, ".", ResultID/binary>>,
        format(
            #{
                <<"base">> => BaseID,
                <<"request">> => <<"balance">>,
                <<"varied-base">> => VBaseID,
                <<"varied-request">> => VReqID,
                <<"dependencies">> => DepsID,
                <<"normalizer">> => replace,
                <<"varied-result">> => ResultID
            },
            Opts
        )
    ).

format_rejects_terminal_result_without_full_witness_test() ->
    ?assertThrow(
        {context_not_viable, unavailable_field, <<"varied-base">>},
        format(
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req">>,
                <<"normalizer">> => replace,
                <<"varied-result-id">> => <<"Result">>
            },
            #{}
        )
    ),
    ?assertEqual(
        <<"Base/Req.Result">>,
        format_weak(
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req">>,
                <<"normalizer">> => replace,
                <<"varied-result-id">> => <<"Result">>
            },
            #{}
        )
    ).

format_rejects_partial_vary_witness_test() ->
    ?assertThrow(
        {context_not_viable, unavailable_field, <<"varied-request">>},
        format(
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req">>,
                <<"varied-base-id">> => <<"VBase">>
            },
            #{}
        )
    ).

format_rejects_slash_binary_request_values_test() ->
    ?assertThrow(
        {invalid_direct_hashpath_value, <<"request">>},
        format(
            #{ <<"base-id">> => <<"Base">>, <<"request">> => <<"a/b">> },
            #{}
        )
    ).

format_rejects_structural_request_values_test() ->
    ?assertThrow(
        {invalid_direct_hashpath_value, <<"request">>},
        format(
            #{ <<"base-id">> => <<"Base">>, <<"request">> => <<"a@b">> },
            #{}
        )
    ).

format_rejects_structural_explicit_component_ids_test() ->
    ?assertThrow(
        {invalid_hashpath_component_id, <<"base">>},
        format(
            #{ <<"base-id">> => <<"Base@bad">>, <<"request-id">> => <<"Req">> },
            #{}
        )
    ),
    ?assertThrow(
        {invalid_hashpath_component_id, <<"dependencies">>},
        format(
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req">>,
                <<"dependencies-id">> => <<"Deps/extra">>
            },
            #{}
        )
    ),
    ?assertThrow(
        {invalid_hashpath_component_id, <<"varied-result">>},
        format(
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req">>,
                <<"normalizer">> => replace,
                <<"varied-result-id">> => <<"Result@bad">>
            },
            #{}
        )
    ).

format_rejects_separator_bearing_id_shaped_values_test() ->
    BadIDShape = <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa@">>,
    ?assert(byte_size(BadIDShape) =:= 43),
    ?assertThrow(
        {invalid_direct_hashpath_value, <<"varied-result">>},
        format(
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req">>,
                <<"normalizer">> => replace,
                <<"varied-result">> => BadIDShape
            },
            #{}
        )
    ).

format_rejects_slash_binary_result_values_test() ->
    ?assertThrow(
        {invalid_direct_hashpath_value, <<"varied-result">>},
        format(
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req">>,
                <<"normalizer">> => replace,
                <<"varied-result">> => <<"foo/bar">>
            },
            #{}
        )
    ).

format_extended_message_base_uses_message_id_test() ->
    Opts = #{},
    Base = #{ <<"x">> => 1, <<"...">> => #{ <<"y">> => 2 } },
    BaseID = hb_message:id(Base, all, Opts),
    ?assertEqual(
        <<BaseID/binary, "/Req">>,
        format(#{ <<"base">> => Base, <<"request-id">> => <<"Req">> }, Opts)
    ).

format_base_prefers_private_hashpath_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base0 = #{ <<"x">> => 1 },
    {ok, BaseID} = hb_cache:write(Base0, Opts),
    Base = hb_private:set_priv(Base0, #{ <<"hashpath">> => BaseID }),
    ?assertEqual(
        <<BaseID/binary, "/Req">>,
        format(#{ <<"base">> => Base, <<"request-id">> => <<"Req">> }, Opts)
    ).

format_base_rejects_stale_private_hashpath_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base0 = #{ <<"x">> => 1 },
    OtherBase = #{ <<"x">> => 2 },
    BaseID = hb_message:id(Base0, all, Opts),
    {ok, OtherBaseID} = hb_cache:write(OtherBase, Opts),
    Base = hb_private:set_priv(Base0, #{ <<"hashpath">> => OtherBaseID }),
    ?assertEqual(
        <<BaseID/binary, "/Req">>,
        format(#{ <<"base">> => Base, <<"request-id">> => <<"Req">> }, Opts)
    ).

format_rejects_stale_private_hashpath_component_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Req0 = #{ <<"path">> => <<"x">> },
    OtherReq = #{ <<"path">> => <<"y">> },
    {ok, OtherReqID} = hb_cache:write(OtherReq, Opts),
    Req = hb_private:set_priv(Req0, #{ <<"hashpath">> => OtherReqID }),
    ?assertThrow(
        {id_mismatch, <<"request">>},
        format(
            #{
                <<"base-id">> => <<"Base">>,
                <<"request">> => Req,
                <<"request-id">> => OtherReqID
            },
            Opts
        )
    ).

explicit_dependency_claim_without_exec_dep_fails_test() ->
    Opts = #{},
    ?assertEqual(
        {error, <<"Dependencies claim not reproduced">>},
        verify_dependencies(
            #{ <<"dependencies">> => #{ <<"base">> => #{}, <<"request">> => #{} } },
            #{},
            Opts
        )
    ).

claim_level_defaults_to_schema_declared_test() ->
    ?assertEqual(
        true,
        verify_claim_level(#{}, #{ <<"claim-level">> => <<"schema-declared">> })
    ).

claim_level_mismatch_fails_test() ->
    ?assertEqual(
        {error, <<"Hashpath claim level does not match execution.">>},
        verify_claim_level(
            #{ <<"claim-level">> => <<"observed-exact">> },
            #{ <<"claim-level">> => <<"schema-declared">> }
        )
    ).

valid_dependencies_shape_passes_test() ->
    Deps = #{
        <<"base">> => #{
            <<"device">> => <<"BaseHP/device">>,
            <<"balance">> => #{
                <<"OUR_ADDRESS">> => <<"BaseHP/balance/OUR_ADDRESS">>,
                <<"SENDER">> => <<"BaseHP/balance/SENDER">>
            }
        },
        <<"request">> => #{
            <<"quantity">> => <<"ReqHP/quantity">>
        }
    },
    ?assertEqual(true, verify_context_ids(#{ <<"dependencies">> => Deps }, #{})).

dependencies_must_be_rooted_under_base_request_test() ->
    ?assertEqual(
        {error, <<"Invalid Dependencies shape.">>},
        verify_context_ids(
            #{ <<"dependencies">> => #{ <<"balance">> => <<"HP/balance">> } },
            #{}
        )
    ),
    ?assertEqual(
        {error, <<"Invalid Dependencies shape.">>},
        verify_context_ids(
            #{ <<"dependencies">> => #{ <<"base">> => <<"HP/base">>, <<"request">> => #{} } },
            #{}
        )
    ),
    ?assertEqual(
        {error, <<"Invalid Dependencies shape.">>},
        verify_context_ids(
            #{ <<"dependencies">> => #{ <<"base">> => #{} } },
            #{}
        )
    ).

dependencies_allow_status_origin_public_keys_test() ->
    Deps = #{
        <<"base">> => #{
            <<"metadata">> => #{
                <<"status">> => <<"BaseHP/metadata/status">>,
                <<"origin">> => <<"BaseHP/metadata/origin">>
            }
        },
        <<"request">> => #{}
    },
    ?assertEqual(true, verify_context_ids(#{ <<"dependencies">> => Deps }, #{})).

dependencies_reject_empty_origins_test() ->
    ?assertEqual(
        {error, <<"Invalid Dependencies shape.">>},
        verify_context_ids(
            #{
                <<"dependencies">> =>
                    #{
                        <<"base">> => #{ <<"x">> => <<>> },
                        <<"request">> => #{}
                    }
            },
            #{}
        )
    ).

dependencies_reject_invalid_origin_syntax_test() ->
    ?assertEqual(
        {error, <<"Invalid Dependencies shape.">>},
        verify_context_ids(
            #{
                <<"dependencies">> =>
                    #{
                        <<"base">> => #{ <<"x">> => <<"Not@AHashpath">> },
                        <<"request">> => #{}
                    }
            },
            #{}
        )
    ).

dependencies_must_cover_varied_positive_leaves_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{
        <<"device">> => <<"process@1.0">>,
        <<"balance">> => #{ <<"OUR_ADDRESS">> => 7 }
    },
    Req = #{ <<"path">> => <<"transfer">> },
    Ctx = #{
        <<"base">> => Base,
        <<"request">> => Req,
        <<"varied-base">> => #{
            <<"device">> => <<"process@1.0">>,
            <<"balance">> => #{ <<"OUR_ADDRESS">> => 7 }
        },
        <<"varied-request">> => #{ <<"path">> => <<"transfer">> },
        <<"dependencies">> => #{
            <<"base">> => #{ <<"device">> => origin_hashpath(Base, [<<"device">>], Opts) },
            <<"request">> => #{ <<"path">> => origin_hashpath(Req, [<<"path">>], Opts) }
        }
    },
    ?assertEqual(
        {error, <<"Dependencies do not cover varied inputs.">>},
        verify_dependencies(Ctx, Ctx, Opts)
    ).

dependencies_cover_varied_positive_leaves_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{
        <<"device">> => <<"process@1.0">>,
        <<"balance">> => #{ <<"OUR_ADDRESS">> => 7 }
    },
    Req = #{
        <<"path">> => <<"transfer">>,
        <<"quantity">> => 3
    },
    Ctx = #{
        <<"base">> => Base,
        <<"request">> => Req,
        <<"varied-base">> => #{
            <<"device">> => <<"process@1.0">>,
            <<"balance">> => #{ <<"OUR_ADDRESS">> => 7 }
        },
        <<"varied-request">> => #{
            <<"path">> => <<"transfer">>,
            <<"quantity">> => 3
        },
        <<"dependencies">> => #{
            <<"base">> => #{
                <<"device">> => origin_hashpath(Base, [<<"device">>], Opts),
                <<"balance">> => #{
                    <<"OUR_ADDRESS">> =>
                        origin_hashpath(
                            Base,
                            [<<"balance">>, <<"OUR_ADDRESS">>],
                            Opts
                        )
                }
            },
            <<"request">> => #{
                <<"path">> => origin_hashpath(Req, [<<"path">>], Opts),
                <<"quantity">> => origin_hashpath(Req, [<<"quantity">>], Opts)
            }
        }
    },
    ?assertEqual(true, verify_dependencies(Ctx, Ctx, Opts)).

dependencies_explicit_found_leaf_covers_scalar_test() ->
    Base = #{ <<"x">> => 1 },
    Ctx = #{
        <<"base">> => Base,
        <<"varied-base">> => #{ <<"x">> => 1 },
        <<"dependencies">> =>
            #{
                <<"base">> =>
                    #{
                        <<"x">> =>
                            #{
                                <<"status">> => <<"found">>,
                                <<"origin">> => origin_hashpath(Base, [<<"x">>], #{})
                            }
                    },
                <<"request">> => #{}
            }
	    },
	    ?assertEqual(true, verify_dependencies(Ctx, Ctx, #{})).

dependencies_explicit_found_leaf_covers_projected_scalar_test() ->
    Base = #{ <<"x">> => <<"3">> },
    Ctx = #{
        <<"base">> => Base,
        <<"varied-base">> => #{ <<"x">> => 3 },
        <<"dependencies">> =>
            #{
                <<"base">> =>
                    #{
                        <<"x">> =>
                            #{
                                <<"status">> => found,
                                <<"origin">> => origin_hashpath(Base, [<<"x">>], #{}),
                                <<"observed">> => <<"3">>,
                                <<"value">> => 3
                            }
                    },
                <<"request">> => #{}
            }
    },
    ?assertEqual(true, verify_dependencies(Ctx, Ctx, #{})).

dependencies_projected_found_leaf_rejects_wrong_observed_test() ->
    Base = #{ <<"x">> => <<"3">> },
    Ctx = #{
        <<"base">> => Base,
        <<"varied-base">> => #{ <<"x">> => 3 },
        <<"dependencies">> =>
            #{
                <<"base">> =>
                    #{
                        <<"x">> =>
                            #{
                                <<"status">> => found,
                                <<"origin">> => origin_hashpath(Base, [<<"x">>], #{}),
                                <<"observed">> => <<"4">>,
                                <<"value">> => 3
                            }
                    },
                <<"request">> => #{}
            }
    },
    ?assertMatch({error, _}, verify_dependencies(Ctx, Ctx, #{})).

dependencies_projected_found_leaf_rejects_wrong_value_test() ->
    Base = #{ <<"x">> => <<"3">> },
    Ctx = #{
        <<"base">> => Base,
        <<"varied-base">> => #{ <<"x">> => 3 },
        <<"dependencies">> =>
            #{
                <<"base">> =>
                    #{
                        <<"x">> =>
                            #{
                                <<"status">> => found,
                                <<"origin">> => origin_hashpath(Base, [<<"x">>], #{}),
                                <<"observed">> => <<"3">>,
                                <<"value">> => 4
                            }
                    },
                <<"request">> => #{}
            }
    },
    ?assertMatch({error, _}, verify_dependencies(Ctx, Ctx, #{})).

dependencies_explicit_not_found_leaf_verifies_test() ->
    Base = #{ <<"x">> => 1 },
    BaseID = hb_message:id(Base, all, #{}),
    Ctx = #{
        <<"base">> => Base,
        <<"request">> => #{ <<"path">> => <<"set">> },
        <<"dependencies">> =>
            #{
                <<"base">> =>
                    #{
                        <<"missing">> =>
                            #{
                                <<"status">> => not_found,
                                <<"origin">> => BaseID,
                                <<"path">> => <<"missing">>
                            }
                    },
                <<"request">> => #{}
            }
    },
    ?assertEqual(true, verify_dependencies(Ctx, Ctx, #{})).

dependencies_explicit_unset_leaf_verifies_test() ->
    Base = #{ <<"masked">> => <<"unset">> },
    Ctx = #{
        <<"base">> => Base,
        <<"request">> => #{ <<"path">> => <<"set">> },
        <<"varied-base">> => #{ <<"masked">> => unset },
        <<"dependencies">> =>
            #{
                <<"base">> =>
                    #{
                        <<"masked">> =>
                            #{
                                <<"status">> => <<"unset">>,
                                <<"origin">> => origin_hashpath(Base, [<<"masked">>], #{}),
                                <<"path">> => <<"masked">>
                            }
                    },
                <<"request">> => #{}
            }
    },
    ?assertEqual(true, verify_dependencies(Ctx, Ctx, #{})).

dependencies_explicit_defaulted_leaf_verifies_test() ->
    Base = #{ <<"x">> => 1 },
    DefaultID = hb_message:id(#{ <<"value">> => 7 }, all, #{}),
    BaseID = hb_message:id(Base, all, #{}),
    Ctx = #{
        <<"base">> => Base,
        <<"request">> => #{ <<"path">> => <<"set">> },
        <<"dependencies">> =>
            #{
                <<"base">> =>
                    #{
                        <<"missing">> =>
                            #{
                                <<"status">> => defaulted,
                                <<"origin">> => BaseID,
                                <<"path">> => <<"missing">>,
                                <<"default">> => DefaultID
                            }
                    },
                <<"request">> => #{}
            }
    },
    ?assertEqual(true, verify_dependencies(Ctx, Ctx, #{})).

dependencies_explicit_defaulted_leaf_does_not_cover_positive_test() ->
    Base = #{ <<"x">> => 1 },
    BaseID = hb_message:id(Base, all, #{}),
    DefaultID = hb_message:id(#{ <<"value">> => 7 }, all, #{}),
    Ctx = #{
        <<"base">> => Base,
        <<"request">> => #{ <<"path">> => <<"set">> },
        <<"varied-base">> => #{ <<"missing">> => DefaultID },
        <<"dependencies">> =>
            #{
                <<"base">> =>
                    #{
                        <<"missing">> =>
	                            #{
	                                <<"status">> => defaulted,
	                                <<"origin">> => BaseID,
	                                <<"path">> => <<"missing">>,
	                                <<"default">> => DefaultID
	                            }
                    },
                <<"request">> => #{}
            }
    },
    ?assertMatch({error, _}, verify_dependencies(Ctx, Ctx, #{})).

dependencies_negative_observation_rejects_present_value_test() ->
    Base = #{ <<"x">> => 1 },
    BaseID = hb_message:id(Base, all, #{}),
    Ctx = #{
        <<"base">> => Base,
        <<"request">> => #{ <<"path">> => <<"set">> },
        <<"dependencies">> =>
            #{
                <<"base">> =>
                    #{
                        <<"x">> =>
                            #{
                                <<"status">> => not_found,
                                <<"origin">> => BaseID,
                                <<"path">> => <<"x">>
                            }
                    },
                <<"request">> => #{}
            }
    },
    ?assertMatch({error, _}, verify_dependencies(Ctx, Ctx, #{})).

dependencies_id_is_loaded_for_coverage_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{ <<"x">> => 1 },
    Req = #{ <<"path">> => <<"set">>, <<"y">> => 2 },
    VariedBase = #{ <<"x">> => 1 },
    VariedReq = #{ <<"path">> => <<"set">>, <<"y">> => 2 },
    Deps = #{
        <<"base">> => #{ <<"x">> => origin_hashpath(Base, [<<"x">>], Opts) },
        <<"request">> => #{
            <<"path">> => origin_hashpath(Req, [<<"path">>], Opts),
            <<"y">> => origin_hashpath(Req, [<<"y">>], Opts)
        }
    },
    {ok, BaseID} = hb_cache:write(Base, Opts),
    {ok, ReqID} = hb_cache:write(Req, Opts),
    {ok, VariedBaseID} = hb_cache:write(VariedBase, Opts),
    {ok, VariedReqID} = hb_cache:write(VariedReq, Opts),
    {ok, DepsID} = hb_cache:write(Deps, Opts),
    HPCtx = #{
        <<"base-id">> => BaseID,
        <<"request-id">> => ReqID,
        <<"varied-base-id">> => VariedBaseID,
        <<"varied-request-id">> => VariedReqID,
        <<"dependencies-id">> => DepsID
    },
    ExecCtx = #{ <<"dependencies">> => Deps },
    ?assertEqual(true, verify_dependencies(HPCtx, ExecCtx, Opts)).

dependency_wrong_positive_origin_fails_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{ <<"x">> => 1 },
    WrongBase = #{ <<"x">> => 1, <<"wrong">> => true },
    Ctx = #{
        <<"base">> => Base,
        <<"varied-base">> => #{ <<"x">> => 1 },
        <<"dependencies">> => #{
            <<"base">> => #{ <<"x">> => origin_hashpath(WrongBase, [<<"x">>], Opts) },
            <<"request">> => #{}
        }
    },
    ?assertMatch({error, _}, verify_dependencies(Ctx, Ctx, Opts)).

dependency_origin_with_extra_suffix_fails_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{ <<"x">> => 1 },
    Ctx = #{
        <<"base">> => Base,
        <<"varied-base">> => #{ <<"x">> => 1 },
        <<"dependencies">> => #{
            <<"base">> => #{
                <<"x">> => <<(origin_hashpath(Base, [<<"x">>], Opts))/binary, "/extra">>
            },
            <<"request">> => #{}
        }
    },
    ?assertMatch({error, _}, verify_dependencies(Ctx, Ctx, Opts)).

dependency_origin_uses_value_ancestor_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Parent = #{ <<"x">> => 1 },
    Base = #{ <<"...">> => Parent },
    Ctx = #{
        <<"base">> => Base,
        <<"varied-base">> => #{ <<"x">> => 1 },
        <<"dependencies">> => #{
            <<"base">> => #{ <<"x">> => origin_hashpath(Base, [<<"x">>], Opts) },
            <<"request">> => #{}
        }
    },
    ?assertEqual(origin_hashpath(Parent, [<<"x">>], Opts), origin_hashpath(Base, [<<"x">>], Opts)),
    ?assertEqual(true, verify_dependencies(Ctx, Ctx, Opts)).

explicit_base_id_mismatch_fails_test() ->
    Opts = #{},
    Base = #{ <<"x">> => <<"1">> },
    OtherBase = #{ <<"x">> => <<"2">> },
    ?assertEqual(
        {error, <<"ID mismatch for `base`.">>},
        verify_context_ids(
            #{
                <<"base">> => Base,
                <<"base-id">> => hb_message:id(OtherBase, all, Opts)
            },
            Opts
        )
    ).

explicit_request_id_mismatch_fails_test() ->
    Opts = #{},
    Req = #{ <<"path">> => <<"set">>, <<"y">> => <<"2">> },
    OtherReq = #{ <<"path">> => <<"set">>, <<"y">> => <<"3">> },
    ?assertEqual(
        {error, <<"ID mismatch for `request`.">>},
        verify_context_ids(
            #{
                <<"request">> => Req,
                <<"request-id">> => hb_message:id(OtherReq, all, Opts)
            },
            Opts
        )
    ).

matching_explicit_value_ids_pass_test() ->
    Opts = #{},
    Base = #{ <<"x">> => <<"1">> },
    Req = #{ <<"path">> => <<"set">>, <<"y">> => <<"2">> },
    ?assertEqual(
        true,
        verify_context_ids(
            #{
                <<"base">> => Base,
                <<"base-id">> => hb_message:id(Base, all, Opts),
                <<"request">> => Req,
                <<"request-id">> => hb_message:id(Req, all, Opts)
            },
            Opts
        )
    ).

request_normalizer_is_not_a_protocol_mode_test() ->
    ?assertMatch(
        {error, _},
        result_from_context(
            #{},
            #{
                <<"normalizer">> => request,
                <<"varied-result">> => #{ <<"x">> => <<"1">> }
            },
            #{}
        )
    ),
    ?assertMatch(
        {error, _},
        result_from_context(
            #{},
            #{
                <<"normalizer">> => request,
                <<"result">> => #{ <<"x">> => <<"1">> }
            },
            #{}
        )
    ).

unsupported_normalizer_is_not_formatted_as_replacement_test() ->
    ?assertThrow(
        {unsupported_normalizer, request},
        format(
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req">>,
                <<"normalizer">> => request,
                <<"varied-result-id">> => <<"Result">>
            },
            #{}
        )
    ).

format_rejects_mismatched_direct_id_test() ->
    Opts = #{},
    Result = #{ <<"body">> => <<"ok">> },
    WrongID = hb_message:id(#{ <<"body">> => <<"no">> }, all, Opts),
    ?assertThrow(
        {id_mismatch, <<"varied-result">>},
        format(
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req">>,
                <<"normalizer">> => replace,
                <<"varied-result">> => Result,
                <<"varied-result-id">> => WrongID
            },
            Opts
        )
    ).

with_context_hashpath_rejects_mismatched_result_id_test() ->
    Opts = #{},
    Result = #{ <<"body">> => <<"ok">> },
    WrongID = hb_message:id(#{ <<"body">> => <<"no">> }, all, Opts),
    ?assertThrow(
        {id_mismatch, <<"varied-result">>},
        with_context_hashpath(
            Result,
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req">>,
                <<"normalizer">> => replace,
                <<"varied-result">> => Result,
                <<"varied-result-id">> => WrongID
            },
            Opts
        )
    ).

compact_terminal_assertion_without_vary_depends_is_weak_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Result = #{ <<"body">> => <<"ok">> },
    Base = #{ <<"x">> => Result },
    {ok, BaseID} = hb_cache:write(Base, Opts),
    ResultID = hb_message:id(Result, all, Opts),
    HP = <<BaseID/binary, "/x.", ResultID/binary>>,
    ?assertEqual(false, verify_all(HP, Opts)),
    ?assertEqual(
        false,
        verify_context(
            #{
                <<"base">> => Base,
                <<"request">> => #{ <<"path">> => <<"x">> },
                <<"normalizer">> => replace,
                <<"result">> => Result
            },
            Opts
        )
    ).

star_materialization_without_vary_depends_is_weak_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{ <<"body">> => <<"ok">> },
    Patch = #{ <<"body">> => <<"ok">> },
    {ok, BaseID} = hb_cache:write(Base, Opts),
    PatchID = hb_message:id(Patch, all, Opts),
    HP = <<BaseID/binary, "/*=", PatchID/binary>>,
    ?assertEqual(false, verify_all(HP, Opts)).

with_context_hashpath_skips_partial_result_witness_test() ->
    Result = #{ <<"body">> => <<"ok">> },
    Stamped =
        with_context_hashpath(
            Result,
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req">>,
                <<"dependencies">> => #{ <<"base">> => #{}, <<"request">> => #{} },
                <<"normalizer">> => replace,
                <<"result">> => Result
            },
            #{}
        ),
    ?assertEqual(undefined, maps:get(<<"hashpath">>, hb_private:from_message(Stamped), undefined)).

result_from_context_extension_links_parent_test() ->
    Base = #{ <<"x">> => <<"1">> },
    Patch = #{ <<"y">> => <<"2">> },
    {ok, Result} =
        result_from_context(
            Base,
            #{ <<"normalizer">> => base, <<"varied-result">> => Patch },
            #{}
        ),
    ?assertEqual(Patch#{ <<"...">> => Base }, Result).

result_from_context_extension_requires_patch_test() ->
    Base = #{ <<"x">> => <<"1">> },
    Result = #{ <<"y">> => <<"2">>, <<"...">> => Base },
    ?assertEqual(
        {error, <<"Extension result requires a varied-result patch.">>},
        result_from_context(
            Base,
            #{ <<"normalizer">> => base, <<"result">> => Result },
            #{}
        )
    ),
    ?assertEqual(
        <<"Base/Req">>,
        format(
            #{
                <<"base-id">> => <<"Base">>,
                <<"request-id">> => <<"Req">>,
                <<"normalizer">> => base,
                <<"result">> => Result
            },
            #{}
        )
    ).

result_from_context_extension_keeps_private_parent_keys_active_test() ->
    Base = #{
        <<"x">> => <<"1">>,
        <<"priv">> => #{
            <<"hashpath">> => <<"BaseHP">>,
            <<"trace">> => <<"Keep">>
        }
    },
    Patch = #{ <<"y">> => <<"2">> },
    {ok, Result} =
        result_from_context(
            Base,
            #{ <<"normalizer">> => base, <<"varied-result">> => Patch },
            #{}
    ),
    ?assertEqual(#{ <<"trace">> => <<"Keep">> }, maps:get(<<"priv">>, Result)),
    ?assertEqual(false, maps:is_key(<<"priv">>, maps:get(<<"...">>, Result))).

result_from_context_replacement_ignores_parent_test() ->
    Base = #{ <<"x">> => <<"1">> },
    Replacement = #{ <<"y">> => <<"2">> },
    ?assertEqual(
        {ok, Replacement},
        result_from_context(
            Base,
            #{ <<"normalizer">> => replace, <<"varied-result">> => Replacement },
            #{}
        )
    ).

materialized_result_must_match_varied_result_test() ->
    Base = #{ <<"x">> => <<"1">> },
    Patch = #{ <<"y">> => <<"2">> },
    Result = Patch#{ <<"...">> => Base },
    ?assertEqual(
        {ok, Result},
        result_from_context(
            Base,
            #{
                <<"normalizer">> => base,
                <<"varied-result">> => Patch,
                <<"result">> => Result
            },
            #{}
        )
    ).

materialized_result_mismatch_fails_test() ->
    Base = #{ <<"x">> => <<"1">> },
    Patch = #{ <<"y">> => <<"2">> },
    WrongResult = #{ <<"z">> => <<"3">> },
    ?assertEqual(
        {error, <<"Materialized result does not match varied result.">>},
        result_from_context(
            Base,
            #{
                <<"normalizer">> => base,
                <<"varied-result">> => Patch,
                <<"result">> => WrongResult
            },
            #{}
        )
    ).

materialized_replacement_result_can_back_uncached_id_test() ->
    Opts = #{},
    Result = #{ <<"body">> => <<"ok">> },
    ResultID = hb_message:id(Result, all, Opts),
    ?assertEqual(
        {ok, Result},
        result_from_context(
            #{},
            #{
                <<"normalizer">> => replace,
                <<"varied-result-id">> => ResultID,
                <<"result">> => Result
            },
            Opts
        )
    ).

materialized_replacement_accepts_varied_result_reference_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Result = #{ <<"body">> => <<"ok">> },
    {ok, ResultID} = hb_cache:write(Result, Opts),
    ?assertEqual(
        {ok, Result},
        result_from_context(
            #{},
            #{
                <<"normalizer">> => replace,
                <<"varied-result">> => ResultID,
                <<"varied-result-id">> => ResultID,
                <<"result">> => Result
            },
            Opts
        )
    ).

materialized_replacement_loads_hashpath_result_id_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Result = #{ <<"body">> => <<"ok">> },
    {ok, ResultID} = hb_cache:write(Result, Opts),
    ResultHP = <<"Base/Req.", ResultID/binary>>,
    {ok, Loaded} =
        result_from_context(
            #{},
            #{
                <<"normalizer">> => replace,
                <<"varied-result-id">> => ResultHP
            },
            Opts
        ),
    ?assertEqual(
        Result,
        hb_private:reset(hb_cache:ensure_all_loaded(Loaded, Opts))
    ).

materialized_replacement_id_mismatch_fails_test() ->
    Opts = #{},
    Result = #{ <<"body">> => <<"ok">> },
    WrongID = hb_message:id(#{ <<"body">> => <<"no">> }, all, Opts),
    ?assertEqual(
        {error, <<"Materialized result does not match varied result.">>},
        result_from_context(
            #{},
            #{
                <<"normalizer">> => replace,
                <<"varied-result-id">> => WrongID,
                <<"result">> => Result
            },
            Opts
        )
    ).

materialized_replacement_rejects_conflicting_direct_varied_result_test() ->
    Opts = #{},
    Result = #{ <<"body">> => <<"ok">> },
    ResultID = hb_message:id(Result, all, Opts),
    OtherVaried = #{ <<"body">> => <<"no">> },
    ?assertEqual(
        {error, <<"Materialized result does not match varied result.">>},
        result_from_context(
            #{},
            #{
                <<"normalizer">> => replace,
                <<"varied-result">> => OtherVaried,
                <<"varied-result-id">> => ResultID,
                <<"result">> => Result
            },
            Opts
        )
    ).

materialized_extension_rejects_conflicting_patch_id_test() ->
    Opts = #{},
    Base = #{ <<"x">> => <<"1">> },
    Patch = #{ <<"y">> => <<"2">> },
    OtherPatchID = hb_message:id(#{ <<"y">> => <<"3">> }, all, Opts),
    Result = Patch#{ <<"...">> => Base },
    ?assertEqual(
        {error, <<"ID mismatch for `varied-result`.">>},
        result_from_context(
            Base,
            #{
                <<"normalizer">> => base,
                <<"varied-result">> => Patch,
                <<"varied-result-id">> => OtherPatchID,
                <<"result">> => Result
            },
            Opts
        )
    ).

direct_result_equivalence_uses_materialized_result_test() ->
    Opts = #{},
    Result = #{ <<"body">> => <<"ok">> },
    ResultID = hb_message:id(Result, all, Opts),
    Ctx = #{
        <<"base-id">> => <<"Base">>,
        <<"request-id">> => <<"Req">>,
        <<"varied-base-id">> => <<"VBase">>,
        <<"varied-request-id">> => <<"VReq">>,
        <<"dependencies-id">> => <<"Deps">>,
        <<"normalizer">> => replace,
        <<"result">> => Result
    },
    ?assertEqual(<<"Base/Req>VBase+VReq@Deps.", ResultID/binary>>, format(Ctx, Opts)),
    ?assertEqual(
        true,
        verify_equivalence(
            #{ <<"varied-result-id">> => ResultID },
            #{ <<"result">> => Result },
            Opts
        )
    ).

load_cached_extension_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{ <<"x">> => <<"1">> },
    Patch = #{ <<"y">> => <<"2">> },
    {ok, BaseID} = hb_cache:write(Base, Opts),
    {ok, PatchID} = hb_cache:write(Patch, Opts),
    Ctx = #{
        <<"base-id">> => BaseID,
        <<"request-id">> => <<"Req">>,
        <<"normalizer">> => base,
        <<"varied-result-id">> => PatchID
    },
    {ok, Loaded} =
        load([Ctx], Opts),
    Full = hb_cache:ensure_all_loaded(Loaded, Opts),
    Public = hb_private:reset(Full),
    ?assertEqual(<<"2">>, maps:get(<<"y">>, Public)),
    ?assertEqual(BaseID, maps:get(<<"...">>, Public)),
    ?assertEqual({ok, <<"1">>}, hb_ao:resolve(Loaded, <<"x">>, Opts)),
    ?assertEqual(
        undefined,
        maps:get(<<"hashpath">>, hb_private:from_message(Loaded), undefined)
    ).

load_cached_replacement_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Replacement = #{ <<"y">> => <<"2">> },
    {ok, ReplacementID} = hb_cache:write(Replacement, Opts),
    Ctx = #{
        <<"base-id">> => <<"UnusedBase">>,
        <<"request-id">> => <<"Req">>,
        <<"normalizer">> => replace,
        <<"varied-result-id">> => ReplacementID
    },
    {ok, Loaded} =
        load([Ctx], Opts),
    ?assertEqual(Replacement, hb_private:reset(hb_cache:ensure_all_loaded(Loaded, Opts))),
    ?assertEqual(
        undefined,
        maps:get(<<"hashpath">>, hb_private:from_message(Loaded), undefined)
    ).

load_cached_base_only_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{ <<"x">> => <<"1">> },
    {ok, BaseID} = hb_cache:write(Base, Opts),
    Ctx = #{ <<"base-id">> => BaseID },
    {ok, Loaded} = load([Ctx], Opts),
    ?assertEqual(Base, hb_private:reset(hb_cache:ensure_all_loaded(Loaded, Opts))),
    ?assertEqual(BaseID, maps:get(<<"hashpath">>, hb_private:from_message(Loaded))).

load_direct_extension_references_base_id_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{ <<"x">> => <<"1">> },
    Patch = #{ <<"y">> => <<"2">> },
    BaseID = hb_message:id(Base, all, Opts),
    {ok, PatchID} = hb_cache:write(Patch, Opts),
    Ctx = #{
        <<"base">> => Base,
        <<"request-id">> => <<"Req">>,
        <<"normalizer">> => base,
        <<"varied-result-id">> => PatchID
    },
    {ok, Loaded} = load([Ctx], Opts),
    Public = hb_private:reset(hb_cache:ensure_all_loaded(Loaded, Opts)),
    ?assertEqual(BaseID, maps:get(<<"...">>, Public)),
    ?assertEqual({ok, <<"1">>}, hb_ao:resolve(Loaded, <<"x">>, Opts)).

load_cached_multisegment_sets_current_hashpath_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{ <<"x">> => <<"1">> },
    Patch1 = #{ <<"y">> => <<"2">> },
    Patch2 = #{ <<"z">> => <<"3">> },
    {ok, BaseID} = hb_cache:write(Base, Opts),
    {ok, Patch1ID} = hb_cache:write(Patch1, Opts),
    {ok, Patch2ID} = hb_cache:write(Patch2, Opts),
    Ctx1 = #{
        <<"base-id">> => BaseID,
        <<"request-id">> => <<"Req1">>,
        <<"normalizer">> => base,
        <<"varied-result-id">> => Patch1ID
    },
    Ctx2 = #{
        <<"request-id">> => <<"Req2">>,
        <<"normalizer">> => base,
        <<"varied-result-id">> => Patch2ID
    },
    Prefix = [Ctx1, Ctx2],
    {ok, Loaded} = load(Prefix, Opts),
    Full = hb_cache:ensure_all_loaded(Loaded, Opts),
    Public = hb_private:reset(Full),
    ?assertEqual(<<"3">>, maps:get(<<"z">>, Public)),
    ?assertEqual(format_weak([Ctx1], Opts), maps:get(<<"...">>, Public)),
    ?assertEqual({ok, <<"1">>}, hb_ao:resolve(Loaded, <<"x">>, Opts)),
    ?assertEqual(undefined, maps:get(<<"hashpath">>, hb_private:from_message(Loaded), undefined)),
    ?assertEqual(false, maps:is_key(<<"priv">>, Public)).

dev_math_chain_depends_are_in_hashpath_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{ <<"device">> => <<"math@1.0">>, <<"x">> => 1, <<"y">> => 2 },
    Req1 = #{ <<"path">> => <<"inc-x">> },
    Req2 = #{ <<"path">> => <<"add-x">>, <<"add">> => 5 },
    Req3 = #{ <<"path">> => <<"with-sum">> },
    {ok, Ctx1} = hb_ao:do(#{ <<"base">> => Base, <<"request">> => Req1, <<"opts">> => Opts }),
    Result1 = maps:get(<<"result">>, Ctx1),
    {ok, Ctx2} = hb_ao:do(#{ <<"base">> => Result1, <<"request">> => Req2, <<"opts">> => Opts }),
    Result2 = maps:get(<<"result">>, Ctx2),
    {ok, Ctx3} = hb_ao:do(#{ <<"base">> => Result2, <<"request">> => Req3, <<"opts">> => Opts }),
    Result3 = maps:get(<<"result">>, Ctx3),
    HP = maps:get(<<"hashpath">>, hb_private:from_message(Result3)),
    [Frame1, Frame2, Frame3] = parse(HP, Opts),
    ?assertEqual(
        hb_message:id(maps:get(<<"dependencies">>, Ctx1), #{ <<"committers">> => <<"none">>, <<"bundle">> => true }, Opts),
        maps:get(<<"dependencies-id">>, Frame1)
    ),
    ?assertMatch({ok, _}, hb_cache:read(maps:get(<<"dependencies-id">>, Frame1), Opts)),
    ?assertEqual(
        hb_message:id(maps:get(<<"dependencies">>, Ctx2), #{ <<"committers">> => <<"none">>, <<"bundle">> => true }, Opts),
        maps:get(<<"dependencies-id">>, Frame2)
    ),
    ?assertMatch({ok, _}, hb_cache:read(maps:get(<<"dependencies-id">>, Frame2), Opts)),
    ?assertEqual(
        hb_message:id(maps:get(<<"dependencies">>, Ctx3), #{ <<"committers">> => <<"none">>, <<"bundle">> => true }, Opts),
        maps:get(<<"dependencies-id">>, Frame3)
    ),
    ?assertMatch({ok, _}, hb_cache:read(maps:get(<<"dependencies-id">>, Frame3), Opts)),
    assert_dependency_keys(Ctx1, <<"base">>, [<<"device">>, <<"x">>]),
    assert_dependency_keys(Ctx1, <<"request">>, [<<"path">>]),
    assert_dependency_keys(Ctx2, <<"base">>, [<<"device">>, <<"x">>]),
    assert_dependency_keys(Ctx2, <<"request">>, [<<"add">>, <<"path">>]),
    assert_dependency_keys(Ctx3, <<"base">>, [<<"device">>, <<"x">>, <<"y">>]),
    assert_dependency_keys(Ctx3, <<"request">>, [<<"path">>]),
    ?assertEqual({ok, 7}, hb_ao:resolve(Result3, <<"x">>, Opts)),
    ?assertEqual({ok, 2}, hb_ao:resolve(Result3, <<"y">>, Opts)),
    ?assertEqual({ok, 9}, hb_ao:resolve(Result3, <<"sum">>, Opts)).

assert_dependency_keys(Ctx, Root, Keys) ->
    Deps = maps:get(Root, maps:get(<<"dependencies">>, Ctx)),
    ?assertEqual(lists:sort(Keys), lists:sort(maps:keys(Deps))).

verify_context_reexecutes_instead_of_trusting_cache_test() ->
    hb:init(),
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{ <<"device">> => <<"message@1.0">>, <<"x">> => 1 },
    Req = #{ <<"path">> => <<"set">>, <<"x">> => 2 },
    {ok, ExecCtx} =
        hb_ao:do(#{
            <<"base">> => Base,
            <<"request">> => Req,
            <<"opts">> => challenge_opts(Opts)
        }),
    WrongPatch = #{ <<"x">> => 99 },
    hb_cache_control:maybe_store(
        maps:get(<<"varied-base">>, ExecCtx),
        maps:get(<<"varied-request">>, ExecCtx),
        WrongPatch,
        Opts
    ),
    ?assertMatch(
        {true, #{ <<"x">> := 2 }},
        verify_context(#{ <<"base">> => Base, <<"request">> => Req }, Opts)
    ).

with_context_hashpath_no_store_skips_component_cache_test() ->
    hb:init(),
    Opts = challenge_opts(#{ <<"store">> => hb_test_utils:test_store() }),
    Base = #{ <<"x">> => 1 },
    Req = #{ <<"path">> => <<"set">>, <<"y">> => 2 },
    Patch = #{ <<"y">> => 2 },
    Deps = #{
        <<"base">> => #{ <<"x">> => <<(hb_message:id(Base, all, Opts))/binary, "/x">> },
        <<"request">> => #{ <<"path">> => <<(hb_message:id(Req, all, Opts))/binary, "/path">> }
    },
    Result = Patch#{ <<"...">> => Base },
    Ctx = #{
        <<"base">> => Base,
        <<"request">> => Req,
        <<"varied-base">> => Base,
        <<"varied-request">> => Req,
        <<"dependencies">> => Deps,
        <<"normalizer">> => base,
        <<"varied-result">> => Patch,
        <<"result">> => Result
    },
    Stamped = with_context_hashpath(Result, Ctx, Opts),
    ?assertMatch(#{ <<"hashpath">> := _ }, hb_private:from_message(Stamped)),
    ?assertEqual({error, not_found}, hb_cache:read(hb_message:id(Base, all, Opts), Opts)),
    ?assertEqual({error, not_found}, hb_cache:read(hb_message:id(Req, all, Opts), Opts)).

verify_all_executes_single_segment_assertion_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{ <<"x">> => <<"1">> },
    Req = #{ <<"path">> => <<"x">> },
    {ok, BaseID} = hb_cache:write(Base, Opts),
    {ok, ReqID} = hb_cache:write(Req, Opts),
    HP = format(#{ <<"base-id">> => BaseID, <<"request-id">> => ReqID }, Opts),
    ?assertMatch({true, _}, verify_part(HP, 1, Opts)),
    ?assertEqual(true, verify_all(HP, Opts)).
