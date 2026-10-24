%%% @doc Hashpaths: Succinct executable claims of AO-Core transitions: Their
%%% results and dependencies, as addressable protocol values.
%%%
%%% A hashpath is `ao:' followed by a path in the grammar of `hb_singleton'
%%% (see its module doc for the syntax in EBNF), whose first part is the base.
%%% `ao:<ID>' is the hashpath of one part: the message with the ID. Each
%%% request in the path may be followed by claims about the result of
%%% executing it on the state before it:
%%% <pre>
%%%     ;VB+VR      The IDs of the varied base and request: the inputs of the
%%%                 execution, as the spec of the executed function selects
%%%                 them.
%%%     @D          The ID of the dependencies: the message that gives the
%%%                 origin of each varied input.
%%%     .R          The ID of the result, which replaces the state.
%%%     *P          The ID of a patch, which extends the base: the result is
%%%                 the base with `P' laid over it.
%%% </pre>
%%%
%%% For example, `ao:BaseID/compute.R' claims that executing `compute' on
%%% the message `BaseID' gives the message `R'.
%%%
%%% A context gives each message that it addresses by its URI: the base and
%%% the request in `base-uri' and `request-uri', and each claim, such as
%%% `varied-result-uri', as the `ao:' URI of its ID.
%%%
%%% `hb_hashpath' writes hashpaths in a compact form, omitting the claims that
%%% can be derived: the varied pair and the dependencies of a replacement
%%% reached by the identity vary, and the result of a request whose result
%%% does not exist yet. A request that is only a key is written as that key,
%%% and any other request as the `ao:' URI of its ID. A base that is only a
%%% device is written as `~Device'. A request without a varied pair is not
%%% special: `HP/**P' is an ordinary claim that resolving the key `*' at `HP'
%%% extends its state with the patch `P'.
-module(hb_hashpath).
%%% Create and parse hashpaths.
-export([from/3, format/2, format_request/2, parse/2, context/2]).
-export([generate/8, attach/3, reset/1]).
%%% Load and execute transition contexts.
-export([load/2, load/3]).
%%% Verify hashpath claims.
-export([verify_all/2, verify_part/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Parse a singleton, a path or a hashpath with the grammar of
%% `hb_singleton'. With `requests' we return the messages to execute, and with
%% `contexts' the contexts of its steps, with their claims. Throws
%% `{invalid_singleton, Type, Offender}' as `hb_singleton' does.
-spec from(binary() | #{ _ => _ }, requests | contexts, #{ _ => _ }) -> [_].
from(Singleton, requests, Opts) -> hb_singleton:from(Singleton, Opts);
from(Singleton, contexts, Opts) -> hb_singleton:parse(Singleton, Opts).

%% @doc Encode a hashpath from an execution context or a sequence of contexts.
%%
%% The first stage of format extracts the first of two universal hashpath
%% elements -- the `Base` ID or existing hashpath. We then recurse with this
%% value and the remaining context.
format([], _Opts) -> <<>>;
format([First | Rest], Opts) ->
    lists:foldl(
        fun(Ctx, Prior) -> format(Ctx#{ <<"base-uri">> => Prior }, Opts) end,
        format(First, Opts),
        Rest
    );
format(Ctx = #{ <<"varied-result">> := Result }, Opts)
        when not is_map(Result), not is_binary(Result), not is_list(Result),
             not ?IS_LINK(Result) ->
    % A native literal has no standalone message ID. Its executable compact
    % address gives the value without mistaking it for a pre-result context.
    format(
        maps:with(
            [<<"base">>, <<"base-uri">>, <<"request">>, <<"request-uri">>],
            Ctx
        ),
        Opts
    );
format(Ctx, Opts) ->
    maybe
        {ok, Base} ?= format_base(Ctx, Opts),
        BasePart =
            case Base of
                <<"ao:", _/binary>> -> Base;
                _ -> <<"ao:", Base/binary>>
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
%% falling back to the URI of the base if known, and recomputing it only if
%% necessary. If the execution input of the base is only a device, we write it
%% as `~Device', as we do for a request. A base that is the `ao:' URI of a key
%% with a device is written as `Key~Device'.
format_base(#{ <<"base-uri">> := URI }, _) -> {ok, URI};
format_base(#{ <<"base">> := Base = {as, _, Msg} }, _) when map_size(Msg) == 0 ->
    hb_singleton:to_part(Base);
format_base(Ctx = #{ <<"base">> := {as, Device, <<"ao:", Key/binary>>} }, Opts) ->
    case hb_singleton:to_part({as, Device, #{ <<"path">> => Key }}) of
        {ok, Part} -> {ok, Part};
        error -> find_uri(<<"base">>, Ctx, Opts)
    end;
format_base(Ctx = #{ <<"base">> := Base }, Opts) ->
    case hb_private:from_message(Base) of
        #{ <<"hashpath">> := HP, <<"hashpath-result">> := ID } ->
            case hb_message:id(Base, all, Opts) of
                ID -> {ok, HP};
                _ -> find_uri(<<"base">>, Ctx, Opts)
            end;
        _ ->
            case hb_private:reset(hb_ao:execution_input(Base, Opts)) of
                Input = #{ <<"device">> := _ } when map_size(Input) == 1 ->
                    case hb_singleton:to_part(Input) of
                        {ok, Part} -> {ok, Part};
                        error -> find_uri(<<"base">>, Ctx, Opts)
                    end;
                _ -> find_uri(<<"base">>, Ctx, Opts)
            end
    end;
format_base(Ctx, Opts) ->
    find_uri(<<"base">>, Ctx, Opts).

%% @doc General utility for extracting the URI of a message by its name from a
%% context if it is already known, recomputing its ID only if necessary.
find_uri(Name, Ctx, _Opts) when is_map_key(<<Name/binary, "-uri">>, Ctx) ->
    {ok, maps:get(<<Name/binary, "-uri">>, Ctx)};
find_uri(Name, Ctx, Opts) when is_map_key(Name, Ctx) ->
    case hb_cache:ensure_loaded(maps:get(Name, Ctx), Opts) of
        Value when is_map(Value); is_binary(Value); is_list(Value) ->
            {ok, <<"ao:", (hb_message:id(Value, all, Opts))/binary>>};
        _ -> {not_found, Name}
    end;
find_uri(Name, _Ctx, _Opts) ->
    {not_found, Name}.

%% @doc A request consisting only of a self-describing key can omit its ID.
format_request(Req, Opts) ->
    format_request_context(#{ <<"request">> => Req }, Opts).

%% @doc Use the request URI when supplied by a parsed receipt. A request that
%% has a smallest form (see `hb_singleton:to_part/1') is written in it, and
%% any other request as its URI. A request URI that is a hashpath is formatted
%% with `to_part/1', so `ao:B/k.R' is the group `(ao:B/k.R)'.
format_request_context(#{ <<"request-uri">> := URI }, _Opts)
        when ?IS_HASHPATH(URI) ->
    hb_singleton:to_part(URI);
format_request_context(#{ <<"request-uri">> := URI }, _Opts) -> {ok, URI};
format_request_context(Ctx = #{ <<"request">> := Req }, Opts) ->
    Input =
        case Req of
            {as, _, _} -> Req;
            _ -> hb_private:reset(hb_ao:execution_input(Req, Opts))
        end,
    case hb_singleton:to_part(Input) of
        {ok, Part} -> {ok, Part};
        error -> find_uri(<<"request">>, Ctx, Opts)
    end;
format_request_context(Ctx, Opts) -> find_uri(<<"request">>, Ctx, Opts).

%% @doc Format the varied base and requests, if given, into their hashpath
%% components.
format_varied(Ctx, Opts) ->
    maybe
        {ok, <<"ao:", VBase/binary>>} ?= find_uri(<<"varied-base">>, Ctx, Opts),
        {ok, <<"ao:", VReq/binary>>} ?= find_uri(<<"varied-request">>, Ctx, Opts),
        <<";", VBase/binary, "+", VReq/binary>>
    else
        {not_found, _} ->
            % Either the base or request is not found, so we omit the varied
            % component of the hashpath.
            <<>>
    end.

%% @doc If the dependencies of a resolution are known, format them into the
%% hashpath depends component. If not, return an empty string. Honors an
%% already calculated dependency URI if provided in the context.
format_dependencies(Ctx, Opts) ->
    case find_uri(<<"dependencies">>, Ctx, Opts) of
        {ok, <<"ao:", Depends/binary>>} -> <<"@", Depends/binary>>;
        {not_found, _} -> <<>>
    end.

%% @doc If the result of the execution has already been calculated, format it
%% into the hashpath equivalence component. If not, return an empty string.
format_equivalence(RawCtx, Opts) ->
    Ctx = equivalence_context(RawCtx, Opts),
    case find_uri(<<"varied-result">>, Ctx, Opts) of
        {ok, <<"ao:", Result/binary>>} ->
            <<(format_normalizer(Ctx, Opts))/binary, Result/binary>>;
        {not_found, _} -> <<>>
    end.

%% @doc Format the normalizer component of the hashpath.
format_normalizer(#{ <<"normalizer">> := base }, _Opts) -> <<"*">>;
format_normalizer(_, _) -> <<".">>.

%% @doc A request overlay replaces the base with the fully overlaid request.
%% Its receipt therefore states that replacement, not the generic patch.
equivalence_context(
    Ctx = #{ <<"normalizer">> := request, <<"result">> := Result },
    _Opts
) ->
    maps:remove(<<"varied-result-uri">>,
        Ctx#{ <<"normalizer">> => replace, <<"varied-result">> => Result });
equivalence_context(Ctx, _) -> Ctx.

%% @doc Decode a hashpath into a list of context segments, with the grammar
%% of `hb_singleton'. The first segment will have both a base and a request
%% part, while the latter segments will only have the request part -- the base
%% being inferred from the result of the prior segments.
parse(<<"ao:", _/binary>> = Hashpath, Opts) ->
    try hb_singleton:parse(Hashpath, Opts) of
        [] -> {error, <<"Empty hashpath.">>};
        Parts -> Parts
    catch throw:{invalid_singleton, _Type, Offender} -> {error, Offender}
    end;
parse(_Hashpath, _Opts) -> {error, <<"Invalid hashpath scheme.">>}.

%% @doc Parse the last segment of a hashpath into an executable context that
%% can be additionally executed upon.
context(Hashpath, Opts) ->
    case parse(Hashpath, Opts) of
        [Only] -> Only;
        Parts when is_list(Parts) ->
            (lists:last(Parts))#{
                <<"base-uri">> => format(lists:droplast(Parts), Opts)
            };
        Error -> Error
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
            verify_all(
                attach(ComputedState, format(Ctx, Opts), Opts),
                Rest,
                Opts
            );
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
%% `Request` fields, then utilize `hb_ao:resolve` to re-execute the context.
%% Assuming successful computation, we then verify the `VariedBase` and
%% `VariedRequest` fields against the parsed context, the `DependenciesID` if
%% given, the `Normalizer` type, and finally the `Result` message itself. If all
%% of these verify, the context is considered valid.
verify_context(#{ <<"base">> := Base } = Ctx, Opts)
        when not is_map_key(<<"request">>, Ctx),
             not is_map_key(<<"request-uri">>, Ctx) ->
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
        {true, maps:get(<<"result">>, ExecutedCtx)}
    else
        _Error ->
            ?event_debug(
                hashpath_debug,
                {hashpath_verify_context_failed, {error, _Error}, {ctx, Ctx}},
                Opts
            ),
            false
    end.

%% @doc If varied `Req` and `Base` statements were present in the hashpath,
%% we verify that they match the executed context.
verify_varied(HPCtx, ExecutedCtx, Opts) ->
    verify_id(<<"varied-base">>, HPCtx, ExecutedCtx, Opts) andalso
        verify_id(<<"varied-request">>, HPCtx, ExecutedCtx, Opts).

%% @doc Check each dependency against the input from which Vary selected it.
%% Coercion is determined by the selected device and checked by `verify_varied`.
verify_dependencies(HPCtx, ExecutedCtx, Opts) ->
    case find_uri(<<"dependencies">>, HPCtx, Opts) of
        {not_found, _} -> true;
        {ok, URI} ->
            DependencyMsg =
                case find_uri(<<"dependencies">>, ExecutedCtx, Opts) of
                    {ok, URI} ->
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
                            maps:get(
                                <<"varied-", Name/binary>>,
                                ExecutedCtx,
                                maps:get(Name, ExecutedCtx)
                            ),
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
    {link, Origin, #{}}.

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
            lists:sort(maps:keys(Deps)) =:= lists:sort(maps:keys(Values))
                andalso verify_commitments(Original, Opts)
                andalso lists:all(
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
    case find_uri(<<"varied-result">>, HPCtx, Opts) of
        {not_found, _} -> true;
        {ok, _} ->
            maps:get(<<"normalizer">>, HPCtx, replace) =:=
                maps:get(<<"normalizer">>, ExecutedCtx, replace) andalso
                verify_id(<<"varied-result">>, HPCtx, ExecutedCtx, Opts)
    end.

%% @doc An omitted claim is compact; a stated claim must be reproduced.
verify_id(Name, Claim, Executed, Opts) ->
    case find_uri(Name, Claim, Opts) of
        {not_found, _} -> true;
        {ok, URI} -> find_uri(Name, Executed, Opts) =:= {ok, URI}
    end.

%% @doc Re-execute a claim through the resolver without trusting cached results.
%% Every message layer of the base and request must verify: the identity vary
%% gives no dependency leaves through which a forged layer would be found.
execute(Ctx, Opts) ->
    maybe
        {ok, Base} ?= load_field(<<"base">>, Ctx, Opts),
        {ok, Req} ?= load_field(<<"request">>, Ctx, Opts),
        true ?= verify_all_commitments(Base, Opts),
        true ?= verify_all_commitments(Req, Opts),
        % A transition is one key. A request message whose path has several
        % parts is refused, as `resolve/3' would apply them in turn.
        true =
            not is_map(Req) orelse
                length(hb_path:from_message(request, Req, Opts)) == 1,
        resolve_context(Base, Req, internal_opts(Opts))
    end.

%% @doc Resolve a request on a base and return the context of its execution.
%% For a request that is only a device, `hb_ao' sets the device on the base
%% and returns that message without a context, so the context is built here
%% with the message as its result.
resolve_context(Base, Req = {as, _, Msg}, ExecOpts) when map_size(Msg) == 0 ->
    maybe
        {ok, Res} ?= hb_ao:resolve(Base, Req, ExecOpts),
        {ok,
            #{
                <<"base">> => Base,
                <<"request">> => Req,
                <<"result">> => Res,
                <<"varied-result">> => Res
            }
        }
    end;
resolve_context(Base, Req, ExecOpts) ->
    hb_ao:resolve(Base, Req, ExecOpts#{ <<"return-context">> => true }).

%% @doc Load the result of a hashpath, or of a given part number within it, by
%% resolving the request of each part on its base. The claims of the hashpath
%% are ignored: the result has the hashpath of this node's resolution. A link
%% to a hashpath loads the hashpath.
load({link, Hashpath, LinkOpts}, Opts) ->
    MergedOpts = hb_store:scope(
        hb_util:deep_merge(Opts, LinkOpts, Opts),
        hb_opts:get(scope, local, LinkOpts)
    ),
    load(Hashpath, MergedOpts);
load(Hashpath, Opts) when is_binary(Hashpath) ->
    load(parse(Hashpath, Opts), Opts);
load([Ctx = #{ <<"base-uri">> := URI }], Opts) when map_size(Ctx) == 1 ->
    % A hashpath of one part has no claims: it is the message stored at its
    % URI.
    read(URI, Opts);
load(Parts, Opts) when is_list(Parts) ->
    load(Parts, length(Parts), Opts);
load({error, _} = Error, _Opts) -> Error.
load(Hashpath, PartNum, Opts) when is_binary(Hashpath) ->
    load(parse(Hashpath, Opts), PartNum, Opts);
load(Parts, PartNum, Opts)
        when is_list(Parts), is_integer(PartNum),
             PartNum > 0, PartNum =< length(Parts) ->
    load_result(Parts, PartNum, lists:nth(PartNum, Parts), Opts);
load(_Parts, _PartNum, _Opts) ->
    {error, <<"Hashpath part number out of bounds.">>}.

%% @doc Resolve the request of a part on its base with `hb_ao:resolve/3'. The
%% resolver returns the cached result if the node has one, and executes the
%% request if not. A part without a request gives its base.
load_result(Parts, PartNum, Ctx, Opts) ->
    maybe
        {ok, Base} ?= load_base(Parts, PartNum, Opts),
        case maps:is_key(<<"request">>, Ctx) orelse
                maps:is_key(<<"request-uri">>, Ctx) of
            false -> {ok, Base};
            true ->
                maybe
                    {ok, Req} ?= load_field(<<"request">>, Ctx, Opts),
                    hb_ao:resolve(Base, Req, Opts#{ <<"hashpath">> => update })
                end
        end
    end.

%% @doc Load the base of a part: the base of the first part, or the result of
%% the part before it.
load_base([First | _], 1, Opts) -> load_field(<<"base">>, First, Opts);
load_base(Parts, PartNum, Opts) -> load(Parts, PartNum - 1, Opts).

%% @doc Read an addressed context value, or use its supplied in-memory value.
%% A base that is an `ao:' URI with a device is read and checked as a
%% `base-uri' is, and the device is set on the message as `hb_ao' sets it.
load_field(
    <<"base">>,
    #{ <<"base">> := {as, Device, URI = <<"ao:", _/binary>>} },
    Opts
) ->
    maybe
        {ok, Base} ?= load_field(<<"base">>, #{ <<"base-uri">> => URI }, Opts),
        {ok, hb_ao:set(Base, <<"device">>, Device, internal_opts(Opts))}
    end;
load_field(Name, Ctx, Opts) when is_map_key(Name, Ctx) ->
    Value = hb_cache:ensure_loaded(maps:get(Name, Ctx), Opts),
    case maps:find(<<Name/binary, "-uri">>, Ctx) of
        error -> {ok, Value};
        {ok, URI} -> check(Value, URI, Opts)
    end;
load_field(Name, Ctx, Opts) ->
    case maps:find(<<Name/binary, "-uri">>, Ctx) of
        {ok, URI} ->
            % A stored value must reproduce its address before it can be a
            % witness.
            maybe
                {ok, Value} ?= read(URI, Opts),
                check(Value, URI, Opts)
            end;
        error -> {error, {missing, Name}}
    end.

%% @doc Read the value at a URI. The store contains the value of an `ao:' URI
%% under its ID.
read(<<"ao:", ID/binary>>, Opts) ->
    case hb_cache:read(ID, Opts) of
        {error, not_found} -> hb_cache:read(?NI_URI(ID), Opts);
        Result -> Result
    end;
read(URI, Opts) -> hb_cache:read(URI, Opts).

%% @doc Check the claimed content address and its attached commitments.
check(Value, URI, Opts) ->
    maybe
        true ?= <<"ao:", (hb_message:id(Value, all, Opts))/binary>> =:= URI
            orelse {error, <<"Hashpath value does not match its ID.">>},
        true ?= verify_commitments(Value, Opts)
            orelse {error, <<"Hashpath value has invalid commitments.">>},
        {ok, Value}
    end.

%% @doc Check the commitments of every message layer of a value. A challenge
%% loads the value in full first: paranoid verification checks the messages
%% it is given and does not follow links, and a forged layer may be stored
%% behind one.
verify_all_commitments(Value, Opts) ->
    try
        hb_message:paranoid_verify(
            hb_cache:ensure_all_loaded(Value, Opts),
            Opts#{ <<"paranoid-verify">> => true }
        )
    catch throw:{paranoid_verification_failure, _, _, _, _} -> false
    end.

%% @doc Check commitments once the value's message layer is loaded.
verify_commitments(Value, Opts) when ?IS_LINK(Value) ->
    verify_commitments(hb_cache:ensure_loaded(Value, Opts), Opts);
verify_commitments(Value, Opts) when is_map(Value) ->
    hb_message:verify(Value, #{ <<"ids">> => <<"all">> }, Opts);
verify_commitments(_Value, _Opts) -> true.

%% @doc Generate the hashpath of a resolution, and return it with its context,
%% which `verify_context/2' compares with the claims of a hashpath. The inputs
%% are the original and the varied base and request, the varied result, and
%% the result with its overlay applied. `store/2' may write the base, the
%% request and the dependencies message to the cache.
generate(RawBase, RawReq, Res, VariedBase, VariedReq, VariedRes, Overlay, Opts) ->
    % `original_witness/3' gives the base and the request of the hashpath. An
    % unsigned one gets a commitment over all of its fields, so that its ID
    % covers the fields that Vary selects.
    Base = original_witness(RawBase, VariedBase, Opts),
    Req = original_witness(RawReq, VariedReq, Opts),
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
        <<"varied-result">> => VariedRes,
        <<"normalizer">> => Normalizer
    },
    % A `.R' result, where Vary changed neither the base nor the request, gets
    % no `;VB+VR' or `@D' claim: the varied base and request are the base and
    % the request, and the dependencies follow from them. Any other result,
    % such as a patch, gets the varied base and request and their dependencies.
    Completed =
        case Normalizer == replace andalso
                hb_message:id(VariedBase, all, Opts) =:=
                    hb_message:id(Base, all, Opts) andalso
                hb_message:id(VariedReq, all, Opts) =:=
                    hb_message:id(Req, all, Opts) of
            true -> Ctx;
            false ->
                Witnessed =
                    Ctx#{
                        <<"varied-base">> => VariedBase,
                        <<"varied-request">> => VariedReq
                    },
                Witnessed#{
                    <<"dependencies">> => dependencies(Witnessed, Opts)
                }
        end,
    HP = format(Completed, Opts),
    store(Completed, Opts),
    {HP, Completed}.

%% @doc Reuse the varied identity when Vary only added unsigned commitments.
%% A distinct original witness must name all its unsigned fields as well.
original_witness(Original, Varied, Opts) ->
    case hb_ao:execution_input(Original, Opts) =:=
            hb_ao:execution_input(Varied, Opts) of
        true -> Varied;
        false -> hb_ao:normalize_input(Original, Opts)
    end.

%% @doc Write the base, the request and the dependencies message of a
%% resolution to the cache, if `hb_cache_control:derive_cache_settings/2' gives
%% `store' for the varied result and the request. The resolver makes the same
%% check before it stores the result. A user with the hashpath can get the
%% dependencies message from the node by the ID in its `@D' claim.
store(Ctx, Opts) ->
    case
        hb_cache_control:derive_cache_settings(
            [
                maps:get(<<"varied-result">>, Ctx),
                maps:get(<<"request">>, Ctx)
            ],
            Opts
        )
    of
        #{ <<"store">> := true } ->
            % The dependencies message links to the base and the request, so
            % it is written after them.
            lists:foreach(
                fun(Name) -> hb_cache:write(maps:get(Name, Ctx), Opts) end,
                [<<"base">>, <<"request">>] ++
                    [
                        <<"dependencies">>
                    ||
                        is_map_key(<<"dependencies">>, Ctx)
                    ]
            );
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

%% @doc Keep a receipt only while it still gives the ID of the returned state.
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
            "ao:BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4"
            "/ao:tYRVDkT2X7wjYYVaZWuBBWWzZatEsMoR2NBjcJ8CmZk"
            ";a2Fub25pY2FsLXZhcmllZC1iYXNlLWlkLTAwMDAwMDA"
            "+a2Fub25pY2FsLXZhcmllZC1yZXEtaWQtMDAwMDAwMDA"
            "@ZGVwZW5kcy1tZXNzYWdlLWlkLTAwMDAwMDAwMDAwMDA"
            "*cGF0Y2gtaWQtMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDA"
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
            <<"ao:BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4">>,
            <<"ao:BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4/balance">>,
            % A request that is only a key is written as that key, at any
            % length.
            <<"ao:BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4"
              "/tYRVDkT2X7wjYYVaZWuBBWWzZatEsMoR2NBjcJ8CmZk">>,
            <<"ao:BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4"
              "/transfer/balance">>,
            <<"ao:BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4/*"
              "*cGF0Y2gtaWQtMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDA">>,
            % A base that is only a device is written as `~Device'.
            <<"ao:~test-device@1.0/compute">>,
            % A request that contains only the value of its key is written as
            % `key=value'.
            <<"ao:BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4/k=v">>,
            % A request that is only a device is written as `~Device'.
            <<"ao:BQQF7TjcHTPT57eIcABDeIbfHkkOTDPKAQ9tJqScTV4"
              "/~test-device@1.0/compute">>
        ]
    ),
    % A request that is a hashpath of several parts is formatted as a group,
    % and a URI with a device as it is; each form then formats as itself.
    R = <<"cGF0Y2gtaWQtMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDA">>,
    lists:foreach(
        fun({HP, Form}) ->
            ?assertEqual(
                [Form, Form],
                [ format(parse(In, Opts), Opts) || In <- [HP, Form] ]
            )
        end,
        [
            {
                <<"ao:a/ao:b%2Fk%2E", R/binary>>,
                <<"ao:a/(ao:b/k.", R/binary, ")">>
            },
            {<<"ao:a/ao:b%2Fk">>, <<"ao:a/(ao:b/k)">>},
            {<<"ao:a/ao:b~d@1.0">>, <<"ao:a/ao:b~d@1.0">>},
            {<<"ao:a/(ao:b~d@1.0)">>, <<"ao:a/(ao:b~d@1.0)">>}
        ]
    ),
    % A resolved base that is only a device is written as `~Device'.
    StoreOpts = Opts#{ <<"store">> => hb_test_utils:test_store() },
    {ok, Info} =
        hb_ao:resolve(
            #{ <<"device">> => <<"test-device@1.0">> },
            <<"info">>,
            StoreOpts
        ),
    ?assertMatch(
        <<"ao:~test-device@1.0/info", _/binary>>,
        hb_path:hashpath(Info, StoreOpts)
    ).

%% @doc Quotes and parentheses in keys do not hide receipt delimiters.
literal_key_receipt_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>]
    },
    lists:foreach(
        fun(Key) ->
            Base = #{ Key => #{ <<"value">> => 7 } },
            Path = hb_escape:encode(Key),
            {ok, Result} = hb_ao:resolve(Base, #{ <<"path">> => Path }, Opts),
            HP = hb_path:hashpath(Result, Opts),
            ?assertEqual(
                #{ <<"path">> => Path },
                maps:get(<<"request">>, context(HP, Opts))
            ),
            ?assert(verify_all(HP, Opts)),
            {ok, Loaded} = load(HP, Opts),
            ?assertEqual(7, hb_maps:get(<<"value">>, Loaded, undefined, Opts))
        end,
        [<<"(">>, <<"\"">>]
    ).

%% @doc Reads preserve errors; forcing a failed load raises its reason.
failed_load_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    {ok, ID} =
        hb_cache:write(
            #{ <<"device">> => <<"test-device@1.0">>, <<"value">> => 7 },
            Opts
        ),
    WrongID = hb_message:id(#{ <<"wrong-result">> => 42 }, all, Opts),
    lists:foreach(
        fun({Part, Reason}) ->
            HP = <<"ao:", ID/binary, "/", Part/binary>>,
            ?assertEqual({error, Reason}, load(HP, Opts)),
            ?assertEqual({error, Reason}, hb_cache:read(HP, Opts)),
            ?assertNot(verify_all(HP, Opts)),
            ?assertNot(verify_part(HP, 1, Opts)),
            lists:foreach(
                fun(LinkOpts) ->
                    Link = {link, HP, LinkOpts},
                    case Reason of
                        not_found ->
                            ?assertThrow({necessary_message_not_found, _, _},
                                hb_cache:ensure_loaded(Link, Opts));
                        _ ->
                            ?assertError(Reason,
                                hb_cache:ensure_loaded(Link, Opts)),
                            ?assertError(Reason,
                                hb_ao:resolve(Link, <<"value">>, Opts))
                    end
                end,
                [#{}]
            )
        end,
        [
            {<<"missing">>, not_found},
            {<<"restore">>, <<"No viable state to restore.">>}
        ]
    ),
    % A load ignores a false claim, in a group too; `verify_all/2' refuses it.
    Claimed = <<"ao:", ID/binary, "/value.", WrongID/binary>>,
    ?assertEqual(
        [{ok, 7}, {ok, 7}, {ok, 7}, false],
        [
            load(Claimed, Opts),
            hb_cache:read(Claimed, Opts),
            hb_ao:resolve_many(
                hb_singleton:from(<<"/(", Claimed/binary, ")">>, Opts),
                Opts
            ),
            verify_all(Claimed, Opts)
        ]
    ).

%% @doc Reject texts that are not hashpaths, and paths that do not follow the
%% grammar.
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
            <<"ao:">>,
            <<"ao:", ID/binary, "/(key">>,
            <<"ao:", ID/binary, "/key)">>,
            <<"ao:", ID/binary, "/\"key">>,
            <<"ao:", ID/binary, "/key~">>,
            <<"ao:", ID/binary, "/key&=value">>,
            <<"ao:", ID/binary, ".", ID/binary>>
        ]
    ).

%% @doc Supplied context values cannot override their claimed addresses.
supplied_context_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base = #{ <<"a">> => 1, <<"b">> => 2 },
    ?assertEqual(
        {ok, 1},
        load(
            [
                #{
                    <<"base">> => Base,
                    <<"request">> => #{ <<"path">> => <<"a">> }
                }
            ],
            Opts
        )
    ),
    {ok, ID} = hb_cache:write(Base, Opts),
    URI = <<"ao:", ID/binary>>,
    WrongBase = [#{ <<"base-uri">> => URI, <<"base">> => #{ <<"a">> => 3 } }],
    ?assertNot(verify_all(WrongBase, Opts)),
    ?assertMatch({error, _}, load(WrongBase, Opts)),
    {ok, KeyURI} = hb_uri:to(#{ <<"path">> => <<"a">> }, #{}, Opts),
    ?assertNot(
        verify_all(
            [
                #{
                    <<"base-uri">> => URI,
                    <<"request-uri">> => KeyURI,
                    <<"request">> => #{ <<"path">> => <<"b">> }
                }
            ],
            Opts
        )
    ).

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
    ?assertEqual(hb_uri:to(Base, #{}, Opts),
        {ok, maps:get(<<"base-uri">>, Ctx1)}),
    ?assertNotEqual(HP1, HP2),
    ?assertEqual(maps:get(<<"varied-base-uri">>, Ctx1),
        maps:get(<<"varied-base-uri">>, Ctx2)),
    ?assertEqual(maps:get(<<"varied-result-uri">>, Ctx1),
        maps:get(<<"varied-result-uri">>, Ctx2)),
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
    ?assertEqual(HP2, maps:get(<<"base-uri">>, context(HP3, Opts))),
    ?assert(verify_all(HP3, Opts)),
    ?assert(verify_part(HP3, 2, Opts)),
    {ok, Reloaded} = load(HP3, Opts),
    ?assertEqual(3, hb_maps:get(<<"counter">>, Reloaded, undefined, Opts)),
    ?assertEqual(
        <<"second">>,
        hb_maps:get(<<"other">>, Reloaded, undefined, Opts)
    ),
    Linked = hb_cache:ensure_loaded({link, HP3, #{}}, Opts),
    ?assertEqual(3, hb_maps:get(<<"counter">>, Linked, undefined, Opts)),
    ?assertNot(
        verify_all(format(Ctx2#{ <<"normalizer">> => replace }, Opts), Opts)
    ),
    ?assertNot(
        verify_all(
            format(
                Ctx2#{
                    <<"varied-result-uri">> =>
                        maps:get(<<"varied-base-uri">>, Ctx2)
                },
                Opts
            ),
            Opts
        )
    ),
    % Port just this transition's witnesses to an independent store.
    PortableOpts = #{ <<"store">> => hb_test_utils:test_store() },
    {ok, Portable} = hb_ao:resolve(OtherBase, Req,
        Opts#{ <<"return-context">> => true }),
    lists:foreach(
        fun(Name) -> hb_cache:write(maps:get(Name, Portable), PortableOpts) end,
        [<<"base">>, <<"request">>, <<"dependencies">>, <<"varied-result">>]
    ),
    {ok, Ported} = load(HP2, PortableOpts),
    ?assertEqual(
        2,
        hb_maps:get(<<"counter">>, Ported, undefined, PortableOpts)
    ),
    ?assert(verify_all(HP2, PortableOpts)),
    {ok, Continued} = hb_ao:resolve(Ported, Req, PortableOpts),
    ?assertEqual(
        3,
        hb_maps:get(<<"counter">>, Continued, undefined, PortableOpts)
    ).

%% @doc A replacement loads by resolving its request on its base, not from its
%% stored result; changing a base after resolution must not re-use its old
%% receipt.
replacement_receipt_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>] },
    Base = #{ <<"child">> => #{ <<"value">> => <<"replacement">> } },
    Req = #{ <<"path">> => <<"child">> },
    {ok, Result} = hb_ao:resolve(Base, Req, Opts),
    HP = hb_path:hashpath(Result, Opts),
    hb_cache:write(Result, Opts),
    ?assertEqual({error, not_found}, load(HP, Opts)),
    hb_cache:write(Base, Opts),
    {ok, Loaded} = load(HP, Opts),
    ?assertEqual(<<"replacement">>,
        hb_maps:get(<<"value">>, Loaded, undefined, Opts)),
    Changed = hb_ao:set(Loaded, <<"another">>, #{ <<"value">> => 2 }, Opts),
    {ok, Next} = hb_ao:resolve(Changed, <<"another">>, Opts),
    NextCtx = context(hb_path:hashpath(Next, Opts), Opts),
    ?assertEqual(hb_uri:to(Changed, #{}, Opts),
        {ok, maps:get(<<"base-uri">>, NextCtx)}),
    ?assertNot(verify_part(HP, 0, Opts)),
    ?assertNot(verify_part(HP, 2, Opts)),
    ?assertMatch({error, _}, load(HP, 0, Opts)).

%% @doc Hashpath verification, and a load that executes upon the base, must not
%% trust a store alias as proof of content.
wrong_content_address_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Claimed = hb_message:id(#{ <<"value">> => <<"claimed">> }, all, Opts),
    {ok, Actual} = hb_cache:write(#{ <<"value">> => <<"actual">> }, Opts),
    hb_cache:link(Actual, Claimed, Opts),
    ?assertNot(verify_all(<<"ao:", Claimed/binary>>, Opts)),
    HP = <<"ao:", Claimed/binary, "/value">>,
    ?assertMatch({error, _}, load(HP, Opts)),
    ?assertMatch({error, _}, load({link, HP, #{}}, Opts)),
    % A device as a request or on the base is set on a base that matches its ID.
    ?assertMatch(
        [{ok, <<"actual">>}, {ok, <<"actual">>}, {error, _}],
        [
            load(<<"ao:", Actual/binary, "/~message@1.0/value">>, Opts),
            load(<<"ao:", Actual/binary, "~message@1.0/value">>, Opts),
            load(<<"ao:", Claimed/binary, "~message@1.0/value">>, Opts)
        ]
    ).

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
    % The node stores the dependencies message of the `@D' claim.
    {ok, Deps} = load_field(<<"dependencies">>, Ctx, Opts),
    BaseID = hb_message:id(Base, all, Opts),
    ReqID = hb_message:id(Req, all, Opts),
    BaseHP = <<"ao:", BaseID/binary>>,
    ReqHP = <<"ao:", ReqID/binary>>,
    ExpectedDeps = #{
        <<"base">> => #{
            <<"device">> => BaseHP,
            <<"required">> => BaseHP,
            <<"deep">> => #{ <<"slot">> => BaseHP }
        },
        <<"request">> => #{
            <<"path">> => ReqHP,
            <<"deep-request">> => #{
                <<"slot">> => ReqHP
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
            <<"required">> => <<BaseHP/binary, "/omitted">>
        }
    },
    {ok, WrongID} = hb_cache:write(WrongDeps, Opts),
    WrongURI = <<"ao:", WrongID/binary>>,
    ?assertNot(verify_all(
        format(Ctx#{ <<"dependencies-uri">> => WrongURI }, Opts), Opts)),
    ?assertNot(verify_all(
        format(Ctx#{ <<"varied-base-uri">> => BaseHP }, Opts), Opts)),
    {ok, CollapsedID} = hb_cache:write(
        #{ <<"base">> => BaseHP, <<"request">> => ReqHP }, Opts),
    CollapsedURI = <<"ao:", CollapsedID/binary>>,
    ?assertNot(verify_all(
        format(Ctx#{ <<"dependencies-uri">> => CollapsedURI }, Opts), Opts)).

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
    ?assertEqual(
        hb_message:signers(Signed, Opts),
        hb_message:signers(
            hb_maps:get(<<"base">>, Loaded, undefined, Opts),
            Opts
        )
    ),
    OtherSigned = hb_message:commit(Base,
        Opts#{ <<"priv-wallet">> => ar_wallet:new() }),
    ?assertMatch({error, #{ <<"status">> := 504 }},
        hb_ao:resolve(OtherSigned, <<"vary-unspecified">>,
            Opts#{ <<"cache-control">> => [<<"only-if-cached">>] })),
    Forged = Signed#{ <<"child">> => #{ <<"value">> => <<"forged">> } },
    ID = hb_message:id(Signed, all, Opts),
    ?assertEqual(ID, hb_message:id(Forged, all, Opts)),
    ?assertNot(verify_all(
        [#{ <<"base-uri">> => <<"ao:", ID/binary>>, <<"base">> => Forged }],
        Opts
    )),
    % The forged messages test this module's own checks, so the cache is not
    % asked to verify them.
    ForgedOpts = Opts#{
        <<"store">> => hb_test_utils:test_store(),
        <<"paranoid-verify">> => false
    },
    hb_cache:write(Forged, ForgedOpts),
    ?assertNot(verify_all(<<"ao:", ID/binary>>, ForgedOpts)),
    ?assertMatch({error, _}, load(<<"ao:", ID/binary>>, ForgedOpts)),
    Child = hb_message:commit(#{ <<"value">> => <<"signed child">> }, Opts),
    ForgedChild = Child#{ <<"value">> => <<"forged child">> },
    lists:foreach(
        fun({OriginalValue, ForgedValue}) ->
            Parent =
                hb_message:commit(
                    Base#{ <<"child">> => OriginalValue },
                    Opts
                ),
            ForgedParent = Parent#{ <<"child">> => ForgedValue },
            ?assert(hb_message:verify(ForgedParent, all, ForgedOpts)),
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
    ?assertEqual(
        Req#{ <<"counter">> => 2 },
        hb_message:uncommitted(hb_private:reset(Result), Opts)
    ),
    HP = hb_path:hashpath(Result, Opts),
    Ctx = context(HP, Opts),
    ?assertEqual(replace, maps:get(<<"normalizer">>, Ctx, replace)),
    ?assertEqual(hb_uri:to(Result, #{}, Opts),
        {ok, maps:get(<<"varied-result-uri">>, Ctx)}),
    {ok, Loaded} = load(HP, Opts),
    ?assertEqual(hb_private:reset(Result),
        hb_private:reset(hb_cache:ensure_all_loaded(Loaded, Opts))),
    ?assert(verify_all(HP, Opts)).

%% @doc The hashpath of a patch without its result claim loads by resolving its
%% request, and gives the patched result with the hashpath of the resolution.
pre_result_context_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{ <<"device">> => <<"test-device@1.0">>, <<"counter">> => 1 },
    {ok, Result} = hb_ao:resolve(Base, <<"vary-overlay">>, Opts),
    Ctx = context(hb_path:hashpath(Result, Opts), Opts),
    BeforePatch =
        format(
            maps:without([<<"varied-result-uri">>, <<"normalizer">>], Ctx),
            Opts
        ),
    {ok, Loaded} = load(BeforePatch, Opts),
    ?assertEqual(2, hb_maps:get(<<"counter">>, Loaded, undefined, Opts)),
    ?assertEqual(
        hb_path:hashpath(Result, Opts),
        hb_path:hashpath(Loaded, Opts)
    ),
    ?assertEqual({ok, 2}, load(<<BeforePatch/binary, "/counter">>, Opts)),
    ?assert(verify_all(BeforePatch, Opts)),
    {ok, Next} = hb_ao:resolve(Loaded, <<"vary-overlay">>, Opts),
    ?assertEqual(3, hb_maps:get(<<"counter">>, Next, undefined, Opts)),
    ?assert(verify_all(hb_path:hashpath(Next, Opts), Opts)).

%% @doc A binary result loads by resolving its request on its base. A key with
%% a literal value, such as `7', loads by its `ao:' path.
literal_result_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Value = <<"literal replacement">>,
    {ok, ValueBaseID} = hb_cache:write(#{ <<"value">> => Value }, Opts),
    HP =
        format(
            #{
                <<"base-uri">> => <<"ao:", ValueBaseID/binary>>,
                <<"request">> => #{ <<"path">> => <<"value">> },
                <<"varied-result">> => Value
            },
            Opts
        ),
    ?assertEqual({ok, Value}, load(HP, Opts)),
    % `ao:' followed by the hashpath loads the same value as the hashpath.
    ?assertEqual({ok, Value}, hb_cache:read(<<"ao:", HP/binary>>, Opts)),
    {ok, BaseID} = hb_cache:write(#{ <<"value">> => 7 }, Opts),
    ?assertEqual({ok, 7}, load(<<"ao:", BaseID/binary, "/value">>, Opts)),
    ?assertEqual(
        {ok, 7},
        hb_cache:read(<<"ao:", BaseID/binary, "/value">>, Opts)
    ),
    Link = {link, <<"ao:", BaseID/binary, "/value">>, #{}},
    ?assertEqual({ok, 7}, load(Link, Opts)),
    ?assertEqual(7, hb_cache:ensure_loaded(Link, Opts)),
    {ok, Ctx} = hb_ao:resolve(<<"ao:", BaseID/binary>>, <<"value">>,
        Opts#{ <<"return-context">> => true }),
    ?assertEqual(<<"ao:", BaseID/binary, "/value">>, format(Ctx, Opts)),
    ?assertEqual({ok, 7}, load(format(Ctx, Opts), Opts)).

%% @doc Ordinary store links and hashpath roots have distinct address syntax.
store_path_link_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Child = #{ <<"value">> => 7 },
    {ok, ID} = hb_cache:write(Child, Opts),
    Path = <<(hb_message:id(#{}, all, Opts))/binary, "/child.name">>,
    hb_cache:link(ID, Path, Opts),
    Loaded = hb_cache:ensure_loaded({link, Path, #{}}, Opts),
    ?assertEqual(hb_message:id(Child, all, Opts),
        hb_message:id(Loaded, all, Opts)),
    HP = <<"ao:", ID/binary>>,
    {ok, Root} = hb_cache:read(HP, Opts),
    ?assertEqual(ID, hb_message:id(Root, all, Opts)),
    {ok, StoredHP} = hb_cache:write(HP, Opts),
    ?assertEqual(
        #{ <<"value">> => HP },
        hb_link:normalize(
            #{ <<"value">> => {link, StoredHP, #{ <<"lazy">> => true }} },
            discard,
            Opts
        )
    ).

%% @doc A hashpath link obeys its store scope, including for the base of its
%% request.
scoped_link_test() ->
    Remote = (hb_test_utils:test_store())#{ <<"scope">> => remote },
    Opts = #{ <<"store">> => [hb_test_utils:test_store(), Remote] },
    Value = <<"remote replacement">>,
    {ok, BaseID} =
        hb_cache:write(
            #{ <<"value">> => Value },
            Opts#{ <<"store">> => [Remote] }
        ),
    HP =
        format(
            #{
                <<"base-uri">> => <<"ao:", BaseID/binary>>,
                <<"request">> => #{ <<"path">> => <<"value">> },
                <<"varied-result">> => Value
            },
            Opts
        ),
    LinkOpts = #{},
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
    Signed =
        hb_message:commit(
            #{
                <<"device">> => <<"test-device@1.0">>,
                <<"status">> => 200,
                <<"counter">> => 1,
                <<"value">> => <<"signed child">>
            },
            Opts#{ <<"priv-wallet">> => ar_wallet:new() }
        ),
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
        {ok, LinkedReply} =
            hb_http:post(
                Node,
                <<"/~message@1.0/value">>,
                #{
                    <<"value">> => {link, HP, #{}}
                },
                Opts
            ),
        ?assertEqual(hb_message:id(Signed, all, Opts),
            hb_message:id(LinkedReply, hb_message:signers(Signed, Opts), Opts)),
        ?assert(hb_message:verify(LinkedReply, all, Opts)),
        ?assert(verify_all(
            hb_maps:get(<<"hashpath">>, LinkedReply, undefined, Opts), Opts)),
        {ok, Loaded} = load(HP, Opts),
        {ok, Next} = hb_ao:resolve(Loaded, <<"vary-overlay">>, Opts),
        ?assertEqual(2, hb_maps:get(<<"counter">>, Next, undefined, Opts)),
        ?assert(verify_all(hb_path:hashpath(Next, Opts), Opts)),
        % The node ignores false claims and gives the hashpath it computes.
        {ok, W} = hb_cache:write(#{ <<"wrong-result">> => 42 }, Opts),
        {ok, Unclaimed} = load(<<"ao:", BaseID/binary, "/child">>, Opts),
        ?assertEqual(
            lists:duplicate(4, hb_path:hashpath(Unclaimed, Opts)),
            [
                hb_maps:get(
                    <<"hashpath">>,
                    hb_util:ok(
                        hb_http:get(
                            Node,
                            <<"/ao:", BaseID/binary, "%2Fchild", Claim/binary>>,
                            Opts
                        )
                    ),
                    undefined,
                    Opts
                )
            ||
                Claim <-
                    [
                        <<"%2E", W/binary>>,
                        <<"%2A", W/binary>>,
                        <<"%3B", W/binary, "%2B", W/binary>>,
                        <<"%40", W/binary>>
                    ]
            ]
        )
    after
        cowboy:stop_listener(hb_util:human_id(ar_wallet:to_address(Wallet)))
    end.

%% @doc A specified function whose schema selects every key of its inputs
%% executes the identity vary: its receipt is compact and verifies.
identity_projection_receipt_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>]
    },
    Req = #{ <<"path">> => <<"set">>, <<"b">> => 2 },
    {ok, Result} = hb_ao:resolve(#{ <<"a">> => 1 }, Req, Opts),
    HP = hb_path:hashpath(Result, Opts),
    ?assertEqual(
        [<<"base-uri">>, <<"request-uri">>, <<"varied-result-uri">>],
        lists:sort(maps:keys(context(HP, Opts)))
    ),
    ?assert(verify_all(HP, Opts)),
    {ok, Loaded} = load(HP, Opts),
    ?assertEqual(
        hb_private:reset(Result),
        hb_private:reset(hb_cache:ensure_all_loaded(Loaded, Opts))
    ).

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
        [<<"base">>, <<"request">>, <<"varied-result">>]
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
        receive {Ref, ready, First} -> ok
        after 5000 -> error('leader-timeout') end,
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
            ?assertNotEqual(maps:get(<<"base-uri">>, CtxA),
                maps:get(<<"base-uri">>, CtxB)),
            ?assertEqual(maps:get(<<"varied-result-uri">>, CtxA),
                maps:get(<<"varied-result-uri">>, CtxB))
        after
            unlink(Second),
            exit(Second, kill)
        end
    after
        unlink(First),
        exit(First, kill)
    end.
