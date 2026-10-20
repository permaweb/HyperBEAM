%%% @doc This module is used by dev_scheduler in order to produce outputs that
%%% are compatible with various forms of AO clients. It features two main formats:
%%%
%%% - `application/json'
%%% - `application/http'
%%%
%%% The `application/json' format is a legacy format that is not recommended for
%%% new integrations of the AO protocol.
-module(lib_scheduler_formats).
-export([assignments_to_bundle/4, assignments_to_aos2/4]).
-export([aos2_to_assignments/3, aos2_to_assignment/2]).
-export([aos2_normalize_types/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Generate a `GET /schedule' response for a process as HTTP-sig bundles.
assignments_to_bundle(ProcID, Assignments, More, Opts) ->
    TimeInfo = ar_timestamp:get(),
    assignments_to_bundle(ProcID, Assignments, More, TimeInfo, Opts).
assignments_to_bundle(ProcID, Assignments, More, TimeInfo, RawOpts) ->
    Opts = format_opts(RawOpts),
    {Timestamp, Height, Hash} = TimeInfo,
    {ok, #{
        <<"type">> => <<"schedule">>,
        <<"process">> => hb_util:human_id(ProcID),
        <<"continues">> => hb_util:atom(More),
        <<"timestamp">> => hb_util:int(Timestamp),
        <<"block-height">> => hb_util:int(Height),
        <<"block-hash">> => Hash,
        <<"assignments">> =>
            hb_message:normalize_commitments(
                hb_maps:from_list(
                    lists:map(
                        fun(Assignment) ->
                            {
                                hb_ao:normalize_key(
                                    hb_ao:get(
                                        <<"slot">>,
                                        Assignment,
                                        Opts#{ <<"hashpath">> => ignore }
                                    )
                                ),
                                Assignment
                            }
                        end,
                        Assignments
                    )
                ),
                Opts
            )
    }}.

%%% Return legacy net-SU compatible results.
assignments_to_aos2(ProcID, Assignments, More, RawOpts) when is_map(Assignments) ->
    assignments_to_aos2(
        ProcID,
        hb_util:message_to_ordered_list(Assignments),
        More,
        format_opts(RawOpts)
    );
assignments_to_aos2(ProcID, Assignments, More, RawOpts) ->
    Opts = format_opts(RawOpts),
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    BodyStruct = 
        #{
            <<"page_info">> =>
                #{
                    <<"process">> => hb_util:human_id(ProcID),
                    <<"has_next_page">> => More,
                    <<"timestamp">> => list_to_binary(integer_to_list(Timestamp)),
                    <<"block-height">> => list_to_binary(integer_to_list(Height)),
                    <<"block-hash">> => Hash
                },
            <<"edges">> =>
                lists:map(
                    fun(Assignment) ->
                        #{
                            <<"cursor">> => cursor(Assignment, Opts),
                            <<"node">> => assignment_to_aos2(Assignment, Opts)
                        }
                    end,
                    Assignments
                )
        },
    Encoded = hb_json:encode(BodyStruct),
    ?event({body_struct, BodyStruct}),
    ?event({encoded, {explicit, Encoded}}),
    {ok, 
        #{
            <<"content-type">> => <<"application/json">>,
            <<"body">> => Encoded
        }
    }.

%% @doc Generate a cursor for an assignment. This should be the slot number, at
%% least in the case of mainnet `ao.N.1' assignments. In the case of legacynet
%% (`ao.TN.1') assignments, we may want to use the assignment ID.
cursor(Assignment, RawOpts) ->
    Opts = format_opts(RawOpts),
    hb_ao:get(<<"slot">>, Assignment, Opts).
%% @doc Convert an assignment to an AOS2-compatible JSON structure.
assignment_to_aos2(Assignment, RawOpts) ->
    Opts = format_opts(RawOpts),
    Message = hb_ao:get(<<"body">>, Assignment, Opts),
    AssignmentWithoutBody = hb_maps:without([<<"body">>], Assignment, Opts),
    {ok, MessageStruct} =
        hb_ao:resolve(
            #{ <<"device">> => <<"json-iface@1.0">> },
            #{
                <<"path">> => <<"to">>,
                <<"message">> => Message
            },
            Opts
        ),
    {ok, AssignmentStruct} =
        hb_ao:resolve(
            #{ <<"device">> => <<"json-iface@1.0">> },
            #{
                <<"path">> => <<"to">>,
                <<"message">> => AssignmentWithoutBody
            },
            Opts
        ),
    #{
        <<"message">> => MessageStruct,
        <<"assignment">> => AssignmentStruct
    }.

%% @doc Convert an AOS2-style JSON structure to a normalized HyperBEAM
%% assignments response.
aos2_to_assignments(ProcID, Body, RawOpts) ->
    Opts = format_opts(RawOpts),
    Assignments = hb_maps:get(<<"edges">>, Body, Opts, Opts),
    ?event({raw_assignments, Assignments}),
    ParsedAssignments =
        lists:map(
            fun(A) -> aos2_to_assignment(A, Opts) end,
            Assignments
        ),
    ?event({parsed_assignments, ParsedAssignments}),
    TimeInfo =
        case ParsedAssignments of
            [] -> {0, 0, hb_util:encode(<<0:256>>)};
            _ ->
                Last = lists:last(ParsedAssignments),
                {
                    hb_ao:get(<<"timestamp">>, Last, Opts),
                    hb_ao:get(<<"block-height">>, Last, Opts),
                    hb_ao:get(<<"block-hash">>, Last, Opts)
                }
        end,
    assignments_to_bundle(ProcID, ParsedAssignments, false, TimeInfo, Opts).

%% @doc Create and normalize an assignment from an AOS2-style JSON structure.
aos2_to_assignment(A, RawOpts) ->
    Opts = format_opts(RawOpts),
    % Unwrap the node if it is provided. Handle GraphQL-style responses with edges.
    Node = case hb_maps:get(<<"edges">>, A, undefined, Opts) of
        [FirstEdge | _] when is_map(FirstEdge) ->
            hb_maps:get(<<"node">>, FirstEdge, A, Opts);
        undefined ->
            hb_maps:get(<<"node">>, A, A, Opts);
        _ ->
            A
    end,
    ?event({node, Node}),
    AssignmentData = hb_maps:get(<<"assignment">>, Node, undefined, Opts),
    ?event({assignment_data, AssignmentData}),
    {ok, Assignment} = aos2_to_message(AssignmentData, Opts),
    ?event({result_assignment, Assignment}),
    NormalizedAssignment = aos2_normalize_types(Assignment),
    {ok, Message} =
        case hb_maps:get(<<"message">>, Node, undefined, Opts) of
            null ->
                RawMessageID = hb_maps:get(<<"message">>, Assignment, undefined, Opts),
                MessageID = 
                    case RawMessageID of
                        <<Prefix:43/binary, _/binary>> -> Prefix;
                        Other -> Other
                    end,
                ?event(error, {scheduler_did_not_provide_message, MessageID}),
                case hb_cache:read(MessageID, Opts) of
                    {ok, Msg} -> {ok, Msg};
                    {error, _} ->
                        throw({error,
                            {message_not_given_by_scheduler_or_cache,
                                MessageID}
                            }
                        )
                end;
            Body ->
                aos2_to_message(Body, Opts)
        end,
    ?event({message, Message}),
    NormalizedAssignment#{ <<"body">> => Message }.

%% @doc Read the original by ID when the legacy SU's JSON cannot verify.
aos2_to_message(JSON, Opts) ->
    Result =
        try hb_client_gateway:result_to_message(aos2_normalize_data(JSON), Opts)
        catch throw:{invalid_field, anchor, _} -> {error, unverifiable_item}
        end,
    case Result of
        {error, unverifiable_item} ->
            ID = hb_maps:get(<<"id">>, JSON, Opts),
            {ok, Cached} = hb_cache:read(ID, Opts),
            Msg = hb_message:with_commitments(ID, Cached, Opts),
            true = hb_message:signers(Msg, Opts) =/= [],
            true = hb_message:verify(
                Msg, #{ <<"ids">> => [ID] }, Opts
            ),
            hb_message:with_only_committed(Msg, Opts);
        _ -> Result
    end.

%% @doc The `hb_client_gateway' module expects all JSON structures to at least
%% have a `data' field. This function ensures that.
aos2_normalize_data(JSONStruct) ->
    case JSONStruct of
        #{<<"data">> := _} -> JSONStruct;
        _ -> JSONStruct#{ <<"data">> => <<>> }
    end.

%% @doc Add the slot and missing block hash for the AOS2-style scheduling API,
%% retaining signed field values.
aos2_normalize_types(Msg = #{ <<"nonce">> := Nonce })
        when is_binary(Nonce) and not is_map_key(<<"slot">>, Msg) ->
    aos2_normalize_types(
        Msg#{ <<"slot">> => hb_util:int(Nonce) }
    );
aos2_normalize_types(Msg) when not is_map_key(<<"block-hash">>, Msg) ->
    ?event({missing_block_hash, Msg}),
    aos2_normalize_types(Msg#{ <<"block-hash">> => hb_util:encode(<<0:256>>) });
aos2_normalize_types(Msg) ->
    ?event(
        {
            aos2_normalized_types,
            {msg, Msg}
        }
    ),
    Msg.

%% @doc For all scheduler format operations, we do not calculate hashpaths,
%% perform cache lookups, or await inprogress results.
format_opts(Opts) ->
    Opts#{
        <<"hashpath">> => ignore,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"await-inprogress">> => false
    }.
