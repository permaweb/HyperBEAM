%%% @doc Simple wrapper module that enables compute on remote machines,
%%% implementing the JSON-Iface. This can be used either as a standalone, to 
%%% bring trusted results into the local node, or as the `Execution-Device' of
%%% an AO process.
-module(dev_delegated_compute).
-specification("../../../specs/delegated-compute@1.0.md").
-device_libraries([lib_process, lib_scheduler_formats]).
-export([init/3, compute/3, normalize/3, snapshot/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The keys with which a request, or the message that it relays, steers
%% `relay@1.0'.
-define(RELAY_KEYS, [
    <<"peer">>, <<"relay-route">>, <<"route-path">>, <<"relay-path">>,
    <<"relay-method">>, <<"relay-body">>, <<"relay-device">>,
    <<"relay-commit-request">>, <<"commit-request">>
]).

%% @doc Initialize or normalize the compute-lite device. For now, we don't
%% need to do anything special here.
-spec init(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) -> {ok, #{ _ => _ }}.
init(Base, _Req, _Opts) ->
    {ok, Base}.

%% @doc Restore checkpoint memory in the delegated engine before removing the
%% snapshot from the process state. Failed restores leave normalization failed.
-spec normalize(
    #{ snapshot => #{ type => binary(), data => _, _ => _ }, _ => _ },
    #{ _ => _ },
    #{ _ => _ }
) -> {ok, #{ _ => _ }} | {error, _} | #{ _ => _ }.
normalize(Base, _Req, Opts) ->
    case hb_maps:find(<<"snapshot">>, Base, Opts) of
        error -> {ok, Base};
        {ok, Snapshot} ->
            case hb_maps:get(<<"type">>, Snapshot, Opts) == <<"Checkpoint">> of
                false -> hb_ao:set(Base, #{ <<"snapshot">> => unset }, Opts);
                true ->
                    maybe
                        {ok, _} ?= load_state(Snapshot, Opts),
                        hb_ao:set(Base, #{ <<"snapshot">> => unset }, Opts)
                    end
            end
    end.

%% @doc Attempt to load a snapshot into the delegated compute server.
load_state(Snapshot, Opts) ->
    ?event(debug_load_snapshot, {loading_snapshot, {snapshot, Snapshot}}),
    Body = hb_maps:get(<<"data">>, Snapshot, Opts),
    % Checkpoint commitments cover data, not the HTTP request body.
    Headers = hb_maps:without(
        [<<"data">>], hb_message:uncommitted(Snapshot, Opts), Opts
    ),
    Res = do_relay(
        <<"POST">>,
        <<"/state">>,
        Body,
        Headers,
        Opts#{
            <<"hashpath">> => ignore,
            <<"cache-control">> => [<<"no-store">>, <<"no-cache">>]
        }
    ),
    ?event(debug_load_snapshot, {load_result, Res}),
    case Res of
        {failure, Error} -> {error, Error};
        _ -> Res
    end.

%% @doc Call the delegated server to compute the result. The endpoint is
%% `POST /compute' and the body is the JSON-encoded message that we want to
%% evaluate.
-spec compute(
    #{ _ => _ },
    #{ type => binary(), slot => integer(), 'process-id' => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ _ => _ }} | {failure, _}.
compute(Base, Req, Opts) ->
    OutputPrefix =
        hb_ao:get(
            <<"output-prefix">>,
            {as, <<"message@1.0">>, Base},
            <<"">>,
            Opts
        ),
    % Extract the process ID - this identifies which process to run compute
    % against.
    ProcessID = get_process_id(Base, Req, Opts),
    % If request is an assignment, we will compute the result
    % Otherwise, it is a dryrun
    Type = hb_ao:get(<<"type">>, Req, not_found, Opts),
    ?event({doing_delegated_compute, {req, Req}, {type, Type}}),
    % Execute the compute via external CU
    {Slot, Res} =
        case Type of
            <<"Assignment">> ->
                {
                    hb_ao:get(<<"slot">>, Req, Opts),
                    do_compute(ProcessID, Req, Opts)
                };
            _ ->
                {dryrun, do_dryrun(ProcessID, Req, Opts)}
        end,
    handle_relay_response(Base, Req, Opts, Res, OutputPrefix, ProcessID, Slot).

%% @doc Execute computation on a remote machine via relay and the JSON-Iface.
do_compute(ProcID, Req, Opts) ->
    ?event({do_compute_msg, {req, Req}}),
    Slot = hb_ao:get(<<"slot">>, Req, Opts),
    {ok, AOS2 = #{ <<"body">> := Body }} =
        lib_scheduler_formats:assignments_to_aos2(
            ProcID,
            #{
                Slot => Req
            },
            false,
            Opts
        ),
    ?event({do_compute_body, {aos2, {string, Body}}}),
    % Send to external CU via relay using /result endpoint
    Response =
        do_relay(
            <<"POST">>,
            <<"/result/", (hb_util:bin(Slot))/binary, "?process-id=", ProcID/binary>>,
            Body,
            AOS2,
            Opts#{
                <<"hashpath">> => ignore,
                <<"cache-control">> => [<<"no-store">>, <<"no-cache">>]
            }
        ),
    extract_json_res(Response, Opts).

%% @doc Execute dry-run computation on a remote machine via relay and use
%% the JSON-Iface to decode the response.
do_dryrun(ProcID, Req, Opts) ->
    ?event({do_dryrun_msg, {req, Req}}),
    % Remove commitments from the message before sending to the external CU
    {ok, Struct} =
        hb_ao:resolve(
            #{ <<"device">> => <<"json-iface@1.0">> },
            #{
                <<"path">> => <<"to">>,
                <<"message">> => hb_maps:without([<<"commitments">>], Req, Opts)
            },
            Opts
        ),
    Body = hb_json:encode(Struct),
    ?event({do_dryrun_body, {string, Body}}),
    % Send to external CU via relay using /dry-run endpoint
    Response = do_relay(
        <<"POST">>,
        <<"/dry-run?process-id=", ProcID/binary>>,
        Body,
        #{},
        Opts#{
            <<"hashpath">> => ignore,
            <<"cache-control">> => [<<"no-store">>, <<"no-cache">>]
        }
    ),
    extract_json_res(Response, Opts).

do_relay(Method, Path, Body, RawHeaders, Opts) ->
    % The headers go to the compute server. They do not choose where the relay
    % goes: a snapshot's keys come from a caller.
    Headers = hb_maps:without(?RELAY_KEYS, RawHeaders, Opts),
    ContentType =
        hb_maps:get(
            <<"content-type">>,
            Headers,
            <<"application/json">>,
            Opts
        ),
    % The compute peer is set by the node operator, so permit it as an internal
    % host for this call.
    hb_ao:resolve(
        #{
            <<"device">> => <<"relay@1.0">>,
            <<"peer">> => hb_opts:get(delegated_compute_peer, not_found, Opts),
            <<"content-type">> => ContentType
        },
        Headers#{
            <<"path">> => <<"call">>,
            <<"relay-subject">> => <<"payload">>,
            <<"payload">> =>
                Headers#{
                    <<"path">> => Path,
                    <<"method">> => Method,
                    <<"body">> => Body,
                    <<"content-type">> => ContentType
                }
        },
        Opts#{ <<"hosts-block-internal">> => false }
    ).

%% @doc Extract the JSON response from the delegated compute response. An
%% answer that is not a result, an error status, or no answer at all means
%% that the server did not compute the message. That depends on the server
%% this node uses, not on the process, so it is a failure of this node.
extract_json_res(Response, Opts) ->
    case Response of 
        {ok, Res} ->
            JSONRes = hb_ao:get(<<"body">>, Res, Opts),
            ?event({
                delegated_compute_res_metadata,
                {req, hb_maps:without([<<"body">>], Res, Opts)}
            }),
            case is_result(hb_json:decode(JSONRes, Opts)) of
                true -> {ok, JSONRes};
                false ->
                    {failure,
                        #{
                            <<"status">> => 502,
                            <<"reason">> =>
                                <<"Delegated compute answer is not a result.">>,
                            <<"body">> => JSONRes
                        }
                    }
            end;
        {Err, Error} when Err == error; Err == failure ->
            {failure, Error}
    end.

%% @doc Return whether a decoded answer of the server is a result, which holds
%% `Output', `Messages' or `Error'. An `Error' is the process's own error, the
%% same on every node.
is_result(#{ <<"Output">> := _ }) -> true;
is_result(#{ <<"Messages">> := _ }) -> true;
is_result(#{ <<"Error">> := _ }) -> true;
is_result(_) -> false.

get_process_id(Base, Req, Opts) ->
    RawProcessID = lib_process:process_id(Base, #{}, Opts),
    case RawProcessID of
        not_found -> hb_ao:get(<<"process-id">>, Req, Opts);
        ProcID -> ProcID
    end.

%% @doc Handle the response from the delegated compute server. Assumes that the
%% response is in AOS2-style format, decoding with the JSON-Iface.
handle_relay_response(Base, Req, Opts, Response, OutputPrefix, ProcessID, Slot) ->
    case Response of 
        {ok, JSONRes} ->
            ?event(
                {compute_lite_res,
                    {process_id, ProcessID},
                    {slot, Slot},
                    {json_res, {string, JSONRes}},
                    {req, Req}
                }
            ),
            {ok, Msg} =
                hb_ao:resolve(
                    #{ <<"device">> => <<"json-iface@1.0">> },
                    #{
                        <<"path">> => <<"from">>,
                        <<"json">> => JSONRes
                    },
                    Opts
                ),
            Raw = hb_json:decode(JSONRes, Opts),
            {ok,
                hb_ao:set(
                    Base,
                    #{
                        <<OutputPrefix/binary, "/results">> => Msg,
                        <<OutputPrefix/binary, "/results/raw">> => Raw
                    },
                    Opts
                )
            };
        {failure, Failure} ->
            {failure, Failure}
    end.

%% @doc Generate a snapshot of a running computation by calling the 
%% `GET /snapshot' endpoint.
-spec snapshot(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) -> {ok, #{ _ => _ }}.
snapshot(Msg, Req, Opts) ->
    ?event({snapshotting, {req, Req}}),
    ProcID = lib_process:process_id(Msg, #{}, Opts),
    Res = 
        hb_ao:resolve(
            #{
                <<"device">> => <<"relay@1.0">>,
                <<"peer">> => hb_opts:get(delegated_compute_peer, not_found, Opts),
                <<"content-type">> => <<"application/json">>
            },
            #{
                <<"path">> => <<"call">>,
                <<"relay-method">> => <<"POST">>,
                <<"relay-path">> => <<"/snapshot/", ProcID/binary>>,
                <<"content-type">> => <<"application/json">>,
                <<"body">> => <<"{}">>
            },
            % The compute peer is set by the node operator, so permit it as an
            % internal host for this call.
            Opts#{
                <<"hashpath">> => ignore,
                <<"cache-control">> => [<<"no-store">>, <<"no-cache">>],
                <<"hosts-block-internal">> => false
            }
        ),
    ?event({snapshotting_result, Res}),
    case Res of
        {ok, Response} ->
            {ok, Response};
        {error, Error} ->
            {ok,
                #{
                    <<"error">> => <<"No checkpoint produced.">>,
                    <<"error-details">> => Error
                }}
    end.
