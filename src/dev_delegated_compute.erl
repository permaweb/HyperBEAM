%%% @doc Simple wrapper module that enables compute on remote machines,
%%% implementing the JSON-Iface. This can be used either as a standalone, to 
%%% bring trusted results into the local node, or as the `Execution-Device' of
%%% an AO process.
-module(dev_delegated_compute).
-export([init/3, compute/3, normalize/3, snapshot/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Initialize or normalize the compute-lite device. For now, we don't
%% need to do anything special here.
init(Msg1, _Msg2, _Opts) ->
    {ok, Msg1}.

%% @doc We assume that the compute engine stores its own internal state,
%% with snapshots triggered only when HyperBEAM requests them. Subsequently,
%% to load a snapshot, we just need to return the original message.
normalize(Msg1, _Msg2, Opts) ->
    case hb_maps:find(<<"snapshot">>, Msg1, Opts) of
        error -> {ok, Msg1};
        {ok, Snapshot} ->
            Unset = hb_ao:set(Msg1, #{ <<"snapshot">> => unset }, Opts),
            case hb_maps:get(<<"type">>, Snapshot, Opts) == <<"Checkpoint">> of
                false -> Unset;
                true ->
                    load_state(Snapshot, Opts),
                    Unset
            end
    end.

%% @doc Attempt to load a snapshot into the delegated compute server.
load_state(Snapshot, Opts) ->
    ?event(debug_load_snapshot, {loading_snapshot, {snapshot, Snapshot}}),
    Body = hb_maps:get(<<"data">>, Snapshot, Opts),
    Headers = hb_maps:without([<<"data">>], Snapshot, Opts),
    Res = do_relay(
        <<"POST">>,
        <<"/state">>,
        Body,
        Headers,
        Opts#{
            hashpath => ignore,
            cache_control => [<<"no-store">>, <<"no-cache">>]
        }
    ),
    ?event(debug_load_snapshot, {load_result, Res}),
    Res.

%% @doc Call the delegated server to compute the result. The endpoint is
%% `POST /compute' and the body is the JSON-encoded message that we want to
%% evaluate.
compute(Msg1, Msg2, Opts) ->
    OutputPrefix = dev_stack:prefix(Msg1, Msg2, Opts),
    % Extract the process ID - this identifies which process to run compute
    % against.
    ProcessID = get_process_id(Msg1, Msg2, Opts),
    % If request is an assignment, we will compute the result
    % Otherwise, it is a dryrun
    Type = hb_ao:get(<<"type">>, Msg2, not_found, Opts),
    ?event({doing_delegated_compute, {msg2, Msg2}, {type, Type}}),
    % Execute the compute via external CU
    case Type of
        <<"Assignment">> ->
            Slot = hb_ao:get(<<"slot">>, Msg2, Opts),
            Res = do_compute(ProcessID, Msg2, Opts);
        _ ->
            Slot = dryrun,
            Res = do_dryrun(ProcessID, Msg2, Opts)
    end,
    handle_relay_response(Msg1, Msg2, Opts, Res, OutputPrefix, ProcessID, Slot).

%% @doc Execute computation on a remote machine via relay and the JSON-Iface.
do_compute(ProcID, Msg2, Opts) ->
    ?event({do_compute_msg, {req, Msg2}}),
    Slot = hb_ao:get(<<"slot">>, Msg2, Opts),
    {ok, AOS2 = #{ <<"body">> := Body }} =
        dev_scheduler_formats:assignments_to_aos2(
            ProcID,
            #{
                Slot => Msg2
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
                hashpath => ignore,
                cache_control => [<<"no-store">>, <<"no-cache">>]
            }
        ),
    extract_json_res(Response, Opts).

%% @doc Execute dry-run computation on a remote machine via relay and use
%% the JSON-Iface to decode the response.
do_dryrun(ProcID, Msg2, Opts) ->
    ?event({do_dryrun_msg, {req, Msg2}}),
    % Remove commitments from the message before sending to the external CU
    Body = 
        hb_json:encode(
            dev_json_iface:message_to_json_struct(
                hb_maps:without([<<"commitments">>], Msg2, Opts),
                Opts
            )
        ),
    ?event({do_dryrun_body, {string, Body}}),
    % Send to external CU via relay using /dry-run endpoint
    Response = do_relay(
        <<"POST">>,
        <<"/dry-run?process-id=", ProcID/binary>>,
        Body,
        #{},
        Opts#{
            hashpath => ignore,
            cache_control => [<<"no-store">>, <<"no-cache">>]
        }
    ),
    extract_json_res(Response, Opts).

do_relay(Method, Path, Body, Headers, Opts) ->
    ContentType =
        hb_maps:get(
            <<"content-type">>,
            Headers,
            <<"application/json">>,
            Opts
        ),
    hb_ao:resolve(
        #{
            <<"device">> => <<"relay@1.0">>,
            <<"content-type">> => ContentType
        },
        Headers#{
            <<"path">> => <<"call">>,
            <<"target">> => <<"payload">>,
            <<"payload">> =>
                Headers#{
                    <<"path">> => Path,
                    <<"method">> => Method,
                    <<"body">> => Body,
                    <<"content-type">> => ContentType
                }
        },
        Opts
    ).

%% @doc Extract the JSON response from the delegated compute response.
extract_json_res(Response, Opts) ->
    case Response of 
        {ok, Res} ->
            JSONRes = hb_ao:get(<<"body">>, Res, Opts),
            ?event({
                delegated_compute_res_metadata,
                {req, hb_maps:without([<<"body">>], Res, Opts)}
            }),
            {ok, JSONRes};
        {Err, Error} when Err == error; Err == failure ->
            {error, Error}
    end.

get_process_id(Msg1, Msg2, Opts) ->
    RawProcessID = dev_process:process_id(Msg1, #{}, Opts),
    case RawProcessID of
        not_found -> hb_ao:get(<<"process-id">>, Msg2, Opts);
        ProcID -> ProcID
    end.

%% @doc Handle the response from the delegated compute server. Assumes that the
%% response is in AOS2-style format, decoding with the JSON-Iface.
handle_relay_response(Msg1, Msg2, Opts, Response, OutputPrefix, ProcessID, Slot) ->
    case Response of 
        {ok, JSONRes} ->
            ?event(
                {compute_lite_res,
                    {process_id, ProcessID},
                    {slot, Slot},
                    {json_res, {string, JSONRes}},
                    {req, Msg2}
                }
            ),
            {ok, Msg} = dev_json_iface:json_to_message(JSONRes, Opts),
            {ok,
                hb_ao:set(
                    Msg1,
                    #{
                        <<OutputPrefix/binary, "/results">> => Msg,
                        <<OutputPrefix/binary, "/results/json">> =>
                            #{
                                <<"content-type">> => <<"application/json">>,
                                <<"body">> => JSONRes
                            }
                    },
                    Opts
                )
            };
        {error, Error} ->
            {error, Error}
    end.

%% @doc Generate a snapshot of a running computation by calling the 
%% `GET /snapshot' endpoint.
snapshot(Msg, Msg2, Opts) ->
    ?event({snapshotting, {req, Msg2}}),
    ProcID = dev_process:process_id(Msg, #{}, Opts),
    Res = 
        hb_ao:resolve(
            #{
                <<"device">> => <<"relay@1.0">>,
                <<"content-type">> => <<"application/json">>
            },
            #{
                <<"path">> => <<"call">>,
                <<"relay-method">> => <<"POST">>,
                <<"relay-path">> => <<"/snapshot/", ProcID/binary>>,
                <<"content-type">> => <<"application/json">>,
                <<"body">> => <<"{}">>
            },
            Opts#{
                hashpath => ignore,
                cache_control => [<<"no-store">>, <<"no-cache">>]
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