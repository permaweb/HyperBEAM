%%% @doc Simple wrapper module that enables compute on remote machines,
%%% implementing the JSON-Iface. This can be used either as a standalone, to 
%%% bring trusted results into the local node, or as the `Execution-Device' of
%%% an AO process.
-module(dev_delegated_compute).
-export([init/3, compute/3, normalize/3, snapshot/3, dryrun/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Initialize or normalize the compute-lite device. For now, we don't
%% need to do anything special here.
init(Msg1, _Msg2, _Opts) ->
    {ok, Msg1}.
normalize(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
snapshot(Msg1, _Msg2, _Opts) -> {ok, Msg1}.

%% @doc Normal compute execution through external CU.
%% This function handles standard process execution that permanently modifies state.
compute(Msg1, Msg2, Opts) ->
    OutputPrefix = dev_stack:prefix(Msg1, Msg2, Opts),
    % Extract the process ID - this identifies which process to run compute against
    ProcessID = get_process_id(Msg1, Msg2, Opts),
    % Execute the compute via external CU
    Res = do_compute(ProcessID, Msg2, Opts),
    Slot = hb_ao:get(<<"slot">>, Msg2, Opts),
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
    ?event({do_compute_msg, {aos2, {string, Body}}}),
    % Send to external CU via relay using /result endpoint
    Response = do_relay(
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
    
%% @doc Dryrun execution handler
%% This function is called when the "dryrun" path is set by dev_genesis_wasm.
dryrun(Msg1, Msg2, Opts) ->
    OutputPrefix = dev_stack:prefix(Msg1, Msg2, Opts),
    % Extract the process ID - this identifies which process to run dryrun against
    ProcessID = get_process_id(Msg1, Msg2, Opts),
    % Execute the dryrun via external CU
    Res = do_dryrun(ProcessID, Msg2, Opts),
    handle_relay_response(Msg1, Msg2, Opts, Res, OutputPrefix, ProcessID, dryrun).

%% @doc Execute dry-run computation on a remote machine via relay and the JSON-Iface.
do_dryrun(ProcID, Msg2, Opts) ->
    ?event({do_dryrun_msg, {req, Msg2}}),
    Body = hb_json:encode(dev_json_iface:message_to_json_struct(Msg2, Opts)),
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

do_relay(Method, Path, Body, AOS2, Opts) ->
    hb_ao:resolve(
        #{
            <<"device">> => <<"relay@1.0">>,
            <<"content-type">> => <<"application/json">>
        },
        AOS2#{
            <<"path">> => <<"call">>,
            <<"relay-method">> => Method,
            <<"relay-body">> => Body,
            <<"relay-path">> => Path,
            <<"content-type">> => <<"application/json">>
        },
        Opts
    ).
extract_json_res(Response, Opts) ->
    io:format("RESPONSE: ~p~n", [Response]),
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