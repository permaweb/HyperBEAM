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
normalize(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
snapshot(Msg1, _Msg2, _Opts) -> {ok, Msg1}.

compute(Msg1, Msg2, Opts) ->
    RawProcessID = dev_process:process_id(Msg1, #{}, Opts),
    OutputPrefix = dev_stack:prefix(Msg1, Msg2, Opts),
    ProcessID =
        case RawProcessID of
            not_found -> hb_ao:get(<<"process-id">>, Msg2, Opts);
            ProcID -> ProcID
        end,
    Res = do_compute(ProcessID, Msg2, Opts),
    case Res of
        {ok, JSONRes} ->
            ?event(
                {compute_lite_res,
                    {process_id, ProcessID},
                    {slot, hb_ao:get(<<"slot">>, Msg2, Opts)},
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
    Res = 
        hb_ao:resolve(
            #{
                <<"device">> => <<"relay@1.0">>,
                <<"content-type">> => <<"application/json">>
            },
            AOS2#{
                <<"path">> => <<"call">>,
                <<"relay-method">> => <<"POST">>,
                <<"relay-body">> => Body,
                <<"relay-path">> =>
                    <<
                        "/result/",
                        (hb_util:bin(Slot))/binary,
                        "?process-id=",
                        ProcID/binary
                    >>,
                <<"content-type">> => <<"application/json">>
            },
            Opts#{
                hashpath => ignore,
                cache_control => [<<"no-store">>, <<"no-cache">>]
            }
        ),
    case Res of
        {ok, Response} ->
            JSONRes = hb_ao:get(<<"body">>, Response, Opts),
            ?event({
                delegated_compute_res_metadata,
                {req, maps:without([<<"body">>], Response)}
            }),
            {ok, JSONRes};
        {Err, Error} when Err == error; Err == failure ->
            {error, Error}
    end.