%%% @doc A device that provides a way for WASM execution to interact with
%%% the HyperBEAM (and AO) systems, using JSON as a shared data representation.
%%% 
%%% The interface is easy to use. It works as follows:
%%% 
%%% 1. The device is given a message that contains a process definition, WASM
%%%    environment, and a message that contains the data to be processed,
%%%    including the image to be used in part of `execute{pass=1}'.
%%% 2. The device is called with `execute{pass=2}', which reads the result of
%%%    the process execution from the WASM environment and adds it to the
%%%    message.
%%%
%%% The device has the following requirements and interface:
%%%     ```
%%%     M1/Computed when /Pass == 1 ->
%%%         Assumes:
%%%             M1/priv/wasm/instance
%%%             M1/Process
%%%             M2/Message
%%%             M2/Assignment/Block-Height
%%%         Generates:
%%%             /wasm/handler
%%%             /wasm/params
%%%         Side-effects:
%%%             Writes the process and message as JSON representations into the
%%%             WASM environment.
%%% 
%%%     M1/Computed when M2/Pass == 2 ->
%%%         Assumes:
%%%             M1/priv/wasm/instance
%%%             M2/Results
%%%             M2/Process
%%%         Generates:
%%%             /Results/Outbox
%%%             /Results/Data'''
-module(dev_json_iface).
-export([init/3, compute/3]).
%%% Public interface helpers:
-export([message_to_json_struct/1, json_to_message/2]).
%%% Test helper exports:
-export([generate_stack/1, generate_stack/2, generate_aos_msg/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Initialize the device.
init(M1, _M2, _Opts) ->
    {ok, hb_converge:set(M1, #{<<"wasm-function">> => <<"handle">>})}.

%% @doc On first pass prepare the call, on second pass get the results.
compute(M1, M2, Opts) ->
    case hb_converge:get(<<"pass">>, M1, Opts) of
        1 -> prep_call(M1, M2, Opts);
        2 -> results(M1, M2, Opts);
        _ -> {ok, M1}
    end.

%% @doc Prepare the WASM environment for execution by writing the process string and
%% the message as JSON representations into the WASM environment.
prep_call(M1, M2, Opts) ->
    ?event({prep_call, M1, M2, Opts}),
    Instance = hb_private:get(<<"priv/wasm/instance">>, M1, Opts),
    Process = hb_converge:get(<<"process">>, M1, Opts#{ hashpath => ignore }),
    Message = hb_converge:get(<<"body">>, M2, Opts#{ hashpath => ignore }),
    Image = hb_converge:get(<<"process/image">>, M1, Opts),
    BlockHeight = hb_converge:get(<<"block-height">>, M2, Opts),
    RawMsgJson = message_to_json_struct(denormalize_message(Message)),
    {Props} = RawMsgJson,
    MsgProps =
        normalize_props(
            Props ++
                [
                    {<<"Module">>, Image},
                    {<<"Block-Height">>, BlockHeight}
                ]
        ),
    MsgJson = jiffy:encode({MsgProps}),
    {ok, MsgJsonPtr} = hb_beamr_io:write_string(Instance, MsgJson),
    ProcessProps =
        normalize_props(
            [{<<"Process">>, message_to_json_struct(Process)}]
        ),
    ProcessJson = jiffy:encode({ProcessProps}),
    {ok, ProcessJsonPtr} = hb_beamr_io:write_string(Instance, ProcessJson),
    {ok,
        hb_converge:set(
            M1,
            #{
                <<"wasm-function">> => <<"handle">>,
                <<"wasm-params">> => [MsgJsonPtr, ProcessJsonPtr]
            },
            Opts
        )
    }.

%% @doc Normalize a message for AOS-compatibility.
denormalize_message(Message) ->
    Signers =
        lists:filter(
            fun(ID) -> ?IS_ID(ID) end,
            hb_converge:get(<<"attestors">>, {as, <<"message@1.0">>, Message})
        ),
    NormOwnerMsg =
        case Signers of
            [] -> Message;
            [Signer|_] ->
                Sig =
                    hb_converge:get(
                        <<"attestations/", Signer/binary, "/signature">>,
                        {as, <<"message@1.0">>, Message}
                    ),
                Message#{ <<"owner">> => Signer, <<"signature">> => Sig }
        end,
    NormOwnerMsg#{
        <<"id">> => hb_message:id(Message, all)
    }.

message_to_json_struct(RawMsg) ->
    message_to_json_struct(RawMsg, [owner_as_address]).
message_to_json_struct(RawMsg, Features) ->
    Message = 
        hb_message:convert(
            hb_private:reset(maps:without([<<"attestations">>], RawMsg)),
            tabm,
            #{}
        ),
    ID = hb_message:id(RawMsg, all),
    Last = hb_converge:get(<<"anchor">>, {as, <<"message@1.0">>, Message}, <<>>, #{}),
	Owner =
        case hb_message:signers(RawMsg) of
            [] -> <<>>;
            [Signer|_] ->
                case lists:member(owner_as_address, Features) of
                    true -> hb_util:native_id(Signer);
                    false ->
                        Attestation =
                            hb_converge:get(
                                <<"attestations/", Signer/binary>>,
                                {as, <<"message@1.0">>, RawMsg},
                                #{}
                            ),
                        case hb_converge:get(<<"owner">>, Attestation, #{}) of
                            not_found ->
                                % The signature is likely a HTTPsig, so we need 
                                % to extract the owner from the signature.
                                case dev_codec_httpsig:public_keys(Attestation) of
                                    [] -> <<>>;
                                    [PubKey|_] -> PubKey
                                end;
                            ANS104Owner -> ANS104Owner
                        end
                end
        end,
    Data = hb_converge:get(<<"data">>, {as, <<"message@1.0">>, Message}, <<>>, #{}),
    Target = hb_converge:get(<<"target">>, {as, <<"message@1.0">>, Message}, <<>>, #{}),
    % Set "From" if From-Process is Tag or set with "Owner" address
    From =
        hb_converge:get(
            <<"from-process">>,
            {as, <<"message@1.0">>, Message},
            hb_util:encode(Owner),
            #{}
        ),
    Sig = hb_converge:get(<<"signature">>, {as, <<"message@1.0">>, Message}, <<>>, #{}),
    Fields = [
        {<<"Id">>, safe_to_id(ID)},
        % NOTE: In Arweave TXs, these are called "last_tx"
        {<<"Anchor">>, Last},
        % NOTE: When sent to ao "Owner" is the wallet address
        {<<"Owner">>, hb_util:encode(Owner)},
        {<<"From">>, case ?IS_ID(From) of true -> safe_to_id(From); false -> From end},
        {<<"Tags">>,
            lists:map(
                fun({Name, Value}) ->
                    {
                        [
                            {name, maybe_list_to_binary(Name)},
                            {value, maybe_list_to_binary(Value)}
                        ]
                    }
                end,
                maps:to_list(
                    maps:without(
                        [
                            <<"id">>, <<"anchor">>, <<"owner">>, <<"data">>,
                            <<"target">>, <<"signature">>, <<"attestations">>
                        ],
                        Message
                    )
                )
            )},
        {<<"Target">>, safe_to_id(Target)},
        {<<"Data">>, Data},
        {<<"Signature">>,
            case byte_size(Sig) of
                0 -> <<>>;
                512 -> hb_util:encode(Sig);
                _ -> Sig
            end}
    ],
    HeaderCaseFields = normalize_props(Fields),
    {HeaderCaseFields}.

%% @doc Translates a compute result -- either from a WASM execution using the 
%% JSON-Iface, or from a `Legacy' CU -- and transforms it into a result message.
json_to_message(JSON, Opts) when is_binary(JSON) ->
    json_to_message(jiffy:decode(JSON, [return_maps]), Opts);
json_to_message(Resp, Opts) when is_map(Resp) ->
    {ok, Data, Messages, Patches} = normalize_results(Resp),
    Output = 
        #{
            <<"outbox">> =>
                maps:from_list(
                    [
                        {MessageNum, preprocess_results(Msg, Opts)}
                    ||
                        {MessageNum, Msg} <-
                            lists:zip(
                                lists:seq(1, length(Messages)),
                                Messages
                            )
                    ]
                ),
            <<"patches">> => lists:map(fun tags_to_map/1, Patches),
            <<"data">> => Data
        },
    {ok, Output};
json_to_message(#{ <<"ok">> := false, <<"error">> := Error }, _Opts) ->
    {error, Error};
json_to_message(Other, _Opts) ->
    {error,
        #{
            <<"error">> => <<"Invalid JSON message input.">>,
            <<"received">> => Other
        }
    }.

safe_to_id(<<>>) -> <<>>;
safe_to_id(ID) -> hb_util:human_id(ID).

maybe_list_to_binary(List) when is_list(List) ->
    list_to_binary(List);
maybe_list_to_binary(Bin) ->
    Bin.

%% @doc Normalize the properties of a message to begin with a capital letter for
%% backwards compatibility with AOS.
normalize_props(Props) ->
    lists:map(
        fun({<<"Tags">>, Values}) ->
            {<<"Tags">>,
                lists:map(
                    fun({[{name, Name}, {value, Value}]}) ->
                        {
                            [
                                {name, header_case_string(Name)},
                                {value, Value}
                            ]
                        }
                    end,
                    Values
                )
            };
        ({Key, Value}) ->
            {header_case_string(Key), Value}
        end,
        Props
    ).

header_case_string(Key) ->
    NormKey = hb_converge:normalize_key(Key),
    Words = string:lexemes(NormKey, "-"),
    TitleCaseWords =
        lists:map(
            fun binary_to_list/1,
            lists:map(
                fun string:titlecase/1,
                Words
            )
        ),
    TitleCaseKey = list_to_binary(string:join(TitleCaseWords, "-")),
    TitleCaseKey.

%% @doc Read the computed results out of the WASM environment, assuming that
%% the environment has been set up by `prep_call/3' and that the WASM executor
%% has been called with `computed{pass=1}'.
results(M1, _M2, Opts) ->
    Instance = hb_private:get(<<"priv/wasm/instance">>, M1, Opts),
    Type = hb_converge:get(<<"results/wasm/type">>, M1, Opts),
    Proc = hb_converge:get(<<"process">>, M1, Opts),
    case hb_converge:normalize_key(Type) of
        <<"error">> ->
            {error,
                hb_converge:set(
                    M1,
                    #{
                        <<"outbox">> => undefined,
                        <<"results">> => 
                            #{
                                <<"body">> => <<"WASM execution error.">>
                            }
                    },
                    Opts
                )
            };
        <<"ok">> ->
            [Ptr] = hb_converge:get(<<"results/wasm/output">>, M1, Opts),
            {ok, Str} = hb_beamr_io:read_string(Instance, Ptr),
            try jiffy:decode(Str, [return_maps]) of
                #{<<"ok">> := true, <<"response">> := Resp} ->
                    {ok, ProcessedResults} = json_to_message(Resp, Opts),
                    PostProcessed = postprocess_outbox(ProcessedResults, Proc, Opts),
                    Out = hb_converge:set(
                        M1,
                        <<"results">>,
                        PostProcessed,
                        Opts
                    ),
                    {ok, Out}
            catch
                _:_ ->
                    ?event(error, {json_error, Str}),
                    {error,
                        hb_converge:set(
                            M1,
                            #{
                                <<"results/outbox">> => undefined,
                                <<"results/body">> =>
                                    <<"JSON error parsing WASM result output.">>
                            },
                            Opts
                        )
                    }
            end
    end.

%% @doc Normalize the results of an evaluation.
normalize_results(
    Msg = #{ <<"Output">> := #{<<"data">> := Data}, <<"Messages">> := Messages}) ->
    {ok, Data, Messages, maps:get(<<"patches">>, Msg, [])};
normalize_results(#{ <<"Error">> := Error }) ->
    {ok, Error, [], []};
normalize_results(Other) ->
    throw({invalid_results, Other}).

%% @doc After the process returns messages from an evaluation, the
%% signing node needs to add some tags to each message and spawn such that
%% the target process knows these messages are created by a process.
preprocess_results(Msg, _Opts) ->
    Tags = tags_to_map(Msg),
    FilteredMsg =
        maps:without(
            [<<"from-process">>, <<"from-image">>, <<"anchor">>, <<"tags">>],
            Msg
        ),
    maps:merge(
        maps:from_list(
            lists:map(
                fun({Key, Value}) ->
                    {hb_converge:normalize_key(Key), Value}
                end,
                maps:to_list(FilteredMsg)
            )
        ),
        Tags
    ).

%% @doc Convert a message with tags into a map of their key-value pairs.
tags_to_map(Msg) ->
    NormMsg = hb_converge:normalize_keys(Msg),
    RawTags = maps:get(<<"tags">>, NormMsg, []),
    TagList =
        [
            {maps:get(<<"name">>, Tag), maps:get(<<"value">>, Tag)}
        ||
            Tag <- RawTags
        ],
    maps:from_list(TagList).

%% @doc Post-process messages in the outbox to add the correct `from-process'
%% and `from-image' tags.
postprocess_outbox(Msg, Proc, Opts) ->
    AdjustedOutbox =
        maps:map(
            fun(_Key, XMsg) ->
                XMsg#{
                    <<"from-process">> => hb_converge:get(id, Proc, Opts),
                    <<"from-image">> => hb_converge:get(<<"image">>, Proc, Opts)
                }
            end,
            hb_converge:get(<<"outbox">>, Msg, #{}, Opts)
        ),
    hb_converge:set(Msg, <<"outbox">>, AdjustedOutbox, Opts).

%%% Tests

test_init() ->
    application:ensure_all_started(hb).

% Disabled due to missing test file
% json_to_message_test() ->
%     {ok, JSON} = file:read_file("test/example_json_iface_result.json"),
%     {ok, Msg} = json_to_message(JSON, #{}),
%     ?event({msg, Msg}),
%     ?assertEqual(<<"OK">>, hb_converge:get(<<"outbox/1/result">>, Msg, #{})).

generate_stack(File) ->
    generate_stack(File, <<"WASM">>).
generate_stack(File, _Mode) ->
    test_init(),
    Wallet = hb:wallet(),
    Msg0 = dev_wasm:cache_wasm_image(File),
    Image = hb_converge:get(<<"image">>, Msg0, #{}),
    Msg1 = Msg0#{
        <<"device">> => <<"Stack@1.0">>,
        <<"device-stack">> =>
            [
                <<"WASI@1.0">>,
                <<"JSON-Iface@1.0">>,
                <<"WASM-64@1.0">>,
                <<"Multipass@1.0">>
            ],
        <<"input-prefix">> => <<"process">>,
        <<"output-prefix">> => <<"wasm">>,
        <<"passes">> => 2,
        <<"stack-keys">> => [<<"init">>, <<"compute">>],
        <<"process">> => 
            hb_message:attest(#{
                <<"type">> => <<"Process">>,
                <<"image">> => Image,
                <<"scheduler">> => hb:address(),
                <<"authority">> => hb:address()
            }, Wallet)
    },
    {ok, Msg2} = hb_converge:resolve(Msg1, <<"init">>, #{}),
    Msg2.

generate_aos_msg(ProcID, Code) ->
    Wallet = hb:wallet(),
    hb_message:attest(#{
        <<"path">> => <<"compute">>,
        <<"body">> => 
            hb_message:attest(#{
                <<"Action">> => <<"Eval">>,
                <<"Data">> => Code,
                <<"Target">> => ProcID
            }, Wallet),
        <<"block-height">> => 1
    }, Wallet).

basic_aos_call_test() ->
    Msg = generate_stack("test/aos-2-pure-xs.wasm"),
    Proc = hb_converge:get(<<"process">>, Msg, #{ hashpath => ignore }),
    ProcID = hb_message:id(Proc, all),
    {ok, Msg3} =
        hb_converge:resolve(
            Msg,
            generate_aos_msg(ProcID, <<"return 1+1">>),
            #{}
        ),
    ?event({res, Msg3}),
    Data = hb_converge:get(<<"results/data">>, Msg3, #{}),
    ?assertEqual(<<"2">>, Data).

aos_stack_benchmark_test_() ->
    {timeout, 20, fun() ->
        BenchTime = 3,
        RawWASMMsg = generate_stack("test/aos-2-pure-xs.wasm"),
        Proc = hb_converge:get(<<"process">>, RawWASMMsg, #{ hashpath => ignore }),
        ProcID = hb_converge:get(id, Proc, #{}),
        {ok, Initialized} =
        hb_converge:resolve(
            RawWASMMsg,
            generate_aos_msg(ProcID, <<"return 1">>),
            #{}
        ),
        Msg = generate_aos_msg(ProcID, <<"return 1+1">>),
        Iterations =
            hb:benchmark(
                fun() -> hb_converge:resolve(Initialized, Msg, #{}) end,
                BenchTime
            ),
        hb_util:eunit_print(
            "Evaluated ~p AOS messages (minimal stack) in ~p sec (~.2f msg/s)",
            [Iterations, BenchTime, Iterations / BenchTime]
        ),
        ?assert(Iterations > 10),
        ok
    end}.