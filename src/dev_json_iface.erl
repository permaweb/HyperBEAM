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
%%% <pre>
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
%%%             /Results/Data</pre>
-module(dev_json_iface).
-export([init/3, compute/3]).
%%% Public interface helpers:
-export([message_to_json_struct/2, json_to_message/2]).
%%% Test helper exports:
-export([generate_stack/1, generate_stack/2, generate_stack/3, generate_aos_msg/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Initialize the device.
init(M1, _M2, Opts) ->
    {ok, hb_ao:set(M1, #{<<"function">> => <<"handle">>}, Opts)}.

%% @doc On first pass prepare the call, on second pass get the results.
compute(M1, M2, Opts) ->
    case hb_ao:get(<<"pass">>, M1, Opts) of
        1 -> prep_call(M1, M2, Opts);
        2 -> results(M1, M2, Opts);
        _ -> {ok, M1}
    end.

%% @doc Prepare the WASM environment for execution by writing the process string and
%% the message as JSON representations into the WASM environment.
prep_call(RawM1, RawM2, Opts) ->
    M1 = hb_cache:ensure_all_loaded(RawM1, Opts),
    M2 = hb_cache:ensure_all_loaded(RawM2, Opts),
    ?event({prep_call, M1, M2, Opts}),
    Process = hb_ao:get(<<"process">>, M1, Opts#{ hashpath => ignore }),
    Message = hb_ao:get(<<"body">>, M2, Opts#{ hashpath => ignore }),
    Image = hb_ao:get(<<"process/image">>, M1, Opts),
    BlockHeight = hb_ao:get(<<"block-height">>, M2, Opts),
    Props = message_to_json_struct(denormalize_message(Message, Opts), Opts),
    MsgProps =
        Props#{
            <<"Module">> => Image,
            <<"Block-Height">> => BlockHeight
        },
    MsgJson = hb_json:encode(MsgProps),
    ProcessProps =
        #{
            <<"Process">> => message_to_json_struct(Process, Opts)
        },
    ProcessJson = hb_json:encode(ProcessProps),
    env_write(ProcessJson, MsgJson, M1, M2, Opts).

%% @doc Normalize a message for AOS-compatibility.
denormalize_message(Message, Opts) ->
    NormOwnerMsg =
        case hb_message:signers(Message, Opts) of
            [] -> Message;
            [PrimarySigner|_] ->
                {ok, _, Commitment} = hb_message:commitment(PrimarySigner, Message, Opts),
                Message#{
                    <<"owner">> => hb_util:human_id(PrimarySigner),
                    <<"signature">> =>
                        hb_ao:get(<<"signature">>, Commitment, <<>>, Opts)
                }
        end,
    NormOwnerMsg#{
        <<"id">> => hb_message:id(Message, all, Opts)
    }.

message_to_json_struct(RawMsg, Opts) ->
    message_to_json_struct(RawMsg, [owner_as_address], Opts).
message_to_json_struct(RawMsg, Features, Opts) ->
    TABM = 
        hb_message:convert(
            hb_private:reset(RawMsg),
            tabm,
            Opts
        ),
    MsgWithoutCommitments = hb_maps:without([<<"commitments">>], TABM, Opts),
    ID = hb_message:id(RawMsg, all),
    ?event({encoding, {id, ID}, {msg, RawMsg}}),
	{Owner, Signature, PublicKey} =
        case hb_message:signers(RawMsg, Opts) of
            [] -> {<<>>, <<>>, <<>>};
            [Signer|_] ->
                {ok, _, Commitment} =
                    hb_message:commitment(Signer, RawMsg, Opts),
                CommitmentSignature =
                    hb_ao:get(<<"signature">>, Commitment, <<>>, Opts),
                CommitmentKeyId =
                    dev_codec_httpsig_keyid:remove_scheme_prefix(
                        hb_ao:get(<<"keyid">>, Commitment, <<>>, Opts)
                    ),
                case lists:member(owner_as_address, Features) of
                    true -> 
                        {
                            hb_util:native_id(Signer),
                            CommitmentSignature,
                            CommitmentKeyId
                        };
                    false ->
                        CommitmentOwner =
                            hb_ao:get_first(
                                [
                                    {Commitment, <<"key">>},
                                    {Commitment, <<"owner">>}
                                ],
                                no_signing_public_key_found_in_commitment,
                                Opts
                            ),
                        {CommitmentOwner, CommitmentSignature, CommitmentKeyId}
                end
        end,
    Last =
        hb_ao:get(
            <<"anchor">>,
            {as, <<"message@1.0">>, MsgWithoutCommitments},
            <<>>,
            Opts
        ),
    DataBytes =
        hb_ao:get(
            <<"data">>,
            {as, <<"message@1.0">>, MsgWithoutCommitments},
            <<>>,
            Opts
        ),
    Data =
        case hb_util:is_printable_string(DataBytes) of
            true -> DataBytes;
            false -> null 
        end,
    Target =
        hb_ao:get(
            <<"target">>,
            {as, <<"message@1.0">>, MsgWithoutCommitments},
            <<>>,
            Opts
        ),
    % Set "From" if From-Process is Tag or set with "Owner" address
    From =
        hb_ao:get(
            <<"from-process">>,
            {as, <<"message@1.0">>, MsgWithoutCommitments},
            hb_util:encode(Owner),
            Opts
        ),
    #{
        <<"Id">> => safe_to_id(ID),
        % NOTE: In Arweave TXs, these are called "last_tx"
        <<"Anchor">> => Last,
        % NOTE: When sent to ao "Owner" is the wallet address
        <<"Owner">> => hb_util:encode(Owner),
        <<"From">> => case ?IS_ID(From) of true -> safe_to_id(From); false -> From end,
        <<"Tags">> => prepare_tags(TABM, Opts),
        <<"Target">> => safe_to_id(Target),
        <<"Data">> => Data,
        <<"Signature">> =>
            case byte_size(Signature) of
                0 -> <<>>;
                512 -> hb_util:encode(Signature);
                _ -> Signature
            end,
        <<"PublicKey">> => PublicKey
    }.
%% @doc Prepare the tags of a message as a key-value list, for use in the 
%% construction of the JSON-Struct message.
prepare_tags(Msg, Opts) ->
    % Prepare an ANS-104 message for JSON-Struct construction.
    case hb_message:commitment(#{ <<"commitment-device">> => <<"ans104@1.0">> }, Msg, Opts) of
        {ok, _, Commitment} ->
            case hb_maps:find(<<"original-tags">>, Commitment, Opts) of
                {ok, OriginalTags} ->
                    Res = hb_util:message_to_ordered_list(OriginalTags),
                    ?event({using_original_tags, Res}),
                    Res;
                error -> 
                    prepare_header_case_tags(Msg, Opts)
            end;
        _ ->
            prepare_header_case_tags(Msg, Opts)
    end.

%% @doc Convert a message without an `original-tags' field into a list of
%% key-value pairs, with the keys in HTTP header-case.
prepare_header_case_tags(TABM, Opts) ->
    % Prepare a non-ANS-104 message for JSON-Struct construction. 
    lists:map(
        fun({Name, Value}) ->
            #{
                <<"name">> => header_case_string(maybe_list_to_binary(Name)),
                <<"value">> => maybe_list_to_binary(Value)
            }
        end,
        hb_maps:to_list(
            hb_maps:without(
                [
                    <<"id">>, <<"anchor">>, <<"owner">>, <<"data">>,
                    <<"target">>, <<"signature">>, <<"commitments">>
                ],
                TABM,
                Opts
            ),
			Opts
        )
    ).

%% @doc Translates a compute result -- either from a WASM execution using the 
%% JSON-Iface, or from a `Legacy' CU -- and transforms it into a result message.
json_to_message(JSON, Opts) when is_binary(JSON) ->
    json_to_message(hb_json:decode(JSON), Opts);
json_to_message(Resp, Opts) when is_map(Resp) ->
    {ok, Data, Messages, Patches} = normalize_results(Resp),
    Output = 
        #{
            <<"outbox">> =>
                hb_maps:from_list(
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
            <<"patches">> => lists:map(fun(Patch) -> tags_to_map(Patch, Opts) end, Patches),
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

header_case_string(Key) ->
    NormKey = hb_ao:normalize_key(Key),
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
results(M1, M2, Opts) ->
    Prefix = dev_stack:prefix(M1, M2, Opts),
    Type = hb_ao:get(<<"results/", Prefix/binary, "/type">>, M1, Opts),
    Proc = hb_ao:get(<<"process">>, M1, Opts),
    case hb_ao:normalize_key(Type) of
        <<"error">> ->
            {error,
                hb_ao:set(
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
            {ok, Str} = env_read(M1, M2, Opts),
            try hb_json:decode(Str) of
                #{<<"ok">> := true, <<"response">> := Resp} ->
                    {ok, ProcessedResults} = json_to_message(Resp, Opts),
                    PostProcessed = postprocess_outbox(ProcessedResults, Proc, Opts),
                    Out = hb_ao:set(
                        M1,
                        <<"results">>,
                        PostProcessed,
                        Opts
                    ),
                    ?event(debug_iface, {results, {processed, ProcessedResults}, {out, Out}}),
                    {ok, Out}
            catch
                _:_ ->
                    ?event(error, {json_error, Str}),
                    {error,
                        hb_ao:set(
                            M1,
                            #{
                                <<"results/outbox">> => undefined,
                                <<"results/body">> =>
                                    <<"JSON error parsing result output.">>
                            },
                            Opts
                        )
                    }
            end
    end.

%% @doc Read the results out of the execution environment.
env_read(M1, M2, Opts) ->
    Prefix = dev_stack:prefix(M1, M2, Opts),
    Output = hb_ao:get(<<"results/", Prefix/binary, "/output">>, M1, Opts),
    case hb_private:get(<<Prefix/binary, "/read">>, M1, Opts) of
        not_found ->
            {ok, Output};
        ReadFn ->
            {ok, Read} = ReadFn(Output),
            {ok, Read}
    end.

%% @doc Write the message and process into the execution environment.
env_write(ProcessStr, MsgStr, Base, Req, Opts) ->
    Prefix = dev_stack:prefix(Base, Req, Opts),
    Params = 
        case hb_private:get(<<Prefix/binary, "/write">>, Base, Opts) of
            not_found ->
                [MsgStr, ProcessStr];
            WriteFn ->
                {ok, MsgJsonPtr} = WriteFn(MsgStr),
                {ok, ProcessJsonPtr} = WriteFn(ProcessStr),
                [MsgJsonPtr, ProcessJsonPtr]
        end,
    {ok,
        hb_ao:set(
            Base,
            #{
                <<"function">> => <<"handle">>,
                <<"parameters">> => Params
            },
            Opts
        )
    }.

%% @doc Normalize the results of an evaluation.
normalize_results(#{ <<"Error">> := Error }) ->
    {ok, Error, [], []};
normalize_results(Msg) ->
    try
        Output = maps:get(<<"Output">>, Msg, #{}),
        Data = maps:get(<<"data">>, Output, maps:get(<<"Data">>, Msg, <<>>)),
        {ok,
            Data,
            maps:get(<<"Messages">>, Msg, []),
            maps:get(<<"patches">>, Msg, [])
        }
    catch
        _:_ ->
            {ok, <<>>, [], []}
    end.

%% @doc After the process returns messages from an evaluation, the
%% signing node needs to add some tags to each message and spawn such that
%% the target process knows these messages are created by a process.
preprocess_results(Msg, Opts) ->
    Tags = tags_to_map(Msg, Opts),
    FilteredMsg =
        hb_maps:without(
            [<<"from-process">>, <<"from-image">>, <<"anchor">>, <<"tags">>],
            Msg,
            Opts
        ),
    hb_maps:merge(
        hb_maps:from_list(
            lists:map(
                fun({Key, Value}) ->
                    {hb_ao:normalize_key(Key), Value}
                end,
                hb_maps:to_list(FilteredMsg, Opts)
            )
        ),
        Tags,
        Opts
    ).

%% @doc Convert a message with tags into a map of their key-value pairs.
tags_to_map(Msg, Opts) ->
    NormMsg = hb_util:lower_case_key_map(
        hb_ao:normalize_keys(Msg, Opts), 
    Opts),
    RawTags = hb_maps:get(<<"tags">>, NormMsg, [], Opts),
    TagList =
        [
            {hb_maps:get(<<"name">>, Tag, Opts), hb_maps:get(<<"value">>, Tag, Opts)}
        ||
            Tag <- RawTags
        ],
    hb_maps:from_list(TagList).

%% @doc Post-process messages in the outbox to add the correct `from-process'
%% and `from-image' tags.
postprocess_outbox(Msg, Proc, Opts) ->
    AdjustedOutbox =
        hb_maps:map(
            fun(_Key, XMsg) ->
                XMsg#{
                    <<"from-process">> => hb_ao:get(id, Proc, Opts),
                    <<"from-image">> => hb_ao:get(<<"image">>, Proc, Opts)
                }
            end,
            hb_ao:get(<<"outbox">>, Msg, #{}, Opts),
            Opts
        ),
    hb_ao:set(Msg, <<"outbox">>, AdjustedOutbox, Opts).

%%% Tests

normalize_test_opts(Opts) ->
    Opts#{
        priv_wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts)
    }.

test_init() ->
    application:ensure_all_started(hb).

generate_stack(File) ->
    generate_stack(File, <<"WASM">>).
generate_stack(File, Mode) ->
    generate_stack(File, Mode, #{}).
generate_stack(File, _Mode, RawOpts) ->
    Opts = normalize_test_opts(RawOpts),
    test_init(),
    Msg0 = dev_wasm:cache_wasm_image(File, Opts),
    Image = hb_ao:get(<<"image">>, Msg0, Opts),
    Base = Msg0#{
        <<"device">> => <<"stack@1.0">>,
        <<"device-stack">> =>
            [
                <<"wasi@1.0">>,
                <<"json-iface@1.0">>,
                <<"wasm-64@1.0">>,
                <<"multipass@1.0">>
            ],
        <<"input-prefix">> => <<"process">>,
        <<"output-prefix">> => <<"wasm">>,
        <<"passes">> => 2,
        <<"stack-keys">> => [<<"init">>, <<"compute">>],
        <<"process">> => 
            hb_message:commit(#{
                <<"type">> => <<"Process">>,
                <<"image">> => Image,
                <<"scheduler">> => hb:address(),
                <<"authority">> => hb:address()
            }, Opts)
    },
    {ok, Req} = hb_ao:resolve(Base, <<"init">>, Opts),
    Req.

generate_aos_msg(ProcID, Code) ->
    generate_aos_msg(ProcID, Code, #{}).
generate_aos_msg(ProcID, Code, RawOpts) ->
    Opts = normalize_test_opts(RawOpts),
    hb_message:commit(#{
        <<"path">> => <<"compute">>,
        <<"body">> => 
            hb_message:commit(#{
                <<"action">> => <<"Eval">>,
                <<"data">> => Code,
                <<"target">> => ProcID
            }, Opts),
        <<"block-height">> => 1
    }, Opts).

basic_aos_call_test_() ->
    {timeout, 20, fun() ->
		Msg = generate_stack("test/aos-2-pure-xs.wasm"),
		Proc = hb_ao:get(<<"process">>, Msg, #{ hashpath => ignore }),
		ProcID = hb_message:id(Proc, all),
		{ok, Res} =
			hb_ao:resolve(
				Msg,
				generate_aos_msg(ProcID, <<"return 1+1">>),
				#{}
			),
		?event({res, Res}),
		Data = hb_ao:get(<<"results/data">>, Res, #{}),
		?assertEqual(<<"2">>, Data)
	end}.

aos_stack_benchmark_test_() ->
    {timeout, 20, fun() ->
        BenchTime = 5,
        Opts = #{ store => hb_test_utils:test_store() },
        RawWASMMsg = generate_stack("test/aos-2-pure-xs.wasm", <<"WASM">>, Opts),
        Proc = hb_ao:get(<<"process">>, RawWASMMsg, Opts#{ hashpath => ignore }),
        ProcID = hb_ao:get(id, Proc, Opts),
        Msg = generate_aos_msg(ProcID, <<"return 1">>, Opts),
        {ok, Initialized} =
            hb_ao:resolve(
                RawWASMMsg,
                Msg,
                Opts
            ),
        Req = generate_aos_msg(ProcID, <<"return 1+1">>, Opts),
        Iterations =
            hb_test_utils:benchmark(
                fun() -> hb_ao:resolve(Initialized, Req, Opts) end,
                BenchTime
            ),
        hb_test_utils:benchmark_print(
            <<"(Minimal AOS stack:) Evaluated">>,
            <<"messages">>,
            Iterations,
            BenchTime
        ),
        ?assert(Iterations >= 10),
        ok
    end}.