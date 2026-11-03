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
-implements(<<"json-iface@1.0">>).
-specification("../../../specs/json-iface@1.0.md").
-export([init/3, compute/3, to/3, from/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Initialize the device.
-spec init(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ function := binary(), _ => _ }}.
init(M1, _M2, Opts) ->
    {ok, hb_ao:set(M1, #{<<"function">> => <<"handle">>}, Opts)}.

%% @doc On first pass prepare the call, on second pass get the results.
-spec compute(
    #{ pass => integer(), process => #{ _ => _ }, _ => _ },
    #{ body => #{ _ => _ }, 'block-height' => integer(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ _ => _ }}.
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
    Process = hb_ao:get(<<"process">>, M1, Opts#{ <<"hashpath">> => ignore }),
    Message = hb_ao:get(<<"body">>, M2, Opts#{ <<"hashpath">> => ignore }),
    Image = hb_ao:get(<<"process/image">>, M1, Opts),
    BlockHeight = hb_ao:get(<<"block-height">>, M2, Opts),
    Props = message_to_json_struct(denormalize_message(Message, Opts), Opts),
    MsgProps =
        Props#{
            <<"Module">> => Image,
            <<"Block-Height">> => BlockHeight,
            % A process reads the tags over the fields that the node gives, so
            % the message gives no tag with the name of one.
            <<"Tags">> =>
                [
                    Tag
                ||
                    Tag = #{ <<"name">> := Name } <- maps:get(<<"Tags">>, Props),
                    not lists:member(
                        hb_util:lowercase_key(Name),
                        [
                            <<"module">>,
                            <<"block-height">>,
                            <<"id">>,
                            <<"signature">>,
                            <<"publickey">>
                        ]
                    )
                ]
        },
    MsgJson = struct_to_json(MsgProps, Opts),
    ProcessProps =
        #{
            <<"Process">> => message_to_json_struct(Process, Opts)
        },
    ProcessJson = struct_to_json(ProcessProps, Opts),
    env_write(ProcessJson, MsgJson, M1, M2, Opts).

%% @doc Encode a JSON-Struct as a JSON string. As in `json@1.0', a binary that
%% is not UTF-8 text is given as its base64url, typed `binary' in the
%% `ao-types' of the object that contains it.
struct_to_json(Struct, Opts) ->
    hb_json:encode(
        hb_message:convert(
            Struct,
            tabm,
            #{
                <<"device">> => <<"structured@1.0">>,
                <<"encode-types">> => [<<"binary">>],
                <<"bundle">> => true
            },
            Opts
        )
    ).

%% @doc Normalize a message for AOS-compatibility.
denormalize_message(Message, Opts) ->
    NormOwnerMsg =
        case hb_message:signers(Message, Opts) of
            [] -> Message;
            [PrimarySigner|_] ->
                Commitment = signer_commitment(PrimarySigner, Message, Opts),
                Message#{
                    <<"owner">> => hb_util:raw_address(PrimarySigner),
                    <<"signature">> =>
                        hb_maps:get(<<"signature">>, Commitment, <<>>, Opts)
                }
        end,
    NormOwnerMsg#{
        <<"id">> => hb_message:id(Message, all, Opts)
    }.

%% @doc Return the commitment of a message that a process sees: the one with
%% the lowest ID among the commitments of its first signer, or among all of its
%% commitments if it has no signer.
primary_commitment([], Msg, Opts) ->
    lowest_id_commitment(hb_maps:get(<<"commitments">>, Msg, #{}, Opts), Opts);
primary_commitment([Signer | _], Msg, Opts) ->
    signer_commitment(Signer, Msg, Opts).

%% @doc The commitment of a signer that a process sees: of two or more, the one
%% with the lowest ID, so that every node gives the same.
signer_commitment(Signer, Msg, Opts) ->
    lowest_id_commitment(hb_message:commitments(Signer, Msg, Opts), Opts).

%% @doc Return the commitment with the lowest ID, or an empty message if there
%% is none.
lowest_id_commitment(Commitments, Opts) ->
    case lists:sort(hb_maps:keys(Commitments, Opts)) of
        [] -> #{};
        [ID | _] -> hb_maps:get(ID, Commitments, #{}, Opts)
    end.

%% @doc The part of a message that its owner signs: the owner is the first
%% signer, so only its commitments and the keys that they commit are given. A
%% message with no signer is given as it is.
owner_view(Msg, Opts) ->
    case hb_message:signers(Msg, Opts) of
        [Owner | _] ->
            hb_util:ok(
                hb_message:with_only_committed(
                    hb_message:with_commitments(
                        #{ <<"committer">> => Owner },
                        Msg,
                        Opts
                    ),
                    Opts
                )
            );
        _ -> Msg
    end.

message_to_json_struct(RawMsg, Opts) ->
    message_to_json_struct(RawMsg, [owner_as_address], Opts).
message_to_json_struct(RawMsg, Features, Opts) ->
    Msg = owner_view(RawMsg, Opts),
    TABM = 
        hb_message:convert(
            hb_private:reset(Msg),
            tabm,
            Opts
        ),
    MsgWithoutCommitments = hb_maps:without([<<"commitments">>], TABM, Opts),
    ID = hb_message:id(RawMsg, all),
    ?event({encoding, {id, ID}, {msg, RawMsg}}),
    Signers = hb_message:signers(Msg, Opts),
    Commitment = primary_commitment(Signers, Msg, Opts),
	{Owner, Signature, PublicKey} =
        case Signers of
            [] -> {<<>>, <<>>, <<>>};
            [Signer|_] ->
                CommitmentSignature =
                    hb_maps:get(<<"signature">>, Commitment, <<>>, Opts),
                CommitmentKey =
                    hb_maps:get_first(
                        [
                            {Commitment, <<"public-key">>},
                            {Commitment, <<"owner">>}
                        ],
                        <<>>,
                        Opts
                    ),
                case lists:member(owner_as_address, Features) of
                    true -> 
                        {
                            hb_util:raw_address(Signer),
                            CommitmentSignature,
                            CommitmentKey
                        };
                    false ->
                        CommitmentOwner =
                            hb_maps:get_first(
                                [
                                    {Commitment, <<"key">>},
                                    {Commitment, <<"public-key">>},
                                    {Commitment, <<"owner">>}
                                ],
                                no_signing_public_key_found_in_commitment,
                                Opts
                            ),
                        {CommitmentOwner, CommitmentSignature, CommitmentKey}
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
        case is_binary(DataBytes) andalso hb_util:is_printable_string(DataBytes) of
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
            Owner,
            Opts
        ),
    #{
        <<"Id">> => safe_to_id(ID),
        % NOTE: In Arweave TXs, these are called "last_tx"
        <<"Anchor">> => Last,
        % NOTE: When sent to ao "Owner" is the wallet address
        <<"Owner">> => Owner,
        <<"From">> => case ?IS_ID(From) of true -> safe_to_id(From); false -> From end,
        <<"Tags">> => prepare_tags(TABM, Msg, Commitment, Opts),
        <<"Target">> => safe_to_id(raw_value(Target)),
        <<"Data">> => Data,
        <<"Signature">> =>
            case byte_size(Signature) of
                0 -> <<>>;
                512 -> hb_util:encode(Signature);
                _ -> Signature
            end,
        <<"PublicKey">> => PublicKey
    }.

%% @doc Convert a message into an AOS2-compatible JSON structure.
to(Base, Req, Opts) ->
    {ok,
        message_to_json_struct(
            hb_maps:get(<<"message">>, Req, Base, Opts),
            Opts
        )
    }.
%% @doc Prepare the tags of a message as a key-value list, for use in the 
%% construction of the JSON-Struct message. `Primary' is the commitment whose
%% `committed' list orders the tags of a message without an ANS-104 commitment.
prepare_tags(Msg, RawMsg, Primary, Opts) ->
    % Prepare an ANS-104 message for JSON-Struct construction.
    case hb_message:commitment(#{ <<"commitment-device">> => <<"ans104@1.0">> }, Msg, Opts) of
        {ok, _, Commitment} ->
            case hb_maps:find(<<"original-tags">>, Commitment, Opts) of
                {ok, OriginalTags} ->
                    Res = hb_util:message_to_ordered_list(OriginalTags),
                    ?event({using_original_tags, Res}),
                    Res;
                error -> 
                    prepare_header_case_tags(Msg, RawMsg, Commitment, Opts)
            end;
        _ ->
            prepare_header_case_tags(Msg, RawMsg, Primary, Opts)
    end.

%% @doc Convert a message without an `original-tags' field into a list of
%% key-value pairs, with the keys in HTTP header-case. The keys are in the order
%% of the commitment's `committed' list, followed in sorted order by the keys
%% that it does not list. A key with a list in the message gives a pair for each
%% element of the list, because AOS reads several values of one tag as repeated
%% tags.
prepare_header_case_tags(TABM, RawMsg, Commitment, Opts) ->
    % Remove the keys that the JSON-Struct gives as its own fields.
    Tags =
        hb_maps:without(
            [
                <<"id">>, <<"anchor">>, <<"owner">>, <<"data">>,
                <<"target">>, <<"signature">>, <<"commitments">>
            ],
            TABM,
            Opts
        ),
    Committed =
        [
            Key
        ||
            Key <-
                hb_util:message_to_ordered_list(
                    hb_maps:get(<<"committed">>, Commitment, [], Opts),
                    Opts
                ),
            hb_maps:is_key(Key, Tags, Opts)
        ],
    lists:flatmap(
        fun({Name, Value}) ->
            Values =
                case hb_maps:get(Name, RawMsg, Value, Opts) of
                    List when is_list(List) -> List;
                    _ -> [Value]
                end,
            [
                #{
                    <<"name">> => header_case_string(maybe_list_to_binary(Name)),
                    <<"value">> => raw_value(maybe_list_to_binary(Element))
                }
            ||
                Element <- Values
            ]
        end,
        [
            {Key, hb_util:ok(hb_maps:find(Key, Tags, Opts))}
        ||
            Key <- Committed
        ] ++
            hb_util:to_sorted_list(hb_maps:without(Committed, Tags, Opts), Opts)
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
            <<"patches">> => 
                lists:map(
                    fun(Patch) -> 
                        convert_unset_values(tags_to_map(Patch, Opts)) 
                    end, 
                    Patches
                ),
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

%% @doc Convert an AOS2-compatible JSON result into a message.
from(Base, Req, Opts) ->
    json_to_message(hb_maps:get(<<"json">>, Req, Base, Opts), Opts).

safe_to_id(<<>>) -> <<>>;
safe_to_id(ID) -> hb_util:human_id(ID).

%% @doc Give a committer URI as the raw address of its key, the form that AOS
%% read before committers had URIs. Other values are given unchanged.
raw_value(Value = <<"uri-did-", _/binary>>) -> hb_util:raw_address(Value);
raw_value(Value) -> Value.

maybe_list_to_binary(List) when is_list(List) ->
    % A list gives bytes only if every element is a byte. Any other list has
    % no JSON value of its own, and is given as `null', as a `data' that is
    % not a binary is.
    case is_byte_list(List) of
        true -> list_to_binary(List);
        false -> null
    end;
maybe_list_to_binary(Bin) ->
    Bin.

is_byte_list([]) ->
    true;
is_byte_list([Byte | Rest]) when is_integer(Byte), Byte >= 0, Byte =< 255 ->
    is_byte_list(Rest);
is_byte_list(_) ->
    false.

%% @doc Convert a key to HTTP header-case. A key that is not UTF-8 text is
%% kept as it is.
header_case_string(Key) ->
    NormKey = hb_ao:normalize_key(Key),
    case unicode:characters_to_binary(NormKey) =:= NormKey of
        true -> title_case(NormKey);
        false -> NormKey
    end.

%% @doc Capitalize each `-'-separated word of a UTF-8 key.
title_case(NormKey) ->
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
    Prefix =
        hb_ao:get(
            <<"output-prefix">>,
            {as, <<"message@1.0">>, M1},
            <<"">>,
            Opts
        ),
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
                    {ok, Out};
                % The call did not trap, but the process reports an error.
                % The reply's `response' is the slot's error result.
                #{<<"ok">> := false} = Reply ->
                    {error,
                        maps:get(
                            <<"response">>,
                            Reply,
                            <<"AOS execution error.">>
                        )}
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
env_read(M1, _M2, Opts) ->
    Prefix =
        hb_ao:get(
            <<"output-prefix">>,
            {as, <<"message@1.0">>, M1},
            <<"">>,
            Opts
        ),
    Output = hb_ao:get(<<"results/", Prefix/binary, "/output">>, M1, Opts),
    case hb_private:get(<<Prefix/binary, "/read">>, M1, Opts) of
        not_found ->
            {ok, Output};
        ReadFn ->
            {ok, Read} = ReadFn(Output),
            {ok, Read}
    end.

%% @doc Write the message and process into the execution environment.
env_write(ProcessStr, MsgStr, Base, _Req, Opts) ->
    Prefix =
        hb_ao:get(
            <<"output-prefix">>,
            {as, <<"message@1.0">>, Base},
            <<"">>,
            Opts
        ),
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
            [<<"from-process">>, <<"from-image">>, <<"anchor">>, <<"Tags">>],
            Msg,
            Opts
        ),
    convert_unset_values(
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
        )
    ).

%% @doc Convert a message with tags into a map of their key-value pairs. AOS
%% JSON contains the tags under `Tags'.
tags_to_map(Msg, Opts) ->
    RawTags = hb_maps:get(<<"Tags">>, Msg, [], Opts),
    TagList =
        [
            {hb_maps:get(<<"name">>, Tag, Opts), hb_maps:get(<<"value">>, Tag, Opts)}
        ||
            Tag <- RawTags
        ],
    hb_maps:from_list(TagList).

%% @doc Recursively convert <<"__ao-unset__">> binary values to the `unset'
%% atom, so that dev_message:set/3 will remove those keys during patch
%% application. This bridges the gap between Lua's nil (which removes keys
%% from tables entirely) and HyperBEAM's unset atom (which signals removal).
convert_unset_values(Map) when is_map(Map) ->
    maps:map(
        fun(_Key, <<"__ao-unset__">>) -> unset;
           (_Key, Value) when is_map(Value) -> convert_unset_values(Value);
           (_Key, Value) -> Value
        end,
        Map
    );
convert_unset_values(Other) ->
    Other.

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
        <<"priv-wallet">> => hb_opts:get(priv_wallet, hb:wallet(), Opts)
    }.

test_init() ->
    application:ensure_all_started(hb).

generate_stack(File) ->
    generate_stack(File, <<"WASM">>).
generate_stack(File, Mode) ->
    generate_stack(File, Mode, #{}).
-spec generate_stack(binary() | list(), binary(), #{ _ => _ }) ->
    #{ _ => _ }.
generate_stack(File, _Mode, RawOpts) ->
    Opts = normalize_test_opts(RawOpts),
    test_init(),
    Msg0 = hb_wasm_test_utils:cache_image(File, Opts),
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
                <<"authority">> =>
                    [
                        <<"E3FJ53E6xtAzcftBpaw2E1H4ZM9h6qy6xz9NXh5lhEQ">>,
                        hb:address()
                    ]
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
		Proc = hb_ao:get(<<"process">>, Msg, #{ <<"hashpath">> => ignore }),
		ProcID = hb_message:id(Proc, all),
		{ok, Res} =
			hb_ao:resolve(
				Msg,
				generate_aos_msg(ProcID, <<"return 1+1">>),
				#{}
			),
		?event({res, Res}),
		Data = hb_ao:get(<<"results/data">>, Res, #{}),
		?assertEqual(<<"2">>, Data),
		% AOS trusts a message signed by the second of the process's two
		% authorities.
		{ok, Trusted} =
			hb_ao:resolve(
				Msg,
				generate_aos_msg(
					ProcID,
					<<"return tostring(ao.isTrusted({ Owner = '",
						(hb:address())/binary, "' }))">>
				),
				#{}
			),
		?assertEqual(<<"true">>, hb_ao:get(<<"results/data">>, Trusted, #{})),
		% `json-iface@1.0' does not run the `device' of a message's commitment.
		AOSMsg = generate_aos_msg(ProcID, <<"return 1+1">>),
		Body = maps:get(<<"body">>, AOSMsg),
		Commitments =
			maps:map(
				fun(_, C) -> C#{ <<"device">> => <<"manifest@1.0">> } end,
				maps:get(<<"commitments">>, Body)
			),
		{ok, WithDevice} =
			hb_ao:resolve(
				Msg,
				AOSMsg#{
					<<"body">> => Body#{ <<"commitments">> => Commitments }
				},
				#{}
			),
		?assertEqual(<<"2">>, hb_ao:get(<<"results/data">>, WithDevice, #{})),
		% A message that its signer commits twice computes.
		TwiceBody =
			hb_message:commit(Body, normalize_test_opts(#{}), <<"ans104@1.0">>),
		{ok, Twice} =
			hb_ao:resolve(Msg, AOSMsg#{ <<"body">> => TwiceBody }, #{}),
		?assertEqual(<<"2">>, hb_ao:get(<<"results/data">>, Twice, #{})),
		% A `data' that is not a binary is given as `null', as one that is not
		% printable is.
		?assertMatch(
			{ok, #{ <<"Data">> := null }},
			to(#{}, #{ <<"message">> => #{ <<"data">> => 1 } }, #{})
		),
		% A tag value that is a list of byte lists gives those bytes, and any
		% other list of values is `null'.
		?assertMatch(
			{ok, #{ <<"Tags">> := [
				#{ <<"name">> := <<"Tag-A">>, <<"value">> := <<1>> }
			] }},
			to(#{}, #{ <<"message">> => #{ <<"tag-a">> => [[1]] } }, #{})
		),
		?assertMatch(
			{ok, #{ <<"Tags">> := [
				#{ <<"name">> := <<"Tag-A">>, <<"value">> := null }
			] }},
			to(#{}, #{ <<"message">> => #{ <<"tag-a">> => [[256]] } }, #{})
		),
		% A signed message gives its tags in the order of its `committed' list.
		?assertMatch(
			{ok, #{ <<"Tags">> := [
				#{ <<"name">> := <<"B">> },
				#{ <<"name">> := <<"A">> }
			] }},
			to(
				#{},
				#{
					<<"message">> =>
						hb_message:commit(
							#{ <<"b">> => <<"1">>, <<"a">> => <<"2">> },
							#{ <<"priv-wallet">> => ar_wallet:new() },
							#{
								<<"commitment-device">> => <<"httpsig@1.0">>,
								<<"committed">> => [<<"b">>, <<"a">>]
							}
						)
				},
				#{}
			)
		),
		% Of a message from two signers that commit different keys, only the
		% owner's key is given.
		Second = #{ <<"priv-wallet">> => ar_wallet:new() },
		Signed =
			hb_message:commit(
				#{ <<"a">> => <<"1">>, <<"b">> => <<"2">> },
				normalize_test_opts(#{}),
				#{
					<<"commitment-device">> => <<"httpsig@1.0">>,
					<<"committed">> => [<<"a">>]
				}
			),
		Both =
			hb_message:commit(
				Signed,
				Second,
				#{
					<<"commitment-device">> => <<"httpsig@1.0">>,
					<<"committed">> => [<<"b">>]
				}
			),
		{ok, #{ <<"Owner">> := Owner, <<"Tags">> := Tags }} =
			to(#{}, #{ <<"message">> => Both }, #{}),
		OwnerKey =
			case Owner =:= hb:address(maps:get(<<"priv-wallet">>, Second)) of
				true -> <<"B">>;
				false -> <<"A">>
			end,
		?assertEqual([OwnerKey], [Name || #{ <<"name">> := Name } <- Tags]),
		% Of a message with one signer and an unsigned commitment over another
		% key, only the signer's key is given.
		OneSigner =
			hb_message:commit(
				Signed,
				normalize_test_opts(#{}),
				#{ <<"type">> => <<"unsigned">>, <<"committed">> => [<<"b">>] }
			),
		{ok, #{ <<"Tags">> := OneTags }} =
			to(#{}, #{ <<"message">> => OneSigner }, #{}),
		?assertEqual([<<"A">>], [Name || #{ <<"name">> := Name } <- OneTags])
	end}.

%% @doc AOS reads the address of the key that signed a message, in the form
%% that the key's type gives it, for every type of key.
signer_address_of_each_key_type_test() ->
    test_init(),
    lists:foreach(
        fun(File) ->
            {ok, Bin} = file:read_file(<<"test/arbundles.js/", File/binary>>),
            TX = ar_bundles:deserialize(Bin),
            Address =
                hb_util:human_id(
                    ar_wallet:to_address(TX#tx.owner, TX#tx.signature_type)
                ),
            Msg =
                hb_message:convert(
                    TX,
                    <<"structured@1.0">>,
                    <<"ans104@1.0">>,
                    #{}
                ),
            ?assertMatch(
                {ok, #{ <<"Owner">> := Address, <<"From">> := Address }},
                to(#{}, #{ <<"message">> => Msg }, #{})
            )
        end,
        [
            <<"ans104-item-ed25519.bin">>,
            <<"ans104-item-ethereum.bin">>,
            <<"ans104-item-typed-ethereum.bin">>,
            <<"ans104-item-solana.bin">>
        ]
    ),
    % A committer URI in the target or a tag is given as the raw address.
    Ethereum = <<"0xAb5801a7D398351b8bE11C439e05C5B3259aeC9B">>,
    Solana = <<"7S3P4HxJpyyigGzodYwHtCxZyUQe9JiBMHyRWXArAaKv">>,
    Arweave = hb_util:human_id(crypto:strong_rand_bytes(32)),
    URIMsg =
        hb_message:commit(
            #{
                <<"target">> => hb_committer:uri(Ethereum),
                <<"recipient">> => hb_committer:uri(Solana),
                <<"sender">> => hb_committer:uri(Arweave)
            },
            #{ <<"priv-wallet">> => ar_wallet:new() }
        ),
    {ok, URIJSON} = to(#{}, #{ <<"message">> => URIMsg }, #{}),
    ?assertMatch(#{ <<"Target">> := Ethereum }, URIJSON),
    ?assertEqual(
        [Solana, Arweave],
        [
            Value
        ||
            #{ <<"name">> := Name, <<"value">> := Value } <-
                maps:get(<<"Tags">>, URIJSON),
            lists:member(Name, [<<"Recipient">>, <<"Sender">>])
        ]
    ).

aos_stack_benchmark_test_() ->
    {timeout, 20, fun() ->
        BenchTime = 0.25,
        Opts = #{ <<"store">> => hb_test_utils:test_store() },
        RawWASMMsg = generate_stack("test/aos-2-pure-xs.wasm", <<"WASM">>, Opts),
        Proc =
            hb_ao:get(
                <<"process">>,
                RawWASMMsg,
                Opts#{ <<"hashpath">> => ignore }
            ),
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
        ?assert(Iterations >= 1),
        ok
    end}.
