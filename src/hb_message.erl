
%%% @doc This module acts an adapter between messages, as modeled in the
%%% Converge Protocol, and their uderlying binary representations.
%%% See `docs/converge-protocol.md' for details on Converge. Unless you are
%%% implementing a new message serialization format, you should not need to 
%%% interact with this module directly. Instead, use the `hb_converge'
%%% interfaces to interact with all messages. The `dev_message' module
%%% implements a device interface for handling messages as the default Converge
%%% device.
-module(hb_message).
-export([load/2, sign/2, verify/1, match/2, type/1]).
-export([serialize/1, serialize/2, deserialize/1, deserialize/2, signers/1]).
-export([message_to_tx/1, tx_to_message/1, minimize/1]).
-export([message_to_http/1]).
%%% Debugging tools:
-export([print/1, format/1, format/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 128).
%% The list of TX fields that users can set directly.
-define(TX_KEYS,
    [id, unsigned_id, last_tx, owner, target, signature]).
-define(FILTERED_TAGS,
    [
        <<"Converge-Large-Binary">>,
        <<"Bundle-Format">>,
        <<"Bundle-Map">>,
        <<"Bundle-Version">>,
        <<"Converge-Type:">>
    ]
).
-define(REGEN_KEYS, [id, unsigned_id]).

%% @doc Pretty-print a message.
print(Msg) -> print(Msg, 0).
print(Msg, Indent) ->
    io:format(standard_error, "~s", [lists:flatten(format(Msg, Indent))]).

%% @doc Format a message for printing, optionally taking an indentation level
%% to start from.
format(Item) -> format(Item, 0).
format(Bin, Indent) when is_binary(Bin) ->
    hb_util:format_indented(
        hb_util:format_binary(Bin),
        Indent
    );
format(Map, Indent) when is_map(Map) ->
    Header = hb_util:format_indented("Message {~n", Indent),
    Res = lists:map(
        fun({Key, Val}) ->
            NormKey = hb_converge:to_key(Key, #{ error_strategy => ignore }),
            KeyStr = 
                case NormKey of
                    Key ->
                        io_lib:format("~p", [NormKey]);
                    undefined ->
                        io_lib:format("~p [!!! INVALID KEY !!!]", [Key]);
                    _ ->
                        io_lib:format("~p [raw: ~p]", [NormKey, Key])
                end,
            hb_util:format_indented(
                "~s := ~s~n",
                [
                    lists:flatten(KeyStr),
                    case Val of
                        NextMap when is_map(NextMap) ->
                            hb_util:format_map(NextMap, Indent + 2);
                        Bin when is_binary(Bin) ->
                            hb_util:format_binary(Bin);
                        Other ->
                            io_lib:format("~p", [Other])
                    end
                ],
                Indent + 1
            )
        end,
        maps:to_list(Map)
    ),
    case Res of
        [] -> "[Empty map]";
        _ ->
            lists:flatten(
                Header ++ Res ++ hb_util:format_indented("}", Indent)
            )
    end;
format(Item, Indent) ->
    % Whatever we have is not a message map.
    hb_util:format_indented("[UNEXPECTED VALUE] ~p", [Item], Indent).

%% @doc Return the signers of a message. For now, this is just the signer
%% of the message itself. In the future, we will support multiple signers.
signers(Msg) when is_map(Msg) ->
    case ar_bundles:signer(message_to_tx(Msg)) of
        undefined -> [];
        Signer -> [Signer]
    end;
signers(TX) when is_record(TX, tx) ->
    ar_bundles:signer(TX);
signers(_) -> [].

%% @doc Sign a message with the given wallet.
sign(Msg, Wallet) ->
    tx_to_message(ar_bundles:sign_item(message_to_tx(Msg), Wallet)).

%% @doc Verify a message.
verify(Msg) ->
    ar_bundles:verify_item(message_to_tx(Msg)).

%% @doc Return the type of a message.
type(TX) when is_record(TX, tx) -> tx;
type(Binary) when is_binary(Binary) -> binary;
type(Msg) when is_map(Msg) ->
    IsDeep = lists:any(
        fun({_, Value}) -> is_map(Value) end,
        lists:filter(
            fun({Key, _}) -> not hb_private:is_private(Key) end,
            maps:to_list(Msg)
        )
    ),
    case IsDeep of
        true -> deep;
        false -> shallow
    end.

%% @doc Load a message from the cache.
load(Store, ID) when is_binary(ID)
        andalso (byte_size(ID) == 43 orelse byte_size(ID) == 32) ->
    tx_to_message(hb_cache:read(Store, ID));
load(Store, Path) ->
    tx_to_message(hb_cache:read(Store, Path)).

%% @doc Check if two maps match, including recursively checking nested maps.
match(Map1, Map2) ->
    Keys1 = maps:keys(NormMap1 = minimize(normalize(Map1))),
    Keys2 = maps:keys(NormMap2 = minimize(normalize(Map2))),
    case Keys1 == Keys2 of
        true ->
            lists:all(
                fun(Key) ->
                    Val1 = maps:get(Key, NormMap1, not_found),
                    Val2 = maps:get(Key, NormMap2, not_found),
                    case is_map(Val1) andalso is_map(Val2) of
                        true -> match(Val1, Val2);
                        false ->
                            case Val1 == Val2 of
                                true -> true;
                                false ->
                                    ?event(
                                        {value_mismatch,
                                            {explicit, {Key, Val1, Val2}}
                                        }
                                    ),
                                    false
                            end
                    end
                end,
                Keys1
            );
        false ->
            ?event({keys_mismatch, Keys1, Keys2}),
            false
    end.
	
matchable_keys(Map) ->
    lists:sort(lists:map(fun hb_converge:key_to_binary/1, maps:keys(Map))).

%% @doc Normalize the keys in a map. Also takes a list of keys and returns a
%% sorted list of normalized keys if the input is a list.
normalize_keys(Keys) when is_list(Keys) ->
    lists:sort(lists:map(fun hb_converge:key_to_binary/1, Keys));
normalize_keys(Map) ->
    maps:from_list(
        lists:map(
            fun({Key, Value}) ->
                {hb_converge:key_to_binary(Key), Value}
            end,
            maps:to_list(Map)
        )
    ).

%% @doc Remove keys from the map that can be regenerated.
minimize(RawVal) when not is_map(RawVal) -> RawVal;
minimize(Map) ->
    NormRegenKeys = normalize_keys(?REGEN_KEYS),
    maps:filter(
        fun(Key, _) ->
            (not lists:member(hb_converge:key_to_binary(Key), NormRegenKeys))
                andalso (not hb_private:is_private(Key))
        end,
        maps:map(fun(_K, V) -> minimize(V) end, Map)
    ).

%% @doc Return a map with only the keys that necessary, without those that can
%% be regenerated.
normalize(Map) ->
    NormalizedMap = normalize_keys(Map),
    FilteredMap = filter_default_tx_keys(NormalizedMap),
    maps:with(matchable_keys(FilteredMap), FilteredMap).

%% @doc Remove keys from a map that have the default values found in the tx
%% record.
filter_default_tx_keys(Map) ->
    DefaultsMap = default_tx_message(),
    maps:filter(
        fun(Key, Value) ->
            case maps:find(hb_converge:key_to_binary(Key), DefaultsMap) of
                {ok, Value} -> false;
                _ -> true
            end
        end,
        Map
    ).

%% @doc Get the normalized fields and default values of the tx record.
default_tx_message() ->
    normalize_keys(maps:from_list(default_tx_list())).

%% @doc Get the ordered list of fields and default values of the tx record.
default_tx_list() ->
    lists:zip(record_info(fields, tx), tl(tuple_to_list(#tx{}))).

%% @doc Serialize a message to a binary representation, either as JSON or the
%% binary format native to the message/bundles spec in use.
serialize(M) -> serialize(M, binary).
serialize(M, json) ->
    jiffy:encode(ar_bundles:item_to_json_struct(M));
serialize(M, binary) ->
    ar_bundles:serialize(message_to_tx(M)).

%% @doc Deserialize a message from a binary representation.
deserialize(B) -> deserialize(B, binary).
deserialize(J, json) ->
    {JSONStruct} = jiffy:decode(J),
    ar_bundles:json_struct_to_item(JSONStruct);
deserialize(B, binary) ->
    tx_to_message(ar_bundles:deserialize(B)).

%% @doc Internal helper to translate a message to its #tx record representation,
%% which can then be used by ar_bundles to serialize the message. We call the 
%% message's device in order to get the keys that we will be checkpointing. We 
%% do this recursively to handle nested messages. The base case is that we hit
%% a binary, which we return as is.
message_to_tx(Binary) when is_binary(Binary) ->
    % ar_bundles cannot serialize just a simple binary or get an ID for it, so
    % we turn it into a TX record with a special tag, tx_to_message will
    % identify this tag and extract just the binary.
    #tx{
        tags= [{<<"Converge-Type">>, <<"Binary">>}],
        data = Binary
    };
message_to_tx(TX) when is_record(TX, tx) -> TX;
message_to_tx(RawM) when is_map(RawM) ->
    % The path is a special case so we normalized it first. It may have been
    % modified by `hb_converge` in order to set it to the current key that is
    % being executed. We should check whether the path is in the
    % `priv/Converge/Original-Path` field, and if so, use that instead of the
    % stated path. This normalizes the path, such that the signed message will
    % continue to validate correctly.
    M =
        case {maps:find(path, RawM), hb_private:from_message(RawM)} of
            {{ok, _}, #{ <<"Converge">> := #{ <<"Original-Path">> := Path } }} ->
                maps:put(path, Path, RawM);
            _ -> RawM
        end,
    % Get the keys that will be serialized, excluding private keys. Note that
    % we do not call hb_converge:resolve here because we want to include the 'raw'
    % data from the message, not the computable values.
    Keys = maps:keys(M),
    % Translate the keys into a binary map. If a key has a value that is a map,
    % we recursively turn its children into messages. Notably, we do not simply
    % call message_to_tx/1 on the inner map because that would lead to adding
    % an extra layer of nesting to the data.
    %?event({message_to_tx, {keys, Keys}, {map, M}}),
    MsgKeyMap =
        maps:from_list(lists:flatten(
            lists:map(
                fun(Key) ->
                    case maps:find(Key, M) of
                        {ok, Map} when is_map(Map) ->
                            {Key, message_to_tx(Map)};
                        {ok, <<>>} ->
                            BinKey = hb_converge:key_to_binary(Key),
                            {<<"Converge-Type:", BinKey/binary>>, <<"Empty-Binary">>};
                        {ok, []} ->
                            BinKey = hb_converge:key_to_binary(Key),
                            {<<"Converge-Type:", BinKey/binary>>, <<"Empty-List">>};
                        {ok, Value} when is_binary(Value) ->
                            {Key, Value};
                        {ok, Value} when
                                is_atom(Value) or is_integer(Value)
                                or is_list(Value) ->
                            ItemKey = hb_converge:key_to_binary(Key),
                            {Type, BinaryValue} = encode_value(Value),
                            [
                                {<<"Converge-Type:", ItemKey/binary>>, Type},
                                {ItemKey, BinaryValue}
                            ];
                        {ok, _} ->
                            []
                    end
                end,
                lists:filter(
                    fun(Key) ->
                        % Filter keys that the user could set directly, but
                        % should be regenerated when moving msg -> TX, as well
                        % as private keys.
                        not lists:member(Key, ?REGEN_KEYS) andalso
                            not hb_private:is_private(Key)
                    end,
                    Keys
                )
            )
        )),
    NormalizedMsgKeyMap = normalize_keys(MsgKeyMap),
    % Iterate through the default fields, replacing them with the values from
    % the message map if they are present.
    {RemainingMap, BaseTXList} = lists:foldl(
        fun({Field, Default}, {RemMap, Acc}) ->
            NormKey = hb_converge:key_to_binary(Field),
            case maps:find(NormKey, NormalizedMsgKeyMap) of
                error -> {RemMap, [Default | Acc]};
                {ok, Value} -> {maps:remove(NormKey, RemMap), [Value | Acc]}
            end
        end,
        {NormalizedMsgKeyMap, []},
        default_tx_list()
    ),
    % Rebuild the tx record from the new list of fields and values.
    TXWithoutTags = list_to_tuple([tx | lists:reverse(BaseTXList)]),
    % Calculate which set of the remaining keys will be used as tags.
    {Tags, RawDataItems} =
        lists:partition(
            fun({_Key, Value}) when is_binary(Value) ->
                    case unicode:characters_to_binary(Value) of
                        {error, _, _} -> false;
                        _ -> byte_size(Value) =< ?MAX_TAG_VAL
                    end;
                (_) -> false
            end,
            [ 
                    {Key, maps:get(Key, RemainingMap)}
                ||
                    Key <- maps:keys(RemainingMap)
            ]
        ),
    % We don't let the user set the tags directly, but they can instead set any
    % number of keys to short binary values, which will be included as tags.
    TX = TXWithoutTags#tx { tags = Tags },
    % Recursively turn the remaining data items into tx records.
    DataItems = maps:from_list(lists:map(
        fun({Key, Value}) ->
            {Key, message_to_tx(Value)}
        end,
        RawDataItems
    )),
    % Set the data based on the remaining keys.
    TXWithData = 
        case {TX#tx.data, maps:size(DataItems)} of
            {Binary, 0} when is_binary(Binary) ->
                TX;
            {?DEFAULT_DATA, _} ->
                TX#tx { data = DataItems };
            {Data, _} when is_map(Data) ->
                TX#tx { data = maps:merge(Data, DataItems) };
            {Data, _} when is_record(Data, tx) ->
                TX#tx { data = DataItems#{ data => Data } };
            {Data, _} when is_binary(Data) ->
                TX#tx { data = DataItems#{ data => message_to_tx(Data) } }
        end,
    % ar_bundles:reset_ids(ar_bundles:normalize(TXWithData));
    Res = try ar_bundles:reset_ids(ar_bundles:normalize(TXWithData))
    catch
        _:Error ->
            ?event(debug, {{reset_ids_error, Error}, {tx_without_data, TX}}),
            ?event(debug, {prepared_tx_before_ids,
                {tags, {explicit, TXWithData#tx.tags}},
                {data, TXWithData#tx.data}
            }),
            throw(Error)
    end,
    %?event(debug, {result, {explicit, Res}}),
    Res;
message_to_tx(Other) ->
    ?event({unexpected_message_form, {explicit, Other}}),
    throw(invalid_tx).

%%% @doc Maps the native HyperBEAM Message 
%%% to an "HTTP" message. Every HyperBEAM Message is mapped to
%%% an HTTP multipart message. The HTTP Message data structure 
%%% has the following shape:
%%% 
%%% #{ 
%%%     headers => [
%%%         {<<"Example-Header">>, <<"Value">>}
%%%     ],
%%%     body: <<"Some body">>
%%% }
%%% 
%%% 
%%% For each HyperBEAM Message Key:
%%% 
%%% The Key will be ignored if:
%%% - The field is private (according to hb_private:is_private/1)
%%% - The field is one of ?REGEN_KEYS
%%%
%%% The Key/Value Pair will be encoded according to the following rules:
%%%     "signatures" -> {SignatureInput, Signature} header Tuples, each encoded as a Structured Field Dictionary
%%%     "body" ->
%%%         - if a map, then recursively encode as its own HyperBEAM message
%%%         - otherwise encode as a normal field
%%%     _ -> encode as a normal field
%%% 
%%% Each field will be mapped to the HTTP Message according to the following rules:
%%%     "body" -> always encoded as part of the body as with Content-Disposition type of "inline"
%%%     _ ->
%%%         - If the byte size of the value is less than the ?MAX_TAG_VALUE, then encode as a header,
%%%         also attempting to encode as a structured field
%%%         - Otherwise encode the value as a part in the multipart response
%%% 
message_to_http(Msg) ->
    PublicMsg = hb_private:reset(Msg),
    MinimizedMsg = minimize(PublicMsg),
    NormalizedMsg = normalize_keys(MinimizedMsg),
    Http = lists:foldl(
        fun
            ({<<"signatures">>, Signatures}, Http) -> signatures_to_http(Http, Signatures);
            ({<<"body">>, Body}, Http) -> body_to_http(Http, Body);
            ({Name, Value}, Http) -> field_to_http(Http, {Name, Value}, #{})
        end,
        #{
            headers => [],
            body => #{}
        },
        maps:to_list(NormalizedMsg)    
    ),
    Body = maps:get(body, Http),
    NewHttp = case maps:size(Body) of
        0 -> maps:put(body, <<>>, Http);
        _ ->
            ?no_prod("What should the Boundary be?"),
            Boundary = base64:encode(crypto:strong_rand_bytes(32)),
            % Transform body into a binary, delimiting each part,
            % with the Boundary
            Bin = maps:fold(
                fun (_, BodyPart, Acc) ->
                    <<Acc/binary, "--", Boundary/binary, "\n", BodyPart/binary, "\n">>
                end,
                <<>>,
                Body
            ),
            % TODO: I _think_ this is needed, according to spec
            % End the body with a final terminating Boundary
            EncodedBody = <<Bin/binary, "--", Boundary/binary, "--">>,
            #{ 
                headers => [
                    {<<"Content-Type">>, <<"multipart/form-data; boundary=", "\"" , Boundary/binary, "\"">>}
                    | maps:get(headers, Http)
                ],
                body => EncodedBody
            }
    end,
    NewHttp.

encode_http_msg (#{ headers := SubHeaders, body := SubBody }) ->
    % Serialize the headers, to be included in the part of the multipart response
    EncodedHeaders = lists:foldl(
        fun ({HeaderName, HeaderValue}, Acc) ->
            <<Acc/binary, "\n", HeaderName/binary, ": ", HeaderValue/binary>>
        end,
        <<>>,
        SubHeaders
    ),
    % Some-Headers: some-value
    % Content-Type: image/png
    % 
    % <body>
    <<EncodedHeaders/binary, <<"\n\n">>, SubBody/binary>>.

signatures_to_http(Http, Signatures) when is_map(Signatures) ->
    signatures_to_http(Http, maps:to_list(Signatures));
signatures_to_http(Http, Signatures) when is_list(Signatures) ->
    {SfSigInputs, SfSigs} = lists:foldl(
        fun ({SigName, SignatureMap = #{ inputs := Inputs, signature := Signature }}, {SfSigInputs, SfSigs}) ->
            NextSigInput = hb_http_signature:sf_signature_params(Inputs, SignatureMap),
            NextSig = hb_http_signature:sf_signature(Signature),
            NextName = hb_converge:key_to_binary(SigName),
            {
                [{NextName, NextSigInput} | SfSigInputs],
                [{NextName, NextSig} | SfSigs] 
            }
        end,
        % Start with empty Structured Field Dictionaries
        {[], []},
        Signatures
    ),
    % Signature and Signature-Input are always encoded as Structured Field dictionaries, and then
    % each transmitted either as a header, or as a part in the multi-part body
    WithSig = field_to_http(Http, {<<"Signature">>, hb_structured_fields:dictionary(SfSigs)}, #{}),
    WithSigAndInput = field_to_http(WithSig, {<<"Signature-Input">>, hb_http_structured_fields:dictionary(SfSigInputs)}, #{}),
    WithSigAndInput.

body_to_http(Http, Body) when is_map(Body) ->
    Disposition = <<"Content-Disposition: inline">>,
    SubHttp = message_to_http(Body),
    EncodedBody = encode_http_msg(SubHttp),
    field_to_http(Http, {<<"body">>, EncodedBody}, #{ disposition => Disposition, where => body });
body_to_http(Http, Body) when is_binary(Body) ->
    Disposition = <<"Content-Disposition: inline">>,
    field_to_http(Http, {<<"body">>, Body}, #{ disposition => Disposition, where => body }).

field_to_http(Http, {Name, MapOrList}, Opts) when is_map(MapOrList) orelse is_list(MapOrList) ->
    {Mapper, Parser} = case MapOrList of
        Map when is_map(Map) -> {fun hb_http_structured_fields:to_dictionary/1, fun hb_http_structured_fields:dictionary/1};
        List when is_list(List) -> {fun hb_http_structured_fields:to_list/1, fun hb_http_structured_fields:list/1}
    end,
    MaybeBin = case Mapper(MapOrList) of
        {ok, Sf} ->
            % Check the size of the encoded dictionary, and signal to store
            % the map as an Structured Field encoded dictionary in the header
            %
            % Otherwise, we will need to convert the Map into its own HTTP message
            % and append as a part of the body in the parent multi-part msg
            EncodedSf = Parser(Sf),
            case byte_size(EncodedSf) of
                Fits when Fits =< ?MAX_TAG_VAL -> EncodedSf;
                _ -> undefined
            end;
        _ -> undefined
    end,
    ?no_prod("What should the name be?"),
    NormalizedName = hb_converge:key_to_binary(Name),
    case MaybeBin of
        Bin when is_binary(Bin) ->
            field_to_http(Http, {NormalizedName, Bin}, Opts);
        undefined when is_map(MapOrList) ->
            SubHttp = message_to_http(MapOrList),
            EncodedHttpMap = encode_http_msg(SubHttp),
            % Append to the serialized field to the parent body, as a part
            field_to_http(Http, {Name, EncodedHttpMap}, Opts);
        undefined when is_list(MapOrList) ->
            ?no_prod("how do we further encode a list?"),
            not_implemented
    end;
% field_to_http(Http, {Name, List}, Opts) when is_list(List) ->
%     {not_implemented, List};
field_to_http(Http, {Name, Value}, Opts) ->
    NormalizedName = hb_converge:key_to_binary(Name),
    NormalizedValue = hb_converge:key_to_binary(Value),

    % Depending on the size of the value, we may need to force
    % the value to be encoded into the body.
    %
    % Otherwise, we place the value according to Opts,
    % defaulting to header
    DefaultWhere = case byte_size(NormalizedValue) of
        Fits when Fits =< ?MAX_TAG_VAL -> headers;
        _ -> maps:get(where, Opts, headers)
    end,

    case maps:get(where, Opts, DefaultWhere) of
        headers ->
            Headers = maps:get(headers, Http),
            NewHeaders = lists:append(Headers, [{NormalizedName, NormalizedValue}]),
            maps:put(headers, NewHeaders, Http);
        % Append the value as a part of the multipart body
        %
        % We'll need to prepend a Content-Disposition header to the part, using
        % the field name as the form part name. (see https://www.rfc-editor.org/rfc/rfc7578#section-4.2).
        % We allow the caller to provide a Content-Disposition in Opts, but default
        % to appending as a field on the form-data
        body ->
            Body = maps:get(body, Http),
            Disposition = maps:get(disposition, Opts, <<"Content-Disposition: form-data; name=", NormalizedName/binary>>),
            BodyPart = <<Disposition/binary, "\n\n", NormalizedValue/binary>>,
            NewBody = maps:put(NormalizedName, BodyPart, Body),
            maps:put(body, NewBody, Http)
    end.

http_to_msg (#{ headers := Headers, body := Body }) ->
    ContentType = lists:keyfind(<<"Content-Type">>, 1, Headers),
    {item, _, Params} = hb_http_structured_fields:item(ContentType),
    Parts = case lists:keyfind(<<"boundary">>, 1, Params) of
        false -> [Body];
        {_, Boundary} ->
            % The first part will always be empty (since the boundary is always placed first
            % in the body
            [_, P] = binary:split(Body, <<"--", Boundary/binary>>),
            % The last part MIGHT be "--" for the terminating boundary.
            %
            % So we need to check and potentially trim off the last
            % element
            TrimmedParts = case lists:last(P) of
                <<"--">> ->
                    lists:sublist(P, length(P) - 1);
                _ -> P
            end
    end,
    % TODO: WIP NOT DONE
    % Take each part and convert into a HB message
    %   - headers become fields
    %       - maybe parse as structured fields?
    %   - parts become fields (recursively parsed)
    %       - "inline" part becomes top level "body" field
    %   - "Signature" & "Signature-Input" are parsed as SF dictionaries and become "Signatures" on HB message

    not_implemented.

%% @doc Convert non-binary values to binary for serialization.
decode_value(integer, Value) ->
    {item, Number, _} = hb_http_structured_fields:parse_item(Value),
    Number;
decode_value(float, Value) ->
    binary_to_float(Value);
decode_value(atom, Value) ->
    {item, {string, AtomString}, _} =
        hb_http_structured_fields:parse_item(Value),
    binary_to_existing_atom(AtomString, latin1);
decode_value(list, Value) ->
    lists:map(
        fun({item, {string, <<"(Converge-Type: ", Rest/binary>>}, _}) ->
            [Type, Item] = binary:split(Rest, <<") ">>),
            decode_value(
                list_to_existing_atom(
                    string:to_lower(binary_to_list(Type))
                ),
                Item
            );
           ({item, {string, Binary}, _}) -> Binary
        end,
        hb_http_structured_fields:parse_list(iolist_to_binary(Value))
    );
decode_value(OtherType, Value) ->
    ?event({unexpected_type, OtherType, Value}),
    throw({unexpected_type, OtherType, Value}).

%% @doc Convert a term to a binary representation, emitting its type for
%% serialization as a separate tag.
encode_value(Value) when is_integer(Value) ->
    [Encoded, _] = hb_http_structured_fields:item({item, Value, []}),
    {<<"Integer">>, Encoded};
encode_value(Value) when is_float(Value) ->
    ?no_prod("Must use structured field representation for floats!"),
    {<<"Float">>, float_to_binary(Value)};
encode_value(Value) when is_atom(Value) ->
    [EncodedIOList, _] =
        hb_http_structured_fields:item(
            {item, {string, atom_to_binary(Value, latin1)}, []}),
    Encoded = list_to_binary(EncodedIOList),
    {<<"Atom">>, Encoded};
encode_value(Values) when is_list(Values) ->
    EncodedValues =
        lists:map(
            fun(Bin) when is_binary(Bin) -> {item, {string, Bin}, []};
               (Item) ->
                {Type, Encoded} = encode_value(Item),
                {
                    item,
                    {
                        string,
                        <<
                            "(Converge-Type: ", Type/binary, ") ",
                            Encoded/binary
                        >>
                    },
                    []
                }
            end,
            Values
        ),
    EncodedList = hb_http_structured_fields:list(EncodedValues),
    {<<"List">>, iolist_to_binary(EncodedList)};
encode_value(Value) when is_binary(Value) ->
    {<<"Binary">>, Value};
encode_value(Value) ->
    Value.

%% @doc Convert a #tx record into a message map recursively.
tx_to_message(Binary) when is_binary(Binary) -> Binary;
tx_to_message(TX) when is_record(TX, tx) ->
    case lists:keyfind(<<"Converge-Type">>, 1, TX#tx.tags) of
        false ->
            do_tx_to_message(TX);
        {<<"Converge-Type">>, <<"Binary">>} ->
            TX#tx.data
    end.
do_tx_to_message(RawTX) ->
    % Ensure the TX is fully deserialized.
    TX0 = ar_bundles:deserialize(ar_bundles:normalize(RawTX)),
    % Find empty tags and replace them with the empty binary.
    TX =
        TX0#tx {
            tags = lists:map(
                fun({<<"Converge-Type:", Key/binary>>, <<"Empty-Binary">>}) ->
                    {Key, <<>>};
                ({<<"Converge-Type:", Key/binary>>, <<"Empty-List">>}) ->
                    {Key, []};
                (Tag) -> Tag
                end,
                TX0#tx.tags
            )
        },
    % We need to generate a map from each field of the tx record.
    % Get the raw fields and values of the tx record and pair them.
    Fields = record_info(fields, tx),
    Values = tl(tuple_to_list(TX)),
    TXKeyVals = lists:zip(Fields, Values),
    % Convert the list of key-value pairs into a map.
    UnfilteredTXMap = maps:from_list(TXKeyVals),
    TXMap = maps:with(?TX_KEYS, UnfilteredTXMap),
    {TXTagsUnparsed, FilteredTags} = lists:partition(
        fun({Key, _}) ->
            not lists:any(
                fun(FilterKey) ->
                    case binary:longest_common_prefix([Key, FilterKey]) of
                        Length when Length == byte_size(FilterKey) ->
                            true;
                        _ ->
                            false
                    end
                end,
                ?FILTERED_TAGS
            );
            (_) -> true
        end,
        TX#tx.tags
    ),
    % Parse tags that have a "Converge-Type:" prefix.
    TagTypes =
        [
            {Name, list_to_existing_atom(
                string:to_lower(binary_to_list(Value))
            )}
        ||
            {<<"Converge-Type:", Name/binary>>, Value} <- FilteredTags
        ],
    TXTags =
        lists:map(
            fun({Name, BinaryValue}) ->
                case lists:keyfind(Name, 1, TagTypes) of
                    false -> {Name, BinaryValue};
                    {_, Type} -> {Name, decode_value(Type, BinaryValue)}
                end
            end,
            TXTagsUnparsed
        ),
    % Next, merge the tags into the map.
    MapWithoutData = maps:merge(TXMap, maps:from_list(TXTags)),
    % Finally, merge the data into the map.
    normalize(
        case TX#tx.data of
            Data when is_map(Data) ->
                % If the data is a map, we need to recursively turn its children
                % into messages from their tx representations.
                ?event({merging_map_and_data, MapWithoutData, Data}),
                maps:merge(
                    MapWithoutData,
                    maps:map(
                        fun(_, InnerValue) ->
                            tx_to_message(InnerValue)
                        end,
                        Data
                    )
                );
            Data when Data == ?DEFAULT_DATA ->
                MapWithoutData;
            Data when is_binary(Data) ->
                MapWithoutData#{ data => Data };
            Data ->
                ?event({unexpected_data_type, {explicit, Data}}),
                ?event({was_processing, {explicit, TX}}),
                throw(invalid_tx)
        end
    ).

%%% Tests

basic_map_to_tx_test() ->
    hb:init(),
    Msg = #{ normal_key => <<"NORMAL_VALUE">> },
    TX = message_to_tx(Msg),
    ?assertEqual([{<<"normal_key">>, <<"NORMAL_VALUE">>}], TX#tx.tags).

%% @doc Test that the filter_default_tx_keys/1 function removes TX fields
%% that have the default values found in the tx record, but not those that
%% have been set by the user.
default_tx_keys_removed_test() ->
    TX = #tx { unsigned_id = << 1:256 >>, last_tx = << 2:256 >> },
    TXMap = #{
        unsigned_id => TX#tx.unsigned_id,
        last_tx => TX#tx.last_tx,
        <<"owner">> => TX#tx.owner,
        <<"target">> => TX#tx.target,
        data => TX#tx.data
    },
    FilteredMap = filter_default_tx_keys(TXMap),
    ?assertEqual(<< 1:256 >>, maps:get(unsigned_id, FilteredMap)),
    ?assertEqual(<< 2:256 >>, maps:get(last_tx, FilteredMap, not_found)),
    ?assertEqual(not_found, maps:get(<<"owner">>, FilteredMap, not_found)),
    ?assertEqual(not_found, maps:get(<<"target">>, FilteredMap, not_found)).

minimization_test() ->
    Msg = #{
        unsigned_id => << 1:256 >>,
        <<"id">> => << 2:256 >>
    },
    MinimizedMsg = minimize(Msg),
    ?event({minimized, MinimizedMsg}),
    ?assertEqual(0, maps:size(MinimizedMsg)).

%% @doc Test that we can convert a message into a tx record and back.
single_layer_message_to_tx_test() ->
    Msg = #{
        last_tx => << 2:256 >>,
        owner => << 3:4096 >>,
        target => << 4:256 >>,
        data => <<"DATA">>,
        <<"Special-Key">> => <<"SPECIAL_VALUE">>
    },
    TX = message_to_tx(Msg),
    ?event({tx_to_message, {msg, Msg}, {tx, TX}}),
    ?assertEqual(maps:get(last_tx, Msg), TX#tx.last_tx),
    ?assertEqual(maps:get(owner, Msg), TX#tx.owner),
    ?assertEqual(maps:get(target, Msg), TX#tx.target),
    ?assertEqual(maps:get(data, Msg), TX#tx.data),
    ?assertEqual({<<"Special-Key">>, <<"SPECIAL_VALUE">>},
        lists:keyfind(<<"Special-Key">>, 1, TX#tx.tags)).

%% @doc Test that we can convert a #tx record into a message correctly.
single_layer_tx_to_message_test() ->
    TX = #tx {
        unsigned_id = << 1:256 >>,
        last_tx = << 2:256 >>,
        owner = << 3:256 >>,
        target = << 4:256 >>,
        data = <<"DATA">>,
        tags = [{<<"special_key">>, <<"SPECIAL_KEY">>}]
    },
    Msg = tx_to_message(TX),
    ?assertEqual(maps:get(<<"special_key">>, Msg), <<"SPECIAL_KEY">>),
    ?assertEqual(<< "DATA">>, maps:get(<<"data">>, Msg)),
    ?assertEqual(<< 2:256 >>, maps:get(<<"last_tx">>, Msg)),
    ?assertEqual(<< 3:256 >>, maps:get(<<"owner">>, Msg)),
    ?assertEqual(<< 4:256 >>, maps:get(<<"target">>, Msg)).

%% @doc Test that the message matching function works.
match_test() ->
    Msg = #{ a => 1, b => 2 },
    TX = message_to_tx(Msg),
    Msg2 = tx_to_message(TX),
    ?assert(match(Msg, Msg2)).

%% @doc Test that two txs match. Note: This function uses tx_to_message/1
%% underneath, which (depending on the test) could potentially lead to false
%% positives.
txs_match(TX1, TX2) ->
    match(tx_to_message(TX1), tx_to_message(TX2)).

%% @doc Structured field parsing tests.
structured_field_atom_parsing_test() ->
    Msg = #{ highly_unusual_http_header => highly_unusual_value },
    ?assert(match(Msg, tx_to_message(message_to_tx(Msg)))).

structured_field_decimal_parsing_test() ->
    Msg = #{ integer_field => 1234567890 },
    ?assert(match(Msg, tx_to_message(message_to_tx(Msg)))).

binary_to_binary_test() ->
    % Serialization must be able to turn a raw binary into a TX, then turn
    % that TX back into a binary and have the result match the original.
    Bin = <<"THIS IS A BINARY, NOT A NORMAL MESSAGE">>,
    Msg = message_to_tx(Bin),
    ?assertEqual(Bin, tx_to_message(Msg)).

%% @doc Test that the data field is correctly managed when we have multiple
%% uses for it (the 'data' key itself, as well as keys that cannot fit in
%% tags).
message_with_large_keys_test() ->
    Msg = #{
        <<"normal_key">> => <<"normal_value">>,
        <<"large_key">> => << 0:((1 + ?MAX_TAG_VAL) * 8) >>,
        <<"another_large_key">> => << 0:((1 + ?MAX_TAG_VAL) * 8) >>,
        <<"another_normal_key">> => <<"another_normal_value">>
    },
    ?assert(match(Msg, tx_to_message(message_to_tx(Msg)))).

%% @doc Check that large keys and data fields are correctly handled together.
nested_message_with_large_keys_and_data_test() ->
    Msg = #{
        <<"normal_key">> => <<"normal_value">>,
        <<"large_key">> => << 0:(?MAX_TAG_VAL * 16) >>,
        <<"another_large_key">> => << 0:(?MAX_TAG_VAL * 16) >>,
        <<"another_normal_key">> => <<"another_normal_value">>,
        data => <<"Hey from the data field!">>
    },
    TX = message_to_tx(Msg),
    Msg2 = tx_to_message(TX),
    ?event({matching, {input, Msg}, {tx, TX}, {output, Msg2}}),
    ?assert(match(Msg, Msg2)).

simple_nested_message_test() ->
    Msg = #{
        a => <<"1">>,
        nested => #{ <<"b">> => <<"1">> },
        c => <<"3">>
    },
    TX = message_to_tx(Msg),
    Msg2 = tx_to_message(TX),
    ?event({matching, {input, Msg}, {output, Msg2}}),
    ?assert(
        match(
            Msg,
            Msg2
        )
    ).

%% @doc Test that the data field is correctly managed when we have multiple
%% uses for it (the 'data' key itself, as well as keys that cannot fit in
%% tags).
nested_message_with_large_data_test() ->
    Msg = #{
        <<"tx_depth">> => <<"outer">>,
        data => #{
            <<"tx_map_item">> =>
                #{
                    <<"tx_depth">> => <<"inner">>,
                    <<"large_data_inner">> => << 0:((1 + ?MAX_TAG_VAL) * 8) >>
                },
            <<"large_data_outer">> => << 0:((1 + ?MAX_TAG_VAL) * 8) >>
        }
    },
    ?assert(match(Msg, tx_to_message(message_to_tx(Msg)))).

%% @doc Test that we can convert a 3 layer nested message into a tx record and back.
deeply_nested_message_with_data_test() ->
    Msg = #{
        <<"tx_depth">> => <<"outer">>,
        data => #{
            <<"tx_map_item">> =>
                #{
                    <<"tx_depth">> => <<"inner">>,
                    data => #{
                        <<"tx_depth">> => <<"innermost">>,
                        data => <<"DATA">>
                    }
                }
        }
    },
    ?assert(match(Msg, tx_to_message(message_to_tx(Msg)))).


nested_structured_fields_test() ->
    NestedMsg = #{ a => #{ b => 1 } },
    ?assert(
        match(
            NestedMsg,
            tx_to_message(message_to_tx(NestedMsg))
        )
    ).

nested_message_with_large_keys_test() ->
    Msg = #{
        a => <<"1">>,
        long_data => << 0:((1 + ?MAX_TAG_VAL) * 8) >>,
        nested => #{ <<"b">> => <<"1">> },
        c => <<"3">>
    },
    ResMsg = tx_to_message(message_to_tx(Msg)),
    ?event({matching, {input, Msg}, {output, ResMsg}}),
    ?assert(match(Msg, ResMsg)).

%% @doc Test that we can convert a signed tx into a message and back.
signed_tx_to_message_and_back_test() ->
    TX = #tx {
        data = <<"TEST_DATA">>,
        tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}]
    },
    SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
    ?assert(ar_bundles:verify_item(SignedTX)),
    SignedMsg = tx_to_message(SignedTX),
    SignedTX2 = message_to_tx(SignedMsg),
    ?assert(ar_bundles:verify_item(SignedTX2)).

signed_deep_tx_serialize_and_deserialize_test() ->
    TX = #tx {
        tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}],
        data = #{
            <<"NESTED_TX">> =>
                #tx {
                    data = <<"NESTED_DATA">>,
                    tags = [{<<"NESTED_KEY">>, <<"NESTED_VALUE">>}]
                }
        }
    },
    SignedTX = ar_bundles:deserialize(
        ar_bundles:sign_item(TX, hb:wallet())
    ),
    ?assert(ar_bundles:verify_item(SignedTX)),
    SerializedTX = serialize(SignedTX),
    DeserializedTX = deserialize(SerializedTX),
    ?assert(
        match(
            tx_to_message(SignedTX),
            DeserializedTX
        )
    ).

simple_message_to_http_test() ->
    Msg = #{ a => 1, b => 2, priv_c => 3, id => <<"regen_ignore">> },
    Http = message_to_http(Msg),
    ?assertEqual(
        #{ headers => [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}], body => <<>> },
        Http    
    ),
    ok.

simple_body_message_to_http_test() ->
    Html = <<"<html><body>Hello</body></html>">>,
    Msg = #{ "Content-Type" => <<"text/html">>, body => Html },
    Http = message_to_http(Msg),
    ok.

calculate_unsigned_message_id_test() ->
    Msg = #{
        data => <<"DATA">>,
        <<"special_key">> => <<"SPECIAL_KEY">>
    },
    UnsignedTX = message_to_tx(Msg),
    UnsignedMessage = tx_to_message(UnsignedTX),
    ?assertEqual(
        hb_util:encode(ar_bundles:id(UnsignedTX, unsigned)),
        hb_util:id(UnsignedMessage, unsigned)
    ).

sign_serialize_deserialize_verify_test() ->
    Msg = #{
        data => <<"DATA">>,
        <<"special_key">> => <<"SPECIAL_KEY">>
    },
    TX = message_to_tx(Msg),
    SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
    ?assert(ar_bundles:verify_item(SignedTX)),
    SerializedMsg = serialize(SignedTX),
    DeserializedMsg = deserialize(SerializedMsg),
    DeserializedTX = message_to_tx(DeserializedMsg),
    ?assert(ar_bundles:verify_item(DeserializedTX)).

unsigned_id_test() ->
    UnsignedTX = ar_bundles:normalize(#tx { data = <<"TEST_DATA">> }),
    UnsignedMessage = tx_to_message(UnsignedTX),
    ?assertEqual(
        hb_util:encode(ar_bundles:id(UnsignedTX, unsigned)),
        hb_util:id(UnsignedMessage, unsigned)
    ).

signed_id_test_disabled() ->
    TX = #tx {
        data = <<"TEST_DATA">>,
        tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}]
    },
    SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
    ?assert(ar_bundles:verify_item(SignedTX)),
    SignedMsg = tx_to_message(SignedTX),
    ?assertEqual(
        hb_util:encode(ar_bundles:id(SignedTX, signed)),
        hb_util:id(SignedMsg, signed)
    ).

list_encoding_test() ->
    % Test that we can encode and decode a list of integers.
    {<<"List">>, Encoded} = encode_value(List1 = [1, 2, 3]),
    Decoded = decode_value(list, Encoded),
    ?assertEqual(List1, Decoded),
    % Test that we can encode and decode a list of binaries.
    {<<"List">>, Encoded2} = encode_value(List2 = [<<"1">>, <<"2">>, <<"3">>]),
    ?assertEqual(List2, decode_value(list, Encoded2)),
    % Test that we can encode and decode a mixed list.
    {<<"List">>, Encoded3} = encode_value(List3 = [1, <<"2">>, 3]),
    ?assertEqual(List3, decode_value(list, Encoded3)).

message_with_simple_list_test() ->
    Msg = #{ a => [<<"1">>, <<"2">>, <<"3">>] },
    ?assert(match(Msg, tx_to_message(message_to_tx(Msg)))).

empty_string_in_tag_test() ->
    Msg =
        #{
            dev =>
                #{
                    <<"stderr">> => <<"">>,
                    <<"stdin">> => <<"b">>,
                    <<"stdout">> => <<"c">>
                }
        },
    Msg2 = minimize(tx_to_message(message_to_tx(Msg))),
    ?assert(match(Msg, Msg2)).
