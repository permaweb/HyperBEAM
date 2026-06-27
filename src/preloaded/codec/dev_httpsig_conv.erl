
%%% @doc A codec that marshals TABM encoded messages to and from the "HTTP"
%%% message structure.
%%% 
%%% Every HTTP message is an HTTP multipart message.
%%% See https://datatracker.ietf.org/doc/html/rfc7578
%%%
%%% For each TABM Key:
%%%
%%% The Key/Value Pair will be encoded according to the following rules:
%%%     "signatures" -> {SignatureInput, Signature} header Tuples, each encoded
%%% 					as a Structured Field Dictionary
%%%     "body" ->
%%%         - if a map, then recursively encode as its own HyperBEAM message
%%%         - otherwise encode as a normal field
%%%     _ -> encode as a normal field
%%% 
%%% Each field will be mapped to the HTTP Message according to the following 
%%% rules:
%%%     "body" -> always encoded part of the body as with Content-Disposition
%%% 			  type of "inline"
%%%     _ ->
%%%         - If the byte size of the value is less than the ?MAX_TAG_VALUE,
%%% 		  then encode as a header, also attempting to encode as a
%%% 		  structured field.
%%%         - Otherwise encode the value as a part in the multipart response
%%% 
-module(dev_httpsig_conv).
-export([to/3, from/3, encode_http_msg/2]).
%%% Helper utilities
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

% The max header length is 4KB
-define(MAX_HEADER_LENGTH, 4096).
% https://datatracker.ietf.org/doc/html/rfc7231#section-3.1.1.4
-define(CRLF, <<"\r\n">>).
-define(DOUBLE_CRLF, <<?CRLF/binary, ?CRLF/binary>>).

%% @doc Convert a HTTP Message into a TABM.
%% HTTP Structured Field is encoded into it's equivalent TABM encoding.
from(Bin, _Req, _Opts) when is_binary(Bin) -> {ok, Bin};
from(Link, _Req, _Opts) when ?IS_LINK(Link) -> {ok, Link};
from(HTTP, _Req, Opts) ->
    % First, parse all headers excluding the signature-related headers, as they
    % are handled separately.
    Headers = hb_maps:without([<<"body">>], HTTP, Opts),
    % Next, we need to potentially parse the body, get the ordering of the body
    % parts, and add them to the TABM.
    {OrderedBodyKeys, BodyTABM} = body_to_tabm(HTTP, Opts),
    % Merge the body keys with the headers.
    WithBodyKeys = maps:merge(Headers, BodyTABM),
    % Decode percent-encoded headers.
    WithIDs = decode_ids(WithBodyKeys, Opts),
    % Remove the signature-related headers, such that they can be reconstructed
    % from the commitments.
    MsgWithoutSigs =
        hb_maps:without(
            [<<"signature">>, <<"signature-input">>, <<"commitments">>],
            WithIDs,
            Opts
        ),
    % Finally, we need to add the signatures to the TABM.
    Commitments =
        dev_httpsig_siginfo:siginfo_to_commitments(
            WithIDs,
            OrderedBodyKeys,
            Opts
        ),
    MsgWithSigs =
        case ?IS_EMPTY_MESSAGE(Commitments) of
            false -> MsgWithoutSigs#{ <<"commitments">> => Commitments };
            true -> MsgWithoutSigs
        end,
    ?event({message_with_commitments, MsgWithSigs}),
    Res =
        hb_maps:without(
            Removed =
                hb_maps:keys(Commitments) ++
                [<<"content-digest">>] ++
                case maps:get(<<"content-type">>, MsgWithSigs, undefined) of
                    <<"multipart/", _/binary>> -> [<<"content-type">>];
                    _ -> []
                end ++
                case hb_message:is_signed_key(<<"ao-body-key">>, MsgWithSigs, Opts) of
                    true -> [];
                    false -> [<<"ao-body-key">>]
                end,
            MsgWithSigs,
            Opts
        ),
    ?event({message_without_commitments, Res, Removed}),
    {ok, Res}.

%% @doc Generate the body TABM from the `body' key of the encoded message.
body_to_tabm(HTTP, Opts) ->
    % Extract the body and content-type from the HTTP message.
    Body = hb_maps:get(<<"body">>, HTTP, no_body, Opts),
    ContentType = hb_maps:get(<<"content-type">>, HTTP, undefined, Opts),
    {_, InlinedKey} = inline_key(HTTP),
    ?event({inlined_body_key, InlinedKey}),
    % Parse the body into a TABM.
    {OrderedBodyKeys, BodyTABM} =
        case body_to_parts(ContentType, Body, Opts) of
            no_body -> {[], #{}};
            {normal, RawBody} ->
                % The body is not a multipart, so we just return the inlined key.
                {[InlinedKey], #{ InlinedKey => RawBody }};
            {multipart, Parts} ->
                % Parse each part of the multipart body into an individual TABM,
                % with its associated key.
                OrderedBodyTABMs =
                    lists:map(
                        fun(Part) ->
                            from_body_part(InlinedKey, Part, Opts)
                        end,
                        Parts
                    ),
                % Merge all of the parts into a single TABM.
                MergedParts =
                    hb_message:convert(
                        maps:from_list(OrderedBodyTABMs),
                        tabm,
                        <<"flat@1.0">>,
                        Opts
                    ),
                % Calculate the ordered body keys of the multipart data. The
                % nested body parts are labelled by `path`, rather than `key`:
                % That is, a body part may contain a `/` in its key, representing
                % that the nested form is not a direct child of the parent 
                % message. Subsequently, we need to take just the first
                % `path part' of the key and return the unique'd list.
                {MessagePaths, _} = lists:unzip(OrderedBodyTABMs),
                Keys =
                    hb_util:unique(
                        lists:map(
                            fun(Path) ->
                                hd(binary:split(Path, <<"/">>, [global]))
                            end,
                            MessagePaths
                        )
                    ),
                % Return both as a pair.
                {Keys, MergedParts}
        end,
    {OrderedBodyKeys, BodyTABM}.

%% @doc Split the body into parts, if it is a multipart.
body_to_parts(_ContentType, no_body, _Opts) -> no_body;
body_to_parts(ContentType, Body, _Opts) ->
    ?event(
        {from_body,
            {content_type, {explicit, ContentType}},
            {body, Body}
        }
    ),
    Params =
        case ContentType of
            undefined -> [];
            _ ->
                {item, {_, _XT}, XParams} =
                    hb_structured_fields:parse_item(ContentType),
                XParams
        end,
    case lists:keyfind(<<"boundary">>, 1, Params) of
        false ->
            % The body is not a multipart, so just set as is to the Inlined key on
            % the TABM.
            {normal, Body};
        {_, {_Type, Boundary}} ->
            % We need to manually parse the multipart body into key/values on the
            % TABM.
            % 
            % Find the sub-part of the body within the boundary.
            % We also make sure to account for the CRLF at end and beginning
            % of the starting and terminating part boundary, respectively
            % 
            % ie.
            % --foo-boundary\r\n
            % My-Awesome: Part
            %
            % an awesome body\r\n
            % --foo-boundary--
            BegPat = <<"--", Boundary/binary, ?CRLF/binary>>,
            EndPat = <<?CRLF/binary, "--", Boundary/binary, "--">>,
            {Start, SL} = binary:match(Body, BegPat),
            {End, _} = binary:match(Body, EndPat),
            BodyPart = binary:part(Body, Start + SL, End - (Start + SL)),
            % By taking into account all parts of the surrounding boundary above,
            % we get precisely the sub-part that we're interested without any
            % additional parsing
            {multipart, binary:split(
                BodyPart,
                [<<?CRLF/binary, "--", Boundary/binary>>],
                [global]
            )}
    end.

%% @doc Parse a single part of a multipart body into a TABM.
from_body_part(InlinedKey, Part, Opts) ->
    % Extract the Headers block and Body. Only split on the FIRST double CRLF
    {RawHeadersBlock, RawBody} =
        case binary:split(Part, [?DOUBLE_CRLF], []) of
            [XRawHeadersBlock] ->
                % The message has no body.
                {XRawHeadersBlock, <<>>};
            [XRawHeadersBlock, XRawBody] ->
                {XRawHeadersBlock, XRawBody}
        end,
    % Extract individual headers
    RawHeaders = binary:split(RawHeadersBlock, ?CRLF, [global]),
    % Now we parse each header, splitting into {Key, Value}
    Headers =
        hb_maps:from_list(lists:filtermap(
            fun(<<>>) -> false;
               (RawHeader) -> 
                    case binary:split(RawHeader, [<<": ">>]) of
                        [Name, Value] -> {true, {Name, Value}};
                        _ ->
                            % skip lines that aren't properly formatted headers
                            false
                    end
            end,
            RawHeaders
        )),
    % The Content-Disposition is from the parent message,
    % so we separate off from the rest of the headers
    case hb_maps:get(<<"content-disposition">>, Headers, undefined, Opts) of
        undefined ->
            % A Content-Disposition header is required for each part
            % in the multipart body
            throw({error, no_content_disposition_in_multipart, Headers});
        RawDisposition when is_binary(RawDisposition) ->
            % Extract the name 
            {item, {_, Disposition}, DispositionParams} =
                hb_structured_fields:parse_item(RawDisposition),
            {ok, PartName} =
                case Disposition of
                    <<"inline">> ->
                        {ok, InlinedKey};
                    _ ->
                        % Otherwise, we need to extract the name of the part
                        % from the Content-Disposition parameters. The name is
                        % percent-encoded on encode (see `encode_body_part/4') so
                        % that arbitrary key bytes survive as a legal
                        % structured-field string, so we decode it here to recover
                        % the original key.
                        case lists:keyfind(<<"name">>, 1, DispositionParams) of
                            {_, {_type, PN}} -> {ok, hb_escape:decode(PN)};
                            false -> no_part_name_found
                        end
                end,
            Commitments =
                dev_httpsig_siginfo:siginfo_to_commitments(
                    Headers#{ PartName => RawBody },
                    [PartName],
                    Opts
                ),
            RestHeaders =
                hb_maps:without(
                    [
                        <<"ao-body-key">>,
                        <<"content-digest">>,
                        <<"content-disposition">>
                    ],
                    Headers,
                    Opts
                ),
            PartNameSplit = binary:split(PartName, <<"/">>, [global]),
            NestedPartName = lists:last(PartNameSplit),
            ParsedPart =
                case hb_maps:size(Commitments, Opts) of
                    0 ->
                        WithoutTypes = maps:without([<<"ao-types">>], RestHeaders),
                        Types =
                            hb_maps:get(
                                <<"ao-types">>,
                                RestHeaders,
                                <<>>,
                                Opts
                            ),
                        case {hb_maps:size(WithoutTypes, Opts), Types, RawBody} of
                            {0, <<"empty-message">>, <<>>} ->
                                % The message is empty, so we return an empty
                                % map.
                                #{};
                            {_, _, <<>>} ->
                                % There is no body to the message, so we return
                                % just the headers.
                                RestHeaders;
                            {0, _, _} ->
                                % There are no headers besides content-disposition,
                                % so we return the body as is.
                                RawBody;
                            {_, _, _} ->
                                % There are other headers, so we need to parse
                                % the body as a TABM.
                                {_, RawBodyKey} = inline_key(Headers),
                                RestHeaders#{ RawBodyKey => RawBody }
                        end;
                    _ -> maps:get(NestedPartName, Commitments, #{})
                end,
            {PartName, ParsedPart}
    end.

%%% @doc Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map
%%% that can translated to a given web server Response API
to(TABM, Req, Opts) -> to(TABM, Req, [], Opts).
to(Bin, _Req, _FormatOpts, _Opts) when is_binary(Bin) -> {ok, Bin};
to(Link, _Req, _FormatOpts, _Opts) when ?IS_LINK(Link) -> {ok, Link};
to(TABM, Req = #{ <<"index">> := true }, _FormatOpts, Opts) ->
    % If the caller has specified that an `index` page is requested, we:
    % 1. Convert the message to HTTPSig as usual.
    % 2. Check if the `body` and `content-type` keys are set. If either are,
    %    we return the message as normal.
    % 3. If they are not, we convert the given message back to its original
    %    form and resolve `path = index` upon it.
    % 4. If this yields a result, we convert it to TABM and merge it with the
    %    original HTTP-Sig encoded message. We prefer keys from the original
    %    if conflicts arise.
    % 5. The resulting combined message is returned to the user.
    {ok, EncOriginal} = to(TABM, maps:without([<<"index">>], Req), Opts),
    OrigBody = hb_maps:get(<<"body">>, EncOriginal, <<>>, Opts),
    OrigContentType = hb_maps:get(<<"content-type">>, EncOriginal, <<>>, Opts),
    case {OrigBody, OrigContentType} of
        {<<>>, <<>>} ->
            % The message has no body or content-type set. Resolve the `index`
            % key upon it to derive it.
            Structured = hb_message:convert(TABM, <<"structured@1.0">>, Opts),
            try hb_ao:resolve(Structured, Req#{ <<"path">> => <<"index">> }, Opts) of
                {ok, IndexMsg} ->
                    % The index message has been calculated successfully. Convert
                    % it to TABM format.
                    IndexTABM = hb_message:convert(IndexMsg, tabm, Opts),
                    % Merge the index message with the original, favoring the 
                    % keys of the original in the event of conflict. Remove the
                    % `priv` message, if present.
                    Merged =
                        hb_maps:merge(
                            hb_private:reset(IndexTABM),
                            hb_maps:without(
                                [<<"body">>, <<"content-type">>],
                                EncOriginal,
                                Opts
                            )
                        ),
                    % Return the merged result.
                    {ok, Merged};
                Err ->
                    % The index resolution executed without error, but the result
                    % was not a valid message. We log a warning for the operator
                    % and return the original message to the caller.
                    ?event(warning, {invalid_index_result, Err}),
                    {ok, EncOriginal}
            catch
                Err:Details:Stacktrace ->
                    % There was an error while generating the index page. We
                    % log a warning for the operator and return the modified
                    % message to the caller.
                    ?event(warning,
                        {error_generating_index,
                            {type, Err},
                            {details, Details},
                            {stacktrace, Stacktrace}
                        }
                    ),
                    {ok, EncOriginal}
            end;
        _ ->
            % Return the encoded HTTPSig message without modification.
            {ok, EncOriginal}
    end;
to(TABM, Req, FormatOpts, Opts) when is_map(TABM) ->
    % Ensure that the material for the message is loaded, if the request is
    % asking for a bundle.
    Msg =
        case hb_util:atom(hb_maps:get(<<"bundle">>, Req, false, Opts)) of
            false -> encode_ids(TABM);
            true ->
                % Convert back to the fully loaded structured@1.0 message, then
                % convert to TABM with bundling enabled.
                Structured = hb_message:convert(TABM, <<"structured@1.0">>, Opts),
                Loaded = hb_cache:ensure_all_loaded(Structured, Opts),
                encode_ids(
                    hb_message:convert(
                        Loaded,
                        tabm,
                        #{
                            <<"device">> => <<"structured@1.0">>,
                            <<"bundle">> => true
                        },
                        Opts
                    )
                )
        end,
    % Group the IDs into a dictionary, so that they can be distributed as
    % HTTP headers. If we did not do this, ID keys would be lower-cased and
    % their comparability against the original keys would be lost.
    Stripped =
        hb_maps:without(
            [
                <<"commitments">>,
                <<"signature">>,
                <<"signature-input">>,
                <<"priv">>
            ],
            Msg,
            Opts
        ),
    {InlineFieldHdrs, InlineKey} = inline_key(Stripped),
    Intermediate =
        do_to(
            Stripped,
            FormatOpts ++ [{inline, InlineFieldHdrs, InlineKey}],
            Opts
        ),
    % Finally, add the signatures to the encoded HTTP message with the
    % commitments from the original message.
    CommitmentsMap =
        case maps:get(<<"commitments">>, Msg, undefined) of
            undefined ->
                case maps:get(<<"signature">>, Msg, undefined) of
                    undefined -> #{};
                    Signature ->
                        MaybeBundleTag = maps:with([<<"bundle">>], Msg),
                        #{
                            Signature => MaybeBundleTag#{
                                <<"signature">> => Signature,
                                <<"keyid">> => maps:get(<<"keyid">>, Msg, <<>>),
                                <<"commitment-device">> => <<"httpsig@1.0">>,
                                <<"type">> => maps:get(<<"type">>, Msg, <<>>),
                                <<"committed">> =>
                                    maps:get(<<"committed">>, Msg, #{})
                            }
                        }
                end;
            Commitments ->
                Commitments
        end,
    ?event({converting_commitments_to_siginfo, Msg}),
    {ok,
        maps:merge(
            Intermediate,
            dev_httpsig_siginfo:commitments_to_siginfo(
                TABM,
                CommitmentsMap,
                Opts
            )
        )
    }.

do_to(Binary, _FormatOpts, _Opts) when is_binary(Binary) -> Binary;
do_to(TABM, FormatOpts, Opts) when is_map(TABM) ->
    InlineKey =
        case lists:keyfind(inline, 1, FormatOpts) of
            {inline, _InlineFieldHdrs, Key} -> Key;
            _ -> not_set
        end,
    % Calculate the initial encoding from the TABM
    Enc0 =
        maps:fold(
            fun(<<"body">>, Value, AccMap) ->
                    OldBody = maps:get(<<"body">>, AccMap, #{}),
                    AccMap#{ <<"body">> => OldBody#{ <<"body">> => Value } };
               (Key, Value, AccMap) when Key =:= InlineKey andalso InlineKey =/= not_set ->
                    OldBody = maps:get(<<"body">>, AccMap, #{}),
                    AccMap#{ <<"body">> => OldBody#{ InlineKey => Value } };
               (Key, Value, AccMap) ->
                    field_to_http(AccMap, {Key, Value}, #{})
            end,
            % Add any inline field denotations to the HTTP message
            case lists:keyfind(inline, 1, FormatOpts) of
                {inline, InlineFieldHdrs, _InlineKey} -> InlineFieldHdrs;
                _ -> #{}
            end,
            maps:without([<<"priv">>], TABM)
        ),
    ?event({prepared_body_map, {msg, Enc0}}),
    BodyMap = maps:get(<<"body">>, Enc0, #{}),
    GroupedBodyMap = group_maps(BodyMap, <<>>, #{}, Opts),
    Enc1 =
        case GroupedBodyMap of
            EmptyBody when map_size(EmptyBody) =:= 0 ->
                % If the body map is empty, then simply set the body to be a 
                % corresponding empty binary.
                ?event({encoding_empty_body, {msg, Enc0}}),
                Enc0;
            #{ InlineKey := UserBody }
                    when map_size(GroupedBodyMap) =:= 1 andalso is_binary(UserBody) ->
                % Simply set the sole body binary as the body of the
                % HTTP message, no further encoding required
                % 
                % NOTE: this may only be done for the top most message as 
                % sub-messages MUST be encoded as sub-parts, in order to preserve 
                % the nested hierarchy of messages, even in the case of a sole 
                % body binary.
                % 
                % In all other cases, the mapping fallsthrough to the case below 
                % that properly encodes a nested body within a sub-part
                ?event({encoding_single_body, {body, UserBody}, {http, Enc0}}),
                hb_maps:put(<<"body">>, UserBody, Enc0, Opts);
            _ ->
                % Otherwise, we need to encode the body map as the
                % multipart body of the HTTP message
                ?event({encoding_multipart, {bodymap, {explicit, GroupedBodyMap}}}),
                PartList = hb_util:to_sorted_list(
                    hb_maps:map(
                        fun(Key, M = #{ <<"body">> := _ }) when map_size(M) =:= 1 ->
                            % If the map has only one key, and it is `body',
                            % then we must encode part name with the additional
                            % `/body' suffix. This is because otherwise, the `body'
                            % element will be assumed to be an inline part, removing
                            % the necessary hierarchy.
                            encode_body_part(
                                <<Key/binary, "/body">>,
                                M,
                                <<"body">>,
                                Opts
                            );
                        (Key, Value) ->
                            encode_body_part(Key, Value, InlineKey, Opts)
                        end,
                        GroupedBodyMap,
                        Opts
                    ),
                    Opts
                ),
                Boundary = boundary_from_parts(PartList),
                % Transform body into a binary, delimiting each part with the
                % boundary
                BodyList = lists:foldl(
                    fun ({_PartName, BodyPart}, Acc) ->
                        [
                            <<
                                "--", Boundary/binary, ?CRLF/binary,
                                BodyPart/binary
                            >>
                        |
                            Acc
                        ]
                    end,
                    [],
                    PartList
                ),
                % Finally, join each part of the multipart body into a single binary
                % to be used as the body of the Http Message
                FinalBody = iolist_to_binary(lists:join(?CRLF, lists:reverse(BodyList))),
                % Ensure we append the Content-Type to be a multipart response
                Enc0#{
                    <<"content-type">> =>
                        <<"multipart/form-data; boundary=", "\"" , Boundary/binary, "\"">>,
                    <<"body">> => <<FinalBody/binary, ?CRLF/binary, "--", Boundary/binary, "--">>
                }
        end,
    % Add the content-digest to the HTTP message. `add_content_digest/1'
    % will return a map with the `content-digest' key set, but the body removed,
    % so we merge the two maps together to maintain the body and the content-digest.
    Enc2 = case hb_maps:get(<<"body">>, Enc1, <<>>, Opts) of
        <<>> -> Enc1;
        _ ->
            ?event({adding_content_digest, {msg, Enc1}}),
            hb_maps:merge(
                Enc1,
                dev_httpsig:add_content_digest(Enc1, Opts),
                Opts
            )
    end,
    ?event({final_body_map, {msg, Enc2}}),
    Enc2.

%% @doc Transform message keys into a form that can be transmitted as HTTP header
%% field names. Any key that is not already a valid lowercase HTTP token is
%% percent-encoded: this covers IDs (which contain uppercase base64url
%% characters) as well as keys containing spaces, emoji, or any other bytes that
%% are illegal in a header name. Previously only ID-shaped keys (matched by byte
%% size) were encoded, so a tag whose name contained e.g. a space or an emoji was
%% emitted verbatim as an invalid header and rejected downstream - observed as a
%% 502 from a fronting proxy. The encoding is reversed by `decode_ids/2', which
%% decodes every key unconditionally.
encode_ids(Msg) ->
    maps:from_list(
        lists:map(
            fun({K, V}) when is_binary(K) -> {hb_escape:encode_http_key(K), V};
                ({K, V}) -> {K, V}
            end,
            maps:to_list(Msg)
        )
    ).

% @doc Decode all ID fields from their percent-encoded form.
decode_ids(Msg, _Opts) ->
    maps:from_list(
        lists:map(
            fun({K, V}) -> {hb_escape:decode(K), V} end,
            maps:to_list(Msg)
        )
    ).

%% @doc Merge maps at the same level, if possible.
group_maps(Map) ->
    group_maps(Map, <<>>, #{}, #{}).
group_maps(Map, Parent, Top, Opts) when is_map(Map) ->
    ?event({group_maps, {map, Map}, {parent, Parent}, {top, Top}}),
    {Flattened, NewTop} = hb_maps:fold(
        fun(Key, Value, {CurMap, CurTop}) ->
            ?event({group_maps, {key, Key}, {value, Value}}),
            NormKey = hb_ao:normalize_key(Key),
            FlatK =
                case Parent of
                    <<>> -> NormKey;
                    _ -> <<Parent/binary, "/", NormKey/binary>>
                end,
            case Value of
                _ when is_map(Value) orelse is_list(Value) ->
                    NormMsg =
                        if is_list(Value) ->
                            hb_message:convert(
                                Value,
                                tabm,
                                <<"structured@1.0">>,
                                Opts
                            );
                        true ->
                            Value
                        end,
                    case hb_maps:size(NormMsg, Opts) of
                        0 ->
                            {
                                CurMap,
                                hb_maps:put(
                                    FlatK,
                                    #{ <<"ao-types">> => <<"empty-message">> },
                                    CurTop,
                                    Opts
                                )
                            };
                        _ ->
                            NewTop = group_maps(NormMsg, FlatK, CurTop, Opts),
                            {CurMap, NewTop}
                    end;
                _ ->
                    ?event({group_maps, {norm_key, NormKey}, {value, Value}}),
                    case byte_size(Value) > ?MAX_HEADER_LENGTH of
                        % the value is too large to be encoded as a header
                        % within a part, so instead lift it to be a top level
                        % part
                        true ->
                            NewTop = hb_maps:put(FlatK, Value, CurTop, Opts),
                            {CurMap, NewTop};
                        % Encode the value in the current part
                        false ->
                            NewCurMap = hb_maps:put(NormKey, Value, CurMap, Opts),
                            {NewCurMap, CurTop}
                    end
            end
        end,
        {#{}, Top},
        Map,
        Opts
    ),
    case hb_maps:size(Flattened, Opts) of
        0 -> NewTop;
        _ -> case Parent of
            <<>> -> hb_maps:merge(NewTop, Flattened, Opts);
            _ ->
                Res = NewTop#{ Parent => Flattened },
                ?event({returning_res, {res, Res}}),
                Res
        end
    end.

%% @doc Generate a unique, reproducible boundary for the
%% multipart body, however we cannot use the id of the message as
%% the boundary, as the id is not known until the message is
%% encoded. Subsequently, we generate each body part individually,
%% concatenate them, and apply a SHA2-256 hash to the result.
%% This ensures that the boundary is unique, reproducible, and
%% secure.
boundary_from_parts(PartList) ->
    BodyBin =
        iolist_to_binary(
            lists:join(?CRLF,
                lists:map(
                    fun ({_PartName, PartBin}) -> PartBin end,
                    PartList
                )
            )
        ),
    RawBoundary = crypto:hash(sha256, BodyBin),
    hb_util:encode(RawBoundary).

%% @doc Encode a multipart body part to a flat binary.
encode_body_part(PartName, BodyPart, InlineKey, Opts) ->
    % We'll need to prepend a Content-Disposition header
    % to the part, using the field name as the form part
    % name.
    % (See https://www.rfc-editor.org/rfc/rfc7578#section-4.2).
    Disposition =
        case PartName of
            % The body is always made the inline part of
            % the multipart body
            InlineKey -> <<"inline">>;
            _ ->
                % The part name is emitted as a quoted structured-field string in
                % the Content-Disposition `name' parameter. The structured-fields
                % string grammar - and our parser,
                % `hb_structured_fields:parse_string/2' - only permits printable
                % ASCII (0x20-0x7E, excluding `"' and `\'). Part names are derived
                % from message keys, which may contain arbitrary bytes: spaces,
                % emoji, uppercase characters, and so on. This is especially
                % relevant for large (> ?MAX_HEADER_LENGTH) and/or nested values,
                % which `group_maps/4' lifts into their own body part keyed by
                % their full flat path; such names are never seen by `encode_ids/1'
                % (which only encodes top-level keys), so they would otherwise
                % reach the wire unescaped and crash the parser on decode. We
                % percent-encode the name here so that it is always a legal
                % structured-field string, and reverse it symmetrically in
                % `from_body_part/3'. This mirrors the existing treatment of
                % committed key names in `dev_httpsig_siginfo'. Names that are
                % already valid (lowercase letters, digits, `-_.' and the `/' path
                % separator, which the encoder leaves intact) are unchanged.
                EncodedPartName = hb_escape:encode(PartName),
                <<"form-data;name=", "\"", EncodedPartName/binary, "\"">>
        end,
    % Sub-parts MUST have at least one header, according to the
    % multipart spec. Adding the Content-Disposition not only
    % satisfies that requirement, but also encodes the
    % HB message field that resolves to the sub-message
    case BodyPart of
        BPMap when is_map(BPMap) ->
            WithDisposition =
                hb_maps:put(
                    <<"content-disposition">>,
                    Disposition,
                    BPMap,
                    Opts
                ),
            encode_http_flat_msg(WithDisposition, Opts);
        BPBin when is_binary(BPBin) ->
            % A properly encoded inlined body part MUST have a CRLF between
            % it and the header block, so we MUST use two CRLF:
            % - first to signal end of the Content-Disposition header
            % - second to signal the end of the header block
            <<
                "content-disposition: ", Disposition/binary, ?CRLF/binary,
                ?CRLF/binary,
                BPBin/binary
            >>
    end.

%% @doc given a message, returns a binary tuple:
%% - A list of pairs to add to the msg, if any
%% - the field name for the inlined key
%%
%% In order to preserve the field name of the inlined
%% part, an additional field may need to be added
inline_key(Msg) ->
    inline_key(Msg, #{}).

inline_key(Msg, Opts) ->
    % The message can name a key whose value will be placed in the body as the
    % inline part. Otherwise, the Msg <<"body">> is used. If not present, the
    % Msg <<"data">> is used.
    InlineBodyKey = hb_maps:get(<<"ao-body-key">>, Msg, false, Opts),
    ?event({inlined, InlineBodyKey}),
    case {
        InlineBodyKey,
        hb_maps:is_key(<<"body">>, Msg, Opts)
            andalso not ?IS_LINK(maps:get(<<"body">>, Msg, Opts)),
        hb_maps:is_key(<<"data">>, Msg, Opts)
            andalso not ?IS_LINK(maps:get(<<"data">>, Msg, Opts))
    } of
        % ao-body-key already exists, so no need to add one
        {Explicit, _, _} when Explicit =/= false -> {#{}, InlineBodyKey};
        % ao-body-key defaults to <<"body">> (see below)
        % So no need to add one
        {_, true, _} -> {#{}, <<"body">>};
        % We need to preserve the ao-body-key, as the <<"data">> field,
        % so that it is preserved during encoding and decoding
        {_, _, true} -> {#{<<"ao-body-key">> => <<"data">>}, <<"data">>};
        % default to body being the inlined part.
        % This makes this utility compatible for both encoding
        % and decoding httpsig@1.0 messages
        _ -> {#{}, <<"body">>}
    end.

%% @doc Encode a HTTP message into a binary, converting it to `httpsig@1.0'
%% first.
encode_http_msg(Msg, Opts) ->
    % Convert the message to a HTTP-Sig encoded output.
    Httpsig = hb_message:convert(Msg, <<"httpsig@1.0">>, Opts),
    encode_http_flat_msg(Httpsig, Opts).

%% @doc Encode a HTTP message into a binary. The input *must* be a raw map of 
%% binary keys and values.
encode_http_flat_msg(Httpsig, Opts) ->
    % Serialize the headers, to be included in the part of the multipart response
    HeaderList =
        lists:foldl(
            fun ({HeaderName, RawHeaderVal}, Acc) ->
                HVal = hb_cache:ensure_loaded(RawHeaderVal, Opts),
                ?event({encoding_http_header, {header, HeaderName}, {value, HVal}}),
                [<<HeaderName/binary, ": ", HVal/binary>> | Acc]
            end,
            [],
            hb_maps:to_list(hb_maps:without([<<"body">>, <<"priv">>], Httpsig, Opts), Opts)
        ),
    EncodedHeaders = iolist_to_binary(lists:join(?CRLF, lists:reverse(HeaderList))),
    case hb_maps:get(<<"body">>, Httpsig, <<>>, Opts) of
        <<>> -> EncodedHeaders;
        % Some-Headers: some-value
        % content-type: image/png
        % 
        % <body>
        SubBody -> <<EncodedHeaders/binary, ?DOUBLE_CRLF/binary, SubBody/binary>>
    end.

%% @doc All maps are encoded into the body of the HTTP message
%% to be further encoded later.
field_to_http(Httpsig, {Name, Value}, Opts) when is_map(Value) ->
    NormalizedName = hb_ao:normalize_key(Name),
    OldBody = hb_maps:get(<<"body">>, Httpsig, #{}, Opts),
    Httpsig#{ <<"body">> => OldBody#{ NormalizedName => Value } };
field_to_http(Httpsig, {Name, Value}, Opts) when is_binary(Value) ->
    NormalizedName = hb_ao:normalize_key(Name),
    % The default location where the value is encoded within the HTTP
    % message depends on its size.
    % 
    % So we check whether the size of the value is within the threshold
    % to encode as a header, and otherwise default to encoding in the body.
    %
    % Note that a "where" Opts may force the location of the encoded
    % value -- this is only a default location if not specified in Opts 
    DefaultWhere =
        case {maps:get(where, Opts, headers), byte_size(Value)} of
            {headers, Fits} when Fits =< ?MAX_HEADER_LENGTH -> headers;
            _ -> body
        end,
    case maps:get(where, Opts, DefaultWhere) of
        headers ->
            Httpsig#{ NormalizedName => Value };
        body ->
            OldBody = hb_maps:get(<<"body">>, Httpsig, #{}, Opts),
            Httpsig#{ <<"body">> => OldBody#{ NormalizedName => Value } }
    end.

group_maps_test() ->
   Map = #{
        <<"a">> => <<"1">>,
        <<"b">> => #{
            <<"x">> => <<"10">>,
            <<"y">> => #{
                <<"z">> => <<"20">>
            },
            <<"foo">> => #{
                <<"bar">> => #{
                    <<"fizz">> => <<"buzz">>
                }
            } 
        },
        <<"c">> => #{
            <<"d">> => <<"30">>
        },
        <<"e">> => <<"2">>,
        <<"buf">> => <<"hello">>,
        <<"nested">> => #{
            <<"foo">> => <<"iiiiii">>,
            <<"here">> => #{
                <<"bar">> => <<"baz">>,
                <<"fizz">> => <<"buzz">>,
                <<"pop">> => #{
                    <<"very-fizzy">> => <<"very-buzzy">>
                }
            }
        }
    },
    Lifted = group_maps(Map),
    ?assertEqual(
        Lifted,
        #{
            <<"a">> => <<"1">>,
            <<"b">> => #{<<"x">> => <<"10">>},
            <<"b/foo/bar">> => #{<<"fizz">> => <<"buzz">>},
            <<"b/y">> => #{<<"z">> => <<"20">>},
            <<"buf">> => <<"hello">>,
            <<"c">> => #{<<"d">> => <<"30">>},
            <<"e">> => <<"2">>,
            <<"nested">> => #{<<"foo">> => <<"iiiiii">>},
            <<"nested/here">> => #{<<"bar">> => <<"baz">>, <<"fizz">> => <<"buzz">>},
            <<"nested/here/pop">> => #{<<"very-fizzy">> => <<"very-buzzy">>}
        }
    ),
    ok.

%% @doc The grouped maps encoding is a subset of the flat encoding,
%% where on keys with maps values are flattened.
%%
%% So despite needing a special encoder to produce it
%% We can simply apply the flat encoder to it to get back
%% the original message.
%% 
%% The test asserts that is indeed the case.
group_maps_flat_compatible_test() ->
    Map = #{
        <<"a">> => <<"1">>,
        <<"b">> => #{
            <<"x">> => <<"10">>,
            <<"y">> => #{
                <<"z">> => <<"20">>
            },
            <<"foo">> => #{
                <<"bar">> => #{
                    <<"fizz">> => <<"buzz">>
                }
            } 
        },
        <<"c">> => #{
            <<"d">> => <<"30">>
        },
        <<"e">> => <<"2">>,
        <<"buf">> => <<"hello">>,
        <<"nested">> => #{
            <<"foo">> => <<"iiiiii">>,
            <<"here">> => #{
                <<"bar">> => <<"baz">>,
                <<"fizz">> => <<"buzz">>
            }
        }
    },
    Lifted = group_maps(Map),
    ?assertEqual(
        hb_message:convert(Lifted, tabm, <<"flat@1.0">>, #{}),
        Map
    ),
    ok.

encode_message_with_links_test() ->
    % hb_cache will only linkify the key if it is longer than 60 characters
    Msg = #{
        <<"immediate-key">> => <<"immediate-value">>,
        <<"long-key">> => binary:copy(<<"a">>, 61),
        <<"short-key">> => <<"short-value">>
    },
    {ok, Path} = hb_cache:write(Msg, #{}),
    {ok, Read} = hb_cache:read(Path, #{}),
    % Ensure that the message now has a lazy link
    ?assertMatch(<<"short-value">>, maps:get(<<"short-key">>, Read, #{})),
    ?assertMatch({link, _, _}, maps:get(<<"long-key">>, Read, #{})),
    % Encode and decode the message as `httpsig@1.0`
    Enc = hb_message:convert(Msg, <<"httpsig@1.0">>, #{}),
    ?event({encoded, Enc}),
    Dec = hb_message:convert(Enc, <<"structured@1.0">>, <<"httpsig@1.0">>, #{}),
    % Ensure that the result is the same as the original message
    ?event({decoded, Dec}),
    ?assert(hb_message:match(Msg, Dec, strict, #{})).

encode_ids_round_trips_weird_keys_test() ->
    % A key whose name is not a valid HTTP token (here, one containing a space
    % and an emoji) must be percent-encoded by `encode_ids/1' so that it forms a
    % legal HTTP header field name, and recovered exactly by `decode_ids/2'.
    % Regression test: previously only ID-shaped keys (matched by byte size) were
    % encoded, so such a key was emitted verbatim as an illegal header (observed
    % as a 502 from a fronting proxy).
    WeirdKey = <<"my ", 240, 159, 152, 128, " tag">>,
    Msg = #{
        <<"content-type">> => <<"text/plain">>,
        WeirdKey => <<"value">>
    },
    Encoded = encode_ids(Msg),
    % Every encoded key is now a legal HTTP header field name: printable,
    % non-uppercase ASCII with no spaces, control characters, or non-ASCII bytes.
    % (The percent-encoded form may contain `%', which is header-legal but is not
    % itself an `is_http_token/1' character, so we check header-safety directly.)
    HeaderSafe =
        fun(Bin) ->
            lists:all(
                fun(C) ->
                    C >= 16#21 andalso C =< 16#7e
                        andalso not (C >= $A andalso C =< $Z)
                end,
                binary_to_list(Bin)
            )
        end,
    lists:foreach(
        fun(K) -> ?assert(HeaderSafe(K)) end,
        maps:keys(Encoded)
    ),
    % The already-valid key is untouched; the weird key has been transformed.
    ?assert(maps:is_key(<<"content-type">>, Encoded)),
    ?assertNot(maps:is_key(WeirdKey, Encoded)),
    % Decoding restores the original message exactly.
    ?assertEqual(Msg, decode_ids(Encoded, #{})).

encode_body_part_escapes_weird_name_test() ->
    % A part name that is not a valid structured-field string (here it contains
    % emoji bytes >= 0x80) must still produce a Content-Disposition that the
    % structured-field parser accepts, and the name must decode back to the exact
    % original. This is the path taken by large (> ?MAX_HEADER_LENGTH) and/or
    % nested values, which are lifted into their own body part keyed by their full
    % flat path - a name that `encode_ids/1' never sees. Regression test:
    % previously the raw emoji bytes crashed the structured-field parser on decode.
    % The `/' path separator must survive intact so that `from_body_part/3' can
    % still split the nested path.
    WeirdName = <<"outer/weird-", 240, 159, 152, 128, "-name">>,
    Body = binary:copy(<<"a">>, ?MAX_HEADER_LENGTH + 1),
    Part = encode_body_part(WeirdName, Body, <<"body">>, #{}),
    % Split the part into its Content-Disposition header and body.
    [DispLine | _] = binary:split(Part, ?CRLF, [global]),
    <<"content-disposition: ", RawDisposition/binary>> = DispLine,
    % The header must parse as a structured field; the raw (unescaped) emoji bytes
    % would otherwise crash `parse_string/2'.
    {item, {_, <<"form-data">>}, DispositionParams} =
        hb_structured_fields:parse_item(RawDisposition),
    % Decoding the `name' parameter - exactly as `from_body_part/3' does - must
    % recover the original name, with the `/' separator preserved.
    {_, {_type, EncodedName}} = lists:keyfind(<<"name">>, 1, DispositionParams),
    ?assertEqual(WeirdName, hb_escape:decode(EncodedName)),
    ?assertMatch([<<"outer">>, _], binary:split(WeirdName, <<"/">>, [global])).

encode_large_nested_weird_key_round_trips_test() ->
    % End-to-end: a value larger than ?MAX_HEADER_LENGTH living under a nested
    % key whose name is not a valid structured-field string is lifted into its
    % own multipart body part and must survive a full httpsig@1.0 encode/decode
    % round-trip. This exercises the `encode_body_part/4' <-> `from_body_part/3'
    % escaping introduced for the Content-Disposition `name' parameter.
    Big = binary:copy(<<"a">>, ?MAX_HEADER_LENGTH + 1),
    WeirdKey = <<"weird-", 240, 159, 152, 128, "-name">>,
    Msg = #{ <<"outer">> => #{ WeirdKey => Big } },
    Enc = hb_message:convert(Msg, <<"httpsig@1.0">>, #{}),
    ?event({encoded, Enc}),
    Dec = hb_message:convert(Enc, <<"structured@1.0">>, <<"httpsig@1.0">>, #{}),
    ?event({decoded, Dec}),
    ?assert(hb_message:match(Msg, Dec, strict, #{})).
