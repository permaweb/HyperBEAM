
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
-export([to/3, from/3, encode_http_msg/2, encode_key/1, boundary/1]).
%%% Helper utilities
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

% The max header length is 4KB
-define(MAX_HEADER_LENGTH, 4096).
% The most bytes that the headers of a message take, with the
% `signature-input' of a commitment over them. cowlib sends an HTTP/2 header
% block in one frame of at most 16384 bytes: this leaves 4096 for the request
% line, the headers of the transport and the parameters of the commitment.
-define(MAX_HEADERS_SIZE, 12288).
% https://datatracker.ietf.org/doc/html/rfc7231#section-3.1.1.4
-define(CRLF, <<"\r\n">>).
-define(DOUBLE_CRLF, <<?CRLF/binary, ?CRLF/binary>>).
% A byte that a header name holds as it is: `a-z', `0-9' and the `tchar'
% punctuation of RFC 9110 other than `%'.
-define(KEY_BYTE(C),
    ((C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) orelse
        C == $! orelse C == $# orelse C == $$ orelse C == $& orelse
        C == $' orelse C == $* orelse C == $+ orelse C == $- orelse
        C == $. orelse C == $^ orelse C == $_ orelse C == $` orelse
        C == $| orelse C == $~)
).
% The headers that HTTP and this codec set themselves: the signatures, the
% digest, length and key of the body, and the fields of the connection and its
% framing, which HTTP servers and clients act on or remove. A key of a message
% with one of these names is carried in a body part, and a header with one of
% them is not a key of the message.
-define(RESERVED_HEADERS, [
    <<"signature">>,
    <<"signature-input">>,
    <<"content-digest">>,
    <<"ao-body-key">>,
    <<"content-length">>,
    <<"connection">>,
    <<"keep-alive">>,
    <<"proxy-connection">>,
    <<"te">>,
    <<"trailer">>,
    <<"transfer-encoding">>,
    <<"upgrade">>
]).

%% @doc Convert a HTTP Message into a TABM.
%% HTTP Structured Field is encoded into it's equivalent TABM encoding.
from(Bin, _Req, _Opts) when is_binary(Bin) -> {ok, Bin};
from(Link, _Req, _Opts) when ?IS_LINK(Link) -> {ok, Link};
from(HTTP, _Req, Opts) ->
    % First, parse all headers excluding the signature-related headers, as they
    % are handled separately.
    Headers =
        maps:map(
            fun(<<"signature">>, Value) -> Value;
               (<<"signature-input">>, Value) -> Value;
               (_Key, Value) when is_binary(Value) -> hb_escape:decode_header(Value);
               (_Key, Value) -> Value
            end,
            hb_maps:without([<<"body">>], HTTP, Opts)
        ),
    % Next, we need to potentially parse the body, get the ordering of the body
    % parts, and add them to the TABM.
    {OrderedBodyKeys, BodyTABM} = body_to_tabm(HTTP, Opts),
    % Merge the body keys with the headers, other than the reserved headers. A
    % `multipart/' `content-type' header with a `boundary' gives the encoding
    % of the body, so we leave it out as well. A key of the message that shares
    % the name of one of these headers is carried in a body part.
    WithBodyKeys =
        maps:merge(
            hb_maps:without(
                case boundary(maps:get(<<"content-type">>, HTTP, undefined)) of
                    false -> ?RESERVED_HEADERS;
                    _ -> [<<"content-type">> | ?RESERVED_HEADERS]
                end,
                Headers,
                Opts
            ),
            BodyTABM
        ),
    % Reconstruct the commitments from the signature headers.
    Commitments =
        dev_httpsig_siginfo:siginfo_to_commitments(
            Headers,
            OrderedBodyKeys,
            Opts
        ),
    MsgWithoutSigs =
        decode_keys(
            hb_maps:without([<<"commitments">>], WithBodyKeys, Opts),
            Opts
        ),
    MsgWithSigs =
        case ?IS_EMPTY_MESSAGE(Commitments) of
            false -> MsgWithoutSigs#{ <<"commitments">> => Commitments };
            true -> MsgWithoutSigs
        end,
    ?event_debug({message_with_commitments, MsgWithSigs}),
    Res =
        hb_maps:without(
            Removed = hb_maps:keys(Commitments),
            MsgWithSigs,
            Opts
        ),
    ?event_debug({message_without_commitments, Res, Removed}),
    {ok, Res}.

%% @doc Generate the body TABM from the `body' key of the encoded message.
body_to_tabm(HTTP, Opts) ->
    % Extract the body and content-type from the HTTP message.
    Body = hb_maps:get(<<"body">>, HTTP, no_body, Opts),
    ContentType = hb_maps:get(<<"content-type">>, HTTP, undefined, Opts),
    InlinedKey = hb_maps:get(<<"ao-body-key">>, HTTP, <<"body">>, Opts),
    ?event_debug({inlined_body_key, InlinedKey}),
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
                % `path part' of the key, decode it, and return the unique'd
                % list.
                {MessagePaths, _} = lists:unzip(OrderedBodyTABMs),
                Keys =
                    hb_util:unique(
                        lists:map(
                            fun(Path) ->
                                hb_escape:decode(
                                    hd(binary:split(Path, <<"/">>, [global]))
                                )
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
    ?event_debug(
        {from_body,
            {content_type, {explicit, ContentType}},
            {body, Body}
        }
    ),
    case boundary(ContentType) of
        false ->
            % The body is not a multipart, so just set as is to the Inlined key on
            % the TABM.
            {normal, Body};
        Boundary ->
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

%% @doc Return the `boundary' of a `multipart/' `content-type', or `false' if
%% it has none. Any other `content-type' gives `false'. A `content-type' is not
%% a structured field, so we parse it with the grammar of RFC 9110. A
%% `multipart/' value that is not valid HTTP raises.
boundary(ContentType = <<"multipart/", _/binary>>) ->
    {_, _, Params} = cow_http_hd:parse_content_type(ContentType),
    proplists:get_value(<<"boundary">>, Params, false);
boundary(_ContentType) -> false.

%% @doc Check whether a key is `content-type' and its value starts with
%% `multipart/'.
is_multipart(<<"content-type">>, <<"multipart/", _/binary>>) -> true;
is_multipart(_Key, _Value) -> false.

%% @doc Check whether a key is `content-type' and its value has the form
%% `application/x.ao.<name>', which gives the codec of an HTTP message (see
%% `hb_http'). A value of the form `application/<name>' is also the type of a
%% document, such as `application/json', so it is not carried in a body part: a
%% stored document is served as itself.
is_codec_media_type(<<"content-type">>, Value) when is_binary(Value) ->
    string:prefix(hb_util:lowercase_key(Value), <<"application/x.ao.">>) =/= nomatch;
is_codec_media_type(_Key, _Value) -> false.

%% @doc Check whether a key is `content-type' and the message contains other
%% messages, as values or as links. Such a message is multipart when it is
%% bundled, so its own `content-type' is carried in a body part in every form.
is_bundle_content_type(<<"content-type">>, Msg) ->
    lists:any(
        fun({Key, Value}) ->
            is_map(Value) orelse ?IS_LINK(hb_link:decode(Key, Value))
        end,
        maps:to_list(maps:without([<<"commitments">>, <<"priv">>], Msg))
    );
is_bundle_content_type(_Key, _Msg) -> false.

%% @doc Parse a single part of a multipart body into a TABM.
from_body_part(InlinedKey, Part, Opts) ->
    % Extract the Headers block and Body. Only split on the FIRST double CRLF
    {RawHeadersBlock, RawBody, HasBody} =
        case binary:split(Part, [?DOUBLE_CRLF], []) of
            [XRawHeadersBlock] ->
                % The message has no body.
                {XRawHeadersBlock, <<>>, false};
            [XRawHeadersBlock, XRawBody] ->
                {XRawHeadersBlock, XRawBody, true}
        end,
    % Extract individual headers
    RawHeaders = binary:split(RawHeadersBlock, ?CRLF, [global]),
    % Now we parse each header, splitting into {Key, Value}
    Headers =
        hb_maps:from_list(lists:filtermap(
            fun(<<>>) -> false;
               (RawHeader) -> 
                    case binary:split(RawHeader, [<<": ">>]) of
                        [Name, Value] ->
                            Key = hb_util:lowercase_key(Name),
                            {true, {Key, hb_escape:decode_header(Value)}};
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
            % Extract the name. A `content-disposition' is not a structured
            % field: browsers write its values in UTF-8.
            {Disposition, DispositionParams} =
                cow_multipart:parse_content_disposition(RawDisposition),
            {ok, PartName} =
                case Disposition of
                    <<"inline">> ->
                        {ok, InlinedKey};
                    _ ->
                        % Otherwise, we need to extract the name of the part
                        % from the Content-Disposition parameters
                        case lists:keyfind(<<"name">>, 1, DispositionParams) of
                            {_, PN} -> {ok, PN};
                            false -> no_part_name_found
                        end
                end,
            % The headers of a part are keys of the message that it holds. Its
            % commitments are encoded as its `commitments' parts, and its body
            % is its `body' key, so its headers `signature', `signature-input'
            % and `ao-body-key' are data.
            RestHeaders =
                hb_maps:without(
                    [<<"content-disposition">>],
                    Headers,
                    Opts
                ),
            WithoutTypes = maps:without([<<"ao-types">>], RestHeaders),
            Types =
                hb_maps:get(
                    <<"ao-types">>,
                    RestHeaders,
                    <<>>,
                    Opts
                ),
            ParsedPart =
                case {hb_maps:size(WithoutTypes, Opts), Types, RawBody} of
                    {0, <<"empty-message">>, <<>>} ->
                        % The message is empty, so we return an empty
                        % map.
                        #{};
                    {_, _, <<>>} when not HasBody ->
                        % There is no body to the message, so we return
                        % just the headers.
                        RestHeaders;
                    {0, <<>>, _} ->
                        % There are no headers besides content-disposition,
                        % so we return the body as is.
                        RawBody;
                    {_, _, _} ->
                        % There are other headers, so we need to parse
                        % the body as a TABM.
                        RestHeaders#{ <<"body">> => RawBody }
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
    % 2. Check if the `body` and `content-type` keys are set. If either are, we
    %    return the message as normal.
    % 3. If they are not, we convert the given message back to its original
    %    form and resolve `path = index` upon it.
    % 4. If this yields a result, we convert it to TABM and merge it with the
    %    original HTTP-Sig encoded message. We prefer keys from the original
    %    if conflicts arise.
    % 5. The resulting combined message is returned to the user.
    {ok, EncOriginal} = to(TABM, maps:without([<<"index">>], Req), Opts),
    BodyKeys =
        hb_maps:with([<<"body">>, <<"content-type">>], EncOriginal, Opts),
    case map_size(BodyKeys) of
        0 ->
            % The message has no body or content-type set. Resolve the `index`
            % key upon it to derive it.
            Structured = hb_message:convert(TABM, <<"structured@1.0">>, Opts),
            try hb_ao:resolve(Structured, Req#{ <<"path">> => <<"index">> }, Opts) of
                {ok, IndexMsg} ->
                    % The index message has been calculated successfully. Convert
                    % its public keys to TABM format: its own commitments are
                    % not part of the message it presents.
                    IndexTABM =
                        hb_message:convert(
                            hb_message:uncommitted(IndexMsg, Opts),
                            tabm,
                            Opts
                        ),
                    % Merge the index message with the original, favoring the 
                    % keys of the original in the event of conflict. Remove the
                    % `priv` message, if present.
                    Merged =
                        hb_maps:merge(
                            % Index values become reply headers, so escape
                            % them as every other header value is. Its body
                            % stays a body.
                            maps:map(
                                fun(<<"body">>, Value) -> Value;
                                   (_Key, Value) when is_binary(Value) ->
                                        hb_escape:encode_header(Value);
                                   (_Key, Value) -> Value
                                end,
                                hb_private:reset(IndexTABM)
                            ),
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
to(TABM, _Req, FormatOpts, Opts) when is_map(TABM) ->
    CommitmentsMap = maps:get(<<"commitments">>, TABM, #{}),
    Covered = covered_keys(CommitmentsMap, Opts),
    Stripped =
        encode_keys(
            without_unsigned_commitments(
                hb_maps:without(
                    [<<"commitments">>, <<"priv">>] ++
                        uncovered_body_keys(TABM, Covered),
                    TABM,
                    Opts
                ),
                Opts
            )
        ),
    {InlineFieldHdrs, InlineKey} = inline_key(Stripped),
    Intermediate =
        do_to(
            Stripped,
            FormatOpts ++
                [{inline, InlineFieldHdrs, InlineKey}, {covered, Covered}],
            Opts
        ),
    % Finally, add the signatures to the encoded HTTP message with the
    % commitments from the original message. A `signature' key of the message
    % itself is data, not a commitment.
    ?event_debug({converting_commitments_to_siginfo, TABM}),
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

%% @doc Return the keys, percent-encoded, that the commitments of a message
%% cover, as the keys of a map, or `all' if it has no commitments.
covered_keys(Commitments, _Opts) when map_size(Commitments) == 0 ->
    all;
covered_keys(Commitments, Opts) ->
    maps:from_keys(
        [
            encode_key(Key)
        ||
            Commitment <- maps:values(Commitments),
            Key <-
                hb_util:message_to_ordered_list(
                    maps:get(<<"committed">>, Commitment, []),
                    Opts
                )
        ],
        true
    ).

%% @doc Return the keys of a message with commitments that no commitment
%% covers and that would be sent in the body. We do not send them: a commitment
%% that covers the body covers all of it with its `content-digest', and a
%% reader of a message with commitments keeps only the keys that they cover. A
%% key that no commitment covers is still sent if it fits in a header.
uncovered_body_keys(_TABM, all) ->
    [];
uncovered_body_keys(TABM, Covered) ->
    {_, InlineKey} = inline_key(TABM),
    [
        Key
    ||
        {Key, Value} <- maps:to_list(TABM),
        not is_map_key(encode_key(Key), Covered),
        Key == <<"body">> orelse Key == InlineKey orelse
            lists:member(Key, ?RESERVED_HEADERS) orelse
            is_multipart(Key, Value) orelse
            is_codec_media_type(Key, Value) orelse
            is_bundle_content_type(Key, TABM) orelse
            default_where(Value) == body
    ].

do_to(Binary, _FormatOpts, _Opts) when is_binary(Binary) -> Binary;
do_to(TABM, FormatOpts, Opts) when is_map(TABM) ->
    InlineKey =
        case lists:keyfind(inline, 1, FormatOpts) of
            {inline, _InlineFieldHdrs, Key} -> Key;
            _ -> not_set
        end,
    {covered, Covered} = lists:keyfind(covered, 1, FormatOpts),
    % Calculate the initial encoding from the TABM, with its keys in order and
    % the size of its headers so far.
    {Enc0, _} =
        lists:foldl(
            fun({<<"body">>, Value}, {AccMap, Size}) ->
                    OldBody = maps:get(<<"body">>, AccMap, #{}),
                    {AccMap#{ <<"body">> => OldBody#{ <<"body">> => Value } }, Size};
               ({Key, Value}, {AccMap, Size})
                        when Key =:= InlineKey andalso InlineKey =/= not_set ->
                    OldBody = maps:get(<<"body">>, AccMap, #{}),
                    {AccMap#{ <<"body">> => OldBody#{ InlineKey => Value } }, Size};
               ({Key, Value}, {AccMap, Size}) ->
                    % A key of the message that shares the name of a reserved
                    % header is carried in a body part. A `multipart/'
                    % `content-type' of the message is also carried in a body
                    % part, because the decoder parses such a header for the
                    % `boundary' of the body. So is a `content-type' of the
                    % form `application/x.ao.<name>', because a receiver reads
                    % such a header as the codec of the whole body. So is the
                    % `content-type' of a message that contains other messages.
                    Where =
                        case lists:member(Key, ?RESERVED_HEADERS) orelse
                                is_multipart(Key, Value) orelse
                                is_codec_media_type(Key, Value) orelse
                                is_bundle_content_type(Key, TABM) of
                            true -> body;
                            false -> default_where(Value)
                        end,
                    % A header takes the bytes of its name and value, 32 more
                    % (RFC 9113, section 6.5.2), and its name and 3 more in the
                    % `signature-input'. If the name of a header that the
                    % commitments cover passes ?MAX_HEADER_LENGTH bytes, or the
                    % headers would then pass ?MAX_HEADERS_SIZE bytes, carry it
                    % in a body part. A key that no commitment covers neither
                    % counts nor moves: a receiver reads each key in the body
                    % as one that a `content-digest' covers.
                    {NewWhere, NewSize} =
                        case Where == headers andalso
                                (Covered == all orelse is_map_key(Key, Covered)) of
                            false -> {Where, Size};
                            true ->
                                HeaderSize =
                                    2 * byte_size(Key) + byte_size(Value) + 35,
                                case byte_size(Key) > ?MAX_HEADER_LENGTH orelse
                                        Size + HeaderSize > ?MAX_HEADERS_SIZE of
                                    true -> {body, Size};
                                    false -> {headers, Size + HeaderSize}
                                end
                        end,
                    {
                        field_to_http(
                            AccMap,
                            {Key, Value},
                            #{ where => NewWhere }
                        ),
                        NewSize
                    }
            end,
            % Add any inline field denotations to the HTTP message
            {
                case lists:keyfind(inline, 1, FormatOpts) of
                    {inline, InlineFieldHdrs, _InlineKey} -> InlineFieldHdrs;
                    _ -> #{}
                end,
                0
            },
            hb_util:to_sorted_list(maps:without([<<"priv">>], TABM))
        ),
    ?event_debug({prepared_body_map, {msg, Enc0}}),
    BodyMap = maps:get(<<"body">>, Enc0, #{}),
    GroupedBodyMap = group_maps(BodyMap, <<>>, #{}, Opts),
    Enc1 =
        case GroupedBodyMap of
            EmptyBody when map_size(EmptyBody) =:= 0 ->
                % If the body map is empty, then simply set the body to be a 
                % corresponding empty binary.
                ?event_debug({encoding_empty_body, {msg, Enc0}}),
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
                ?event_debug({encoding_single_body, {body, UserBody}, {http, Enc0}}),
                hb_maps:put(<<"body">>, UserBody, Enc0, Opts);
            _ ->
                % Otherwise, we need to encode the body map as the
                % multipart body of the HTTP message
                ?event_debug({encoding_multipart, {bodymap, {explicit, GroupedBodyMap}}}),
                % The message's own `content-type' header moves into a part.
                % Part bodies are not escaped, so the part contains the value as
                % given.
                Parts =
                    maps:merge(
                        maps:map(
                            fun(_, Value) -> hb_escape:decode_header(Value) end,
                            maps:with([<<"content-type">>], Enc0)
                        ),
                        GroupedBodyMap
                    ),
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
                        Parts,
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
    Enc2 = case hb_maps:find(<<"body">>, Enc1, Opts) of
        {ok, Body} when is_binary(Body) ->
            ?event_debug({adding_content_digest, {msg, Enc1}}),
            hb_maps:merge(
                Enc1,
                dev_httpsig:add_content_digest(Enc1, Opts),
                Opts
            );
        _ -> Enc1
    end,
    ?event_debug({final_body_map, {msg, Enc2}}),
    Enc2.

%% @doc Percent-encode the keys of a message, and of each message nested in it,
%% with `encode_key/1'.
encode_keys(Msg) when is_map(Msg) ->
    maps:from_list(
        lists:map(
            fun({K, V}) -> {encode_key(K), encode_keys(V)} end,
            maps:to_list(Msg)
        )
    );
encode_keys(Value) -> Value.

%% @doc Remove the unsigned commitments (those with no `committer') of a
%% message and of each message nested in it. A nested message is read from a
%% store with the commitments that the ID linking it selects: its signed
%% commitments, or the unsigned commitment stored under its unsigned ID if it
%% has none, whether or not it had that commitment when written. A signature
%% over a bundled message covers its nested messages with only their signed
%% commitments, so it verifies after a write and a read.
without_unsigned_commitments(Msg, Opts) when is_map(Msg) ->
    maps:filtermap(
        fun(<<"commitments">>, Commitments) ->
                Signed =
                    hb_maps:filter(
                        fun(_ID, Commitment) ->
                            hb_maps:is_key(<<"committer">>, Commitment, Opts)
                        end,
                        Commitments,
                        Opts
                    ),
                case map_size(Signed) of
                    0 -> false;
                    _ -> {true, Signed}
                end;
           (_Key, Value) ->
                {true, without_unsigned_commitments(Value, Opts)}
        end,
        Msg
    );
without_unsigned_commitments(Value, _Opts) -> Value.

%% @doc Percent-encode a key as a header name. A header name is a token of the
%% `tchar' bytes of RFC 9110, and HTTP lowercases it. A key keeps `a-z', `0-9'
%% and the `tchar' punctuation other than `%', which starts an escape. Every
%% other byte, including each capital, is written as `%xx' in lowercase hex.
%% The key is scanned once: up to its first byte to escape, it is kept as it is.
encode_key(Key) -> encode_key(Key, Key, 0).
encode_key(Key, <<>>, _N) -> Key;
encode_key(Key, <<C, Rest/binary>>, N) when ?KEY_BYTE(C) ->
    encode_key(Key, Rest, N + 1);
encode_key(Key, _Rest, N) ->
    <<Kept:N/binary, Escaped/binary>> = Key,
    Encoded = << <<(encode_key_byte(C))/binary>> || <<C>> <= Escaped >>,
    <<Kept/binary, Encoded/binary>>.

encode_key_byte(C) when ?KEY_BYTE(C) -> <<C>>;
encode_key_byte(C) -> <<$%, (hex_digit(C bsr 4)), (hex_digit(C band 15))>>.

hex_digit(D) when D < 10 -> $0 + D;
hex_digit(D) -> $a + D - 10.

%% @doc Decode the keys of a message, and of each message nested in it, from
%% their percent-encoded form.
decode_keys(Msg, Opts) when is_map(Msg) ->
    maps:from_list(
        lists:map(
            fun({K, V}) -> {hb_escape:decode(K), decode_keys(V, Opts)} end,
            maps:to_list(Msg)
        )
    );
decode_keys(Value, _Opts) -> Value.

%% @doc Merge maps at the same level, if possible.
group_maps(Map) ->
    group_maps(Map, <<>>, #{}, #{}).
group_maps(Map, Parent, Top, Opts) when is_map(Map) ->
    ?event_debug({group_maps, {map, Map}, {parent, Parent}, {top, Top}}),
    {Flattened, NewTop} = hb_maps:fold(
        fun(Key, Value, {CurMap, CurTop}) ->
            ?event_debug({group_maps, {key, Key}, {value, Value}}),
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
                    ?event_debug({group_maps, {norm_key, NormKey}, {value, Value}}),
                    case NormKey =:= <<"content-disposition">>
                            orelse byte_size(Value) > ?MAX_HEADER_LENGTH of
                        % Content-Disposition frames multipart parts, while
                        % large values cannot be headers. Lift either one.
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
                ?event_debug({returning_res, {res, Res}}),
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
            _ -> <<"form-data;name=", "\"", PartName/binary, "\"">>
        end,
    % Sub-parts MUST have at least one header, according to the
    % multipart spec. Adding the Content-Disposition not only
    % satisfies that requirement, but also encodes the
    % HB message field that resolves to the sub-message
    case BodyPart of
        BPMap when is_map(BPMap) ->
            % The fields of the part other than its body are its headers, so
            % their values are escaped as the headers of the message itself
            % are.
            WithDisposition =
                hb_maps:put(
                    <<"content-disposition">>,
                    Disposition,
                    maps:map(
                        fun(<<"body">>, Value) -> Value;
                           (_Key, Value) when is_binary(Value) ->
                                hb_escape:encode_header(Value);
                           (_Key, Value) -> Value
                        end,
                        BPMap
                    ),
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
    % The Msg <<"body">> is placed in the body as the inline part. If not
    % present, the Msg <<"data">> is used. A message's own `ao-body-key' is
    % carried in a body part.
    case {
        hb_maps:is_key(<<"body">>, Msg, Opts)
            andalso not ?IS_LINK(maps:get(<<"body">>, Msg, Opts)),
        hb_maps:is_key(<<"data">>, Msg, Opts)
            andalso not ?IS_LINK(maps:get(<<"data">>, Msg, Opts))
    } of
        % ao-body-key defaults to <<"body">> (see below)
        % So no need to add one
        {true, _} -> {#{}, <<"body">>};
        % We need to preserve the ao-body-key, as the <<"data">> field,
        % so that it is preserved during encoding and decoding
        {_, true} -> {#{<<"ao-body-key">> => <<"data">>}, <<"data">>};
        % default to body being the inlined part.
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
                ?event_debug({encoding_http_header, {header, HeaderName}, {value, HVal}}),
                [<<HeaderName/binary, ": ", HVal/binary>> | Acc]
            end,
            [],
            hb_maps:to_list(hb_maps:without([<<"body">>, <<"priv">>], Httpsig, Opts), Opts)
        ),
    EncodedHeaders = iolist_to_binary(lists:join(?CRLF, lists:reverse(HeaderList))),
    case hb_maps:find(<<"body">>, Httpsig, Opts) of
        error -> EncodedHeaders;
        % Some-Headers: some-value
        % content-type: image/png
        % 
        % <body>
        {ok, SubBody} -> <<EncodedHeaders/binary, ?DOUBLE_CRLF/binary, SubBody/binary>>
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
    % message depends on its size, and on whether it starts or ends with
    % whitespace.
    % 
    % So we check whether the size of the value is within the threshold
    % to encode as a header, and otherwise default to encoding in the body.
    %
    % Note that a "where" Opts may force the location of the encoded
    % value -- this is only a default location if not specified in Opts 
    case maps:get(where, Opts, default_where(Value)) of
        headers ->
            Httpsig#{ NormalizedName => hb_escape:encode_header(Value) };
        body ->
            OldBody = hb_maps:get(<<"body">>, Httpsig, #{}, Opts),
            Httpsig#{ <<"body">> => OldBody#{ NormalizedName => Value } }
    end.

%% @doc Return where a value is sent by default: in a header if it is a
%% binary that fits in one, and does not start or end with whitespace.
%% Any other value is sent in the body.
default_where(Value)
        when is_binary(Value), byte_size(Value) =< ?MAX_HEADER_LENGTH ->
    case edge_whitespace(Value) of
        true -> body;
        false -> headers
    end;
default_where(_Value) ->
    body.

%% @doc Whether a value starts or ends with a space, tab, vertical tab or form
%% feed. HTTP strips spaces and tabs from a header value, and hackney strips
%% the other two as well, so such a value is not sent as one.
edge_whitespace(<<>>) -> false;
edge_whitespace(Value) ->
    lists:member(binary:first(Value), " \t\v\f")
        orelse lists:member(binary:last(Value), " \t\v\f").

%% @doc Multipart headers preserve literal backslashes and line breaks.
multipart_header_bytes_roundtrip_test() ->
    Opts = #{ <<"priv-wallet">> => ar_wallet:new() },
    Value = <<"before\r\ninjected: value\n\\n\\r\\after", 0, 255>>,
    Signed = hb_message:commit(#{
        <<"nested">> => #{ <<"value">> => Value, <<"body">> => Value }
    }, Opts),
    Wire = hb_message:convert(Signed,
        #{ <<"device">> => <<"httpsig@1.0">>, <<"bundle">> => true }, Opts),
    Decoded = hb_message:convert(Wire,
        <<"structured@1.0">>, <<"httpsig@1.0">>, Opts),
    ?assertEqual(Value, hb_ao:get(<<"nested/value">>, Decoded, Opts)),
    ?assertEqual(Value, hb_ao:get(<<"nested/body">>, Decoded, Opts)),
    ?assert(hb_message:verify(Decoded, all, Opts)),
    Node = hb_http_server:start_node(#{
        <<"priv-wallet">> => ar_wallet:new(), <<"store">> => hb_test_utils:test_store()
    }),
    ?assertEqual({ok, Value}, hb_http:post(Node,
        <<"//nested/value">>, Signed, Opts)),
    % hackney strips a vertical tab or form feed at either end of a header.
    ?assertMatch(
        {ok, #{ <<"k">> := <<"\va\f">> }},
        hb_http:post(
            Node,
            <<"//nested">>,
            #{ <<"nested">> => #{ <<"k">> => <<"\va\f">> } },
            Opts
        )
    ).

%% @doc Multipart parts distinguish an absent body from an empty binary body,
%% including when the empty value is covered by a nested commitment.
multipart_empty_body_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ar_wallet:new()
    },
    lists:foreach(
        fun(Child) ->
            Signed = hb_message:commit(Child, Opts),
            Parent = #{ <<"child">> => Signed },
            Encoded =
                hb_message:convert(
                    Parent,
                    #{ <<"device">> => <<"httpsig@1.0">>, <<"bundle">> => true },
                    Opts
                ),
            Decoded =
                hb_message:convert(
                    Encoded, <<"structured@1.0">>, <<"httpsig@1.0">>, Opts
                ),
            ?assertEqual(true, hb_message:deep_verify(Decoded, Opts)),
            ?assertEqual(
                Child,
                hb_message:uncommitted(
                    hb_maps:get(<<"child">>, Decoded, undefined, Opts)
                )
            )
        end,
        [
            #{ <<"body">> => <<>> },
            #{ <<"value">> => <<"present">>, <<"body">> => <<>> },
            #{ <<"value">> => <<"present">> }
        ]
    ).

%% @doc A multipart encoding preserves the message's content-type.
multipart_content_type_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ar_wallet:new()
    },
    Msg = #{
        <<"content-type">> => <<"text/plain">>,
        <<"body">> => <<"Example message.">>,
        <<"nested">> => #{ <<"value">> => 42 }
    },
    Signed = hb_message:commit(Msg, Opts, #{ <<"bundle">> => true }),
    % The flag as a binary, as it arrives over HTTP, signs the same form.
    ?assert(
        hb_message:verify(
            hb_message:commit(Msg, Opts, #{ <<"bundle">> => <<"true">> }),
            all,
            Opts
        )
    ),
    ?assert(
        lists:member(<<"content-type">>, hb_message:committed(Signed, all, Opts))
    ),
    Encoded =
        hb_message:convert(
            Signed,
            #{ <<"device">> => <<"httpsig@1.0">>, <<"bundle">> => true },
            Opts
        ),
    Decoded =
        hb_message:convert(Encoded, <<"structured@1.0">>, <<"httpsig@1.0">>, Opts),
    ?assertEqual(true, hb_message:deep_verify(Decoded, Opts)),
    ?assert(
        lists:member(<<"content-type">>, hb_message:committed(Decoded, all, Opts))
    ),
    ?assertNot(
        hb_message:verify(
            Decoded#{ <<"content-type">> => <<"text/html">> }, all, Opts
        )
    ),
    ?assertEqual(
        Msg,
        hb_message:uncommitted(hb_cache:ensure_all_loaded(Decoded, Opts), Opts)
    ),
    % An unbundled commitment verifies when the message is sent bundled.
    ?assert(
        hb_message:verify(
            hb_message:convert(
                hb_message:convert(
                    hb_message:commit(maps:without([<<"body">>], Msg), Opts),
                    #{ <<"device">> => <<"httpsig@1.0">>, <<"bundle">> => true },
                    Opts
                ),
                <<"structured@1.0">>,
                <<"httpsig@1.0">>,
                Opts
            ),
            all,
            Opts
        )
    ),
    % A browser sends the `boundary' unquoted, starting with `-'.
    ?assertEqual(
        #{ <<"a">> => <<"x">> },
        hb_message:convert(
            #{
                <<"content-type">> => <<"multipart/form-data; boundary=--b">>,
                <<"body">> =>
                    <<"----b\r\ncontent-disposition: form-data; name=\"a\"\r\n"
                        "\r\nx\r\n----b--\r\n">>
            },
            <<"structured@1.0">>,
            <<"httpsig@1.0">>,
            Opts
        )
    ),
    % A message's own `multipart/' `content-type' is kept and committed.
    Multipart =
        hb_message:commit(
            #{
                <<"content-type">> => <<"multipart/mixed; boundary=\"zz\"">>,
                <<"x">> => <<"1">>
            },
            Opts
        ),
    ?assert(
        hb_message:verify(
            hb_message:convert(
                hb_message:convert(Multipart, <<"httpsig@1.0">>, Opts),
                <<"structured@1.0">>,
                <<"httpsig@1.0">>,
                Opts
            ),
            all,
            Opts
        )
    ),
    % A message's own `content-type' of the form `application/x.ao.<name>' is
    % carried in a body part, and one of the form `application/<name>' is the
    % `content-type' of the HTTP message.
    ContentType =
        fun(Type) ->
            maps:get(
                <<"content-type">>,
                hb_message:convert(
                    #{ <<"content-type">> => Type, <<"body">> => <<"{}">> },
                    <<"httpsig@1.0">>,
                    Opts
                )
            )
        end,
    ?assertMatch(
        <<"multipart/form-data", _/binary>>,
        ContentType(<<"application/x.ao.json">>)
    ),
    ?assertEqual(<<"application/json">>, ContentType(<<"application/json">>)),
    % A `content-type' that is not UTF-8 is a header as well.
    ?assertEqual(<<16#E6>>, ContentType(<<16#E6>>)).

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
    Msg = #{
        <<"immediate-key">> => <<"immediate-value">>,
        <<"long-key">> => binary:copy(<<"a">>, 61),
        <<"short-key">> => <<"short-value">>,
        <<"typed-key">> => 4
    },
    {ok, Path} = hb_cache:write(Msg, #{}),
    {ok, Read} = hb_cache:read(Path, #{}),
    % Ensure that small values are materialized directly while larger values
    % stay lazy.
    ?assertEqual(<<"short-value">>, maps:get(<<"short-key">>, Read, #{})),
    ?assertEqual(4, maps:get(<<"typed-key">>, Read, #{})),
    ?assertMatch({link, _, _}, maps:get(<<"long-key">>, Read, #{})),
    ?assertEqual(Msg, hb_cache:ensure_all_loaded(Read, #{})),
    % Encode and decode the cached message as `httpsig@1.0`.
    Enc = hb_message:convert(Read, <<"httpsig@1.0">>, #{}),
    ?event({encoded, Enc}),
    Dec = hb_message:convert(Enc, <<"structured@1.0">>, <<"httpsig@1.0">>, #{}),
    % Ensure that the result is the same as the original message
    ?event({decoded, Dec}),
    ?assert(hb_message:match(Msg, Dec, strict, #{})).

nested_content_disposition_roundtrip_test() ->
    Msg = #{
        <<"child">> => #{
            <<"content-disposition">> => <<"attachment">>,
            <<"value">> => <<"x">>
        }
    },
    Codec = #{ <<"device">> => <<"httpsig@1.0">>, <<"bundle">> => true },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, #{}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assertEqual(Msg, Decoded),
    % Part header names are case-insensitive, as HTTP header names are.
    Capitalised =
        binary:replace(
            maps:get(<<"body">>, Encoded),
            <<"content-disposition: ">>,
            <<"Content-Disposition: ">>,
            [global]
        ),
    ?assertEqual(
        Msg,
        hb_message:convert(
            Encoded#{ <<"body">> => Capitalised },
            <<"structured@1.0">>,
            Codec,
            #{}
        )
    ),
    % A browser writes the `filename' of a file in UTF-8.
    ?assertEqual(
        #{ <<"a">> => <<"x">> },
        hb_message:convert(
            #{
                <<"content-type">> => <<"multipart/form-data; boundary=b">>,
                <<"body">> =>
                    <<"--b\r\ncontent-disposition: form-data; name=\"a\"; "
                        "filename=\"", 195, 169, "\"\r\n\r\nx\r\n--b--\r\n">>
            },
            <<"structured@1.0">>,
            Codec,
            #{}
        )
    ).

%% @doc Escape an index value that becomes a reply header, so it cannot split
%% the response.
index_header_value_escape_test() ->
    Value = <<"ok\r\n\r\nHTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n">>,
    Opts = #{
        <<"default-index">> => #{
            <<"device">> => #{
                <<"index">> => fun(_, _, _) -> {ok, #{ <<"zzz">> => Value }} end
            }
        }
    },
    {ok, Encoded} =
        to(
            #{ <<"foo">> => <<"bar">> },
            #{ <<"index">> => true },
            Opts
        ),
    ?assertEqual(
        hb_escape:encode_header(Value),
        hb_maps:get(<<"zzz">>, Encoded, not_found, #{})
    ).

%% @doc A message's own keys with the names of reserved headers are served by
%% ID and posted as they were written, over HTTP/1.1 and HTTP/2.
reserved_header_keys_test_() ->
    [
        {binary_to_list(<<Key/binary, ": ", Value/binary>>), fun() ->
            NodeOpts =
                #{
                    <<"priv-wallet">> => ar_wallet:new(),
                    <<"store">> => hb_test_utils:test_store()
                },
            Node = hb_http_server:start_node(NodeOpts),
            Msg =
                hb_message:commit(
                    #{
                        <<"path">> => <<"//", Key/binary>>,
                        <<"note">> => <<"kept">>,
                        <<"body">> => <<"hello">>,
                        Key => Value
                    },
                    #{ <<"priv-wallet">> => ar_wallet:new() }
                ),
            {ok, _} = hb_cache:write(Msg, NodeOpts),
            Path = <<"/", (hb_message:id(Msg, signed, NodeOpts))/binary>>,
            lists:foreach(
                fun(Opts) ->
                    {ok, Read} = hb_http:get(Node, Path, Opts),
                    ?assertEqual(
                        {Value, <<"kept">>},
                        {
                            hb_maps:get(Key, Read, not_found, Opts),
                            hb_maps:get(<<"note">>, Read, not_found, Opts)
                        }
                    ),
                    ?assert(hb_message:verify(Read, all, Opts)),
                    ?assertEqual({ok, Value}, hb_http:post(Node, Msg, Opts))
                end,
                [
                    #{ <<"store">> => hb_test_utils:test_store() },
                    #{
                        <<"store">> => hb_test_utils:test_store(),
                        <<"http-client">> => gun,
                        <<"protocol">> => http2
                    }
                ]
            )
        end}
    ||
        {Key, Value} <-
            [
                {<<"content-length">>, <<"3">>},
                {<<"connection">>, <<"foo bar">>},
                {<<"connection">>, <<"note">>},
                {<<"keep-alive">>, <<"timeout=5">>},
                {<<"proxy-connection">>, <<"keep-alive">>},
                {<<"te">>, <<"gzip">>},
                {<<"trailer">>, <<"x-note">>},
                {<<"transfer-encoding">>, <<"gzip">>},
                {<<"upgrade">>, <<"websocket">>},
                % The `ao-body-key' header gives the key of the body.
                {<<"ao-body-key">>, <<"note">>}
            ]
    ].
