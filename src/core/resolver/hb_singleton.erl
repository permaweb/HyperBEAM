%%% @doc A parser that translates AO-Core HTTP API requests in TABM format,
%%% and hashpaths, into an ordered list of messages to evaluate. The details
%%% of this format are described in `docs/ao-core-http-api.md'.
%%%
%%% Syntax overview:
%%% <pre>
%%%     Singleton: Message containing keys and a `path' field,
%%%                which may also contain a query string of key-value pairs.
%%%
%%%     Part: (Key + Resolution), Device?, #{ K => V}?, Claims?
%%%         - Part => #{ path => Part }
%%%         - `Part&Key=Value => #{ path => Part, Key => Value }'
%%%         - `Part=Value&... => #{ path => Part, Part => Value, ... }'
%%%         - `Part&Key => #{ path => Part, Key => true }'
%%%         - `Part&k1=v1&k2=v2 => #{ path => Part, k1 => `<<"v1">>', k2 => `<<"v2">>' }'
%%%         - `Part~Device => {as, Device, #{ path => Part }}'
%%%         - `Part~D&K1=V1 => {as, D, #{ path => Part, K1 => `<<"v1">>' }}'
%%%         - `pt&k1+integer=1 => #{ path => pt, k1 => 1 }'
%%%         - `pt~d&k1+integer=1 => {as, d, #{ path => pt, k1 => 1 }}'
%%%         - `(/nested/path) => Resolution of the path /nested/path'
%%%         - `(/nested/path&k1=v1) => (resolve /nested/path)#{k1 => v1}'
%%%         - `(/nested/path~D&K1=V1) => (resolve /nested/path)#{K1 => V1}'
%%%         - `pt&k1+resolve=(/a/b/c) => #{ path => pt, k1 => (resolve /a/b/c) }'
%%%         - `Part;VB+VR@D.R => #{ path => Part }' and its claims, which are
%%%           read by `hb_hashpath'
%%%     Key:
%%%         - key: `<<"value">>' => #{ key => `<<"value">>', ... } for all messages
%%%         - n.key: `<<"value">>' => #{ key => `<<"value">>', ... } for Nth message
%%%         - key+integer: 1 => #{ key => 1, ... }
%%%         - key+resolve: /nested/path => #{ key => (resolve /nested/path), ... }
%%%         - N.Key+resolve: /a/b/c => #{ Key => (resolve /a/b/c), ... }
%%% </pre>
%%%
%%% A path is a list of path parts, separated by `/'. The first path part is
%%% the base:
%%% - A first path part without a key is the carried message: the singleton's
%%%   other keys, with the device and keys of that part applied to it, as in
%%%   `//a', `/~meta@1.0' and `/&k=v'.
%%% - A first path part that is a key, alone or with a device, is read as the
%%%   `ao:' URI of that key, upon the node that executes the path. The device
%%%   is applied to the loaded message.
%%% - A first path part with a key that sets other keys, as in `/a&k=v' and
%%%   `/a~d@1.0&k=v', is a request upon the carried message.
%%% Each later path part is a request, applied in turn to the result of the
%%% parts before it. A hashpath is `ao:' followed by a path without its first
%%% `/', in which each request may be followed by claims about the result of
%%% its execution. `hb_hashpath' reads and checks these claims. For example:
%%% <pre>
%%%     //a/b           => [#{}, #{ path => a }, #{ path => b }]
%%%     /a/b            => [ao:a, #{ path => b }]
%%%     /a~d@1.0/b      => [{as, d@1.0, ao:a}, #{ path => b }]
%%%     /a&k=v/b        => [#{}, #{ path => a, k => v }, #{ path => b }]
%%%     /a~d@1.0&k=v/b  => [{as, d@1.0, #{ path => a, k => v }}, #{ path => b }]
%%%     /~meta@1.0/info => [{as, meta@1.0, #{}}, #{ path => info }]
%%%     /ID/ID2         => [ao:ID, #{ path => ID2 }]
%%%     /ID/ao:ID2      => [ao:ID, ao:ID2]
%%%     /ao:ID~d@1.0/k  => [{as, d@1.0, ao:ID}, #{ path => k }]
%%%     //a/(//b/c)/d   => [#{}, #{ path => a }, (resolve //b/c), #{ path => d }]
%%%     ao:ID/k.ID2     => [ao:ID, #{ path => k }], claiming its result is ID2
%%% </pre>
%%%
%%% The grammar of a path is given below in EBNF (ISO 14977), over the bytes
%%% of the path. `hb_singleton_lexer' and `hb_singleton_parser' implement it,
%%% and are generated from the `.xrl' and `.yrl' files of the same names that
%%% sit beside this module.
%%% <pre>
%%%     hashpath     = "ao:", path ;
%%%     path         = [ "/" ], [ part ], { "/", [ part ] }, [ "?", query ] ;
%%%     part         = subpath | step, [ claims ] ;
%%%     subpath      = group ;
%%%     group        = "(", { group byte | group | quoted }, ")" ;
%%%     quoted       = '"', { quoted byte | "\", byte }, '"' ;
%%%     step         = key
%%%                  | head, "=", value, [ items ]
%%%                  | [ head ], "~", device, [ items ]
%%%                  | [ head ], items ;
%%%     head         = key | group ;
%%%     key          = ( key byte | quoted ), { key byte | group | quoted }
%%%                  | group, ( key byte | group | quoted ),
%%%                    { key byte | group | quoted } ;
%%%     value        = { value byte | group | quoted } ;
%%%     device       = ( value byte | group | quoted ), value ;
%%%     items        = "&", item, { "&", item } ;
%%%     item         = [ item key, [ "=", value ] ] ;
%%%     item key     = ( key byte | "~" | group | quoted ),
%%%                    { key byte | "~" | group | quoted } ;
%%%     claims       = varied, [ dependencies ], [ equivalence ]
%%%                  | dependencies, [ equivalence ]
%%%                  | equivalence ;
%%%     varied       = ";", id, "+", id ;
%%%     dependencies = "@", id ;
%%%     equivalence  = ( "." | "*" ), id ;
%%%     id           = 43 * ( letter | digit | "-" | "_" ) ;
%%%     query        = ? the bytes to the end of the path, read as an
%%%                    application/x-www-form-urlencoded string ? ;
%%%     key byte     = byte - ( "/" | "?" | "&" | "=" | "~" | "(" | ")" | '"' ) ;
%%%     value byte   = key byte | "=" | "~" ;
%%%     group byte   = value byte | "&" | "/" | "?" ;
%%%     quoted byte  = byte - ( '"' | "\" ) ;
%%% </pre>
%%%
%%% The grammar does not express the following rules, which are applied to
%%% the parts that it reads:
%%% <ul>
%%%   <li>A key that starts with a URI scheme and its `:' (see `hb_uri'),
%%%       alone or with only a device, is that URI, percent-decoded. A claim
%%%       or a device after the URI is that of its part, so the characters of
%%%       the URI that are the syntax of a part (`.', `~', `&', `=', `+', `;',
%%%       `@', `*', `(', `)', `/', `?', `%', `"' and space) must be
%%%       percent-encoded, as must `#'. A URI alone is a step of its own: as
%%%       the first part it is the base, and after the base it is the request
%%%       that it identifies. A URI with a device is the message that the
%%%       device is applied to. With `=' or `&key=value', a key that starts
%%%       with a URI scheme is read as any other key.</li>
%%%   <li>A key that is a URI in host form (see `hb_uri'): `uri-', a scheme,
%%%       `-' and the rest, all in lowercase letters, digits and `-', is read
%%%       as any other URI.</li>
%%%   <li>The query of a path starts at the first `?' that is not inside a
%%%       group.</li>
%%%   <li>Claims are only read at the end of a part, in the order that the
%%%       grammar gives them. Anywhere else, `;', `@', `.' and `*' are
%%%       ordinary characters of a key, as they are when the text after them
%%%       is not an ID of 43 bytes. For example, `~meta@1.0' is a device,
%%%       and `a.b' is a key. These characters are not in the base64url
%%%       alphabet of IDs, so a claim never needs to be escaped.</li>
%%%   <li>A key is read in the same way whatever its length. A message is
%%%       given by its URI, such as `ao:<ID>'.</li>
%%%   <li>Claims follow a request, never the base.</li>
%%%   <li>In `key=value', and in `&key=value' after a step, the key is split
%%%       at its first `+' or space that is not inside a group or a quoted
%%%       string. The text after it is the type of the value: `k+integer=1'
%%%       sets `k' to the integer 1.</li>
%%%   <li>A value or a device that is a single group, such as `(/a/b)', is a
%%%       subpath. It is resolved, and its result is used in its place.</li>
%%%   <li>A hashpath in a path is given as a group, which is a subpath, as in
%%%       `/(ao:ID/k.ID2)/d'. A client or a proxy may decode `%2E' to `.' and
%%%       `%7E' to `~' (RFC 3986 s2.3): `/ao:ID%2Fk%2EID2/d' may reach the
%%%       node as `/ao:ID%2Fk.ID2/d', in which `.ID2' is a claim of the
%%%       part. A subpath with claims is loaded as a hashpath: `hb_hashpath'
%%%       resolves its requests and ignores the claims.</li>
%%%   <li>The key of a step keeps its percent-escapes, unless the step is a
%%%       URI or gives the key a value with `='. Every other key, value and
%%%       device is percent-decoded after it is parsed.</li>
%%% </ul>
-module(hb_singleton).
-export([from/2, from_request/2, from_path/1, to/1, to_part/1, parse/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The characters that cannot be written as they are in a key, or in a value
%%% or a device, of a path part.
-define(KEY_SYNTAX, [<<"/">>, <<"?">>, <<"&">>, <<"=">>, <<"~">>, <<"(">>,
    <<")">>, <<"\"">>, <<":">>, <<" ">>]).
-define(VALUE_SYNTAX, [<<"/">>, <<"?">>, <<"&">>, <<"(">>, <<")">>, <<"\"">>,
    <<"%">>]).

%%% The tokens that a syntax error may list as expected, with the names that
%%% it gives them.
-define(SYNTAX_TOKENS, [
    {text, <<"text">>},
    {'(', <<"'('">>},
    {')', <<"')'">>},
    {'/', <<"'/'">>},
    {'&', <<"'&'">>},
    {'=', <<"'='">>},
    {'~', <<"'~'">>},
    {result, <<"a claim">>}
]).

-type ao_message() :: map() | binary().
-type tabm_message() :: map().

%% @doc Convert a list of AO-Core message into TABM message.
-spec to(list(ao_message())) -> tabm_message().
to(Messages) ->
    % Iterate through all AO-Core messages folding them into the TABM message
    % Scopes contains the following map: #{Key => [StageIndex, StageIndex2...]}
    % that allows to scope keys to the given stage.
    {TABMMessage, _FinalIndex, Scopes} =
        lists:foldl(
            fun
                % Special case when AO-Core message is a URI
                (Message, {Acc, Index, ScopedModifications}) when is_binary(Message) ->
                    {append_path(Message, Acc), Index + 1, ScopedModifications};
                % Special case when AO-Core message can be written as one part
                ({part, Part}, {Acc, Index, ScopedModifications}) ->
                    {append_path(Part, Acc), Index + 1, ScopedModifications};
                % Special case when AO-Core message contains resolve command
                ({resolve, SubMessages0}, {Acc, Index, ScopedModifications}) ->
                    SubMessages1 = hb_maps:get(<<"path">>, to(SubMessages0)),
                    SubMessages = <<"(", SubMessages1/binary, ")">>,
                    {append_path(SubMessages, Acc), Index + 1, ScopedModifications};
                % Regular case when message is a map
                (Message, {Acc, Index, ScopedModifications}) ->
                    {NewMessage, NewScopedModifications} =
                        hb_maps:fold(
                            fun
                                (<<"path">>, PathPart, {AccIn, Scoped}) ->
                                    {append_path(PathPart, AccIn), Scoped};
                                % Specifically ignore method field from scope modifications
                                (<<"method">>, Value, {AccIn, Scoped}) ->
                                    {hb_maps:put(<<"method">>, Value, AccIn), Scoped};
                                (Key, {resolve, SubMessages}, {AccIn, Scoped}) ->
                                    NewKey = <<Key/binary, "+resolve">>,
                                    NewSubMessages = hb_maps:get(<<"path">>, to(SubMessages)),
                                    {
                                        hb_maps:put(NewKey, NewSubMessages, AccIn),
                                        hb_maps:update_with(
                                            NewKey,
                                            fun(Indexes) -> [Index | Indexes] end,
                                            [Index],
                                            Scoped
                                        )
                                    };
                                (Key, Value, {AccIn, Scoped}) ->
                                    {
                                        hb_maps:put(Key, Value, AccIn),
                                        hb_maps:update_with(
                                            Key,
                                            fun(Indexes) -> [Index | Indexes] end,
                                            [Index],
                                            Scoped
                                        )
                                    }
                            end,
                            {Acc, ScopedModifications},
                            Message),
                        {NewMessage, Index + 1, NewScopedModifications}

            end,
            {base_path(Messages), 0, #{}},
            smallest(Messages)),
    MessageWithTypeAndScopes =
        hb_maps:fold(
            fun
                % For the case when a given Key appeared only once in scopes
                (Key, [SingleIndexScope], AccIn) ->
                    Index = integer_to_binary(SingleIndexScope),
                    {Value, NewAccIn} = hb_maps:take(Key, AccIn),
                    {NewKey, NewValue} =
                        case type(Value) of
                            integer ->
                                K = <<Index/binary, ".", Key/binary, "+integer">>,
                                V = hb_structured_fields:bare_item(Value),
                                {K, V};
                            _ -> {<<Index/binary, ".", Key/binary>>, Value}
                        end,
                    hb_maps:put(NewKey, NewValue, NewAccIn);
                (_Key, _Value, AccIn) -> AccIn
            end,
            TABMMessage,
            Scopes),
    MessageWithTypeAndScopes.

%% @doc Start the path of a singleton. A base that is a message without a
%% path is the carried message, so the path starts with an empty path part.
base_path([Base | _]) when is_map(Base) ->
    case is_map_key(<<"path">>, Base) orelse smallest([Base]) =/= [Base] of
        true -> #{};
        false -> #{ <<"path">> => <<"/">> }
    end;
base_path(_) -> #{}.

%% @doc Mark the messages that `to_part/1' can write in their smallest form:
%% every request that has one, a base that is only a device, a base that is
%% the `ao:' URI of a key, with or without a device, and a base that is any
%% other hashpath or the resolution of one.
smallest([]) -> [];
smallest([Base | Requests]) ->
    [
        case Base of
            #{ <<"device">> := _ } when map_size(Base) == 1 -> smallest_part(Base);
            {as, _, Msg} when map_size(Msg) == 0 -> smallest_part(Base);
            {as, D, <<"ao:", Key/binary>>} ->
                smallest_part({as, D, #{ <<"path">> => Key }});
            <<"ao:", Key/binary>> ->
                case to_part(#{ <<"path">> => Key }) of
                    {ok, Part} -> {part, Part};
                    error -> smallest_part(Base)
                end;
            {resolve, _} -> smallest_part(Base);
            _ -> Base
        end
    |
        lists:map(fun smallest_part/1, Requests)
    ].

smallest_part(Msg) ->
    case to_part(Msg) of
        {ok, Part} -> {part, Part};
        error -> Msg
    end.

%% @doc Write a message in its smallest form, as a single path part. Returns
%% `error' if the message has no such form that `from/2' reads back as the
%% same message. The forms are:
%% <pre>
%%     #{ path => X }               => X
%%     #{ path => X, X => V }       => X=V
%%     #{ device => D }             => ~D
%%     {as, D, #{ path => X }}      => X~D
%%     {as, D, ao:K}                => ao:K~D
%%     {resolve, [ao:P]}            => (ao:P)
%%     {resolve, [M1, M2, ...]}     => (ao:P1/P2/...)
%%     ao:P                         => ao:P, or (ao:P) if P is not a key
%% </pre>
%% `from/2' reads `(ao:P)' as a subpath, which resolves to the message that
%% the hashpath `ao:P' gives. The parts of a subpath are those that
%% `smallest/1' gives its messages, so each message must have one.
to_part(#{ <<"device">> := Device } = Msg) when map_size(Msg) == 1 ->
    to_part({as, Device, #{}});
to_part({as, Device, Msg}) when map_size(Msg) == 0 ->
    maybe
        {ok, D} ?= literal(Device, ?VALUE_SYNTAX),
        true ?= byte_size(D) > 0 orelse error,
        {ok, <<"~", D/binary>>}
    end;
to_part({as, Device, #{ <<"path">> := Key } = Msg}) when map_size(Msg) == 1 ->
    maybe
        {ok, K} ?= literal(Key, ?KEY_SYNTAX),
        {ok, <<"~", D/binary>>} ?= to_part({as, Device, #{}}),
        {ok, <<K/binary, "~", D/binary>>}
    end;
to_part({as, Device, <<"ao:", Key/binary>>}) ->
    maybe
        {ok, _} ?= literal(Key, [<<"%">> | ?KEY_SYNTAX]),
        {ok, <<"~", D/binary>>} ?= to_part({as, Device, #{}}),
        {ok, <<"ao:", Key/binary, "~", D/binary>>}
    end;
to_part(#{ <<"path">> := Key } = Msg) when map_size(Msg) == 1 ->
    maybe
        {ok, K} ?= literal(Key, ?KEY_SYNTAX),
        true ?= byte_size(K) > 0 orelse error,
        {ok, K}
    end;
to_part(#{ <<"path">> := Key } = Msg) when map_size(Msg) == 2 ->
    maybe
        {ok, Value} ?= maps:find(Key, Msg),
        {ok, K} ?= literal(Key, [<<"%">>, <<"+">> | ?KEY_SYNTAX]),
        {ok, V} ?= literal(Value, ?VALUE_SYNTAX),
        true ?= byte_size(K) > 0 orelse error,
        {ok, <<K/binary, "=", V/binary>>}
    end;
to_part({resolve, [<<"ao:", _/binary>> = Hashpath]}) ->
    {ok, <<"(", Hashpath/binary, ")">>};
to_part({resolve, Msgs = [_ | _]}) ->
    Parts = [ Part || {part, Part} <- smallest(Msgs) ],
    maybe
        true ?= length(Parts) == length(Msgs) orelse error,
        Path = iolist_to_binary(lists:join(<<"/">>, Parts)),
        {ok, <<"(ao:", Path/binary, ")">>}
    end;
to_part(<<"ao:", Path/binary>> = Hashpath) ->
    case literal(Path, [<<"%">> | ?KEY_SYNTAX]) of
        {ok, _} -> {ok, Hashpath};
        error -> to_part({resolve, [Hashpath]})
    end;
to_part(_) -> error.

%% @doc Check that a binary can be written in a path part as it is. It must
%% not contain any of the given characters, and if it is long enough to end in a
%% claim, it must not contain a claim separator. Returns `{ok, Bin}' or `error'.
literal(Bin, Syntax) when is_binary(Bin) ->
    case binary:match(Bin, Syntax) of
        nomatch when byte_size(Bin) < 44 -> {ok, Bin};
        nomatch ->
            case binary:match(Bin, [<<";">>, <<"@">>, <<".">>, <<"*">>]) of
                nomatch -> {ok, Bin};
                _ -> error
            end;
        _ -> error
    end;
literal(_, _) -> error.

append_path(PathPart, #{<<"path">> := Path} = Message) ->
    hb_maps:put(<<"path">>, <<Path/binary, "/", PathPart/binary>>, Message);
append_path(PathPart, Message) ->
    hb_maps:put(<<"path">>, <<"/", PathPart/binary>>, Message).

type(Value) when is_binary(Value) -> binary;
type(Value) when is_integer(Value) -> integer;
type(_Value) -> unknown.

%% @doc Normalize a singleton TABM message into a list of executable AO-Core
%% messages. Throws `{invalid_singleton, Type, Offender}' if a part of the
%% message cannot be parsed, where `Offender' is that part.
from(RawMsg, Opts) ->
    requests(parse(RawMsg, Opts)).

%% @doc Parse a singleton TABM message, a path or a hashpath into the contexts
%% of its steps. The first context has the base, and each context has a
%% request and the claims that follow it in the path. A base or a request that
%% is a URI is given as its `base-uri' or `request-uri', and each claim as the
%% `ao:' URI of its ID. Throws as `from/2' does.
parse(RawMsg, Opts) when is_binary(RawMsg) ->
    parse(#{ <<"path">> => RawMsg }, Opts);
parse(RawMsg, Opts) ->
    RawPath = hb_maps:get(<<"path">>, RawMsg, <<>>, Opts),
    ?event_debug(parsing, {raw_path, RawPath}),
    {ok, Parts, Query} = from_path(RawPath),
    ?event_debug(parsing, {parsed_path, Parts, Query}),
    MsgWithoutBasePath =
        hb_maps:merge(
            hb_maps:remove(<<"path">>, RawMsg, Opts),
            Query,
            Opts
        ),
    contexts(Parts, MsgWithoutBasePath, Opts).

%% @doc Build the contexts of the parts of a path. The other keys of the
%% singleton are added to the messages, as their scopes direct.
contexts(Parts, MsgWithoutBasePath, Opts) ->
    % 2. Build the message of each part, and collect the claims after it.
    {RawMsgs, Claims} =
        lists:unzip(normalize_base([ build_part(Part, Opts) || Part <- Parts ])),
    ?event_debug(parsing, {raw_messages, RawMsgs}),
    % 3. Type keys and values
    Typed = apply_types(MsgWithoutBasePath, Opts),
    ?event_debug(parsing, {typed_messages, Typed}),
    % 4. Group keys by N-scope and global scope
    ScopedModifications = group_scoped(Typed, RawMsgs, Opts),
    ?event_debug(parsing, {scoped_modifications, ScopedModifications}),
    % 5. Generate the list of messages (plus-notation, device, typed keys).
    Result = build_messages(RawMsgs, ScopedModifications, Opts),
    ?event_debug(parsing, {result, Result}),
    pair(Result, Claims).

%% @doc Pair each request with the claims that follow it, and add the base to
%% the first context. Throws `invalid_claim' if the base is followed by claims.
pair([], []) -> [];
pair([Base | Requests], [[] | Claims]) ->
    BaseCtx = step_context(<<"base">>, Base),
    case lists:zipwith(fun request_context/2, Requests, Claims) of
        [] -> [BaseCtx];
        [First | Rest] -> [maps:merge(BaseCtx, First) | Rest]
    end;
pair(_Msgs, [[Claim | _] | _]) ->
    throw({invalid_singleton, invalid_claim, text([Claim])}).

%% @doc Make the context of a request, with the keys of its claims.
request_context(Req, Claims) ->
    lists:foldl(fun claim_context/2, step_context(<<"request">>, Req), Claims).

%% @doc Make the context of a base or a request. A base or a request that is a
%% URI is held in `base-uri' or `request-uri'.
step_context(Name, URI) when is_binary(URI) ->
    #{ <<Name/binary, "-uri">> => URI };
step_context(Name, Step) -> #{ Name => Step }.

%% @doc Add the keys of a claim to a context, each as the `ao:' URI of the ID
%% that it claims.
claim_context({varied, _, {Base, Req, _}}, Ctx) ->
    Ctx#{
        <<"varied-base-uri">> => <<"ao:", (hb_util:bin(Base))/binary>>,
        <<"varied-request-uri">> => <<"ao:", (hb_util:bin(Req))/binary>>
    };
claim_context({dependencies, _, {ID, _}}, Ctx) ->
    Ctx#{ <<"dependencies-uri">> => <<"ao:", (hb_util:bin(ID))/binary>> };
claim_context({result, _, {ID, _}}, Ctx) ->
    Ctx#{ <<"varied-result-uri">> => <<"ao:", (hb_util:bin(ID))/binary>> };
claim_context({extension, _, {ID, _}}, Ctx) ->
    Ctx#{
        <<"varied-result-uri">> => <<"ao:", (hb_util:bin(ID))/binary>>,
        <<"normalizer">> => base
    }.

%% @doc Convert a list of contexts into the messages to execute: the base,
%% then each request. A base or a request given by its URI is the URI.
requests([]) -> [];
requests(Contexts = [First | _]) ->
    [
        case First of
            #{ <<"base-uri">> := URI } -> URI;
            #{ <<"base">> := Base } -> Base
        end
    |
        [
            case Ctx of
                #{ <<"request-uri">> := URI } -> URI;
                #{ <<"request">> := Req } -> Req
            end
        ||
            Ctx <- Contexts,
            is_map_key(<<"request">>, Ctx) orelse
                is_map_key(<<"request-uri">>, Ctx)
        ]
    ].

%% @doc Parse a request's path and apply its fields to each request.
from_request(Req, Opts) ->
    Path = hb_maps:get(<<"path">>, Req, undefined, Opts),
    {ok, Parts, Query} = from_path(Path),
    Fields = hb_maps:merge(Req, Query, Opts),
    [
        case build_part(Part, Opts) of
            {Msg, _} when is_map(Msg) -> hb_maps:merge(Fields, Msg, Opts);
            {Subres = {resolve, _}, _} -> Subres;
            {Step, _} -> Fields#{ <<"path">> => Step }
        end
    ||
        Part <- Parts
    ].

%% @doc Parse the relative reference into its parts and its query. The parts
%% of a hashpath follow its `ao:'. A path given as a list is already split:
%% each element is one part, even if it contains a `/'.
from_path(Parts) when is_list(Parts) -> {ok, lists:map(fun list_part/1, Parts), #{}};
from_path(RelativeRef) when is_binary(RelativeRef) ->
    {PathTokens, QStr} = split_query(tokens(RelativeRef)),
    {ok, parts(PathTokens), parse_query(QStr)};
from_path(Other) -> from_path(hb_util:bin(Other)).

%% @doc Parse an element of a path that is given as a list. In a binary, a
%% `/' or `?' outside of a group is read as a character of the key, not as a
%% separator. Any other term is used as the step as it is.
list_part(<<>>) -> {step, {key, []}, []};
list_part(Part) when is_binary(Part) ->
    {Tokens, _} =
        lists:mapfoldl(
            fun
                ({Separator, Loc}, 0) when Separator == '/'; Separator == '?' ->
                    {{text, Loc, atom_to_list(Separator)}, 0};
                (Token = {'(', _}, Depth) -> {Token, Depth + 1};
                (Token = {')', _}, Depth) when Depth > 0 -> {Token, Depth - 1};
                (Token, Depth) -> {Token, Depth}
            end,
            0,
            lex(Part)
        ),
    [Parsed] = parts(Tokens),
    Parsed;
list_part(Part) -> {term, Part}.

%% @doc Scan a path into its tokens. The `ao:' of a hashpath is dropped.
tokens(<<"ao:", _/binary>> = Hashpath) ->
    [{scheme, _, "ao:"} | Tokens] = lex(Hashpath),
    Tokens;
tokens(Path) -> lex(Path).

%% @doc Scan a path with `hb_singleton_lexer'. Each byte of the path is given
%% to the lexer as one character, followed by the code point 256 to mark the
%% end of the path.
lex(Path) ->
    {ok, Tokens, _} = hb_singleton_lexer:string(binary_to_list(Path) ++ [256]),
    Tokens.

%% @doc Split the tokens of a path at the first `?' that is not inside a
%% group. Returns the tokens before it, and the text of the query after it.
split_query(Tokens) -> split_query(Tokens, 0, []).
split_query([{'?', _} | Query], 0, Path) ->
    {lists:reverse(Path), text(Query)};
split_query([Token = {'(', _} | Rest], Depth, Path) ->
    split_query(Rest, Depth + 1, [Token | Path]);
split_query([Token = {')', _} | Rest], Depth, Path) when Depth > 0 ->
    split_query(Rest, Depth - 1, [Token | Path]);
split_query([Token | Rest], Depth, Path) ->
    split_query(Rest, Depth, [Token | Path]);
split_query([], _Depth, Path) ->
    {lists:reverse(Path), <<>>}.

%% @doc Parse the tokens of a path into its parts with `hb_singleton_parser'.
%% If the path does not follow the grammar, we throw `invalid_path' with a
%% description of the error.
parts(Tokens) ->
    case hb_singleton_parser:parse(Tokens) of
        {ok, Parts} -> Parts;
        {error, {Loc, _, [_, Before]}} ->
            throw(
                {invalid_singleton,
                    invalid_path,
                    syntax_error(Tokens, Loc, Before =/= [])
                }
            )
    end.

%% @doc Describe a syntax error in a path: the character at which it stops
%% following the grammar, what was found there, and the tokens that the
%% grammar would accept instead. The parser gives the location of the token
%% that it cannot accept, or of the last token if the path ends too soon.
syntax_error(Tokens, Loc, AtToken) ->
    {Accepted, Found} =
        case AtToken of
            true ->
                lists:splitwith(
                    fun(Token) -> element(2, Token) =/= Loc end,
                    Tokens
                );
            false -> {Tokens, []}
        end,
    Expected =
        [
            Name
        ||
            {Category, Name} <- ?SYNTAX_TOKENS,
            accepts(Accepted ++ [syntax_token(Category)])
        ] ++
        [
            <<"the end of the path">>
        ||
            {ok, _} <- [hb_singleton_parser:parse(Accepted)]
        ],
    iolist_to_binary(
        [
            <<"character ">>,
            integer_to_binary(character(Tokens, Accepted)),
            <<": expected ">>,
            lists:join(<<", ">>, Expected),
            <<"; found ">>,
            case {Found, open_group(Tokens)} of
                {[{'"', _} | _], _} -> <<"a quote that is not closed">>;
                {[Other | _], _} -> [<<"'">>, text([Other]), <<"'">>];
                {[], none} -> <<"the end of the path">>;
                {[], BeforeOpen} ->
                    [
                        <<"the end of the path, with the '(' at character ">>,
                        integer_to_binary(character(Tokens, BeforeOpen)),
                        <<" not closed">>
                    ]
            end,
            <<".">>
        ]
    ).

%% @doc Find the first `(' in a list of tokens that is not closed, and return
%% the tokens before it. Returns `none' if every group is closed.
open_group(Tokens) ->
    Open =
        lists:foldl(
            fun
                ({'(', Loc}, Locs) -> [Loc | Locs];
                ({')', _}, [_ | Locs]) -> Locs;
                (_, Locs) -> Locs
            end,
            [],
            Tokens
        ),
    case Open of
        [] -> none;
        _ ->
            OpenLoc = lists:last(Open),
            {Before, _} =
                lists:splitwith(
                    fun(Token) -> element(2, Token) =/= OpenLoc end,
                    Tokens
                ),
            Before
    end.

%% @doc Find the position of the character that follows the given leading
%% tokens of a path, counted in bytes from the start of the text. We take the
%% column of the first token from the lexer, as only `ao:' can come before it,
%% and add the bytes of the tokens that follow. The lexer's columns cannot
%% be used for later tokens: they do not count bytes after a tab, a newline,
%% a byte above 127, or a separator that does not start a claim, such as the
%% `@' of `~meta@1.0'.
character([], _Before) -> 1;
character([First | _], Before) ->
    {1, Column} = element(2, First),
    Column + byte_size(text(Before)).

%% @doc Check whether the grammar accepts a list of tokens as the start of a
%% path. A list that ends too soon is accepted. The list is refused only if
%% the parser stops at a token made by `syntax_token/1', which has the
%% location `{0, 0}'.
accepts(Tokens) ->
    case hb_singleton_parser:parse(Tokens) of
        {error, {{0, 0}, _, [_, [_ | _]]}} -> false;
        _ -> true
    end.

%% @doc Make a token of the given category, to test with `accepts/1'. It has
%% the location `{0, 0}', which no token of a path has.
syntax_token(Category) when Category == text; Category == quoted;
        Category == scheme ->
    {Category, {0, 0}, "x"};
syntax_token(varied) -> {varied, {0, 0}, {"x", "x", "x"}};
syntax_token(Category) when Category == dependencies; Category == result;
        Category == extension ->
    {Category, {0, 0}, {"x", "x"}};
syntax_token(Category) -> {Category, {0, 0}}.

%% @doc Return the text of a list of tokens. Every byte of a path is in
%% exactly one token, so this is the text of the path that the tokens cover.
text(Tokens) ->
    iolist_to_binary(lists:map(fun token_text/1, Tokens)).

token_text({group, _, Tokens}) -> [$(, lists:map(fun token_text/1, Tokens), $)];
token_text({varied, _, {_, _, Text}}) -> Text;
token_text({_, _, {_, Text}}) -> Text;
token_text({_, _, Chars}) -> Chars;
token_text({Separator, _}) -> atom_to_list(Separator).

%% @doc Parse a query string into a message of its unquoted values, with the
%% keys in lowercase, or throw `invalid_query' with the query string. A query
%% is caller input, so its private keys are removed before they are merged
%% into a message.
parse_query(QStr) ->
    try
        filter_private_query_params(
            maps:map(
                fun(_, Val) -> hb_util:unquote(Val) end,
                hb_maps:from_list(
                    [
                        {hb_util:lowercase_key(Key), Val}
                    ||
                        {Key, Val} <- cowboy_req:parse_qs(#{ qs => QStr })
                    ]
                )
            )
        )
    catch _:_ -> throw({invalid_singleton, invalid_query, QStr})
    end.

%% @doc Remove the private keys of a query. A key may be scoped to a step as
%% `N.Key', and the scope is applied after parsing, so the check uses the key
%% that will reach the message.
filter_private_query_params(Query) ->
    maps:filter(
        fun(Key, _Val) -> not hb_private:is_private(unscope_key(Key)) end,
        Query
    ).

%% @doc Return the key without its optional numeric step scope.
unscope_key(Key) ->
    case binary:split(Key, <<".">>) of
        [Scope, Unscoped] ->
            case catch binary_to_integer(Scope) of
                N when is_integer(N), N >= 0 -> Unscoped;
                _ -> Key
            end;
        _ -> Key
    end.

%% @doc Normalize the base path. A first path part without a key is the
%% carried message, with the rest of the part applied to it. A key, alone or
%% with a device, is read as an `ao:' URI. A key that sets other keys, with or
%% without a device, is a request upon the carried message.
normalize_base([]) -> [];
normalize_base([{empty, Claims} | Rest]) -> [{#{}, Claims} | Rest];
normalize_base([First = {Base, _} | Rest]) when is_binary(Base) -> [First | Rest];
normalize_base([{{as, Dev, Raw = #{ <<"path">> := Key }}, Claims} | Rest])
        when map_size(Raw) == 1, Key =/= <<>> ->
    [{{as, Dev, <<"ao:", Key/binary>>}, Claims} | Rest];
normalize_base([First = {{as, _, _}, _} | Rest]) -> [First | Rest];
normalize_base([First = {{resolve, _}, _} | Rest]) -> [First | Rest];
normalize_base([{Base = #{ <<"path">> := <<>> }, Claims} | Rest]) ->
    [{maps:remove(<<"path">>, Base), Claims} | Rest];
normalize_base([{Base = #{ <<"path">> := Key }, Claims} | Rest])
        when map_size(Base) == 1 ->
    [{<<"ao:", Key/binary>>, Claims} | Rest];
normalize_base(Rest) -> [{#{}, []} | Rest].

%% @doc Step 3: Apply types to values and remove specifiers. A link is loaded
%% only when its key has a type.
apply_types(Msg, Opts) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            {_, K, V} = maybe_typed(Key, Val, Opts),
            hb_maps:put(K, V, Acc, Opts)
        end,
        #{},
        Msg
    ).

%% @doc Step 4: Group headers/query by N-scope.
%% `N.Key' => applies to Nth step. Otherwise => `global'
group_scoped(Map, Msgs, Opts) ->
    {NScope, Global} =
        maps:fold(
            fun(KeyBin, Val, {Ns, Gs}) ->
                case parse_scope(KeyBin) of
                    {OkN, RealKey} ->
                        Curr = hb_maps:get(OkN, Ns, #{}, Opts),
                        Ns2 = hb_maps:put(OkN,
                            put_unless_private(RealKey, Val, Curr, Opts),
                            Ns,
                            Opts
                        ),
                        {Ns2, Gs};
                    global -> {Ns, hb_maps:put(KeyBin, Val, Gs, Opts)}
                end
          end,
          {#{}, #{}},
          Map
        ),
    [
        hb_maps:merge(Global, hb_maps:get(N, NScope, #{}, Opts), Opts)
    ||
        N <- lists:seq(1, length(Msgs))
    ].

%% @doc Get the scope of a key. Adds 1 to account for the base message.
parse_scope(KeyBin) ->
    case binary:split(KeyBin, <<".">>, [global]) of
        [Front, Remainder] ->
            case catch erlang:binary_to_integer(Front) of
                NInt when is_integer(NInt), NInt >= 0, Remainder =/= <<>> ->
                    {NInt + 1, Remainder};
                _ -> throw({invalid_singleton, invalid_scope, KeyBin})
            end;
        _ -> global
    end.

%% @doc Step 5: Merge the base message with the scoped messages.
build_messages(Msgs, ScopedModifications, Opts) ->
    do_build(1, Msgs, ScopedModifications, Opts).

do_build(_, [], _, _) -> [];
do_build(I, [{as, DevID, RawMsg} | Rest], ScopedKeys, Opts) when is_map(RawMsg) ->
    % We are processing an `as' message. If the path is empty, we need to
    % remove it from the message and the additional message, such that AO-Core
    % returns only the message with the device specifier changed. If the message
    % does have a path, AO-Core will subresolve it.
    RawAdditional = lists:nth(I, ScopedKeys),
    {Msg, Additional} =
        case hb_maps:get(<<"path">>, RawMsg, <<"">>, Opts) of
            <<"">> ->
                % When we have an empty path, we remove the path from both
                % messages. AO-Core will then simply set the device specifier
                % and not execute a subresolve.
                {
                    hb_maps:remove(<<"path">>, RawMsg, Opts),
                    hb_maps:remove(<<"path">>, RawAdditional, Opts)
                };
            _BasePath ->
                % When we have a non-empty path, we merge the messages in
                % totality. The path-part's path will be subresolved.
                {RawMsg, RawAdditional}
        end,
    Merged = hb_maps:merge(Additional, Msg, Opts),
    StepMsg = hb_message:convert(
        Merged,
        <<"structured@1.0">>,
        Opts#{ <<"topic">> => ao_internal, <<"linkify-mode">> => keep }
    ),
    ?event_debug(parsing, {build_messages, {base, Msg}, {additional, Additional}}),
    [{as, DevID, StepMsg} | do_build(I + 1, Rest, ScopedKeys, Opts)];
do_build(I, [Msg | Rest], ScopedKeys, Opts) when not is_map(Msg) ->
    [Msg | do_build(I + 1, Rest, ScopedKeys, Opts)];
do_build(I, [Msg | Rest], ScopedKeys, Opts) ->
    Additional = lists:nth(I, ScopedKeys),
    Merged = hb_maps:merge(Additional, Msg, Opts),
    StepMsg = hb_message:convert(
        Merged,
        <<"structured@1.0">>,
        Opts#{ <<"topic">> => ao_internal, <<"linkify-mode">> => keep }
    ),
    ?event_debug(parsing, {build_messages, {base, Msg}, {additional, Additional}}),
    [StepMsg | do_build(I + 1, Rest, ScopedKeys, Opts)].

%% @doc Build the step of a part, and return it with the claims that follow
%% it. A subpath is parsed as a path, and the keys of a step are built in the
%% following order:
%% 1. The key, and its value after `=' or its device after `~'
%% 2. Inlined key-value pairs after `&'
%% 3. The device specifier
build_part(empty, _Opts) -> {empty, []};
build_part({term, Term}, _Opts) -> {Term, []};
build_part({subpath, {group, _, Tokens}}, Opts) ->
    {{resolve, subpath(Tokens, Opts)}, []};
build_part({step, Step, Claims}, Opts) -> {step(Step, Opts), Claims}.

%% @doc Build the message of a step. A key that starts with a URI scheme is
%% the URI, percent-decoded: alone, it is the step, and with a device and no
%% other keys, it is the message that the device is applied to.
step({key, Tokens = [{scheme, _, _} | _]}, _Opts) ->
    decode_string(text(Tokens));
step({key, Tokens}, _Opts) ->
    #{ <<"path">> => path_key(text(Tokens)) };
step({value, KeyTokens, ValueTokens, Items}, Opts) ->
    % The key of `Key=Value' or `Key+Type=Value' is the path.
    {Key, Val} = key_value(KeyTokens, ValueTokens, Opts),
    Msg = put_unless_private(Key, Val, #{ <<"path">> => path_key(Key) }, Opts),
    items(Items, Msg, Opts);
step({device, KeyTokens = [{scheme, _, _} | _], DeviceTokens, []}, Opts) ->
    {as,
        decode_string(maybe_subpath(DeviceTokens, Opts)),
        decode_string(text(KeyTokens))
    };
step({device, KeyTokens, DeviceTokens, Items}, Opts) ->
    {as,
        decode_string(maybe_subpath(DeviceTokens, Opts)),
        items(Items, #{ <<"path">> => path_key(text(KeyTokens)) }, Opts)
    };
step({mods, KeyTokens, Items}, Opts) ->
    items(Items, #{ <<"path">> => path_key(text(KeyTokens)) }, Opts).

%% @doc Escape the `/' characters of a key, so that it can be used as a path.
path_key(Key) -> binary:replace(Key, <<"/">>, <<"%2f">>, [global]).

%% @doc Add the inlined keys of a step to its message. A key without a value
%% is set to `true'.
items(Items, Msg, Opts) ->
    lists:foldl(
        fun(empty, Acc) -> Acc;
            ({flag, KeyTokens}, Acc) ->
                put_unless_private(
                    decode_string(text(KeyTokens)), true, Acc, Opts
                );
            ({value, KeyTokens, ValueTokens}, Acc) ->
                {Key, Val} = key_value(KeyTokens, ValueTokens, Opts),
                ?event_debug({inlined_key, {explicit, Key}, {explicit, Val}}),
                put_unless_private(Key, Val, Acc, Opts)
        end,
        Msg,
        Items
    ).

%% @doc Add a key to a message, unless the key is private.
put_unless_private(Key, Val, Msg, Opts) ->
    case hb_private:is_private(Key) of
        true -> Msg;
        false -> hb_maps:put(Key, Val, Msg, Opts)
    end.

%% @doc Parse the key and the value of a `Key=Value' pair. The key may give a
%% type for the value, as with typical keys.
key_value(KeyTokens, ValueTokens, Opts) ->
    V = hb_util:unquote(decode_string(maybe_subpath(ValueTokens, Opts))),
    {_, Key, Val} = maybe_typed(text(KeyTokens), V, Opts),
    {decode_string(Key), Val}.

%% @doc Attempt Cowboy URL decode, then sanitize the result.
decode_string(Value) when not is_binary(Value) -> Value;
decode_string(B) ->
    case catch uri_string:unquote(B) of
        DecodedBin when is_binary(DecodedBin) -> DecodedBin;
        {error, Type, _} -> throw({invalid_singleton, Type, B});
        _ -> throw({error, cannot_decode, B})
    end.

%% @doc Parse a value or a device that is a single group as a subpath. Any
%% other value or device is returned as its text.
maybe_subpath([{group, _, Tokens}], Opts) -> {resolve, subpath(Tokens, Opts)};
maybe_subpath(Tokens, _Opts) -> text(Tokens).

%% @doc Parse the tokens of a subpath into its requests. A subpath is a path,
%% and may have a query of its own. A subpath with claims gives its hashpath,
%% which `hb_hashpath' loads. The hashpath is the subpath if it starts with
%% `ao:', and `ao:' followed by the subpath if not.
subpath(Tokens, Opts) ->
    {PathTokens, QStr} = split_query(Tokens),
    Parts = parts(PathTokens),
    case {[ Part || Part = {step, _, [_ | _]} <- Parts ], text(Tokens)} of
        {[], _} -> requests(contexts(Parts, parse_query(QStr), Opts));
        {_, Hashpath = <<"ao:", _/binary>>} -> [Hashpath];
        {_, Path} -> [<<"ao:", Path/binary>>]
    end.

%% @doc Extract the characters from the binary until a separator is found.
%% The first argument of the function is an explicit separator character, or
%% a list of separator characters. Returns a tuple with the separator, the
%% accumulated characters, and the rest of the binary.
part(Sep, Bin) when not is_list(Sep) ->
    part([Sep], Bin);
part(Seps, Bin) ->
    hb_util:split_depth_string_aware_single(Seps, Bin).

%% @doc Parse a key's type (applying it to the value) and device name if present.
%% We allow ` ` characters as type indicators because some URL-string encoders
%% (e.g. Chrome) will encode `+` characters in a form that query-string parsers
%% interpret as ` ' characters.
maybe_typed(Key, Value, Opts) ->
    case part([$+, $ ], Key) of
        % A key with no name, untyped (`<<>>') or typed (`+Type').
        {_, <<>>, _} -> throw({invalid_singleton, invalid_key, Key});
        {no_match, OnlyKey, <<>>} -> {untyped, OnlyKey, Value};
        {_, OnlyKey, Type} ->
            case {Type, hb_cache:ensure_loaded(Value, Opts)} of
                % The value is a group, which is already parsed as a subpath.
                {<<"resolve">>, Subres = {resolve, _}} ->
                    {typed, OnlyKey, Subres};
                {<<"resolve">>, Subpath} ->
                    % If the value needs to be resolved before it is converted,
                    % use the `Codec/1.0' device to resolve it.
                    % For example:
                    % `/a/b&k+Int=(/x/y/z) => /a/b&k=(/x/y/z/body&Type=Int+Codec)'
                    {typed,
                        OnlyKey,
                        {resolve, from(#{ <<"path">> => Subpath }, Opts)}
                    };
                {_T, RawValue} when is_binary(RawValue) ->
                    Decoded = hb_escape:decode_quotes(RawValue),
                    {typed,
                        OnlyKey,
                        try hb_util:decode(Type, Decoded)
                        catch _:_ ->
                            throw({invalid_singleton, invalid_type, Key})
                        end
                    };
                % The key has a type, but there is no text to decode, as in
                % `?k+integer'.
                _ -> throw({invalid_singleton, invalid_type, Key})
            end
    end.

%%% Tests

%% @doc The steps of a signed request keep its signer, but a step that changes
%% a signed key is written under its own ID: the cache never writes the step's
%% content under the request's ID.
signed_request_steps_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ar_wallet:new()
    },
    Signed =
        hb_message:commit(
            #{
                <<"path">> => <<"/~meta@1.0/info/address">>,
                <<"x">> => <<"1">>
            },
            Opts
        ),
    ID = hb_message:id(Signed, all, Opts),
    Steps = [ Step || Step <- from(Signed, Opts), is_map(Step) ],
    ?assertEqual(2, length(Steps)),
    lists:foreach(
        fun(Step) ->
            ?assertEqual(
                hb_message:signers(Signed, Opts),
                hb_message:signers(Step, Opts)
            ),
            {ok, _} = hb_cache:write(Step, Opts)
        end,
        Steps
    ),
    ?assertEqual({error, not_found}, hb_cache:read(ID, Opts)).

parse_explicit_message_test() ->
    Singleton1 = #{
        <<"path">> => <<"//a">>,
        <<"a">> => <<"b">>
    },
    ?assertEqual(
        [
            #{ <<"a">> => <<"b">>},
            #{ <<"path">> => <<"a">>, <<"a">> => <<"b">> }
        ],
        from(Singleton1, #{})
    ),
    % A first path part that is a key is read as an `ao:' URI.
    ?assertEqual(
        [<<"ao:a">>, #{ <<"path">> => <<"b">>, <<"a">> => <<"b">> }],
        from(Singleton1#{ <<"path">> => <<"/a/b">> }, #{})
    ),
    DummyID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Singleton2 = #{
        <<"path">> => <<"/", DummyID/binary, "/a">>
    },
    ?assertEqual(
        [<<"ao:", DummyID/binary>>, #{ <<"path">> => <<"a">> }],
        from(Singleton2, #{})
    ),
    Singleton3 = #{
        <<"path">> => <<"/", DummyID/binary, "/a">>,
        <<"a">> => <<"b">>
    },
    ?assertEqual(
        [
            <<"ao:", DummyID/binary>>,
            #{ <<"path">> => <<"a">>, <<"a">> => <<"b">> }
        ],
        from(Singleton3, #{})
    ),
    % The other keys of the singleton are given to its requests, not to an
    % `as' upon the `ao:' URI of a key.
    ?assertEqual(
        [
            [
                {as, <<"d@1.0">>, <<"ao:", DummyID/binary>>},
                #{ <<"path">> => <<"a">>, <<"a">> => <<"b">> }
            ],
            [
                {as, <<"d@1.0">>, <<"ao:k">>},
                #{ <<"path">> => <<"a">>, <<"a">> => <<"b">> }
            ]
        ],
        [
            from(Singleton3#{ <<"path">> => Path }, #{})
        ||
            Path <- [<<"/", DummyID/binary, "~d@1.0/a">>, <<"/k~d@1.0/a">>]
        ]
    ),
    % After the base, a key of any length takes the keys of the request.
    ?assertEqual(
        [
            {as, <<"location@1.0">>, #{ <<"variant">> => <<"ao.N.1">> }},
            #{ <<"path">> => DummyID, <<"variant">> => <<"ao.N.1">> }
        ],
        from(<<"/~location@1.0/", DummyID/binary, "?variant=ao.N.1">>, #{})
    ),
    % A key that starts with a URI scheme is the URI, percent-decoded. Its `&',
    % `~' and `=' are percent-encoded: unencoded, they are those of its part.
    ?assertEqual(
        [<<"ni:///sha-256;x&k=v~d@1.0">>, #{ <<"path">> => <<"a">> }],
        from(<<"/ni:%2f%2f%2fsha-256;x%26k%3dv%7ed@1.0/a">>, #{})
    ),
    ?assertEqual(
        [{as, <<"d@1.0">>, <<"ni:///sha-256;x">>}, #{ <<"path">> => <<"a">> }],
        from(<<"/ni:%2f%2f%2fsha-256;x~d@1.0/a">>, #{})
    ),
    % A URI in host form is a URI too.
    ?assertEqual(
        [<<"uri-did-ar-x">>, #{ <<"path">> => <<"a">> }],
        from(<<"/uri-did-ar-x/a">>, #{})
    ).

%% @doc Device fields are data while parsing, including on device-only steps.
parse_device_fields_without_resolution_test() ->
    Fields = #{ <<"device">> => <<"lua@5.3a">>, <<"value">> => <<"kept">> },
    Opts = #{ <<"store">> => hb_test_utils:test_store(hb_store_lmdb) },
    lists:foreach(
        fun(Message) ->
            ?assertEqual(
                [
                    {as, <<"meta@1.0">>, Fields},
                    Fields#{ <<"path">> => <<"info">> }
                ],
                from(Message, Opts)
            )
        end,
        [
            Fields#{ <<"path">> => <<"/~meta@1.0/info">> },
            <<"/~meta@1.0/info?device=lua@5.3a&value=kept">>
        ]
    ),
    ?assertEqual(
        [{as, <<"meta@1.0">>, Fields}, #{ <<"path">> => <<"info">> }],
        from(<<"/~meta@1.0&device=lua@5.3a&value=kept/info">>, Opts)
    ).

%%% `to/1' function tests
to_suite_test_() ->
    [
        fun simple_to_test/0,
        fun multiple_messages_to_test/0,
        fun basic_hashpath_to_test/0,
        fun scoped_key_to_test/0,
        fun typed_key_to_test/0,
        fun subpath_in_key_to_test/0,
        fun subpath_in_path_to_test/0,
        fun inlined_keys_to_test/0,
        fun multiple_inlined_keys_to_test/0,
        fun subpath_in_inlined_to_test/0
    ].

simple_to_test() ->
    Messages = [
        #{<<"test-key">> => <<"test-value">>},
        #{<<"path">> => <<"a">>, <<"test-key">> => <<"test-value">>}
    ],
    Expected = #{<<"path">> => <<"//a">>, <<"test-key">> => <<"test-value">>},
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})),
    % A request that contains only the value of its key is written `key=value'.
    KeyValue = [#{}, #{ <<"path">> => <<"k">>, <<"k">> => <<"v">> }],
    ?assertEqual(#{ <<"path">> => <<"//k=v">> }, to(KeyValue)),
    ?assertEqual(KeyValue, from(to(KeyValue), #{})),
    % A message that is only a device is written `~device'.
    Device = [{as, <<"meta@1.0">>, #{}}, #{ <<"path">> => <<"info">> }],
    ?assertEqual(#{ <<"path">> => <<"/~meta@1.0/info">> }, to(Device)),
    ?assertEqual(Device, from(to(Device), #{})),
    % A key in the first path part is its `ao:' URI, with or without a device.
    Key = [<<"ao:a">>, #{ <<"path">> => <<"b">> }],
    ?assertEqual(#{ <<"path">> => <<"/a/b">> }, to(Key)),
    ?assertEqual(Key, from(to(Key), #{})),
    KeyDevice = [{as, <<"d@1.0">>, <<"ao:a">>}, #{ <<"path">> => <<"b">> }],
    ?assertEqual(#{ <<"path">> => <<"/a~d@1.0/b">> }, to(KeyDevice)),
    ?assertEqual(KeyDevice, from(to(KeyDevice), #{})),
    % A hashpath that is not a key is a group, as the base or as a request.
    ?assertEqual(
        #{ <<"path">> => <<"/(ao:a/b)/(ao:c~d@1.0/e)">> },
        to([<<"ao:a/b">>, <<"ao:c~d@1.0/e">>])
    ).

multiple_messages_to_test() ->
    Messages =
        [
            #{<<"test-key">> => <<"test-value">>},
            #{<<"path">> => <<"a">>, <<"test-key">> => <<"test-value">>},
            #{<<"path">> => <<"b">>, <<"test-key">> => <<"test-value">>},
            #{<<"path">> => <<"c">>, <<"test-key">> => <<"test-value">>}
        ],
    Expected = #{
        <<"path">> => <<"//a/b/c">>,
        <<"test-key">> => <<"test-value">>
    },
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).

basic_hashpath_to_test() ->
    Messages = [
        <<"ao:e5ohB7TgMYRoc0BLllkmAqkqLy1SrliEkOPJlNPXBQ8">>,
        #{<<"method">> => <<"GET">>, <<"path">> => <<"some-other">>}
    ],
    Expected = #{
        <<"path">> => <<"/e5ohB7TgMYRoc0BLllkmAqkqLy1SrliEkOPJlNPXBQ8/some-other">>,
        <<"method">> => <<"GET">>
    },
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).

scoped_key_to_test() ->
    Messages = [
        #{},
        #{<<"path">> => <<"a">>},
        #{<<"path">> => <<"b">>, <<"test-key">> => <<"test-value">>},
        #{<<"path">> => <<"c">>}
    ],
    Expected = #{<<"2.test-key">> => <<"test-value">>, <<"path">> => <<"//a/b/c">>},
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).

typed_key_to_test() ->
    Messages =
        [
            #{},
            #{<<"path">> => <<"a">>},
            #{<<"path">> => <<"b">>, <<"test-key">> => 123},
            #{<<"path">> => <<"c">>}
        ],
    Expected = #{<<"2.test-key+integer">> => <<"123">>, <<"path">> => <<"//a/b/c">>},
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).

subpath_in_key_to_test() ->
    Messages = [
        #{},
        #{<<"path">> => <<"a">>},
        #{
            <<"path">> => <<"b">>,
            <<"test-key">> =>
                {resolve,
                    [
                        #{},
                        #{<<"path">> => <<"x">>},
                        #{<<"path">> => <<"y">>},
                        #{<<"path">> => <<"z">>}
                    ]
                }
        },
        #{<<"path">> => <<"c">>}
    ],
    Expected = #{<<"2.test-key+resolve">> => <<"//x/y/z">>, <<"path">> => <<"//a/b/c">>},
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).

subpath_in_path_to_test() ->
    Messages = [
        #{},
        #{<<"path">> => <<"a">>},
        {resolve,
            [
                #{},
                #{<<"path">> => <<"x">>},
                #{<<"path">> => <<"y">>},
                #{<<"path">> => <<"z">>}
            ]
        },
        #{<<"path">> => <<"z">>}
    ],
    Expected = #{
        <<"path">> => <<"//a/(//x/y/z)/z">>
    },
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).

inlined_keys_to_test() ->
    Messages =
        [
            #{<<"method">> => <<"POST">>},
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"a">>
            },
            #{
                <<"k1">> => <<"v1">>,
                <<"method">> => <<"POST">>,
                <<"path">> => <<"b">>
            },
            #{
                <<"k2">> => <<"v2">>,
                <<"method">> => <<"POST">>,
                <<"path">> => <<"c">>
            }
        ],
    % NOTE: The implementation above does not convert the given list of messages
    % into the original format, however it assures that the `to/1' and `from/1'
    % operations are idempotent.
    ?assertEqual(Messages, from(to(Messages), #{})).

multiple_inlined_keys_to_test() ->
    Messages = [
        #{<<"method">> => <<"POST">>},
            #{<<"method">> => <<"POST">>, <<"path">> => <<"a">>},
            #{
                <<"k1">> => <<"v1">>,
                <<"k2">> => <<"v2">>,
                <<"method">> => <<"POST">>,
                <<"path">> => <<"b">>
            }
        ],
    % NOTE: The implementation above does not convert the given list of messages
    % into the original format, however it assures that the `to/1' and `from/1'
    % operations are idempotent.
    ?assertEqual(Messages, from(to(Messages), #{})).

subpath_in_inlined_to_test() ->
    Messages = [
        #{},
        #{<<"path">> => <<"part1">>},
        #{<<"b">> =>
                {resolve,
                    [#{},
                     #{<<"path">> => <<"x">>},
                     #{<<"path">> => <<"y">>}]},
                    <<"path">> => <<"part2">>,
                    <<"test">> => <<"1">>},
        #{<<"path">> => <<"part3">>}],
    % NOTE: The implementation above does not convert the given list of messages
    % into the original format, however it assures that the `to/1' and `from/1'
    % operations are idempotent.
    ?assertEqual(Messages, from(to(Messages), #{})).

%%% `from/1' function tests
single_message_test() ->
    % This is a singleton TABM message
    Req = #{
        <<"path">> => <<"//a">>,
        <<"test-key">> => <<"test-value">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(2, length(Msgs)),
    ?assert(is_map(hd(Msgs))),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, hd(Msgs))).

basic_hashpath_test() ->
    Hashpath = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Path = <<"/", Hashpath/binary, "/some-other">>,
    Req = #{
        <<"path">> => Path,
        <<"method">> => <<"GET">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(2, length(Msgs)),
    [Base, Msg2] = Msgs,
    ?assertEqual(Base, <<"ao:", Hashpath/binary>>),
    ?assertEqual(<<"GET">>, hb_maps:get(<<"method">>, Msg2)),
    ?assertEqual(<<"some-other">>, hb_maps:get(<<"path">>, Msg2)).

multiple_messages_test() ->
    Req = #{
        <<"path">> => <<"//a/b/c">>,
        <<"test-key">> => <<"test-value">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_Base, Base, Msg2, Res] = Msgs,
    ?assert(lists:all(fun is_map/1, Msgs)),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, Base)),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, Msg2)),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, Res)).

%%% Advanced key syntax tests

scoped_key_test() ->
    Req = #{
        <<"path">> => <<"//a/b/c">>,
        <<"2.test-key">> => <<"test-value">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Base, not_found)),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, Msg2, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Res, not_found)).

typed_key_test() ->
    Req = #{
        <<"path">> => <<"//a/b/c">>,
        <<"2.test-key+integer">> => <<"123">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Base, not_found)),
    ?assertEqual(123, hb_maps:get(<<"test-key">>, Msg2, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Res, not_found)).

%% @doc A link under an untyped key is not loaded, even when the node does
%% not have its message. A link under a typed key is loaded to apply the type.
linked_key_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    {ok, ID} = hb_cache:write(<<"123">>, Opts),
    {ok, PathID} = hb_cache:write(<<"//a">>, Opts),
    Missing =
        {link,
            <<"ao:", (hb_util:human_id(crypto:strong_rand_bytes(32)))/binary>>,
            #{}
        },
    Nested = #{ <<"a">> => <<"b">> },
    Req = #{
        <<"path">> => {link, PathID, #{}},
        <<"test-key">> => Missing,
        <<"typed-key+integer">> => {link, ID, #{}},
        <<"nested">> => Nested
    },
    [_, Msg] = from(Req, Opts),
    ?assertEqual(Missing, maps:get(<<"test-key">>, Msg)),
    ?assertEqual(123, maps:get(<<"typed-key">>, Msg)),
    % A nested message stays in its step: the parse writes nothing.
    ?assertEqual(
        {error, not_found},
        hb_cache:read(hb_message:id(Nested, all, Opts), Opts)
    ).

subpath_in_key_test() ->
    Req = #{
        <<"path">> => <<"//a/b/c">>,
        <<"2.test-key+resolve">> => <<"//x/y/z">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Base, not_found)),
    ?assertEqual(
        {resolve,
            [
                #{},
                #{ <<"path">> => <<"x">> },
                #{ <<"path">> => <<"y">> },
                #{ <<"path">> => <<"z">> }
            ]
        },
        hb_maps:get(<<"test-key">>, Msg2, not_found)
    ),
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Res, not_found)).

%%% Advanced path syntax tests

subpath_in_path_test() ->
    Req = #{
        <<"path">> => <<"//a/(//x/y/z)/z">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?assertEqual(<<"a">>, hb_maps:get(<<"path">>, Base)),
    ?assertEqual(
        {resolve,
            [
                #{},
                #{ <<"path">> => <<"x">> },
                #{ <<"path">> => <<"y">> },
                #{ <<"path">> => <<"z">> }
            ]
        },
        Msg2
    ),
    ?assertEqual(<<"z">>, hb_maps:get(<<"path">>, Res)).

inlined_keys_test() ->
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"//a/b&k1=v1/c&k2=v2">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?assertEqual(<<"v1">>, hb_maps:get(<<"k1">>, Msg2)),
    ?assertEqual(<<"v2">>, hb_maps:get(<<"k2">>, Res)),
    ?assertEqual(not_found, hb_maps:get(<<"k1">>, Base, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"k2">>, Msg2, not_found)).

inlined_keys_long_segment_test() ->
    Req = #{
        <<"path">> =>
            <<"//chunk&offset=377813969707255&length=262144">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(2, length(Msgs)),
    [Base, Msg] = Msgs,
    ?assertEqual(<<"chunk">>, hb_maps:get(<<"path">>, Msg)),
    ?assertEqual(<<"377813969707255">>, hb_maps:get(<<"offset">>, Msg)),
    ?assertEqual(<<"262144">>, hb_maps:get(<<"length">>, Msg)),
    ?assertEqual(not_found, hb_maps:get(<<"offset">>, Base, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"length">>, Base, not_found)).

inlined_quoted_key_test() ->
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"//a/b&k1=\"v/1\"/c&k2=v2">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?assertEqual(<<"v/1">>, hb_maps:get(<<"k1">>, Msg2)),
    ?assertEqual(<<"v2">>, hb_maps:get(<<"k2">>, Res)),
    ?assertEqual(not_found, hb_maps:get(<<"k1">>, Base, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"k2">>, Msg2, not_found)),
    ReqB = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/~profile@1.0/eval=%22~meta@1.0%2finfo%22">>
    },
    MsgsB = from(ReqB, #{}),
    [_, Msg2b] = MsgsB,
    ?assertEqual(<<"~meta@1.0/info">>, hb_maps:get(<<"eval">>, Msg2b)),
    % A value that starts with a quote, but does not end with one, keeps it.
    ?assertMatch(
        [_, #{ <<"k">> := <<"\"v">> }],
        from(#{ <<"path">> => <<"//a&k=%22v">> }, #{})
    ).

inlined_assumed_key_test() ->
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"//a/b=4/c&k2=v2">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?event_debug({parsed, Msgs}),
    ?assertEqual(<<"4">>, hb_maps:get(<<"b">>, Msg2)),
    ?assertEqual(not_found, hb_maps:get(<<"b">>, Base, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"b">>, Res, not_found)),
    ReqB = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"//a/b+integer=4/c&k2=v2">>
    },
    MsgsB = from(ReqB, #{}),
    [_, Msg1b, Msg2b, Msg3b] = MsgsB,
    ?event_debug({parsed, MsgsB}),
    ?assertEqual(4, hb_maps:get(<<"b">>, Msg2b)),
    ?assertEqual(not_found, hb_maps:get(<<"b">>, Msg1b, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"b">>, Msg3b, not_found)).


multiple_inlined_keys_test() ->
    Path = <<"//a/b&k1=v1&k2=v2">>,
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => Path
    },
    Msgs = from(Req, #{}),
    ?assertEqual(3, length(Msgs)),
    [_, Base, Msg2] = Msgs,
    ?assertEqual(not_found, hb_maps:get(<<"k1">>, Base, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"k2">>, Base, not_found)),
    ?assertEqual(<<"v1">>, hb_maps:get(<<"k1">>, Msg2, not_found)),
    ?assertEqual(<<"v2">>, hb_maps:get(<<"k2">>, Msg2, not_found)).

subpath_in_inlined_test() ->
    Path = <<"//part1/part2&test=1&b=(//x/y)/part3">>,
    Req = #{
        <<"path">> => Path
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, First, Second, Third] = Msgs,
    ?assertEqual(<<"part1">>, hb_maps:get(<<"path">>, First)),
    ?assertEqual(<<"part3">>, hb_maps:get(<<"path">>, Third)),
    ?assertEqual(
        {resolve, [#{}, #{ <<"path">> => <<"x">> }, #{ <<"path">> => <<"y">> }] },
        hb_maps:get(<<"b">>, Second)
    ),
    % A group given with the `+resolve' type is read as a single subpath.
    ?assertEqual(
        Msgs,
        from(
            #{ <<"path">> => <<"//part1/part2&test=1&b+resolve=(//x/y)/part3">> },
            #{}
        )
    ).

path_parts_test() ->
    % A `/' in a group does not split the path.
    ?assertMatch(
        [
            #{},
            #{ <<"path">> := <<"a">> },
            #{ <<"path">> := <<"b">>, <<"c">> := {resolve, [_, _, _]} },
            #{ <<"path">> := <<"f">> }
        ],
        from(<<"//a/b&c=(//d/e)/f">>, #{})
    ),
    ?assertMatch([#{}, #{ <<"path">> := <<"a">> }], from(<<"//a">>, #{})),
    ?assertMatch(
        [#{}, #{ <<"path">> := <<"a">> }, #{ <<"path">> := <<"b">> }, _],
        from(<<"//a/b/c">>, #{})
    ),
    ?assertMatch(
        [<<"ao:IYkkrqlZNW_J-4T-5eFApZOMRl5P4VjvrcOXWvIqB1Q">>, #{}],
        from(<<"/IYkkrqlZNW_J-4T-5eFApZOMRl5P4VjvrcOXWvIqB1Q/req">>, #{})
    ),
    ?assertMatch(
        [
            #{},
            #{ <<"path">> := <<"a">> },
            #{ <<"path">> := <<"b">>, <<"k1">> := <<"V1">> },
            #{ <<"path">> := <<"c">>, <<"k2">> := <<"V2">> }
        ],
        from(<<"//a/b&K1=V1/c&K2=V2">>, #{})
    ),
    ?assertMatch(
        [#{}, #{ <<"path">> := <<"a">> }, {resolve, [_, _, _, _]}, _],
        from(<<"//a/(//x/y/z)/c">>, #{})
    ),
    % A path that does not follow the grammar is refused with the position of
    % the error, counted in bytes, also after a character of two bytes.
    ?assertThrow(
        {invalid_singleton, invalid_path, <<"character 6: ", _/binary>>},
        from(<<"/", 16#C3, 16#A9, "/c)">>, #{})
    ),
    % The end of the path is expected only where the path can end.
    ?assertThrow(
        {invalid_singleton,
            invalid_path,
            <<
                "character 6: expected text, '(', '=', '~'; "
                "found the end of the path."
            >>
        },
        from(<<"/a/b~">>, #{})
    ),
    ok.

parse_part_space_edge_case_test() ->
    ?assertMatch(
        [
            {as, <<"manifest@1.0">>, _},
            #{ <<"path">> := <<"[object Object]">> }
        ],
        from(
            <<
                "/42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA~manifest@1.0"
                "/[object Object]"
            >>,
            #{}
        )
    ).
