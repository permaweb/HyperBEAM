%%% @doc DNS questions and replies as AO-Core messages.
%%%
%%% `~dns@1.0` connects DNS to AO-Core. It represents DNS questions and replies as
%%% messages and delegates resolution to the `on/dns-resolve` hook. The same message
%%% interface serves direct AO-Core calls and queries received over DNS transports.
%%%
%%% The device does not prescribe where records come from or how they are resolved.
%%% Authoritative data, forwarding, recursion and access policy belong to the
%%% resolver. Messages and their fields may be linked; lists and numbered messages
%%% represent the same ordered sequence.
%%%
%%% ## `start`
%%%
%%% Start a DNS listener using the node's `dns` options:
%%%
%%% | Option | Meaning |
%%% | --- | --- |
%%% | `dns/port` | Required integer from 0 to 65535. `0` selects an available port. |
%%% | `dns/address` | Optional IPv4 or IPv6 bind address. Defaults to `0.0.0.0`. |
%%%
%%% The listener accepts UDP and TCP on the same port. `start` returns an error if
%%% the port is absent or invalid, the address is invalid, a listener is already
%%% running, or the requested address and port cannot be bound. On success, it
%%% returns the incoming message with `dns-port` set to the bound port, preserving
%%% the request body.
%%%
%%% Configuration alone does not start a listener. To start one at boot, add the
%%% device to `on/start`, retaining any other startup handlers:
%%%
%%% ```json
%%% {
%%%   "dns": {
%%%     "port": 53,
%%%     "address": "0.0.0.0"
%%%   },
%%%   "on": {
%%%     "start": { "device": "dns@1.0" }
%%%   }
%%% }
%%% ```
%%%
%%% ## `resolve`
%%%
%%% Resolve a DNS question through `on/dns-resolve`. This key can be called without
%%% starting a listener. Without a configured resolver, the result is
%%% `{error, not_authorized}` (`REFUSED`).
%%%
%%% The question supplies `name`, `type` and `class`. A network query also carries
%%% its ID, recursion preference and transport metadata. For example, the hook
%%% receives:
%%%
%%% ```json
%%% {
%%%   "path": "dns-resolve",
%%%   "id": 42,
%%%   "name": "www.example.com",
%%%   "type": "a",
%%%   "class": "in",
%%%   "recursion-desired": true,
%%%   "transport": "udp",
%%%   "peer": { "address": "192.0.2.1", "port": 49152 }
%%% }
%%% ```
%%%
%%% Names use DNS presentation syntax, retaining the query's case, without a trailing
%%% dot except for the root (`.`). Name comparisons are case-insensitive. Type and
%%% class names are lower-case strings, such as `a` and `in`; numeric codes allow
%%% types and classes without a named form.
%%%
%%% `transport` is `udp` or `tcp`. EDNS queries include an `edns` message with
%%% `version` and `udp-payload-size`. Network metadata is optional for direct
%%% AO-Core calls. Node options and private state are not part of the question.
%%%
%%% ### Resolver results
%%%
%%% The hook returns an AO-Core result. The device interprets it as follows:
%%%
%%% | Result | DNS reply |
%%% | --- | --- |
%%% | `{ok, Reply}` | A non-numbered message supplies the complete reply. |
%%% | `{ok, Answers}` | A list or numbered message supplies `answers`, with `rcode: 0` and `authoritative: true`. |
%%% | `{error, not_found}` | `rcode: 3` (`NXDOMAIN`): the queried name does not exist. |
%%% | `{error, not_authorized}` | `rcode: 5` (`REFUSED`): the resolver declines the query. |
%%% | `{error, not_implemented}` | `rcode: 4` (`NOTIMP`): the requested operation is not supported. |
%%% | `{error, Details}` | A reply message with `rcode` set to 1 (`FORMERR`). |
%%% | `{failure, Details}` | A reply message with `rcode` set to 2 (`SERVFAIL`). |
%%%
%%% For the last two forms, `Details` supplies reply fields; the result category
%%% determines `rcode`. A complete reply can specify any applicable DNS response
%%% code without requiring another shorthand. These codes have the meanings defined
%%% in [DNS response headers](https://www.rfc-editor.org/rfc/rfc1035.html#section-4.1.1).
%%%
%%% The complete reply is returned directly, not inside a `body` field. For example,
%%% `{ok, Reply}` may contain:
%%%
%%% ```json
%%% {
%%%   "rcode": 0,
%%%   "authoritative": true,
%%%   "answers": [
%%%     {
%%%       "name": "www.example.com",
%%%       "type": "a",
%%%       "class": "in",
%%%       "ttl": 60,
%%%       "data": "192.0.2.80"
%%%     }
%%%   ]
%%% }
%%% ```
%%%
%%% Returning just the `answers` sequence above as `{ok, Answers}` is equivalent.
%%%
%%% ### Complete replies
%%%
%%% | Field | Default | Meaning |
%%% | --- | --- | --- |
%%% | `rcode` | `0` | DNS response code. Extended codes require EDNS. |
%%% | `authoritative` | `false` | Whether this response is authoritative for the queried name. |
%%% | `recursion-available` | `false` | Whether the server offers recursion. |
%%% | `answers` | Empty list | Answer records. |
%%% | `authority` | Empty list | Authority records. |
%%% | `additional` | Empty list | Related records. |
%%%
%%% The answer-list shorthand overrides `authoritative` to `true`; other omitted
%%% fields use these defaults. Only DNS reply fields are encoded into the response.
%%%
%%% `authoritative: false` is not a redirect: forwarded or cached answers can also
%%% be non-authoritative. A referral uses NS records in `authority`, with address
%%% records in `additional` where needed.
%%%
%%% `NXDOMAIN` means the name does not exist. An existing name with no records of
%%% the requested type instead has `rcode: 0` with no matching answers (NODATA).
%%% Neither an empty answer list nor an unfamiliar record type implies `NXDOMAIN`.
%%% Authoritative negative replies must include the zone's SOA in `authority`, as
%%% specified by [DNS negative caching](https://www.rfc-editor.org/rfc/rfc2308.html#section-3).
%%% Use a complete reply for authoritative negative responses; the shorthand does
%%% not supply the required SOA.
%%%
%%% ## Resource records
%%%
%%% Each record requires `name`, `type` and `data`. `class` defaults to `in`; `ttl`
%%% defaults to 0 and is measured in seconds. Record sections accept lists or
%%% numbered messages, including linked records and fields.
%%%
%%% | Record type | `data` |
%%% | --- | --- |
%%% | `a`, `aaaa` | IP address string |
%%% | `ns`, `cname`, `ptr` | Domain name string |
%%% | `txt`, `spf` | List of strings, each at most 255 bytes |
%%% | `mx` | `preference`, `exchange` |
%%% | `srv` | `priority`, `weight`, `port`, `target` |
%%% | `soa` | `mname`, `rname`, `serial`, `refresh`, `retry`, `expire`, `minimum` |
%%% | `caa` | `flags`, `tag`, `value` |
%%% | `uri` | `priority`, `weight`, `target` |
%%% | `naptr` | `order`, `preference`, `flags`, `services`, `regexp`, `replacement` |
%%% | `hinfo`, `minfo` | `cpu`, `os`; or `rmailbx`, `emailbx` |
%%% | Numeric type code | Binary containing opaque, uncompressed RDATA |
%%%
%%% Compound data uses a message with the named fields. Multiple strings in one TXT
%%% record are parts of that record; multiple TXT values require separate records.
%%% The numeric-type form preserves opaque RDATA for records without a structured
%%% representation, following [unknown record handling](https://www.rfc-editor.org/rfc/rfc3597.html).
%%%
%%% ## DNS transport contract
%%%
%%% The device converts questions and replies between DNS packets and AO-Core
%%% messages. It preserves the query's ID, question and recursion-desired flag in
%%% the response. The resolver supplies record content and response semantics, not
%%% packet framing or compression.
%%%
%%% UDP replies respect DNS and EDNS payload limits. Replies that cannot fit are
%%% truncated with `TC` set, allowing the client to retry over TCP. The device
%%% manages EDNS OPT records separately from the resolver's `additional` records.
%%%
%%% Malformed queries produce `FORMERR` when a response can be formed. Unsupported
%%% operations produce `NOTIMP`. Resolver failures and invalid replies produce
%%% `SERVFAIL`; a failed query must not stop the listener.
-module(dev_dns).
-export([device_info/1, start/3, resolve/3]).
-include("include/hb.hrl").

-type link() :: {link, binary(), map()}.
%% Numeric codes precede names because unions coerce in declaration order.
-type dns_symbol() :: 0..65535 | binary().
-type question() :: #{
    name := binary(), type := dns_symbol(), class := dns_symbol(),
    id => 0..65535, 'recursion-desired' => boolean(), transport => binary(),
    peer => #{ address := binary(), port := 0..65535, _ => _ },
    edns => #{ version := 0..255, 'udp-payload-size' := 0..65535, _ => _ },
    _ => _
}.
-type answer() :: #{
    name := binary(), type := dns_symbol(), class => dns_symbol(),
    ttl => 0..16#7fffffff, data := _, _ => _
}.
%% Answer sequences may be lists, numbered messages, or links to either.
-type answers() :: [answer() | link()] | #{ _ => _ } | link().
-type reply() :: #{
    rcode => 0..4095, authoritative => boolean(),
    'recursion-available' => boolean(),
    answers => answers(), authority => answers(), additional => answers(),
    _ => _
}.
-type result() ::
    {ok, reply() | answers()}
    | {error, not_found | not_authorized | not_implemented | reply() | link()}
    | {failure, reply() | link() | binary()}.

%% @doc Export the listener and resolution keys.
-spec device_info(#{ _ => _ }) -> #{ exports := [binary()] }.
device_info(_) -> #{ exports => [<<"start">>, <<"resolve">>] }.

%% @doc Start the configured listener, preserving the `on/start' message body.
-spec start(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ 'dns-port' := 0..65535, 'cache-control' := binary(), _ => _ }}
    | {error, #{ status := 400 | 409, body := binary(), _ => _ }}.
start(_Base, Req, Opts) ->
    DNS = hb_maps:get(<<"dns">>, Opts, #{}, Opts),
    case hb_maps:get(<<"port">>, DNS, undefined, Opts) of
        undefined -> error_response(400, <<"dns/port is required.">>);
        Port when is_integer(Port), Port >= 0, Port =< 65535 ->
            Address = hb_maps:get(<<"address">>, DNS, <<"0.0.0.0">>, Opts),
            case inet:parse_strict_address(hb_util:list(Address)) of
                {ok, IP} ->
                    case dev_dns_server:start(
                        IP, Port, fun(Packet, Peer) -> packet(Packet, Peer, Opts) end
                    ) of
                        {ok, BoundPort} ->
                            {ok, hb_maps:merge(
                                hb_message:uncommitted(Req, Opts),
                                #{
                                    <<"dns-port">> => BoundPort,
                                    <<"cache-control">> => <<"no-store">>
                                },
                                Opts
                            )};
                        {error, Reason} ->
                            error_response(409, hb_util:bin(Reason))
                    end;
                {error, _} -> error_response(400, <<"Invalid dns/address.">>)
            end;
        _ -> error_response(400, <<"Invalid dns/port.">>)
    end.

%% @doc Resolve a question through the node hook, preserving its AO-Core result.
-spec resolve(#{ _ => _ }, question(), #{ _ => _ }) -> result().
resolve(_Base, Req, Opts) ->
    case hb_hook:find(<<"dns-resolve">>, Opts) of
        [] -> {error, not_authorized};
        _ -> hb_hook:on(
            <<"dns-resolve">>, hb_message:uncommitted(Req, Opts), Opts
        )
    end.

%% @doc Decode a DNS query, resolve it via AO-Core, and encode its response.
packet(Packet, Peer, Opts) ->
    case catch inet_dns:decode(Packet, false) of
        {ok, Query} ->
            Header = inet_dns:msg(Query, header),
            case inet_dns:header(Header, qr) of
                true -> ignore;
                false -> answer(Query, Peer, Opts)
            end;
        _ ->
            case Packet of
                <<ID:16, 0:1, _/bitstring>> ->
                    inet_dns:encode(inet_dns:make_msg([{header,
                        inet_dns:make_header([{id, ID}, {qr, true}, {rcode, 1}])
                    }]), false);
                _ -> ignore
            end
    end.

%% @doc Keep malformed hook results and resolver failures local to one query.
answer(Query, Peer, Opts) ->
    try
        encode_response(Query, query(Query, Peer, Opts), Peer, Opts)
    catch
        Class:Reason ->
            ?event(dns, {resolution_failed, Class, Reason}),
            encode_response(Query, #{ <<"rcode">> => 2 }, Peer, Opts)
    end.

%% @doc Convert a single ordinary question into the hook's request message.
query(Query, Peer, Opts) ->
    Header = inet_dns:msg(Query, header),
    case {inet_dns:header(Header, opcode), inet_dns:msg(Query, qdlist)} of
        {query, [Question]} ->
            case edns(Query) of
                {_, Version} when Version =/= 0 -> #{ <<"rcode">> => 16 };
                _ ->
                    Request = Peer#{
                        <<"path">> => <<"resolve">>,
                        <<"id">> => inet_dns:header(Header, id),
                        <<"name">> => list_to_binary(
                            inet_dns:dns_query(Question, domain)
                        ),
                        <<"type">> => symbol(inet_dns:dns_query(Question, type)),
                        <<"class">> => symbol(inet_dns:dns_query(Question, class)),
                        <<"recursion-desired">> => inet_dns:header(Header, rd)
                    },
                    Result = hb_ao:resolve(
                        #{ <<"device">> => <<"dns@1.0">> },
                        case edns(Query) of
                            none -> Request;
                            {Size, Version} -> Request#{ <<"edns">> => #{
                                <<"udp-payload-size">> => Size,
                                <<"version">> => Version
                            }}
                        end,
                        Opts#{
                            <<"hashpath">> => ignore,
                            <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
                        }
                    ),
                    response(Result, Opts)
            end;
        {query, _} -> #{ <<"rcode">> => 1 };
        _ -> #{ <<"rcode">> => 4 }
    end.

%% @doc Interpret AO-Core results at the DNS boundary. Answer sequences are
%% authoritative; complete replies retain their own flags and record sections.
response({ok, Result}, Opts) ->
    Reply = hb_cache:ensure_loaded(Result, Opts),
    case hb_util:is_ordered_list(Reply, Opts) of
        true -> #{
            <<"rcode">> => 0, <<"authoritative">> => true,
            <<"answers">> => Reply
        };
        false -> Reply
    end;
response({error, not_found}, _Opts) -> #{ <<"rcode">> => 3 };
response({error, not_authorized}, _Opts) -> #{ <<"rcode">> => 5 };
response({error, not_implemented}, _Opts) -> #{ <<"rcode">> => 4 };
response({Status, Details}, Opts) when Status =:= error; Status =:= failure ->
    hb_maps:put(
        <<"rcode">>,
        case Status of error -> 1; failure -> 2 end,
        hb_message:uncommitted(Details, Opts),
        Opts
    ).

%% @doc Encode records while preserving the query's ID, question and RD flag.
encode_response(Query, Reply, Peer, Opts) ->
    RCode = hb_maps:get(<<"rcode">>, Reply, 0, Opts),
    true = is_integer(RCode) andalso RCode >= 0 andalso RCode =< 4095,
    true = RCode =< 15 orelse edns(Query) =/= none,
    Header = inet_dns:make_header(inet_dns:msg(Query, header), [
        {qr, true}, {rcode, RCode band 15},
        {aa, hb_maps:get(<<"authoritative">>, Reply, false, Opts)},
        {ra, hb_maps:get(<<"recursion-available">>, Reply, false, Opts)},
        {tc, false}, {pr, false}
    ]),
    Extra = case edns(Query) of
        none -> [];
        _ -> [inet_dns:make_rr([
            {type, opt}, {udp_payload_size, 1232}, {ext_rcode, RCode bsr 4}
        ])]
    end,
    Response = inet_dns:make_msg([
        {header, Header}, {qdlist, case inet_dns:msg(Query, qdlist) of
            [Question] -> [Question];
            _ -> []
        end},
        {anlist, records(<<"answers">>, Reply, Opts)},
        {nslist, records(<<"authority">>, Reply, Opts)},
        {arlist, records(<<"additional">>, Reply, Opts) ++ Extra}
    ]),
    Encoded = inet_dns:encode(Response, false),
    Limit = case maps:get(<<"transport">>, Peer) of
        <<"tcp">> -> 65535;
        <<"udp">> -> case edns(Query) of
            none -> 512;
            {Size, _} -> min(1232, max(512, Size))
        end
    end,
    case byte_size(Encoded) =< Limit of
        true -> Encoded;
        false -> inet_dns:encode(inet_dns:make_msg(Response, [
            {header, inet_dns:make_header(Header, tc, true)},
            {anlist, []}, {nslist, []}, {arlist, Extra}
        ]), false)
    end.

%% @doc Read the EDNS payload limit and version, if the query advertises them.
edns(Query) ->
    case [RR || RR <- inet_dns:msg(Query, arlist),
            inet_dns:record_type(RR) =:= rr,
            inet_dns:rr(RR, type) =:= opt] of
        [] -> none;
        [RR] -> {
            inet_dns:rr(RR, udp_payload_size), inet_dns:rr(RR, version)
        }
    end.

%% @doc Turn an AO-Core record list into OTP DNS resource records.
records(Key, Reply, Opts) ->
    [record(RR, Opts) || RR <- hb_util:message_to_ordered_list(
        hb_maps:get(Key, Reply, [], Opts), Opts
    )].

record(RR, Opts) ->
    Type = native_symbol(hb_maps:get(<<"type">>, RR, undefined, Opts)),
    TTL = hb_maps:get(<<"ttl">>, RR, 0, Opts),
    true = is_integer(TTL) andalso TTL >= 0 andalso TTL =< 16#7fffffff,
    inet_dns:make_rr([
        {domain, hb_util:list(hb_maps:get(<<"name">>, RR, undefined, Opts))},
        {type, Type},
        {class, native_symbol(hb_maps:get(<<"class">>, RR, <<"in">>, Opts))},
        {ttl, TTL},
        {data, record_data(Type, hb_maps:get(<<"data">>, RR, undefined, Opts), Opts)}
    ]).

%% @doc Keep numeric RR types available for arbitrary, opaque RDATA. Standard
%% compound records use named fields; TXT records preserve string boundaries.
record_data(Type, Data, _Opts) when Type =:= a; Type =:= aaaa ->
    {ok, IP} = inet:parse_strict_address(hb_util:list(Data)),
    IP;
record_data(Type, Data, Opts) when Type =:= txt; Type =:= spf ->
    [hb_cache:ensure_loaded(String, Opts)
        || String <- hb_util:message_to_ordered_list(Data, Opts)];
record_data(Type, Data, _Opts) when is_integer(Type) -> Data;
record_data(Type, Data, Opts) ->
    case data_fields(Type) of
        [] -> hb_util:list(Data);
        Fields -> list_to_tuple([
            case hb_maps:get(Key, Data, undefined, Opts) of
                Value when Key =:= <<"regexp">> ->
                    unicode:characters_to_list(Value);
                Value when is_binary(Value) -> binary_to_list(Value);
                Value -> Value
            end
        || Key <- Fields])
    end.

%% @doc Field order required by OTP for compound DNS resource data.
data_fields(soa) -> [<<"mname">>, <<"rname">>, <<"serial">>, <<"refresh">>,
    <<"retry">>, <<"expire">>, <<"minimum">>];
data_fields(mx) -> [<<"preference">>, <<"exchange">>];
data_fields(srv) -> [<<"priority">>, <<"weight">>, <<"port">>, <<"target">>];
data_fields(caa) -> [<<"flags">>, <<"tag">>, <<"value">>];
data_fields(uri) -> [<<"priority">>, <<"weight">>, <<"target">>];
data_fields(naptr) -> [<<"order">>, <<"preference">>, <<"flags">>,
    <<"services">>, <<"regexp">>, <<"replacement">>];
data_fields(hinfo) -> [<<"cpu">>, <<"os">>];
data_fields(minfo) -> [<<"rmailbx">>, <<"emailbx">>];
data_fields(_) -> [].

%% @doc DNS symbols are binaries in messages; unknown codes remain integers.
symbol(Value) when is_atom(Value) -> atom_to_binary(Value);
symbol(Value) -> Value.

native_symbol(Value) when is_binary(Value) ->
    binary_to_existing_atom(hb_util:to_lower(Value));
native_symbol(Value) when is_integer(Value), Value >= 0, Value =< 65535 -> Value.

error_response(Status, Reason) ->
    {error, #{
        <<"status">> => Status, <<"body">> => Reason,
        <<"cache-control">> => <<"no-store">>
    }}.
