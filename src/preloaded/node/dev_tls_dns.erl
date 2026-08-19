%%% @doc DNS-01 challenge providers for the node TLS lifecycle.
-module(dev_tls_dns).
-export([challenge/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(DIGITALOCEAN_API_URL, <<"https://api.digitalocean.com/v2">>).
-define(CLOUDFLARE_API_URL, <<"https://api.cloudflare.com/client/v4">>).
-define(REQUEST_TIMEOUT, 30000).
-define(RESPONSE_LIMIT, 1024 * 1024).

%% @doc Execute a DNS-01 challenge action using the configured provider.
challenge(Action, TLS, Opts) ->
    ACME = hb_maps:get(<<"acme">>, TLS, #{}, Opts),
    Provider =
        case hb_maps:get(<<"dns-provider">>, ACME, undefined, Opts) of
            undefined -> undefined;
            ProviderValue -> hb_util:to_lower(hb_util:bin(ProviderValue))
        end,
    ?event(tls, {dns_challenge, {provider, Provider},
        {action, action_name(Action)}}),
    case {Action, Provider} of
        {{dns_wait, Record, Value, MaxWait}, _} ->
            wait_dns(Record, Value, MaxWait, ACME, Opts);
        {_, <<"digitalocean">>} -> digitalocean(Action, ACME, Opts);
        {_, <<"cloudflare">>} -> cloudflare(Action, ACME, Opts);
        {_, undefined} -> {error, 'acme-dns-provider-missing'};
        _ -> {error, {'unsupported-acme-dns-provider', Provider}}
    end.

%% @doc Return the event name for a DNS challenge action.
action_name({dns_put, _, _}) -> put;
action_name({dns_delete, _}) -> delete;
action_name({dns_wait, _, _, _}) -> wait;
action_name(_) -> invalid.

%% @doc Wait until every authoritative nameserver for the configured zone can
%% serve the exact TXT value. Recursive resolvers are used only to discover the
%% authorities and their addresses; TXT queries go directly to those servers.
wait_dns(Record, Value, MaxWait, ACME, Opts) ->
    Zone = hb_util:to_lower(hb_maps:get(<<"dns-zone">>, ACME, <<>>, Opts)),
    ValidRecord = valid_name(Zone) andalso record_in_zone(Record, Zone),
    maybe
        {true, true, {ok, ConfiguredTimeout, Interval}} ?=
            {ValidRecord, is_binary(Value), propagation_config(ACME, Opts)},
        Timeout = min(ConfiguredTimeout, MaxWait),
        PollInterval = min(Interval, Timeout),
        {ok, Servers} ?= authoritative_servers(Zone, Opts),
        ?event(tls, {dns_propagation_started,
            {nameserver_count, length(Servers)},
            {timeout_ms, Timeout}}),
        Deadline = erlang:monotonic_time(millisecond) + Timeout,
        poll_dns(Record, Value, Servers, Deadline, PollInterval)
    else
        {false, _, _} -> {error, 'invalid-acme-dns-record'};
        {_, false, _} -> {error, 'invalid-acme-dns-value'};
        {_, _, {error, _} = Error} -> Error;
        {error, _} = Error -> Error
    end.

%% @doc Return whether `Record' belongs to the configured DNS `Zone'.
record_in_zone(Record, Zone) ->
    try _ = relative_name(Record, Zone), true
    catch _:_ -> false
    end.

%% @doc Validate and return DNS propagation polling configuration.
propagation_config(ACME, Opts) ->
    Legacy = hb_maps:get(<<"dns-propagation-delay">>, ACME, 30000, Opts),
    Timeout = hb_maps:get(<<"dns-propagation-timeout">>, ACME, Legacy, Opts),
    Interval = hb_maps:get(<<"dns-poll-interval">>, ACME, 2000, Opts),
    case is_integer(Timeout) andalso Timeout > 0
            andalso is_integer(Interval) andalso Interval > 0
            andalso Interval =< Timeout of
        true -> {ok, Timeout, Interval};
        false -> {error, 'invalid-dns-propagation-options'}
    end.

%% @doc Discover the authoritative nameservers and public IPs for `Zone'.
authoritative_servers(Zone, Opts) ->
    try
        validate_authorities(
            hb_hostname:records(Zone, ns, Opts),
            fun(Name) -> hb_hostname:public_ips(Name, Opts) end
        )
    catch _:_ -> {error, 'acme-dns-authorities-not-found'}
    end.

%% @doc Normalize and validate discovered authoritative nameservers.
validate_authorities(Names, Resolve) ->
    Normalized = lists:usort([hb_hostname:normalize(Name) || Name <- Names]),
    Servers = [{Name, Resolve(Name)} || Name <- Normalized],
    case Normalized =/= [] andalso lists:all(fun({_Name, IPs}) ->
            IPs =/= []
        end, Servers) of
        true -> {ok, Servers};
        false -> {error, 'acme-dns-authorities-not-found'}
    end.

%% @doc Poll authoritative nameservers until they serve the expected TXT value.
poll_dns(Record, Value, Servers, Deadline, Interval) ->
    Query = fun(IP) -> authoritative_txt(IP, Record, Value, Interval) end,
    poll_dns(Servers, Deadline, Interval, Query).

%% @doc Poll DNS using the supplied nameserver query function.
poll_dns(Servers, Deadline, Interval, Query) ->
    Visible = lists:all(fun({_Name, IPs}) ->
        lists:any(Query, IPs)
    end, Servers),
    case Visible of
        true ->
            ?event(tls, {dns_record_propagated,
                {nameserver_count, length(Servers)}}),
            ok;
        false -> poll_dns_retry(Servers, Deadline, Interval, Query)
    end.

%% @doc Retry an unpropagated DNS query until its deadline expires.
poll_dns_retry(Servers, Deadline, Interval, Query) ->
    Remaining = Deadline - erlang:monotonic_time(millisecond),
    case Remaining > 0 of
        true ->
            timer:sleep(min(Interval, Remaining)),
            poll_dns(Servers, Deadline, Interval, Query);
        false ->
            ?event(tls, {dns_propagation_timeout,
                {nameserver_count, length(Servers)}}),
            {error, 'acme-dns-propagation-timeout'}
    end.

%% @doc Return whether an authoritative server exposes the expected TXT value.
authoritative_txt(IP, Record, Value, Interval) ->
    Answers = dns_lookup(Record, txt, [
        {nameservers, [{IP, 53}]},
        {timeout, min(1000, Interval)},
        {retry, 1}
    ]),
    lists:any(fun(Answer) -> txt_value(Answer) =:= Value end, Answers).

%% @doc Resolve DNS records, returning an empty list on resolver failure.
dns_lookup(Name, Type, ResolverOpts) ->
    try inet_res:lookup(hb_util:list(Name), in, Type, ResolverOpts)
    catch _:_ -> []
    end.

%% @doc Normalize a DNS TXT answer to a binary value.
txt_value(Answer) when is_binary(Answer) -> Answer;
txt_value(Answer) when is_list(Answer) ->
    try iolist_to_binary(Answer) catch _:_ -> <<>> end;
txt_value(_) -> <<>>.

%% @doc Create or delete a DigitalOcean DNS-01 TXT record.
digitalocean({dns_put, Record, Value}, ACME, Opts) ->
    with_config(
        ACME, Opts, ?DIGITALOCEAN_API_URL,
        fun(Zone, Token, APIURL) ->
            {Path, Method, Body} = digitalocean_request(
                {dns_put, Record, Value}, Zone
            ),
            maybe
                {ok, _} = Result ?= digitalocean_result(Zone,
                    request(APIURL, Path, Method, Token, Body, Opts)),
                ?event(tls, {dns_record_created, {provider, digitalocean}}),
                Result
            else
                {error, _} = Error -> Error
            end
        end
    );
digitalocean({dns_delete, {digitalocean, Zone, ID}}, ACME, Opts) ->
    with_config(
        ACME, Opts, ?DIGITALOCEAN_API_URL,
        fun(ConfiguredZone, Token, APIURL) ->
            maybe
                true ?= Zone =:= ConfiguredZone,
                {Path, Method, Body} = digitalocean_request(
                    {dns_delete, {digitalocean, Zone, ID}}, Zone
                ),
                ok ?= digitalocean_delete_result(
                    request(APIURL, Path, Method, Token, Body, Opts)
                ),
                ?event(tls, {dns_record_deleted, {provider, digitalocean}}),
                ok
            else
                false -> {error, 'invalid-digitalocean-dns-handle'};
                {error, _} = Error -> Error
            end
        end
    );
digitalocean(_, _ACME, _Opts) ->
    {error, 'invalid-digitalocean-dns-action'}.

%% @doc Build a DigitalOcean DNS record request.
digitalocean_request({dns_put, Record, Value}, Zone) ->
    {
        <<"/domains/", Zone/binary, "/records">>,
        <<"POST">>,
        hb_json:encode(#{
            <<"type">> => <<"TXT">>,
            <<"name">> => relative_name(Record, Zone),
            <<"data">> => Value,
            <<"ttl">> => 30
        })
    };
digitalocean_request({dns_delete, {digitalocean, Zone, ID}}, Zone) ->
    {
        <<"/domains/", Zone/binary, "/records/",
            (integer_to_binary(ID))/binary>>,
        <<"DELETE">>,
        <<>>
    }.

%% @doc Validate a DigitalOcean DNS record creation response.
digitalocean_result(Zone, {ok, 201, _, Body}) ->
    try
        #{<<"domain_record">> := #{<<"id">> := ID}} = hb_json:decode(Body),
        true = is_integer(ID),
        {ok, {digitalocean, Zone, ID}}
    catch _:_ -> {error, 'invalid-digitalocean-dns-response'}
    end;
digitalocean_result(_Zone, Response) ->
    provider_error(Response).

%% @doc Validate a DigitalOcean DNS record deletion response.
digitalocean_delete_result({ok, 204, _, _}) -> ok;
digitalocean_delete_result(Response) -> provider_error(Response).

%% @doc Create or delete a Cloudflare DNS-01 TXT record.
cloudflare({dns_put, Record, Value}, ACME, Opts) ->
    with_config(
        ACME, Opts, ?CLOUDFLARE_API_URL,
        fun(Zone, Token, APIURL) ->
            _ = relative_name(Record, Zone),
            maybe
                {ok, ZoneID} ?=
                    cloudflare_zone_id(ACME, Opts, Zone, Token, APIURL),
                {Path, Method, Body} = cloudflare_request(
                    {dns_put, Record, Value}, ZoneID
                ),
                {ok, #{<<"id">> := ID}} ?= cloudflare_result(request(
                    APIURL, Path, Method, Token, Body, Opts
                )),
                true ?= valid_id(ID),
                ?event(tls, {dns_record_created, {provider, cloudflare}}),
                {ok, {cloudflare, Zone, ZoneID, ID}}
            else
                {ok, _} -> {error, 'invalid-cloudflare-dns-response'};
                false -> {error, 'invalid-cloudflare-dns-response'};
                {error, _} = Error -> Error
            end
        end
    );
cloudflare({dns_delete, {cloudflare, Zone, ZoneID, ID}}, ACME, Opts) ->
    with_config(
        ACME, Opts, ?CLOUDFLARE_API_URL,
        fun(ConfiguredZone, Token, APIURL) ->
            maybe
                true ?= Zone =:= ConfiguredZone andalso valid_id(ZoneID)
                    andalso valid_id(ID),
                {Path, Method, Body} = cloudflare_request(
                    {dns_delete, {cloudflare, Zone, ZoneID, ID}}, ZoneID
                ),
                {ok, _} ?= cloudflare_result(request(
                    APIURL, Path, Method, Token, Body, Opts
                )),
                ?event(tls, {dns_record_deleted, {provider, cloudflare}}),
                ok
            else
                false -> {error, 'invalid-cloudflare-dns-handle'};
                {error, _} = Error -> Error
            end
        end
    );
cloudflare(_, _ACME, _Opts) ->
    {error, 'invalid-cloudflare-dns-action'}.

%% @doc Build a Cloudflare DNS record request.
cloudflare_request({dns_put, Record, Value}, ZoneID) ->
    {
        <<"/zones/", ZoneID/binary, "/dns_records">>,
        <<"POST">>,
        hb_json:encode(#{
            <<"type">> => <<"TXT">>,
            <<"name">> => Record,
            <<"content">> => Value,
            <<"ttl">> => 60
        })
    };
cloudflare_request({dns_delete, {cloudflare, _, ZoneID, ID}}, ZoneID) ->
    {
        <<"/zones/", ZoneID/binary, "/dns_records/", ID/binary>>,
        <<"DELETE">>,
        <<>>
    }.

%% @doc Resolve and validate the Cloudflare zone identifier.
cloudflare_zone_id(ACME, Opts, Zone, Token, APIURL) ->
    case hb_maps:get(<<"dns-zone-id">>, ACME, undefined, Opts) of
        ZoneID when is_binary(ZoneID) ->
            maybe
                true ?= valid_id(ZoneID),
                {ok, ZoneID}
            else
                false -> {error, 'invalid-cloudflare-zone-id'}
            end;
        undefined ->
            Path = <<"/zones?name=", Zone/binary,
                "&status=active&per_page=1">>,
            maybe
                {ok, [#{<<"id">> := ZoneID}]} ?= cloudflare_result(request(
                    APIURL, Path, <<"GET">>, Token, <<>>, Opts
                )),
                true ?= valid_id(ZoneID),
                {ok, ZoneID}
            else
                {ok, _} -> {error, 'cloudflare-zone-not-found'};
                false -> {error, 'invalid-cloudflare-zone-response'};
                {error, _} = Error -> Error
            end;
        _ -> {error, 'invalid-cloudflare-zone-id'}
    end.

%% @doc Normalize a Cloudflare API response.
cloudflare_result({ok, 200, _, Body}) ->
    try
        case hb_json:decode(Body) of
            #{<<"success">> := true, <<"result">> := Result} -> {ok, Result};
            Response -> {error, {'cloudflare-dns-error', Response}}
        end
    catch _:_ -> {error, 'invalid-cloudflare-dns-response'}
    end;
cloudflare_result({ok, Status, _, Body}) ->
    {error, {'cloudflare-dns-status', Status, problem(Body)}};
cloudflare_result({error, Reason}) -> {error, Reason}.

%% @doc Validate provider configuration before invoking `Fun'.
with_config(ACME, Opts, DefaultAPIURL, Fun) ->
    Zone = hb_util:to_lower(hb_maps:get(<<"dns-zone">>, ACME, <<>>, Opts)),
    Token = token(private_token(ACME, Opts)),
    APIURL = hb_maps:get(<<"dns-api-url">>, ACME, DefaultAPIURL, Opts),
    case {valid_name(Zone), Token, valid_api_url(APIURL)} of
        {true, {ok, Value}, true} -> Fun(Zone, Value, APIURL);
        {false, _, _} -> {error, 'invalid-acme-dns-zone'};
        {_, {error, Reason}, _} -> {error, Reason};
        {_, _, false} -> {error, 'invalid-acme-dns-api-url'}
    end.

%% @doc Read the private DNS provider API token.
private_token(ACME, Opts) ->
    hb_maps:get(<<"priv-dns-api-token">>, ACME, undefined, Opts).

%% @doc Validate a DNS provider API token.
token(undefined) -> {error, 'acme-dns-api-token-missing'};
token(Token) when is_binary(Token), byte_size(Token) > 0 -> {ok, Token};
token(_) -> {error, 'invalid-acme-dns-api-token'}.

%% @doc Return a DNS record name relative to `Zone'.
relative_name(Zone, Zone) -> <<"@">>;
relative_name(Record, Zone) ->
    Suffix = <<".", Zone/binary>>,
    Size = byte_size(Record) - byte_size(Suffix),
    case Size > 0 andalso binary:part(Record, Size, byte_size(Suffix))
            =:= Suffix of
        true -> binary:part(Record, 0, Size);
        false -> throw({acme, 'acme-dns-record-outside-zone'})
    end.

%% @doc Return whether a DNS name is non-empty and syntactically valid.
valid_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    re:run(Name, <<"^[a-z0-9.-]+$">>, [{capture, none}]) =:= match;
valid_name(_) -> false.

%% @doc Return whether a provider identifier has the expected hexadecimal form.
valid_id(ID) when is_binary(ID) ->
    re:run(ID, <<"^[A-Fa-f0-9]{32}$">>, [{capture, none}]) =:= match;
valid_id(_) -> false.

%% @doc Return whether a provider API URL is a safe HTTPS base URL.
valid_api_url(URL) when is_binary(URL) ->
    try
        URI = uri_string:parse(URL),
        hb_util:to_lower(hb_util:bin(maps:get(scheme, URI, <<>>))) =:= <<"https">>
            andalso maps:is_key(host, URI)
            andalso not maps:is_key(userinfo, URI)
            andalso not maps:is_key(query, URI)
            andalso not maps:is_key(fragment, URI)
    catch _:_ -> false
    end;
valid_api_url(_) -> false.

%% @doc Send an authenticated request to a DNS provider API.
request(APIURL, Path, Method, Token, Body, Opts) ->
    URL = <<APIURL/binary, Path/binary>>,
    URI = uri_string:parse(URL),
    Peer = uri_string:recompose(
        (maps:without([path, query], URI))#{path => <<>>}
    ),
    RequestPath = case maps:find(query, URI) of
        {ok, Query} -> <<(hb_util:bin(maps:get(path, URI)))/binary,
            "?", (hb_util:bin(Query))/binary>>;
        error -> hb_util:bin(maps:get(path, URI))
    end,
    hb_http_client:request(#{
        peer => Peer,
        path => RequestPath,
        method => Method,
        headers => #{
            <<"authorization">> => <<"Bearer ", Token/binary>>,
            <<"content-type">> => <<"application/json">>,
            <<"user-agent">> => <<"HyperBEAM ACME">>
        },
        body => Body,
        limit => ?RESPONSE_LIMIT
    }, http_options(Opts)).

%% @doc Build bounded HTTP options for DNS provider API requests.
http_options(Opts) ->
    Opts#{
        <<"protocol">> => http1,
        <<"http-retry">> => 0,
        <<"http-client-connect-timeout">> => ?REQUEST_TIMEOUT,
        <<"http-client-send-timeout">> => ?REQUEST_TIMEOUT,
        <<"http-client-tls-ca">> => public_key:cacerts_get()
    }.

%% @doc Normalize a DigitalOcean API error response.
provider_error({ok, Status, _, Body}) ->
    {error, {'digitalocean-dns-status', Status, problem(Body)}};
provider_error({error, Reason}) -> {error, Reason}.

%% @doc Decode a provider error body when it contains JSON.
problem(Body) ->
    try hb_json:decode(Body) catch _:_ -> Body end.

%%% Tests

%% @doc Test conversion of fully qualified records to zone-relative names.
relative_name_test() ->
    ?assertEqual(<<"_acme-challenge.hb">>, relative_name(
        <<"_acme-challenge.hb.ajprincipe.pt">>, <<"ajprincipe.pt">>
    )),
    ?assertThrow({acme, 'acme-dns-record-outside-zone'}, relative_name(
        <<"_acme-challenge.other.example">>, <<"ajprincipe.pt">>
    )).

%% @doc Test DNS provider configuration validation.
provider_validation_test() ->
    ?assertEqual({error, 'acme-dns-provider-missing'},
        challenge({dns_put, <<"a">>, <<"b">>}, #{<<"acme">> => #{}}, #{})),
    ?assertEqual(false, valid_api_url(<<"http://api.digitalocean.com/v2">>)),
    ?assertEqual(true, valid_id(<<"023e105f4ecef8ad9ca31a8372d0c353">>)),
    ?assertEqual(false, valid_id(<<"not-a-zone-id">>)).

%% @doc Test private DNS provider token lookup and validation.
private_token_test() ->
    ?assertEqual(<<"binary-token">>, private_token(
        #{<<"priv-dns-api-token">> => <<"binary-token">>}, #{}
    )),
    ?assertEqual(undefined, private_token(#{}, #{})),
    ?assertEqual(undefined, private_token(
        #{<<"dns-api-token">> => <<"not-private">>}, #{}
    )),
    ?assertEqual({error, 'invalid-acme-dns-api-token'}, token(<<>>)).

%% @doc Test normalization and validation of Cloudflare API responses.
cloudflare_response_test() ->
    ZoneID = <<"023e105f4ecef8ad9ca31a8372d0c353">>,
    ?assertEqual({ok, ZoneID}, cloudflare_zone_id(
        #{<<"dns-zone-id">> => ZoneID}, #{}, <<"example.com">>,
        <<"unused">>, <<"https://unused.example">>
    )),
    ?assertEqual({ok, #{<<"id">> => ZoneID}}, cloudflare_result(
        {ok, 200, [], hb_json:encode(#{
            <<"success">> => true,
            <<"result">> => #{<<"id">> => ZoneID}
        })}
    )),
    ?assertMatch({error, {'cloudflare-dns-error', _}}, cloudflare_result(
        {ok, 200, [], hb_json:encode(#{
            <<"success">> => false,
            <<"errors">> => [#{<<"message">> => <<"denied">>}]
        })}
    )),
    ?assertEqual(
        {error, 'invalid-cloudflare-dns-response'},
        cloudflare_result({ok, 200, [], <<"not-json">>})
    ),
    ?assertMatch(
        {error, {'cloudflare-dns-status', 403, _}},
        cloudflare_result({ok, 403, [], <<"denied">>})
    ).

%% @doc Test exact provider request paths, methods, and DNS record bodies.
provider_request_test() ->
    Zone = <<"example.com">>,
    Record = <<"_acme-challenge.example.com">>,
    Value = <<"digest">>,
    {DOCreatePath, <<"POST">>, DOCreateBody} = digitalocean_request(
        {dns_put, Record, Value}, Zone
    ),
    ?assertEqual(<<"/domains/example.com/records">>, DOCreatePath),
    ?assertEqual(#{
        <<"type">> => <<"TXT">>,
        <<"name">> => <<"_acme-challenge">>,
        <<"data">> => Value,
        <<"ttl">> => 30
    }, hb_json:decode(DOCreateBody)),
    ?assertEqual(
        {<<"/domains/example.com/records/42">>, <<"DELETE">>, <<>>},
        digitalocean_request(
            {dns_delete, {digitalocean, Zone, 42}}, Zone
        )
    ),
    ZoneID = <<"023e105f4ecef8ad9ca31a8372d0c353">>,
    RecordID = <<"372e67954025e0ba6aaa6d586b9e0b59">>,
    {CFCreatePath, <<"POST">>, CFCreateBody} = cloudflare_request(
        {dns_put, Record, Value}, ZoneID
    ),
    ?assertEqual(
        <<"/zones/023e105f4ecef8ad9ca31a8372d0c353/dns_records">>,
        CFCreatePath
    ),
    ?assertEqual(#{
        <<"type">> => <<"TXT">>,
        <<"name">> => Record,
        <<"content">> => Value,
        <<"ttl">> => 60
    }, hb_json:decode(CFCreateBody)),
    ?assertEqual(
        {<<"/zones/023e105f4ecef8ad9ca31a8372d0c353/dns_records/",
            RecordID/binary>>, <<"DELETE">>, <<>>},
        cloudflare_request(
            {dns_delete, {cloudflare, Zone, ZoneID, RecordID}}, ZoneID
        )
    ).

%% @doc Test DigitalOcean record creation response validation.
digitalocean_response_test() ->
    Zone = <<"example.com">>,
    ?assertEqual(
        {ok, {digitalocean, Zone, 42}},
        digitalocean_result(Zone, {ok, 201, [], hb_json:encode(#{
            <<"domain_record">> => #{<<"id">> => 42}
        })})
    ),
    ?assertEqual(
        {error, 'invalid-digitalocean-dns-response'},
        digitalocean_result(Zone, {ok, 201, [], hb_json:encode(#{
            <<"domain_record">> => #{<<"id">> => <<"invalid">>}
        })})
    ),
    ?assertMatch(
        {error, {'digitalocean-dns-status', 403, _}},
        digitalocean_result(Zone, {ok, 403, [], <<"denied">>})
    ),
    ?assertEqual(ok, digitalocean_delete_result({ok, 204, [], <<>>})),
    ?assertMatch(
        {error, {'digitalocean-dns-status', 200, _}},
        digitalocean_delete_result({ok, 200, [], <<"unexpected">>})
    ).

%% @doc Test authoritative nameserver normalization and completeness checks.
authoritative_servers_test() ->
    Resolve = fun
        (<<"ns1.example">>) -> [{1, 1, 1, 1}];
        (<<"ns2.example">>) -> [{8, 8, 8, 8}];
        (_) -> []
    end,
    ?assertEqual(
        {ok, [
            {<<"ns1.example">>, [{1, 1, 1, 1}]},
            {<<"ns2.example">>, [{8, 8, 8, 8}]}
        ]},
        validate_authorities(
            [<<"NS2.EXAMPLE.">>, <<"ns1.example">>, <<"ns1.example">>],
            Resolve
        )
    ),
    ?assertEqual(
        {error, 'acme-dns-authorities-not-found'},
        validate_authorities([], Resolve)
    ),
    ?assertEqual(
        {error, 'acme-dns-authorities-not-found'},
        validate_authorities([<<"missing.example">>], Resolve)
    ).

%% @doc Test preserving a configured HTTP client in provider request options.
configured_http_client_test() ->
    HTTPOpts = http_options(#{ <<"http-client">> => httpc }),
    ?assertEqual(httpc, hb_opts:get(http_client, undefined, HTTPOpts)).

%% @doc Test DNS propagation timeout and interval configuration.
propagation_config_test() ->
    ?assertEqual({ok, 30000, 2000}, propagation_config(#{}, #{})),
    ?assertEqual({ok, 45000, 1000}, propagation_config(#{
        <<"dns-propagation-delay">> => 45000,
        <<"dns-poll-interval">> => 1000
    }, #{})),
    ?assertEqual({ok, 60000, 1000}, propagation_config(#{
        <<"dns-propagation-delay">> => 45000,
        <<"dns-propagation-timeout">> => 60000,
        <<"dns-poll-interval">> => 1000
    }, #{})),
    ?assertEqual({error, 'invalid-dns-propagation-options'},
        propagation_config(#{<<"dns-poll-interval">> => 0}, #{})).

%% @doc Test polling all authoritative nameservers for DNS propagation.
authoritative_poll_test() ->
    Servers = [
        {<<"ns1.example">>, [first]},
        {<<"ns2.example">>, [second]}
    ],
    ?assertEqual(ok, poll_dns(
        Servers, erlang:monotonic_time(millisecond) + 1000, 1,
        fun(_IP) -> true end
    )),
    ?assertEqual({error, 'acme-dns-propagation-timeout'}, poll_dns(
        Servers, erlang:monotonic_time(millisecond) - 1, 1,
        fun(_IP) -> false end
    )),
    ?assertEqual(<<"expected">>, txt_value(["ex", "pected"])),
    ?assert(record_in_zone(
        <<"_acme-challenge.example.com">>, <<"example.com">>
    )).
