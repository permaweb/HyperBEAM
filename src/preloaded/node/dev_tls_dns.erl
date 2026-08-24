%%% @doc DNS-01 challenge orchestration for the node TLS lifecycle.
-module(dev_tls_dns).
-device_libraries([lib_tls_dns_provider]).
-export([challenge/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Execute a DNS-01 challenge action using the configured provider.
challenge(Action, TLS, Opts) ->
    ACME = hb_maps:get(<<"acme">>, TLS, #{}, Opts),
    Provider = hb_maps:get(<<"dns-provider">>, ACME, undefined, Opts),
    ?event(tls, {dns_challenge, {provider, Provider},
        {action, action_name(Action)}}),
    case Action of
        {dns_wait, Record, Value, MaxWait} ->
            wait_dns(Record, Value, MaxWait, ACME, Opts);
        _ when Provider =:= undefined ->
            {error, 'acme-dns-provider-missing'};
        _ when is_binary(Provider) ->
            invoke_provider(Action, Provider, ACME, Opts);
        _ ->
            {error, 'invalid-acme-dns-provider'}
    end.

%% @doc Invoke a configured DNS provider through its AO-Core device API.
invoke_provider(Action, Provider, ACME, Opts) ->
    try
        Base = ACME#{ <<"device">> => Provider },
        provider_result(Action, hb_ao:raw(Base, provider_request(Action), Opts))
    catch
        throw:{acme, Reason} -> {error, Reason};
        throw:{error, {device_not_loadable, _, Reason}} ->
            {error, {'acme-dns-provider-not-loadable', Provider, Reason}};
        Class:Reason ->
            {error, {'acme-dns-provider-failed', Provider, Class, Reason}}
    end.

%% @doc Convert an internal DNS action into a provider device request.
provider_request({dns_put, Record, Value}) ->
    #{
        <<"path">> => <<"put">>,
        <<"record">> => Record,
        <<"value">> => Value
    };
provider_request({dns_delete, Handle}) ->
    #{ <<"path">> => <<"delete">>, <<"handle">> => Handle };
provider_request(_) ->
    throw({acme, 'invalid-acme-dns-action'}).

%% @doc Normalize a provider device response for the ACME client.
provider_result({dns_put, _, _}, {ok, Handle}) when is_map(Handle) ->
    {ok, Handle};
provider_result({dns_delete, _}, {ok, ok}) -> ok;
provider_result(_, {error, _} = Error) -> Error;
provider_result(_, _) -> {error, 'invalid-acme-dns-provider-response'}.

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
    try _ = lib_tls_dns_provider:relative_name(Record, Zone), true
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

%% @doc Return whether a DNS name is non-empty and syntactically valid.
valid_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    re:run(Name, <<"^[a-z0-9.-]+$">>, [{capture, none}]) =:= match;
valid_name(_) -> false.

%%% Tests

%% @doc Test DNS provider configuration validation.
provider_validation_test() ->
    ?assertEqual({error, 'acme-dns-provider-missing'},
        challenge({dns_put, <<"a">>, <<"b">>}, #{<<"acme">> => #{}}, #{})),
    ?assertEqual({error, 'invalid-acme-dns-provider'}, challenge(
        {dns_put, <<"a">>, <<"b">>},
        #{<<"acme">> => #{<<"dns-provider">> => 42}}, #{}
    )).

%% @doc Test the provider request and response interface.
provider_interface_test() ->
    Handle = #{<<"record-id">> => 42},
    ?assertEqual(#{
        <<"path">> => <<"put">>,
        <<"record">> => <<"record">>,
        <<"value">> => <<"value">>
    }, provider_request({dns_put, <<"record">>, <<"value">>})),
    ?assertEqual(
        #{<<"path">> => <<"delete">>, <<"handle">> => Handle},
        provider_request({dns_delete, Handle})
    ),
    ?assertEqual(
        {ok, Handle},
        provider_result({dns_put, <<"record">>, <<"value">>}, {ok, Handle})
    ),
    ?assertEqual(ok, provider_result({dns_delete, Handle}, {ok, ok})),
    ?assertEqual(
        {error, 'invalid-acme-dns-provider-response'},
        provider_result({dns_put, <<"record">>, <<"value">>}, {ok, invalid})
    ).

%% @doc Test dispatch to a separately packaged provider device.
provider_dispatch_test() ->
    TLS = #{<<"acme">> => #{
        <<"dns-provider">> => <<"tls-dns-cloudflare@1.0">>,
        <<"dns-zone">> => <<"example.com">>,
        <<"priv-dns-api-token">> => <<"unused">>
    }},
    ?assertEqual(
        {error, 'invalid-cloudflare-dns-handle'},
        challenge({dns_delete, #{}}, TLS, #{})
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
