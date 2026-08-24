%%% @doc Cloudflare implementation of the TLS DNS provider interface.
-module(dev_tls_dns_cloudflare).
-implements(<<"tls-dns-cloudflare@1.0">>).
-device_libraries([lib_tls_dns_provider]).
-export([info/1, put/3, delete/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(API_URL, <<"https://api.cloudflare.com/client/v4">>).

%% @doc Describe the DNS provider interface exported by this device.
info(_Opts) -> #{exports => [put, delete]}.

%% @doc Create an ACME DNS-01 TXT record and return its cleanup handle.
put(Base, Req, Opts) ->
    Record = hb_maps:get(<<"record">>, Req, undefined, Opts),
    Value = hb_maps:get(<<"value">>, Req, undefined, Opts),
    lib_tls_dns_provider:with_config(
        Base, Opts, ?API_URL,
        fun(Zone, Token, APIURL) ->
            maybe
                {true, true} ?= {is_binary(Record), is_binary(Value)},
                _ = lib_tls_dns_provider:relative_name(Record, Zone),
                {ok, ZoneID} ?= zone_id(Base, Opts, Zone, Token, APIURL),
                {Path, Method, Body} = provider_request(Record, Value, ZoneID),
                {ok, #{<<"id">> := ID}} ?= provider_result(
                    lib_tls_dns_provider:request(
                        APIURL, Path, Method, Token, Body, Opts
                    )
                ),
                true ?= valid_id(ID),
                ?event(tls, {dns_record_created, {provider, cloudflare}}),
                {ok, #{
                    <<"provider">> => <<"cloudflare">>,
                    <<"zone">> => Zone,
                    <<"zone-id">> => ZoneID,
                    <<"record-id">> => ID
                }}
            else
                {false, _} -> {error, 'invalid-acme-dns-record'};
                {_, false} -> {error, 'invalid-acme-dns-value'};
                {ok, _} -> {error, 'invalid-cloudflare-dns-response'};
                false -> {error, 'invalid-cloudflare-dns-response'};
                {error, _} = Error -> Error
            end
        end
    ).

%% @doc Delete an ACME DNS-01 TXT record from a cleanup handle.
delete(Base, Req, Opts) ->
    Handle = hb_maps:get(<<"handle">>, Req, #{}, Opts),
    lib_tls_dns_provider:with_config(
        Base, Opts, ?API_URL,
        fun(ConfiguredZone, Token, APIURL) ->
            Zone = hb_maps:get(<<"zone">>, Handle, undefined, Opts),
            ZoneID = hb_maps:get(<<"zone-id">>, Handle, undefined, Opts),
            ID = hb_maps:get(<<"record-id">>, Handle, undefined, Opts),
            Provider = hb_maps:get(<<"provider">>, Handle, undefined, Opts),
            maybe
                true ?= Provider =:= <<"cloudflare">>
                    andalso Zone =:= ConfiguredZone andalso valid_id(ZoneID)
                    andalso valid_id(ID),
                Path = <<"/zones/", ZoneID/binary, "/dns_records/", ID/binary>>,
                {ok, _} ?= provider_result(lib_tls_dns_provider:request(
                    APIURL, Path, <<"DELETE">>, Token, <<>>, Opts
                )),
                ?event(tls, {dns_record_deleted, {provider, cloudflare}}),
                {ok, ok}
            else
                false -> {error, 'invalid-cloudflare-dns-handle'};
                {error, _} = Error -> Error
            end
        end
    ).

%% @doc Build a Cloudflare DNS record creation request.
provider_request(Record, Value, ZoneID) ->
    {
        <<"/zones/", ZoneID/binary, "/dns_records">>,
        <<"POST">>,
        hb_json:encode(#{
            <<"type">> => <<"TXT">>,
            <<"name">> => Record,
            <<"content">> => Value,
            <<"ttl">> => 60
        })
    }.

%% @doc Resolve and validate the Cloudflare zone identifier.
zone_id(Base, Opts, Zone, Token, APIURL) ->
    case hb_maps:get(<<"dns-zone-id">>, Base, undefined, Opts) of
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
                {ok, [#{<<"id">> := ZoneID}]} ?= provider_result(
                    lib_tls_dns_provider:request(
                        APIURL, Path, <<"GET">>, Token, <<>>, Opts
                    )
                ),
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
provider_result({ok, 200, _, Body}) ->
    try
        case hb_json:decode(Body) of
            #{<<"success">> := true, <<"result">> := Result} -> {ok, Result};
            Response -> {error, {'cloudflare-dns-error', Response}}
        end
    catch _:_ -> {error, 'invalid-cloudflare-dns-response'}
    end;
provider_result({ok, Status, _, Body}) ->
    {error, {'cloudflare-dns-status', Status,
        lib_tls_dns_provider:problem(Body)}};
provider_result({error, Reason}) -> {error, Reason}.

%% @doc Return whether an API identifier has the expected hexadecimal form.
valid_id(ID) when is_binary(ID) ->
    re:run(ID, <<"^[A-Fa-f0-9]{32}$">>, [{capture, none}]) =:= match;
valid_id(_) -> false.

%%% Tests

%% @doc Test exact Cloudflare request paths and record bodies.
provider_request_test() ->
    ZoneID = <<"023e105f4ecef8ad9ca31a8372d0c353">>,
    Record = <<"_acme-challenge.example.com">>,
    Value = <<"digest">>,
    {Path, <<"POST">>, Body} = provider_request(Record, Value, ZoneID),
    ?assertEqual(
        <<"/zones/023e105f4ecef8ad9ca31a8372d0c353/dns_records">>,
        Path
    ),
    ?assertEqual(#{
        <<"type">> => <<"TXT">>,
        <<"name">> => Record,
        <<"content">> => Value,
        <<"ttl">> => 60
    }, hb_json:decode(Body)).

%% @doc Test Cloudflare response and identifier validation.
provider_response_test() ->
    ZoneID = <<"023e105f4ecef8ad9ca31a8372d0c353">>,
    ?assertEqual({ok, ZoneID}, zone_id(
        #{<<"dns-zone-id">> => ZoneID}, #{}, <<"example.com">>,
        <<"unused">>, <<"https://unused.example">>
    )),
    ?assertEqual({ok, #{<<"id">> => ZoneID}}, provider_result(
        {ok, 200, [], hb_json:encode(#{
            <<"success">> => true,
            <<"result">> => #{<<"id">> => ZoneID}
        })}
    )),
    ?assertMatch({error, {'cloudflare-dns-error', _}}, provider_result(
        {ok, 200, [], hb_json:encode(#{
            <<"success">> => false,
            <<"errors">> => [#{<<"message">> => <<"denied">>}]
        })}
    )),
    ?assertEqual(
        {error, 'invalid-cloudflare-dns-response'},
        provider_result({ok, 200, [], <<"not-json">>})
    ),
    ?assertMatch(
        {error, {'cloudflare-dns-status', 403, _}},
        provider_result({ok, 403, [], <<"denied">>})
    ),
    ?assertEqual(false, valid_id(<<"not-a-zone-id">>)),
    ?assertEqual(
        {error, 'acme-dns-record-outside-zone'},
        put(
            #{
                <<"dns-zone">> => <<"example.com">>,
                <<"priv-dns-api-token">> => <<"unused">>
            },
            #{
                <<"record">> => <<"_acme-challenge.other.example">>,
                <<"value">> => <<"value">>
            },
            #{}
        )
    ).
