%%% @doc DigitalOcean implementation of the TLS DNS provider interface.
-module(dev_tls_dns_digitalocean).
-implements(<<"tls-dns-digitalocean@1.0">>).
-device_libraries([lib_tls_dns_provider]).
-export([info/1, put/3, delete/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(API_URL, <<"https://api.digitalocean.com/v2">>).

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
                {Path, Method, Body} = provider_request(Record, Value, Zone),
                {ok, ID} ?= create_result(lib_tls_dns_provider:request(
                    APIURL, Path, Method, Token, Body, Opts
                )),
                ?event(tls, {dns_record_created, {provider, digitalocean}}),
                {ok, #{
                    <<"provider">> => <<"digitalocean">>,
                    <<"zone">> => Zone,
                    <<"record-id">> => ID
                }}
            else
                {false, _} -> {error, 'invalid-acme-dns-record'};
                {_, false} -> {error, 'invalid-acme-dns-value'};
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
            ID = hb_maps:get(<<"record-id">>, Handle, undefined, Opts),
            Provider = hb_maps:get(<<"provider">>, Handle, undefined, Opts),
            maybe
                true ?= Provider =:= <<"digitalocean">>
                    andalso Zone =:= ConfiguredZone andalso is_integer(ID),
                Path = <<"/domains/", Zone/binary, "/records/",
                    (integer_to_binary(ID))/binary>>,
                ok ?= delete_result(lib_tls_dns_provider:request(
                    APIURL, Path, <<"DELETE">>, Token, <<>>, Opts
                )),
                ?event(tls, {dns_record_deleted, {provider, digitalocean}}),
                {ok, ok}
            else
                false -> {error, 'invalid-digitalocean-dns-handle'};
                {error, _} = Error -> Error
            end
        end
    ).

%% @doc Build a DigitalOcean DNS record creation request.
provider_request(Record, Value, Zone) ->
    {
        <<"/domains/", Zone/binary, "/records">>,
        <<"POST">>,
        hb_json:encode(#{
            <<"type">> => <<"TXT">>,
            <<"name">> => lib_tls_dns_provider:relative_name(Record, Zone),
            <<"data">> => Value,
            <<"ttl">> => 30
        })
    }.

%% @doc Validate a DigitalOcean DNS record creation response.
create_result({ok, 201, _, Body}) ->
    try
        #{<<"domain_record">> := #{<<"id">> := ID}} = hb_json:decode(Body),
        true = is_integer(ID),
        {ok, ID}
    catch _:_ -> {error, 'invalid-digitalocean-dns-response'}
    end;
create_result(Response) -> provider_error(Response).

%% @doc Validate a DigitalOcean DNS record deletion response.
delete_result({ok, 204, _, _}) -> ok;
delete_result(Response) -> provider_error(Response).

%% @doc Normalize a DigitalOcean API error response.
provider_error({ok, Status, _, Body}) ->
    {error, {'digitalocean-dns-status', Status,
        lib_tls_dns_provider:problem(Body)}};
provider_error({error, Reason}) -> {error, Reason}.

%%% Tests

%% @doc Test exact DigitalOcean request paths and record bodies.
provider_request_test() ->
    Zone = <<"example.com">>,
    Record = <<"_acme-challenge.example.com">>,
    Value = <<"digest">>,
    {Path, <<"POST">>, Body} = provider_request(Record, Value, Zone),
    ?assertEqual(<<"/domains/example.com/records">>, Path),
    ?assertEqual(#{
        <<"type">> => <<"TXT">>,
        <<"name">> => <<"_acme-challenge">>,
        <<"data">> => Value,
        <<"ttl">> => 30
    }, hb_json:decode(Body)).

%% @doc Test DigitalOcean response validation.
provider_response_test() ->
    ?assertEqual({ok, 42}, create_result(
        {ok, 201, [], hb_json:encode(#{
            <<"domain_record">> => #{<<"id">> => 42}
        })}
    )),
    ?assertEqual(
        {error, 'invalid-digitalocean-dns-response'},
        create_result({ok, 201, [], hb_json:encode(#{
            <<"domain_record">> => #{<<"id">> => <<"invalid">>}
        })})
    ),
    ?assertMatch(
        {error, {'digitalocean-dns-status', 403, _}},
        create_result({ok, 403, [], <<"denied">>})
    ),
    ?assertEqual(ok, delete_result({ok, 204, [], <<>>})).
