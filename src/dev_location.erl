%%% @doc Location registration records for nodes executing AO-Core computations.
%%% This device allows nodes to specify the physical location (resolved through
%%% DNS and IP addresses) that their cryptographic addresses will be found at
%%% for a period of time.
-module(dev_location).
-export([info/0, read/2, node/3, known/3, all/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_TTL, 28 * 24 * 60 * 60). % 28 days.
-define(DEFAULT_CODEC, <<"httpsig@1.0">>).

%% @doc Handle all requests aside `known` with the `location/4' resolver.
info() ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>],
        default_handler => fun read/4
    }.

%% @doc Route either `POST' or `GET' requests to the correct handler for known
%% location records.
known(Base, Req, Opts) ->
    case hb_ao:get(<<"method">>, Req, <<"GET">>, Opts) of
        <<"POST">> -> write_foreign(Base, Req, Opts);
        <<"GET">> -> all(Base, Req, Opts)
    end.

%% @doc List all known location records.
all(_Base, _Req, Opts) ->
    dev_location_cache:list(Opts).

%% @doc Search for the location of the scheduler in the scheduler-location
%% cache. If an address is provided, we search for the location of that
%% specific scheduler. Otherwise, we return the location record for the current
%% node's scheduler, if it has been established.
read(Address, _Base, _Req, Opts) ->
    read(Address, Opts).
read(Address, Opts) ->
    % Search for the location of the scheduler in the scheduler-location cache.
    case dev_location_cache:read(Address, Opts) of
        {ok, Location} -> {ok, Location};
        _ ->
            case hb_gateway_client:location(Address, Opts) of
                {ok, Location} ->
                    % Cache the location record locally, now that we have found it.
                    dev_location_cache:write(Location, Opts),
                    {ok, Location};
                not_found ->
                    {error,
                        #{
                            <<"status">> => 404,
                            <<"body">> =>
                                <<"No location found for address: ", Address/binary>>
                            }
                        }
            end
    end.

%% @doc Find the target to be used for during a request.
find_record(Base, RawReq, Opts) ->
    % Ensure that the request is signed by the operator.
    Req =
        case hb_ao:get_first(
            [{Base, <<"target">>}, {RawReq, <<"target">>}],
            not_found,
            Opts
        ) of
            not_found -> RawReq;
            <<"self">> -> Base;
            <<"request">> -> RawReq;
            Target -> hb_ao:get(Target, RawReq, not_found, Opts)
        end,
    {ok, OnlyCommitted} = hb_message:with_only_committed(Req, Opts),
    OnlyCommitted.

%% @doc Generate a new scheduler location record and register it. We both send 
%% the new scheduler-location to the given registry, and return it to the caller.
node(Base, RawReq, RawOpts) ->
    Opts =
        case dev_whois:ensure_host(RawOpts) of
            {ok, NewOpts} -> NewOpts;
            _ -> RawOpts
        end,
    Req = find_record(Base, RawReq, Opts),
    % Ensure that the request is signed by the operator.
    {ok, OnlyCommitted} = hb_message:with_only_committed(Req, Opts),
    ?event(
        location,
        {scheduler_location_registration_request, OnlyCommitted},
        Opts
    ),
    Signers = hb_message:signers(OnlyCommitted, Opts),
    Self = hb_util:human_id(hb_opts:get(priv_wallet, hb:wallet(), Opts)),
    IsOperator = lists:member(Self, Signers),
    NewNonce = hb_ao:get(<<"nonce">>, OnlyCommitted, not_found, Opts),
    case NewNonce of
        not_found when not IsOperator ->
            % A non-operator has requested that we generate a new location record.
            % First we check if we have a valid location record already and if
            % so return that instead.
            case dev_location_cache:read(Self, Opts) of
                {ok, Location} ->
                    {ok, Location};
                not_found ->
                    case hb_opts:get(location_open_generation, true, Opts) of
                        true ->
                            % We don't have a valid location record, so we generate a new
                            % one. We will not use any provided parameters as the caller
                            % is not trusted. Instead, we generate new ones from the
                            % node's configuration.
                            generate_new_location(
                                default_url(Opts),
                                erlang:system_time(microsecond),
                                hb_opts:get(location_ttl, ?DEFAULT_TTL, Opts),
                                hb_opts:get(location_codec, ?DEFAULT_CODEC, Opts),
                                Opts
                            );
                        false ->
                            {error,
                                #{
                                    <<"status">> => 403,
                                    <<"body">> =>
                                        <<
                                            "Unauthorized location generation not",
                                            "permitted on this node."
                                        >>
                                }
                            }
                    end
            end;
        _ when not IsOperator ->
            % Specific-nonce generation requests are not permitted for
            % non-operators.
            {error, <<"Non-operators cannot request specific nonces.">>};
        SpecificNonce when IsOperator ->
            generate_new_location(SpecificNonce, Base, OnlyCommitted, Opts)
    end.

%% @doc Generate the default location record URL from the node's configuration.
default_url(Opts) ->
    Port = hb_util:bin(hb_opts:get(port, 8734, Opts)),
    Host = hb_opts:get(host, <<"localhost">>, Opts),
    Protocol = hb_opts:get(protocol, http1, Opts),
    ProtoStr =
        case Protocol of
            http1 -> <<"http">>;
            _ -> <<"https">>
        end,
    <<ProtoStr/binary, "://", Host/binary, ":", Port/binary>>.

%% @doc We have been asked to generate a new location record, given the nonce,
%% TTL, and codec. We will generate the record, sign it, store it in the cache,
%% asynchronously upload it to Arweave, and notify the peers specified in the
%% `location_notify' option. Finally, we will return the signed location record
%% to the caller.
generate_new_location(Nonce, Base, OnlyCommitted, Opts) ->
    TimeToLive =
        hb_ao:get_first(
            [
                {Base, <<"time-to-live">>},
                {OnlyCommitted, <<"time-to-live">>}
            ],
            hb_opts:get(scheduler_location_ttl, 1000 * 60 * 60, Opts),
            Opts
        ),
    URL =
        case hb_ao:get(<<"url">>, OnlyCommitted, Opts) of
            not_found -> default_url(Opts);
            GivenURL -> GivenURL
        end,
    % Construct the new scheduler location message.
    Codec =
        hb_ao:get_first(
            [
                {Base, <<"require-codec">>},
                {OnlyCommitted, <<"require-codec">>}
            ],
            <<"httpsig@1.0">>,
            Opts
        ),
    generate_new_location(URL, Nonce, TimeToLive, Codec, Opts).
generate_new_location(URL, Nonce, TTL, Codec, Opts) ->
    NewSchedulerLocation =
        #{
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"type">> => <<"location">>,
            <<"url">> => URL,
            <<"nonce">> => Nonce,
            <<"time-to-live">> => TTL,
            <<"codec-device">> => Codec
        },
    Signed = hb_message:commit(NewSchedulerLocation, Opts, Codec),
    dev_location_cache:write(Signed, Opts),
    ?event(location,
        {uploading_signed_scheduler_location, Signed}
    ),
    % Asynchronously upload the location record to Arweave.
    spawn(
        fun() ->
            hb_client:upload(Signed, Opts)
        end
    ),
    % Post the new scheduler location to the peers specified in the
    % `location_notify' option.
    Results =
        lists:map(
            fun(Node) ->
                PostRes = hb_http:post(
                    Node,
                    <<"/~scheduler@1.0/location">>,
                    Signed,
                    Opts
                ),
                ?event(scheduler_location,
                    {outbound_request, {res, PostRes}}
                )
            end,
            hb_opts:get(location_notify, [], Opts)
        ),
    ?event(location,
        {location_registration_success,
            {arweave_publication, async_upload_initiated},
            {foreign_peers_notified, length(Results)}
        }
    ),
    {ok, Signed}.

%% @doc Verify and write a location record for a foreign peer to the cache.
write_foreign(Base, RawReq, Opts) ->
    MaybeLocation = find_record(Base, RawReq, Opts),
    maybe
        Signers = hb_message:signers(MaybeLocation, Opts),
        true ?= hb_message:verify(MaybeLocation, signed, Opts)
            orelse {error, <<"Invalid location record signature.">>},
        true ?=
            (hb_maps:get(<<"type">>, MaybeLocation, Opts) =:= <<"scheduler-location">>)
            orelse {error, <<"Invalid location record type.">>},
        true ?=
            (hb_maps:get(<<"url">>, MaybeLocation, Opts) =/= not_found)
            orelse {error, <<"Missing location record URL.">>},
        true ?=
            (hb_maps:get(<<"nonce">>, MaybeLocation, Opts) =/= not_found)
            orelse {error, <<"Missing location record nonce.">>},
        true ?=
            (hb_maps:get(<<"time-to-live">>, MaybeLocation, Opts) =/= not_found)
            orelse {error, <<"Missing location record time-to-live.">>},
        Nonce = hb_ao:get(<<"nonce">>, MaybeLocation, Opts),
        Res = lists:any(
            fun(Signer) ->
                case latest_nonce(Signer, Nonce, Opts) of
                    true ->
                        dev_location_cache:write(MaybeLocation, Opts);
                    false ->
                        ?event(
                            location,
                            {newer_foreign_peer_location_already_exists,
                                {signer, Signer},
                                {nonce, Nonce},
                                {location, MaybeLocation}
                            }
                        )
                end

           end,
            Signers
        ),
        case Res of
            true ->
                {ok, MaybeLocation};
            false ->
                {error,
                    #{
                        <<"status">> => 400,
                        <<"body">> =>
                            <<"Known nonce(s) higher than requested nonce.">>,
                        <<"requested-nonce">> => Nonce,
                        <<"signers">> => Signers
                    }
                }
        end
    end.

%% @doc Check if a given nonce is the latest nonce for a given signer.
latest_nonce(Signer, Nonce, Opts) ->
    case dev_location_cache:read(Signer, Opts) of
        {ok, Location} ->
            hb_util:int(hb_ao:get(<<"nonce">>, Location, 0, Opts)) > Nonce;
        not_found ->
            -1
    end.

%%% Tests

register_scheduler_test() ->
    Opts = #{ store => [hb_test_utils:test_store()], priv_wallet => ar_wallet:new() },
    Node = hb_http_server:start_node(Opts),
    Base =
        hb_message:commit(
            #{
                <<"path">> => <<"/~scheduler@1.0/location">>,
                <<"url">> => <<"https://hyperbeam-test-ignore.com">>,
                <<"method">> => <<"POST">>,
                <<"nonce">> => 1,
                <<"require-codec">> => <<"ans104@1.0">>
            },
            Opts
        ),
    {ok, Res} = hb_http:post(Node, Base, Opts),
    ?assertMatch(#{ <<"url">> := Location } when is_binary(Location), Res).

%% @doc Test that a scheduler location is registered on boot.
register_location_on_boot_test() ->
    NotifiedPeerWallet = ar_wallet:new(),
    RegisteringNodeWallet = ar_wallet:new(),
    hb_http_server:start_node(#{}),
    NotifiedPeer =
        hb_http_server:start_node(#{
            priv_wallet => NotifiedPeerWallet,
            store => [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-TEST/scheduler-location-notified">>
                }
            ]
        }),
    RegisteringNode = hb_http_server:start_node(
        #{
            priv_wallet => RegisteringNodeWallet,
            on =>
                #{
                    <<"start">> => #{
                        <<"device">> => <<"scheduler@1.0">>,
                        <<"path">> => <<"location">>,
                        <<"method">> => <<"POST">>,
                        <<"target">> => <<"self">>,
                        <<"require-codec">> => <<"ans104@1.0">>,
                        <<"url">> => <<"https://hyperbeam-test-ignore.com">>,
                        <<"hook">> => #{
                            <<"result">> => <<"ignore">>,
                            <<"commit-request">> => true
                        }
                    }
                },
            location_notify => [NotifiedPeer]
        }
    ),
    Address = hb_util:human_id(ar_wallet:to_address(RegisteringNodeWallet)),
    {ok, CurrentLocation} =
        hb_http:get(
            RegisteringNode,
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"/~location@1.0/node">>
            },
            #{}
        ),
    ?event({current_location, CurrentLocation}),
    ?assertMatch(
        #{
            <<"url">> := <<"https://hyperbeam-test-ignore.com">>,
            <<"nonce">> := 0
        },
        hb_ao:get(<<"body">>, CurrentLocation, #{})
    ),
    ?assertMatch(
        #{
            <<"url">> := <<"https://hyperbeam-test-ignore.com">>,
            <<"nonce">> := 0
        },
        hb_http:get(RegisteringNode, <<"/~location@1.0/", Address/binary>>, #{})
    ),
    ok.