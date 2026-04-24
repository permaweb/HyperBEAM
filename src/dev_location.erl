%%% @doc Location registration records for nodes executing AO-Core computations.
%%% This device allows nodes to specify the physical location (resolved through
%%% DNS and IP addresses) that their cryptographic addresses will be found at
%%% for a period of time.
%%% 
%%% The interface is as follows:
%%% 
%%% `GET /~location@1.0/<address>': Read a location record from the cache or
%%%                                 gateway. If the record is retreived from a
%%%                                 gateway it will be cached locally.
%%% `GET /~location@1.0/node':      Generate a new location record and register it.
%%%                                 If signed by the operator, the record can
%%%                                 be generated for a specific nonce. Otherwise,
%%%                                 the record will be generated with a new nonce
%%%                                 chosen by the node.
%%% `POST /~location@1.0/known':    Cache a location record for a foreign peer
%%%                                 if the record is valid and newer than the
%%%                                 known nonce for the signer.
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
        default => fun read/4
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
                    dev_location_cache:write(Location, Opts),
                    {ok, Location};
                _ ->
                    {error,
                        #{
                            <<"status">> => 404,
                            <<"body">> =>
                                <<"No location found for address: ", Address/binary>>
                        }
                    }
            end
    end.

%% @doc Find the latest known nonce for an address by checking the local cache
%% first and then the gateway.
latest_known_nonce(Address, Opts) ->
    case read(Address, Opts) of
        {ok, Location} -> hb_maps:get(<<"nonce">>, Location, -1, Opts);
        _ -> -1
    end.

%% @doc Find the target to be used for during a request.
find_target(Base, RawReq, Opts) ->
    % Ensure that the request is signed by the operator.
    TargetSpec =
        hb_maps:get(
            <<"target">>,
            Base,
            hb_maps:get(<<"target">>, RawReq, not_found, Opts),
            Opts
        ),
    Req =
        case TargetSpec of
            not_found -> RawReq;
            <<"self">> -> Base;
            <<"request">> -> RawReq;
            Target ->
                hb_maps:get(Target, RawReq, RawReq, Opts)
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
    Req = find_target(Base, RawReq, Opts),
    % Ensure that the request is signed by the operator.
    {ok, OnlyCommitted} = hb_message:with_only_committed(Req, Opts),
    ?event(
        location,
        {scheduler_location_registration_request, OnlyCommitted},
        Opts
    ),
    Signers = hb_message:signers(OnlyCommitted, Opts),
    Self =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, hb:wallet(), Opts)
            )
        ),
    IsOperator = lists:member(Self, Signers),
    ExistingNonce = latest_known_nonce(Self, Opts),
    RequestedNonce = hb_maps:get(<<"nonce">>, OnlyCommitted, not_found, Opts),
    case {IsOperator, RequestedNonce} of
        {false, not_found} ->
            % A non-operator has requested that we generate a new location record.
            % First we check if we have a valid location record already and if
            % so return that instead.
            case dev_location_cache:read(Self, Opts) of
                {ok, Location} ->
                    {ok, Location};
                {error, not_found} ->
                    case hb_opts:get(location_open_generation, true, Opts) of
                        true ->
                            % We don't have a valid location record, so we generate a new
                            % one. We will not use any provided parameters as the caller
                            % is not trusted. Instead, we generate new ones from the
                            % node's configuration.
                            generate_new_location(
                                default_url(Opts),
                                erlang:system_time(millisecond),
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
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {false, _} ->
            % Specific-nonce generation requests are not permitted for
            % non-operators.
            {error, <<"Non-operators cannot request specific nonces.">>};
        {true, not_found} ->
            % The operator has requested a new location record with an unknown
            % nonce. We will generate a new one.
            generate_new_location(
                erlang:system_time(millisecond),
                Base,
                OnlyCommitted,
                Opts
            );
        {true, SpecificNonce} ->
            case SpecificNonce > ExistingNonce of
                true ->
                    generate_new_location(SpecificNonce, Base, OnlyCommitted, Opts);
                false ->
                    {error,
                        #{
                            <<"status">> => 400,
                            <<"body">> => <<"Known nonce higher than requested nonce.">>,
                            <<"requested-nonce">> => SpecificNonce,
                            <<"existing-nonce">> => ExistingNonce,
                            <<"signers">> => Signers
                        }
                    }
            end
    end.

%% @doc Generate the default location record URL from the node's configuration.
%% If a custom URL is provided in the `location_url' option, we will use that,
%% otherwise we will construct the URL from the node's configuration (host, port,
%% and protocol).
default_url(Opts) ->
    case hb_opts:get(location_url, not_found, Opts) of
        not_found ->
            Port = hb_util:bin(hb_opts:get(port, 8734, Opts)),
            Host = hb_opts:get(node_host, <<"localhost">>, Opts),
            Protocol = hb_opts:get(protocol, http1, Opts),
            ProtoStr =
                case Protocol of
                    http1 -> <<"http">>;
                    _ -> <<"https">>
                end,
            <<ProtoStr/binary, "://", Host/binary, ":", Port/binary>>;
        GivenURL -> GivenURL
    end.

%% @doc We have been asked to generate a new location record, given the nonce,
%% TTL, and codec. We will generate the record, sign it, store it in the cache,
%% asynchronously upload it to Arweave, and notify the peers specified in the
%% `location_notify' option. Finally, we will return the signed location record
%% to the caller.
generate_new_location(Nonce, Base, OnlyCommitted, Opts) ->
    DefaultTTL =
        hb_opts:get(
            location_ttl,
            hb_opts:get(scheduler_location_ttl, 1000 * 60 * 60, Opts),
            Opts
        ),
    TimeToLive =
        case hb_maps:get(<<"time-to-live">>, Base, not_found, Opts) of
            not_found ->
                hb_maps:get(<<"time-to-live">>, OnlyCommitted, DefaultTTL, Opts);
            TTLValue ->
                TTLValue
        end,
    URL =
        case hb_maps:get(<<"url">>, OnlyCommitted, not_found, Opts) of
            not_found -> default_url(Opts);
            GivenURL -> GivenURL
        end,
    % Construct the new scheduler location message.
    DefaultCodec = hb_opts:get(location_codec, ?DEFAULT_CODEC, Opts),
    Codec =
        case hb_maps:get(<<"require-codec">>, Base, not_found, Opts) of
            not_found ->
                hb_maps:get(<<"require-codec">>, OnlyCommitted, DefaultCodec, Opts);
            CodecValue ->
                CodecValue
        end,
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
                PostRes =
                    hb_http:post(
                        Node,
                        <<"/~location@1.0/known">>,
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
    MaybeLocation = find_target(Base, RawReq, Opts),
    maybe
        Signers = hb_message:signers(MaybeLocation, Opts),
        LocationType =
            hb_ao:get_first(
                [
                    {MaybeLocation, <<"type">>},
                    {MaybeLocation, <<"Type">>}
                ],
                not_found,
                Opts
            ),
        NormalizedType =
            case LocationType of
                not_found -> not_found;
                _ -> hb_ao:normalize_key(LocationType)
            end,
        true ?= hb_message:verify(MaybeLocation, all, Opts)
            orelse {error, <<"Invalid location record signature.">>},
        true ?=
            lists:member(
                NormalizedType,
                [<<"location">>, <<"scheduler-location">>]
            )
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
        Nonce = hb_util:int(hb_ao:get(<<"nonce">>, MaybeLocation, 0, Opts)),
        SignerChecks =
            lists:map(
                fun(Signer) ->
                    {Signer, latest_nonce(Signer, Nonce, Opts)}
                end,
                Signers
            ),
        lists:foreach(
            fun
                ({Signer, false}) ->
                    ?event(
                        location,
                        {newer_foreign_peer_location_already_exists,
                            {signer, Signer},
                            {nonce, Nonce},
                            {location, MaybeLocation}
                        }
                    );
                (_) ->
                    ok
            end,
            SignerChecks
        ),
        CanWrite =
            lists:any(
                fun({_Signer, IsLatest}) -> IsLatest end,
                SignerChecks
            ),
        case CanWrite of
            true ->
                case dev_location_cache:write(MaybeLocation, Opts) of
                    ok ->
                        {ok, MaybeLocation};
                    {error, Reason} ->
                        {error,
                            #{
                                <<"status">> => 400,
                                <<"body">> =>
                                    <<"Failed to store new location record.">>,
                                <<"reason">> => Reason
                            }
                        }
                end;
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
            hb_util:int(hb_ao:get(<<"nonce">>, Location, -1, Opts)) < Nonce;
        _ ->
            true
    end.

%%% Tests

register_scheduler_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()], <<"priv-wallet">> => ar_wallet:new() },
    Node = hb_http_server:start_node(Opts),
    Base =
        hb_message:commit(
            #{
                <<"path">> => <<"/~location@1.0/node">>,
                <<"url">> => <<"https://hyperbeam-test-ignore.com">>,
                <<"method">> => <<"POST">>,
                <<"nonce">> => 1,
                <<"require-codec">> => <<"ans104@1.0">>
            },
            Opts
        ),
    {ok, Res} = hb_http:post(Node, Base, Opts),
    ?assertMatch(#{ <<"url">> := Location } when is_binary(Location), Res).

%% @doc Test that unsigned GET calls to `node' return the same location record
%% once one has been generated.
unsigned_get_node_is_idempotent_test() ->
    Wallet = ar_wallet:new(),
    Opts = #{
        <<"store">> => [hb_test_utils:test_store()],
        <<"priv-wallet">> => Wallet
    },
    Node = hb_http_server:start_node(Opts),
    {ok, FirstRes} = hb_http:get(Node, <<"/~location@1.0/node">>, #{}),
    FirstLocation = hb_ao:get(<<"body">>, FirstRes, FirstRes, #{}),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    {ok, CachedAfterFirst} = dev_location_cache:read(Address, Opts),
    timer:sleep(10),
    {ok, SecondRes} = hb_http:get(Node, <<"/~location@1.0/node">>, #{}),
    SecondLocation = hb_ao:get(<<"body">>, SecondRes, SecondRes, #{}),
    {ok, CachedAfterSecond} = dev_location_cache:read(Address, Opts),
    ?assertEqual(CachedAfterFirst, CachedAfterSecond),
    FirstNonce = hb_util:int(hb_maps:get(<<"nonce">>, FirstLocation, -1, #{})),
    SecondNonce = hb_util:int(hb_maps:get(<<"nonce">>, SecondLocation, -1, #{})),
    CachedNonce = hb_util:int(hb_maps:get(<<"nonce">>, CachedAfterSecond, -1, #{})),
    ?assert(FirstNonce > 0),
    ?assertEqual(FirstNonce, SecondNonce),
    ?assertEqual(FirstNonce, CachedNonce),
    ?assertEqual(
        hb_maps:get(<<"url">>, FirstLocation, not_found, #{}),
        hb_maps:get(<<"url">>, SecondLocation, not_found, #{})
    ).

%% @doc Test that a scheduler location is registered on boot.
register_location_on_boot_test() ->
    NotifiedPeerWallet = ar_wallet:new(),
    RegisteringNodeWallet = ar_wallet:new(),
    hb_http_server:start_node(#{}),
    NotifiedPeer =
        hb_http_server:start_node(#{
            <<"priv-wallet">> => NotifiedPeerWallet,
            <<"store">> => [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-TEST/scheduler-location-notified">>
                }
            ]
        }),
    RegisteringNode = hb_http_server:start_node(
        #{
            <<"priv-wallet">> => RegisteringNodeWallet,
            <<"on">> =>
                #{
                    <<"start">> => #{
                        <<"device">> => <<"location@1.0">>,
                        <<"path">> => <<"node">>,
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
            <<"location-notify">> => [NotifiedPeer]
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
    CurrentBody = hb_ao:get(<<"body">>, CurrentLocation, CurrentLocation, #{}),
    ?event({current_location, CurrentLocation}),
    ?assertMatch(
        #{
            <<"url">> := <<"https://hyperbeam-test-ignore.com">>,
            <<"nonce">> := Nonce
        } when Nonce > 0,
        CurrentBody
    ),
    {ok, RemoteLocation} =
        hb_http:get(
            RegisteringNode,
            <<"/~location@1.0/", Address/binary>>,
            #{}
        ),
    ?assertMatch(
        #{
            <<"url">> := <<"https://hyperbeam-test-ignore.com">>,
            <<"nonce">> := Nonce
        } when Nonce > 0,
        RemoteLocation
    ).
