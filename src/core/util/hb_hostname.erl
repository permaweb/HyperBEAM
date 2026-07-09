%%% @doc Public DNS and local-network exclusion helpers.
-module(hb_hostname).
-export([is_public/2, public_ips/2, is_public_ip/1, uri_host/1, normalize/1]).
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_DNS_SERVERS, [{{1, 1, 1, 1}, 53}, {{8, 8, 8, 8}, 53}]).

%% @doc True iff `Host' resolves publicly, and all answers are public IPs.
is_public(Host, Opts) ->
    public_ips(Host, Opts) =/= [].

%% @doc Return the normalized host from a URI, or `not_found' if absent.
uri_host(URI) ->
    case uri_string:parse(URI) of
        #{host := Host} -> {ok, normalize(Host)};
        _ -> {error, invalid_uri}
    end.

normalize(Host) ->
    strip_trailing_dot(string:lowercase(hb_util:bin(Host))).

%% @doc Return vetted public IPs for `Host', or `[]' if any answer is unsafe.
public_ips(Host, Opts) ->
    case inet:parse_strict_address(hb_util:list(Host)) of
        {ok, IP} ->
            safe_ips([IP]);
        {error, _} ->
            safe_ips(dns_ips(Host, Opts))
    end.

%% @doc Return public-DNS A and AAAA answers for `Host'.
dns_ips(Host, Opts) ->
    ResOpts = [
        % Query DNS directly so host files and search domains are ignored.
        {nameservers, dns_servers(Opts)},
        {timeout, hb_opts:get(dns_timeout, 1000, Opts)},
        {retry, hb_opts:get(dns_retries, 1, Opts)}
    ],
    lookup(Host, a, ResOpts) ++ lookup(Host, aaaa, ResOpts).

%% @doc True iff `IP' is neither special-use nor on a local interface subnet.
is_public_ip(IP) ->
    valid_ip(IP) andalso not blocked_ip(IP) andalso not local_ip(IP).

%% @doc Return `IPs' iff none are unsafe.
safe_ips([]) -> [];
safe_ips(IPs) ->
    case lists:filter(fun(IP) -> not is_public_ip(IP) end, IPs) of
        [] -> IPs;
        _ -> []
    end.

%% @doc Resolve a single RR type, treating DNS failure as no public answer.
lookup(Host, Type, ResOpts) ->
    try inet_res:lookup(hb_util:list(Host), in, Type, ResOpts)
    catch
        _:_ -> []
    end.

dns_servers(Opts) ->
    Servers = dns_server_list(hb_opts:get(dns_servers, ?DEFAULT_DNS_SERVERS, Opts)),
    case lists:map(fun dns_server/1, Servers) of
        [] -> throw(invalid_dns_server);
        ValidServers -> ValidServers
    end.

dns_server({IP, Port}) when is_integer(Port), Port > 0, Port =< 65535 ->
    dns_server(IP, Port);
dns_server(IP) when is_tuple(IP) ->
    dns_server(IP, 53);
dns_server(Server) ->
    case inet:parse_strict_address(hb_util:list(Server)) of
        {ok, IP} -> {IP, 53};
        _ -> throw(invalid_dns_server)
    end.

dns_server(IP, Port) ->
    case valid_ip(IP) of
        true -> {IP, Port};
        false -> throw(invalid_dns_server)
    end.

dns_server_list(Server) when is_binary(Server); is_tuple(Server) -> [Server];
dns_server_list(Servers) when is_list(Servers) -> Servers;
dns_server_list(_) -> throw(invalid_dns_server).

valid_ip(IP) when is_tuple(IP), tuple_size(IP) =:= 4 ->
    lists:all(fun(Part) -> valid_part(Part, 255) end, tuple_to_list(IP));
valid_ip(IP) when is_tuple(IP), tuple_size(IP) =:= 8 ->
    lists:all(fun(Part) -> valid_part(Part, 16#ffff) end, tuple_to_list(IP));
valid_ip(_) ->
    false.

valid_part(Part, Max) ->
    is_integer(Part) andalso Part >= 0 andalso Part =< Max.

strip_trailing_dot(<<>>) -> <<>>;
strip_trailing_dot(Host) ->
    case binary:last(Host) of
        $. -> binary:part(Host, 0, byte_size(Host) - 1);
        _ -> Host
    end.

%% @doc True iff `IP' is in a special-use range.
blocked_ip(IP) ->
    lists:any(fun(Net) -> in_net(IP, Net) end, blocked_nets()).

%% @doc Special-use ranges that should never be relay targets.
blocked_nets() ->
    [
        {{0, 0, 0, 0}, 8}, {{10, 0, 0, 0}, 8}, {{100, 64, 0, 0}, 10},
        {{127, 0, 0, 0}, 8}, {{169, 254, 0, 0}, 16},
        {{172, 16, 0, 0}, 12}, {{192, 0, 0, 0}, 24},
        {{192, 0, 2, 0}, 24}, {{192, 88, 99, 0}, 24},
        {{192, 168, 0, 0}, 16}, {{198, 18, 0, 0}, 15},
        {{198, 51, 100, 0}, 24}, {{203, 0, 113, 0}, 24},
        {{224, 0, 0, 0}, 4}, {{240, 0, 0, 0}, 4},
        {{0, 0, 0, 0, 0, 0, 0, 0}, 128},
        {{0, 0, 0, 0, 0, 0, 0, 1}, 128},
        {{0, 0, 0, 0, 0, 0, 0, 0}, 96},
        {{0, 0, 0, 0, 0, 16#ffff, 0, 0}, 96},
        {{16#64, 16#ff9b, 0, 0, 0, 0, 0, 0}, 96},
        {{16#64, 16#ff9b, 1, 0, 0, 0, 0, 0}, 48},
        {{16#100, 0, 0, 0, 0, 0, 0, 0}, 64},
        {{16#2001, 0, 0, 0, 0, 0, 0, 0}, 23},
        {{16#2001, 16#db8, 0, 0, 0, 0, 0, 0}, 32},
        {{16#2002, 0, 0, 0, 0, 0, 0, 0}, 16},
        {{16#fc00, 0, 0, 0, 0, 0, 0, 0}, 7},
        {{16#fe80, 0, 0, 0, 0, 0, 0, 0}, 10},
        {{16#ff00, 0, 0, 0, 0, 0, 0, 0}, 8}
    ].

%% @doc True iff `IP' is on an attached interface subnet.
local_ip(IP) ->
    case inet:getifaddrs() of
        {ok, Ifs} ->
            local_ip(
                IP,
                lists:flatmap(fun({_Name, Attrs}) -> nets(Attrs) end, Ifs)
            );
        _ ->
            false
    end.

%% @doc True iff `IP' is on any `{Address, Netmask}' subnet.
local_ip(IP, Nets) ->
    lists:any(
        fun({Addr, Mask}) ->
            tuple_size(IP) =:= tuple_size(Addr)
                andalso (ip_int(IP) band ip_int(Mask))
                    =:= (ip_int(Addr) band ip_int(Mask))
        end,
        Nets
    ).

%% @doc Extract interface address/netmask pairs.
nets(Attrs) ->
    nets(Attrs, undefined).
nets([{addr, Addr} | Rest], _) ->
    nets(Rest, Addr);
nets([{netmask, Mask} | Rest], Addr)
        when is_tuple(Addr), tuple_size(Addr) =:= tuple_size(Mask) ->
    [{Addr, Mask} | nets(Rest, undefined)];
nets([_ | Rest], Addr) ->
    nets(Rest, Addr);
nets([], _) ->
    [].

%% @doc True iff `IP' is in `Net/Prefix'.
in_net(IP, {Net, Prefix}) when tuple_size(IP) =:= tuple_size(Net) ->
    Shift = ip_bits(IP) - Prefix,
    (ip_int(IP) bsr Shift) =:= (ip_int(Net) bsr Shift);
in_net(_, _) ->
    false.

%% @doc Return the address width in bits.
ip_bits(IP) when tuple_size(IP) =:= 4 -> 32;
ip_bits(IP) when tuple_size(IP) =:= 8 -> 128.

%% @doc Convert an IP tuple to an integer.
ip_int(IP) when tuple_size(IP) =:= 4 ->
    lists:foldl(fun(Part, Acc) -> (Acc bsl 8) bor Part end, 0, tuple_to_list(IP));
ip_int(IP) when tuple_size(IP) =:= 8 ->
    lists:foldl(fun(Part, Acc) -> (Acc bsl 16) bor Part end, 0, tuple_to_list(IP)).

%%% Tests

is_public_ip_test() ->
    ?assert(is_public_ip({8, 8, 8, 8})),
    ?assert(is_public_ip({16#2001, 16#4860, 16#4860, 0, 0, 0, 0, 16#8888})),
    ?assertNot(is_public_ip({127, 0, 0, 1})),
    ?assertNot(is_public_ip({192, 168, 1, 1})),
    ?assertNot(is_public_ip({169, 254, 169, 254})),
    ?assertNot(is_public_ip({16#fc00, 0, 0, 0, 0, 0, 0, 1})),
    ?assertNot(is_public_ip({16#fe80, 0, 0, 0, 0, 0, 0, 1})).

local_ip_test() ->
    Nets = [{{192, 168, 10, 211}, {255, 255, 255, 0}}],
    ?assert(local_ip({192, 168, 10, 1}, Nets)),
    ?assertNot(local_ip({192, 168, 11, 1}, Nets)).

public_ips_literal_test() ->
    ?assertEqual([{8, 8, 8, 8}], public_ips(<<"8.8.8.8">>, #{})),
    ?assertEqual([], public_ips(<<"127.0.0.1">>, #{})).

uri_host_test() ->
    ?assertEqual({ok, <<"example.com">>}, uri_host(<<"https://Example.COM./x">>)),
    ?assertEqual({error, invalid_uri}, uri_host(<<"/x">>)).
