%%% @doc TLS listener options for HyperBEAM's HTTP server. Builds the ssl socket
%%% options from the node message `tls' block and answers per-handshake
%%% certificate lookups via an `sni_fun'. Certificates are held per-host in
%%% `persistent_term', so a node can serve multiple domains (SNI) and rotate a
%%% certificate live (by refreshing the table) without restarting the listener.
-module(hb_tls).
-export([socket_opts/1, refresh/1, sni_lookup/1]).
-include_lib("eunit/include/eunit.hrl").

-define(CERTS, {?MODULE, certs}).

%% @doc Build the ssl listener options from the `tls' block of the node message,
%% or `no_tls' when no `tls' block is present (the caller then starts a plain
%% listener, so existing nodes are unaffected).
socket_opts(NodeMsg) ->
    case maps:get(<<"tls">>, NodeMsg, not_found) of
        not_found ->
            no_tls;
        Tls when is_map(Tls) ->
            ok = refresh(NodeMsg),
            [
                {sni_fun, fun ?MODULE:sni_lookup/1},
                {versions, versions(Tls)},
                %% HTTP/2 only: advertise just h2 over ALPN. Clients that offer
                %% only http/1.1 fail the handshake. Clients that send no ALPN
                %% are caught by the sub-h2 guard in hb_http_server.
                {alpn_preferred_protocols, [<<"h2">>]},
                {honor_cipher_order, true}
            ]
    end.

%% @doc (Re)build the per-host certificate table and store it in
%% `persistent_term'. Called when the listener is created and, for live
%% rotation, whenever the node `tls' configuration changes.
refresh(NodeMsg) ->
    Tls = maps:get(<<"tls">>, NodeMsg, #{}),
    Table = build_table(maps:get(<<"certs">>, Tls, [])),
    persistent_term:put(?CERTS, Table),
    ok.

%% @doc The ssl `sni_fun' callback. Returns the certificate options for the
%% requested server name, falling back to a `default' entry. An unknown name
%% with no default returns `[]', which fails the handshake closed.
sni_lookup(ServerName) ->
    Table = persistent_term:get(?CERTS, #{}),
    maps:get(list_to_binary(ServerName), Table, maps:get(default, Table, [])).

%% Build #{ Host => CertKeyOpts } from the configured entries. A single-cert
%% node serves its certificate for any SNI; a multi-cert node serves the
%% per-host certificate and fails closed for an unmatched name unless an entry
%% sets `default => true'.
build_table(Certs) ->
    Entries = [{Entry, entry_opts(Entry)} || Entry <- Certs],
    Table = lists:foldl(fun add_entry/2, #{}, Entries),
    case {Entries, maps:is_key(default, Table)} of
        {[{_, Opts}], false} -> Table#{ default => Opts };
        _ -> Table
    end.

add_entry({Entry, Opts}, Acc) ->
    WithHosts =
        lists:foldl(
            fun(Domain, Map) -> Map#{ Domain => Opts } end,
            Acc,
            maps:get(<<"domains">>, Entry, [])
        ),
    case maps:get(<<"default">>, Entry, false) of
        true -> WithHosts#{ default => Opts };
        _ -> WithHosts
    end.

%% Inline PEM is parsed to in-memory DER, so the private key is never written to
%% disk. File paths are passed through to ssl as `certfile'/`keyfile'.
entry_opts(#{ <<"cert">> := CertPem, <<"key">> := KeyPem }) ->
    CertDERs = [Der || {'Certificate', Der, _} <- public_key:pem_decode(CertPem)],
    [{KeyTag, KeyDer, _} | _] = public_key:pem_decode(KeyPem),
    [{cert, CertDERs}, {key, {KeyTag, KeyDer}}];
entry_opts(#{ <<"certfile">> := CertFile, <<"keyfile">> := KeyFile }) ->
    [{certfile, binary_to_list(CertFile)}, {keyfile, binary_to_list(KeyFile)}].

%% TLS version floor: 1.2 and 1.3 by default, or 1.3 only when requested.
versions(Tls) ->
    case maps:get(<<"min-version">>, Tls, <<"tlsv1.2">>) of
        <<"tlsv1.3">> -> ['tlsv1.3'];
        _ -> ['tlsv1.2', 'tlsv1.3']
    end.

%%% Tests

%% @doc Boot a node with an inline-PEM TLS config and confirm it terminates TLS
%% as HTTP/2 (serving a real request through the pipeline) and refuses HTTP/1.1.
tls_node_test() ->
    {ok, CertPem} = file:read_file("test/test-tls.pem"),
    {ok, KeyPem} = file:read_file("test/test-tls.key"),
    URL =
        hb_http_server:start_node(#{
            <<"tls">> => #{
                <<"certs">> => [#{
                    <<"domains">> => [<<"localhost">>],
                    <<"cert">> => CertPem,
                    <<"key">> => KeyPem
                }]
            }
        }),
    Port = url_port(URL),
    %% An HTTP/2 client is served through the normal pipeline.
    {ok, Conn} =
        gun:open("localhost", Port,
            #{transport => tls,
              tls_opts => [{verify, verify_none}],
              protocols => [http2]}),
    {ok, http2} = gun:await_up(Conn, 5000),
    StreamRef = gun:get(Conn, "/~meta@1.0/info"),
    {response, _, Status, _} = gun:await(Conn, StreamRef, 5000),
    gun:close(Conn),
    ?assertEqual(200, Status),
    %% An http/1.1-only client is refused at the TLS layer (ALPN mismatch).
    ?assertMatch({error, _},
        ssl:connect("127.0.0.1", Port,
            [{verify, verify_none},
             {server_name_indication, "localhost"},
             {alpn_advertised_protocols, [<<"http/1.1">>]}], 2000)).

url_port(URL) ->
    [_, AfterHost] = binary:split(URL, <<"localhost:">>),
    [PortBin | _] = binary:split(AfterHost, <<"/">>),
    binary_to_integer(PortBin).
