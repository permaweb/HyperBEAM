%%% @doc TLS policy and node-wallet key adapter.
-module(hb_tls).
-export([config/1, certificate_expiry/1, install/3, socket_options/2]).
-include_lib("public_key/include/public_key.hrl").

-define(UNIX_EPOCH, 62167219200).

config(NodeMsg) ->
    case hb_opts:get(tls, false, NodeMsg) of
        false -> false;
        TLS ->
            case hb_cache:ensure_all_loaded(TLS, NodeMsg) of
                Config when is_map(Config) -> Config;
                Invalid -> error({'invalid-tls-config', Invalid})
            end
    end.

%% @doc Replace a listener's leaf without dropping established connections.
install(ServerID, Wallet, Chain) ->
    case socket_options(Wallet, Chain) of
        {error, _} = Error -> Error;
        {ok, TLSOpts} ->
            try
                TransportOpts = ranch:get_transport_options(ServerID),
                SocketOpts = maps:get(socket_opts, TransportOpts, []),
                Keep = lists:foldl(
                    fun(Key, Opts) -> lists:keydelete(Key, 1, Opts) end,
                    SocketOpts,
                    [port, certs_keys]
                ),
                NewOpts = TransportOpts#{socket_opts =>
                    [{port, ranch:get_port(ServerID)} | TLSOpts ++ Keep]},
                ok = ranch:suspend_listener(ServerID),
                try
                    ok = ranch:set_transport_options(ServerID, NewOpts),
                    ok = ranch:resume_listener(ServerID)
                catch
                    Class:Reason:Stack ->
                        ranch:resume_listener(ServerID),
                        erlang:raise(Class, Reason, Stack)
                end
            catch
                _:UpdateReason ->
                    {error, {'tls-listener-update-failed', UpdateReason}}
            end
    end.

%% @doc Millisecond Unix expiry of the leaf certificate.
certificate_expiry([Leaf | _]) ->
    Certificate = public_key:pkix_decode_cert(Leaf, otp),
    TBS = Certificate#'OTPCertificate'.tbsCertificate,
    certificate_time(TBS#'OTPTBSCertificate'.validity#'Validity'.notAfter).

certificate_time({generalTime, Time}) ->
    certificate_time(Time);
certificate_time({utcTime, [Y1, Y2 | Rest]}) ->
    Century = case [Y1, Y2] >= "50" of true -> "19"; false -> "20" end,
    certificate_time(Century ++ [Y1, Y2 | Rest]);
certificate_time(Time) ->
    {ok, [Y, M, D, H, I, S], _} =
        io_lib:fread("~4d~2d~2d~2d~2d~2d", Time),
    1000 * (calendar:datetime_to_gregorian_seconds(
        {{Y, M, D}, {H, I, S}}
    ) - ?UNIX_EPOCH).

%% @doc Build SSL options only when the leaf carries the exact wallet key.
socket_options({{{rsa, E}, D, N}, {{rsa, E}, N}}, [Leaf | _] = Chain) ->
    try
        Certificate = public_key:pkix_decode_cert(Leaf, otp),
        TBS = Certificate#'OTPCertificate'.tbsCertificate,
        SPKI = TBS#'OTPTBSCertificate'.subjectPublicKeyInfo,
        true = SPKI#'OTPSubjectPublicKeyInfo'.subjectPublicKey =:=
            rsa_public_key(E, N),
        Sign = fun(Data, Digest, Options) ->
            crypto:sign(rsa, Digest, Data,
                [E, binary:decode_unsigned(N), binary:decode_unsigned(D)],
                Options)
        end,
        {ok, [{certs_keys, [#{
            cert => Chain,
            key => #{algorithm => rsa, sign_fun => Sign}
        }]}]}
    catch
        error:{badmatch, false} -> {error, 'certificate-key-mismatch'};
        _:_ -> {error, 'invalid-certificate-chain'}
    end;
socket_options(_, _) ->
    {error, 'unsupported-tls-wallet'}.

rsa_public_key(E, N) ->
    #'RSAPublicKey'{
        publicExponent = E,
        modulus = binary:decode_unsigned(N)
    }.
