%%% @doc In-node ACME certificate renewal (`~cert@1.0'), driven on a schedule
%%% by ~cron@1.0. For each domain in `tls.acme' whose installed certificate is
%%% within `renew-before-days' of expiry (or missing), obtains a fresh
%%% certificate over HTTP-01 and installs it live with hb_tls:install/3.
-module(dev_cert).
-export([renew/3, info/1, info/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

info(_) ->
    #{ exports => [<<"info">>, <<"renew">>] }.

info(_Base, _Req, _Opts) ->
    {ok, #{
        <<"status">> => 200,
        <<"body">> => #{
            <<"description">> => <<"In-node ACME certificate renewal">>,
            <<"version">> => <<"1.0">>
        }
    }}.

%% @doc Renew every configured domain that is due. Each domain is wrapped so
%% one failure does not abort the rest; the body maps domain => outcome.
renew(_Base, _Req, Opts) ->
    Tls = hb_maps:get(<<"tls">>, Opts, #{}, Opts),
    case hb_maps:get(<<"acme">>, Tls, undefined, Opts) of
        undefined ->
            {ok, #{<<"status">> => 200, <<"body">> => <<"no acme configured">>}};
        AcmeCfg ->
            Domains = maps:get(<<"domains">>, AcmeCfg, []),
            RenewBeforeDays = maps:get(<<"renew-before-days">>, AcmeCfg, 30),
            Results =
                maps:from_list([
                    {Domain, renew_domain(Domain, RenewBeforeDays, AcmeCfg, Opts)}
                 || Domain <- Domains
                ]),
            {ok, #{<<"status">> => 200, <<"body">> => #{<<"results">> => Results}}}
    end.

%% Renew when the cert is missing or within RenewBeforeDays of expiry.
renew_domain(Domain, RenewBeforeDays, AcmeCfg, Opts) ->
    try
        case hb_tls:expiry(Domain) of
            undefined -> obtain_and_install(Domain, AcmeCfg, Opts);
            DaysLeft when DaysLeft =< RenewBeforeDays ->
                obtain_and_install(Domain, AcmeCfg, Opts);
            DaysLeft -> {skipped, DaysLeft}
        end
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

obtain_and_install(Domain, AcmeCfg, Opts) ->
    ObtainCfg = AcmeCfg#{
        <<"domains">> => [Domain],
        <<"publish">> => fun hb_acme_http:publish/2,
        <<"unpublish">> => fun hb_acme_http:unpublish/1
    },
    case hb_acme:obtain(ObtainCfg, Opts) of
        {ok, #{<<"cert">> := C, <<"key">> := K}} ->
            ok = hb_tls:install(Domain, C, K),
            renewed;
        {error, R} ->
            {error, R}
    end.
