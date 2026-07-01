%%% @doc A device that renews the node's TLS certificates in-node via ACME.
%%% Reads the stored ACME config (hb_acme:config/0), and for each domain whose
%%% installed certificate is within `renew-before-days' of expiry (or missing),
%%% obtains a fresh certificate over HTTP-01 and installs it live with
%%% hb_tls:install/3. Intended to be driven on a schedule (e.g. by ~cron@1.0).
-module(dev_cert).
-export([renew/3, info/1, info/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Exported function for getting device info.
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

%% @doc Renew every configured domain that is due. Each domain is wrapped so one
%% failure does not abort the rest; the result body maps domain => outcome.
renew(_Base, _Req, Opts) ->
    case hb_acme:config() of
        undefined ->
            {ok, #{<<"status">> => 200, <<"body">> => <<"no acme configured">>}};
        AcmeCfg ->
            Domains = maps:get(<<"domains">>, AcmeCfg, []),
            RenewBeforeDays = maps:get(<<"renew-before-days">>, AcmeCfg, 30),
            Results =
                lists:foldl(
                    fun(Domain, Acc) ->
                        Acc#{ Domain => renew_domain(Domain, RenewBeforeDays, AcmeCfg, Opts) }
                    end,
                    #{},
                    Domains
                ),
            {ok, #{<<"status">> => 200, <<"body">> => #{<<"results">> => Results}}}
    end.

%% Renew a single domain when its cert is missing or within RenewBeforeDays of
%% expiry, otherwise skip it. Any crash is caught and reported as an error.
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
    ObtainCfg = #{
        <<"domains">> => [Domain],
        <<"email">> => maps:get(<<"email">>, AcmeCfg, <<>>),
        <<"directory_url">> => maps:get(<<"directory_url">>, AcmeCfg, <<>>),
        <<"publish">> => fun hb_acme_http:publish/2,
        <<"unpublish">> => fun hb_acme_http:unpublish/1,
        <<"http_opts">> => maps:get(<<"http_opts">>, AcmeCfg, [])
    },
    case hb_acme:obtain(ObtainCfg, Opts) of
        {ok, #{<<"cert">> := C, <<"key">> := K}} ->
            ok = hb_tls:install(Domain, C, K),
            renewed;
        {error, R} ->
            {error, R}
    end.
