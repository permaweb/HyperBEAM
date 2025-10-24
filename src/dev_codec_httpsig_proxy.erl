%%% @doc A utility module that contains proxy functions for calling the
%%% `~httpsig@1.0' codec's HMAC commitment functions with secret keys.
%%% 
%%% These tools are helpful for implementing a standardized pattern:
%%% 1. A device verifies a user's request/derives a secret key for them.
%%% 2. The device then wants to commit a message with the user's secret key
%%%    using the `secret:[h(secret)]' commitment scheme.
%%% 3. The commitment must then be modified to reference a different device
%%%    as the `commitment-device' key.
%%% 4. When `/verify' is called, the `~httpsig@1.0' codec is used under-the-hood
%%%    to validate the commitment on the re-derived secret key.
%%% 
%%% This module is currently used by the `~cookie@1.0' and `~http-auth@1.0'
%%% devices.
-module(dev_codec_httpsig_proxy).
-export([commit/5, verify/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Commit to a given `Base' message with a given `Secret', setting the 
%% `commitment-device' key to `Device' afterwards.
commit(Device, Secret, Base, Req, Opts) ->
    % If there are no existing commitments, we use the unmodified base message.
    % If there are, we remove the uncommitted parts of the message.
    ExistingComms = hb_maps:get(<<"commitments">>, Base, #{}, Opts),
    OnlyCommittedBase =
        case map_size(ExistingComms) of
            0 -> Base;
            _ ->
                {ok, CommittedBase} = hb_message:with_only_committed(Base, Opts),
                hb_message:uncommitted(CommittedBase, Opts)
        end,
    % Commit the message with the given key.
    CommittedMsg =
        hb_message:commit(
            OnlyCommittedBase,
            Opts,
            Req#{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"type">> => <<"hmac-sha256">>,
                <<"scheme">> => <<"secret">>,
                <<"secret">> => Secret
            }
        ),
    {ok, CommitmentID, Commitment} =
        hb_message:commitment(
            #{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"type">> => <<"hmac-sha256">>
            },
            CommittedMsg,
            Opts
        ),
    % Modify the commitment device to the given device.
    ModCommittedMsg =
        CommittedMsg#{
            <<"commitments">> =>
                ExistingComms#{
                    CommitmentID =>
                        Commitment#{
                            <<"commitment-device">> => Device
                        }
                }
        },
    ?event({cookie_commitment, {id, CommitmentID}, {commitment, ModCommittedMsg}}),
    {ok, ModCommittedMsg}.

%% @doc Verify a given `Base' message with a given `Secret' using the `~httpsig@1.0'
%% HMAC commitment scheme.
verify(Secret, Base, RawReq, Opts) ->
    ProxyRequest =
        RawReq#{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"path">> => <<"verify">>,
            <<"secret">> => Secret
        },
    ?event({proxy_request, ProxyRequest}),
    {ok, hb_message:verify(Base, ProxyRequest, Opts)}.