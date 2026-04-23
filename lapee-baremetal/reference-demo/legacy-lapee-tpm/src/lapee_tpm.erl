%%%-------------------------------------------------------------------
%%% @doc lapee_tpm — Erlang-friendly API over lapee_tpm_nif.
%%%-------------------------------------------------------------------
-module(lapee_tpm).

-export([
    startup/0,
    pcr_read/1,
    pcr_extend/2,
    create_ek/0,
    create_signing_key/1,
    quote/3,
    sign/2,
    flush/1
]).

startup() ->
    lapee_tpm_nif:startup().

pcr_read(Idx) when is_integer(Idx), Idx >= 0, Idx =< 23 ->
    lapee_tpm_nif:pcr_read(Idx).

pcr_extend(Idx, Bin) when is_integer(Idx), Idx >= 0, Idx =< 23, is_binary(Bin) ->
    lapee_tpm_nif:pcr_extend(Idx, Bin).

create_ek() ->
    lapee_tpm_nif:create_primary_ek().

create_signing_key(ParentHandle) when is_integer(ParentHandle) ->
    lapee_tpm_nif:create_signing_key(ParentHandle).

quote(SignHandle, PcrList, Nonce) when is_integer(SignHandle),
                                       is_list(PcrList),
                                       is_binary(Nonce) ->
    lapee_tpm_nif:quote(SignHandle, PcrList, Nonce).

sign(SignHandle, Message) when is_integer(SignHandle), is_binary(Message) ->
    lapee_tpm_nif:sign(SignHandle, Message).

flush(Handle) when is_integer(Handle) ->
    lapee_tpm_nif:flush_context(Handle).
