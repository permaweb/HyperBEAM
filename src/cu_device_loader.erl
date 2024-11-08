-module(cu_device_loader).
-export([from_id/1, from_id/2, from_message/1,default/0]).

-include("include/ao.hrl").

%%% A module for handling the loading and execution of device modules inside
%%% the Erlang environments.

from_id(ID) -> from_id(ID, #{}).

from_id({ID, Func}, Opts) ->
    case from_id(ID, Opts) of
        {ok, Mod} -> {ok, {Mod, Func}};
        Error -> Error
    end;
from_id(ID, _Opts) when is_atom(ID) ->
    try ID:module_info(), {ok, ID}
    catch _:_ -> {error, not_loadable}
    end;
from_id(ID, _Opts) when is_binary(ID) and byte_size(ID) == 43 ->
    case lists:member(ID, ao:get(loadable_devices)) of
        true ->
            Msg = ao_client:download(ID),
            RelBin = erlang:system_info(otp_release),
            case lists:keyfind(<<"Content-Type">>, 1, Msg#tx.tags) of
                <<"BEAM/", RelBin/bitstring>> ->
                    {_, ModNameBin} = lists:keyfind(<<"Module-Name">>, 1, Msg#tx.tags),
                    ModName = list_to_atom(binary_to_list(ModNameBin)),
                    case erlang:load_module(ModName, Msg#tx.data) of
                        {module, _} -> {ok, ModName};
                        {error, Reason} -> {error, Reason}
                    end
            end;
        false -> {error, module_not_admissable}
    end;
from_id(ID, _Opts) ->
    case maps:get(ID, ao:get(preloaded_devices), unsupported) of
        unsupported -> {error, module_not_admissable};
        Mod -> {ok, Mod}
    end.

default() -> dev_identity.

%% @doc Locate the appropriate device module to execute for the given message.
%% If the message specifies a device for itself, use that. Else, use the default
%% device.
from_message(M) ->
    case lists:keyfind(<<"Device">>, 1, M#tx.tags) of
        {_, DeviceID} ->
            from_id(DeviceID);
        false ->
            {ok, default()}
    end.
