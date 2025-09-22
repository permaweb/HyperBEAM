%%% @doc A library for working with HyperBEAM-compatible AO-Core devices.
%%% Offers services for loading, verifying executability, and extracting Erlang
%%% functions from a device.
-module(hb_ao_device).
-export([truncate_args/2, message_to_fun/3, message_to_device/2, load/2]).
-export([is_direct_key_access/3, is_direct_key_access/4]).
-export([find_exported_function/5, is_exported/4, info/2, info/3, default/0]).
-include("include/hb.hrl").

%%% All keys in the `message@1.0` device that are not resolved to underlying
%%% data in the their Erlang map representations.
-define(MESSAGE_KEYS, [
    <<"get">>,
    <<"set">>,
    <<"remove">>,
    <<"keys">>,
    <<"id">>,
    <<"commit">>,
    <<"verify">>,
    <<"committers">>,
    <<"committed">>
]).

%% @doc Truncate the arguments of a function to the number of arguments it
%% actually takes.
truncate_args(Fun, Args) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    lists:sublist(Args, Arity).

%% @doc Calculate the Erlang function that should be called to get a value for
%% a given key from a device.
%%
%% This comes in 7 forms:
%% 1. The message does not specify a device, so we use the default device.
%% 2. The device has a `handler' key in its `Dev:info()' map, which is a
%% function that takes a key and returns a function to handle that key. We pass
%% the key as an additional argument to this function.
%% 3. The device has a function of the name `Key', which should be called
%% directly.
%% 4. The device does not implement the key, but does have a default handler
%% for us to call. We pass it the key as an additional argument.
%% 5. The device does not implement the key, and has no default handler. We use
%% the default device to handle the key.
%% Error: If the device is specified, but not loadable, we raise an error.
%%
%% Returns {ok | add_key, Fun} where Fun is the function to call, and add_key
%% indicates that the key should be added to the start of the call's arguments.
message_to_fun(Msg, Key, Opts) ->
    % Get the device module from the message.
	Dev = message_to_device(Msg, Opts),
    Info = info(Dev, Msg, Opts),
    % Is the key exported by the device?
    Exported = is_exported(Info, Key, Opts),
	?event(
        ao_devices,
        {message_to_fun,
            {dev, Dev},
            {key, Key},
            {is_exported, Exported},
            {opts, Opts}
        },
		Opts
    ),
    % Does the device have an explicit handler function?
    case {hb_maps:find(handler, Info, Opts), Exported} of
        {{ok, Handler}, true} ->
			% Case 2: The device has an explicit handler function.
			?event(
                ao_devices,
                {handler_found, {dev, Dev}, {key, Key}, {handler, Handler}}
            ),
			{Status, Func} = info_handler_to_fun(Handler, Msg, Key, Opts),
            {Status, Dev, Func};
		_ ->
			?event(ao_devices, {no_override_handler, {dev, Dev}, {key, Key}}),
			case {find_exported_function(Msg, Dev, Key, 3, Opts), Exported} of
				{{ok, Func}, true} ->
					% Case 3: The device has a function of the name `Key'.
					{ok, Dev, Func};
				_ ->
					case {hb_maps:find(default, Info, Opts), Exported} of
						{{ok, DefaultFunc}, true} when is_function(DefaultFunc) ->
							% Case 4: The device has a default handler.
                            ?event({found_default_handler, {func, DefaultFunc}}),
							{add_key, Dev, DefaultFunc};
                        {{ok, DefaultMod}, true} when is_atom(DefaultMod) ->
							?event({found_default_handler, {mod, DefaultMod}}),
                            {Status, Func} =
                                message_to_fun(
                                    Msg#{ <<"device">> => DefaultMod }, Key, Opts
                                ),
                            {Status, Dev, Func};
						_ ->
							% Case 5: The device has no default handler.
							% We use the default device to handle the key.
							case default() of
								Dev ->
									% We are already using the default device,
									% so we cannot resolve the key. This should
									% never actually happen in practice, but it
									% resolves an infinite loop that can occur
									% during development.
									throw({
										error,
										default_device_could_not_resolve_key,
										{key, Key}
									});
								DefaultDev ->
                                    ?event(
                                        {
                                            using_default_device,
                                            {dev, DefaultDev}
                                        }),
                                    message_to_fun(
                                        Msg#{ <<"device">> => DefaultDev },
                                        Key,
                                        Opts
                                    )
							end
					end
			end
	end.

%% @doc Extract the device module from a message.
message_to_device(Msg, Opts) ->
    case dev_message:get(<<"device">>, Msg, Opts) of
        {error, not_found} ->
            % The message does not specify a device, so we use the default device.
            default();
        {ok, DevID} ->
            case load(DevID, Opts) of
                {error, Reason} ->
                    % Error case: A device is specified, but it is not loadable.
                    throw({error, {device_not_loadable, DevID, Reason}});
                {ok, DevMod} -> DevMod
            end
    end.

%% @doc Parse a handler key given by a device's `info'.
info_handler_to_fun(Handler, _Msg, _Key, _Opts) when is_function(Handler) ->
	{add_key, Handler};
info_handler_to_fun(HandlerMap, Msg, Key, Opts) ->
	case hb_maps:find(excludes, HandlerMap, Opts) of
		{ok, Exclude} ->
			case lists:member(Key, Exclude) of
				true ->
					{ok, MsgWithoutDevice} =
						dev_message:remove(Msg, #{ item => device }, Opts),
					message_to_fun(
						MsgWithoutDevice#{ <<"device">> => default() },
						Key,
						Opts
					);
				false -> {add_key, hb_maps:get(func, HandlerMap, undefined, Opts)}
			end;
		error -> {add_key, hb_maps:get(func, HandlerMap, undefined, Opts)}
	end.

%% @doc Find the function with the highest arity that has the given name, if it
%% exists.
%%
%% If the device is a module, we look for a function with the given name.
%%
%% If the device is a map, we look for a key in the map. First we try to find
%% the key using its literal value. If that fails, we cast the key to an atom
%% and try again.
find_exported_function(Msg, Dev, Key, MaxArity, Opts) when is_map(Dev) ->
    NormKey = hb_ao:normalize_key(Key),
    NormDev = hb_ao:normalize_keys(Dev, Opts),
	case hb_maps:get(NormKey, NormDev, not_found, Opts) of
		not_found -> not_found;
		Fun when is_function(Fun) ->
			case erlang:fun_info(Fun, arity) of
				{arity, Arity} when Arity =< MaxArity ->
					case is_exported(Msg, Dev, Key, Opts) of
						true -> {ok, Fun};
						false -> not_found
					end;
				_ -> not_found
			end
	end;
find_exported_function(_Msg, _Mod, _Key, Arity, _Opts) when Arity < 0 ->
    not_found;
find_exported_function(Msg, Mod, Key, Arity, Opts) when not is_atom(Key) ->
	try hb_util:key_to_atom(Key, false) of
		KeyAtom -> find_exported_function(Msg, Mod, KeyAtom, Arity, Opts)
	catch _:_ -> not_found
	end;
find_exported_function(Msg, Mod, Key, Arity, Opts) ->
	case erlang:function_exported(Mod, Key, Arity) of
		true ->
			case is_exported(Msg, Mod, Key, Opts) of
				true -> {ok, fun Mod:Key/Arity};
				false -> not_found
			end;
		false ->
			find_exported_function(Msg, Mod, Key, Arity - 1, Opts)
	end.

%% @doc Check if a device is guarding a key via its `exports' list. Defaults to
%% true if the device does not specify an `exports' list. The `info' function is
%% always exported, if it exists. Elements of the `exludes' list are not
%% exported. Note that we check for info _twice_ -- once when the device is
%% given but the info result is not, and once when the info result is given.
%% The reason for this is that `info/3' calls other functions that may need to
%% check if a key is exported, so we must avoid infinite loops. We must, however,
%% also return a consistent result in the case that only the info result is
%% given, so we check for it in both cases.
is_exported(_Msg, _Dev, info, _Opts) -> true;
is_exported(Msg, Dev, Key, Opts) ->
	is_exported(info(Dev, Msg, Opts), Key, Opts).
is_exported(_, info, _Opts) -> true;
is_exported(Info = #{ excludes := Excludes }, Key, Opts) ->
    NormKey = hb_ao:normalize_key(Key),
    case lists:member(NormKey, lists:map(fun hb_ao:normalize_key/1, Excludes)) of
        true -> false;
        false -> is_exported(hb_maps:remove(excludes, Info, Opts), Key, Opts)
    end;
is_exported(#{ exports := Exports }, Key, _Opts) ->
    lists:member(
        hb_ao:normalize_key(Key),
        lists:map(fun hb_ao:normalize_key/1, Exports)
    );
is_exported(_Info, _Key, _Opts) -> true.

%% @doc Load a device module from its name or a message ID.
%% Returns {ok, Executable} where Executable is the device module. On error,
%% a tuple of the form {error, Reason} is returned.
load(Map, _Opts) when is_map(Map) -> {ok, Map};
load(ID, _Opts) when is_atom(ID) ->
    try ID:module_info(), {ok, ID}
    catch _:_ -> {error, not_loadable}
    end;
load(ID, Opts) when ?IS_ID(ID) ->
    ?event(device_load, {requested_load, {id, ID}}, Opts),
	case hb_opts:get(load_remote_devices, false, Opts) of
        false ->
            {error, remote_devices_disabled};
		true ->
            ?event(device_load, {loading_from_cache, {id, ID}}, Opts),
			{ok, Msg} = hb_cache:read(ID, Opts),
            ?event(device_load, {received_device, {id, ID}, {msg, Msg}}, Opts),
            TrustedSigners = hb_opts:get(trusted_device_signers, [], Opts),
			Trusted =
				lists:any(
					fun(Signer) ->
						lists:member(Signer, TrustedSigners)
					end,
					hb_message:signers(Msg, Opts)
				),
            ?event(device_load,
                {verifying_device_trust,
                    {id, ID},
                    {trusted, Trusted},
                    {signers, hb_message:signers(Msg, Opts)}
                },
                Opts
            ),
			case Trusted of
				false -> {error, device_signer_not_trusted};
				true ->
                    ?event(device_load, {loading_device, {id, ID}}, Opts),
					case hb_maps:get(<<"content-type">>, Msg, undefined, Opts) of
						<<"application/beam">> ->
                            case verify_device_compatibility(Msg, Opts) of
                                ok ->
                                    ModName =
                                        hb_util:key_to_atom(
                                            hb_maps:get(
                                                <<"module-name">>,
                                                Msg,
                                                undefined,
                                                Opts
                                            ),
                                            new_atoms
                                        ),
                                    LoadRes = 
                                        erlang:load_module(
                                            ModName,
                                            hb_maps:get(
                                                <<"body">>,
                                                Msg,
                                                undefined,
                                                Opts
                                            )
                                        ),
                                    case LoadRes of
                                        {module, _} ->
                                            {ok, ModName};
                                        {error, Reason} ->
                                            {error, {device_load_failed, Reason}}
                                    end;
                                {error, Reason} ->
                                    {error, {device_load_failed, Reason}}
                            end;
                        Other ->
                            {error,
                                {device_load_failed,
                                    {incompatible_content_type, Other},
                                    {expected, <<"application/beam">>},
                                    {found, Other}
                                }
                            }
                    end
			end
	end;
load(ID, Opts) ->
    NormKey =
        case is_atom(ID) of
            true -> ID;
            false -> hb_ao:normalize_key(ID)
        end,
    case lists:search(
        fun (#{ <<"name">> := Name }) -> Name =:= NormKey end,
        Preloaded = hb_opts:get(preloaded_devices, [], Opts)
    ) of
        false -> {error, {module_not_admissable, NormKey, Preloaded}};
        {value, #{ <<"module">> := Mod }} -> load(Mod, Opts)
    end.

%% @doc Verify that a device is compatible with the current machine.
verify_device_compatibility(Msg, Opts) ->
    ?event(device_load, {verifying_device_compatibility, {msg, Msg}}, Opts),
    Required =
        lists:filtermap(
            fun({<<"requires-", Key/binary>>, Value}) ->
                {true,
                    {
                        hb_util:key_to_atom(
                            hb_ao:normalize_key(Key),
                            new_atoms
                        ),
                        hb_cache:ensure_loaded(Value, Opts)
                    }
                };
            (_) -> false
            end,
            hb_maps:to_list(Msg, Opts)
        ),
    ?event(device_load,
        {discerned_requirements,
            {required, Required},
            {msg, Msg}
        },
        Opts
    ),
    FailedToMatch =
        lists:filtermap(
            fun({Property, Value}) ->
                % The values of these properties are _not_ 'keys', but we normalize
                % them as such in order to make them comparable.
                SystemValue = erlang:system_info(Property),
                Res = hb_ao:normalize_key(SystemValue) == hb_ao:normalize_key(Value),
                % If the property matched, we remove it from the list of required
                % properties. If it doesn't we return it with the found value, such
                % that the caller knows which properties were not satisfied.
                case Res of
                    true -> false;
                    false -> {true, {Property, Value}}
                end
            end,
            Required
        ),
    case FailedToMatch of
        [] -> ok;
        _ -> {error, {failed_requirements, FailedToMatch}}
    end.

%% @doc Get the info map for a device, optionally giving it a message if the
%% device's info function is parameterized by one.
info(Msg, Opts) ->
    info(message_to_device(Msg, Opts), Msg, Opts).
info(DevMod, Msg, Opts) ->
	%?event({calculating_info, {dev, DevMod}, {msg, Msg}}),
    case find_exported_function(Msg, DevMod, info, 2, Opts) of
		{ok, Fun} ->
			Res = apply(Fun, truncate_args(Fun, [Msg, Opts])),
			% ?event({
            %     info_result,
            %     {dev, DevMod},
            %     {args, truncate_args(Fun, [Msg])},
            %     {result, Res}
            % }),
			Res;
		not_found -> #{}
	end.

%% @doc Determine if a device is a `direct access': If there is a literal key
%% in the message's Erlang map representation, will it always be returned?
is_direct_key_access(Base, Req, Opts) ->
    is_direct_key_access(Base, Req, Opts, unknown).
is_direct_key_access(Base, Req, Opts, MaybeStore) when ?IS_ID(Base) ->
    Store =
        if MaybeStore =:= unknown -> hb_opts:get(store, no_viable_store, Opts);
        true -> MaybeStore
        end,
    DevPath = hb_store:resolve(Store, [Base, <<"device">>]),
    case hb_store:read(Store, DevPath) of
        {ok, Dev} ->
            do_is_direct_key_access(Dev, Req, Opts);
        not_found ->
            case hb_store:type(Store, Base) of
                not_found -> unknown;
                _ -> do_is_direct_key_access(<<"message@1.0">>, Req, Opts)
            end
    end;
is_direct_key_access(Base, Req, Opts, _) when is_map(Base) ->
    do_is_direct_key_access(hb_maps:find(<<"device">>, Base, Opts), Req, Opts).

do_is_direct_key_access(DevRes, #{ <<"path">> := Key }, Opts) ->
    do_is_direct_key_access(DevRes, Key, Opts);
do_is_direct_key_access({_Status, DevRes}, Key, Opts) ->
    do_is_direct_key_access(DevRes, Key, Opts);
do_is_direct_key_access(not_found, Key, Opts) ->
    do_is_direct_key_access(<<"message@1.0">>, Key, Opts);
do_is_direct_key_access(error, Key, Opts) ->
    do_is_direct_key_access(<<"message@1.0">>, Key, Opts);
do_is_direct_key_access(<<"message@1.0">>, Key, _Opts) ->
    not lists:member(Key, ?MESSAGE_KEYS);
do_is_direct_key_access(Dev, NormKey, Opts) ->
    ?event(read_cached, {calculating_info, {device, Dev}}),
    case info(#{ <<"device">> => Dev}, Opts) of
        Info = #{ exports := Exports }
            when not is_map_key(handler, Info) andalso not is_map_key(default, Info) ->
            ?event(read_cached,
                {exports,
                    {device, Dev},
                    {key, NormKey},
                    {exports, Exports}
                }
            ),
            not lists:member(NormKey, Exports ++ ?MESSAGE_KEYS);
        _ -> false
    end;
do_is_direct_key_access(_, _, _) ->
    false.

%% @doc The default device is the identity device, which simply returns the
%% value associated with any key as it exists in its Erlang map. It should also
%% implement the `set' key, which returns a `Message3' with the values changed
%% according to the `Message2' passed to it.
default() -> dev_message.