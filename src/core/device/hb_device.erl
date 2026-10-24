%%% @doc A library for working with HyperBEAM-compatible AO-Core devices.
%%% Offers services for loading, verifying executability, and extracting Erlang
%%% functions from a device.
-module(hb_device).
-export([truncate_args/2, message_to_fun/3, message_to_device/2]).
-export([is_direct_key_access/3, is_direct_key_access/4]).
-export([find_exported_function/5, is_exported/4]).
-export([device_info/2, device_info/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_DEVICE, <<"message@1.0">>).

%%% All keys in the `message@1.0` device that are not resolved to underlying
%%% data in the their Erlang map representations.
-define(MESSAGE_KEYS, [
    <<"index">>,
    <<"info">>,
    <<"set">>,
    <<"set-path">>,
    <<"set_path">>,
    <<"remove">>,
    <<"keys">>,
    <<"id">>,
    <<"commit">>,
    <<"verify">>,
    <<"with-commitments">>,
    <<"with_commitments">>,
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
%% 2. The device has a `handler' key in its `Dev:device_info()' map, which is a
%% function that takes a key and returns a function to handle that key. We pass
%% the key as an additional argument to this function:
%%     `Mod:Handler(Key, Base, Req, Opts) -> {Status, Fun}'
%% 3. The device has a function of the name `Key', which should be called
%% directly.
%% 4. The device does not implement the key, but does have a default function
%% for us to call. We pass it the key as an additional argument, as with (2).
%% `default' differs from `handler' in that it only matches for keys where the
%% module exports no function of the given name.
%% 5. The device has a `default' key with a device or module name as its value.
%% We use this device to handle the key, restarting the process of resolving the
%% key to a function.
%% 6. The device does not implement the key and states no defaults. We use the
%% global default device to handle the key.
%% Error: If the device is specified, but not loadable, we raise an error.
%%
%% Returns {ok | add_key, Fun} where Fun is the function to call, and add_key
%% indicates that the key should be added to the start of the call's arguments.
message_to_fun(Msg, Key, Opts) ->
    % Get the device module from the message and recurse.
    message_to_fun(message_to_device(Msg, Opts), Msg, Key, Opts).
message_to_fun(Dev, Msg, Key, Opts) ->
    Info = device_info(Dev, Msg, Opts),
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
			?event_debug(ao_devices, {no_override_handler, {dev, Dev}, {key, Key}}),
			case {find_exported_function(Msg, Dev, Key, 3, 1, Opts), Exported} of
				{{ok, Func}, true} ->
					% Case 3: The device has a function of the name `Key'.
					{ok, Dev, Func};
				_ ->
					case {hb_maps:find(default, Info, Opts), Exported} of
						{{ok, DefaultFunc}, true} when is_function(DefaultFunc) ->
							% Case 4: The device has a default handler.
                            ?event_debug({found_default_handler, {func, DefaultFunc}}),
							{add_key, Dev, DefaultFunc};
                        {{ok, DefaultDevice}, true} when is_binary(DefaultDevice)
                                orelse is_atom(DefaultDevice) ->
                            % Case 5: The device gives a specific further device
                            % to default to. Recurse with it and apply the same
                            % rules.
							?event_debug({found_default_device, {mod, DefaultDevice}}),
                            message_to_fun(
                                Msg#{ <<"device">> => DefaultDevice },
                                Key,
                                Opts
                            );
						_ ->
							% Case 6: The device has no default handler.
							% We retry with the default unless the message
							% already has it as its `device' (loop guard).
							case hb_maps:get(<<"device">>, Msg, undefined, Opts) of
								?DEFAULT_DEVICE ->
									throw({
										error,
										default_device_could_not_resolve_key,
										{key, Key}
									});
								_ ->
									?event_debug({using_default_device, ?DEFAULT_DEVICE}),
									message_to_fun(
										Msg#{ <<"device">> => ?DEFAULT_DEVICE },
										Key,
										Opts
									)
							end
					end
			end
	end.

%% @doc Extract the runtime device module from a message. When the
%% message has no `<<"device">>' key, we resolve the default
%% (`message@1.0') just like any other device: There is no privileged
%% internal module-loading path.
message_to_device(Msg, Opts) ->
    DevID = hb_maps:get(<<"device">>, Msg, ?DEFAULT_DEVICE, Opts),
    % Refuse a device in `exclude-devices' before `hb_device_load' resolves it.
    case lists:member(DevID, hb_opts:get(exclude_devices, [], Opts)) of
        true -> throw({device_not_loadable, DevID, <<"device-excluded">>});
        false -> ok
    end,
    case hb_device_load:reference(DevID, Opts) of
        {error, Reason} -> throw({device_not_loadable, DevID, Reason});
        {ok, DevMod} -> DevMod
    end.

%% @doc A device in `exclude-devices' is refused, with or without an
%% implementation in `trusted-devices'.
excluded_device_test() ->
    Ref = <<"excluded-test-device@1.0">>,
    Msg = #{ <<"device">> => Ref },
    Opts = #{ <<"exclude-devices">> => [Ref] },
    ?assertThrow(
        {device_not_loadable, Ref, <<"device-excluded">>},
        message_to_device(Msg, Opts)
    ),
    ?assertThrow(
        {device_not_loadable, Ref, <<"device-excluded">>},
        message_to_device(Msg, Opts#{
            <<"trusted-devices">> => #{ Ref => <<0:344>> }
        })
    ),
    % A node replies to a request for the device with a 403.
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        hb_http:get(
            hb_http_server:start_node(Opts),
            <<"/~", Ref/binary, "/key">>,
            #{}
        )
    ).

%% @doc A resolved device cannot bypass a subsequently applied exclusion.
excluded_cached_device_test() ->
    Ref = <<"message@1.0">>,
    Msg = #{ <<"device">> => Ref },
    Mod = message_to_device(Msg, #{}),
    ?assertEqual(Mod, message_to_device(Msg, #{ <<"exclude-devices">> => [] })),
    ?assertThrow(
        {device_not_loadable, Ref, <<"device-excluded">>},
        message_to_device(Msg, #{ <<"exclude-devices">> => [Ref] })
    ),
    ?assertThrow(
        {device_not_loadable, Ref, <<"device-excluded">>},
        message_to_device(#{}, #{ <<"exclude-devices">> => [Ref] })
    ),
    ?assertEqual(Mod, message_to_device(#{}, #{})).

%% @doc Parse a handler key given by a device's `device_info'.
info_handler_to_fun(Handler, _Msg, _Key, _Opts) when is_function(Handler) ->
	{add_key, Handler};
info_handler_to_fun(HandlerMap, Msg, Key, Opts) ->
	case hb_maps:find(excludes, HandlerMap, Opts) of
		{ok, Exclude} ->
			case lists:member(Key, Exclude) of
				true ->
					MsgWithoutDevice =
						hb_maps:without([<<"device">>], Msg, Opts),
					message_to_fun(
						MsgWithoutDevice#{ <<"device">> => ?DEFAULT_DEVICE },
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
find_exported_function(Msg, Mod, Key, Arity, Opts) ->
    find_exported_function(Msg, Mod, Key, Arity, 0, Opts).
find_exported_function(_Msg, _Dev, Key, _MaxArity, _MinArity, _Opts)
        when Key =:= device_info; Key =:= <<"device_info">> ->
    % `device_info' gives the kernel the options of a device: it is never a key
    % of the device.
    not_found;
find_exported_function(Msg, Dev, Key, MaxArity, MinArity, Opts) when is_map(Dev) ->
    NormKey = hb_ao:normalize_key(Key),
    NormDev = hb_ao:normalize_keys(Dev, Opts),
	case hb_maps:get(NormKey, NormDev, not_found, Opts) of
		not_found when not is_atom(Key) ->
			try hb_util:key_to_atom(Key, false) of
				KeyAtom -> find_exported_function(Msg, Dev, KeyAtom, MaxArity, MinArity, Opts)
			catch _:_ -> not_found
			end;
		not_found -> not_found;
		Fun when is_function(Fun) ->
			case erlang:fun_info(Fun, arity) of
				{arity, Arity} when Arity >= MinArity andalso Arity =< MaxArity ->
					case is_exported(Msg, Dev, Key, Opts) of
						true -> {ok, Fun};
						false -> not_found
					end;
				_ -> not_found
			end
	end;
find_exported_function(Msg, Mod, Key, Arity, MinArity, Opts) when not is_atom(Key) ->
	try hb_util:key_to_atom(Key, false) of
		KeyAtom -> find_exported_function(Msg, Mod, KeyAtom, Arity, MinArity, Opts)
	catch _:_ -> not_found
	end;
find_exported_function(_Msg, _Mod, _Key, Arity, MinArity, _Opts) when Arity < MinArity ->
    not_found;
find_exported_function(_Msg, _Mod, module_info, _Arity, _MinArity, _Opts) ->
    % The Erlang compiler exports `module_info' from every module: it is
    % never a key of a device.
    not_found;
find_exported_function(Msg, Mod, Key, Arity, MinArity, Opts) ->
	case erlang:function_exported(Mod, Key, Arity) of
		true ->
			case is_exported(Msg, Mod, Key, Opts) of
				true -> {ok, fun Mod:Key/Arity};
				false -> not_found
			end;
		false ->
			find_exported_function(Msg, Mod, Key, Arity - 1, MinArity, Opts)
	end.

%% @doc Check if a device is guarding a key via its `exports' list. Defaults to
%% true if the device does not specify an `exports' list. Elements of the
%% `excludes' list are not exported.
is_exported(Msg, Dev, Key, Opts) ->
	is_exported(device_info(Dev, Msg, Opts), Key, Opts).
is_exported(Info = #{ excludes := Excludes }, Key, Opts) ->
    NormKey = maybe_normalize_device_key(Key, existing),
    case lists:member(NormKey, lists:map(fun maybe_normalize_device_key/1, Excludes)) of
        true -> false;
        false -> is_exported(hb_maps:remove(excludes, Info, Opts), Key, Opts)
    end;
is_exported(#{ exports := Exports }, Key, _Opts) ->
    lists:member(
        maybe_normalize_device_key(Key, existing),
        lists:map(fun maybe_normalize_device_key/1, Exports)
    );
is_exported(_Info, _Key, _Opts) -> true.

%% @doc Normalize an exported key to its canonical atomized form. By default
%% new atoms are created if necessary. In practice this is used for keys that
%% originate from a device's `device_info', but _not_ for keys that could be
%% chosen by non-author users. This imparts a requirement that device developers
%% should not generate too many different exports/excludes -- just as they should
%% not generate too many atoms.
maybe_normalize_device_key(Key) -> maybe_normalize_device_key(Key, new_atoms).
maybe_normalize_device_key(Key, Mode) ->
    try hb_util:key_to_atom(hb_ao:normalize_key(Key), Mode)
    catch _:_ -> Key
    end.

%% @doc Get the options map of a device, optionally giving it a message if the
%% device's `device_info' function is parameterized by one. As `device_info' is
%% never a key, we read it from the device directly: a map device contains it
%% under `device_info', and a module exports it with an arity of 0 to 2.
device_info(Msg, Opts) -> device_info(message_to_device(Msg, Opts), Msg, Opts).
device_info(Dev, Msg, Opts) when is_map(Dev) ->
    NormDev = hb_ao:normalize_keys(Dev, Opts),
    case hb_maps:find(<<"device_info">>, NormDev, Opts) of
        {ok, Fun} -> apply(Fun, truncate_args(Fun, [Msg, Opts]));
        error -> #{}
    end;
device_info(Mod, Msg, Opts) ->
    case [N || N <- [2, 1, 0], erlang:function_exported(Mod, device_info, N)] of
        [Arity | _] ->
            apply(Mod, device_info, lists:sublist([Msg, Opts], Arity));
        [] -> #{}
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
    ReadOpts = Opts#{ <<"store">> => Store },
    case hb_cache:read(<<Base/binary, "/device">>, ReadOpts) of
        {ok, Dev} ->
            do_is_direct_key_access(Dev, Req, Opts);
        _ ->
            fallback_direct_key_access(Base, Req, ReadOpts)
    end;
is_direct_key_access(Base, Req, Opts, _) when is_map(Base) ->
    do_is_direct_key_access(hb_maps:find(<<"device">>, Base, Opts), Req, Opts).

%% @doc Determine whether a key can be read directly from a message that has no
%% `device' key in the stores. Such a message is read by `message@1.0', and an
%% ID that the stores do not hold is `unknown'. We first list one key under
%% the ID in the local stores. If there is none, we list the ID with
%% `hb_cache:list/2', which also follows a signed ID to its message.
fallback_direct_key_access(Base, Req, Opts) ->
    Local = hb_store:scope(hb_opts:get(store, no_viable_store, Opts), local),
    OneKey = #{ <<"list">> => Base, <<"limit">> => 1 },
    Listed =
        case hb_store:list(Local, OneKey, Opts) of
            {ok, [_ | _]} -> true;
            _ -> hb_cache:list(Base, Opts) =/= []
        end,
    case Listed of
        false -> unknown;
        true -> do_is_direct_key_access(<<"message@1.0">>, Req, Opts)
    end.

do_is_direct_key_access(DevRes, #{ <<"path">> := Key }, Opts) ->
    do_is_direct_key_access(DevRes, hb_util:lowercase_key(Key), Opts);
do_is_direct_key_access({_Status, DevRes}, Key, Opts) ->
    do_is_direct_key_access(DevRes, Key, Opts);
do_is_direct_key_access(not_found, Key, Opts) ->
    do_is_direct_key_access(<<"message@1.0">>, Key, Opts);
do_is_direct_key_access(error, Key, Opts) ->
    do_is_direct_key_access(<<"message@1.0">>, Key, Opts);
do_is_direct_key_access(<<"message@1.0">>, Key, _Opts) ->
    not lists:member(Key, ?MESSAGE_KEYS) andalso not hb_private:is_private(Key);
do_is_direct_key_access(Dev, NormKey, Opts) ->
    ?event_debug(debug_read_cached, {calculating_info, {device, Dev}}),
    case device_info(#{ <<"device">> => Dev}, Opts) of
        Info = #{ exports := Exports }
            when not is_map_key(handler, Info) andalso not is_map_key(default, Info) ->
            ?event_debug(debug_read_cached,
                {exports,
                    {device, Dev},
                    {key, NormKey},
                    {exports, Exports}
                }
            ),
            not lists:member(NormKey, ?MESSAGE_KEYS)
                andalso not hb_private:is_private(NormKey)
                andalso not is_exported(Info, NormKey, Opts);
        _ -> false
    end.
