%%% @doc A library for working with HyperBEAM-compatible AO-Core devices.
%%% Offers services for loading, verifying executability, and extracting Erlang
%%% functions from a device.
-module(hb_ao_device).
-export([truncate_args/2, message_to_fun/3, message_to_device/2, load/2]).
-export([implementation_dir/1]).
-export([is_direct_key_access/3, is_direct_key_access/4]).
-export([find_exported_function/5, is_exported/4, info/2, info/3]).
-include("include/hb.hrl").

-define(DEFAULT_DEVICE, <<"message@1.0">>).

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
                        {{ok, DefaultDevice}, true} when is_binary(DefaultDevice)
                                orelse is_atom(DefaultDevice) ->
                            % Case 5: The device gives a specific further device
                            % to default to. Recurse with it and apply the same
                            % rules.
							?event({found_default_device, {mod, DefaultDevice}}),
                            message_to_fun(
                                Msg#{ <<"device">> => DefaultDevice },
                                Key,
                                Opts
                            );
						_ ->
							% Case 6: The device has no default handler.
							% We retry with the default unless the message
							% already names it (loop guard).
							case hb_maps:get(<<"device">>, Msg, undefined, Opts) of
								?DEFAULT_DEVICE ->
									throw({
										error,
										default_device_could_not_resolve_key,
										{key, Key}
									});
								_ ->
									?event({using_default_device, ?DEFAULT_DEVICE}),
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
    case load(DevID, Opts) of
        {error, Reason} ->
            throw({error, {device_not_loadable, DevID, Reason}});
        {ok, DevMod} -> DevMod
    end.

%% @doc Parse a handler key given by a device's `info'.
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
find_exported_function(Msg, Mod, Key, Arity, Opts) when not is_atom(Key) ->
	try hb_util:key_to_atom(Key, false) of
		KeyAtom -> find_exported_function(Msg, Mod, KeyAtom, Arity, Opts)
	catch _:_ -> not_found
	end;
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
%% orinate from a device's `info' response, but _not_ for keys that could be
%% chosen by non-author users. This imparts a requirement that device developers
%% should not generate too many different exports/excludes -- just as they should
%% not generate too many atoms.
maybe_normalize_device_key(Key) -> maybe_normalize_device_key(Key, new_atoms).
maybe_normalize_device_key(Key, Mode) ->
    try hb_util:key_to_atom(hb_ao:normalize_key(Key), Mode)
    catch _:_ -> Key
    end.

%% @doc Resolve a device reference to its Erlang module.
%%
%% Three direct forms are handled first: an inline device map, an
%% already-loaded generated module atom, and a binary device name/ID.
%%
%% A binary name takes one of two paths that never intermingle:
%%
%% <ul>
%%   <li><b>Forge build.</b> If `forge-bootstrap' is set in `Opts' the
%%       device search <em>is</em> a `maps:find' in that map and
%%       nothing else -- no store, resolver, archive, or cache. Only
%%       the Forge sets it, supplying the seed codecs under their
%%       ordinary module names so it can compute message IDs and sign
%%       the preloaded messages.</li>
%%   <li><b>Runtime.</b> Otherwise the name is resolved through the
%%       preloaded store: `name@1.0' maps it to a specification ID, the
%%       matching signed `application/beam-archive' implementation is
%%       trust- and compatibility-checked, and its archive is loaded.
%%       The resolved module is memoised in the device-store cache.</li>
%% </ul>
load(Map, _Opts) when is_map(Map) ->
    {ok, Map};
load(Atom, _Opts) when is_atom(Atom) ->
    case hb_device_name:is_generated(Atom) of
        false -> {error, load_error(<<"device-must-be-packaged">>, Atom)};
        true ->
            case loaded_module(Atom) of
                {ok, _} = Ok -> Ok;
                not_found -> {error, load_error(<<"device-not-loaded">>, Atom)}
            end
    end;
load(Ref, Opts) when is_binary(Ref) ->
    NormRef = hb_ao:normalize_key(Ref),
    case hb_opts:get(forge_bootstrap, undefined, Opts) of
        Seeds when is_map(Seeds) ->
            % Forge build path: the seed map is the whole device search.
            case maps:find(NormRef, Seeds) of
                {ok, Mod} -> {ok, Mod};
                error -> {error, load_error(<<"forge-seed-missing">>, NormRef)}
            end;
        _ ->
            load_runtime(NormRef, Opts)
    end.

%%% --------------------------------------------------------------------
%%% Runtime device loading (pure preloaded store)
%%% --------------------------------------------------------------------

%% @doc Resolve a device name through the preloaded store, memoising the
%% loaded module under both its name and specification ID.
load_runtime(NormRef, Opts) ->
    maybe
        true ?= is_admissible(NormRef, Opts),
        not_found ?= lookup_device_cache(NormRef, Opts),
        {ok, SpecID} ?= resolve_to_spec_id(NormRef, Opts),
        {ok, Mod} ?= load_implementation(NormRef, SpecID, Opts),
        cache_device_module(NormRef, Mod, Opts),
        cache_device_module(SpecID, Mod, Opts),
        {ok, Mod}
    else
        false -> {error, load_error(<<"device-not-admissible">>, NormRef)};
        {ok, _} = Cached -> Cached;
        not_found -> {error, load_error(<<"device-not-found">>, NormRef)};
        {error, _} = Error -> Error
    end.

%% @doc Optional operator allow-list for runtime-loadable devices.
is_admissible(Ref, Opts) ->
    case hb_opts:get(admissible_devices, all, Opts) of
        Names when is_list(Names) ->
            lists:any(fun(N) -> hb_util:bin(N) =:= Ref end, Names);
        _ -> true
    end.

%% @doc Resolve an ID to itself, or a name to its specification ID via
%% the `name@1.0' resolver bootstrapped from the preloaded store.
resolve_to_spec_id(Ref, _Opts) when ?IS_ID(Ref) ->
    {ok, Ref};
resolve_to_spec_id(<<"name@1.0">>, Opts) ->
    case preloaded_resolver(Opts) of
        {Resolver, ROpts} ->
            spec_result(
                hb_ao:resolve(
                    Resolver,
                    #{ <<"path">> => <<"name@1.0">> },
                    resolve_opts(ROpts)
                )
            );
        not_found ->
            not_found
    end;
resolve_to_spec_id(Ref, Opts) ->
    try
        spec_result(
            hb_ao:resolve(
                #{ <<"device">> => <<"name@1.0">> },
                #{ <<"path">> => Ref, <<"load">> => false },
                resolve_opts(with_preloaded_name_resolver(Opts))
            )
        )
    catch
        throw:{error, {device_not_loadable, _Dev, Msg = #{}}} -> {error, Msg}
    end.

spec_result({ok, ID}) when ?IS_ID(ID) -> {ok, ID};
spec_result({error, Msg = #{}}) -> {error, Msg};
spec_result(_) -> not_found.

resolve_opts(Opts) ->
    Opts#{
        <<"error-strategy">> => return,
        <<"force-message">> => false,
        <<"paranoid-verify">> => false
    }.

%% @doc Find and load a device's signed implementation: the preloaded
%% store first, then any other configured stores (preloaded prepended),
%% then a gateway when remote device loading is enabled.
load_implementation(Ref, SpecID, Opts) ->
    Trusted = trusted_devices(Opts),
    maybe
        not_found ?=
            match_and_load(
                Ref, SpecID, Trusted,
                preloaded_opts(Opts),
                fun(ID, O) -> hb_cache:read(ID, O) end
            ),
        not_found ?=
            match_and_load(
                Ref, SpecID, Trusted,
                (with_preloaded_store(Opts))#{ <<"match-index">> => false },
                fun(ID, O) -> hb_cache:read(ID, O) end
            ),
        false ?= hb_opts:get(load_remote_devices, false, Opts),
        not_found
    else
        {ok, _} = Ok -> Ok;
        {error, _} = Error -> Error;
        true -> load_remote(Ref, SpecID, Trusted, Opts)
    end.

preloaded_opts(Opts) ->
    case preloaded_store(Opts) of
        undefined -> Opts#{ <<"store">> => [] };
        Store -> Opts#{ <<"store">> => Store, <<"cache-read-mode">> => raw }
    end.

%% @doc Match the implementation message(s) for `SpecID' in the given
%% store and load the first whose trust and metadata check out.
match_and_load(Ref, SpecID, Trusted, Opts, Read) ->
    Matched =
        case
            hb_cache:match(
                #{
                    <<"data-protocol">> => <<"ao">>,
                    <<"variant">> => <<"ao.N.1">>,
                    <<"content-type">> => <<"application/beam-archive">>,
                    <<"implements-device">> => SpecID
                },
                Opts
            )
        of
            {ok, IDs} -> IDs;
            _ -> []
        end,
    load_ids(Matched ++ Trusted, Ref, SpecID, Opts, Read).

load_remote(Ref, SpecID, Trusted, Opts) ->
    maybe
        not_found ?=
            load_ids(
                Trusted, Ref, SpecID, Opts,
                fun(ID, O) -> hb_gateway_client:read(ID, O) end
            ),
        case hb_gateway_client:device(SpecID, Opts) of
            {ok, Msg} -> verify_implementation(Msg, Ref, SpecID, undefined, Opts);
            _ -> {error, load_error(<<"remote-device-not-found">>, Ref, SpecID)}
        end
    end.

load_ids([], _Ref, _SpecID, _Opts, _Read) ->
    not_found;
load_ids([ID | IDs], Ref, SpecID, Opts, Read) ->
    case Read(ID, Opts) of
        {ok, Msg} ->
            case verify_implementation(Msg, Ref, SpecID, ID, Opts) of
                {ok, _} = Ok -> Ok;
                {error, _} = Error when IDs =:= [] -> Error;
                {error, _} -> load_ids(IDs, Ref, SpecID, Opts, Read)
            end;
        _ ->
            load_ids(IDs, Ref, SpecID, Opts, Read)
    end.

%% @doc Verify an implementation message's trust and metadata, then load
%% its archive. The implementation is trusted if its ID is explicitly
%% trusted or one of its signers is.
verify_implementation(RawMsg, Ref, SpecID, ID, Opts) ->
    Msg = load_implementation_message(RawMsg, Opts),
    Trusted =
        is_trusted_device(ID, Opts)
            orelse is_signer_trusted(
                implementation_signers(Msg, Opts), trusted_signers(Opts)),
    maybe
        true ?= Trusted,
        {SpecID, <<"ao">>, <<"ao.N.1">>, <<"application/beam-archive">>} ?=
            {
                hb_maps:get(<<"implements-device">>, Msg, undefined, Opts),
                hb_maps:get(<<"data-protocol">>, Msg, undefined, Opts),
                hb_maps:get(<<"variant">>, Msg, undefined, Opts),
                hb_maps:get(<<"content-type">>, Msg, undefined, Opts)
            },
        ok ?= verify_device_compatibility(Msg, Opts),
        load_archive(Ref, Msg, Opts)
    else
        false ->
            {error, load_error(<<"device-signer-not-trusted">>, Ref)};
        {error, Reason} ->
            {error,
                load_error(<<"device-requirements-not-met">>, Ref, Reason)};
        {Bad, _, _, _} when Bad =/= SpecID ->
            {error, load_error(<<"wrong-device-specification">>, Ref, Bad)};
        _ ->
            {error, load_error(<<"wrong-device-message">>, Ref)}
    end.

%% @doc Load implementation links without bootstrapping codec devices.
load_implementation_message(Msg, Opts) ->
    case hb_opts:get(cache_read_mode, normal, Opts) of
        raw -> hb_cache:ensure_all_loaded(Msg, Opts);
        _ -> hb_cache:read_all_commitments(Msg, Opts)
    end.

implementation_signers(Msg, Opts) ->
    hb_maps:values(
        hb_maps:filtermap(
            fun(_ID, C) ->
                case hb_maps:get(<<"committer">>, C, undefined, Opts) of
                    undefined -> false;
                    Signer -> {true, Signer}
                end
            end,
            hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
            Opts
        ),
        Opts
    ).

%% @doc Load the verified implementation's BEAM archive.
load_archive(Ref, Msg, Opts) ->
    ModBin = hb_maps:get(<<"module-name">>, Msg, undefined, Opts),
    Archive =
        hb_maps:get(
            <<"body">>,
            Msg,
            hb_maps:get(<<"data">>, Msg, undefined, Opts),
            Opts
        ),
    case hb_device_archive:load(ModBin, Archive, Msg, Opts) of
        {ok, Mod} -> {ok, Mod};
        {error, Reason} ->
            {error, load_error(<<"device-archive-load-failed">>, Ref, Reason)}
    end.

%% @doc Verify that an implementation's `requires-*' keys match the
%% running machine.
verify_device_compatibility(Msg, Opts) ->
    Failed =
        lists:filtermap(
            fun
                ({<<"requires-", Key/binary>>, Value}) ->
                    Prop =
                        hb_util:key_to_atom(
                            hb_ao:normalize_key(Key), new_atoms),
                    Want = hb_cache:ensure_loaded(Value, Opts),
                    case
                        hb_ao:normalize_key(erlang:system_info(Prop))
                            == hb_ao:normalize_key(Want)
                    of
                        true -> false;
                        false -> {true, {Prop, Want}}
                    end;
                (_) ->
                    false
            end,
            hb_maps:to_list(Msg, Opts)
        ),
    case Failed of
        [] -> ok;
        _ -> {error, {failed_requirements, Failed}}
    end.

%% @doc Return the extracted implementation directory for a device.
implementation_dir(Module) when is_atom(Module) ->
    hb_device_archive:implementation_dir(Module).

%%% --------------------------------------------------------------------
%%% Device-store cache, store + trust helpers
%%% --------------------------------------------------------------------

-define(DEV_CACHE_PREFIX, <<"devices/">>).

%% @doc Look up a previously-resolved generated module for a name or
%% spec ID, in the process dictionary then the device store.
lookup_device_cache(Ref, Opts) ->
    case erlang:get({?MODULE, device_cache, Ref}) of
        Mod when is_atom(Mod), Mod =/= undefined ->
            case loaded_module(Mod) of
                {ok, _} = Ok -> Ok;
                not_found -> lookup_device_store(Ref, Opts)
            end;
        _ ->
            lookup_device_store(Ref, Opts)
    end.

lookup_device_store(Ref, Opts) ->
    case device_store(Opts) of
        undefined ->
            not_found;
        Store ->
            maybe
                {ok, ModBin} ?=
                    hb_store:read(
                        Store, <<?DEV_CACHE_PREFIX/binary, Ref/binary>>, Opts),
                true ?= hb_device_name:is_generated(ModBin),
                {ok, Mod} ?= existing_atom(ModBin),
                {ok, _} ?= loaded_module(Mod),
                erlang:put({?MODULE, device_cache, Ref}, Mod),
                {ok, Mod}
            else
                _ -> not_found
            end
    end.

%% @doc `{ok, Atom}' for an existing atom of `Bin', else `not_found'
%% (so a stale cache entry never crashes resolution).
existing_atom(Bin) ->
    try {ok, hb_util:key_to_atom(Bin, existing)}
    catch _:_ -> not_found
    end.

%% @doc Whether a generated module atom is loaded in the code server.
loaded_module(ModName) ->
    case code:is_loaded(ModName) of
        {file, _} -> {ok, ModName};
        false -> not_found
    end.

%% @doc Memoise a name or spec ID -> generated module.
cache_device_module(Ref, Mod, Opts) when is_atom(Mod) ->
    erlang:put({?MODULE, device_cache, Ref}, Mod),
    case device_store(Opts) of
        undefined -> ok;
        Store ->
            hb_store:write(
                Store,
                #{
                    <<?DEV_CACHE_PREFIX/binary, Ref/binary>> =>
                        hb_util:bin(Mod)
                },
                Opts
            )
    end.

device_store(Opts) -> hb_opts:get(device_store, undefined, Opts).

preloaded_store(Opts) ->
    hb_opts:get(preloaded_store, undefined, node_config_opts(Opts)).

node_config_opts(Opts) ->
    maps:without([<<"cache-control">>, <<"only">>, <<"prefer">>], Opts).

%% @doc The signed name->spec resolver message read straight from the
%% preloaded store via a minimal in-memory lookup device (so resolving
%% device names needs no other device).
preloaded_resolver(Opts) ->
    NodeOpts = node_config_opts(Opts),
    Store = preloaded_store(NodeOpts),
    IndexID = hb_opts:get(preloaded_devices_index, undefined, NodeOpts),
    maybe
        true ?= Store =/= undefined andalso IndexID =/= undefined,
        ReadOpts =
            NodeOpts#{ <<"store">> => Store, <<"cache-read-mode">> => raw },
        {ok, Resolver} ?= hb_cache:read(IndexID, ReadOpts),
        {
            Resolver#{ <<"device">> => primitive_name_resolver(ReadOpts) },
            with_preloaded_store(NodeOpts)
        }
    else
        _ -> not_found
    end.

with_preloaded_name_resolver(Opts) ->
    case preloaded_resolver(Opts) of
        {Resolver, ROpts} ->
            ROpts#{
                <<"name-resolvers">> =>
                    [Resolver | listify(hb_opts:get(name_resolvers, [], ROpts))]
            };
        not_found ->
            Opts
    end.

primitive_name_resolver(ReadOpts) ->
    #{
        info =>
            fun() ->
                #{
                    default =>
                        fun(Key, Base, _Req, _Opts) ->
                            case hb_maps:get(Key, Base, not_found, ReadOpts) of
                                not_found -> not_found;
                                Value -> {ok, Value}
                            end
                        end
                }
            end
    }.

with_preloaded_store(Opts) ->
    case preloaded_store(Opts) of
        undefined -> Opts;
        Pre ->
            Opts#{
                <<"store">> =>
                    [Pre | listify(hb_opts:get(store, [], Opts))]
            }
    end.

listify(L) when is_list(L) -> L;
listify(undefined) -> [];
listify(X) -> [X].

trusted_devices(Opts) ->
    case hb_opts:get(trusted_devices, [], Opts) of
        IDs when is_list(IDs) -> [hb_util:bin(ID) || ID <- IDs];
        _ -> []
    end.

trusted_signers(Opts) ->
    case hb_opts:get(trusted_device_signers, [], Opts) of
        [] -> [hb:address()];
        Configured -> Configured
    end.

is_trusted_device(undefined, _Opts) -> false;
is_trusted_device(ID, Opts) ->
    lists:member(hb_util:bin(ID), trusted_devices(Opts)).

is_signer_trusted([], _) -> false;
is_signer_trusted(_Signers, all) -> true;
is_signer_trusted(Signers, List) when is_list(List) ->
    lists:any(fun(S) -> lists:member(S, List) end, Signers);
is_signer_trusted(_Signers, _) -> false.

%% @doc Build an AO-message-shaped load error.
load_error(Code, Ref) ->
    #{ <<"error">> => Code, <<"device">> => hb_util:bin(Ref) }.

load_error(Code, Ref, Reason) ->
    (load_error(Code, Ref))#{ <<"reason">> => reason_bin(Reason) }.

reason_bin(Reason) when is_binary(Reason) -> Reason;
reason_bin(Reason) when is_atom(Reason) -> hb_util:bin(Reason);
reason_bin(Reason) -> iolist_to_binary(io_lib:format("~p", [Reason])).

%% @doc Get the info map for a device, optionally giving it a message if the
%% device's info function is parameterized by one.
info(Msg, Opts) -> info(message_to_device(Msg, Opts), Msg, Opts).
info(DevMod, Msg, Opts) ->
    case find_exported_function(Msg, DevMod, info, 2, Opts) of
		{ok, Fun} -> apply(Fun, truncate_args(Fun, [Msg, Opts]));
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
    DevPath =
        hb_util:ok_or(
            hb_store:resolve(Store, [Base, <<"device">>], Opts),
            [Base, <<"device">>]
        ),
    case hb_store:read(Store, DevPath, Opts) of
        {ok, Dev} ->
            do_is_direct_key_access(Dev, Req, Opts);
        {error, not_found} ->
            fallback_direct_key_access(Store, Base, Req, Opts)
    end;
is_direct_key_access(Base, Req, Opts, _) when is_map(Base) ->
    do_is_direct_key_access(hb_maps:find(<<"device">>, Base, Opts), Req, Opts).

fallback_direct_key_access(Store, Base, Req, Opts) ->
    case hb_store:type(Store, Base, Opts) of
        {error, not_found} -> unknown;
        {ok, _} -> do_is_direct_key_access(<<"message@1.0">>, Req, Opts)
    end.

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
    ?event(debug_read_cached, {calculating_info, {device, Dev}}),
    case info(#{ <<"device">> => Dev}, Opts) of
        Info = #{ exports := Exports }
            when not is_map_key(handler, Info) andalso not is_map_key(default, Info) ->
            ?event(debug_read_cached,
                {exports,
                    {device, Dev},
                    {key, NormKey},
                    {exports, Exports}
                }
            ),
            not lists:member(NormKey, Exports ++ ?MESSAGE_KEYS);
        _ -> false
    end.
