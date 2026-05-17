%%% @doc A library for working with HyperBEAM-compatible AO-Core devices.
%%% Offers services for loading, verifying executability, and extracting Erlang
%%% functions from a device.
-module(hb_ao_device).
-export([truncate_args/2, message_to_fun/3, message_to_device/2, load/2]).
-export([implementation_dir/1]).
-export([is_direct_key_access/3, is_direct_key_access/4]).
-export([find_exported_function/5, is_exported/4, info/2, info/3, default/0]).
-include("include/hb.hrl").

-define(ON_LOAD_FORMAT, <<"hb-device-on-load-v1">>).
-define(DEFAULT_IMPLEMENTATION_DIR, "_build/device-implementations").

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
							DefaultDev = default(),
							case hb_maps:get(<<"device">>, Msg, undefined, Opts) of
								DefaultDev ->
									throw({
										error,
										default_device_could_not_resolve_key,
										{key, Key}
									});
								_ ->
									?event(
										{using_default_device,
										 {dev, DefaultDev}}),
									message_to_fun(
										Msg#{ <<"device">> => DefaultDev },
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
    DevID =
        case hb_maps:get(<<"device">>, Msg, not_found, Opts) of
            not_found -> default();
            ID -> ID
        end,
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

%% @doc Load a device by name, specification ID, inline device map, or
%% already-resolved generated module atom. Source `dev_*' atoms are rejected:
%% runtime devices must resolve to signed `_hb_device_*' modules.
load(Map, _Opts) when is_map(Map) -> {ok, Map};
load(Atom, _Opts) when is_atom(Atom) ->
    case hb_device_name:is_generated(Atom) of
        true ->
            case loaded_module(Atom) of
                {ok, _} = Ok -> Ok;
                not_found -> {error, load_error(<<"device-not-loaded">>, Atom)}
            end;
        false -> {error, load_error(<<"device-must-be-packaged">>, Atom)}
    end;
load(Ref, Opts) when is_binary(Ref) ->
    NormRef = hb_ao:normalize_key(Ref),
    ?event(device_load, {requested_load, {ref, NormRef}}, Opts),
    case is_admissible(NormRef, Opts) of
        false ->
            {error, load_error(<<"device-not-admissible">>, NormRef)};
        true ->
            load_binary(NormRef, Opts)
    end.

%% @doc Resolve and load a normalized binary device reference.
load_binary(NormRef, Opts) ->
    case lookup_device_cache(NormRef, Opts) of
        {ok, Atom} ->
            {ok, Atom};
        not_found ->
            maybe
                {ok, SpecID} ?= resolve_to_spec_id(NormRef, Opts),
                {ok, ModName} ?= load_implementation(NormRef, SpecID, Opts),
                cache_device_module(NormRef, ModName, Opts),
                cache_device_module(SpecID, ModName, Opts),
                {ok, ModName}
            else
                not_found ->
                    {error, load_error(<<"device-not-found">>, NormRef)};
                {error, _} = Error -> Error
            end
    end.

%% @doc Check the optional operator allow-list for runtime-loadable devices.
is_admissible(Ref, Opts) ->
    case hb_opts:get(admissible_devices, all, Opts) of
        all -> true;
        Names when is_list(Names) ->
            lists:any(fun(N) -> hb_util:bin(N) =:= Ref end, Names);
        _ -> true
    end.

%%% --------------------------------------------------------------------
%%% Resolution helpers
%%% --------------------------------------------------------------------

%% @doc Resolve IDs directly, bootstrap `name@1.0' from the preloaded
%% resolver message, and resolve every other name through `name@1.0'.
resolve_to_spec_id(Ref, _Opts) when ?IS_ID(Ref) -> {ok, Ref};
resolve_to_spec_id(<<"name@1.0">>, Opts) ->
    case preloaded_resolver(Opts) of
        {Resolver, ResolverOpts} ->
            spec_result(hb_ao:resolve(
                Resolver,
                #{ <<"path">> => <<"name@1.0">> },
                device_resolve_opts(ResolverOpts)
            ));
        not_found ->
            not_found
    end;
resolve_to_spec_id(Ref, Opts) ->
    try spec_result(hb_ao:resolve(
            #{ <<"device">> => <<"name@1.0">> },
            #{ <<"path">> => Ref, <<"load">> => false },
            device_resolve_opts(with_preloaded_name_resolver(Opts))
        ))
    catch
        throw:{error, {device_not_loadable, _Device, Msg = #{}}} ->
            {error, Msg}
    end.

%% @doc Normalize AO-Core name-resolution results to a specification ID.
spec_result({ok, ID}) when ?IS_ID(ID) ->
    {ok, ID};
spec_result({error, Msg = #{}}) ->
    {error, Msg};
spec_result(_) ->
    not_found.

%% @doc Options for internal device-name resolutions.
device_resolve_opts(Opts) ->
    Opts#{
        <<"error-strategy">> => return,
        <<"force-message">> => false,
        <<"paranoid-verify">> => false
    }.

%% @doc Find, verify, and load a signed implementation archive.
load_implementation(Ref, SpecID, Opts) ->
    case load_preloaded_implementation(Ref, SpecID, Opts) of
        {ok, _} = Ok -> Ok;
        not_found -> load_cached_or_remote_implementation(Ref, SpecID, Opts);
        Error -> Error
    end.

%% @doc Load from the preloaded-store without requiring codec devices.
load_preloaded_implementation(Ref, SpecID, Opts) ->
    case preloaded_store(Opts) of
        undefined ->
            not_found;
        Store ->
            load_local_implementation(Ref, SpecID, Opts#{
                <<"store">> => Store,
                <<"cache-read-mode">> => raw
            })
    end.

%% @doc Load from configured stores, then gateways when enabled.
load_cached_or_remote_implementation(Ref, SpecID, Opts) ->
    SearchOpts = (with_preloaded_store(Opts))#{ <<"match-index">> => false },
    case {
        load_local_implementation(Ref, SpecID, SearchOpts),
        hb_opts:get(load_remote_devices, false, Opts)
    } of
        {{ok, _} = Ok, _} -> Ok;
        {{error, _} = Error, _} -> Error;
        {not_found, false} -> not_found;
        {not_found, true} -> load_remote_implementation(Ref, SpecID, Opts)
    end.

%% @doc Match local implementation messages and load the first valid hit.
load_local_implementation(Ref, SpecID, Opts) ->
    case hb_cache:match(
        #{
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"content-type">> => <<"application/beam-archive">>,
            <<"implements-device">> => SpecID
        },
        Opts
    ) of
        {ok, IDs} ->
            load_implementation_ids(
                IDs ++ trusted_devices(Opts),
                Ref,
                SpecID,
                Opts
            );
        _ ->
            load_implementation_ids(trusted_devices(Opts), Ref, SpecID, Opts)
    end.

%% @doc Try matching implementation IDs in store order.
load_implementation_ids([], _Ref, _SpecID, _Opts) ->
    not_found;
load_implementation_ids([ID | IDs], Ref, SpecID, Opts) ->
    case hb_cache:read(ID, Opts) of
        {ok, Msg} ->
            case verify_implementation(Msg, Ref, SpecID, ID, Opts) of
                {ok, _} = Ok -> Ok;
                {error, _} = Error when IDs =:= [] -> Error;
                {error, _} -> load_implementation_ids(IDs, Ref, SpecID, Opts)
            end;
        _ ->
            load_implementation_ids(IDs, Ref, SpecID, Opts)
    end.

%% @doc Load remotely by trusted implementation IDs, then by trusted signers.
load_remote_implementation(Ref, SpecID, Opts) ->
    case load_remote_trusted_implementation(
        Ref, SpecID, trusted_devices(Opts), Opts
    ) of
        {ok, _} = Ok -> Ok;
        not_found ->
            case hb_gateway_client:device(SpecID, Opts) of
                {ok, Msg} -> verify_implementation(Msg, Ref, SpecID, Opts);
                _ ->
                    {error, load_error(
                        <<"remote-device-not-found">>, Ref, SpecID)}
            end;
        {error, _} = Error -> Error
    end.

load_remote_trusted_implementation(_Ref, _SpecID, [], _Opts) ->
    not_found;
load_remote_trusted_implementation(Ref, SpecID, [ID | IDs], Opts) ->
    case hb_gateway_client:read(ID, Opts) of
        {ok, Msg} ->
            case verify_implementation(Msg, Ref, SpecID, ID, Opts) of
                {ok, _} = Ok -> Ok;
                {error, _} = Error when IDs =:= [] -> Error;
                {error, _} ->
                    load_remote_trusted_implementation(Ref, SpecID, IDs, Opts)
            end;
        _ ->
            load_remote_trusted_implementation(Ref, SpecID, IDs, Opts)
    end.

%% @doc Verify a signed implementation and load its root module.
verify_implementation(Msg, Ref, SpecID, Opts) ->
    verify_implementation(Msg, Ref, SpecID, undefined, Opts).

verify_implementation(Msg, Ref, SpecID, ID, Opts) ->
    LoadedMsg = load_implementation_message(Msg, Opts),
    Signers = implementation_signers(LoadedMsg, Opts),
    TrustedDevice = is_trusted_device(ID, Opts),
    Trusted = TrustedDevice orelse
        is_signer_trusted(Signers, trusted_signers(Opts)),
    ?event(device_load,
        {verifying_device_trust,
            {ref, Ref},
            {trusted, Trusted},
            {trusted_device, TrustedDevice},
            {implementation, ID},
            {signers, Signers}
        },
        Opts
    ),
    case Trusted of
        false ->
            {error, load_error(<<"device-signer-not-trusted">>, Ref)};
        true ->
            verify_implementation_message(LoadedMsg, Ref, SpecID, Opts)
    end.

%% @doc Extract implementation signers without loading the message device.
implementation_signers(Msg, Opts) ->
    hb_maps:values(
        hb_maps:filtermap(
            fun(_ID, Commitment) ->
                case hb_maps:get(<<"committer">>, Commitment, undefined, Opts) of
                    undefined -> false;
                    Signer -> {true, Signer}
                end
            end,
            hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
            Opts
        ),
        Opts
    ).

%% @doc Load links in an implementation message without bootstrapping codecs.
load_implementation_message(Msg, Opts) ->
    case hb_opts:get(cache_read_mode, normal, Opts) of
        raw -> hb_cache:ensure_all_loaded(Msg, Opts);
        _ -> hb_cache:read_all_commitments(Msg, Opts)
    end.

%% @doc Verify implementation metadata before loading the archive.
verify_implementation_message(Msg, Ref, SpecID, Opts) ->
    case {
        hb_maps:get(<<"implements-device">>, Msg, undefined, Opts),
        hb_maps:get(<<"data-protocol">>, Msg, undefined, Opts),
        hb_maps:get(<<"variant">>, Msg, undefined, Opts),
        hb_maps:get(<<"content-type">>, Msg, undefined, Opts)
    } of
        {SpecID, <<"ao">>, <<"ao.N.1">>, <<"application/beam-archive">>} ->
            case verify_device_compatibility(Msg, Opts) of
                ok ->
                    load_archive(
                        hb_maps:get(<<"module-name">>, Msg, undefined, Opts),
                        hb_maps:get(
                            <<"body">>,
                            Msg,
                            hb_maps:get(<<"data">>, Msg, undefined, Opts),
                            Opts
                        ),
                        Msg,
                        Ref,
                        Opts
                    );
                {error, Reason} ->
                    {error, load_error(
                        <<"device-requirements-not-met">>, Ref, Reason
                    )}
            end;
        {Other, _, _, _} when Other =/= SpecID ->
            {error, load_error(<<"wrong-device-specification">>, Ref, Other)};
        {_, Protocol, Variant, _}
                when Protocol =/= <<"ao">>; Variant =/= <<"ao.N.1">> ->
            {error, load_error(<<"wrong-device-protocol">>, Ref)};
        {_, _, _, ContentType} ->
            {error, load_error(
                <<"wrong-device-content-type">>, Ref, ContentType)}
    end.

%% @doc Load a device archive into the runtime. The root module atom
%% provided in the implementation message must be in `_hb_device_*'
%% form. All archive members must also be generated module names.
load_archive(undefined, _Archive, _Msg, Ref, _Opts) ->
    {error, load_error(<<"missing-module-name">>, Ref)};
load_archive(_, undefined, _Msg, Ref, _Opts) ->
    {error, load_error(<<"missing-archive">>, Ref)};
load_archive(ModBin, Archive, Msg, Ref, Opts) ->
    case hb_device_name:is_generated(ModBin) of
        false ->
            {error, load_error(
                <<"non-generated-module-name">>, Ref, ModBin
            )};
        true ->
            ModName = hb_util:key_to_atom(ModBin, new_atoms),
            case archive_contents(Archive) of
                {ok, Modules, ResourceFiles} ->
                    do_load_archive(ModName, Modules, ResourceFiles, Msg, Opts);
                {error, Reason} ->
                    {error, load_error(<<"invalid-archive">>, Ref, Reason)}
            end
    end.

%% @doc Extract loadable content from a deterministic implementation archive.
archive_contents(Archive) ->
    case zip:unzip(Archive, [memory]) of
        {ok, Files} ->
            read_archive_entries(Files, [], []);
        {error, Reason} ->
            {error, {archive_extract_failed, Reason}}
    end.

%% @doc Validate archive entries and gather BEAMs and resource files.
read_archive_entries([], ModulesAcc, ResourceAcc) ->
    Modules = [Mod || {Mod, _, _} <- ModulesAcc],
    ResourcePaths = [Path || {Path, _} <- ResourceAcc],
    case {
        length(Modules) =:= length(lists:usort(Modules)),
        length(ResourcePaths) =:= length(lists:usort(ResourcePaths))
    } of
        {true, true} ->
            {ok, lists:reverse(ModulesAcc), lists:reverse(ResourceAcc)};
        {false, _} -> {error, duplicate_archive_module};
        {_, false} -> {error, duplicate_archive_file}
    end;
read_archive_entries([{Path0, Body} | Rest], ModulesAcc, ResourceAcc) ->
    Path = hb_util:bin(Path0),
    case Path of
        <<"ebin/", _/binary>> ->
            case archive_beam_module(Path, Body) of
                {ok, Mod} ->
                    read_archive_entries(
                        Rest,
                        [{Mod, binary_to_list(Path), Body} | ModulesAcc],
                        ResourceAcc
                    );
                {error, Reason} ->
                    {error, Reason}
            end;
        <<"priv/", Rel/binary>> ->
            case safe_archive_resource(Rel) of
                ok ->
                    read_archive_entries(
                        Rest,
                        ModulesAcc,
                        [{Rel, Body} | ResourceAcc]
                    );
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, {unsupported_archive_path, Path}}
    end.

%% @doc Confirm that an archive member is a generated BEAM at its own path.
archive_beam_module(Path, Beam) ->
    case beam_lib:chunks(Beam, [exports]) of
        {ok, {Mod, _Chunks}} ->
            ModBin = atom_to_binary(Mod, utf8),
            ExpectedPath = <<"ebin/", ModBin/binary, ".beam">>,
            case {hb_device_name:is_generated(Mod), Path} of
                {false, _} ->
                    {error, {non_generated_module_name, ModBin}};
                {true, ExpectedPath} ->
                    {ok, Mod};
                {true, _} ->
                    {error, {archive_path_mismatch, Path, ExpectedPath}}
            end;
        {error, _Module, Reason} ->
            {error, {invalid_beam, Path, Reason}}
    end.

%% @doc Atomically load every module in the archive, then run generated
%% on-load callbacks in the order recorded by the implementation message.
do_load_archive(RootMod, Modules, ResourceFiles, Msg, Opts) ->
    case lists:keymember(RootMod, 1, Modules) of
        false ->
            {error, load_error(<<"archive-missing-root">>, RootMod)};
        true ->
            RootBin = atom_to_binary(RootMod, utf8),
            case archive_modules_match_root(RootBin, Modules) of
                ok ->
                    maybe_load_archive(
                        RootMod, Modules, ResourceFiles, Msg, Opts
                    );
                {error, Reason} ->
                    {error, load_error(
                        <<"invalid-archive-namespace">>, RootMod, Reason
                    )}
            end
    end.

%% @doc Ensure every archive member belongs to the root module namespace.
archive_modules_match_root(RootBin, Modules) ->
    Prefix = <<RootBin/binary, "__">>,
    Bad =
        [
            Mod
        ||
            {Mod, _, _} <- Modules,
            not begin
                ModBin = atom_to_binary(Mod, utf8),
                ModBin =:= RootBin orelse
                    binary:match(ModBin, Prefix) =:= {0, byte_size(Prefix)}
            end
        ],
    case Bad of
        [] -> ok;
        _ -> {error, {archive_module_outside_namespace, Bad}}
    end.

safe_archive_resource(<<>>) ->
    {error, empty_archive_resource_path};
safe_archive_resource(Rel) ->
    Parts = binary:split(Rel, <<"/">>, [global]),
    case binary:match(Rel, <<"\\">>) =/= nomatch orelse
        lists:any(fun unsafe_archive_resource_part/1, Parts)
    of
        true -> {error, {unsafe_archive_resource_path, Rel}};
        false -> ok
    end.

unsafe_archive_resource_part(<<>>) -> true;
unsafe_archive_resource_part(<<".">>) -> true;
unsafe_archive_resource_part(<<"..">>) -> true;
unsafe_archive_resource_part(_) -> false.

%% @doc Prepare archive resources and load unless every module is present.
maybe_load_archive(RootMod, Modules, ResourceFiles, Msg, Opts) ->
    case archive_loaded(Modules) of
        true ->
            {ok, RootMod};
        false ->
            case prepare_implementation_dir(
                RootMod,
                atom_to_binary(RootMod, utf8),
                ResourceFiles,
                Opts
            ) of
                ok -> load_archive_modules(RootMod, Modules, Msg, Opts);
                {error, Reason} ->
                    {error, load_error(
                        <<"implementation-dir-failed">>, RootMod, Reason
                    )}
            end
    end.

load_archive_modules(RootMod, Modules, Msg, Opts) ->
    case code:atomic_load(Modules) of
        ok ->
            case run_archive_on_loads(Msg, Opts) of
                ok -> {ok, RootMod};
                {error, Reason} ->
                    {error, load_error(
                        <<"on-load-failed">>, RootMod, Reason)}
            end;
        {error, Reason} ->
            case archive_loaded(Modules) of
                true -> {ok, RootMod};
                false ->
                    {error, load_error(
                        <<"archive-load-failed">>, RootMod, Reason
                    )}
            end
    end.

%% @doc Build an AO-message-shaped load error.
load_error(Code, Ref) ->
    #{ <<"error">> => Code, <<"device">> => hb_util:bin(Ref) }.

load_error(Code, Ref, Reason) ->
    (load_error(Code, Ref))#{ <<"reason">> => reason_bin(Reason) }.

reason_bin(Reason) when is_binary(Reason) ->
    Reason;
reason_bin(Reason) when is_atom(Reason) ->
    hb_util:bin(Reason);
reason_bin(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% @doc Check that every module in an archive is present in the code server.
archive_loaded(Modules) ->
    lists:all(fun({Mod, _, _}) -> code:is_loaded(Mod) =/= false end, Modules).

%% @doc Execute the flat on-load metadata embedded in the implementation.
run_archive_on_loads(Msg, Opts) ->
    case hb_maps:get(<<"on-load">>, Msg, <<>>, Opts) of
        <<>> ->
            ok;
        OnLoad when is_binary(OnLoad) ->
            case hb_maps:get(<<"on-load-format">>, Msg, undefined, Opts) of
                ?ON_LOAD_FORMAT ->
                    case decode_archive_on_loads(OnLoad) of
                        {ok, OnLoads} ->
                            run_archive_on_load_list(OnLoads);
                        {error, _} = Error ->
                            Error
                    end;
                Other ->
                    {error, {unsupported_on_load_format, Other}}
            end;
        Other ->
            {error, {invalid_on_load_metadata, Other}}
    end.

%% @doc Decode `{module,function}' pairs from a length-framed binary.
decode_archive_on_loads(Bin) when is_binary(Bin) ->
    decode_archive_on_loads(Bin, []).

decode_archive_on_loads(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
decode_archive_on_loads(<<ModLen:32, Rest0/binary>>, Acc)
        when byte_size(Rest0) >= ModLen + 4 ->
    <<ModBin:ModLen/binary, FunLen:32, Rest1/binary>> = Rest0,
    case byte_size(Rest1) >= FunLen of
        true ->
            <<FunBin:FunLen/binary, Rest2/binary>> = Rest1,
            decode_archive_on_loads(
                Rest2,
                [#{
                    <<"module-name">> => ModBin,
                    <<"function">> => FunBin
                } | Acc]
            );
        false ->
            {error, invalid_on_load_metadata}
    end;
decode_archive_on_loads(_Other, _Acc) ->
    {error, invalid_on_load_metadata}.

%% @doc Run decoded on-load callbacks in package order.
run_archive_on_load_list([]) ->
    ok;
run_archive_on_load_list([#{ <<"module-name">> := ModBin,
                             <<"function">> := FunBin } | Rest]) ->
    Mod = hb_util:key_to_atom(ModBin, existing),
    Fun = hb_util:key_to_atom(FunBin, existing),
    case apply(Mod, Fun, []) of
        ok -> run_archive_on_load_list(Rest);
        Other -> {error, {on_load_failed, Mod, Fun, Other}}
    end.

%% @doc Return the extracted implementation directory for a generated device.
implementation_dir(Module) when is_atom(Module) ->
    Root = generated_root(Module),
    persistent_term:get(
        {?MODULE, implementation_dir, Root},
        filename:join(implementation_root(), atom_to_list(Root))
    ).

prepare_implementation_dir(_RootMod, _ImplementationID, [], _Opts) ->
    ok;
prepare_implementation_dir(RootMod, ImplementationID, Files, Opts) ->
    Root = generated_root(RootMod),
    Dir = filename:join(
        implementation_root(Opts),
        hb_util:list(ImplementationID)
    ),
    case write_implementation_files(Dir, Files) of
        ok ->
            persistent_term:put({?MODULE, implementation_dir, Root}, Dir),
            ok;
        {error, _} = Error ->
            Error
    end.

implementation_root() ->
    case os:getenv("HB_DEVICE_IMPLEMENTATION_DIR") of
        false -> filename:absname(?DEFAULT_IMPLEMENTATION_DIR);
        Dir -> Dir
    end.

implementation_root(Opts) ->
    hb_util:list(
        hb_opts:get(
            <<"device-implementation-dir">>,
            implementation_root(),
            Opts
        )
    ).

write_implementation_files(_Dir, []) ->
    ok;
write_implementation_files(Dir, [{Rel, Body} | Rest]) ->
    Path = filename:join(Dir, hb_util:list(Rel)),
    case filelib:ensure_dir(Path) of
        ok ->
            case file:write_file(Path, Body) of
                ok ->
                    maybe_make_executable(Rel, Path),
                    write_implementation_files(Dir, Rest);
                {error, Reason} ->
                    {error, {resource_write_failed, Rel, Reason}}
            end;
        {error, Reason} ->
            {error, {resource_dir_failed, Rel, Reason}}
    end.

maybe_make_executable(<<"bin/", _/binary>>, Path) ->
    file:change_mode(Path, 8#100755);
maybe_make_executable(Rel, Path) ->
    case filename:extension(hb_util:list(Rel)) of
        ".sh" -> file:change_mode(Path, 8#100755);
        _ -> ok
    end.

generated_root(Module) ->
    [Root | _] = binary:split(atom_to_binary(Module, utf8), <<"__">>),
    binary_to_atom(Root, utf8).

%%% --------------------------------------------------------------------
%%% Compatibility checks
%%% --------------------------------------------------------------------

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
                SystemValue = erlang:system_info(Property),
                Res = hb_ao:normalize_key(SystemValue) == hb_ao:normalize_key(Value),
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

%%% --------------------------------------------------------------------
%%% Device-store cache (key -> generated module atom)
%%% --------------------------------------------------------------------

-define(DEV_CACHE_PREFIX, <<"devices/">>).

%% @doc Look up a previously-resolved generated module atom for a name
%% or specification ID. Returns `not_found' if either the cache miss or
%% the cached BEAM is no longer loaded.
lookup_device_cache(Ref, Opts) ->
    case erlang:get({?MODULE, device_cache, Ref}) of
        ModName when is_atom(ModName), ModName =/= undefined ->
            case loaded_module(ModName) of
                {ok, _} = Ok -> Ok;
                not_found -> lookup_device_store(Ref, Opts)
            end;
        _ ->
            lookup_device_store(Ref, Opts)
    end.

lookup_device_store(Ref, Opts) ->
    case device_store(Opts) of
        undefined -> not_found;
        Store ->
            Key = <<?DEV_CACHE_PREFIX/binary, Ref/binary>>,
            case hb_store:read(Store, Key, Opts) of
                {ok, ModBin} ->
                    case lookup_cached_module(ModBin) of
                        {ok, ModName} = Ok ->
                            erlang:put({?MODULE, device_cache, Ref}, ModName),
                            Ok;
                        not_found ->
                            not_found
                    end;
                _ -> not_found
            end
    end.

%% @doc Turn a cached generated module binary into a loaded atom.
lookup_cached_module(ModBin) when is_binary(ModBin) ->
    case hb_device_name:is_generated(ModBin) of
        false -> not_found;
        true ->
            try hb_util:key_to_atom(ModBin, existing) of
                ModName -> loaded_module(ModName)
            catch _:_ -> not_found
            end
    end;
lookup_cached_module(_ModBin) ->
    not_found.

%% @doc Check whether a generated module atom is already loaded.
loaded_module(ModName) ->
    case code:is_loaded(ModName) of
        {file, _} -> {ok, ModName};
        false -> not_found
    end.

%% @doc Cache a device name or specification ID as a generated module binary.
cache_device_module(Ref, ModName, Opts) when is_atom(ModName) ->
    erlang:put({?MODULE, device_cache, Ref}, ModName),
    cache_device_module(Ref, hb_util:bin(ModName), Opts);
cache_device_module(Ref, ModBin, Opts) when is_binary(Ref), is_binary(ModBin) ->
    case device_store(Opts) of
        undefined -> ok;
        Store ->
            hb_store:write(
                Store,
                #{ <<?DEV_CACHE_PREFIX/binary, Ref/binary>> => ModBin },
                Opts
            )
    end.

%%% --------------------------------------------------------------------
%%% Store helpers
%%% --------------------------------------------------------------------

%% @doc The fast volatile cache of name/ID -> loaded module atom.
device_store(Opts) ->
    hb_opts:get(device_store, undefined, Opts).

%% @doc The build-time store containing preloaded specs/impls + index.
preloaded_store(Opts) ->
    hb_opts:get(preloaded_store, undefined, node_config_opts(Opts)).

%% @doc Read node configuration even from request-local resolver contexts.
node_config_opts(Opts) ->
    maps:without([<<"cache-control">>, <<"only">>, <<"prefer">>], Opts).

%% @doc Load the preloaded resolver message with an in-memory primitive device.
preloaded_resolver(Opts) ->
    NodeOpts = node_config_opts(Opts),
    Store = preloaded_store(NodeOpts),
    IndexID = hb_opts:get(preloaded_devices_index, undefined, NodeOpts),
    case Store =:= undefined orelse IndexID =:= undefined of
        true ->
            not_found;
        false ->
            ReadOpts = NodeOpts#{
                    <<"store">> => Store,
                    <<"cache-read-mode">> => raw
            },
            case hb_cache:read(IndexID, ReadOpts) of
                {ok, Resolver} ->
                    {Resolver#{
                            <<"device">> => primitive_name_resolver(ReadOpts)},
                        with_preloaded_store(NodeOpts)
                    };
                _ ->
                    not_found
            end
    end.

%% @doc Prepend the preloaded resolver to name resolution.
with_preloaded_name_resolver(Opts) ->
    case preloaded_resolver(Opts) of
        {Resolver, ResolverOpts} ->
            Resolvers = hb_opts:get(name_resolvers, [], ResolverOpts),
            ResolverOpts#{
                <<"name-resolvers">> => [Resolver | listify(Resolvers)]
            };
        not_found ->
            Opts
    end.

%% @doc Minimal resolver device for a preloaded name->spec resolver message.
primitive_name_resolver(ReadOpts) ->
    #{
        info =>
            fun() ->
                #{ default =>
                    fun(Key, Base, _Req, _Opts) ->
                        case hb_maps:get(Key, Base, not_found, ReadOpts) of
                            not_found -> not_found;
                            Value -> {ok, Value}
                        end
                    end
                }
            end
    }.

%% @doc Build an Opts map whose store list has the preloaded-store
%% prepended, so cache reads/matches see preloaded artifacts first.
with_preloaded_store(Opts) ->
    case preloaded_store(Opts) of
        undefined -> Opts;
        Pre ->
            Existing = hb_opts:get(store, [], Opts),
            Opts#{ <<"store">> => [Pre | listify(Existing)] }
    end.

%% @doc Normalize store configuration to a list for lookup ordering.
listify(L) when is_list(L) -> L;
listify(M) when is_map(M) -> [M];
listify(undefined) -> [];
listify(X) -> [X].

%% @doc Return trusted implementation message IDs.
trusted_devices(Opts) ->
    case hb_opts:get(trusted_devices, [], Opts) of
        IDs when is_list(IDs) -> [hb_util:bin(ID) || ID <- IDs];
        _ -> []
    end.

%% @doc Return configured trusted signers or the node-wallet default.
trusted_signers(Opts) ->
    case hb_opts:get(trusted_device_signers, [], Opts) of
        [] -> default_trusted_signers(Opts);
        Configured -> Configured
    end.

%% @doc The production default is the node wallet address.
default_trusted_signers(Opts) ->
    try
        KeyLoc = hb_opts:get(
            priv_key_location,
            hb_opts:get(priv_key_location),
            Opts
        ),
        [hb:address(hb:wallet(
            KeyLoc
        ))]
    catch _:_ -> [] end.

%% @doc Determine whether an implementation ID is explicitly trusted.
is_trusted_device(undefined, _Opts) ->
    false;
is_trusted_device(ID, Opts) ->
    lists:member(hb_util:bin(ID), trusted_devices(Opts)).

%% @doc Determine whether an implementation signer is trusted.
is_signer_trusted([], _TrustedSigners) ->
    false;
is_signer_trusted(_Signers, all) ->
    true;
is_signer_trusted(Signers, List) when is_list(List) ->
    case lists:member(all, List) of
        true -> true;
        false ->
            lists:any(
                fun(Signer) -> lists:member(Signer, List) end,
                Signers
            )
    end;
is_signer_trusted(_Signers, _TrustedSigners) ->
    false.

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

%% @doc The default device is the identity device. We refer to it by
%% its public name, `message@1.0', so it is resolved through the
%% normal {@link load/2} path and ends up as the generated
%% `_hb_device_message_*' module rather than the source `dev_message'.
default() -> <<"message@1.0">>.
