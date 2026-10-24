%%% @doc Resolve a device reference to its Erlang module.
%%%
%%% Two fundamental, mutually exclusive modes of device loading:
%%%
%%% <b>Forge build.</b> If `forge-bootstrap' maps a device name to a
%%% loaded module, that module is returned directly and never cached.
%%% Only the Forge build sets it, supplying the seed codecs under their
%%% module names so it can compute IDs and sign the preloaded messages.
%%%
%%% <b>Runtime.</b> Every device is loaded from an archive of compiled
%%% BEAM modules. Each module name is rewritten at compile time to the
%%% AO-Core ID of the source message that defined it, so every
%%% implementation has its own namespace and versions never conflict.
%%% Implementations are sourced most-trusted first:
%%%
%%% <ol>
%%%   <li><b>High trust</b> (no signature check -- trusted as the
%%%       runtime itself): the process cache; the operator's
%%%       `trusted-devices' map; the build-signed preloaded store.</li>
%%%   <li><b>Low trust</b> (verified on first use): names resolved
%%%       through the node's wider caches and Arweave, each
%%%       implementation requiring a `trusted-device-signers'
%%%       signature.</li>
%%% </ol>
%%%
%%% The loader also gives its preloaded store (`store/1'), the signed
%%% specification of a preloaded device (`spec/2'), and every implementation
%%% of a specification in its sources (`implementations/2').
-module(hb_device_load).
-export([reference/2, schema/2, store/1, spec/2, implementations/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc A message is already a device. A binary reference is resolved,
%% then memoised in the process cache unless it is a forge seed.
%% An empty binary or any other value returns `{error, not_found}'.
reference(Loaded, _Opts) when is_map(Loaded) ->
    {ok, Loaded};
reference(Ref, _Opts) when Ref == <<>>; not is_binary(Ref) ->
    {error, not_found};
reference(Ref, Opts) when is_binary(Ref) ->
    NormRef = hb_ao:normalize_key(Ref),
    case from_forge_bootstrap(NormRef, Opts) of
        {ok, Mod} -> {ok, Mod};
        {error, not_found} -> resolve_cached(NormRef, Opts);
        {error, Err} -> {error, Err}
    end.

do_not_try_to_load_empty_device_test() ->
    ?assertEqual({error, not_found}, reference(<<>>, #{})),
    % `false' is not a device, and `name@1.0' gets the name `%zz' as it is.
    ?assertEqual({error, not_found}, reference(false, #{})),
    ?assertEqual(
        {error, <<"device-name-not-resolvable">>},
        reference(<<"%zz">>, #{})
    ).

%% @doc Resolve and memoise a reference, refusing re-entrant resolutions:
%% a reference whose resolution requires resolving itself (for example,
%% `name@1.0' resolving its own name when it is absent from every
%% high-trust source) can never succeed, only recurse. The references
%% being resolved are carried in `Opts', scoping them to this resolution
%% tree.
resolve_cached(Ref, Opts) ->
    Resolving = maps:get(<<"resolving-devices">>, Opts, []),
    case lists:member(Ref, Resolving) of
        true ->
            {error,
                <<
                    "Circular device resolution detected for reference `",
                    Ref/binary,
                    "`. Check `preloaded-store` viability."
                >>
            };
        false ->
            ResOpts = Opts#{ <<"resolving-devices">> => [Ref | Resolving] },
            case resolve(Ref, ResOpts) of
                {cached, Mod} -> {ok, Mod};
                {ok, Mod} = Ok -> put_resolved_device(Ref, Mod, Opts), Ok;
                {error, Err} -> {error, Err}
            end
    end.

%% @doc The resolved-device store, then the high-trust sources, then the
%% low-trust sources. The first `{ok, _}' wins; a real error from a
%% trusted source is returned rather than falling through.
resolve(Ref, Opts) ->
    maybe
        {error, not_found} ?= get_resolved_device(Ref, Opts),
        {error, not_found} ?= from_high_trust(Ref, Opts),
        from_low_trust(Ref, Opts)
    end.

%% @doc Look up an already-resolved module: the process dictionary
%% first, then the node's `loaded-device-store'. The first process to
%% resolve a device spares every other the index read and archive
%% extraction. The store defaults to `[]', which `hb_store' treats as
%% no viable store, so the shared tier disables itself with no branch.
get_resolved_device(Ref, Opts) ->
    case erlang:get({?MODULE, Ref}) of
        Mod when is_atom(Mod), Mod =/= undefined ->
            {cached, Mod};
        _ ->
            maybe
                {ok, Bin} ?=
                    hb_store:read(
                        loaded_device_store(Opts),
                        store_key(Ref),
                        Opts
                    ),
                Mod = hb_util:atom(Bin),
                % We always stash in the process dictionary, despite the fact
                % the reference was already in the global store.
                erlang:put({?MODULE, Ref}, Mod),
                {cached, Mod}
            end
    end.

%% @doc Memoise a resolved device in the process dictionary and the
%% shared `loaded-device-store'.
put_resolved_device(Ref, Mod, Opts) ->
    erlang:put({?MODULE, Ref}, Mod),
    hb_store:write(
        loaded_device_store(Opts),
        #{ store_key(Ref) => hb_util:bin(Mod) },
        Opts
    ).

loaded_device_store(Opts) -> hb_opts:get(loaded_device_store, [], Opts).

store_key(Ref) -> <<"~meta@1.0/devices/", Ref/binary>>.

%%% --------------------------------------------------------------------
%%% High trust
%%% --------------------------------------------------------------------

from_high_trust(Ref, Opts) ->
    maybe
        {error, not_found} ?= from_trusted_devices(Ref, Opts),
        from_preloaded(Ref, Opts)
    end.

%% @doc Forge-only map from seed device name to source module atom.
from_forge_bootstrap(Ref, Opts) ->
    case hb_opts:get(forge_bootstrap, #{}, Opts) of
        #{ Ref := Mod } when is_atom(Mod) -> {ok, Mod};
        Seeds when is_map(Seeds), map_size(Seeds) > 0 ->
            {error, {forge_bootstrap_device_not_found, Ref}};
        _ -> {error, not_found}
    end.

%% @doc Operator-pinned map from device name or spec ID to implementation ID.
%% Its keys and the reference are compared lowercase, as a config file loads
%% the keys.
from_trusted_devices(Ref, Opts) ->
    Trusted =
        maps:fold(
            fun(Key, ID, Acc) -> Acc#{ hb_util:lowercase_key(Key) => ID } end,
            #{},
            hb_opts:get(trusted_devices, #{}, Opts)
        ),
    case maps:find(hb_util:lowercase_key(Ref), Trusted) of
        {ok, ID} when ?IS_ID(ID) -> load_archive(ID, Opts);
        _ -> {error, not_found}
    end.

%% @doc Resolve the device's spec ID from the flat preloaded index (or
%% use the reference directly if it is already an ID), then load the
%% first implementation that declares `implements-device' for it. The
%% read is codec-free: the codecs are themselves preloaded packages,
%% so decoding their messages cannot depend on a loaded codec. The
%% preloaded store is build-signed, so no signature check is needed.
from_preloaded(Ref, Opts) ->
    Store = store(Opts),
    PreOpts = Opts#{ <<"store">> => Store, <<"cache-read-mode">> => raw },
    maybe
        {ok, SpecID} ?= preloaded_spec(Ref, Store, PreOpts),
        lazy_first(
            fun(ID) -> load_archive(ID, PreOpts) end,
            [
                fun() ->
                    hb_util:ok_or(
                        hb_cache:match(implementation_query(SpecID), PreOpts),
                        []
                    )
                end
            ]
        )
    end.

preloaded_spec(Ref, _Store, _Opts) when ?IS_ID(Ref) ->
    {ok, Ref};
preloaded_spec(Ref, Store, Opts) ->
    % The index key holds `ao:' and the ID of the index, whose keys are the
    % device names.
    case hb_store:read(Store, ?PRELOADED_INDEX_KEY, Opts) of
        {ok, <<"ao:", IndexID/binary>>} ->
            Key = hb_escape:encode_path_component(Ref),
            hb_cache:read(<<IndexID/binary, "/", Key/binary>>, Opts);
        _ ->
            {error, not_found}
    end.

%% @doc Device names are literal members of the preloaded index.
preloaded_literal_names_test() ->
    Store = hb_test_utils:test_store(hb_store_volatile),
    Opts = #{ <<"store">> => Store, <<"cache-read-mode">> => raw },
    Index = #{
        <<"test/device@1.0">> => hb_util:human_id(crypto:strong_rand_bytes(32)),
        <<"test%2fdevice@1.0">> => hb_util:human_id(crypto:strong_rand_bytes(32))
    },
    {ok, ID} = hb_cache:write(Index, Opts),
    hb_cache:link(ID, ?PRELOADED_INDEX_KEY, Opts),
    maps:foreach(
        fun(Name, SpecID) ->
            ?assertEqual({ok, SpecID}, preloaded_spec(Name, Store, Opts))
        end,
        Index
    ),
    hb_store:stop(Store).

%% @doc Return the preloaded store, with request-local cache keys stripped so
%% it is visible inside a request-scoped resolution. Without one, the store is
%% `[]', which `hb_store' treats as no viable store.
store(Opts) ->
    Node = maps:without([<<"cache-control">>, <<"only">>, <<"prefer">>], Opts),
    hb_opts:get(preloaded_store, [], Node).

%%% --------------------------------------------------------------------
%%% Low trust
%%% --------------------------------------------------------------------

%% @doc Resolve the name through `name@1.0' (safe: a resolution that
%% re-enters itself is refused by `resolve_cached/2'), then load the first
%% signed, compatible implementation. Local caches are always searched --
%% gateway lookup is gated by an explicit `trusted-device-signers' list.
from_low_trust(Ref, Opts) ->
    maybe
        {ok, SpecID} ?= resolve_spec(Ref, Opts),
        LocalIterators =
            [
                fun() ->
                    hb_util:ok_or(
                        hb_cache:match(implementation_query(SpecID), Opts),
                        []
                    )
                end
            ],
        RemoteSigners =
            case hb_opts:get(trusted_device_signers, [], Opts) of
                [_ | _] = Signers ->
                    trusted_signer_entries(Ref, SpecID, Signers, Opts);
                _ ->
                    []
            end,
        RemoteIterators =
            case RemoteSigners of
                [_ | _] ->
                    [
                        fun() ->
                            hb_util:ok_or(
                                hb_client_gateway:device(
                                    SpecID,
                                    RemoteSigners,
                                    Opts
                                ),
                                []
                            )
                        end
                    ];
                _ ->
                    []
            end,
        lazy_first(
            fun(ID) -> verify_and_load(Ref, SpecID, ID, Opts) end,
            LocalIterators ++ RemoteIterators
        )
    end.

resolve_spec(Ref, _Opts) when ?IS_ID(Ref) ->
    {ok, Ref};
resolve_spec(Ref, Opts) ->
    case
        hb_ao:raw(
            #{ <<"device">> => <<"name@1.0">> },
            #{
                <<"path">> => hb_escape:encode_path_component(Ref),
                <<"load">> => false
            },
            Opts
        )
    of
        {ok, SpecID} when ?IS_ID(SpecID) -> {ok, SpecID};
        _ -> {error, <<"device-name-not-resolvable">>}
    end.

%% @doc A low-trust implementation must be signed by a trusted signer,
%% implement the requested specification, and be machine-compatible.
verify_and_load(Ref, SpecID, ID, Opts) ->
    maybe
        {ok, Msg} ?= hb_cache:read(ID, Opts),
        Signers = signers(Msg, Opts),
        true ?=
            hb_message:verify(Msg, Signers, Opts)
                orelse {error, <<"implementation-signature-invalid">>},
        true ?=
            lists:any(
                fun(S) -> lists:member(S, trusted_signers(Ref, SpecID, Opts)) end,
                Signers
            ) orelse {error, <<"device-signer-untrusted">>},
        ok ?= implements(SpecID, Msg, Opts),
        ok ?= compatible(Msg, Opts),
        load_archive_message(Msg, Opts)
    end.

%% @doc Apply `F' to each element produced by the iterators in turn,
%% returning the first `{ok, _}'.
lazy_first(F, Iterators) ->
    lazy_first(F, [], Iterators).

lazy_first(_F, [], []) ->
    {error, not_found};
lazy_first(F, [], [Next | Rest]) ->
    lazy_first(F, Next(), Rest);
lazy_first(F, [X | Xs], Iterators) ->
    case F(X) of
        {ok, _} = Ok -> Ok;
        {error, _} -> lazy_first(F, Xs, Iterators)
    end.

%%% --------------------------------------------------------------------
%%% Archive loading, verification and shared helpers
%%% --------------------------------------------------------------------

%% @doc Load an implementation by its archive ID, trusting it (the
%% caller is a high-trust source).
load_archive(ID, Opts) ->
    maybe
        {ok, Msg} ?= hb_cache:read(ID, Opts),
        load_archive_message(Msg, Opts)
    end.

load_archive_message(Msg, Opts) ->
    Archive = hb_maps:get(<<"body">>, Msg, undefined, Opts),
    maybe
        {ok, Module} ?=
            hb_device_archive:load(
                hb_maps:get(<<"module-name">>, Msg, undefined, Opts),
                Archive,
                Msg,
                Opts
            ),
        put_schemas(Archive, Opts),
        {ok, Module}
    end.

%% @doc The function schemas of a loaded module (see `hb_types'): the
%% process dictionary first, then the shared `loaded-device-store', then
%% the module's own object code. A generated module name carries the hash
%% of the source that built it, so the atom alone identifies its schemas.
schema(Module, Opts) ->
    case erlang:get({?MODULE, schema, Module}) of
        undefined ->
            Schemas =
                maybe
                    {error, not_found} ?=
                        hb_store:read(
                            loaded_device_store(Opts),
                            schema_key(Module),
                            Opts
                        ),
                    hb_types:extract(Module)
                end,
            erlang:put({?MODULE, schema, Module}, Schemas),
            Schemas;
        Schemas ->
            Schemas
    end.

%% @doc Memoise the function schemas of every module in a loaded archive in
%% the process cache and shared `loaded-device-store'. Archive modules are
%% loaded from memory, so their BEAMs are in hand here alone. The
%% schemas are Erlang terms: the store must be an `hb_store_volatile'.
put_schemas(Archive, Opts) ->
    {ok, Modules, _Resources} = hb_device_archive:contents(Archive),
    hb_store:write(
        loaded_device_store(Opts),
        maps:from_list(
            [
                begin
                    erlang:put({?MODULE, schema, Module}, {ok, Schemas}),
                    {schema_key(Module), Schemas}
                end
            ||
                {Module, _Path, Beam} <- Modules,
                {ok, Schemas} <- [hb_types:extract(Beam)]
            ]
        ),
        Opts
    ).

schema_key(Module) ->
    <<"~meta@1.0/devices/schemas/", (hb_util:bin(Module))/binary>>.

%% @doc Return the specification of a preloaded device: the signed
%% `Device-Specification' message that the preloaded index gives for its name,
%% read from the preloaded store.
spec(Ref, Opts) ->
    Store = store(Opts),
    PreOpts = Opts#{ <<"store">> => Store },
    maybe
        {ok, SpecID} ?=
            preloaded_spec(hb_ao:normalize_key(Ref), Store, PreOpts),
        hb_cache:read(SpecID, PreOpts)
    end.

%% @doc Return every implementation ID of a specification in the sources of the
%% loader: the preloaded store, the node's stores, then the items of the
%% trusted signers on Arweave. The loader loads only the first of them that it
%% can load.
implementations(SpecID, Opts) ->
    Query = implementation_query(SpecID),
    PreOpts = Opts#{ <<"store">> => store(Opts), <<"cache-read-mode">> => raw },
    Preloaded = hb_util:ok_or(hb_cache:match(Query, PreOpts), []),
    Local = hb_util:ok_or(hb_cache:match(Query, Opts), []),
    Remote =
        case
            trusted_signer_entries(
                SpecID,
                SpecID,
                hb_opts:get(trusted_device_signers, [], Opts),
                Opts
            )
        of
            [] -> [];
            Signers ->
                hb_util:ok_or(
                    hb_client_gateway:device(SpecID, Signers, Opts),
                    []
                )
        end,
    {ok, lists:uniq(Preloaded ++ Local ++ Remote)}.

implementation_query(SpecID) ->
    #{
        <<"data-protocol">> => <<"ao">>,
        <<"variant">> => <<"ao.N.1">>,
        <<"content-type">> => <<"application/beam-archive">>,
        <<"implements-device">> => SpecID
    }.

implements(SpecID, Msg, Opts) ->
    case hb_maps:get(<<"implements-device">>, Msg, undefined, Opts) of
        SpecID -> ok;
        Other -> {error, {<<"wrong-device-specification">>, Other}}
    end.

%% @doc The commitment signers, read inline rather than via
%% `hb_message:signers/2' to avoid invoking `message@1.0'.
signers(Msg, Opts) ->
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

%% @doc Return the trusted signers, defaulting to the node's own committer.
%% Computed lazily so the default config need not read the node's key.
trusted_signers(Ref, SpecID, Opts) ->
    [
        Address
    ||
        Signer <- trusted_signer_entries(Ref, SpecID, Opts),
        Address <- [trusted_signer_address(Signer, Opts)],
        Address =/= undefined
    ].

trusted_signer_entries(Ref, SpecID, Opts) ->
    trusted_signer_entries(Ref, SpecID, trusted_signer_entries(Opts), Opts).

trusted_signer_entries(Ref, SpecID, Signers, Opts) ->
    [
        Signer
    ||
        Signer <- Signers,
        trusted_signer_accepts(Ref, SpecID, Signer, Opts)
    ].

trusted_signer_entries(Opts) ->
    case hb_opts:get(trusted_device_signers, [], Opts) of
        [] -> [hb_opts:committer(Opts)];
        Signers when is_list(Signers) -> Signers
    end.

trusted_signer_accepts(_Ref, _SpecID, Signer, _Opts) when is_binary(Signer) ->
    true;
trusted_signer_accepts(Ref, SpecID, Signer, Opts) when is_map(Signer) ->
    case hb_maps:get(<<"devices">>, Signer, undefined, Opts) of
        undefined -> true;
        Devices when is_list(Devices) ->
            lists:member(Ref, Devices) orelse lists:member(SpecID, Devices);
        _ ->
            false
    end;
trusted_signer_accepts(_Ref, _SpecID, _Signer, _Opts) ->
    false.

trusted_signer_address(Signer, Opts) when is_binary(Signer) ->
    hb_committer:uri(Signer, Opts);
trusted_signer_address(Signer, Opts) when is_map(Signer) ->
    hb_committer:uri(hb_maps:get(<<"address">>, Signer, undefined, Opts));
trusted_signer_address(_Signer, _Opts) ->
    undefined.

trusted_signer_devices_test() ->
    Ref = <<"copycat@1.0">>,
    SpecID = <<"SPEC_ID">>,
    [Plain, Unscoped, ByRef, BySpec, Other] =
        [hb_util:human_id(<<N:256>>) || N <- lists:seq(1, 5)],
    ?assertEqual(
        [
            hb_committer:uri(Address)
        ||
            Address <- [Plain, Unscoped, ByRef, BySpec]
        ],
        trusted_signers(
            Ref,
            SpecID,
            #{
                <<"trusted-device-signers">> => [
                    Plain,
                    #{<<"address">> => Unscoped},
                    #{<<"address">> => ByRef, <<"devices">> => [Ref]},
                    #{<<"address">> => BySpec, <<"devices">> => [SpecID]},
                    #{
                        <<"address">> => Other,
                        <<"devices">> => [<<"arweave@2.9">>]
                    }
                ]
            }
        )
    ),
    % A signer that is neither an Arweave address nor a URI is not a
    % committer, and is kept as it is.
    ?assertEqual(
        [<<"plain">>],
        trusted_signers(
            Ref,
            SpecID,
            #{ <<"trusted-device-signers">> => [<<"plain">>] }
        )
    ),
    % With no signers configured, the node's committer is trusted.
    NodeOpts =
        #{
            <<"priv-wallet">> => ar_wallet:new(?ETHEREUM_KEY_TYPE),
            <<"trusted-device-signers">> => []
        },
    ?assertEqual(
        [hb_opts:committer(NodeOpts)],
        trusted_signers(Ref, SpecID, NodeOpts)
    ).

%% @doc Every `requires-*' key must match this machine:
%% `requires-ao-runtime-version' must be the version of the AO runtime that
%% this node implements, and every other key the value of the same name in its
%% `system_info'.
compatible(Msg, Opts) ->
    Failed =
        lists:filtermap(
            fun
                ({<<"requires-", Key/binary>>, Value}) ->
                    Prop =
                        hb_util:key_to_atom(
                            hb_ao:normalize_key(Key), new_atoms),
                    Want = hb_cache:ensure_loaded(Value, Opts),
                    case
                        hb_ao:normalize_key(requirement(Prop))
                            == hb_ao:normalize_key(Want)
                    of
                        true -> false;
                        false -> {true, {Prop, Want}}
                    end;
                (_) ->
                    false
            end,
            hb_maps:to_list(Msg, Opts)),
    case Failed of
        [] -> ok;
        _ -> {error, {failed_requirements, Failed}}
    end.

%% @doc Return the value of a requirement on this machine.
requirement(ao_runtime_version) -> ?AO_RUNTIME_VERSION;
requirement(Prop) -> erlang:system_info(Prop).

%% @doc An archive built for another AO runtime version is refused.
requires_ao_runtime_version_test() ->
    ?assertEqual(
        ok,
        compatible(
            #{ <<"requires-ao-runtime-version">> => ?AO_RUNTIME_VERSION },
            #{}
        )
    ),
    ?assertMatch(
        {error, {failed_requirements, [{ao_runtime_version, <<"0.10">>}]}},
        compatible(#{ <<"requires-ao-runtime-version">> => <<"0.10">> }, #{})
    ).

%% @doc Loading a device memoises its modules' schemas in the shared store,
%% where a process that never loaded the archive finds them. The schemas are a
%% message that the cache stores as it is, and the signed ID of the device's
%% specification is the one that its implementations declare.
schema_memoised_test() ->
    erlang:erase({?MODULE, <<"test-device@1.0">>}),
    Opts =
        #{
            <<"loaded-device-store">> =>
                [hb_test_utils:test_store(hb_store_volatile)]
        },
    {ok, Module} = reference(<<"test-device@1.0">>, Opts),
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() -> Parent ! {Ref, schema(Module, Opts)} end),
    receive
        {Ref, Schemas} ->
            ?assertMatch({ok, #{ <<"snapshot">> := [_] }}, Schemas)
    end,
    StoreOpts = Opts#{ <<"store">> => [hb_test_utils:test_store()] },
    {ok, ModuleSchemas} = schema(Module, Opts),
    {ok, SchemaID} = hb_cache:write(ModuleSchemas, StoreOpts),
    {ok, Read} = hb_cache:read(SchemaID, StoreOpts),
    ?assertEqual(ModuleSchemas, hb_cache:ensure_all_loaded(Read, StoreOpts)),
    {ok, Spec} = spec(<<"test-device@1.0">>, Opts),
    SpecID = hb_message:id(Spec, signed, Opts),
    ?assertMatch({ok, [_ | _]}, implementations(SpecID, StoreOpts)).

%% @doc Resolution against a preloaded store with no devices must fail
%% promptly with a circular-resolution error, not recurse through `name@1.0'.
circular_name_resolution_test() ->
    % Shed this (shared EUnit worker) process's memos of the devices used.
    erlang:erase({?MODULE, <<"message@1.0">>}),
    erlang:erase({?MODULE, <<"name@1.0">>}),
    Opts =
        #{
            <<"preloaded-store">> =>
                hb_test_utils:test_store(hb_store_lmdb, <<"preload-empty">>),
            <<"loaded-device-store">> => []
        },
    ?assertThrow(
        {device_not_loadable,
            <<"name@1.0">>,
            <<"Circular device resolution detected for reference "
                "`name@1.0`. Check `preloaded-store` viability.">>
        },
        reference(<<"message@1.0">>, Opts)
    ).
