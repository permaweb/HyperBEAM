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
-module(hb_device_load).
-export([reference/2]).
-include("include/hb.hrl").

%% @doc A message is already a device. A binary reference is resolved,
%% then memoised in the process cache unless it is a forge seed.
reference(Loaded, _Opts) when is_map(Loaded) ->
    {ok, Loaded};
reference(Ref, Opts) when is_binary(Ref) ->
    NormRef = hb_ao:normalize_key(Ref),
    case from_forge_bootstrap(NormRef, Opts) of
        {ok, Mod} -> {ok, Mod};
        {error, not_found} -> resolve_cached(NormRef, Opts);
        {error, Err} -> {error, Err}
    end.

resolve_cached(Ref, Opts) ->
    case resolve(Ref, Opts) of
        {cached, Mod} -> {ok, Mod};
        {ok, Mod} = Ok -> put_resolved_device(Ref, Mod, Opts), Ok;
        {error, Err} -> {error, Err}
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

%% @doc Look up a previously-resolved module: the process dictionary
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
from_trusted_devices(Ref, Opts) ->
    case hb_opts:get(trusted_devices, #{}, Opts) of
        #{ Ref := ID } when ?IS_ID(ID) -> load_archive(ID, Opts);
        _ -> {error, not_found}
    end.

%% @doc Resolve the device's spec ID from the flat preloaded index (or
%% use the reference directly if it is already an ID), then load the
%% first implementation that declares `implements-device' for it. The
%% read is codec-free: the codecs are themselves preloaded packages,
%% so decoding their messages cannot depend on a loaded codec. The
%% preloaded store is build-signed, so no signature check is needed.
from_preloaded(Ref, Opts) ->
    case preloaded(Opts) of
        undefined ->
            {error, not_found};
        {Store, IndexID} ->
            PreOpts =
                Opts#{ <<"store">> => [Store], <<"cache-read-mode">> => raw },
            maybe
                {ok, SpecID} ?= preloaded_spec(Ref, Store, IndexID, PreOpts),
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
            end
    end.

preloaded_spec(Ref, _Store, _IndexID, _Opts) when ?IS_ID(Ref) ->
    {ok, Ref};
preloaded_spec(Ref, Store, IndexID, Opts) ->
    hb_store:read(Store, <<IndexID/binary, "/", Ref/binary>>, Opts).

%% @doc The preloaded store and its signed index ID, from node config
%% (request-local cache keys stripped so it is visible inside a
%% request-scoped resolution).
preloaded(Opts) ->
    Node = maps:without([<<"cache-control">>, <<"only">>, <<"prefer">>], Opts),
    case
        {
            hb_opts:get(preloaded_store, undefined, Node),
            hb_opts:get(preloaded_devices_index, undefined, Node)
        }
    of
        {Store, IndexID} when Store =/= undefined, IndexID =/= undefined ->
            {Store, IndexID};
        _ ->
            undefined
    end.

%%% --------------------------------------------------------------------
%%% Low trust
%%% --------------------------------------------------------------------

%% @doc Resolve the name through `name@1.0' (safe here -- the codecs are
%% already loaded via the high-trust path), then load the first signed,
%% compatible implementation. Local caches are always searched -- gateway
%% lookup is gated by `load-remote-devices'.
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
        RemoteIterators =
            case hb_opts:get(<<"load-remote-devices">>, false, Opts) of
                true ->
                    [
                        fun() ->
                            hb_util:ok_or(
                                hb_client_gateway:device(
                                    SpecID,
                                    trusted_signers(Opts),
                                    Opts
                                ),
                                []
                            )
                        end
                    ];
                false ->
                    []
            end,
        lazy_first(
            fun(ID) -> verify_and_load(SpecID, ID, Opts) end,
            LocalIterators ++ RemoteIterators
        )
    end.

resolve_spec(Ref, _Opts) when ?IS_ID(Ref) ->
    {ok, Ref};
resolve_spec(Ref, Opts) ->
    case
        hb_ao:raw(
            #{ <<"device">> => <<"name@1.0">> },
            #{ <<"path">> => Ref, <<"load">> => false },
            Opts
        )
    of
        {ok, SpecID} when ?IS_ID(SpecID) -> {ok, SpecID};
        _ -> {error, <<"device-name-not-resolvable">>}
    end.

%% @doc A low-trust implementation must be signed by a trusted signer,
%% implement the requested specification, and be machine-compatible.
verify_and_load(SpecID, ID, Opts) ->
    maybe
        {ok, Msg} ?= hb_cache:read(ID, Opts),
        Signers = signers(Msg, Opts),
        true ?=
            hb_message:verify(Msg, Signers, Opts)
                orelse {error, <<"implementation-signature-invalid">>},
        true ?=
            lists:any(
                fun(S) -> lists:member(S, trusted_signers(Opts)) end,
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
    hb_device_archive:load(
        hb_maps:get(<<"module-name">>, Msg, undefined, Opts),
        hb_maps:get(<<"body">>, Msg, undefined, Opts),
        Msg,
        Opts
    ).

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
%% @doc Trusted signers, defaulting to the node's own address.
%% Computed lazily so the default config need not call `hb:address/0'.
trusted_signers(Opts) ->
    case hb_opts:get(trusted_device_signers, [], Opts) of
        [] -> [hb:address()];
        Signers when is_list(Signers) -> Signers
    end.

%% @doc Every `requires-*' key must match this machine's `system_info'.
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
                        hb_ao:normalize_key(erlang:system_info(Prop))
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
