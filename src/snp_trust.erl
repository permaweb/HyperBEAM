%%% @doc Software trust validation for SNP commitment reports.
%%%
%%% This module handles the validation of software configurations against
%%% trusted software lists, including filtering by enforced keys and matching
%%% against trusted configurations.
-module(snp_trust).
-export([execute_is_trusted/3, get_filtered_local_hashes/2, 
         get_enforced_keys/1, is_software_trusted/3]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").

%% @doc Validate that all software hashes match trusted configurations.
%%
%% This function ensures that the firmware, kernel, and other system components
%% in the SNP report match approved configurations. The validation process:
%% 1. Extracts local hashes from the message
%% 2. Filters hashes to only include enforced keys
%% 3. Compares filtered hashes against trusted software configurations
%% 4. Returns true only if the configuration matches a trusted entry
%%
%% Configuration options in NodeOpts map:
%% - snp_trusted: List of maps containing trusted software configurations
%% - snp_enforced_keys: Keys to enforce during validation (defaults to all 
%%   committed parameters)
%%
%% @param _M1 Ignored parameter
%% @param Msg The SNP message containing local software hashes
%% @param NodeOpts A map of configuration options including trusted software
%% @returns `{ok, true}' if software is trusted, `{ok, false}' otherwise
-spec execute_is_trusted(M1 :: term(), Msg :: map(), NodeOpts :: map()) ->
    {ok, boolean()}.
execute_is_trusted(_M1, Msg, NodeOpts) ->
    FilteredLocalHashes = get_filtered_local_hashes(Msg, NodeOpts),
    TrustedSoftware = hb_opts:get(snp_trusted, [#{}], NodeOpts),
    IsTrusted = 
        is_software_trusted(
            FilteredLocalHashes, 
            TrustedSoftware, 
            NodeOpts
        ),
    ?event(snp_short, {is_all_software_trusted, IsTrusted}),
    {ok, IsTrusted}.

%% @doc Extract local hashes filtered to only include enforced keys.
%%
%% This function retrieves the local software hashes from the message and
%% filters them to only include the keys that are configured for enforcement.
%% Local-hashes keys are normalized to binary so that atom-key and binary-key
%% maps are both handled correctly (avoids empty filter when key types differ).
%%
%% @param Msg The SNP message containing local hashes
%% @param NodeOpts A map of configuration options
%% @returns A map of filtered local hashes with only enforced keys (binary keys)
-spec get_filtered_local_hashes(Msg :: map(), NodeOpts :: map()) -> map().
get_filtered_local_hashes(Msg, NodeOpts) ->
    LocalHashesRaw = hb_ao:get(<<"local-hashes">>, Msg, NodeOpts),
    LocalHashes = normalize_map_keys_to_binary(LocalHashesRaw),
    EnforcedKeys = get_enforced_keys(NodeOpts),
    FilteredLocalHashes = hb_cache:ensure_all_loaded(
        maps:with(EnforcedKeys, LocalHashes),
        NodeOpts
    ),
    FilteredLocalHashes.

%% @doc Normalize a map so all keys are binaries (for consistent filtering with EnforcedKeys).
%% Non-map input is treated as empty map.
-spec normalize_map_keys_to_binary(term()) -> map().
normalize_map_keys_to_binary(M) when is_map(M) ->
    maps:fold(
        fun(K, V, Acc) ->
            maps:put(ensure_binary_key(K), V, Acc)
        end,
        #{},
        M
    );
normalize_map_keys_to_binary(_) ->
    #{}.

-spec ensure_binary_key(atom() | binary() | term()) -> binary().
ensure_binary_key(K) when is_binary(K) -> K;
ensure_binary_key(K) when is_atom(K) -> atom_to_binary(K, utf8);
ensure_binary_key(K) -> hb_util:bin(K).

%% @doc Get the list of enforced keys for software validation.
%%
%% This function retrieves the configuration specifying which software
%% component keys should be enforced during trust validation.
%%
%% @param NodeOpts A map of configuration options
%% @returns A list of binary keys that should be enforced
-spec get_enforced_keys(NodeOpts :: map()) -> [binary()].
get_enforced_keys(NodeOpts) ->
    lists:map(
        fun atom_to_binary/1,
        hb_opts:get(snp_enforced_keys, ?COMMITTED_PARAMETERS, NodeOpts)
    ).

%% @doc Check if filtered local hashes match any trusted configurations.
%%
%% This function compares the filtered local hashes against a list of
%% trusted software configurations, returning true if any configuration
%% matches exactly. It handles three cases:
%% 1. Empty list of trusted configurations (returns false)
%% 2. Valid list of trusted configurations (performs matching)
%% 3. Invalid trusted software configuration (returns false)
%%
%% @param FilteredLocalHashes The software hashes to validate
%% @param TrustedSoftware List of trusted software configurations or invalid input
%% @param NodeOpts Configuration options for matching
%% @returns `true' if hashes match a trusted configuration, `false' otherwise
-spec is_software_trusted(map(), [] | [map()] | term(), map()) -> boolean().
is_software_trusted(_FilteredLocalHashes, [], _NodeOpts) ->
    false;
is_software_trusted(FilteredLocalHashes, TrustedSoftware, NodeOpts) 
    when is_list(TrustedSoftware) ->
    lists:any(
        fun(TrustedMap) ->
            TrustedNormalized = normalize_map_keys_to_binary(TrustedMap),
            Match =
                hb_message:match(
                    FilteredLocalHashes,
                    TrustedNormalized,
                    primary,
                    NodeOpts
                ),
            is_map(TrustedMap) andalso Match == true
        end,
        TrustedSoftware
    );
is_software_trusted(_FilteredLocalHashes, _TrustedSoftware, _NodeOpts) ->
    false.

