%%% @doc Static TPM-interpretation database loader.
%%%
%%% Loads the set of JSON files under `priv/tpm-interpret/' into an
%%% in-memory map once per node, then serves lookups. The database
%%% is treated as immutable data — rebuilding the release is how you
%%% update it. The layout is intentionally a directory of small JSON
%%% files (one per known firmware/UKI/manufacturer) so a reviewer can
%%% add new entries without touching code.
%%%
%%% File formats
%%%
%%%     manufacturers.json          {"vendors": {"49465800": {...}, ...}}
%%%     pcr-profiles/*.json         {"name":..., "pcrs": {"0":"<hex>", ...},
%%%                                  "notes":..., "source":...}
%%%     uki-measurements/*.json     {"name":..., "pcrs": {"11":"<hex>",
%%%                                  "12":"<hex>", ...}, ...}
%%%
%%% The public contract is a single map:
%%%
%%%     #{
%%%         vendors       => #{ <<"HEXID">> => VendorEntry, ... },
%%%         pcr_profiles  => #{ <<"file-name">> => ProfileEntry, ... },
%%%         uki_profiles  => #{ <<"file-name">> => UkiEntry, ... },
%%%         cert_roots    => [ #{name, pem, ...}, ... ]
%%%     }
-module(hb_db_tpm).
-export([load/1, priv_dir/0]).

-define(APPNAME, hb).
-define(DB_SUBDIR, "tpm-interpret").
-define(CACHE_KEY, {hb_db_tpm, loaded}).

%%%============================================================================
%%% Public API
%%%============================================================================

%% @doc Load (or return the cached) database. Safe to call from any
%% process; backed by `persistent_term' for O(1) lookup.
load(_Opts) ->
    case persistent_term:get(?CACHE_KEY, undefined) of
        undefined ->
            Db = load_fresh(),
            persistent_term:put(?CACHE_KEY, Db),
            Db;
        Db -> Db
    end.

priv_dir() ->
    case code:priv_dir(?APPNAME) of
        {error, _} ->
            %% Fallback for dev builds where priv/ isn't via
            %% code:priv_dir (same pattern as lapee_tpm_nif).
            filename:join([filename:dirname(
                filename:dirname(code:which(?MODULE))), "priv"]);
        Dir -> Dir
    end.

%%%============================================================================
%%% Loading
%%%============================================================================

load_fresh() ->
    Root = filename:join(priv_dir(), ?DB_SUBDIR),
    #{
        <<"vendors">> =>
            read_json_map(filename:join(Root, "manufacturers.json"),
                          <<"vendors">>),
        <<"event_types">> =>
            read_json_map(filename:join(Root, "event-types.json"),
                          <<"types">>),
        <<"pcr_profiles">> =>
            read_dir_of_json(filename:join(Root, "pcr-profiles")),
        <<"uki_profiles">> =>
            read_dir_of_json(filename:join(Root, "uki-measurements")),
        <<"firmware_versions">> =>
            read_dir_of_json(filename:join(Root, "firmware-versions")),
        <<"cert_roots">> =>
            read_cert_roots(filename:join(Root, "root-cas"))
    }.

read_json_map(Path, InnerKey) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            try
                Decoded = json:decode(Bin),
                maps:get(InnerKey, Decoded, #{})
            catch _:_ -> #{}
            end;
        _ -> #{}
    end.

read_dir_of_json(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            maps:from_list(
                [{list_to_binary(filename:rootname(F)),
                  read_json(filename:join(Dir, F))}
                 || F <- Files, filename:extension(F) =:= ".json"]);
        _ -> #{}
    end.

read_json(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            try json:decode(Bin) catch _:_ -> #{} end;
        _ -> #{}
    end.

read_cert_roots(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            [#{<<"name">> => list_to_binary(filename:rootname(F)),
               <<"pem">>  => element(2, file:read_file(
                                filename:join(Dir, F)))}
             || F <- Files, filename:extension(F) =:= ".pem"];
        _ -> []
    end.
