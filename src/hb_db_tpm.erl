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
%%%     event-types.json            {"types": {"0x8": {...}, ...}}
%%%     pcr-profiles/*.json         {"name":..., "pcrs": {"0":"<hex>", ...},
%%%                                  "notes":..., "source":...}
%%%     uki-measurements/*.json     {"name":..., "match": {...},
%%%                                  "claims": {...}, ...}
%%%     firmware-versions/*.json    {"name":..., "match": {...},
%%%                                  "vendor":..., "trust-tier":..., ...}
%%%     cpu-models.json             {"intel": {"<fam>-<model>": {...}, ...},
%%%                                  "amd":   {"<fam>-<model>": {...}, ...}}
%%%     root-cas/*.pem              per-vendor EK root CA PEMs.
%%%
%%% The public contract is a single map (kebab-case keys on the wire):
%%%
%%%     #{
%%%         <<"vendors">>            => #{ <<"HEXID">> => VendorEntry, ... },
%%%         <<"event-types">>        => #{ ... },
%%%         <<"pcr-profiles">>       => #{ <<"file-name">> => ProfileEntry, ... },
%%%         <<"uki-profiles">>       => #{ <<"file-name">> => UkiEntry, ... },
%%%         <<"firmware-versions">>  => #{ <<"file-name">> => FwEntry, ... },
%%%         <<"cpu-models">>         => #{ <<"intel">> => #{...},
%%%                                         <<"amd">> => #{...}, ... },
%%%         <<"cert-roots">>         => [ #{name, pem, ...}, ... ]
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
        <<"event-types">> =>
            read_json_map(filename:join(Root, "event-types.json"),
                          <<"types">>),
        <<"pcr-profiles">> =>
            read_dir_of_json(filename:join(Root, "pcr-profiles")),
        <<"uki-profiles">> =>
            read_dir_of_json(filename:join(Root, "uki-measurements")),
        <<"firmware-versions">> =>
            read_dir_of_json(filename:join(Root, "firmware-versions")),
        <<"cpu-models">> =>
            read_json(filename:join(Root, "cpu-models.json")),
        <<"boot-images">> =>
            read_dir_of_json(filename:join(Root, "boot-images")),
        <<"ima-policies">> =>
            read_dir_of_json(filename:join(Root, "ima-policies")),
        <<"cert-roots">> =>
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
