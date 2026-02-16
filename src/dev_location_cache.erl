%%% @doc Pseudo-path cache for node location records.
%%% Writes location records to ~location@1.0/ADDRESS -> LocationRecord.
-module(dev_location_cache).
-export([write/2, read/2, list/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The pseudo-path prefix which the location cache should use.
-define(LOCATION_CACHE_PREFIX, <<"~location@1.0">>).

%% @doc Merge the location store with the main store. Used before writing
%% to the cache.
opts(Opts) ->
    Opts#{
        store =>
            hb_opts:get(
                scheduler_store,
                hb_opts:get(store, no_viable_store, Opts),
                Opts
            )
    }.

%% @doc Read the latest known scheduler location for an address.
read(Address, RawOpts) ->
    Opts = opts(RawOpts),
    Res =
        hb_cache:read(
            hb_store:path(
                hb_opts:get(store, no_viable_store, Opts),
                [
                    ?LOCATION_CACHE_PREFIX,
                    hb_util:human_id(Address)
                ]
            ),
            Opts
        ),
    Event =
        case Res of
            {ok, _} -> found_in_store;
            not_found -> not_found_in_store;
            _ -> local_lookup_unexpected_result
        end,
    ?event(scheduler_location, {Event, {address, Address}, {res, Res}}),
    Res.

%% @doc Write the latest known scheduler location for an address.
write(LocationMsg, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    Signers = hb_message:signers(LocationMsg, Opts),
    ?event(
        scheduler_location,
        {caching_locally,
            {signers, Signers},
            {location_msg, LocationMsg}
        }
    ),
    case hb_cache:write(LocationMsg, Opts) of
        {ok, RootPath} ->
            lists:foreach(
                fun(Signer) ->
                    hb_store:make_link(
                        Store,
                        RootPath,
                        hb_store:path(
                            Store,
                            [
                                ?LOCATION_CACHE_PREFIX,
                                hb_util:human_id(Signer)
                            ]
                        )
                    )
                end,
                Signers
            ),
            ok;
        false ->
            % The message is not valid, so we don't cache it.
            {error, <<"Invalid scheduler location message. Not caching.">>};
        {error, Reason} ->
            ?event(warning, {failed_to_cache_location_msg, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Return a list of all known location records.
list(RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:list(Store, [?LOCATION_CACHE_PREFIX]).