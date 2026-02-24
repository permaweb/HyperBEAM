%%% @doc A request hook device for content moderation by blacklist.
%%%
%%% The node operator configures a blacklist provider via the `blacklist-provider`
%%% key in the node message options. The provider can be a message or a path that
%%% returns a message or binary. If a binary is returned from the provider, it is
%%% parsed as a newline-delimited list of IDs.
%%% 
%%% The device is intended for use as a `~hook@1.0` `on/request` handler. It
%%% blocks requests when any ID present in the hook payload matches the active
%%% blacklist. The device also implements a `refresh` key that can be used to
%%% force a reload of the blacklist cache, potentially on node startup or on a 
%%% `~cron@1.0/every` trigger.
%%% 
%%% The principle of this device is the same as the content policies utilized in
%%% the Arweave network: No central enforcement, but each node is capable of
%%% enforcing its own content policies based on its own free choice and
%%% configuration.
-module(dev_blacklist).
-export([request/3, refresh/3]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CACHE_TABLE, ?MODULE).
-define(DEFAULT_PROVIDER, #{ <<"body">> => [] }).

%% @doc Hook handler: block requests that involve blacklisted IDs.
request(_Base, HookReq, Opts) ->
    ensure_cache_table(),
    case is_match(HookReq, Opts) of
        false -> {ok, HookReq};
        ID -> {error, block_response(ID)}
    end.

%% @doc Check if the message contains any blacklisted IDs.
is_match(Msg, Opts) ->
    IDs = collect_ids(Msg, Opts),
    case lists:filter(fun(ID) -> not ets:lookup(?CACHE_TABLE, ID) /= [] end, IDs) of
        [] -> false;
        [ID|_] -> ID
    end.

%% @doc Force a reload of the blacklist cache. Returns the number of newly 
%% inserted IDs.
refresh(_Base, _Req, Opts) ->
    update_blacklist_cache(Opts).

%%% Internal

%% @doc Fetch the blacklist and store the results in the cache table.
update_blacklist_cache(Opts) ->
    case execute_provider(Opts) of
        {ok, Blacklist} ->
            {ok, IDs} = parse_blacklist(Blacklist, Opts),
            BlacklistID = hb_message:id(Blacklist, all, Opts),
            {ok, insert_ids(IDs, BlacklistID, Opts)};
        {error, _} = Error ->
            Error
    end.

%% @doc Execute the blacklist provider, returning the result.
execute_provider(Opts) ->
    Path = hb_opts:get(blacklist_provider, ?DEFAULT_PROVIDER, Opts),
    Request =
        case hb_cache:ensure_loaded(Path, Opts) of
            Msg when is_map(Msg) -> Msg;
            Bin when is_binary(Bin) -> #{ <<"path">> => Path }
        end,
    hb_ao:resolve(Request, Opts).

%% @doc Parse the blacklist body, returning a list of IDs.
parse_blacklist(Link, Opts) when ?IS_LINK(Link) ->
    parse_blacklist(hb_cache:ensure_loaded(Link, Opts), Opts);
parse_blacklist(Body, _Opts) when is_list(Body) ->
    {ok, lists:filtermap(fun parse_blacklist_line/1, Body)};
parse_blacklist(Msg, Opts) when is_map(Msg) ->
    maybe
        {ok, <<"content-policy">>} ?= hb_maps:find(<<"data-protocol">>, Msg, Opts),
        {ok, Body} = hb_maps:find(<<"body">>, Msg, Opts),
        parse_blacklist(Body, Opts)
    end;
parse_blacklist(Body, _Opts) when is_binary(Body) ->
    Lines = binary:split(Body, <<"\n">>, [global]),
    {ok, lists:filtermap(fun parse_blacklist_line/1, Lines)}.

%% @doc Parse a single line of the blacklist body, returning the ID if it is valid,
%% and `false' otherwise.
parse_blacklist_line(Line) ->
    Trimmed = string:trim(Line, both),
    case Trimmed of
        <<>> -> false;
        <<"#", _/binary>> -> false;
        ID when ?IS_ID(ID) -> {true, hb_util:human_id(ID)};
        _ -> false
    end.

%% @doc Collect all IDs found as elements of a given message.
collect_ids(Msg, Opts) -> lists:usort(collect_ids(Msg, [], Opts)).
collect_ids(Bin, Acc, _Opts) when ?IS_ID(Bin) -> [hb_util:human_id(Bin) | Acc];
collect_ids(Bin, Acc, _Opts) when is_binary(Bin) -> Acc;
collect_ids(Link, Acc, Opts) when ?IS_LINK(Link) ->
    collect_ids(hb_cache:ensure_loaded(Link, Opts), Acc, Opts);
collect_ids(Msg, Acc, Opts) when is_map(Msg) ->
    hb_maps:fold(
        fun(_Key, Value, AccIn) -> collect_ids(Value, AccIn, Opts) end,
        Acc,
        Msg
    );
collect_ids(List, Acc, Opts) when is_list(List) ->
    lists:foldl(
        fun(Elem, AccIn) -> collect_ids(Elem, AccIn, Opts) end,
        Acc,
        List
    );
collect_ids(_Other, Acc, _Opts) -> Acc.

block_response(BlockedID) ->
    #{
        <<"status">> => 451,
        <<"body">> => <<"Requested message blocked by this node's content policy.">>,
        <<"reason">> => <<"content-policy">>,
        <<"blocked-id">> => BlockedID
    }.

%% @doc Insert a list of IDs into the cache table, returning the number of new IDs
%% inserted. Each ID is inserted as a key with the current timestamp as the value.
insert_ids([], _Value, _Opts) -> 0;
insert_ids([ID | IDs], Value, Opts) when ?IS_ID(ID) ->
    case ets:lookup(?CACHE_TABLE, ID) of
        [] ->
            ets:insert(?CACHE_TABLE, {ID, Value}),
            1 + insert_ids(IDs, Value, Opts);
        _ -> insert_ids(IDs, Value, Opts)
    end.

%% @doc Ensure the cache table exists.
ensure_cache_table() ->
    case ets:info(?CACHE_TABLE) of
        undefined ->
            try
                ets:new(
                    ?CACHE_TABLE,
                    [
                        named_table,
                        set,
                        public,
                        {read_concurrency, true},
                        {write_concurrency, true}
                    ]
                )
            catch
                error:badarg -> ok
            end;
        _ -> ok
    end,
    ?CACHE_TABLE.

%% @doc Clear the entire cache table.
clear_cache() ->
    case ets:info(?CACHE_TABLE) of
        undefined -> ok;
        _ -> ets:delete_all_objects(?CACHE_TABLE)
    end.