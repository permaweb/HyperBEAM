%%% @doc A device for registering and looking up local names. This device uses
%%% the node message to store a local cache of its known names, and the typical
%%% non-volatile storage of the node message to store the names long-term.
-module(dev_local_name).
-export([info/1, lookup/3, register/3]).
%%% HyperBEAM public (non-AO resolvable) functions.
-export([direct_register/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The location that the device should use in the store for its links.
-define(DEV_CACHE, <<"local-name@1.0">>).

%% @doc Export only the `lookup' and `register' functions.
info(_Opts) ->
    #{
        excludes => [<<"direct_register">>, <<"keys">>, <<"set">>],
        default => fun default_lookup/4
    }.

%% @doc Takes a `key' argument and returns the value of the name, if it exists.
lookup(_, Req, Opts) ->
    Key = hb_ao:get(<<"key">>, Req, no_key_specified, Opts),
    ?event(local_name, {lookup, Key}),
    hb_ao:resolve(
        find_names(Opts),
        Key,
        Opts
    ).

%% @doc Handle all other requests by delegating to the lookup function.
default_lookup(Key, _, Req, Opts) ->
    lookup(Key, Req#{ <<"key">> => Key }, Opts).

%% @doc Takes a `key' and `value' argument and registers the name. The caller
%% must be the node operator in order to register a name.
register(_, Req, Opts) ->
    case dev_meta:is(admin, Req, Opts) of
        false ->
            {error,
                #{
                    <<"status">> => 403,
                    <<"message">> => <<"Unauthorized.">>
                }
            };
        true ->
            direct_register(Req, Opts)
    end.

%% @doc Register a name without checking if the caller is an operator. Exported
%% for use by other devices, but not publicly available.
direct_register(Req, Opts) ->
    case hb_cache:write(hb_ao:get(<<"value">>, Req, Opts), Opts) of
        {ok, MsgPath} ->
            hb_cache:link(
                MsgPath,
                LinkPath =
                    [
                        ?DEV_CACHE,
                        Name = hb_ao:get(<<"key">>, Req, Opts)
                    ],
                Opts
            ),
            load_names(Opts),
            ?event(
                local_name,
                {registered,
                    Name,
                    {link, LinkPath},
                    {msg, MsgPath}
                }
            ),
            {ok, <<"Registered.">>};
        {error, _} ->
            not_found
    end.

%% @doc Returns a message containing all known names.
find_names(Opts) ->
    case hb_opts:get(local_names, not_found, Opts#{ only => local }) of
        not_found ->
            find_names(load_names(Opts));
        LocalNames ->
            LocalNames
    end.

%% @doc Loads all known names from the cache and returns the new `node message'
%% with those names loaded into it.
load_names(Opts) ->
    LocalNames =
        maps:from_list(lists:map(
            fun(Key) ->
                ?event(local_name, {loading, Key}),
                case hb_cache:read([?DEV_CACHE, Key], Opts) of
                    {ok, Value} ->
                        {Key, Value};
                    {error, _} ->
                        {Key, not_found}
                end
            end,
            hb_cache:list(?DEV_CACHE, Opts)
        )),
    ?event(local_name, {found_cache_keys, LocalNames}),
    update_names(LocalNames, Opts).

%% @doc Updates the node message with the new names. Further HTTP requests will
%% use this new message, removing the need to look up the names from non-volatile
%% storage.
update_names(LocalNames, Opts) ->
    hb_http_server:set_opts(NewOpts = Opts#{ local_names => LocalNames }),
    NewOpts.

%%% Tests

generate_test_opts() ->
    Opts = #{
        store =>
            [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"prefix">> => "cache-TEST/"
                }
            ],
        priv_wallet => ar_wallet:new()
    },
    Opts.

no_names_test() ->
    ?assertEqual(
        {error, not_found},
        lookup(#{}, #{ <<"key">> => <<"name1">> }, #{})
    ).

lookup_opts_name_test() ->
    ?assertEqual(
        {ok, <<"value1">>},
        lookup(
            #{},
            #{ <<"key">> => <<"name1">> },
            #{ local_names => #{ <<"name1">> => <<"value1">>} }
        )
    ).

register_test() ->
    TestName = <<"TEST-", (integer_to_binary(os:system_time(millisecond)))/binary>>,
    Value = <<"TEST-VALUE-", (integer_to_binary(os:system_time(millisecond)))/binary>>,
    Opts = generate_test_opts(),
    ?assertEqual(
        {ok, <<"Registered.">>},
        register(
            #{},
            hb_message:commit(
                #{ <<"key">> => TestName, <<"value">> => Value },
                Opts
            ),
            Opts
        )
    ),
    ?assertEqual(
        {ok, Value},
        lookup(#{}, #{ <<"key">> => TestName, <<"load">> => false }, Opts)
    ).

unauthorized_test() ->
    Opts = generate_test_opts(),
    ?assertEqual(
        {error, #{ <<"status">> => 403, <<"message">> => <<"Unauthorized.">> }},
        register(
            #{},
            hb_message:commit(
                #{ <<"key">> => <<"name1">>, <<"value">> => <<"value1">> },
                Opts#{ priv_wallet => ar_wallet:new() }
            ),
            Opts
        )
    ).

http_test() ->
    Opts = generate_test_opts(),
    Node = hb_http_server:start_node(Opts),
    hb_http:post(
        Node,
        <<"/~local-name@1.0/register">>,
        hb_message:commit(
            #{ <<"key">> => <<"name1">>, <<"value">> => <<"value1">> },
            Opts
        ),
        Opts
    ),
    ?assertEqual(
        {ok, <<"value1">>},
        hb_http:get(
            Node,
            <<"/~local-name@1.0/lookup?key=name1">>,
            Opts
        )
    ).