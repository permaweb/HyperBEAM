%%% @doc A device that serves the documentation pages of a node. The `info' of
%%% a message, from `message@1.0', is a message of this device (the node's
%%% `docs-device') for the device of that message. It has no body, so when a
%%% browser requests it, `httpsig@1.0' resolves `index' upon it and the browser
%%% receives the page of this device. The page's script, `docs.js', then reads
%%% the info as JSON, with the schema and the specification of the device in
%%% it, the recipes for the device through the node's GraphQL, and the
%%% implementations of the device, and renders them.
%%%
%%% The package of the device contains the whole site: the page, its scripts and
%%% styles, its fonts, and its guides. The device's keys are:
%%% <pre>
%%%     index: the page, `index.html'. It is the same for every device.
%%%     implementations: the ID of the specification of the device in `for',
%%%         with the IDs of every implementation of it that the node's loader
%%%         can find.
%%%     devices: the devices of the node's preloaded index, each with the ID
%%%         of its specification.
%%%     asset: a file of the package, at the path after `asset':
%%%         `/~docs@1.0/asset/fonts/dm-sans-400.woff2', or in the request's
%%%         `asset' key: `/~docs@1.0/asset=site.css'.
%%% </pre>
%%% A device with a `handler' or a `default' answers `info' itself, so the
%%% page reaches every device as `/~message@1.0/info?for=<device>'. The replies
%%% are not stored (`cache-control: no-store'), as each follows the node's
%%% loaded devices, apart from the files, which a client may keep for an hour.
-module(dev_docs).
-specification("../../../specs/docs@1.0.md").
-export([device_info/1, index/3, implementations/3, devices/3, asset/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NO_STORE, <<"no-store">>).
-define(FILE_CACHE, <<"public, max-age=3600">>).

%% @doc Return the options of the device. A directory of the package answers
%% each key with its file or directory of that name. Any other message of the
%% device exports the device's keys.
device_info(#{ <<"asset-directory">> := _ }) -> #{ default => fun entry/4 };
device_info(_) ->
    #{
        exports =>
            [<<"index">>, <<"implementations">>, <<"devices">>, <<"asset">>]
    }.

%% @doc Return the page of the site.
-spec index(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ body := binary(), _ => _ }} | {error, not_found}.
index(_Base, _Req, _Opts) ->
    read_file([<<"index.html">>], ?NO_STORE).

%% @doc Return the ID of the specification of the device in `for', and the IDs
%% of every implementation of it that the node's loader can find. The device
%% is the `for' of the request, or else of the base, the info of a device.
-spec implementations(
    #{ for => binary(), _ => _ },
    #{ for => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ specification := binary(), implementations := [binary()], _ => _ }}
    | {error, _}.
implementations(Base, Req, Opts) ->
    For =
        hb_maps:get(
            <<"for">>,
            Req,
            hb_maps:get(<<"for">>, Base, <<"message@1.0">>, Opts),
            Opts
        ),
    maybe
        {ok, Spec} ?= hb_device_load:spec(For, Opts),
        SpecID = hb_message:id(Spec, signed, Opts),
        {ok, IDs} ?= hb_device_load:implementations(SpecID, Opts),
        {ok,
            #{
                <<"specification">> => SpecID,
                <<"implementations">> => IDs,
                <<"cache-control">> => ?NO_STORE
            }
        }
    end.

%% @doc Return the devices of the node's preloaded index: each name, with the
%% ID of its specification.
-spec devices(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | {error, _}.
devices(_Base, _Req, Opts) ->
    Store = hb_device_load:store(Opts),
    maybe
        {ok, <<"ao:", IndexID/binary>>} ?=
            hb_store:read(Store, ?PRELOADED_INDEX_KEY, Opts),
        {ok, Index} ?= hb_cache:read(IndexID, Opts#{ <<"store">> => Store }),
        {ok,
            (hb_message:uncommitted(Index, Opts))#{
                <<"cache-control">> => ?NO_STORE
            }
        }
    end.

%% @doc Return a file of the package, at the path in the request's `asset' key.
%% Without one, return the root directory of the package, whose keys are its
%% files and directories.
-spec asset(#{ _ => _ }, #{ asset => binary(), _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | {error, not_found}.
asset(_Base, #{ <<"asset">> := Path }, _Opts) when is_binary(Path) ->
    read_file(binary:split(Path, <<"/">>, [global]), ?FILE_CACHE);
asset(_Base, _Req, _Opts) ->
    {ok, directory([])}.

%% @doc Return the file or the directory of a given name in a directory of the
%% package.
entry(Name, #{ <<"asset-directory">> := Dir }, _Req, _Opts) ->
    Path = binary:split(Dir, <<"/">>, [global, trim_all]) ++ [Name],
    case is_directory(Path) of
        true -> {ok, directory(Path)};
        false -> read_file(Path, ?FILE_CACHE)
    end.

%% @doc Return whether a path is a directory of the package.
is_directory(Path) ->
    case package_path(Path) of
        {ok, FilePath} -> filelib:is_dir(FilePath);
        {error, not_found} -> false
    end.

%% @doc Return the message of a directory of the package.
directory(Path) ->
    #{
        <<"device">> => <<"docs@1.0">>,
        <<"asset-directory">> => hb_util:bin(lists:join(<<"/">>, Path))
    }.

%% @doc Read a file of the package by the parts of its path.
read_file(Path, CacheControl) ->
    maybe
        {ok, FilePath} ?= package_path(Path),
        true ?= filelib:is_regular(FilePath) orelse {error, not_found},
        {ok, Body} ?= file:read_file(FilePath),
        {ok,
            #{
                <<"body">> => Body,
                <<"content-type">> => content_type(Path),
                <<"cache-control">> => CacheControl
            }
        }
    end.

%% @doc Return the path on disk of a file of the package, in the directory that
%% the loader extracts the package's files to. Each part of the path is split
%% at `/', as a URL can encode one inside a part (`%2F'). An element that is
%% empty, `.' or `..' is refused, so that no path leaves the package.
package_path(Path) ->
    Elements =
        lists:append([binary:split(Part, <<"/">>, [global]) || Part <- Path]),
    Refused = [<<>>, <<".">>, <<"..">>],
    case [Element || Element <- Elements, lists:member(Element, Refused)] of
        [] ->
            {ok,
                filename:join(
                    [hb_device_archive:implementation_dir(?MODULE)]
                        ++ [hb_util:list(Element) || Element <- Elements]
                )
            };
        _ ->
            {error, not_found}
    end.

%% @doc Return the content type of a file of the package, by its extension.
content_type(Path) ->
    case filename:extension(hb_util:list(lists:last(Path))) of
        ".html" -> <<"text/html; charset=utf-8">>;
        ".css" -> <<"text/css; charset=utf-8">>;
        ".js" -> <<"text/javascript; charset=utf-8">>;
        ".md" -> <<"text/markdown; charset=utf-8">>;
        ".svg" -> <<"image/svg+xml">>;
        ".woff2" -> <<"font/woff2">>;
        _ -> <<"application/octet-stream">>
    end.

%%% Tests

%% @doc A node answers each request of the docs of a device: the page, the
%% info as JSON with the schema and the specification in it, and the
%% implementations.
info_test() ->
    Node = hb_http_server:start_node(#{ <<"priv-wallet">> => ar_wallet:new() }),
    {ok, Info} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/~message@1.0/info?for=router@1.0">>,
                <<"accept">> => <<"application/json">>,
                <<"accept-bundle">> => <<"true">>
            },
            #{}
        ),
    #{ <<"device">> := <<"docs@1.0">>, <<"for">> := <<"router@1.0">> } = Info,
    % The client keeps the messages of the info in its cache, as links.
    #{ <<"schema">> := Schema, <<"specification">> := Spec } =
        hb_cache:ensure_all_loaded(Info, #{}),
    ?assertMatch(#{ <<"route">> := [_ | _] }, Schema),
    ?assertEqual(<<"router@1.0">>, hb_maps:get(<<"name">>, Spec)),
    % A browser receives the page: the info has no body, so `httpsig@1.0'
    % resolves `index' upon it.
    {ok, Page} = hb_ao:resolve(Info, <<"index">>, #{}),
    ?assertMatch(<<"<!doctype html>", _/binary>>, maps:get(<<"body">>, Page)),
    {ok, Implementations} =
        hb_http:get(
            Node,
            <<"/~message@1.0/info/implementations?for=router@1.0">>,
            #{}
        ),
    SpecID = hb_message:id(Spec, signed, #{}),
    ?assertMatch(
        #{ <<"specification">> := SpecID, <<"implementations">> := [_ | _] },
        Implementations
    ),
    % `router@1.0' answers `info' through `message@1.0'.
    ?assertMatch(
        {ok,
            #{ <<"device">> := <<"docs@1.0">>, <<"for">> := <<"router@1.0">> }
        },
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/~router@1.0/info">>,
                <<"accept">> => <<"application/json">>
            },
            #{}
        )
    ).

%% @doc A file of the package is given by its path, in either form, and a path
%% that leaves the package is refused.
asset_test() ->
    Node = hb_http_server:start_node(#{ <<"priv-wallet">> => ar_wallet:new() }),
    {ok, Font} =
        hb_http:get(Node, <<"/~docs@1.0/asset/fonts/dm-sans-400.woff2">>, #{}),
    ?assertEqual(<<"font/woff2">>, hb_maps:get(<<"content-type">>, Font)),
    {ok, CSS} = hb_http:get(Node, <<"/~docs@1.0/asset=fonts.css">>, #{}),
    ?assertMatch(<<"@font-face", _/binary>>, hb_maps:get(<<"body">>, CSS)),
    ?assertMatch(
        {error, _},
        hb_http:get(Node, <<"/~docs@1.0/asset/../../rebar.config">>, #{})
    ),
    % A URL can encode `/' as `%2F' inside one part of the path. Such a part
    % must not leave the package either, by `..' or by being absolute.
    lists:foreach(
        fun(Path) ->
            ?assertMatch({error, _}, hb_http:get(Node, Path, #{}))
        end,
        [
            <<"/~docs@1.0/asset/fonts", (binary:copy(<<"%2F..">>, 30))/binary,
                "%2Fetc%2Fhosts">>,
            <<"/~docs@1.0/asset/%2Fetc%2Fhosts">>
        ]
    ).
