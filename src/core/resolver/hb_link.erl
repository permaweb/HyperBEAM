%%% @doc Utility functions for working with links.
%%%
%%% A link is a value that refers to another message with an `ao:' URI. In
%%% memory, a link is represented as `{link, URI, LinkOpts}', where `URI' is
%%% given as it was written. If the `lazy' option is set, the tuple instead
%%% refers to a value that has not been loaded yet, and its second element
%%% gives the location of that value in the store.
-module(hb_link).
-export([decode/1, decode/2]).
-export([normalize/2, normalize/3]).
-export([format/1, format/2, format/3]).
-export([format_unresolved/1, format_unresolved/2, format_unresolved/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Takes a structured message and ensures that its sub-message links are
%% normalized to TABM form:
%%
%% - All lazily-loadable values are in-memory.
%% - All links are represented by their URIs.
%% - All submaps are represented as links, optionally offloading their local
%%   values to the cache. In `keep' mode, they stay in memory.
%% - All other values are left unchanged (including their potential types).
%%
%% The response is a fully loaded message. It may still contain types. Outside
%% `keep' mode it is non-recursive: all of its submessages are linkified.
normalize(Msg, Opts) when is_map(Opts) ->
    normalize(Msg, hb_opts:get(linkify_mode, offload, Opts), Opts).

normalize(Msg, false, _Opts) ->
    Msg;
normalize(Msg, Mode, Opts) when is_map(Msg) ->
    Private = lists:filter(fun hb_private:is_private/1, maps:keys(Msg)),
    maps:merge(
        maps:with([<<"commitments">> | Private], Msg),
        maps:map(
            fun(Key, V) -> normalize_value(Key, V, Mode, Opts) end,
            maps:without([<<"commitments">> | Private], Msg)
        )
    );
normalize(OtherVal, Mode, Opts) when is_list(OtherVal) ->
    lists:map(fun(X) -> normalize(X, Mode, Opts) end, OtherVal);
normalize(OtherVal, _Mode, _Opts) ->
    OtherVal.

%% @doc Normalize the value of a key, returning it as it appears in the
%% normalized message.
normalize_value(_Key, V, keep, _Opts) when is_map(V) or is_list(V) ->
    % The submessage stays in local memory, without a link.
    V;
normalize_value(Key, V, Mode, Opts) when is_map(V) or is_list(V) ->
    ?event(debug_linkify, {linkifying_submessage, Key}),
    % The value is a submessage that we have in local memory.
    % We must offload it such that it is cached, and
    % referenced by a link.
    % Generate the ID of the child message as it is held, with its own
    % submessages: `hb_message:id/3' normalizes them. The cache writes the
    % child as given and offloads its own submessages.
    ID = hb_message:id(V, all, Opts),
    % If we are in `offload' mode, we write the message to the
    % cache. If we are in `discard' mode, we simply drop the
    % nested message.
    case Mode of
        discard -> do_nothing;
        offload ->
            % Write the child to the store to ensure its
            % storage and availability. The child is written
            % as a structured message: the cache converts it
            % and verifies it in that form.
            hb_cache:write(V, Opts)
    end,
    ?event(debug_linkify, {generated_link, {key, Key}, {id, ID}}),
    <<"ao:", ID/binary>>;
normalize_value(Key, {link, Path, LinkOpts = #{ <<"lazy">> := true }},
        Mode, Opts) when not is_map_key(<<"type">>, LinkOpts) ->
    % The value is not loaded yet. We read it as it is stored, as we do for
    % the values in a commitment, so that a link in it stays a link.
    ReadOpts = hb_util:deep_merge(Opts, LinkOpts, Opts),
    {ok, Value} = hb_cache:read(Path, ReadOpts#{ <<"commitment">> => true }),
    normalize_value(Key, Value, Mode, Opts);
normalize_value(Key, V = {link, _Path, #{ <<"lazy">> := true }}, Mode, Opts) ->
    % The value is a lazy link with a type. We load it, which decodes it to
    % its type, and then normalize the result.
    normalize_value(Key, hb_cache:ensure_loaded(V, Opts), Mode, Opts);
normalize_value(_Key, {link, URI, _LinkOpts}, _Mode, _Opts) ->
    % The value is a link. Its normalized form is its URI.
    URI;
normalize_value(_Key, V, _Mode, _Opts) ->
    % The value is a primitive type. We do not need to do
    % anything.
    V.

%% @doc Decode the value of a key, returning a link if the value is a binary
%% that starts with `ao:'. The values of the keys in `?AO_CORE_KEYS', such as
%% `path' and `hashpath', are AO-Core syntax, so they are returned unchanged.
decode(Key, Value) ->
    case lists:member(Key, ?AO_CORE_KEYS) of
        true -> Value;
        false -> decode(Value)
    end.

%% @doc Decode a value, returning a link if it is a binary that starts with
%% `ao:'. Any other value is returned as it is.
decode(URI = <<"ao:", _/binary>>) -> {link, URI, #{}};
decode(Value) -> Value.

%% @doc Format a link as a short string suitable for printing. Checks the node
%% options (optionally) given, to see how many link levels it should resolve
%% before printing. `true' resolves links without a depth limit.
format(Link) -> format(Link, #{}).
format(Link, Opts) ->
    format(Link, Opts, 0).
format(Link, Opts, Indent) ->
    case hb_opts:get(debug_resolve_links, false, Opts) of
        true -> format_resolved(Link, Opts, Indent);
        Depth when is_integer(Depth) andalso Depth > 0 ->
            format_resolved(
                Link,
                Opts#{ <<"debug-resolve-links">> => Depth - 1 },
                Indent
            );
        _ -> format_unresolved(Link, Opts, Indent)
    end.

%% @doc Resolve and format one link using the remaining resolution depth.
format_resolved(Link, Opts, Indent) ->
    try
        hb_format:message(hb_cache:ensure_loaded(Link, Opts), Opts, Indent)
    catch
        _:_ -> << "!UNRESOLVABLE! ", (format_unresolved(Link, Opts))/binary >>
    end.

%% @doc Format a link without resolving it.
format_unresolved(Link) ->
    format_unresolved(Link, #{}).
format_unresolved({link, ID, Opts}, BaseOpts) ->
    format_unresolved({link, ID, Opts}, BaseOpts, 0).
format_unresolved({link, ID, Opts}, BaseOpts, Indent) ->
    hb_util:bin(
        hb_format:indent(
            "~s~s: ~s",
            [
                case maps:get(<<"lazy">>, Opts, false) of
                    true -> <<"Lazy link">>;
                    false -> <<"Link">>
                end,
                case maps:get(<<"type">>, Opts, no_type) of
                    no_type -> <<>>;
                    Type -> <<" (to ", (hb_util:bin(Type))/binary, ")" >>
                end,
                ID
            ],
            BaseOpts,
            Indent
        )
    ).

%%% Tests

offload_linked_message_test() ->
    Opts = #{},
    Msg = #{
        <<"immediate-key">> => <<"immediate-value">>,
        <<"link-key">> => #{
            <<"immediate-key-2">> => <<"link-value">>,
            <<"link-key-2">> => #{
                <<"immediate-key-3">> => <<"link-value-2">>
            }
        }
    },
    Offloaded = normalize(Msg, offload, Opts),
    Structured = hb_message:convert(Offloaded, <<"structured@1.0">>, tabm, Opts),
    ?event(linkify, {test_recvd_linkified, {msg, Structured}}),
    Loaded = hb_cache:ensure_all_loaded(Structured, Opts),
    ?event(linkify, {test_recvd_loaded, {msg, Loaded}}),
    ?assert(hb_message:match(Msg, Loaded, primary, Opts)).

offload_list_test() ->
    Opts = #{},
    Msg = #{
        <<"list-key">> => [1.0, 2.0, 3.0]
    },
    TABM = hb_message:convert(Msg, tabm, <<"structured@1.0">>, Opts),
    Linkified = normalize(TABM, offload, Opts),
    Req = hb_message:convert(Linkified, <<"structured@1.0">>, tabm, Opts),
    Res = hb_cache:ensure_all_loaded(Req, Opts),
    ?assertEqual(Msg, Res).
