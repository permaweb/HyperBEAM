%%% @doc Utility functions for working with links.
-module(hb_link).
-export([is_link_key/1, remove_link_specifier/1]).
-export([normalize/2, normalize/3]).
-export([decode_all_links/1]).
-export([format/1, format/2, format/3]).
-export([format_unresolved/1, format_unresolved/2, format_unresolved/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Takes a structured message and ensures that its sub-message links are
%% normalized to TABM form:
%% 
%% - All literal (binary) lazily-loadable values are in-memory.
%% - All submaps are represented as links, optionally offloading their local 
%%   values to the cache.
%% - All other values are left unchanged (including their potential types).
%% 
%% The response is a non-recursive, fully loaded message. It may still contain
%% types, but all submessages are guaranteed to be linkified. This stands in 
%% contrast to `linkify', which takes a structured message and returns a message
%% with structured links.
normalize(Msg, Opts) when is_map(Opts) ->
    normalize(Msg, hb_opts:get(linkify_mode, offload, Opts), Opts).

normalize(Msg, false, _Opts) ->
    Msg;
normalize(Msg, Mode, Opts) when is_map(Msg) ->
    maps:merge(
        maps:with([<<"commitments">>, <<"priv">>], Msg),
            maps:from_list(
                lists:map(
                    fun({Key, {link, ID, LinkOpts = #{ <<"type">> := <<"link">> }}}) ->
                        % The value is a link. Deconstruct it and ensure it is
                        % normalized (lazy links are made greedy, and both are
                        % returned in binary TABM form).
                        NormKey = hb_util:bin(Key),
                        UnderlyingID =
                            case maps:get(<<"lazy">>, LinkOpts, false) of
                                true ->
                                    case hb_cache:read(ID, Opts) of
                                        {ok, Underlying} when ?IS_ID(Underlying) ->
                                            Underlying;
                                        Err ->
                                            throw(
                                                {could_not_read_lazy_link,
                                                    {key, Key},
                                                    {lazy_id, ID},
                                                    {error, Err}
                                                }
                                            )
                                    end;
                                false ->
                                    % The ID given is already in 'greedy' form.
                                    % We embed it in the result unchanged.
                                    ID
                            end,
                        ?event(debug_linkify, {link_normalized, Key, UnderlyingID}),
                        {<< NormKey/binary, "+link">>, UnderlyingID};
                    ({Key, V}) when is_map(V) or is_list(V) ->
                        ?event(debug_linkify, {linkifying_submessage, Key}),
                        % The value is a submessage that we have in local memory.
                        % We must offload it such that it is cached, and
                        % referenced by a link.
                        % We start by normalizing the child message, generating 
                        % its IDs by proxy.
                        NormChild = normalize(V, Mode, Opts),
                        NormKey = hb_util:bin(Key),
                        % Generate the ID of the normalized child message.
                        ID = hb_message:id(NormChild, all, Opts),
                        % If we are in `offload' mode, we write the message to the
                        % cache. If we are in `discard' mode, we simply drop the 
                        % nested message.
                        case Mode of
                            discard -> do_nothing;
                            offload ->
                                % Write the child to the store to ensure its
                                % storage and availability.
                                hb_cache:write(NormChild, Opts)
                        end,
                        ?event(debug_linkify, {generated_link, {key, Key}, {id, ID}}),
                        {<<NormKey/binary, "+link">>, ID};
                    ({Key, {link, ID, LinkOpts = #{ <<"lazy">> := true }}})
                            when ?IS_ID(ID) andalso
                                    not is_map_key(<<"type">>, LinkOpts) ->
                        % The link is lazy, but its target is a top-level ID
                        % rather than a subkey path -- which means the ID points
                        % to another message in the cache. We only need to
                        % linkify the key; the underlying message can stay
                        % un-loaded until something actually reads it.
                        NormKey = hb_util:bin(Key),
                        ?event(debug_linkify, {lazy_link_to_message, Key, ID}),
                        {<< NormKey/binary, "+link">>, ID};
                    ({Key, V}) when ?IS_LINK(V) ->
                        % The link is not a submap. We load it such that it is
                        % local in-memory. This clause is used when we are
                        % normalizing a lazily-loaded message.
                        {Key, hb_cache:ensure_loaded(V, Opts)};
                    ({Key, V}) ->
                        % The value is a primitive type. We do not need to do
                        % anything.
                        {Key, V}
                    end,
                    maps:to_list(maps:without([<<"commitments">>, <<"priv">>], Msg))
                )
            )
    );
normalize(OtherVal, Mode, Opts) when is_list(OtherVal) ->
    lists:map(fun(X) -> normalize(X, Mode, Opts) end, OtherVal);
normalize(OtherVal, _Mode, _Opts) ->
    OtherVal.

%% @doc Decode links embedded in the headers of a message.
decode_all_links(Msg) when is_map(Msg) ->
    maps:from_list(
        lists:map(
            fun({Key, MaybeID}) ->
                case is_link_key(Key) of
                    true ->
                        NewKey = binary:part(Key, 0, byte_size(Key) - 5),
                        {NewKey, 
                            {
                                link,
                                MaybeID,
                                #{
                                    <<"type">> => <<"link">>,
                                    <<"lazy">> => false
                                }
                            }
                        };
                    _ -> {Key, MaybeID}
                end
            end,
            maps:to_list(Msg)
        )
    );
decode_all_links(List) when is_list(List) ->
    lists:map(fun(X) -> decode_all_links(X) end, List);
decode_all_links(OtherVal) ->
    OtherVal.

%% @doc Determine if a key is an encoded link.
is_link_key(Key) when byte_size(Key) >= 5 ->
    binary:part(Key, byte_size(Key) - 5, 5) =:= <<"+link">>;
is_link_key(_) -> false.

%% @doc Remove any `+link` suffixes from a key.
remove_link_specifier(Key) ->
    case is_link_key(Key) of
        true -> binary:part(Key, 0, byte_size(Key) - 5);
        false -> Key
    end.

%% @doc Format a link as a short string suitable for printing. Checks the node
%% options (optionally) given, to see if it should resolve the link to a value
%% before printing.
format(Link) -> format(Link, #{}).
format(Link, Opts) ->
    format(Link, Opts, 0).
format(Link, Opts, Indent) ->
    case hb_opts:get(debug_resolve_links, false, Opts) of
        true ->
            try
                hb_format:message(
                    hb_cache:ensure_all_loaded(Link, Opts),
                    Opts,
                    Indent
                )
            catch
                _:_ -> << "!UNRESOLVABLE! ", (format_unresolved(Link, Opts))/binary >>
            end;
        false -> format_unresolved(Link, Opts, Indent)
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

%% @doc Normalising a map whose value is a lazy link to another message must
%% not trigger a read of the underlying message -- only the cheap lazy-to-ID
%% step is required at this stage.
lazy_link_to_message_is_not_loaded_test() ->
    Opts = #{},
    Inner = #{<<"hello">> => <<"world">>, <<"nested">> => <<"value">>},
    {ok, InnerID} = hb_cache:write(Inner, Opts),
    OuterBase = #{<<"top">> => {link, InnerID, #{ <<"lazy">> => true }}},
    % Count `hb_cache:read' calls on the normalising process only.
    Self = self(),
    Tracer = spawn_link(fun() -> trace_count_loop(Self, 0) end),
    erlang:trace(self(), true, [call, {tracer, Tracer}]),
    erlang:trace_pattern({hb_cache, read, '_'}, true, [local]),
    try
        Norm = normalize(OuterBase, offload, Opts),
        % The normalised form must expose the link as a '+link' key pointing
        % at the underlying message ID, rather than an inlined copy of the
        % submessage.
        ?assertEqual(#{ <<"top+link">> => InnerID }, Norm)
    after
        erlang:trace(self(), false, [call]),
        Tracer ! {get, self()}
    end,
    Count = receive {count, C} -> C after 1000 -> -1 end,
    Tracer ! stop,
    ?assertEqual(0, Count).

trace_count_loop(Traced, N) ->
    receive
        {trace, Traced, call, _} -> trace_count_loop(Traced, N + 1);
        {get, From} -> From ! {count, N}, trace_count_loop(Traced, N);
        stop -> ok
    end.
