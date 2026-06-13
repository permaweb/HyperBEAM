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
                        NormKey = hb_util:bin(Key),
                        MaterializeDiscard =
                            should_materialize_discard_link(NormKey, Msg, Opts),
                        NormChild =
                            case has_bundle_commitment(V, Opts) of
                                true -> V;
                                false -> normalize(V, Mode, Opts)
                            end,
                        LinkedChild =
                            case {Mode, MaterializeDiscard} of
                                {discard, true} ->
                                    normalize_materialized_child(NormChild, Opts);
                                _ ->
                                    normalize_extension_child(NormChild, Opts)
                            end,
                        % If we are in `offload' mode, we write the message to the
                        % cache. If we are in `discard' mode, we simply drop the 
                        % nested message.
                        ID = case {Mode, MaterializeDiscard} of
                            {discard, true} ->
                                {ok, WrittenID} = hb_cache:write(LinkedChild, Opts),
                                offloaded_link_id(LinkedChild, WrittenID, Opts);
                            {discard, false} ->
                                hb_message:id(LinkedChild, all, Opts);
                            {offload, _} ->
                                % Write the child to the store to ensure its
                                % storage and availability.
                                {ok, WrittenID} = hb_cache:write(LinkedChild, Opts),
                                offloaded_link_id(LinkedChild, WrittenID, Opts)
                        end,
                        ?event(debug_linkify, {generated_link, {key, Key}, {id, ID}}),
                        {<<NormKey/binary, "+link">>, ID};
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

should_materialize_discard_link(<<"...">>, _Msg, _Opts) -> true;
should_materialize_discard_link(<<"committed">>, _Msg, _Opts) -> true;
should_materialize_discard_link(Key, Msg, Opts) ->
    lists:any(
        fun({_ID, Commitment}) ->
            lists:member(Key, committed_keys(Commitment, Opts))
        end,
        maps:to_list(maps:get(<<"commitments">>, Msg, #{}))
    ).

committed_keys(Commitment, Opts) ->
    lists:map(
        fun remove_link_specifier/1,
        hb_util:message_to_ordered_list(
            maps:get(<<"committed">>, Commitment, []),
            Opts
        )
    ).

offloaded_link_id(LinkedChild, WrittenID, Opts) when is_map(LinkedChild) ->
    case has_commitment_view(LinkedChild) of
        true ->
            case hb_message:signers(LinkedChild, Opts) of
                [] ->
                    WrittenID;
                _ ->
                    LinkID = hb_message:id(LinkedChild, all, Opts),
                    case ensure_resolvable_link_id(WrittenID, LinkID, Opts) of
                        ok ->
                            LinkID;
                        conflict ->
                            ?event(debug_linkify,
                                {link_id_conflict_fallback,
                                    {written_id, WrittenID},
                                    {link_id, LinkID}}),
                            WrittenID
                    end
            end;
        false ->
            WrittenID
    end;
offloaded_link_id(_LinkedChild, WrittenID, _Opts) ->
    WrittenID.

has_commitment_view(Msg) ->
    maps:is_key(<<"commitments">>, Msg)
        orelse maps:is_key(<<"...">>, Msg)
        orelse maps:is_key(<<"...+link">>, Msg).

normalize_extension_child(Child, Opts) when is_map(Child) ->
    case maps:is_key(<<"commitments">>, Child)
            orelse maps:is_key(<<"...">>, Child)
            orelse maps:is_key(<<"...+link">>, Child) of
        true -> hb_message:normalize_commitments(Child, Opts, verify);
        false -> Child
    end;
normalize_extension_child(Child, _Opts) ->
    Child.

has_bundle_commitment(Msg, Opts) when is_map(Msg) ->
    lists:any(
        fun({_ID, Commitment}) ->
            hb_util:atom(hb_maps:get(<<"bundle">>, Commitment, false, Opts))
        end,
        maps:to_list(maps:get(<<"commitments">>, Msg, #{}))
    );
has_bundle_commitment(_Msg, _Opts) ->
    false.

normalize_materialized_child(Child, Opts)
        when is_map(Child) ->
    case is_list_like(Child, Opts) of
        true -> Child;
        false -> hb_message:normalize_commitments(Child, Opts, verify)
    end;
normalize_materialized_child(Child, _Opts) ->
    Child.

is_list_like(Child, Opts) ->
    hb_util:is_ordered_list(Child, Opts)
        orelse hb_util:is_ordered_list(
            maps:from_list(
                lists:map(
                    fun({Key, Value}) ->
                        {remove_link_specifier(hb_ao:normalize_key(Key)), Value}
                    end,
                    maps:to_list(Child)
                )
            ),
            Opts
        ).

ensure_resolvable_link_id(ID, ID, _Opts) ->
    ok;
ensure_resolvable_link_id(WrittenID, LinkID, Opts) ->
    case hb_cache:link(WrittenID, LinkID, Opts) of
        ok -> ok;
        {error, Exists} when Exists == eexist; Exists == already_added ->
            ensure_existing_link_id(WrittenID, LinkID, Opts);
        _Error ->
            conflict
    end.

ensure_existing_link_id(WrittenID, LinkID, Opts) ->
    case {hb_cache:read(WrittenID, Opts), hb_cache:read(LinkID, Opts)} of
        {{ok, Msg}, {ok, Msg}} ->
            ok;
        {{ok, Expected}, {ok, Actual}} ->
            case equivalent_cached_values(Expected, Actual, Opts) of
                true -> ok;
                false -> conflict
            end;
        {_WrittenRes, _LinkRes} ->
            conflict
    end.

equivalent_cached_values(Expected, Actual, Opts)
        when is_map(Expected), is_map(Actual) ->
    (hb_private:reset(Expected) =:= hb_private:reset(Actual))
        orelse (
            (hb_message:id(Expected, all, Opts) =:= hb_message:id(Actual, all, Opts))
                andalso
                    (unsigned_commitments(Expected, Opts)
                        =:= unsigned_commitments(Actual, Opts))
        );
equivalent_cached_values(Expected, Actual, _Opts) ->
    Expected =:= Actual.

unsigned_commitments(Msg, Opts) ->
    hb_maps:filter(
        fun(_, #{ <<"committer">> := _Committer }) -> false;
           (_, _) -> true
        end,
        maps:get(<<"commitments">>, Msg, #{}),
        Opts
    ).

%% @doc Decode links embedded in the headers of a message.
decode_all_links(Msg) when is_map(Msg) ->
    maps:from_list(
        lists:map(
            fun({Key, MaybeID}) ->
                case is_link_key(Key) of
                    true ->
                        NewKey = remove_link_specifier(Key),
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
                    _ -> {Key, decode_all_links(MaybeID)}
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
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
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
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Msg = #{
        <<"list-key">> => [1.0, 2.0, 3.0]
    },
    TABM = hb_message:convert(Msg, tabm, <<"structured@1.0">>, Opts),
    Linkified = normalize(TABM, offload, Opts),
    Req = hb_message:convert(Linkified, <<"structured@1.0">>, tabm, Opts),
    Res = hb_cache:ensure_all_loaded(Req, Opts),
    ?assertEqual(Msg, Res).
