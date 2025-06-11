%%% @doc Utility functions for working with links.
-module(hb_link).
-export([read/1, read/2, is_link_key/1, remove_link_specifier/1]).
-export([normalize/2, normalize/3]).
-export([decode_all_links/1]).
-export([format/1, format/2]).
-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").


%% @doc Read a link into memory. Uses `hb_cache:ensure_loaded/2' under-the-hood.
read(Link) -> read(Link, #{}).


read(Link, Opts) ->
    try
        hb_cache:ensure_loaded(Link, Opts)
    catch
        throw:{necessary_message_not_found, ID} ->
            {error, {link_not_found, ID}}
    end.


%% @doc Takes a message and ensures that it is normalized:
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
          fun({Key, {link, ID, LinkOpts = #{<<"type">> := <<"link">>}}}) ->
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
                                         {error, Err}})
                              end;
                          false ->
                              % The ID given is already in 'greedy' form.
                              % We embed it in the result unchanged.
                              ID
                      end,
                  ?event(debug_linkify, {link_normalized, Key, UnderlyingID}),
                  {<<NormKey/binary, "+link">>, UnderlyingID};
             ({Key, V}) when is_map(V) or is_list(V) ->
                  ?event(debug_linkify, {case2, Key}),
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
                  ?event(debug_linkify, {generated_id, {key, Key}, {id, ID}}),
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
          maps:to_list(maps:without([<<"commitments">>, <<"priv">>], Msg)))));
normalize(OtherVal, Mode, Opts) when is_list(OtherVal) ->
    lists:map(fun(X) -> normalize(X, Mode, Opts) end, OtherVal);
normalize(OtherVal, _Mode, _Opts) ->
    OtherVal.


%% @doc Decode links embedded in the headers of a message.
decode_all_links(Msg) ->
    maps:from_list(
      lists:map(
        fun({Key, MaybeID}) ->
                case is_link_key(Key) of
                    true ->
                        NewKey = binary:part(Key, 0, byte_size(Key) - 5),
                        {NewKey,
                         {link,
                          MaybeID,
                          #{
                            <<"type">> => <<"link">>,
                            <<"lazy">> => false
                           }}};
                    _ -> {Key, MaybeID}
                end
        end,
        maps:to_list(Msg))).


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
    case hb_opts:get(debug_resolve_links, false, Opts) of
        true ->
            try
                hb_message:format(hb_cache:ensure_all_loaded(Link))
            catch
                _:_ -> <<"!UNRESOLVABLE! ", (format_unresolved(Link))/binary>>
            end;
        false -> format_unresolved(Link)
    end.


%% @doc Format a link without resolving it.
format_unresolved({link, ID, Opts}) ->
    hb_util:bin(
      io_lib:format(
        "~s~s: ~s",
        [case maps:get(<<"lazy">>, Opts, false) of
             true -> <<"Lazy link">>;
             false -> <<"Link">>
         end,
         case maps:get(<<"type">>, Opts, no_type) of
             no_type -> <<>>;
             Type -> <<" (to ", (hb_util:bin(Type))/binary, ")">>
         end,
         ID])).


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
    ?assertEqual(Msg, Loaded).


offload_list_test() ->
    Opts = #{},
    Msg = #{
            <<"list-key">> => [1.0, 2.0, 3.0]
           },
    TABM = hb_message:convert(Msg, tabm, <<"structured@1.0">>, Opts),
    Linkified = normalize(TABM, offload, Opts),
    Msg2 = hb_message:convert(Linkified, <<"structured@1.0">>, tabm, Opts),
    Res = hb_cache:ensure_all_loaded(Msg2, Opts),
    ?assertEqual(Msg, Res).
