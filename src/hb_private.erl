%%% @doc This module provides basic helper utilities for managing the
%%% private element of a message, which can be used to store state that is
%%% not included in serialized messages, or those granted to users via the
%%% APIs. Private elements of a message can be useful for storing state that
%%% is only relevant temporarily. For example, a device might use the private
%%% element to store a cache of values that are expensive to recompute. They
%%% should _not_ be used for encoding state that makes the execution of a
%%% device non-deterministic (unless you are sure you know what you are doing).
%%%
%%% The `set' and `get' functions of this module allow you to run those keys
%%% as AO-Core paths if you would like to have private `devices' in the
%%% messages non-public zone.
%%% 
%%% See `hb_ao' for more information about the AO-Core protocol
%%% and private elements of messages.
-module(hb_private).
-export([opts/1]).
-export([from_message/1, reset/1, is_private/1]).
-export([get/3, get/4, set/4, set/3, set_priv/2, merge/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Return the `private' key from a message. If the key does not exist, an
%% empty map is returned.
from_message(Msg) when is_map(Msg) ->
    case maps:is_key(<<"priv">>, Msg) of
        true -> maps:get(<<"priv">>, Msg, #{});
        false -> maps:get(priv, Msg, #{})
    end;
from_message(_NonMapMessage) -> #{}.

%% @doc Helper for getting a value from the private element of a message. Uses
%% AO-Core resolve under-the-hood, removing the private specifier from the
%% path if it exists.
get(Key, Msg, Opts) ->
    get(Key, Msg, not_found, Opts).
get(InputPath, Msg, Default, Opts) ->
    % Resolve the path against the private element of the message.
    Resolved =
        hb_util:deep_get(
            remove_private_specifier(InputPath, Opts),
            from_message(Msg),
            opts(Opts)
        ),
    case Resolved of
        not_found -> Default;
        Value -> Value
    end.

%% @doc Helper function for setting a key in the private element of a message.
set(Msg, InputPath, Value, Opts) ->
    Path = remove_private_specifier(InputPath, Opts),
    Priv = from_message(Msg),
    ?event({set_private, {in, Path}, {out, Path}, {value, Value}, {opts, Opts}}),
    NewPriv = hb_util:deep_set(Path, Value, Priv, opts(Opts)),
    ?event({set_private_res, {out, NewPriv}}),
    set_priv(Msg, NewPriv).
set(Msg, PrivMap, Opts) ->
    CurrentPriv = from_message(Msg),
    ?event({set_private, {in, PrivMap}, {opts, Opts}}),
    NewPriv = hb_util:deep_merge(CurrentPriv, PrivMap, opts(Opts)),
    ?event({set_private_res, {out, NewPriv}}),
    set_priv(Msg, NewPriv).

%% @doc Merge the private elements of two messages into one. The keys in the
%% second message will override the keys in the first message. The base keys
%% from the first message will be preserved, but the keys in the second message
%% will be lost.
merge(Base, Req, Opts) ->
    % Merge the private elements of the two messages.
    Merged =
        hb_util:deep_merge(
            from_message(Base),
            from_message(Req),
            opts(Opts)
        ),
    % Set the merged private element on the first message.
    set_priv(Base, Merged).

%% @doc Helper function for setting the complete private element of a message.
set_priv(Msg, PrivMap)
        when map_size(PrivMap) =:= 0 andalso not is_map_key(<<"priv">>, Msg) ->
    Msg;
set_priv(Msg, PrivMap) ->
    Msg#{ <<"priv">> => PrivMap }.

%% @doc Check if a key is private.
is_private(Key) ->
	try hb_util:bin(Key) of
		<<"priv", _/binary>> -> true;
		_ -> false
    catch _:_ -> false
	end.

%% @doc Remove the first key from the path if it is a private specifier.
remove_private_specifier(InputPath, Opts) ->
    case is_private(hd(Path = hb_path:term_to_path_parts(InputPath, Opts))) of
        true -> tl(Path);
        false -> Path
    end.

%% @doc The opts map that should be used when resolving paths against the
%% private element of a message. We add the `priv_store' option if set, such that
%% evaluations are not inadvertently persisted in public storage but this module
%% can still access data from the normal stores. This mechanism requires that
%% the priv_store is writable. We also ensure that no cache entries are
%% generated from downstream AO-Core resolutions.
opts(Opts) ->
    PrivStore =
        case hb_opts:get(priv_store, undefined, Opts) of
            undefined -> [];
            PrivateStores when is_list(PrivateStores) -> PrivateStores;
            PrivateStore -> [PrivateStore]
        end,
    BaseStore =
        case hb_opts:get(store, [], Opts) of
            SingleStore when is_map(SingleStore) -> [SingleStore];
            Stores when is_list(Stores) -> Stores
        end,
    NormStore = PrivStore ++ BaseStore,
    Opts#{
        hashpath => ignore,
        cache_control => [<<"no-cache">>, <<"no-store">>],
        store => NormStore
    }.

%% @doc Unset all of the private keys in a message or deep Erlang term.
%% This function operates on all types of data, such that it can be used on
%% non-message terms to ensure that `priv` elements can _never_ pass through.
reset(Msg) when is_map(Msg) ->
    maps:map(
        fun(_Key, Val) -> reset(Val) end,
        maps:without(
            lists:filter(fun is_private/1, maps:keys(Msg)),
            Msg
        )
    );
reset(List) when is_list(List) ->
    % Check if any of the terms in the list are private specifiers, return an
    % empty list if so.
    case lists:any(fun is_private/1, List) of
        true -> [];
        false ->
            % The list itself is safe. Check each of the children.
            lists:map(fun reset/1, List)
    end;
reset(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(reset(tuple_to_list(Tuple)));
reset(NonMapMessage) ->
    NonMapMessage.

%%% Tests

set_private_test() ->
    ?assertEqual(
        #{<<"a">> => 1, <<"priv">> => #{<<"b">> => 2}},
        set(#{<<"a">> => 1}, <<"b">>, 2, #{})
    ),
    Res = set(#{<<"a">> => 1}, <<"a">>, 1, #{}),
    ?assertEqual(#{<<"a">> => 1, <<"priv">> => #{<<"a">> => 1}}, Res),
    ?assertEqual(
        #{<<"a">> => 1, <<"priv">> => #{<<"a">> => 1}},
        set(Res, <<"a">>, 1, #{})
    ).

get_private_key_test() ->
    M1 = #{<<"a">> => 1, <<"priv">> => #{<<"b">> => 2}},
    ?assertEqual(not_found, get(<<"a">>, M1, #{})),
    {ok, [<<"a">>]} = hb_ao:resolve(M1, <<"keys">>, #{}),
    ?assertEqual(2, get(<<"b">>, M1, #{})),
    {error, _} = hb_ao:resolve(M1, <<"priv/a">>, #{}),
    {error, _} = hb_ao:resolve(M1, <<"priv">>, #{}).

get_deep_key_test() ->
    M1 = #{<<"a">> => 1, <<"priv">> => #{<<"b">> => #{<<"c">> => 3}}},
    ?assertEqual(3, get(<<"b/c">>, M1, #{})).

priv_opts_store_read_link_test() ->
    % Write a message to the public store.
    PublicStore = [hb_test_utils:test_store(hb_store_lmdb)],
    timer:sleep(1),
    OnlyPrivStore = [hb_test_utils:test_store(hb_store_fs)],
    ok = hb_store:write(PublicStore, <<"key">>, <<"test-message">>),
    {ok, <<"test-message">>} = hb_store:read(PublicStore, <<"key">>),
    % Make a link to the key in the public store.
    ok = hb_store:make_link(PublicStore, <<"key">>, <<"link">>),
    {ok, <<"test-message">>} = hb_store:read(PublicStore, <<"link">>),
    % Read the link from the private store. First as a simple store read, then
    % as a link.
    Opts = #{ store => PublicStore, priv_store => OnlyPrivStore },
    PrivOpts = #{ store := PrivStore } = opts(Opts),
    {ok, <<"test-message">>} = hb_store:read(PrivStore, <<"link">>),
    Loaded =
        hb_cache:ensure_loaded(
            {link, <<"link">>, #{ <<"type">> => <<"link">>, <<"lazy">> => false }},
            PrivOpts
        ),
    ?assertEqual(<<"test-message">>, Loaded).

priv_opts_cache_read_message_test() ->
    hb:init(),
    PublicStore = [hb_test_utils:test_store(hb_store_lmdb)],
    OnlyPrivStore = [hb_test_utils:test_store(hb_store_fs)],
    Opts = #{ store => PublicStore, priv_store => OnlyPrivStore },
    PrivOpts = opts(Opts),
    % Use the `~scheduler@1.0' and `~process@1.0' infrastructure to write a
    % complex message into the public store.
    Msg = hb_cache:ensure_all_loaded(dev_process:test_aos_process(Opts), Opts),
    {ok, ID} = hb_cache:write(Msg, Opts),
    % Ensure we can read the message using the public store.
    {ok, PubMsg} = hb_cache:read(ID, Opts),
    PubMsgWithCommitments = hb_cache:read_all_commitments(PubMsg, Opts),
    PubMsgLoaded = hb_cache:ensure_all_loaded(PubMsgWithCommitments, Opts),
    ?assertEqual(Msg, PubMsgLoaded),
    % Read the message using the private store.
    {ok, PrivMsg} = hb_cache:read(ID, PrivOpts),
    PrivMsgWithCommitments = hb_cache:read_all_commitments(PrivMsg, PrivOpts),
    PrivMsgLoaded = hb_cache:ensure_all_loaded(PrivMsgWithCommitments, PrivOpts),
    ?assertEqual(Msg, PrivMsgLoaded).