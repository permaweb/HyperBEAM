%%% @doc A reverse index for finding all message IDs with a given key-value pair.
-module(dev_match).
-export([info/0, all/3, write/3]).
-include("include/hb.hrl").

-define(CACHE_PREFIX, <<"~match@1.0">>).

%% @doc Default all non-message@1.0 and device keys to match a single key in the
%% index.
info() ->
    #{
        excludes =>
            [<<"set">>, <<"remove">>, <<"id">>, <<"verify">>, <<"write">>],
        default => fun match/4
    }.

%% @doc Get the store configured for the match index.
store(Opts) ->
    LocalMatchIndex = local_opt(match_index, Opts, undefined),
    LocalStore = local_opt(store, Opts, undefined),
    GlobalMatchIndex = hb_opts:get(match_index, false, #{ only => global }),
    MatchIndexStore =
        case {LocalMatchIndex, LocalStore} of
            {undefined, undefined} ->
                GlobalMatchIndex;
            {undefined, _} ->
                LocalStore;
            {Local, Store}
                    when Store =/= undefined andalso
                        Local =:= GlobalMatchIndex ->
                Store;
            {Local, _} ->
                Local
        end,
    case MatchIndexStore of
        false -> [];
        true -> hb_opts:get(store, [], Opts);
        ResolvedStore when not is_list(ResolvedStore) -> [ResolvedStore];
        ResolvedStore -> ResolvedStore
    end.

%% @doc Read a local option from either atom or binary key shape.
local_opt(Key, Opts, Default) ->
    case maps:find(Key, Opts) of
        {ok, Value} ->
            Value;
        error ->
            case maps:find(atom_to_binary(Key), Opts) of
                {ok, Value} -> Value;
                error -> Default
            end
    end.

%% @doc Calculate the address of a key-value pair in the match index. We use the
%% 'as device with key=value' form of hashpath such that triple is only two
%% messages, as is typical for AO-Core.
address(Key, Value) ->
    KeyBin = to_match_bin(Key),
    ValueBin = to_match_bin(Value),
    iolist_to_binary([?CACHE_PREFIX, "&", KeyBin, "=", ValueBin]).
address(Key, Value, ID) ->
    IDBin = to_match_bin(ID),
    <<(address(Key, Value))/binary, "/", IDBin/binary>>.

to_match_bin(Bin) when is_binary(Bin) -> Bin;
to_match_bin(Atom) when is_atom(Atom) -> atom_to_binary(Atom);
to_match_bin(Int) when is_integer(Int) -> integer_to_binary(Int);
to_match_bin(Float) when is_float(Float) ->
    float_to_binary(Float, [compact]);
to_match_bin(List) when is_list(List) ->
    try
        iolist_to_binary(List)
    catch
        _:_ -> term_to_binary(List)
    end;
to_match_bin(Other) ->
    term_to_binary(Other).

%% @doc Return the path representation used by cache key-value links.
value_path(Bin, Opts) when is_binary(Bin) ->
    <<"data/", (hb_path:hashpath(Bin, Opts))/binary>>;
value_path(Map, Opts) when is_map(Map) ->
    hb_message:id(Map, none, Opts#{ linkify_mode => discard });
value_path(List, Opts) when is_list(List) ->
    case io_lib:printable_unicode_list(List) of
        true ->
            value_path(iolist_to_binary(List), Opts);
        false ->
            value_path(
                hb_message:convert(List, tabm, <<"structured@1.0">>, Opts),
                Opts
            )
    end;
value_path(Other, Opts) ->
    value_path(hb_path:to_binary(Other), Opts).

%% @doc Write all keys in the base message to the match index. Expects the `Base'
%% message to already be converted to a TABM.
write(IDs, Base, Opts) ->
    case store(Opts) of
        [] -> {skip, <<"No store configured for match index.">>};
        Store ->
            IndexBase = hb_message:uncommitted(hb_private:reset(Base)),
            hb_maps:map(
                fun(RawKey, Value) ->
                    Key = hb_ao:normalize_key(RawKey),
                    ValuePath = value_path(Value, Opts),
                    lists:foreach(
                        fun(ID) ->
                            hb_store:write(
                                Store,
                                address(Key, ValuePath, ID),
                                <<"">>
                            )
                        end,
                        IDs
                    )
                end,
                IndexBase
            )
    end.

%% @doc Match a single key-value pair in the index, returning all message IDs that
%% contain the key-value pair.
match(Key, Base, _Req, Opts) -> match(Key, Base, Opts).
match(Key, Base, Opts) ->
    Store = store(Opts),
    {ok, Value} = hb_maps:find(Key, Base, Opts),
    case hb_store:list(
        Store,
        address(
            hb_ao:normalize_key(Key),
            value_path(Value, Opts)
        )
    ) of
        {ok, Messages} -> {ok, Messages};
        _ -> {error, not_found}
    end.

%% @doc Match the full base message against the index, returning the intersection
%% of all matches for each key.
all(Base, _Req, Opts) ->
    IndexBase = hb_message:uncommitted(hb_private:reset(Base)),
    Keys =
        hb_maps:keys(
            IndexBase
        ),
    case Keys of
        [] -> {ok, []};
        [FirstKey | Rest] ->
            case match(FirstKey, IndexBase, Opts) of
                {ok, FirstMatches} ->
                    lists:foldl(
                        fun(Key, {ok, Acc}) ->
                            case match(Key, IndexBase, Opts) of
                                {ok, Matches} ->
                                    {ok, hb_util:list_with(Acc, Matches)};
                                _ ->
                                    {error, not_found}
                            end;
                           (_Key, Error) ->
                                Error
                        end,
                        {ok, FirstMatches},
                        Rest
                    );
                _ ->
                    {error, not_found}
            end
    end.
