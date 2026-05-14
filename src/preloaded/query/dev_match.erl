%%% @doc A reverse index for finding all message IDs with a given key-value pair.
-module(dev_match).
-export([info/0, all/3]).
-include("include/hb.hrl").

%% @doc Default all non-message@1.0 and device keys to match a single key in the
%% index.
info() ->
    #{
        excludes => [<<"set">>, <<"remove">>, <<"id">>, <<"verify">>],
        default => fun match/4
    }.

%% @doc Match a single key-value pair in the index, returning all message IDs that
%% contain the key-value pair.
match(Key, Base, _Req, Opts) -> match(Key, Base, Opts).
match(Key, Base, Opts) ->
    Store = hb_cache:match_store(Opts),
    {ok, Value} = hb_maps:find(Key, Base, Opts),
    case hb_store:list(
        Store,
        hb_cache:match_address(
            hb_ao:normalize_key(Key),
            hb_cache:match_value_path(Value, Opts)
        ),
        Opts
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
