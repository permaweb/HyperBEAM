%%% @doc A reverse index from `key=value' pairs to the messages carrying
%%% them, in weave order: every message the node caches is indexed here as
%%% it is written, and `~query@1.0' serves Arweave GraphQL from it.
%%%
%%% Each pair a message carries names one store group,
%%% `~match@1.0/<name>=<value-path>', holding one key per message carrying
%%% the pair, with no value: the message's weave offset as twenty decimal
%%% digits, then its 43-byte signed ID and commitment device. The value's
%%% path is the hashpath of a binary value and the ID of a nested message
%%% -- the path `hb_cache' links the value under -- so a group's path is
%%% bounded and path-safe.
%%%
%%% The offset field sorts a group's keys by weave position, as bytes and as
%%% terms alike: `-1' for a message with no weave position, then the offsets,
%%% zero-padded, then `infinity' for an item awaiting its block. The ID
%%% distinguishes results at the same offset.
%%%
%%% The stores of the index are the node's `match-index' stores (`store/1').
%%% Each store's `from-key' pipeline returns messages with `offset', `id',
%%% and `commitment-device'; unknown IDs and devices are empty binaries.
%%% The default `key' and `entry' pipelines encode and decode native keys.
%%% Write entries carry their group's path in `path'. A published index maps
%%% groups and keys onto its rows with `row' and decodes them with `member',
%%% setting its commitment device in the pipeline. A page is read from
%%% every store, their entries merged.
%%%
%%% The pairs of a message are its own keys, but its commitments and private
%%% keys, and those two maps of the node's options -- else of the hook's
%%% handler message -- name: `match-paths', each the value a path resolves
%%% to on the message, and `match-all-paths', each element of the list a
%%% path resolves to. By default `match-all-paths' indexes a message under
%%% `committer' for each of its `committers'.
%%%
%%% Keys:
%%% ```
%%%     index:    Write the message in the request's `body' under each of
%%%               its pairs -- its keys, and the pairs `match-paths' and
%%%               `match-all-paths' name -- per ID in `signed-ids',
%%%               at the offset of its `priv/offset': a weave offset,
%%%               `infinity', or `-1' when it carries none. Only the
%%%               kernel's `cache-write' hook is served.
%%%     all:      The IDs of every message carrying all of the base's pairs.
%%%     <key>:    The IDs of every message carrying that pair of the base.
%%%     locate:   The matches of the base's pairs in weave order, each its
%%%               key as `member', its `offset', and its `id' when known:
%%%               from the request's `from', inclusive, or past its `after',
%%%               to its `to', exclusive, in its `direction' (`asc', the
%%%               default, or `desc'), at most its `limit'. A bound is a
%%%               key, an offset, `-1' or `infinity'.
%%%     row:      A group's path, or a key of one, as the row bits of a
%%%               published index, at the request's `key-hash-size',
%%%               `value-hash-size' and `offset-size'.
%%%     key:      An entry message as a native key, under its optional `path'.
%%%     entry:    A native key as an offset, ID and commitment-device message.
%%%     member:   A row as a message carrying its offset.
%%% '''
-module(dev_match).
-export([info/0, all/3, index/3, locate/3, row/3, member/3, entry/3, key/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The store path of the index, under which a published index hashes names.
-define(PREFIX, <<"~match@1.0/">>).
%% The width of a key's offset field at a weave offset.
-define(OFFSET_DIGITS, 20).
%% A byte above every ID, closing a cursor's offset when reading down.
-define(LAST_ID, <<"~">>).
%% The pairs indexed by each element of a list, unless a node names others.
-define(DEFAULT_ALL_PATHS,
    #{ <<"committer">> => <<"committers~message@1.0">> }
).

%% @doc Default all non-message@1.0 and device keys to match a single key in the
%% index.
info() ->
    #{
        excludes => [<<"set">>, <<"remove">>, <<"id">>, <<"verify">>],
        default => fun match/4
    }.

%% @doc Get the store configured for the match index.
store(Opts) ->
    LocalMatchIndex = maps:get(<<"match-index">>, Opts, undefined),
    LocalStore = maps:get(<<"store">>, Opts, undefined),
    GlobalMatchIndex = hb_opts:get(match_index, false, #{ <<"only">> => global }),
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
    Stores =
        case MatchIndexStore of
            false -> [];
            true -> hb_opts:get(store, [], Opts);
            ResolvedStore when not is_list(ResolvedStore) -> [ResolvedStore];
            ResolvedStore -> ResolvedStore
        end,
    Defaults = #{
        <<"to-key">> => <<"~match@1.0/key">>,
        <<"from-key">> => <<"~match@1.0/entry">>
    },
    [ maps:merge(Defaults, S) || S <- Stores ].

%%% Groups and their keys.

%% @doc The path of a pair's group.
group(Name, Value, Opts) ->
    <<
        ?PREFIX/binary,
        (hb_ao:normalize_key(Name))/binary, "=",
        (value_path(Value, Opts))/binary
    >>.

%% @doc The path `hb_cache' links a value under: the hashpath of a binary,
%% the ID of a message.
value_path(Link, Opts) when ?IS_LINK(Link) ->
    value_path(hb_cache:ensure_loaded(Link, Opts), Opts);
value_path(Bin, Opts) when is_binary(Bin) ->
    hb_path:hashpath(Bin, Opts);
value_path(Msg, Opts) ->
    hb_message:id(Msg, none, Opts#{ <<"linkify-mode">> => discard }).

%% @doc Encode an entry's fields and optional path, or pass a group through.
key(Base, _Req, Opts) ->
    {ok, key(hb_cache:ensure_all_loaded(
        hb_maps:get(<<"body">>, Base, Base, Opts), Opts
    ))}.

%% @doc A key of a group: the offset, signed ID, and commitment device.
key(#{ <<"path">> := Path } = Match) ->
    hb_path:to_binary([Path, key(maps:remove(<<"path">>, Match))]);
key(#{ <<"offset">> := Offset } = Match) ->
    ID = maps:get(<<"id">>, Match, <<>>),
    Device = maps:get(<<"commitment-device">>, Match, <<>>),
    case ID of
        <<>> -> digits(Offset);
        _ -> <<(digits(Offset))/binary, ID/binary, Device/binary>>
    end;
key(Path) when is_binary(Path) -> Path.

%% @doc An offset as a key's field: twenty digits at a weave offset, and
%% `-1' and `infinity' as themselves.
digits(Offset) when is_integer(Offset), Offset >= 0 ->
    hb_util:bin(io_lib:format("~*..0B", [?OFFSET_DIGITS, Offset]));
digits(Other) ->
    hb_util:bin(Other).

%% @doc Decode a native key, or normalize the fields decoded by a pipeline.
entry(Base, _Req, Opts) ->
    {ok, parse(hb_maps:get(<<"body">>, Base, Base, Opts))}.

%% @doc An encoded key or decoded fields as an entry message.
parse(#{ <<"offset">> := At } = Match) ->
    #{ <<"offset">> := Offset } = cursor(At),
    maps:merge(parse(Offset, <<>>), Match#{ <<"offset">> => Offset });
parse(<<"-1", Rest/binary>>) -> parse(-1, Rest);
parse(<<"infinity", Rest/binary>>) -> parse(infinity, Rest);
parse(<<Digits:?OFFSET_DIGITS/binary, Rest/binary>>) ->
    parse(hb_util:int(Digits), Rest);
parse(Digits) -> parse(hb_util:int(Digits), <<>>).

%% @doc Split the fixed-width signed ID from its trailing device.
parse(Offset, <<ID:43/binary, Device/binary>>) ->
    #{ <<"offset">> => Offset, <<"id">> => ID, <<"commitment-device">> => Device };
parse(Offset, ID) ->
    #{ <<"offset">> => Offset, <<"id">> => ID, <<"commitment-device">> => <<>> }.

%% @doc The position of a key: its offset and ID. An empty ID names an
%% offset boundary, closing the offset when reading down.
position(desc, #{ <<"offset">> := Offset, <<"id">> := <<>> }) ->
    {Offset, ?LAST_ID};
position(_Direction, #{ <<"offset">> := Offset, <<"id">> := ID }) ->
    {Offset, ID}.

%%% Writing the index.

%% @doc Write the message in the request's `body' under each of its pairs,
%% per ID the request names, at the offset of its `priv/offset': a weave
%% offset, `infinity', or `-1' when it carries none. A node without stores
%% of the index, or with none that takes the keys, writes nothing. Only the
%% kernel's `cache-write' hook is served: it marks its request in a private
%% key, which a request over HTTP cannot carry.
index(Handler, Req, Opts) ->
    case hb_private:get(<<"hook-caller">>, Req, Opts) of
        <<"kernel">> -> index_message(Handler, Req, Opts);
        _ ->
            {error,
                #{
                    <<"status">> => 401,
                    <<"body">> => <<"Unauthorized caller.">>
                }
            }
    end.
%% @doc The message written under its pairs, per ID. A commitment is not
%% indexed as a message of its own: the message it commits is indexed under
%% its committers.
index_message(
    _Handler,
    Req = #{ <<"body">> := #{ <<"commitment-device">> := _ } },
    _Opts
) ->
    {ok, Req};
index_message(Handler, Req, Opts) ->
    case store(Opts) of
        [] ->
            {ok, Req};
        Stores ->
            index_message(
                Handler, Req, hb_maps:get(<<"signed-ids">>, Req, [], Opts),
                Stores, Opts
            )
    end.
%% @doc Unsigned messages have no IDs to index; signed messages write their
%% pairs to the configured stores.
index_message(_Handler, Req, [], _Stores, _Opts) ->
    {ok, Req};
index_message(Handler, Req, IDs, Stores, Opts) ->
    Msg = hb_maps:get(<<"body">>, Req, #{}, Opts),
    Offset = hb_private:get(<<"offset">>, Msg, -1, Opts),
    Groups =
        [
            group(Name, Value, Opts)
        ||
            {Name, Value} <- pairs(Handler, Msg, Opts)
        ],
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    Members =
        [
            #{
                <<"offset">> => Offset, <<"id">> => ID,
                <<"commitment-device">> =>
                    hb_util:deep_get([ID, <<"commitment-device">>], Commitments, Opts)
            }
        ||
            ID <- IDs
        ],
    Keys =
        maps:from_list(
            [
                {Member#{ <<"path">> => Group }, <<>>}
            ||
                Group <- Groups,
                Member <- Members
            ]
        ),
    lists:foreach(
        fun(Group) -> hb_store:group(Stores, Group, Opts) end,
        Groups
    ),
    case hb_store:write(Stores, Keys, Opts) of
        ok -> {ok, Req};
        {error, not_found} -> {ok, Req};
        Error -> Error
    end.

%% @doc The pairs a message carries: its own keys, but its commitments and
%% private keys; the value each path of `match-paths' resolves to; and each
%% element of the list each path of `match-all-paths' resolves to.
pairs(Handler, Msg, Opts) ->
    Single = resolved(<<"match-paths">>, #{}, Handler, Msg, Opts),
    All =
        resolved(
            <<"match-all-paths">>, ?DEFAULT_ALL_PATHS, Handler, Msg, Opts
        ),
    Own =
        hb_maps:without(
            [ Name || {Name, _} <- Single ++ All ],
            hb_message:uncommitted(hb_private:reset(Msg)),
            Opts
        ),
    hb_maps:to_list(Own, Opts) ++ Single ++
        [
            {Name, Element}
        ||
            {Name, List} <- All,
            Element <- elements(List)
        ].

%% @doc The pairs a map of paths names, each the value its path resolves to
%% on the message. The node's options hold the map, else the hook's handler
%% message, else the default.
%% A computed key may select its device with `KEY~DEVICE'. Without `~DEVICE',
%% indexing attempts to load each message's device, subject to the node's
%% trust settings.
resolved(Key, Default, Handler, Msg, Opts) ->
    Paths = hb_opts:get(Key, hb_maps:get(Key, Handler, Default, Opts), Opts),
    [
        {hb_ao:normalize_key(Name), Value}
    ||
        {Name, Path} <- hb_maps:to_list(Paths, Opts),
        {ok, Value} <-
            [case binary:split(Path, <<"~">>) of
                [Part, Device] -> hb_ao:raw(Device, Msg, #{ <<"path">> => Part }, Opts);
                [Part] -> hb_ao:raw(Msg, #{ <<"path">> => Part }, Opts)
            end]
    ].

%% @doc The elements of a list, or a value alone.
elements(List) when is_list(List) -> List;
elements(Value) -> [Value].

%%% Matching by ID.

%% @doc Match a single key-value pair in the index, returning all message IDs
%% that contain the key-value pair.
match(Key, Base, Req, Opts) ->
    case hb_maps:find(Key, Base, Opts) of
        {ok, Value} -> ids(#{ Key => Value }, Req, Opts);
        error -> {error, not_found}
    end.

%% @doc Match the full base message against the index, returning the
%% intersection of all matches for each key.
all(Base, Req, Opts) ->
    ids(Base, Req, Opts).

%% @doc The IDs of the messages carrying every pair of a template: those of
%% its matches -- every one, unless the request bounds the page -- that
%% carry an ID, each once. The node's own stores hold them: those of local
%% scope, as a published index carries no IDs.
ids(Template, Req, Opts) ->
    Local = hb_store:scope(store(Opts), local),
    Bounded = maps:merge(#{ <<"limit">> => all }, Req),
    case locate(Template, Bounded, Opts#{ <<"match-index">> => Local }) of
        {ok, Matches} ->
            {ok, hb_util:unique([ ID || #{ <<"id">> := ID } <- identified(Matches) ])};
        {error, _} ->
            {error, not_found}
    end.

%%% Reading the index.

%% @doc The matches of the base message's pairs in weave order: each pair's
%% group is read from every store, and a key every group holds at one
%% position is a match. A base without pairs matches nothing, and a node
%% without stores of the index is `not_found'.
locate(Base, Req, Opts) ->
    {Direction, Cursor, Exclusive, To, Limit} = bounds(Req, Opts),
    Stores = store(Opts),
    Groups =
        [
            {group(Name, Value, Opts), [ {Store, unread} || Store <- Stores ]}
        ||
            {Name, Value} <- template(Base, Opts)
        ],
    case {Groups, Stores} of
        {[], _} ->
            {ok, []};
        {_, []} ->
            {error, not_found};
        _ ->
            maybe
                {ok, Matches} ?=
                    locate(
                        Direction, Groups, Cursor, Exclusive, To, Limit,
                        [], Opts
                    ),
                {ok, [ Match#{ <<"member">> => key(Match) } || Match <- Matches ]}
            end
    end.

%% @doc The pairs a base message names: every key but its commitments and
%% private keys, in the wire form the index is written from.
template(Base, Opts) ->
    hb_maps:to_list(
        hb_maps:without(
            [<<"ao-types">>],
            hb_message:convert(
                hb_message:uncommitted(hb_private:reset(Base)),
                tabm,
                <<"structured@1.0">>,
                Opts
            ),
            Opts
        ),
        Opts
    ).

%% @doc The direction, cursor and bounds of a page: it starts at the
%% request's `from' -- the index's low end ascending, its high end
%% descending, when unnamed -- or past its `after', stops short of its
%% `to', and takes at most its `limit' matches.
bounds(Req, Opts) ->
    Direction = hb_util:atom(hb_maps:get(<<"direction">>, Req, asc, Opts)),
    {Cursor, Exclusive} =
        case hb_maps:get(<<"after">>, Req, none, Opts) of
            none ->
                From = hb_maps:get(<<"from">>, Req, start(Direction), Opts),
                {cursor(From), false};
            After ->
                {cursor(After), true}
        end,
    Limit =
        case hb_maps:get(<<"limit">>, Req, all, Opts) of
            all -> all;
            Count -> hb_util:int(Count)
        end,
    To = cursor(hb_maps:get(<<"to">>, Req, none, Opts)),
    {Direction, Cursor, Exclusive, To, Limit}.

%% @doc The end of the index a page starts from without a cursor.
start(asc) -> -1;
start(desc) -> infinity.

%% @doc A cursor from a key, an offset, `-1' or `infinity'.
cursor(none) -> none;
cursor(Offset) when is_integer(Offset); is_atom(Offset) -> parse(Offset, <<>>);
cursor(Key) -> parse(Key).

%% @doc The matches from the cursor -- inclusive until a match, and
%% exclusive past it -- until the page is full, the `to' bound is reached
%% or a group runs out. Each group's keys are read from every store a page
%% at a time, as the cursor passes them.
locate(_Direction, _Groups, _Cursor, _Exclusive, _To, 0, Acc, _Opts) ->
    {ok, lists:reverse(Acc)};
locate(Direction, Groups, Cursor, Exclusive, To, Limit, Acc, Opts) ->
    case step(Direction, Groups, Cursor, Exclusive, Opts) of
        {Status, Key, Read} ->
            case reached(Direction, Key, To) of
                true ->
                    {ok, lists:reverse(Acc)};
                false when Status =:= match ->
                    locate(
                        Direction, Read, Key, true, To, remaining(Limit),
                        [Key | Acc], Opts
                    );
                false ->
                    locate(Direction, Read, Key, false, To, Limit, Acc, Opts)
            end;
        exhausted ->
            {ok, lists:reverse(Acc)};
        {error, _} = Error ->
            Error
    end.

%% @doc The matches a page has room for after one.
remaining(all) -> all;
remaining(Limit) -> Limit - 1.

%% @doc Whether a key has reached the page's exclusive `to' bound.
reached(_Direction, _Key, none) -> false;
reached(asc, Key, To) -> position(asc, Key) >= position(asc, To);
reached(desc, Key, To) -> position(desc, Key) =< position(desc, To).

%% @doc One step from the cursor: each group's next key. At one offset,
%% groups without an ID-less row must agree on ID; other groups accept any ID.
%% The last in the direction is otherwise the `next' cursor, as no match lies
%% before it. A group with nothing left ends the page. The groups come back
%% with their pages as read.
step(Direction, Groups, Cursor, Exclusive, Opts) ->
    maybe
        {ok, Next} ?= next_keys(Direction, Groups, Cursor, Exclusive, Opts),
        Keys = [ Key || {_Group, Key, _Pages} <- Next ],
        Read = [ {Group, Pages} || {Group, _Key, Pages} <- Next ],
        Required =
            [
                case [H || {_, [H = #{ <<"offset">> := O2, <<"id">> := <<>> } | _]}
                        <- Pages, O2 =:= O] of
                    [Head | _] -> Head;
                    [] -> Key
                end
            ||
                {_Group, Key = #{ <<"offset">> := O }, Pages} <- Next
            ],
        Known = identified(Required),
        Offsets = lists:usort([ O || #{ <<"offset">> := O } <- Keys ]),
        IDs = lists:usort([ ID || #{ <<"id">> := ID } <- Known ]),
        case {Offsets, IDs, identified(Keys)} of
            {[Offset], [], []} when Exclusive,
                    map_get(<<"offset">>, Cursor) =:= Offset,
                    map_get(<<"id">>, Cursor) =/= <<>> ->
                % An offset-only row must not repeat an ID already returned here.
                step(Direction, Read, cursor(Offset), true, Opts);
            {[_], IDs, _} when length(IDs) =< 1 ->
                {match, hd(Known ++ [first_of(Direction, Keys)]), Read};
            _ -> {next, last_of(Direction, Required), Read}
        end
    end.

%% @doc Each group's next key with its pages as read, in order.
next_keys(_Direction, [], _Cursor, _Exclusive, _Opts) ->
    {ok, []};
next_keys(Direction, [{Group, Pages} | Rest], Cursor, Exclusive, Opts) ->
    maybe
        {ok, Key, Read} ?=
            next_key(Direction, Group, Pages, Cursor, Exclusive, Opts),
        {ok, Others} ?= next_keys(Direction, Rest, Cursor, Exclusive, Opts),
        {ok, [{Group, Key, Read} | Others]}
    end.

%% @doc The keys carrying an ID.
identified(Keys) -> [ Key || Key = #{ <<"id">> := ID } <- Keys, ID =/= <<>> ].

%% @doc The last key in the direction.
last_of(asc, Keys) ->
    element(2, lists:max([ {position(asc, Key), Key} || Key <- Keys ]));
last_of(desc, Keys) ->
    element(2, lists:min([ {position(desc, Key), Key} || Key <- Keys ])).

%% @doc A group's next key from the cursor across the stores: the first at
%% or past it ascending, the last descending -- past it alone when the
%% cursor is exclusive -- or `exhausted' when none is left.
next_key(Direction, Group, Pages, Cursor, Exclusive, Opts) ->
    maybe
        {ok, Read} ?= pages(Direction, Group, Pages, Cursor, Exclusive, Opts),
        case [ Head || {_Store, [Head | _]} <- Read ] of
            [] -> exhausted;
            Heads -> {ok, first_of(Direction, Heads), Read}
        end
    end.

%% @doc Each store's page of a group's keys from the cursor.
pages(_Direction, _Group, [], _Cursor, _Exclusive, _Opts) ->
    {ok, []};
pages(Direction, Group, [{Store, Page} | Rest], Cursor, Exclusive, Opts) ->
    maybe
        {ok, Left} ?=
            from_cursor(Direction, Group, Page, Cursor, Exclusive, Store, Opts),
        {ok, Others} ?= pages(Direction, Group, Rest, Cursor, Exclusive, Opts),
        {ok, [{Store, Left} | Others]}
    end.

%% @doc A store's page from the cursor: the keys behind it dropped, and a
%% page the cursor has passed -- or none read yet -- read from the cursor.
%% A store whose batch held nothing past an earlier cursor has no keys
%% left.
from_cursor(_Direction, _Group, [], _Cursor, _Exclusive, _Store, _Opts) ->
    {ok, []};
from_cursor(Direction, Group, Page, Cursor, Exclusive, Store, Opts) ->
    Behind = fun(Key) -> behind(Direction, Key, Cursor, Exclusive) end,
    case Page =/= unread andalso lists:dropwhile(Behind, Page) of
        [_ | _] = Left ->
            {ok, Left};
        _ ->
            maybe
                {ok, Read} ?= page(Direction, Group, Cursor, Store, Opts),
                {ok, lists:dropwhile(Behind, Read)}
            end
    end.

%% @doc Whether a key lies behind the cursor in the direction, or at it
%% when the cursor is exclusive. An ID-less row remains for IDs at its offset.
behind(_Direction, #{ <<"offset">> := Offset },
        #{ <<"offset">> := Offset, <<"id">> := <<>> }, Exclusive) ->
    Exclusive;
behind(_Direction, #{ <<"offset">> := Offset, <<"id">> := <<>> },
        #{ <<"offset">> := Offset }, _Exclusive) ->
    false;
behind(Direction, Key, Cursor, Exclusive) ->
    case {position(Direction, Key), position(Direction, Cursor)} of
        {Same, Same} -> Exclusive;
        {At, From} when Direction =:= asc -> At < From;
        {At, From} -> At > From
    end.

%% @doc The first of the stores' first keys in the direction, one carrying
%% an ID ahead of one at the same offset without.
first_of(asc, Heads) ->
    {_, _, _, Head} =
        lists:min(
            [ {Offset, ID =:= <<>>, ID, K}
            || K = #{ <<"offset">> := Offset, <<"id">> := ID } <- Heads ]
        ),
    Head;
first_of(desc, Heads) ->
    {_, _, _, Head} =
        lists:max(
            [ {Offset, ID =/= <<>>, ID, K}
            || K = #{ <<"offset">> := Offset, <<"id">> := ID } <- Heads ]
        ),
    Head.

%% @doc One store's page of a group's keys from the cursor, in the
%% direction: the batch the store finds without further work. A batch
%% holding nothing past the cursor ends the store's part, so a store's
%% batch must hold a key past it while any remains. A store without the
%% group has none.
page(Direction, Group, Cursor, Store, Opts) ->
    Request =
        #{
            <<"list">> => Group,
            <<"from">> => from(Direction, Cursor),
            <<"limit">> => batch,
            <<"direction">> => Direction
        },
    case hb_store:list([Store], Request, Opts) of
        {error, not_found} -> {ok, []};
        Result -> Result
    end.

%% @doc The key a store's page is read from: a cursor naming no ID stands
%% for every key of its offset, so reading down it closes with the byte
%% above every ID.
from(desc, #{ <<"id">> := <<>> } = Cursor) ->
    Cursor#{ <<"id">> => ?LAST_ID, <<"commitment-device">> => <<>> };
from(desc, #{ <<"commitment-device">> := <<>> } = Cursor) ->
    Cursor#{ <<"commitment-device">> => ?LAST_ID };
from(_Direction, Cursor) -> Cursor.

%%% The rows of a published index.

%% @doc A group's path, or a key of one, as the row bits of a published
%% index: the leading `key-hash-size' bits of the SHA-256 of the name under
%% `~match@1.0/' and the leading `value-hash-size' bits of the digest the
%% value's path names; or a key's offset in `offset-size' bits, `-1' as zero
%% and `infinity', with every offset past the field, as its maximum.
row(Base, Req, Opts) ->
    {ok, row(
        hb_cache:ensure_all_loaded(hb_maps:get(<<"body">>, Base, <<>>, Opts), Opts),
        sizes(Req, Opts)
    )}.

%% @doc Encode a group, an offset, or a complete row from their fields.
row(#{ <<"path">> := Path } = Match, Sizes) ->
    <<(row(Path, Sizes))/bitstring,
        (row(maps:remove(<<"path">>, Match), Sizes))/bitstring>>;
row(#{ <<"offset">> := Offset }, {_, _, OffsetBits}) ->
    Max = (1 bsl OffsetBits) - 1,
    case Offset of
        -1 -> <<0:OffsetBits>>;
        _ when is_integer(Offset), Offset < Max -> <<Offset:OffsetBits>>;
        _ -> <<Max:OffsetBits>>
    end;
row(Body, {KeyBits, ValueBits, _OffsetBits}) ->
    [Name, ValuePath] = binary:split(Body, <<"=">>),
    <<KeyHash:KeyBits/bitstring, _/bitstring>> =
        crypto:hash(sha256, <<?PREFIX/binary, Name/binary>>),
    <<ValueHash:ValueBits/bitstring, _/bitstring>> = hb_util:decode(ValuePath),
    <<KeyHash/bitstring, ValueHash/bitstring>>.

%% @doc A row as a message carrying its trailing `offset-size' bits as
%% `offset', with `infinity' at the field's maximum.
member(Base, Req, Opts) ->
    maybe
        {ok, Row} ?= body(Base, Opts),
        {KeyBits, ValueBits, OffsetBits} = sizes(Req, Opts),
        Max = (1 bsl OffsetBits) - 1,
        case Row of
            <<_:KeyBits, _:ValueBits, Max:OffsetBits>> ->
                {ok, cursor(infinity)};
            <<_:KeyBits, _:ValueBits, Offset:OffsetBits>> ->
                {ok, cursor(Offset)};
            _ ->
                {error, {'invalid-row', Row}}
        end
    end.

%% @doc The widths of a row's fields, from the request.
sizes(Req, Opts) ->
    {
        hb_util:int(hb_maps:get(<<"key-hash-size">>, Req, 0, Opts)),
        hb_util:int(hb_maps:get(<<"value-hash-size">>, Req, 0, Opts)),
        hb_util:int(hb_maps:get(<<"offset-size">>, Req, 0, Opts))
    }.

%% @doc The binary body of the base message.
body(Base, Opts) ->
    case hb_maps:find(<<"body">>, Base, Opts) of
        {ok, Body} when is_binary(Body) -> {ok, Body};
        _ -> {error, {'invalid-body', <<"No binary `body' key found.">>}}
    end.

%%% Tests

%% The map size of the LMDB test store: every environment reserves its
%% map's address space, which the suite's stores exhaust at the default.
-define(TEST_CAPACITY, 1024 * 1024 * 1024).

%% @doc A node indexing into an LMDB store of its own.
test_opts() ->
    Store =
        (hb_test_utils:test_store(hb_store_lmdb))#{
            <<"capacity">> => ?TEST_CAPACITY
        },
    hb_store:start([Store]),
    #{ <<"store">> => [Store], <<"match-index">> => [Store] }.

%% @doc Cache a message at an offset, answering its ID.
cache(Msg, Offset, Opts) ->
    {ok, ID} =
        hb_cache:write(hb_private:set(Msg, <<"offset">>, Offset, Opts), Opts),
    ID.

%% @doc The signed commitment IDs indexed for a message.
ids(Msg, Opts) ->
    [
        ID
    ||
        {ID, Comm} <- hb_maps:to_list(
            hb_maps:get(<<"commitments">>, Msg, #{}, Opts), Opts
        ),
        hb_maps:is_key(<<"committer">>, Comm, Opts)
    ].

%% @doc The matches of a template as `{Offset, ID}' pairs.
matches(Template, Req, Opts) ->
    {ok, Matches} =
        hb_ao:raw(
            <<"match@1.0">>,
            Template,
            Req#{ <<"path">> => <<"locate">> },
            Opts
        ),
    [
        {Offset, maps:get(<<"id">>, Match, <<>>)}
    ||
        Match = #{ <<"offset">> := Offset } <- Matches
    ].

%% @doc Messages cached at weave offsets, at `infinity' and without an
%% offset are located in weave order in either direction, between bounds,
%% by one pair or by two, and `all' answers their IDs.
weave_order_test() ->
    Opts = (test_opts())#{ <<"priv-wallet">> => ar_wallet:new() },
    Cache =
        fun(Msg, Offset) ->
            Signed = hb_message:commit(Msg, Opts),
            cache(Signed, Offset, Opts),
            [ID] = ids(Signed, Opts),
            ID
        end,
    Template = #{ <<"type">> => <<"Message">>, <<"device">> => <<"message@1.0">> },
    Mined = Cache(Template#{ <<"n">> => <<"1">>, <<"a">> => <<"yes">> }, 5),
    {ok, [MinedMatch]} =
        hb_ao:raw(
            <<"match@1.0">>, <<"locate">>, Template, #{}, Opts
        ),
    Device = hb_maps:get(<<"commitment-device">>, MinedMatch),
    ?assertEqual(<<"httpsig@1.0">>, Device),
    ?assertEqual(key(MinedMatch), maps:get(<<"member">>, MinedMatch)),
    ?assertEqual(
        {ok, [maps:remove(<<"member">>, MinedMatch)]},
        hb_store:list(
            store(Opts), #{ <<"list">> => group(<<"n">>, <<"1">>, Opts) }, Opts
        )
    ),
    Normalized =
        [ S#{ <<"from-key">> =>
            <<"~bits@1.0/from=offset:160,id:344,commitment-device:_",
                "/~match@1.0/entry/set&source=pipeline">> }
        || S <- store(Opts) ],
    ?assertEqual(
        {ok, [MinedMatch#{
            <<"device">> => <<"match@1.0">>, <<"source">> => <<"pipeline">>
        }]},
        hb_ao:raw(
            <<"match@1.0">>, <<"locate">>, Template, #{},
            Opts#{ <<"match-index">> => Normalized }
        )
    ),
    Later = Cache(Template#{ <<"n">> => <<"2">> }, 7),
    Pending = Cache(Template#{ <<"n">> => <<"3">> }, infinity),
    Unmined = Cache(Template#{ <<"n">> => <<"4">> }, -1),
    ?assertEqual([], matches(Template#{ <<"device">> => <<"other">> }, #{}, Opts)),
    ?assertEqual(
        [{-1, Unmined}, {5, Mined}, {7, Later}, {infinity, Pending}],
        matches(Template, #{}, Opts)
    ),
    ?assertEqual(
        [{infinity, Pending}, {7, Later}, {5, Mined}, {-1, Unmined}],
        matches(Template, #{ <<"direction">> => desc }, Opts)
    ),
    % Bounds: an offset range leaves `-1' and `infinity' out, a key resumes
    % past itself, and a limit cuts the page.
    ?assertEqual(
        [{5, Mined}, {7, Later}],
        matches(Template, #{ <<"from">> => 0, <<"to">> => infinity }, Opts)
    ),
    ?assertEqual(
        [{7, Later}, {5, Mined}],
        matches(
            Template,
            #{ <<"direction">> => desc, <<"from">> => 7, <<"to">> => -1 },
            Opts
        )
    ),
    ?assertEqual(
        [{7, Later}],
        matches(
            Template,
            #{
                <<"after">> => key(#{ <<"offset">> => 5, <<"id">> => Mined }),
                <<"limit">> => 1
            },
            Opts
        )
    ),
    % Two pairs intersect, and the IDs are answered by `all' and by a key.
    ?assertEqual(
        [{7, Later}],
        matches(Template#{ <<"n">> => <<"2">> }, #{}, Opts)
    ),
    {ok, All} =
        hb_ao:raw(
            <<"match@1.0">>,
            Template,
            #{ <<"path">> => <<"all">> },
            Opts
        ),
    ?assertEqual(lists:sort([Mined, Later, Pending, Unmined]), lists:sort(All)),
    ?assertEqual(
        {ok, [Later]},
        hb_ao:raw(
            <<"match@1.0">>,
            Template#{ <<"n">> => <<"2">> },
            #{ <<"path">> => <<"n">> },
            Opts
        )
    ),
    % Distinct signed IDs at one offset survive matching and both cursor orders.
    Peer = Cache(Template#{ <<"b">> => <<"yes">> }, 5),
    [First, Second] = Shared = lists:sort([{5, Mined}, {5, Peer}]),
    [FirstCursor, SecondCursor] =
        [ #{ <<"offset">> => O, <<"id">> => ID } || {O, ID} <- Shared ],
    Asc = #{ <<"from">> => 5, <<"to">> => 7 },
    Desc = #{ <<"direction">> => desc, <<"from">> => 5, <<"to">> => -1 },
    ?assertEqual(Shared, matches(Template, Asc, Opts)),
    ?assertEqual(lists:reverse(Shared), matches(Template, Desc, Opts)),
    ?assertEqual([First], matches(Template, Asc#{ <<"limit">> => 1 }, Opts)),
    ?assertEqual(
        [Second], matches(Template, Asc#{ <<"after">> => FirstCursor }, Opts)
    ),
    ?assertEqual(
        [First], matches(Template, Desc#{ <<"after">> => SecondCursor }, Opts)
    ),
    ?assertEqual(
        [{7, Later}],
        matches(Template, #{ <<"after">> => 5, <<"to">> => infinity }, Opts)
    ),
    ?assertEqual(
        [Second, First],
        matches(Template, Desc#{ <<"from">> => SecondCursor }, Opts)
    ),
    ?assertEqual([], matches(Template, Desc#{ <<"after">> => 5 }, Opts)),
    ?assertEqual([], matches(Template, Asc#{ <<"to">> => 5 }, Opts)),
    ?assertEqual([], matches(Template, Desc#{ <<"to">> => 5 }, Opts)),
    ?assertEqual(
        [], matches(#{ <<"a">> => <<"yes">>, <<"b">> => <<"yes">> }, #{}, Opts)
    ),
    % Published rows carry only offsets, including pairs absent locally.
    Published = test_opts(),
    Stores = store(Published),
    Mixed = Template#{ <<"published">> => <<"yes">> },
    Asymmetric = #{ <<"a">> => <<"yes">>, <<"b">> => <<"yes">> },
    lists:foreach(
        fun({Name, Value}) ->
            Group = group(Name, Value, Opts),
            hb_store:group(Stores, Group, Opts),
            ok = hb_store:write(
                Stores, #{ <<Group/binary, "/", (digits(5))/binary>> => <<>> },
                Opts
            )
        end,
        maps:to_list(maps:merge(Mixed, Asymmetric))
    ),
    ?assertEqual([{5, <<>>}], matches(Mixed, Asc, Published)),
    MixedOpts = Opts#{ <<"match-index">> => store(Opts) ++ Stores },
    ?assertEqual(Shared, matches(Mixed, Asc, MixedOpts)),
    ?assertEqual(
        lists:reverse(Shared), matches(Mixed, Desc, MixedOpts)
    ),
    ?assertEqual(Shared, matches(Asymmetric, Asc, MixedOpts)),
    ?assertEqual(
        lists:reverse(Shared), matches(Asymmetric, Desc, MixedOpts)
    ).

%% @doc A message is indexed under each of its committers, and a
%% `match-all-paths' map in the node's options names the pair over the
%% hook's handler message's.
paths_test() ->
    Opts = test_opts(),
    Wallets = [ar_wallet:new(), ar_wallet:new()],
    Addresses = [ hb_util:human_id(ar_wallet:to_address(W)) || W <- Wallets ],
    Signed =
        lists:foldl(
            fun(Wallet, Msg) ->
                hb_message:commit(Msg, Opts#{ <<"priv-wallet">> => Wallet })
            end,
            #{
                <<"a">> => <<"b">>,
                <<"device">> => <<"unavailable@1.0">>,
                <<"committer">> => <<"untrusted">>
            },
            Wallets
        ),
    % Each signed ID contributes a separate result at the same offset.
    {ok, CommitmentsPath} =
        hb_cache:write(hb_maps:get(<<"commitments">>, Signed), Opts),
    cache(Signed#{ <<"commitments">> => {link, CommitmentsPath, #{}} }, 1, Opts),
    IDs = lists:sort(ids(Signed, Opts)),
    ?assertEqual(2, length(IDs)),
    cache(#{ <<"committer">> => <<"untrusted">> }, 3, Opts),
    ?assertEqual([], matches(#{ <<"committer">> => <<"untrusted">> }, #{}, Opts)),
    lists:foreach(
        fun(Address) ->
            {ok, Found} =
                hb_cache:match(#{ <<"committer">> => Address }, Opts),
            ?assertEqual(IDs, lists:sort(Found))
        end,
        Addresses
    ),
    Named =
        Opts#{
            <<"match-paths">> => #{ <<"computed">> => <<"test-func">> },
            <<"match-all-paths">> => #{ signer => <<"committers">> },
            <<"on">> => #{
                <<"cache-write">> => #{
                    <<"device">> => <<"match@1.0">>,
                    <<"path">> => <<"index">>,
                    <<"match-all-paths">> => ?DEFAULT_ALL_PATHS
                }
            }
        },
    [Wallet | _] = Wallets,
    [Address | _] = Addresses,
    Second =
        hb_message:commit(
            #{
                <<"c">> => <<"d">>, <<"signer">> => <<"untrusted">>,
                <<"device">> => <<"test-device@1.0">>
            },
            Opts#{ <<"priv-wallet">> => Wallet }
        ),
    cache(Second, 2, Named),
    SecondIDs = ids(Second, Opts),
    ?assertMatch(
        [{2, _}], matches(#{ <<"computed">> => <<"GOOD FUNCTION">> }, #{}, Named)
    ),
    ?assertEqual([], matches(#{ <<"signer">> => <<"untrusted">> }, #{}, Named)),
    {ok, [Signer]} = hb_cache:match(#{ <<"signer">> => Address }, Named),
    ?assert(lists:member(Signer, SecondIDs)),
    {ok, Committers} = hb_cache:match(#{ <<"committer">> => Address }, Named),
    ?assertEqual(IDs, lists:sort(Committers)).

%% @doc A group's path hashes to the row prefix of a published index, a key
%% to its offset bits, and a row back to its key.
row_test() ->
    Args = <<"&key-hash-size=39&value-hash-size=40&offset-size=49">>,
    Normalize =
        fun(Key, Body) ->
            hb_ao:raw(
                #{
                    <<"path">> => <<"~match@1.0/", Key/binary, Args/binary>>,
                    <<"0.body">> => Body
                },
                #{}
            )
        end,
    ValuePath = hb_path:hashpath(<<"Message">>, #{}),
    <<KeyHash:39/bitstring, _/bitstring>> =
        crypto:hash(sha256, <<"~match@1.0/type">>),
    <<ValueHash:40/bitstring, _/bitstring>> = hb_util:decode(ValuePath),
    ?assertEqual(
        {ok, <<KeyHash/bitstring, ValueHash/bitstring>>},
        Normalize(<<"row">>, <<"type=", ValuePath/binary>>)
    ),
    ?assertEqual(
        {ok, <<5:49>>},
        Normalize(<<"row">>, #{ <<"offset">> => 5 })
    ),
    ?assertEqual({ok, <<0:49>>}, Normalize(<<"row">>, #{ <<"offset">> => -1 })),
    ?assertEqual(
        {ok, <<((1 bsl 49) - 1):49>>},
        Normalize(<<"row">>, #{ <<"offset">> => infinity })
    ),
    {ok, Row} = Normalize(<<"row">>, #{
        <<"path">> => <<"type=", ValuePath/binary>>, <<"offset">> => 5
    }),
    ?assertEqual(<<KeyHash/bitstring, ValueHash/bitstring, 5:49>>, Row),
    ?assertEqual(
        {ok, #{ <<"offset">> => 5, <<"id">> => <<>>,
            <<"commitment-device">> => <<>> }},
        Normalize(<<"member">>, Row)
    ).

%% @doc A request over HTTP loses its private keys, so it is not the kernel.
unauthorized_index_test() ->
    Node =
        hb_http_server:start_node(
            #{ <<"store">> => hb_test_utils:test_store() }
        ),
    Res =
        hb_http:post(
            Node,
            #{
                <<"path">> => <<"/~match@1.0/index">>,
                <<"body">> => #{ <<"a">> => <<"b">> },
                <<"signed-ids">> => [<<"id">>],
                <<"priv">> => #{ <<"hook-caller">> => <<"kernel">> }
            },
            #{}
        ),
    ?assertMatch({error, #{ <<"status">> := 401 }}, Res).
