%%% @doc A reverse index from `key=value' pairs to the messages carrying
%%% them, in weave order: every message the node caches is indexed here as
%%% it is written, and `~query@1.0' serves Arweave GraphQL from it.
%%%
%%% Each pair a message carries names one store group,
%%% `~match@1.0/<name>=<value-path>', holding one key per message carrying
%%% the pair, with no value: the message's weave offset as twenty decimal
%%% digits, then its ID. The value's path is the hashpath of a binary value
%%% and the ID of a nested message -- the path `hb_cache' links the value
%%% under -- so a group's path is bounded and path-safe.
%%%
%%% The offset field sorts a group's keys by weave position, as bytes and as
%%% terms alike: `-1' for a message with no weave position, then the offsets,
%%% zero-padded, then `infinity' for an item awaiting its block. At an
%%% offset the offset alone identifies the item, as a published index
%%% carries no IDs; at `-1' and `infinity' the ID does.
%%%
%%% The stores of the index are the node's `match-index' stores (`store/1').
%%% A store of the node's own holds a group's keys as its children; a
%%% published index
%%% maps groups and keys onto its rows through its store message's `to-key'
%%% and `from-key', this device's `row' and `member' keys. A page is read
%%% from every store, their keys merged.
%%%
%%% Keys:
%%% ```
%%%     index:    Write the message in the request's `body' under each of
%%%               its pairs -- its keys, and the committer and target of its
%%%               commitments -- per ID the request's `ids' name, at the
%%%               offset of its `priv/offset': a weave offset, `infinity',
%%%               or `-1' when it carries none. Only the kernel's
%%%               `cache-write' hook is served.
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
%%%     member:   The row of a published index as a key of its group.
%%% '''
-module(dev_match).
-export([info/0, all/3, index/3, locate/3, row/3, member/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The store path of the index, under which a published index hashes names.
-define(PREFIX, <<"~match@1.0/">>).
%% The width of a key's offset field at a weave offset.
-define(OFFSET_DIGITS, 20).
%% A byte above every ID, closing a cursor's offset when reading down.
-define(LAST_ID, <<"~">>).

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
    case MatchIndexStore of
        false -> [];
        true -> hb_opts:get(store, [], Opts);
        ResolvedStore when not is_list(ResolvedStore) -> [ResolvedStore];
        ResolvedStore -> ResolvedStore
    end.

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

%% @doc A key of a group: the offset then the ID.
key({Offset, ID}) -> <<(digits(Offset))/binary, ID/binary>>.

%% @doc An offset as a key's field: twenty digits at a weave offset, and
%% `-1' and `infinity' as themselves.
digits(Offset) when is_integer(Offset), Offset >= 0 ->
    hb_util:bin(io_lib:format("~*..0B", [?OFFSET_DIGITS, Offset]));
digits(Other) ->
    hb_util:bin(Other).

%% @doc The offset and ID a key carries; a bare offset carries no ID.
parse(<<"-1", ID/binary>>) -> {-1, ID};
parse(<<"infinity", ID/binary>>) -> {infinity, ID};
parse(<<Digits:?OFFSET_DIGITS/binary, ID/binary>>) -> {hb_util:int(Digits), ID};
parse(Digits) -> {hb_util:int(Digits), <<>>}.

%% @doc The position of a key in the order read: its offset alone at a
%% weave offset, where it identifies the item; with its ID at `-1' and
%% `infinity', where an empty ID -- a bound naming every key of the offset
%% -- stands for the last of them in the direction read.
position(_Direction, {Offset, _ID}) when is_integer(Offset), Offset >= 0 ->
    {Offset, <<>>};
position(desc, {Offset, <<>>}) ->
    {Offset, ?LAST_ID};
position(_Direction, Key) ->
    Key.

%% @doc A match as a message: its key, its offset and its ID when known.
result(Key = {Offset, ID}) ->
    Match = #{ <<"member">> => key(Key), <<"offset">> => Offset },
    case ID of
        <<>> -> Match;
        _ -> Match#{ <<"id">> => ID }
    end.

%%% Writing the index.

%% @doc Write the message in the request's `body' under each of its pairs,
%% per ID the request names, at the offset of its `priv/offset': a weave
%% offset, `infinity', or `-1' when it carries none. A node without stores
%% of the index, or with none that takes the keys, writes nothing. Only the
%% kernel's `cache-write' hook is served: it marks its request in a private
%% key, which a request over HTTP cannot carry.
index(_Base, Req, Opts) ->
    case hb_private:get(<<"hook-caller">>, Req, Opts) of
        <<"kernel">> -> index(Req, Opts);
        _ ->
            {error,
                #{
                    <<"status">> => 401,
                    <<"body">> => <<"Unauthorized caller.">>
                }
            }
    end.
index(Req, Opts) ->
    Msg = hb_maps:get(<<"body">>, Req, #{}, Opts),
    Offset = hb_private:get(<<"offset">>, Msg, -1, Opts),
    Groups =
        [ group(Name, Value, Opts) || {Name, Value} <- pairs(Msg, Opts) ],
    Keys =
        maps:from_list(
            [
                {<<Group/binary, "/", (key({Offset, ID}))/binary>>, <<>>}
            ||
                Group <- Groups,
                ID <- hb_maps:get(<<"ids">>, Req, [], Opts)
            ]
        ),
    case store(Opts) of
        [] ->
            {ok, Req};
        Stores ->
            lists:foreach(
                fun(Group) -> hb_store:group(Stores, Group, Opts) end,
                Groups
            ),
            case hb_store:write(Stores, Keys, Opts) of
                ok -> {ok, Req};
                {error, not_found} -> {ok, Req};
                Error -> Error
            end
    end.

%% @doc The pairs a message carries: its keys, but its commitments and
%% private keys, and the committer and target of each of its commitments.
pairs(Base, Opts) ->
    Own = hb_message:uncommitted(hb_private:reset(Base)),
    Commitments =
        hb_message:commitments(#{ <<"committer">> => '_' }, Base, Opts),
    hb_maps:to_list(Own, Opts) ++
        [
            {Name, Value}
        ||
            Commitment <- hb_maps:values(Commitments, Opts),
            Name <- [<<"committer">>, <<"field-target">>],
            Value <- [hb_maps:get(Name, Commitment, <<>>, Opts)],
            Value =/= <<>>
        ].

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
            {ok, hb_util:unique([ ID || #{ <<"id">> := ID } <- Matches ])};
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
                {ok, [ result(Match) || Match <- Matches ]}
            end
    end.

%% @doc The pairs a base message names: every key but its device,
%% commitments and private keys, in the wire form the index is written
%% from.
template(Base, Opts) ->
    hb_maps:to_list(
        hb_maps:without(
            [<<"ao-types">>, <<"device">>],
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
cursor(Offset) when is_integer(Offset); is_atom(Offset) -> {Offset, <<>>};
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

%% @doc One step from the cursor: each group's next key. Keys at one
%% position are a `match', carrying the ID any of them knows; the last of
%% them in the direction is otherwise the `next' cursor, as no match lies
%% before it. A group with nothing left ends the page. The groups come back
%% with their pages as read.
step(Direction, Groups, Cursor, Exclusive, Opts) ->
    maybe
        {ok, Next} ?= next_keys(Direction, Groups, Cursor, Exclusive, Opts),
        Keys = [ Key || {_Group, Key, _Pages} <- Next ],
        Read = [ {Group, Pages} || {Group, _Key, Pages} <- Next ],
        case lists:usort([ position(Direction, Key) || Key <- Keys ]) of
            [_Position] -> {match, hd(identified(Keys) ++ Keys), Read};
            _ -> {next, last_of(Direction, Keys), Read}
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
identified(Keys) -> [ Key || Key = {_Offset, ID} <- Keys, ID =/= <<>> ].

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
%% when the cursor is exclusive.
behind(Direction, Key, Cursor, Exclusive) ->
    case {position(Direction, Key), position(Direction, Cursor)} of
        {Same, Same} -> Exclusive;
        {At, From} when Direction =:= asc -> At < From;
        {At, From} -> At > From
    end.

%% @doc The first of the stores' first keys in the direction, one carrying
%% an ID ahead of one at the same position without.
first_of(asc, Heads) ->
    {_, _, Head} =
        lists:min(
            [ {position(asc, K), ID =:= <<>>, K} || K = {_, ID} <- Heads ]
        ),
    Head;
first_of(desc, Heads) ->
    {_, _, Head} =
        lists:max(
            [ {position(desc, K), ID =/= <<>>, K} || K = {_, ID} <- Heads ]
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
        {ok, Keys} -> {ok, [ parse(Key) || Key <- Keys ]};
        {error, not_found} -> {ok, []};
        {error, _} = Error -> Error
    end.

%% @doc The key a store's page is read from: a cursor naming no ID stands
%% for every key of its offset, so reading down it closes with the byte
%% above every ID.
from(desc, {Offset, <<>>}) -> key({Offset, ?LAST_ID});
from(_Direction, Cursor) -> key(Cursor).

%%% The rows of a published index.

%% @doc A group's path, or a key of one, as the row bits of a published
%% index: the leading `key-hash-size' bits of the SHA-256 of the name under
%% `~match@1.0/' and the leading `value-hash-size' bits of the digest the
%% value's path names; or a key's offset in `offset-size' bits, `-1' as zero
%% and `infinity', with every offset past the field, as its maximum.
row(Base, Req, Opts) ->
    maybe
        {ok, Body} ?= body(Base, Opts),
        {KeyBits, ValueBits, OffsetBits} = sizes(Req, Opts),
        case binary:split(Body, <<"=">>) of
            [Name, ValuePath] ->
                <<KeyHash:KeyBits/bitstring, _/bitstring>> =
                    crypto:hash(sha256, <<?PREFIX/binary, Name/binary>>),
                <<ValueHash:ValueBits/bitstring, _/bitstring>> =
                    hb_util:decode(ValuePath),
                {ok, <<KeyHash/bitstring, ValueHash/bitstring>>};
            [Key] ->
                {Offset, _ID} = parse(Key),
                Max = (1 bsl OffsetBits) - 1,
                case Offset of
                    -1 -> {ok, <<0:OffsetBits>>};
                    _ when is_integer(Offset), Offset < Max ->
                        {ok, <<Offset:OffsetBits>>};
                    _ -> {ok, <<Max:OffsetBits>>}
                end
        end
    end.

%% @doc The row of a published index as a key of its group: the offset its
%% trailing `offset-size' bits carry, `infinity' at the field's maximum.
member(Base, Req, Opts) ->
    maybe
        {ok, Row} ?= body(Base, Opts),
        {KeyBits, ValueBits, OffsetBits} = sizes(Req, Opts),
        Max = (1 bsl OffsetBits) - 1,
        case Row of
            <<_:KeyBits, _:ValueBits, Max:OffsetBits>> ->
                {ok, digits(infinity)};
            <<_:KeyBits, _:ValueBits, Offset:OffsetBits>> ->
                {ok, digits(Offset)};
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
    Opts = test_opts(),
    Template = #{ <<"type">> => <<"Message">> },
    Mined = cache(Template#{ <<"n">> => <<"1">> }, 5, Opts),
    Later = cache(Template#{ <<"n">> => <<"2">> }, 7, Opts),
    Pending = cache(Template#{ <<"n">> => <<"3">> }, infinity, Opts),
    {ok, Unmined} = hb_cache:write(Template#{ <<"n">> => <<"4">> }, Opts),
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
            #{ <<"after">> => key({5, Mined}), <<"limit">> => 1 },
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
    ).

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
    ?assertEqual({ok, <<5:49>>}, Normalize(<<"row">>, key({5, <<"id">>}))),
    ?assertEqual({ok, <<0:49>>}, Normalize(<<"row">>, <<"-1">>)),
    ?assertEqual(
        {ok, <<((1 bsl 49) - 1):49>>},
        Normalize(<<"row">>, <<"infinity~">>)
    ),
    ?assertEqual(
        {ok, digits(5)},
        Normalize(
            <<"member">>,
            <<KeyHash/bitstring, ValueHash/bitstring, 5:49>>
        )
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
                <<"ids">> => [<<"id">>],
                <<"priv">> => #{ <<"hook-caller">> => <<"kernel">> }
            },
            #{}
        ),
    ?assertMatch({error, #{ <<"status">> := 401 }}, Res).
