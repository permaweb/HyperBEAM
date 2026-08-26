%%% @doc The fixed-width rows of the published Arweave indexes.
%%%
%%% Two indexes, two item widths, one builder. The offset index maps a data
%%% item's ID to its byte range in the weave as 21-byte items; the match index
%%% maps a `key=value' predicate to the weave offsets of matching items as
%%% 17-byte items. Both are packed big-endian so that `memcmp' order is
%%% numeric order on the leading field, which is what lets sorted runs of them
%%% concatenate into the published `MDB_DUPFIXED' containers by appends alone.
%%%
%%% ```
%%% offset item (21 bytes):
%%%     bit 167..88  id      80  first 10 bytes of hb_util:native_id(ID)
%%%     bit  87..84  type     4  0=tx@1.0 1=ans102 2=ans104@1.0 3=httpsig@1.0
%%%     bit  83..34  offset  50  absolute weave offset of the first byte
%%%     bit  33..0   length  34  byte length of the item
%%%
%%% match item (17 bytes):
%%%     bit 135..56  hash    80  first 10 bytes of SHA-256(predicate)
%%%     bit  55..7   offset  49  absolute weave offset of the item
%%%     bit   6..0   zero     7
%%%
%%% predicate = <<"~match@1.0/", LowerCaseKey/binary, "=", Value/binary>>
%%% '''
%%%
%%% One match row per indexed predicate per item: each tag (lower-cased name,
%%% raw un-normalised value) except `bundle-format' and `bundle-version',
%%% which the ans104 codec consumes; the committer's address under
%%% `committer'; the target address under `field-target' when the item
%%% carries one; the enclosing ans104 item's ID under `parent' for nested
%%% items; and the commitment device under `commitment-device', always
%%% `ans104@1.0'. Items whose offset or length overflows its field get no
%%% row of the affected kind. Policy exclusion -- RedStone oracle items --
%%% is the scan's concern, applied by weave-offset interval
%%% (`lib_arweave_index_exclude') before any row is built.
-module(lib_arweave_index_rows).
-export([offset_item/4, match_item/2, predicate/2, match_rows/2, redstone/1]).
-export([decode_offset_item/1, decode_match_item/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The field widths of the two items, in bits.
-define(ID_PREFIX_SIZE, 80).
-define(TYPE_SIZE, 4).
-define(OFFSET_SIZE, 50).
-define(LENGTH_SIZE, 34).
-define(MATCH_OFFSET_SIZE, 49).
-define(MATCH_PAD_SIZE, 7).

%%% The tag names whose joint presence marks a RedStone oracle item. The
%%% detector validates the interval-based exclusion in tests and audits;
%%% the row paths never consult it.
-define(REDSTONE_TAGS,
    [
        <<"dataFeedId">>,
        <<"dataServiceId">>,
        <<"signerAddress">>,
        <<"timestamp">>,
        <<"type">>
    ]
).

%%% The tag names the ans104 codec consumes: they describe the transport
%%% encoding, not the item, and get no match rows.
-define(CODEC_TAGS, [<<"bundle-format">>, <<"bundle-version">>]).

%% @doc Encode one offset-index item, or `excluded' for a row whose offset or
%% length does not fit its field. The ID may be given in native or human form.
offset_item(ID, Type, Offset, Length)
        when Offset >= 0 andalso Offset < (1 bsl ?OFFSET_SIZE)
        andalso Length >= 0 andalso Length < (1 bsl ?LENGTH_SIZE) ->
    << Prefix:?ID_PREFIX_SIZE, _/bitstring >> = hb_util:native_id(ID),
    <<
        Prefix:?ID_PREFIX_SIZE,
        (type_nibble(Type)):?TYPE_SIZE,
        Offset:?OFFSET_SIZE,
        Length:?LENGTH_SIZE
    >>;
offset_item(_ID, _Type, _Offset, _Length) ->
    excluded.

%% @doc Encode one match-index item for a predicate binary, or `excluded' for
%% an offset that does not fit its field.
match_item(Predicate, Offset)
        when Offset >= 0 andalso Offset < (1 bsl ?MATCH_OFFSET_SIZE) ->
    << Hash:?ID_PREFIX_SIZE, _/bitstring >> = crypto:hash(sha256, Predicate),
    << Hash:?ID_PREFIX_SIZE, Offset:?MATCH_OFFSET_SIZE, 0:?MATCH_PAD_SIZE >>;
match_item(_Predicate, _Offset) ->
    excluded.

%% @doc The predicate string one key and value are matched under. The key is
%% lower-cased; the value is not normalised in any way.
predicate(Key, Value) ->
    <<"~match@1.0/", (hb_util:to_lower(Key))/binary, "=", Value/binary>>.

%% @doc Every match-index item of one data item, given its parsed header and
%% its absolute weave offset. The header is a map of the fields the scan
%% recovers: `tags' as `[{Name, Value}]' in item order, `committer' and
%% optionally `field-target' and `parent' as human-readable IDs. Codec-
%% consumed tags and predicates whose row cannot be encoded are dropped.
match_rows(Header, Offset) ->
    Tags = maps:get(<<"tags">>, Header, []),
    Fields =
        [
            {<<"commitment-device">>, <<"ans104@1.0">>},
            {<<"committer">>, maps:get(<<"committer">>, Header)}
        |
            [
                {Key, maps:get(Key, Header)}
            ||
                Key <- [<<"field-target">>, <<"parent">>],
                maps:is_key(Key, Header)
            ]
        ],
    [
        Item
    ||
        {Key, Value} <- Tags ++ Fields,
        (LowerKey = lower(Key)) /= invalid,
        not lists:member(LowerKey, ?CODEC_TAGS),
        (Item =
            match_item(
                <<"~match@1.0/", LowerKey/binary, "=", Value/binary>>,
                Offset
            )) /= excluded
    ].

%% @doc Whether a tag list carries the RedStone oracle signature: all five of
%% the marker names present, byte-exact. A validator for the exclusion
%% intervals, not a row-path check.
redstone(Tags) ->
    Names = [Name || {Name, _Value} <- Tags],
    lists:all(fun(Marker) -> lists:member(Marker, Names) end, ?REDSTONE_TAGS).

%% @doc Decode one offset-index item, for audits and tests.
decode_offset_item(
        <<
            Prefix:?ID_PREFIX_SIZE,
            Type:?TYPE_SIZE,
            Offset:?OFFSET_SIZE,
            Length:?LENGTH_SIZE
        >>
) ->
    #{
        <<"id-prefix">> => << Prefix:?ID_PREFIX_SIZE >>,
        <<"type">> => type_name(Type),
        <<"offset">> => Offset,
        <<"length">> => Length
    }.

%% @doc Decode one match-index item, for audits and tests.
decode_match_item(
        << Hash:?ID_PREFIX_SIZE, Offset:?MATCH_OFFSET_SIZE, 0:?MATCH_PAD_SIZE >>
) ->
    #{
        <<"hash-prefix">> => << Hash:?ID_PREFIX_SIZE >>,
        <<"offset">> => Offset
    }.

%%% Internal functions.

%% @doc Lower-case a predicate key, or `invalid' for bytes that are not a
%% UTF-8 string. On-chain tag names are arbitrary bytes; a name the AO-Core
%% tag path would reject gets no predicate rather than a crashed scan.
lower(Key) ->
    try hb_util:to_lower(Key)
    catch error:badarg -> invalid
    end.

%% @doc The nibble one codec name is stored as. Codec indexes are sorted by
%% the time of the format's first write to Arweave.
type_nibble(<<"tx@1.0">>) -> 0;
type_nibble(<<"ans102@1.0">>) -> 1;
type_nibble(<<"ans104@1.0">>) -> 2;
type_nibble(<<"httpsig@1.0">>) -> 3.

%% @doc The codec name one nibble stores.
type_name(0) -> <<"tx@1.0">>;
type_name(1) -> <<"ans102@1.0">>;
type_name(2) -> <<"ans104@1.0">>;
type_name(3) -> <<"httpsig@1.0">>.

%%% Tests.

%% @doc An offset item packs each field where the specification places it.
%% Expectations are built with independent arithmetic on an ID hashed here,
%% not read back through the encoder.
offset_item_layout_test() ->
    ID = crypto:hash(sha256, <<"an item signature">>),
    Offset = 378123456789012,
    Length = 1234567,
    Item = offset_item(ID, <<"ans104@1.0">>, Offset, Length),
    ?assertEqual(21, byte_size(Item)),
    % The leading 10 bytes are the ID's own first 10 bytes, so item order is
    % ID order.
    ?assertEqual(binary:part(ID, 0, 10), binary:part(Item, 0, 10)),
    % The trailing 88 bits are type, offset and length packed big-endian:
    % byte-wise, the value is Type * 2^84 + Offset * 2^34 + Length.
    << _:10/binary, Tail:88 >> = Item,
    ?assertEqual(
        2 * (1 bsl 84) + Offset * (1 bsl 34) + Length,
        Tail
    ),
    ?assertEqual(
        #{
            <<"id-prefix">> => binary:part(ID, 0, 10),
            <<"type">> => <<"ans104@1.0">>,
            <<"offset">> => Offset,
            <<"length">> => Length
        },
        decode_offset_item(Item)
    ).

%% @doc A match item's hash prefix is the independent SHA-256 of the exact
%% predicate string, and the offset sits left of seven zero bits.
match_item_layout_test() ->
    Predicate = <<"~match@1.0/content-type=text/html">>,
    Offset = 378123456789012,
    Item = match_item(Predicate, Offset),
    ?assertEqual(17, byte_size(Item)),
    ?assertEqual(
        binary:part(crypto:hash(sha256, Predicate), 0, 10),
        binary:part(Item, 0, 10)
    ),
    << _:10/binary, Tail:56 >> = Item,
    ?assertEqual(Offset * (1 bsl 7), Tail).

%% @doc Predicates lower-case the key and leave the value untouched; the
%% full row set of an item covers tags, committer, field-target, parent and
%% commitment-device, with the codec-consumed bundle tags dropped.
match_rows_test() ->
    ?assertEqual(
        <<"~match@1.0/content-type=Text/HTML">>,
        predicate(<<"Content-Type">>, <<"Text/HTML">>)
    ),
    Offset = 1024,
    Header =
        #{
            <<"tags">> =>
                [
                    {<<"App-Name">>, <<"Test">>},
                    {<<"Bundle-Format">>, <<"binary">>},
                    {<<"Bundle-Version">>, <<"2.0.0">>}
                ],
            <<"committer">> => <<"committerAddr">>,
            <<"field-target">> => <<"targetAddr">>,
            <<"parent">> => <<"parentID">>
        },
    Rows = match_rows(Header, Offset),
    Expected =
        [
            match_item(<<"~match@1.0/app-name=Test">>, Offset),
            match_item(<<"~match@1.0/commitment-device=ans104@1.0">>, Offset),
            match_item(<<"~match@1.0/committer=committerAddr">>, Offset),
            match_item(<<"~match@1.0/field-target=targetAddr">>, Offset),
            match_item(<<"~match@1.0/parent=parentID">>, Offset)
        ],
    ?assertEqual(lists:sort(Expected), lists:sort(Rows)).

%% @doc Rows that overflow a field are excluded rather than truncated.
exclusion_test() ->
    ID = crypto:hash(sha256, <<"sig">>),
    ?assertEqual(excluded, offset_item(ID, <<"ans104@1.0">>, 1 bsl 50, 1)),
    ?assertEqual(excluded, offset_item(ID, <<"ans104@1.0">>, 1, 1 bsl 34)),
    ?assertEqual(excluded, match_item(<<"~match@1.0/a=b">>, 1 bsl 49)),
    ?assertMatch(<<_:21/binary>>,
        offset_item(ID, <<"ans104@1.0">>, (1 bsl 50) - 1, (1 bsl 34) - 1)).

%% @doc The RedStone signature is all five marker tags, exactly spelled;
%% four of five, or a case variant, is not RedStone.
redstone_test() ->
    Marked =
        [
            {<<"dataFeedId">>, <<"BTC">>},
            {<<"dataServiceId">>, <<"redstone-primary-prod">>},
            {<<"signerAddress">>, <<"0x0">>},
            {<<"timestamp">>, <<"1700000000">>},
            {<<"type">>, <<"data-package">>},
            {<<"app">>, <<"anything else">>}
        ],
    ?assert(redstone(Marked)),
    ?assertNot(redstone(tl(Marked))),
    ?assertNot(redstone([{<<"DataFeedId">>, <<"BTC">>} | tl(Marked)])),
    ?assertNot(redstone([])).
