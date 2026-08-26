%%% @doc One ANS-104 data item's published-index rows, from its header
%%% window: the scan's per-item hot loop, computed natively.
%%%
%%% `rows/4' takes what the scanner read for one item -- a window of its
%%% bytes, its absolute weave offset and full size, and the enclosing
%%% bundle's ID (empty for a top-level item) -- and returns the finished
%%% artifacts: the item's 21-byte offset-index item (or `excluded'), its
%%% 17-byte match-index items, and, when its tags name it a bundle, the
%%% header size and recomputed ID the scan recurses with. RedStone items
%%% come back as the atom `redstone' before anything is hashed; a window
%%% the header does not parse in is `failed', for the caller to regrow or
%%% count malformed.
%%%
%%% The NIF performs the whole computation -- the
%%% `ar_bundles:deserialize_header/1' walk, the RedStone check, the
%%% sha256 of the signature and predicates, the owner address, the row
%%% encoding of `lib_arweave_index_rows' -- in one native pass. Its byte
%%% semantics mirror `reference/4', the pure Erlang computation kept both
%%% as the parity oracle for the tests and as the live fallback: input the
%%% native code cannot reproduce byte-exactly (tag names beyond ASCII,
%%% bundle-tag values beyond ASCII, varints in bignum territory, base58
%%% addresses of unusual width) runs through the reference instead, one
%%% item at a time. Windows above the first ladder rung may carry tag
%%% sections whose walk and hashing exceed a normal scheduler's budget,
%%% so they run on a dirty CPU scheduler.
-module(lib_arweave_index_item).
-export([rows/4, reference/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-on_load(init/0).

%%% The window size above which the NIF runs on a dirty CPU scheduler: a
%%% larger window can hold a tag section whose decode and per-tag hashing
%%% run past the millisecond a normal scheduler tolerates.
-define(DIRTY_WINDOW, 4096).

%% @doc Load the NIF backing `rows/4'.
init() ->
    SoName = filename:join([code:priv_dir(hb), "lib_arweave_index_item"]),
    erlang:load_nif(SoName, 0).

%% @doc One item's rows: `redstone', `failed',
%% `{ok, OffsetItem | excluded, MatchItems}', or
%% `{bundle, OffsetItem | excluded, MatchItems, HeaderSize, ID}' for an
%% item to recurse into. `Parent' is the enclosing ans104 item's
%% human-readable ID, or empty at the top level.
rows(Window, Offset, Size, Parent) ->
    Result =
        case byte_size(Window) > ?DIRTY_WINDOW of
            true -> rows_dirty_nif(Window, Offset, Size, Parent);
            false -> rows_nif(Window, Offset, Size, Parent)
        end,
    case Result of
        fallback -> reference(Window, Offset, Size, Parent);
        Rows -> Rows
    end.

%% @doc The pure Erlang computation of `rows/4': the same parse the AO-Core
%% write path applies, the same row builders the specification is tested
%% against. The NIF must agree with this function byte-for-byte on every
%% input it does not defer.
reference(Window, Offset, Size, Parent) ->
    case header(Window) of
        {ok, HeaderSize, TX} -> emit(TX, HeaderSize, Offset, Size, Parent);
        failed -> failed
    end.

%%% Internal functions.

rows_nif(_Window, _Offset, _Size, _Parent) ->
    erlang:nif_error(not_loaded).

rows_dirty_nif(_Window, _Offset, _Size, _Parent) ->
    erlang:nif_error(not_loaded).

%% @doc Deserialize an item header through `ar_bundles', reporting any parse
%% failure -- a window ending mid-field, an unsupported signature type, a tag
%% section that does not decode -- as `failed' for the caller to size up or
%% count. The try is confined to the vendored parser, whose interface for
%% foreign bytes is to throw.
header(Bin) ->
    try ar_bundles:deserialize_header(Bin)
    catch
        throw:{invalid_ans104_tags, _} -> failed;
        error:_ -> failed
    end.

%% @doc Build one parsed item's rows. RedStone items produce no rows and
%% are never bundles.
emit(TX, HeaderSize, Pos, Size, Parent) ->
    #tx{ signature = Signature, tags = Tags } = TX,
    case lib_arweave_index_rows:redstone(Tags) of
        true ->
            redstone;
        false ->
            ID = crypto:hash(sha256, Signature),
            OffsetItem =
                lib_arweave_index_rows:offset_item(
                    ID, <<"ans104@1.0">>, Pos, Size),
            MatchItems =
                lib_arweave_index_rows:match_rows(header_map(TX, Parent), Pos),
            case bundle_tagged(TX) of
                true ->
                    {bundle, OffsetItem, MatchItems, HeaderSize,
                        hb_util:human_id(ID)};
                false ->
                    {ok, OffsetItem, MatchItems}
            end
    end.

%% @doc The parsed fields the row builder draws predicates from.
header_map(TX, Parent) ->
    #tx{
        owner = Owner,
        signature_type = SigType,
        target = Target,
        tags = Tags
    } = TX,
    Base =
        #{
            <<"tags">> => Tags,
            <<"committer">> =>
                hb_util:human_id(ar_wallet:to_address(Owner, SigType))
        },
    WithTarget =
        case Target of
            <<>> -> Base;
            _ -> Base#{ <<"field-target">> => hb_util:human_id(Target) }
        end,
    case Parent of
        <<>> -> WithTarget;
        _ -> WithTarget#{ <<"parent">> => Parent }
    end.

%% @doc Whether the item's tags name it a bundle (`bundle-format: binary',
%% `bundle-version: 2.0.0', names case-insensitive, values lower-cased), as
%% `ar_tx:type/1' reads them.
bundle_tagged(TX) ->
    Format = ar_tx:tagfind(<<"bundle-format">>, TX#tx.tags, <<>>),
    Version = ar_tx:tagfind(<<"bundle-version">>, TX#tx.tags, <<>>),
    {hb_util:to_lower(Format), hb_util:to_lower(Version)}
        == {<<"binary">>, <<"2.0.0">>}.

%%% Tests.

%% @doc The native path and the Erlang reference agree, term for term and
%% byte for byte, across fabricated items bracketing the parser's envelope:
%% every signature type, RedStone and near-RedStone tag sets, bundle tags in
%% case variants, Unicode tags (deferred to the reference), empty and long
%% values, and windows and sizes that overflow each encoded field.
parity_test() ->
    rand:seed(exsss, {101, 102, 103}),
    Offsets =
        [0, 4096, 381948870323729,
            (1 bsl 49) - 1, 1 bsl 49, (1 bsl 50) - 1, 1 bsl 50],
    Parents = [<<>>, hb_util:human_id(crypto:hash(sha256, <<"parent">>))],
    Items =
        [fabricated(Kind) || Kind <- kinds()]
            ++ [fabricated(mixed_kind()) || _ <- lists:seq(1, 64)],
    [
        assert_parity(Item, Offset, Parent)
    ||
        Item <- Items,
        Offset <- Offsets,
        Parent <- Parents
    ],
    ok.

%% @doc Truncated windows fail identically on both paths, at every prefix
%% of a valid item.
truncation_parity_test() ->
    rand:seed(exsss, {7, 8, 9}),
    Item = fabricated(#{}),
    Size = byte_size(Item) + 1024,
    [
        assert_windowed_parity(binary:part(Item, 0, Take), 4096, Size, <<>>)
    ||
        Take <- [0, 1, 2, 700, 1030, 1060, byte_size(Item) - 1]
    ],
    ok.

%% @doc Hand-built Avro block form (negative count, size-prefixed) parses
%% identically: the encoder never writes it, but the chain can carry it.
avro_block_form_parity_test() ->
    rand:seed(exsss, {21, 22, 23}),
    % Two tags in one block: count -2 (zigzag 3), block size 8 (zigzag 16),
    % then the terminator.
    Section =
        << 3, 16, 2, $A, 2, $b, 2, $C, 2, $d, 0 >>,
    Item = enveloped(<< 2, 0 >>, << 0 >>, 2, Section, <<"data">>),
    assert_windowed_parity(Item, 4096, byte_size(Item), <<>>),
    {ok, _Offset, Rows} =
        rows(Item, 4096, byte_size(Item), <<>>),
    % Two tag rows, the commitment-device row and the committer row.
    ?assertEqual(4, length(Rows)).

%% @doc A varint reaching Erlang-bignum territory defers to the reference
%% rather than wrapping in native arithmetic.
bignum_varint_parity_test() ->
    rand:seed(exsss, {31, 32, 33}),
    Section = << 2, (binary:copy(<< 255 >>, 10))/binary, 1, 0 >>,
    Item = enveloped(<< 2, 0 >>, << 0 >>, 1, Section, <<>>),
    assert_windowed_parity(Item, 4096, byte_size(Item), <<>>).

%% @doc The native ethereum owner path reproduces the known checksummed
%% address vector, end to end into the committer predicate row.
ethereum_committer_row_test() ->
    rand:seed(exsss, {41, 42, 43}),
    Key =
        hb_util:decode(
            <<"BAoixXds4JhW42pzlLb83B3-I21lX78j3Q7cPaoFiCjMgjYwYLDj-xL1"
                "32J147ifZFwRBmzmEMC8eYAXzbRNWuA">>),
    ?assertEqual(65, byte_size(Key)),
    Item =
        fabricated(
            #{
                <<"sig-type">> => << 3, 0 >>,
                <<"owner">> => Key,
                <<"tags">> => []
            }
        ),
    {ok, _OffsetItem, [_DeviceRow, CommitterRow]} =
        rows(Item, 4096, byte_size(Item), <<>>),
    ?assertEqual(
        lib_arweave_index_rows:match_item(
            <<"~match@1.0/committer=",
                "0xb7B4360F7F6298dE2e7a11009270F35F189Bd77E">>,
            4096
        ),
        CommitterRow
    ).

%% @doc One item's native and reference results, over the full item as the
%% window, with the size extended past the window as data items commonly
%% are.
assert_parity(Item, Offset, Parent) ->
    assert_windowed_parity(Item, Offset, byte_size(Item), Parent),
    assert_windowed_parity(Item, Offset, byte_size(Item) + 100000, Parent).

assert_windowed_parity(Window, Offset, Size, Parent) ->
    ?assertEqual(
        reference(Window, Offset, Size, Parent),
        rows(Window, Offset, Size, Parent)
    ).

%% @doc The envelope shapes the parity sweep covers.
kinds() ->
    [
        #{},
        #{ <<"sig-type">> => << 2, 0 >> },
        #{ <<"sig-type">> => << 3, 0 >> },
        #{ <<"sig-type">> => << 4, 0 >> },
        #{ <<"sig-type">> => << 7, 0 >> },
        #{ <<"sig-type">> => << 5, 0 >> },
        #{ <<"tags">> => [] },
        #{ <<"tags">> => redstone_tags() },
        #{ <<"sig-type">> => << 3, 0 >>, <<"tags">> => redstone_tags() },
        #{ <<"tags">> => tl(redstone_tags()) },
        #{ <<"tags">> =>
            [{<<"DataFeedId">>, <<"BTC">>} | tl(redstone_tags())] },
        #{ <<"tags">> => bundle_tags(<<"binary">>, <<"2.0.0">>) },
        #{ <<"tags">> => bundle_tags(<<"BINARY">>, <<"2.0.0">>) },
        #{ <<"tags">> => bundle_tags(<<"binary">>, <<"1.0.0">>) },
        #{ <<"tags">> => [{<<"Bundle-Format">>, <<"binary">>}] },
        #{ <<"tags">> => [{<<"Größe"/utf8>>, <<"x">>}] },
        #{ <<"tags">> => [{<<"name">>, <<"Größe"/utf8>>}] },
        #{ <<"tags">> =>
            bundle_tags(<<"BINARY"/utf8>>, <<"Straße"/utf8>>) },
        #{ <<"tags">> => [{<<"empty">>, <<>>}] },
        #{ <<"tags">> =>
            [{<<"long">>, binary:copy(<<"v">>, 5000)}] },
        #{ <<"tags">> =>
            [{<<"t", (integer_to_binary(N))/binary>>, <<"v">>}
                || N <- lists:seq(1, 100)] },
        #{ <<"target">> => rand:bytes(32) },
        #{ <<"target">> => rand:bytes(32),
            <<"tags">> => bundle_tags(<<"binary">>, <<"2.0.0">>) }
    ].

%% @doc One random envelope: signature type, tag mix and target drawn from
%% the shapes above.
mixed_kind() ->
    SigTypes =
        [<< 1, 0 >>, << 2, 0 >>, << 3, 0 >>, << 4, 0 >>, << 7, 0 >>],
    Tags =
        [
            {rand:bytes(1 + rand:uniform(16)), rand:bytes(rand:uniform(64))}
        ||
            _ <- lists:seq(1, rand:uniform(12))
        ],
    Ascii =
        [
            {hb_util:encode(Name), Value}
        ||
            {Name, Value} <- Tags
        ],
    Base =
        #{
            <<"sig-type">> => lists:nth(rand:uniform(5), SigTypes),
            <<"tags">> => Ascii
        },
    case rand:uniform(3) of
        1 -> Base#{ <<"target">> => rand:bytes(32) };
        _ -> Base
    end.

redstone_tags() ->
    [
        {<<"dataFeedId">>, <<"ETH">>},
        {<<"dataServiceId">>, <<"redstone-primary-prod">>},
        {<<"signerAddress">>, <<"0x926E370Fd53c23f8B71ad2B3217b227E41A92b12">>},
        {<<"timestamp">>, <<"1700000000">>},
        {<<"type">>, <<"redstone-oracles">>}
    ].

bundle_tags(Format, Version) ->
    [
        {<<"Bundle-Format">>, Format},
        {<<"Bundle-Version">>, Version},
        {<<"App-Name">>, <<"parity">>}
    ].

%% @doc A structurally valid item of a spec: real layout, random signature
%% and owner unless given.
fabricated(Spec) ->
    SigType = maps:get(<<"sig-type">>, Spec, << 1, 0 >>),
    {SigSize, OwnerSize} = envelope_sizes(SigType),
    Owner = maps:get(<<"owner">>, Spec, rand:bytes(OwnerSize)),
    Target =
        case maps:get(<<"target">>, Spec, <<>>) of
            <<>> -> << 0 >>;
            Recipient -> << 1, Recipient/binary >>
        end,
    Tags = maps:get(<<"tags">>, Spec, default_tags()),
    Encoded = ar_bundles:encode_tags(Tags),
    iolist_to_binary(
        [
            SigType,
            rand:bytes(SigSize),
            Owner,
            Target,
            << 0 >>,
            << (length(Tags)):64/little, (byte_size(Encoded)):64/little >>,
            Encoded,
            rand:bytes(maps:get(<<"data">>, Spec, 64))
        ]
    ).

%% @doc An item around a hand-built tag section, for byte shapes the
%% encoder never writes.
enveloped(SigType, Target, TagCount, Section, Data) ->
    {SigSize, OwnerSize} = envelope_sizes(SigType),
    iolist_to_binary(
        [
            SigType,
            rand:bytes(SigSize),
            rand:bytes(OwnerSize),
            Target,
            << 0 >>,
            << TagCount:64/little, (byte_size(Section)):64/little >>,
            Section,
            Data
        ]
    ).

%% @doc The signature and owner widths of each envelope, including one the
%% parser refuses.
envelope_sizes(<< 1, 0 >>) -> {512, 512};
envelope_sizes(<< 2, 0 >>) -> {64, 32};
envelope_sizes(<< 3, 0 >>) -> {65, 65};
envelope_sizes(<< 4, 0 >>) -> {64, 32};
envelope_sizes(<< 7, 0 >>) -> {65, 42};
envelope_sizes(<< 5, 0 >>) -> {64, 32}.

default_tags() ->
    [
        {<<"Content-Type">>, <<"application/octet-stream">>},
        {<<"App-Name">>, <<"Parity-Sweep">>}
    ].
