%%% @doc A codec over declaratively packed bits.
%%%
%%% A format describes a packed binary as a sequence of bit fields, or as a
%%% homogeneous sequence of records of such fields:
%%% ```
%%%     format = fields | "repeat(" fields ")"
%%%     fields = field ("," field)*
%%%     field  = name ":" size ("+" type)?
%%%     type   = "integer" | "float" | "atom" | enum(atom ("," atom)*)
%%%     name   = any bytes except `:' and `,'; `_' alone is padding
%%%     size   = a positive decimal bit count
%%% '''
%%% For example, `_:77,start:49+integer,length:34+integer' names the two
%%% trailing fields of a 160-bit row and discards its leading 77 bits, and
%%% `repeat(start:64+integer,end:64+integer)' reads a body as consecutive
%%% 128-bit records, decoding to the list of their messages in order. A
%%% repeated body's bit size must be a whole multiple of the record size --
%%% the empty body is the empty list -- and `repeat' does not nest.
%%%
%%% Fields pack contiguously in the order written, with no alignment or
%%% padding between them, and their sizes must sum to exactly the decoded
%%% body's bit size. A field named `_' is anonymous padding: its bits are
%%% consumed and omitted from the result (any other name, including ones
%%% merely beginning with `_', is an ordinary field, and a repeated name
%%% keeps the last field's value). `integer' fields decode as unsigned
%%% big-endian integers of their bit size; `float' fields as IEEE floats of
%%% 16, 32 or 64 bits; `atom' fields as the atom whose UTF-8 name fills the
%%% field, at byte-multiple sizes; `enum' fields as the member named by the
%%% field's unsigned integer value, zero-indexed, returned as a binary;
%%% untyped fields decode as bitstrings, byte-aligned only when their size
%%% is a multiple of eight. The empty format, an empty field name, a
%%% non-positive or non-decimal size, an unknown type, a float or atom size
%%% outside those above, an enum value past its last member, and a body
%%% whose bit size does not match the format are refused.
%%%
%%% Keys:
%%% ```
%%%     from=Format:  Decode the base message's `body' per `Format' into a
%%%                   message of the named fields.
%%%     take=N:       Return the leading `N' bits of the base message's
%%%                   `body' as a bitstring.
%%% '''
-module(dev_bits).
-export([from/3, take/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Decode the base message's body into a message of the fields named by
%% the format given as the key's argument, or into a list of such messages
%% for a `repeat(...)' format.
-spec from(
    #{ body => bitstring() },
    #{ from => binary() },
    #{ _ => _ }
) -> {ok, #{ _ => _ } | [#{ _ => _ }]} | {error, _}.
from(Base, Req, Opts) ->
    maybe
        {ok, Body} ?= find_body(Base, Opts),
        {ok, Parsed} ?= parse_format(hb_maps:get(<<"from">>, Req, <<>>, Opts)),
        decode(Parsed, Body)
    end.

%% @doc Decode a body as the single record, or the record sequence, that its
%% parsed format describes. The body's bit size must equal the record size,
%% or a whole multiple of it under `repeat'.
decode({repeat, Fields}, Body) ->
    Record = record_size(Fields),
    maybe
        true ?=
            bit_size(Body) rem Record =:= 0
                orelse {error, {'invalid-body-size', bit_size(Body), Record}},
        decode_records(Fields, Record, Body, [])
    end;
decode(Fields, Body) ->
    Total = record_size(Fields),
    maybe
        true ?=
            Total =:= bit_size(Body)
                orelse {error, {'invalid-body-size', bit_size(Body), Total}},
        decode_fields(Fields, Body, #{})
    end.

%% @doc The bit size of one record of the given fields.
record_size(Fields) ->
    lists:sum([ Size || {_Name, Size, _Type} <- Fields ]).

%% @doc Decode each record of a repeated body in order.
decode_records(_Fields, _Record, <<>>, Records) ->
    {ok, lists:reverse(Records)};
decode_records(Fields, Record, Body, Records) ->
    <<Head:Record/bitstring, Rest/bitstring>> = Body,
    maybe
        {ok, Decoded} ?= decode_fields(Fields, Head, #{}),
        decode_records(Fields, Record, Rest, [Decoded | Records])
    end.

%% @doc Return the leading N bits of the base message's body.
-spec take(#{ body => bitstring() }, #{ take => pos_integer() }, #{ _ => _ }) ->
    {ok, bitstring()} | {error, _}.
take(Base, Req, Opts) ->
    maybe
        {ok, Body} ?= find_body(Base, Opts),
        {ok, Bits} ?= parse_size(hb_maps:get(<<"take">>, Req, <<>>, Opts)),
        true ?=
            Bits =< bit_size(Body)
                orelse {error, {'invalid-body-size', bit_size(Body), Bits}},
        <<Taken:Bits/bitstring, _/bitstring>> = Body,
        {ok, Taken}
    end.

%% @doc Find the body of the base message, refusing messages without one.
find_body(Base, Opts) ->
    case hb_maps:find(<<"body">>, Base, Opts) of
        {ok, Body} when is_bitstring(Body) -> {ok, Body};
        _ -> {error, {'invalid-body', <<"No binary `body' key found.">>}}
    end.

%% @doc Parse a comma-separated `name:size[+type]' format.
parse_format(<<>>) -> {error, {'invalid-format', <<>>}};
parse_format(<<"repeat(", Inner/binary>>) ->
    maybe
        {ok, Fields} ?=
            case binary:split(Inner, <<")">>) of
                [Format, <<>>] -> parse_format(Format);
                _ -> {error, {'invalid-format', <<"repeat(", Inner/binary>>}}
            end,
        {ok, {repeat, Fields}}
    end;
parse_format(Format) ->
    % The depth-aware split keeps an enum's comma-separated members inside
    % their field.
    parse_fields(hb_util:split_depth_string_aware($,, Format), []).
parse_fields([], Fields) -> {ok, lists:reverse(Fields)};
parse_fields([Spec | Rest], Fields) ->
    maybe
        {ok, Field} ?= parse_field(Spec),
        parse_fields(Rest, [Field | Fields])
    end.

%% @doc Parse a single field: a name, a bit size, and an optional value type.
parse_field(Spec) ->
    case binary:split(Spec, <<":">>) of
        [Name, SizeType] when Name =/= <<>> ->
            case binary:split(SizeType, <<"+">>) of
                [Size] -> sized_field(Name, Size, bitstring);
                [Size, <<"integer">>] -> sized_field(Name, Size, integer);
                [Size, <<"float">>] -> float_field(Name, Size);
                [Size, <<"atom">>] -> atom_field(Name, Size);
                [Size, <<"enum(", Members/binary>>] ->
                    enum_field(Name, Size, Members);
                [_Size, Type] -> {error, {'invalid-format-type', Type}}
            end;
        _ ->
            {error, {'invalid-format-field', Spec}}
    end.

%% @doc Attach a parsed bit size to a field.
sized_field(Name, Size, Type) ->
    maybe
        {ok, Bits} ?= parse_size(Size),
        {ok, {Name, Bits, Type}}
    end.

%% @doc A float field, at the IEEE sizes alone.
float_field(Name, Size) ->
    maybe
        {ok, Field = {_, Bits, _}} ?= sized_field(Name, Size, float),
        true ?=
            lists:member(Bits, [16, 32, 64])
                orelse {error, {'invalid-float-size', Bits}},
        {ok, Field}
    end.

%% @doc An atom field: its UTF-8 name fills the field, so the size must be a
%% whole number of bytes.
atom_field(Name, Size) ->
    maybe
        {ok, Field = {_, Bits, _}} ?= sized_field(Name, Size, atom),
        true ?=
            Bits rem 8 =:= 0
                orelse {error, {'invalid-atom-size', Bits}},
        {ok, Field}
    end.

%% @doc An enum field: the members arrive as the comma-separated remainder of
%% the type, up to its closing parenthesis.
enum_field(Name, Size, Members) ->
    MembersSize = byte_size(Members) - 1,
    case Members of
        <<Names:MembersSize/binary, ")">> when Names =/= <<>> ->
            sized_field(
                Name,
                Size,
                {enum, binary:split(Names, <<",">>, [global])}
            );
        _ ->
            {error, {'invalid-format-type', <<"enum(", Members/binary>>}}
    end.

%% @doc Parse a positive bit count, refusing malformed values.
parse_size(Value) ->
    try hb_util:int(Value) of
        Bits when Bits > 0 -> {ok, Bits};
        Bits -> {error, {'invalid-size', Bits}}
    catch _:_ -> {error, {'invalid-size', Value}}
    end.

%% @doc Decode the fields from the body in order, skipping `_' padding.
decode_fields([], <<>>, Msg) -> {ok, Msg};
decode_fields([{Name, Size, Type} | Rest], Body, Msg) ->
    <<Value:Size/bitstring, Remaining/bitstring>> = Body,
    case Name of
        <<"_">> -> decode_fields(Rest, Remaining, Msg);
        _ ->
            maybe
                {ok, Typed} ?= typed(Type, Value),
                decode_fields(Rest, Remaining, Msg#{ Name => Typed })
            end
    end.

%% @doc Convert a decoded bitstring by field type. Integers are unsigned and
%% big-endian; an enum value past its last member is refused.
typed(bitstring, Bits) -> {ok, Bits};
typed(integer, Bits) ->
    Size = bit_size(Bits),
    <<Int:Size/integer>> = Bits,
    {ok, Int};
typed(float, Bits) ->
    Size = bit_size(Bits),
    <<Float:Size/float>> = Bits,
    {ok, Float};
typed(atom, Bits) -> {ok, hb_util:atom(Bits)};
typed({enum, Members}, Bits) ->
    {ok, Index} = typed(integer, Bits),
    maybe
        true ?=
            Index < length(Members)
                orelse {error, {'invalid-enum-value', Index, Members}},
        {ok, lists:nth(Index + 1, Members)}
    end.

%%% Tests

%% @doc Decode a packed body, skipping padding and typing integers.
from_test() ->
    {ok, Decoded} =
        hb_ao:resolve(
            #{
                <<"path">> => <<"~bits@1.0/from=_:3,count:13+integer,tail:16">>,
                <<"body">> => <<2#101:3, 999:13, "ok">>
            },
            #{}
        ),
    ?assertEqual(999, hb_maps:get(<<"count">>, Decoded)),
    ?assertEqual(<<"ok">>, hb_maps:get(<<"tail">>, Decoded)),
    ?assertEqual(error, hb_maps:find(<<"_">>, Decoded)).

%% @doc Untyped fields decode as bitstrings on non-byte boundaries.
from_bitstring_field_test() ->
    {ok, Decoded} =
        hb_ao:resolve(
            #{
                <<"path">> => <<"~bits@1.0/from=lead:5,rest:11+integer">>,
                <<"body">> => <<2#10110:5, 1234:11>>
            },
            #{}
        ),
    ?assertEqual(<<2#10110:5>>, hb_maps:get(<<"lead">>, Decoded)),
    ?assertEqual(1234, hb_maps:get(<<"rest">>, Decoded)).

%% @doc Bodies that do not match their format's size are refused.
from_size_mismatch_test() ->
    ?assertMatch(
        {error, _},
        hb_ao:resolve(
            #{
                <<"path">> => <<"~bits@1.0/from=value:16+integer">>,
                <<"body">> => <<1>>
            },
            #{}
        )
    ).

%% @doc Malformed formats are refused.
from_malformed_format_test() ->
    ?assertMatch(
        {error, _},
        hb_ao:resolve(
            #{
                <<"path">> => <<"~bits@1.0/from=value:banana">>,
                <<"body">> => <<1>>
            },
            #{}
        )
    ),
    ?assertMatch(
        {error, _},
        hb_ao:resolve(
            #{
                <<"path">> => <<"~bits@1.0/from=value:8+float">>,
                <<"body">> => <<1>>
            },
            #{}
        )
    ).

%% @doc Take leading bits across byte boundaries, refusing over-long takes.
take_test() ->
    {ok, Taken} =
        hb_ao:resolve(
            #{
                <<"path">> => <<"~bits@1.0/take=7">>,
                <<"body">> => <<255, 1>>
            },
            #{}
        ),
    ?assertEqual(<<127:7>>, Taken),
    ?assertMatch(
        {error, _},
        hb_ao:resolve(
            #{
                <<"path">> => <<"~bits@1.0/take=17">>,
                <<"body">> => <<255, 1>>
            },
            #{}
        )
    ).

%% @doc Decode a repeated body to its records in order, refusing a body that
%% is not a whole multiple of the record size and a nested repeat.
from_repeat_test() ->
    Format = <<"~bits@1.0/from=repeat(start:64+integer,end:64+integer)">>,
    {ok, Records} =
        hb_ao:resolve(
            #{
                <<"path">> => Format,
                <<"body">> => <<1:64, 2:64, 3:64, 5:64, 8:64, 13:64>>
            },
            #{}
        ),
    ?assertEqual(
        [
            #{ <<"start">> => 1, <<"end">> => 2 },
            #{ <<"start">> => 3, <<"end">> => 5 },
            #{ <<"start">> => 8, <<"end">> => 13 }
        ],
        Records
    ),
    ?assertMatch(
        {error, _},
        hb_ao:resolve(
            #{ <<"path">> => Format, <<"body">> => <<1:64, 2:64, 3:32>> },
            #{}
        )
    ),
    ?assertMatch(
        {error, _},
        hb_ao:resolve(
            #{
                <<"path">> => <<"~bits@1.0/from=repeat(repeat(a:8))">>,
                <<"body">> => <<1>>
            },
            #{}
        )
    ).

%% @doc Decode the first chunk of the published RedStone exclusion-interval
%% artifact, `CAEPGRdVcywDxFrSDz2NMhsHD1-WDQIKDLJOCjpjOPE': 16,384 whole
%% records per 256 KiB chunk, whose first interval is known and whose
%% records ascend without overlap.
from_published_intervals_test_() ->
    {timeout, 120, fun from_published_intervals/0}.
from_published_intervals() ->
    {ok, Body} = hb_store_arweave:read_chunks(390190281040118, 262144, #{}),
    {ok, Records} =
        hb_ao:resolve(
            #{
                <<"path">> =>
                    <<"~bits@1.0/from=repeat(start:64+integer,end:64+integer)">>,
                <<"body">> => Body
            },
            #{}
        ),
    ?assertEqual(16384, length(Records)),
    [First | _] = Records,
    ?assertEqual(
        #{ <<"start">> => 165257438863606, <<"end">> => 165257438865427 },
        First
    ),
    lists:foldl(
        fun(Record, Previous) ->
            ?assert(hb_maps:get(<<"start">>, Record) >= Previous),
            hb_maps:get(<<"end">>, Record)
        end,
        0,
        lists:sublist(Records, 100)
    ).

%% @doc Decode float, atom and enum fields, refusing malformed sizes and an
%% enum value past its last member.
from_typed_fields_test() ->
    {ok, Decoded} =
        hb_ao:resolve(
            #{
                <<"path">> =>
                    <<"~bits@1.0/from=ratio:32+float,unit:40+atom,"
                        "codec:4+enum(tx@1.0,ans104@1.0),_:4">>,
                <<"body">> =>
                    <<1.5:32/float, "chunk", 1:4, 0:4>>
            },
            #{}
        ),
    ?assertEqual(1.5, hb_maps:get(<<"ratio">>, Decoded)),
    ?assertEqual(chunk, hb_maps:get(<<"unit">>, Decoded)),
    ?assertEqual(<<"ans104@1.0">>, hb_maps:get(<<"codec">>, Decoded)),
    ?assertMatch(
        {error, _},
        hb_ao:resolve(
            #{
                <<"path">> => <<"~bits@1.0/from=bad:24+float">>,
                <<"body">> => <<0:24>>
            },
            #{}
        )
    ),
    ?assertMatch(
        {error, _},
        hb_ao:resolve(
            #{
                <<"path">> => <<"~bits@1.0/from=bad:12+atom">>,
                <<"body">> => <<0:12>>
            },
            #{}
        )
    ),
    ?assertMatch(
        {error, _},
        hb_ao:resolve(
            #{
                <<"path">> => <<"~bits@1.0/from=c:4+enum(a,b),_:4">>,
                <<"body">> => <<3:4, 0:4>>
            },
            #{}
        )
    ).
