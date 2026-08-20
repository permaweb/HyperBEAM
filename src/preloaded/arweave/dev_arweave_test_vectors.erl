%%% @doc Test vectors for the `~arweave@2.9' gateway surface.
-module(dev_arweave_test_vectors).
-export([live_chunk_proof/0]).
-include("include/hb.hrl").
-include("include/ar.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc `block' and `validated' share one strict block-hash parser before a
%% caller-supplied value can become a store path.
block_refuses_a_traversal_test() ->
    Traversal = <<(binary:copy(<<"../">>, 21))/binary, "x">>,
    Base = #{ <<"device">> => <<"arweave@2.9">> },
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    ?assertEqual(64, byte_size(Traversal)),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-block">> }},
        hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"block">>, <<"block">> => Traversal },
            Opts
        )
    ),
    Wellformed = binary:copy(<<"a">>, 64),
    ?assertNotMatch(
        {error, #{ <<"message">> := <<"invalid-block">> }},
        hb_ao:resolve(
            Base,
            #{
                <<"path">> => <<"block">>,
                <<"block">> => Wellformed,
                <<"cache-control">> => [<<"only-if-cached">>]
            },
            Opts
        )
    ).

%% @doc `chunk-proof' answers with the whole of a peer's chunk response: the
%% bytes, the two Merkle paths that place them in the weave, and the absolute
%% end offset the chunk's packing is keyed on. A miner reads the weave through
%% this key, so every field it needs has to survive the projection.
%%
%% The offsets are the two either side of the first chunk boundary of the
%% weave, because that boundary is where the answer is easiest to get wrong: a
%% recall byte counts from zero and Arweave addresses a chunk by a byte inside
%% it, counting from one, so the last byte of a chunk and the first byte of the
%% next are one apart in the caller's coordinates and resolve to different
%% chunks. A key that passed the caller's byte through unchanged would answer
%% every boundary with the chunk below it, and the proof of access checked
%% against that answer would be for a chunk the block does not recall.
%%
%% The offsets are fixed and low, so the chunks they name are as old as the
%% weave and every peer holds them.
live_chunk_proof() ->
    Opts = #{},
    ?assertEqual(?DATA_CHUNK_SIZE, live_chunk_end(?DATA_CHUNK_SIZE - 1, Opts)),
    ?assertEqual(2 * ?DATA_CHUNK_SIZE, live_chunk_end(?DATA_CHUNK_SIZE, Opts)).

%% @doc The end offset of the chunk holding a byte, having checked that the
%% answer carries everything a proof of access is built from.
live_chunk_end(Offset, Opts) ->
    {ok, Proof} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"chunk-proof">>, <<"offset">> => Offset },
            Opts
        ),
    Field = fun(Key) -> hb_maps:get(Key, Proof, not_found, Opts) end,
    ?assertEqual(<<"unpacked">>, Field(<<"packing">>)),
    ?assertEqual(
        Field(<<"chunk-size">>),
        byte_size(hb_util:decode(Field(<<"chunk">>)))
    ),
    ?assertNotEqual(not_found, Field(<<"data-path">>)),
    ?assertNotEqual(not_found, Field(<<"tx-path">>)),
    Field(<<"absolute-end-offset">>).
