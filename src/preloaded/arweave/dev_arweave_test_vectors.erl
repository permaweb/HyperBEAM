%%% @doc Test vectors for the `~arweave@2.9' gateway surface.
-module(dev_arweave_test_vectors).
-include_lib("eunit/include/eunit.hrl").

%% @doc `block' and `validated' share one strict block-hash parser before a
%% caller-supplied value can become a store path.
block_refuses_a_traversal_test() ->
    Traversal = <<(binary:copy(<<"../">>, 21))/binary, "x">>,
    ?assertEqual(64, byte_size(Traversal)),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-block">> }},
        dev_arweave:block({id, Traversal}, #{}, #{})
    ),
    Wellformed = binary:copy(<<"a">>, 64),
    ?assertNotMatch(
        {error, #{ <<"message">> := <<"invalid-block">> }},
        dev_arweave:block(
            {id, Wellformed},
            #{ <<"cache-control">> => [<<"only-if-cached">>] },
            #{ <<"store">> => [hb_test_utils:test_store()] }
        )
    ).
