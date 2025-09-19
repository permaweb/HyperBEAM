-module(hb_data_reader_test).
-include_lib("eunit/include/eunit.hrl").

compute_next_range_middle_test() ->
    ?assertEqual({0, 2, false}, hb_data_reader:compute_next_range(0, 10, 3)).

compute_next_range_final_test() ->
    ?assertEqual({9, 9, true}, hb_data_reader:compute_next_range(9, 10, 4)).

compute_next_range_zero_size_test() ->
    ?assertEqual({0, -1, true}, hb_data_reader:compute_next_range(0, 0, 4)).
