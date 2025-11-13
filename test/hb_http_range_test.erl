-module(hb_http_range_test).
-include_lib("eunit/include/eunit.hrl").

abs_range_test() ->
    ?assertEqual({ok, {0, 99}}, hb_http_range:parse(<<"bytes=0-99">>, 200)).

open_end_range_test() ->
    ?assertEqual({ok, {10, 199}}, hb_http_range:parse(<<"bytes=10-">>, 200)).

suffix_range_test() ->
    ?assertEqual({ok, {150, 199}}, hb_http_range:parse(<<"bytes=-50">>, 200)).

invalid_range_format_test() ->
    ?assertEqual({error, invalid_range}, hb_http_range:parse(<<"bytes=foo-bar">>, 200)).

unsatisfiable_range_test() ->
    ?assertEqual({error, {range_not_satisfiable, 100}}, hb_http_range:parse(<<"bytes=150-199">>, 100)).

build_content_range_test() ->
    ?assertEqual(<<"bytes 0-9/100">>, hb_http_range:build_content_range(0, 9, 100)).

build_unsatisfied_content_range_test() ->
    ?assertEqual(<<"bytes */100">>, hb_http_range:build_unsatisfied_content_range(100)).
