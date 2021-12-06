-module(day5_test).
-include_lib("eunit/include/eunit.hrl").


part1_test() ->
    ?assertEqual(4512, day5:part1()),
    ok.

part2_test() ->
    ?assertEqual(1924, day4:part2()),
    ok.