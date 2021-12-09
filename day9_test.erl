-module(day9_test).
-include_lib("eunit/include/eunit.hrl").


get_input() ->
    ["2199943210", 
    "3987894921", 
    "9856789892", 
    "8767896789", 
    "9899965678"].

part1_test() ->
    ?assertEqual(15, day9:part1(get_input() )),
    ok.

part2_test() ->
    ?assertEqual(168, day9:part2(get_input())),
    ok.