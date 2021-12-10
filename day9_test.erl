-module(day9_test).
-include_lib("eunit/include/eunit.hrl").


-define(INPUT,
    [
        "2199943210", 
        "3987894921", 
        "9856789892", 
        "8767896789", 
        "9899965678"
    ]).

part1_test() ->
    ?assertEqual(15, day9:part1(?INPUT)),
    ok.

part2_test() ->
    ?assertEqual(1134, day9:part2(?INPUT)),
    ok.