-module(day15_test).
-include_lib("eunit/include/eunit.hrl").


-define(INPUT, [
        "1163751742",
        "1381373672",
        "2136511328",
        "3694931569",
        "7463417111",
        "1319128137",
        "1359912421",
        "3125421639",
        "1293138521",
        "2311944581"
    ]).




part1_test() ->
    ?assertEqual(40, day15:part1(?INPUT)),
    ok.

part2_test() ->
    ?assertEqual(315, day15:part2(?INPUT)),
    ok.