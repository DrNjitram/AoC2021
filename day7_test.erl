-module(day7_test).
-include_lib("eunit/include/eunit.hrl").


part1_test() ->
    ?assertEqual(37, day7:part1(["16,1,2,0,4,2,7,1,2,14"])),
    ok.

part2_test() ->
    ?assertEqual(168, day7:part2(["16,1,2,0,4,2,7,1,2,14"])),
    ok.