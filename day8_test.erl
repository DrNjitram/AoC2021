-module(day8_test).
-include_lib("eunit/include/eunit.hrl").


part1_test() ->
    ?assertEqual(0, day8:part1(["16,1,2,0,4,2,7,1,2,14"])),
    ok.

part2_test() ->
    ?assertEqual(0, day8:part2(["16,1,2,0,4,2,7,1,2,14"])),
    ok.