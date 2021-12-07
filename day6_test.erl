-module(day6_test).
-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    ?assertEqual(5934, day6:part1(["3,4,3,1,2"])),
    ok.

part2_test() ->
    ?assertEqual(26984457539, day6:part2(["3,4,3,1,2"])),
    ok.