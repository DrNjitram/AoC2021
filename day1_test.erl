-module(day1_test).
-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    ?assertEqual(2, day1:part1(["12"])),
    ?assertEqual(2, day1:part1(["14"])),
    ?assertEqual(654, day1:part1(["1969"])),
    ?assertEqual(33583, day1:part1(["100756"])),
    ok.

part2_test() ->
    ?assertEqual(2, day1:part2(["14"])),
    ?assertEqual(966, day1:part2(["1969"])),
    ?assertEqual(50346, day1:part2(["100756"])),
    ok.
