-module(day1_test).
-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    ?assertEqual(7, day1:part1(["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"])),
    ok.

part2_test() ->
    ?assertEqual(5, day1:part2(["607", "618", "618", "617", "647", "716", "769", "792"])),
    ok.