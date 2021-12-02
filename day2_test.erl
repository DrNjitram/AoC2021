-module(day2_test).
-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    ?assertEqual(150, day2:part1(["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"])),
    ok.

part2_test() ->
    ?assertEqual(900, day2:part2(["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"])),
    ok.