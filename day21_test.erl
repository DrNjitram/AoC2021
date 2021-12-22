-module(day21_test).
-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    ?assertEqual(739785, day21:part1(["Player 1 starting position: 4", "Player 2 starting position: 8"])),
    ok.

part2_test() ->
    ?assertEqual(444356092776315, day21:part2(["Player 1 starting position: 4", "Player 2 starting position: 8"])),
    ok.