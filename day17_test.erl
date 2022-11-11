-module(day17_test).
-include_lib("eunit/include/eunit.hrl").



part1_and_2_test() ->
    ?assertEqual([112, 45.0], day17:part1_and_2(["target area: x=20..30, y=-10..-5"])),
    ok.

