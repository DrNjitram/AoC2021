-module(day11_test).
-include_lib("eunit/include/eunit.hrl").


-define(INPUT,
    [
        "5483143223",
        "2745854711",
        "5264556173",
        "6141336146",
        "6357385478",
        "4167524645",
        "2176841721",
        "6882881134",
        "4846848554",
        "5283751526"
    ]).



part1_test() ->
    ?assertEqual(1656, day11:part1(?INPUT)),
    ok.

part2_test() ->
    ?assertEqual(195, day11:part2(?INPUT)),
    ok.