-module(day12_test).
-include_lib("eunit/include/eunit.hrl").


-define(INPUT1,
    [
        "start-A",
        "start-b",
        "A-c",
        "A-b",
        "b-d",
        "A-end",
        "b-end"
    ]).

-define(INPUT2,
    [
        "dc-end",
        "HN-start",
        "start-kj",
        "dc-start",
        "dc-HN",
        "LN-dc",
        "HN-end",
        "kj-sa",
        "kj-HN",
        "kj-dc"
    ]).

-define(INPUT3,
    [
        "fs-end",
        "he-DX",
        "fs-he",
        "start-DX",
        "pj-DX",
        "end-zg",
        "zg-sl",
        "zg-pj",
        "pj-he",
        "RW-he",
        "fs-DX",
        "pj-RW",
        "zg-RW",
        "start-pj",
        "he-WI",
        "zg-he",
        "pj-fs",
        "start-RW"
    ]).



part1_test() ->
    ?assertEqual(10, day12:part1(?INPUT1)),
    ?assertEqual(19, day12:part1(?INPUT2)),
    ?assertEqual(226, day12:part1(?INPUT3)),
    ok.

part2_test() ->
    ?assertEqual(36, day12:part2(?INPUT1)),
    ?assertEqual(103, day12:part2(?INPUT2)),
    ?assertEqual(3509, day12:part2(?INPUT3)),
    ok.