-module(day14_test).
-include_lib("eunit/include/eunit.hrl").


-define(INPUT, 
    [
        "NNCB",
        "CH -> B",
        "HH -> N",
        "CB -> H",
        "NH -> C",
        "HB -> C",
        "HC -> B",
        "HN -> C",
        "NN -> C",
        "BH -> H",
        "NC -> B",
        "NB -> B",
        "BN -> B",
        "BB -> N",
        "BC -> B",
        "CC -> N",
        "CN -> C"
    ]
).

part1_test() ->
    ?assertEqual(1588, day14:part1(?INPUT)),
    ok.