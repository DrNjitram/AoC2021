-module(day15).

-export([part1/1, part2/1]).


-define(INPUT, [
        "1163751742",
        "1381373672",
        "2136511328",
        "3694931569",
        "7463417111",
        "1319128137",
        "1359912421",
        "3125421639",
        "1293138521",
        "2311944581"
    ]).


part1(Lines) ->
    Map = util:to_hashmap(Lines),
    {X0, Y0, X1, Y1} = util:get_extend(Map),
    Path = pathfinding:shortest_path(Map, {X0, Y0}, {X1, Y1}),
    io:format("~p~n", [[{P, maps:get(P, Map)} || P <- Path]]),
    util:print_map_path(Map, Path),
    lists:sum([ maps:get(P, Map) || P <- Path]) - 1.


part2(_) ->
    ok.