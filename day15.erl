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
    pathfinding:shortest_path(Map, {X0, Y0}, {X1, Y1}).

get_value(V, Layer) when V + Layer > 9 -> ((V + Layer) rem 10) + 1;
get_value(V, Layer) -> V + Layer.

add_entries({{X, Y}, V}, Map, Iters, Xm, Ym) ->
    Duplicates = [{{X + (Xi * (Xm+1)), Y +  (Yi * (Ym+1))}, (Xi + Yi)} || Xi <- lists:seq(0, Iters - 1), Yi <- lists:seq(0, Iters - 1)],
    lists:foldl(fun({Pos, Layer}, Accin) -> Accin#{Pos => get_value(V, Layer)} end, Map, Duplicates).


extend_map(Original, Iters, {_, _, Xm, Ym}) -> 
    lists:foldl(fun(E, Accin) -> add_entries(E, Accin, Iters, Xm, Ym) end, Original, maps:to_list(Original)).


part2(Lines) ->
    Map = util:to_hashmap(Lines),
    NewMap = extend_map(Map, 5, util:get_extend(Map)),
    {X0, Y0, X1, Y1} = util:get_extend(NewMap),
    pathfinding:shortest_path(NewMap, {X0, Y0}, {X1, Y1}).
