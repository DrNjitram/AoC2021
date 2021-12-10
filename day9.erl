-module(day9).

-export([part1/1, part2/1]).

get_adjecents({X0, Y0}, Map) -> [maps:get({X0 + X, Y0 + Y}, Map, 9) || {X, Y} <- [{0, -1}, {0, 1}, {1, 0}, {-1, 0}]].
check_low({X, Y}, Map) -> length([Height || Height <- get_adjecents({X, Y}, Map), Height =< maps:get({X, Y}, Map)]) == 0.
get_low_locations(Map) -> [{X, Y} || {X, Y} <- maps:keys(Map), check_low({X, Y}, Map)].

get_basin_area({X, Y}, Map) -> get_basin_area([{X, Y}], Map, 1, [{X, Y}]).
get_basin_area([], _, Acc, _) -> Acc;
get_basin_area([{X, Y}| Rest], Map, Acc, Checked) ->
    Adj = [ {{X0 + X, Y0 + Y}, maps:get({X0 + X, Y0 + Y}, Map, 9)} || {X0, Y0} <- [{0, -1}, {0, 1}, {1, 0}, {-1, 0}]],
    MoreBasin = [{X0, Y0} || {{X0, Y0}, Height} <- Adj, lists:member({X0, Y0}, Checked) == false, Height < 9],
    get_basin_area(Rest ++ MoreBasin, Map, Acc + length(MoreBasin), Checked ++ [{X0, Y0} || {{X0, Y0}, _} <- Adj]).

answer(Areas) -> sum_list(lists:sort(fun(A, B) -> A > B end, Areas)).
sum_list([A, B, C|_]) -> A * B * C.

part1(Lines) ->
    Map = util:to_hashmap(Lines),
    lists:sum([maps:get({X, Y}, Map) + 1 ||  {X, Y} <- get_low_locations(Map)]).

part2(Lines) ->
    Map = util:to_hashmap(Lines),
    answer([get_basin_area(Point, Map) || Point <- get_low_locations(Map)]).
