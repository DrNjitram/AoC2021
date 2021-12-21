-module(pathfinding).

-export([shortest_path/3]).

h({X0, Y0}, {X1, Y1}) -> abs(X0 - X1) + abs(Y0 - Y1).

apply_neighbours(_, _, _,[], GScore, OpenSet, CameFrom) -> [GScore, OpenSet, CameFrom];
apply_neighbours(Map, Current, Goal, [Neighbour|Rest], GScore, OpenSet, CameFrom) ->
    TentativeG = maps:get(Current, GScore) + maps:get(Neighbour, Map),
    PreviousPath = maps:get(Neighbour, GScore, infinity),
    if TentativeG < PreviousPath -> 
        NewOpenSet = case lists:member(Neighbour, [ Pos || {Pos, _} <- OpenSet]) of
            true -> OpenSet;
            false -> 
                util:insert({Neighbour, TentativeG + h(Neighbour, Goal)}, OpenSet)
        end,
        apply_neighbours(Map, Current, Goal, Rest,
            GScore#{Neighbour => TentativeG},
            NewOpenSet,
            CameFrom#{Neighbour => Current}
        );
        true ->
            apply_neighbours(Map, Current, Goal, Rest, GScore, OpenSet, CameFrom) 
    end.


reconstruct_path(CameFrom, Current) -> reconstruct_path(CameFrom, Current, [Current]).
reconstruct_path(CameFrom, Current, TotalPath) ->
    case maps:is_key(Current, CameFrom) of
        false -> TotalPath;
        true ->
            Next = maps:get(Current, CameFrom),
            reconstruct_path(CameFrom, Next, [Next|TotalPath])
    end.

shortest_path(Map, Start, End) ->
    shortest_path(Map, Start, End, [{Start, h(Start, End)}], #{}, #{Start => 0}).
shortest_path(_, _, End, [], CameFrom, _) -> reconstruct_path(CameFrom, End);
shortest_path(_, _, End, [{End, _}|_], CameFrom, _) -> reconstruct_path(CameFrom, End);
shortest_path(Map, Start, End, [{Current, _}|OpenSet], CameFrom, GScore) ->
    io:format("~p~n", [maps:size(CameFrom)]),
    Neighbours = util:get_adjecents_positions(Current, Map, adj4),
    [NewGScore, NewOpenSet, NewCameFrom] = apply_neighbours(Map, Current, End, Neighbours, GScore, OpenSet, CameFrom),
    shortest_path(Map, Start, End, NewOpenSet, NewCameFrom, NewGScore).