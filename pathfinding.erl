-module(pathfinding).

-export([shortest_path/3]).

h({X0, Y0}, {X1, Y1}) -> (abs(X0 - X1) + abs(Y0 - Y1)) div 2.

apply_neighbours(_, _, _, [], GScore, OpenSet, CameFrom) -> [GScore, OpenSet, CameFrom];
apply_neighbours(Map, Current, Goal, [Neighbour|Rest], GScore, OpenSet, CameFrom) ->
    TentativeG = maps:get(Current, GScore) + maps:get(Neighbour, Map),
    PreviousPath = maps:get(Neighbour, GScore, infinity),
    if TentativeG < PreviousPath -> 
        NewOpenSet = case lists:member(Neighbour, gb_trees:values(OpenSet)) of
            true -> OpenSet;
            false -> 
                Cost = TentativeG + h(Neighbour, Goal),
                case gb_trees:lookup(Cost, OpenSet) of
                    {value, V} -> gb_trees:update(Cost, V ++ [Neighbour], OpenSet);
                    none -> gb_trees:insert(Cost, [Neighbour], OpenSet)
                end
        end,
        apply_neighbours(Map, Current, Goal, Rest,
            GScore#{Neighbour => TentativeG},
            NewOpenSet,
            CameFrom#{Neighbour => Current}
        );
        true ->
            apply_neighbours(Map, Current, Goal, Rest, GScore, OpenSet, CameFrom) 
    end.

shortest_path(Map, Start, End) ->
    shortest_path(Map, Start, End, gb_trees:insert(h(Start, End), [Start], gb_trees:empty()), #{}, #{Start => 0}).
shortest_path(Map, Start, End, OpenSet, CameFrom, GScore) ->
    {K, V, OpenSet2} = gb_trees:take_smallest(OpenSet),
    [Current, OpenSet3] = case V of
        [V1] -> [V1, OpenSet2];
        [V1|Rest] -> [V1, gb_trees:insert(K, Rest, OpenSet2)]
    end,
    if Current == End -> 
            maps:get(End, GScore);
        true ->
            Neighbours = util:get_adjecents_positions(Current, Map, adj4),
            [NewGScore, OpenSet4, NewCameFrom] = apply_neighbours(Map, Current, End, Neighbours, GScore, OpenSet3, CameFrom),
            shortest_path(Map, Start, End, OpenSet4, NewCameFrom, NewGScore)
    end.