-module(day11).

-export([part1/1, part2/1]).

process_flash(Pos, Map) -> lists:foldl(fun(Elem, Acc) -> Acc#{Elem => maps:get(Elem, Acc) + 1} end, Map, util:get_adjecents_positions(Pos, Map, adj8)).

process_flashing(Map)  -> process_flashing(Map, 0, []).
process_flashing(Map, Acc, Flashed) ->
    Flashing = maps:filter(fun(K, V) -> V > 9 andalso not lists:member(K, Flashed) end, Map),
    Flashes = maps:size(Flashing),
    if Flashes == 0 -> [Acc + Flashes, Map];
        Flashes > 0 -> 
            NewMap = lists:foldl(fun(Flash, AccMap) -> process_flash(Flash, AccMap) end, Map, maps:keys(Flashing)),
            process_flashing(NewMap, Acc + Flashes, Flashed ++ maps:keys(Flashing))
    end.

step(Map, Steps) -> step(Map, Steps, 0).
step(_, 0, Flashes) -> Flashes;
step(Map, Steps, Flashes) ->
    NewMap = maps:map(fun(_, V) -> V + 1 end, Map),
    [NewFlashes, NewerMap] = process_flashing(NewMap),
    NewestMap = maps:map(fun(_, V) when V > 9 -> 0; (_, V) -> V end, NewerMap),
    step(NewestMap, Steps - 1, Flashes + NewFlashes).


step2(Map, Step) ->
    NewMap = maps:map(fun(_, V) -> V + 1 end, Map),
    [NewFlashes, NewerMap] = process_flashing(NewMap),
    if NewFlashes == 100 -> Step + 1;
        NewFlashes < 100 ->
            NewestMap = maps:map(fun(_, V) when V > 9 -> 0; (_, V) -> V end, NewerMap),
            step2(NewestMap, Step + 1)
    end.

part1(Lines) ->
    step(util:to_hashmap(Lines), 100).

part2(Lines) ->
    step2(util:to_hashmap(Lines), 0).