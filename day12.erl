-module(day12).

-export([part1/1, part2/1, traverse2/2]).

can_visit_cave(Path, Cave, MaxVisits) when Cave > "Z" -> true;
can_visit_cave(Path, Cave, 1) -> not lists:member(Connection, Traverse);
can_visit_cave(Path, Cave, MaxVisits) ->
    PresentSmall = [ {SmallCave, util:count_list(SmallCave, Path)} || SmallCave <- Path, SmallCave > "Z", SmallCave /= "start", SmallCave /= "end"],



traverse(Map) -> traverse(Map, [["start"]], []).
traverse(_, [], Finished) -> Finished;
traverse(Map, [Traverse|Rest], FinishedPaths) ->
    Connections = maps:get(lists:last(Traverse), Map),
    NewPaths = [Traverse ++ [Connection] || Connection <- Connections, (Connection > "Z" andalso not lists:member(Connection, Traverse)) orelse Connection =< "Z"],
    Unfinished = [NewPath || NewPath <- NewPaths, lists:last(NewPath) /= "end"],
    Finished = [NewPath || NewPath <- NewPaths, lists:last(NewPath) == "end"],
    traverse(Map, Rest ++ Unfinished, FinishedPaths ++ Finished).


traverse2(Map, DoubleVisit) -> traverse2(Map, [["start"]], [], DoubleVisit).
traverse2(_, [], Finished, _) -> sets:from_list(Finished);
traverse2(Map, [Traverse|Rest], FinishedPaths, DoubleVisit) ->
    Connections = maps:get(lists:last(Traverse), Map),
    NewPaths = [Traverse ++ [Connection] || Connection <- Connections, (Connection > "Z" andalso not lists:member(Connection, Traverse)) orelse Connection =< "Z" orelse (Connection == DoubleVisit andalso util:count_list(Connection, Traverse) < 2)],
    Unfinished = [NewPath || NewPath <- NewPaths, lists:last(NewPath) /= "end"],
    Finished = [NewPath || NewPath <- NewPaths, lists:last(NewPath) == "end"],
    %io:format("~p ~p ~p~n", [Traverse, Rest, FinishedPaths]),
    %io:format("~p ~p ~p ~p~n", [Connections, NewPaths, Unfinished, Finished]),
    traverse2(Map, Rest ++ Unfinished, FinishedPaths ++ Finished, DoubleVisit).

part1(Lines) ->
    Map = util:parse_edges(Lines),
    length(traverse(Map)).


part2(Lines) ->
    Map = util:parse_edges(Lines),
    PathCalls = [rpc:async_call(node(), day12, traverse2, [Map, SmallCave])  || SmallCave <- maps:keys(Map), SmallCave > "Z", SmallCave /= "start", SmallCave /= "end"],
    Paths = [ rpc:yield(C) || C <- PathCalls ],
    %[util:print_set(Path) || Path <- Paths],
    %util:print_set("Union", sets:union(Paths)),
    sets:size(sets:union(Paths)).
