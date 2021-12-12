-module(day12).

-export([part1/1, part2/1]).

occurcs_more_than(_, [], _) -> false;
occurcs_more_than(_, _, 0) -> true;
occurcs_more_than(Elem, [Elem|T], Limit) -> occurcs_more_than(Elem, T, Limit - 1) ;
occurcs_more_than(Elem, [_|T], Limit) -> occurcs_more_than(Elem, T, Limit).


can_visit_cave(_, "start", _) -> false;
can_visit_cave(_, "end", _) -> true;
can_visit_cave(_, Cave, _) when Cave =< "Z" -> true;
can_visit_cave(Path, Cave, 1) -> not lists:member(Cave, Path);
can_visit_cave(Path, Cave, MaxVisits) ->
    PresentSmall = [ {SmallCave, util:count_list(SmallCave, Path)} || SmallCave <- Path, SmallCave > "Z", SmallCave /= "start", SmallCave /= "end"],
    AllBelowLimit = lists:all(fun({_, Visits}) -> Visits < MaxVisits end, PresentSmall),
    AllBelowLimit orelse not lists:member(Cave, Path).

traverse(Map, MaxVisits) -> traverse(Map, [["start"]], [], MaxVisits).
traverse(_, [], Finished, _) -> Finished;
traverse(Map, [Traverse|Rest], FinishedPaths, MaxVisits) ->
    io:format("~p:~p~n", [length(Rest), length(FinishedPaths)]),
    Connections = maps:get(lists:last(Traverse), Map),
    NewPaths = [Traverse ++ [Connection] || Connection <- Connections, can_visit_cave(Traverse, Connection, MaxVisits)],
    Unfinished = [NewPath || NewPath <- NewPaths, lists:last(NewPath) /= "end"],
    Finished = [NewPath || NewPath <- NewPaths, lists:last(NewPath) == "end"],
    traverse(Map, Rest ++ Unfinished, FinishedPaths ++ Finished, MaxVisits).

part1(Lines) ->
    Map = util:parse_edges(Lines),
    length(traverse(Map, 1)).

part2(Lines) ->
    Map = util:parse_edges(Lines),
    length(traverse(Map, 2)).
