-module(day13).

-export([part1/1]).

flip_map(Map, Axis, Line) ->
    RelevantPoints = maps:filter(fun({X, Y}, _) -> (X > Line orelse Axis == y) andalso (Y > Line orelse Axis == x) end, Map),
    case Axis of 
        x -> FlippedPoints = maps:from_list([{{2*Line - X, Y}, 1} || {X, Y} <- maps:keys(RelevantPoints)]);
        y -> FlippedPoints = maps:from_list([{{X, 2*Line - Y}, 1} || {X, Y} <- maps:keys(RelevantPoints)])
    end,
    MapWithoutFoldedPoints = maps:filter(fun(K, _) -> maps:find(K, RelevantPoints) == error end, Map),
    maps:fold(fun(K, _, Accin) -> Accin#{K => 1} end, MapWithoutFoldedPoints, FlippedPoints).

parse_input(Commands) -> parse_input(Commands, #{}).
parse_input([], Map) -> Map;
parse_input([[]|Rest], Map) -> parse_input(Rest, Map);
parse_input(["fold along " ++ Fold | Rest], Map) ->  
    [Axis, Position] = util:parse(Fold, [atom, integer], "="),
    parse_input(Rest, flip_map(Map, Axis, Position));
parse_input([Coord| Rest], Map) -> 
    [X, Y] = util:parse(Coord, [integer, integer], ","),
    parse_input(Rest, Map#{{X, Y} => 1}).

part1(Lines) ->
    Result = parse_input(Lines),
    util:print_binary_map(Result).
