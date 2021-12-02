-module(day2).

-export([part1/1, part2/1]).

get_depth_height([], Depth, Distance) ->
    Depth * Distance;
get_depth_height([Command|Rest], Depth, Distance) ->
    [Direction, Value] = string:tokens(Command, " "),
    case Direction of
        "forward" -> 
            get_depth_height(Rest, Depth, Distance + list_to_integer(Value));
        "down" ->
            get_depth_height(Rest, Depth + list_to_integer(Value), Distance);
        "up" ->
            get_depth_height(Rest, Depth - list_to_integer(Value), Distance)
    end.

get_aim([], Depth, Distance, _) ->
    Depth * Distance;
get_aim([Command|Rest], Depth, Distance, Aim) ->
    [Direction, Value] = string:tokens(Command, " "),
    case Direction of
        "forward" -> 
            get_aim(Rest, Depth + (Aim * list_to_integer(Value)), Distance + list_to_integer(Value), Aim);
        "down" ->
            get_aim(Rest, Depth, Distance, Aim  + list_to_integer(Value));
        "up" ->
            get_aim(Rest, Depth, Distance, Aim  - list_to_integer(Value))
    end.


part1(Lines) ->
    get_depth_height(Lines, 0, 0).

part2(Lines) ->
    get_aim(Lines, 0, 0, 0).