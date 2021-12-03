-module(day2).

-export([part1/1, part2/1]).


get_depth_height([], Depth, Distance) ->
    Depth * Distance;
get_depth_height([[Direction, Value]|Rest], Depth, Distance) ->
    case Direction of
        forward -> 
            get_depth_height(Rest, Depth, Distance + Value);
        down ->
            get_depth_height(Rest, Depth + Value, Distance);
        up ->
            get_depth_height(Rest, Depth - Value, Distance)
    end.

get_aim([], Depth, Distance, _) ->
    Depth * Distance;
get_aim([[Direction, Value]|Rest], Depth, Distance, Aim) ->
    case Direction of
        forward -> 
            get_aim(Rest, Depth + (Aim * Value), Distance + Value, Aim);
        down ->
            get_aim(Rest, Depth, Distance, Aim  + Value);
        up ->
            get_aim(Rest, Depth, Distance, Aim  - Value)
    end.


part1(Lines) ->
    get_depth_height(util:parse(Lines, [atom, integer]), 0, 0).

part2(Lines) ->
    get_aim(util:parse(Lines, [atom, integer]), 0, 0, 0).