-module(day9).

-export([part1/1, part2/1]).


get_char(X, Y, Map) -> list_to_integer([lists:nth(X, lists:nth(Y, Map))]).
to_hashmap(Lines) ->
    maps:from_list([ {{X, Y}, get_char(X, Y, Lines)} || X <- lists:seq(1, length(hd(Lines))), Y <- lists:seq(1, length(Lines))]).



part1(Lines) ->
    Map = to_hashmap(Lines),
    maps:get({1, 1}, Map).

part2(Lines) ->
    ok.