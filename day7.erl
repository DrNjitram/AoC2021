-module(day7).

-export([part1/1, part2/1]).

get_fuel1(Val, Vals) ->
    lists:sum([abs(Val - V) || V <- Vals]).

get_fuel2(Val, Vals) ->
    lists:sum([lists:sum(lists:seq(0, abs(V-Val))) || V <- Vals]).

part1([Lines]) ->
    Vals = [list_to_integer(X) || X <- string:tokens(Lines, ",")],
    Fuels = [get_fuel1(V, Vals) || V <- lists:seq(lists:min(Vals), lists:max(Vals))],
    hd(lists:sort(Fuels)).


part2([Lines]) ->
    Vals = [list_to_integer(X) || X <- string:tokens(Lines, ",")],
    Fuels = [get_fuel2(V, Vals) || V <- lists:seq(lists:min(Vals), lists:max(Vals))],
    hd(lists:sort(Fuels)).