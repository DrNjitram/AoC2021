-module(day7).

-export([part1/1, part2/1]).

create_costs(Max) -> [round(N*(N+1)/2) || N <- lists:seq(0, Max + 1)].

get_fuel1(Val, Vals) -> lists:sum([abs(Val - V) || V <- Vals]).

get_fuel2(Val, Vals, Costs) -> lists:sum([ lists:nth(abs(Val - V) + 1, Costs) || V <- Vals]).


part1([Lines]) ->
    Vals = [list_to_integer(X) || X <- string:tokens(Lines, ",")],
    get_fuel1(lists:nth(round(length(Vals)/2),lists:sort(Vals)), Vals).

part2([Lines]) ->
    Vals = [list_to_integer(X) || X <- string:tokens(Lines, ",")],
    [Min, Max] = [lists:min(Vals), lists:max(Vals)],
    get_fuel2(round(lists:sum(Vals)/(length(Vals) + 0.5)), Vals, create_costs(Max - Min)).