-module(day7).

-export([part1/1, part2/1]).

create_costs(Max) ->
    create_costs(Max + 1, [0], 1).
create_costs(Max, Acc, Max) -> Acc;
create_costs(Max, Acc, Curr) ->
    create_costs(Max, Acc ++ [lists:last(Acc) + Curr], Curr + 1).


get_fuel1(Val, Vals) ->
    lists:sum([abs(Val - V) || V <- Vals]).

get_fuels(Vals) ->
    Min = lists:min(Vals),
    Max = lists:max(Vals),
    Range =  Max - Min,
    All_costs = create_costs(Range),
    [lists:sum([lists:nth(abs(V1 - V2) + 1, All_costs) || V2 <- Vals]) || V1 <- lists:seq(Min, Max)].

part1([Lines]) ->
    Vals = [list_to_integer(X) || X <- string:tokens(Lines, ",")],
    Fuels = [get_fuel1(V, Vals) || V <- lists:seq(lists:min(Vals), lists:max(Vals))],
    hd(lists:sort(Fuels)).

part2([Lines]) ->
    Vals = [list_to_integer(X) || X <- string:tokens(Lines, ",")],
    Fuels = get_fuels(Vals),
    hd(lists:sort(Fuels)).