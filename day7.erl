-module(day7).

-export([part1/1, part2/1]).

get_fuel1(Val, Vals) -> lists:sum([abs(Val - V) || V <- Vals]).

get_fuel2(Val, Vals) -> lists:sum([get_fuel(Val, V) || V <- Vals]).

get_fuel(A, B) when A > B -> N = A - B, trunc(N*(N+1)/2); 
get_fuel(A, B) when A < B -> N = B - A, trunc(N*(N+1)/2); 
get_fuel(A, A) -> 0.    

part1([Lines]) ->
    Vals = [list_to_integer(X) || X <- string:tokens(Lines, ",")],
    get_fuel1(lists:nth(round(length(Vals)/2),lists:sort(Vals)), Vals).

part2([Lines]) ->
    Vals = [list_to_integer(X) || X <- string:tokens(Lines, ",")],
    Avg = round(lists:sum(Vals)/(length(Vals))),
    lists:min([get_fuel2(Avg, Vals), get_fuel2(Avg - 1, Vals), get_fuel2(Avg + 1, Vals)]).