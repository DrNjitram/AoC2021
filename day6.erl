-module(day6).

-export([part1/1, part2/1]).

age_fish(Fish) when Fish > 0 -> Fish - 1;
age_fish(_) -> [6, 8].

% 2^((80 - 3)/7)

step(Fishes, 0) ->
    lists:flatten(Fishes);
step(Fishes, Days) ->
    step(lists:flatten([age_fish(Fish) || Fish <- Fishes]), Days - 1).

smart_step(Fishes, Days) ->
    lists:sum([ math:pow(2, (Days - Fish)/7) || Fish <- Fishes]).

part1([Lines]) ->
    length(step([list_to_integer(Value) || Value <- string:tokens(Lines, ",")], 80)).

part2([Lines]) ->
    length(step([list_to_integer(Value) || Value <- string:tokens(Lines, ",")], 256)).