-module(day6).

-export([part1/1, part2/1]).

smart_step(Fishes, 0) ->
    lists:sum(Fishes);
smart_step([D0, D1, D2, D3, D4, D5, D6, D7, D8], Days) ->
    smart_step([D1, D2, D3, D4, D5, D6, D7+ D0, D8, D0], Days - 1).

parse_fishes(Lines) ->
    Line = [list_to_integer(Value) || Value <- string:tokens(Lines, ",")],
    [util:count(I, Line) || I <- lists:seq(0, 8)].

part1([Lines]) ->
    smart_step( parse_fishes(Lines), 80).

part2([Lines]) ->
     smart_step( parse_fishes(Lines), 256).