-module(day1).

-export([part1/1, part2/1]).

part1(Lines) ->
    check_heights(["999"] ++ [list_to_integer(Line) || Line <- Lines], 0).


check_heights([_], Acc) -> Acc;
check_heights([H1, H2 | T], Acc) when H1 < H2 ->
    check_heights([H2|T], Acc + 1);
check_heights([_|T], Acc) ->
    check_heights(T, Acc).

check_heights_window([_, _, _], Acc) -> Acc;
check_heights_window([H1, H2, H3, H4 | T], Acc) when H1 < H4 ->
    check_heights_window([H2, H3, H4|T], Acc + 1);
check_heights_window([_|T], Acc) ->
    check_heights_window(T, Acc).

part2(Lines) ->
    check_heights_window([list_to_integer(Line) || Line <- Lines], 0).
