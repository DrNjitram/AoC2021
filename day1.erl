-module(day1).

-export([part1/1, part2/1]).


part1(Lines) ->
    check_heights(["999"] ++ [ list_to_integer(Line) || Line <- Lines], 0).


check_heights([_], Acc) ->
    Acc;
check_heights(Lines, Acc) ->
    [H1, H2| Rest] = Lines,
    check_heights(
        [H2] ++ Rest, 
        Acc + if H1 < H2 -> 1;
            true -> 0
        end
    ).

check_heights_window([_, _ ,_], Acc) ->
    Acc;
check_heights_window(Lines, Acc) ->
    [H1, _, _, H4| _] = Lines,
    [_|Tail] = Lines,
    check_heights_window(
        Tail, 
        Acc + if H1 < H4 -> 1;
            true -> 0
        end
    ).

part2(Lines) ->
    check_heights_window([list_to_integer(Line) || Line <- Lines], 0).

