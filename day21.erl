-module(day21).

-export([part1/1, part2/1]).

-define(ROLLS, [{3, 1}, {4, 3}, {5, 6}, {6, 7}, {7, 6}, {8, 3}, {9, 1}]).

roll_over(Die, Inc) when (Die + Inc) > 101 -> (Die + Inc) rem 101 + 1;
roll_over(Die, Inc) -> Die + Inc.

move(Pos, Die) -> ((Pos + rolls(Die)) rem 11 + (Pos + rolls(Die) - 1) div 10) rem 11.

rolls(Die) -> Die + roll_over(Die, 1) + roll_over(Die, 2). 

round(P1, P2) -> round({P1, 0}, {P2, 0}, 1, 0, p1).
round({_, S1}, {_, S2}, _, Counter, _) when S1 >= 1000 orelse S2 >= 1000 -> min(S1, S2) * Counter;
round({P1, S1}, P2, Die, Counter, p1) -> round({move(P1, Die), S1 + move(P1, Die)}, P2, roll_over(Die, 3), Counter + 3, p2);
round(P1, {P2, S2}, Die, Counter, p2) -> round(P1, {move(P2, Die), S2 + move(P2, Die)}, roll_over(Die, 3), Counter + 3, p1).


parse_result(Count, {P1W, P2W}, {PrevP1W, PrevP2W}) -> {PrevP1W + (Count * P1W), PrevP2W + (Count * P2W)}.
parse_roll({P, S}, Roll) -> {(P + Roll) rem 10, S + 1 + ((P + Roll) rem 10)}.

dirac(P1, P2) -> dirac({P1 - 1, 0}, {P2 - 1, 0}, p1).
dirac({_, S1}, _, _) when S1 >= 21 -> {1, 0};
dirac(_, {_, S2}, _) when S2 >= 21 -> {0, 1};
dirac(P1, P2, p1) -> lists:foldl(fun({Rolls, Count}, Prev) -> parse_result(Count, dirac(parse_roll(P1, Rolls), P2, p2), Prev) end, {0, 0}, ?ROLLS);
dirac(P1, P2, p2) -> lists:foldl(fun({Rolls, Count}, Prev) -> parse_result(Count, dirac(P1, parse_roll(P2, Rolls), p1), Prev) end, {0, 0}, ?ROLLS).


part1(Lines) ->
    [P1, P2] = [ lists:last(Line) - $0|| Line <- Lines],
    round(P1, P2).

part2(Lines) ->
    [P1, P2] = [ lists:last(Line) - $0|| Line <- Lines],
    {P1W, P2W} = dirac(P1, P2),
    max(P1W, P2W).

