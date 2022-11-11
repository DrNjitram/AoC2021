-module(day17).

-export([part1_and_2/1]).

check_target({X, Y}, [Xmin, Xmax, Ymin, Ymax]) when Xmin =< X, X =< Xmax, Ymin =< Y, Y =< Ymax -> hit;
check_target({X, Y}, [_Xmin, Xmax, Ymin, _Ymax]) when Y < Ymin; X > Xmax -> overshot;
check_target(_Pos, _Target) -> continue.

decX(X) when X > 0 -> X - 1;
decX(0) -> 0.

step(_Pos, _Target, _Vel, overshot) -> overshot;
step(_Pos, _Target, _Vel, hit) -> hit;
step({X, Y}, Target, {Xvel, Yvel}, continue) -> step({X+Xvel, Y+Yvel}, Target, {decX(Xvel), Yvel-1}, check_target({X+Xvel, Y+Yvel}, Target)).

minXvel(Xmin) -> trunc((-1+math:sqrt(8*Xmin+1))/2).

loop({_X, Y}, _Xstart, _Xend, Yend, _Target, Acc) when Y < Yend -> Acc;
loop({X, Y}, Xstart, Xend, Yend, Target, Acc) when X > Xend -> loop({Xstart, Y-1}, Xstart, Xend, Yend, Target, Acc);
loop({X, Y}, Xstart, Xend, Yend, Target, Acc) -> 
    case step({0,0}, Target, {X, Y}, continue) of
        overshot -> loop({X+1, Y}, Xstart, Xend, Yend, Target, Acc);
        hit -> loop({X+1, Y}, Xstart, Xend, Yend, Target, Acc ++ [{X, Y}]) 
    end.     

part1_and_2([Line]) ->
    [Xmin, Xmax, Ymin, Ymax] = util:parse_regex(Line, "target area: x=([-0-9]+)..([-0-9]+), y=([-0-9]+)..([-0-9]+)", integer),
    StartY = abs(Ymax)*5,
    [{_, Y} | Hits] = loop({minXvel(Xmin), StartY}, minXvel(Xmin), Xmax, Ymin, [Xmin, Xmax, Ymin, Ymax], []),
    [length(Hits)+1, Y*((1+Y)/2)].
