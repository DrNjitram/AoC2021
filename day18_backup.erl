-module(day18).

-export([part1/1]).

find_subunits([H | T]) -> find_subunits(T, util:count("[", H) - util:count("]", H)).

find_subunits([], 0) -> 1;
find_subunits([_ | T], 0) -> length(T) + 1;
find_subunits([H | T], Acc) -> find_subunits(T, Acc + util:count("[", H) - util:count("]", H)).
    
convert(Line) when length(Line) == 1 -> list_to_integer(Line);
convert(Line) -> 
    Token = string:sub_string(Line, 2, length(Line)-1),
    Tokens = string:tokens(Token, ","),
    case length(Tokens) of
            1 -> list_to_integer(string:trim(hd(Tokens), both, "[]"));
            _ ->
                Split = length(Tokens) - find_subunits(Tokens),
                A = string:join(lists:sublist(Tokens, Split), ","),
                B = string:join(lists:sublist(Tokens, Split+1, length(Tokens)), ","),
                [convert(A), convert(B)]
    end.


magnitude([L, R]) when length(L) > 1, length(R) > 1 -> magnitude([magnitude(L), magnitude(R)]);
magnitude([L, R]) when length(L) > 1 -> magnitude([magnitude(L), R]);
magnitude([L, R]) when length(R) > 1 -> magnitude([L, magnitude(R)]);
magnitude([L, R]) -> L * 3 + R * 2;
magnitude([E]) -> magnitude(E).



combine(A, B) when is_integer(A), is_integer(B) -> A + B;
combine([A1, A2], B) -> [A1, combine(A2, B)];
combine(A, [B1, B2]) -> [combine(A, B1), B2].

check([[[0, R1, _E], R]]) ->[[[0, R1], R]];
check([[L, [L1, 0, _E]]]) -> [[L, [L1, 0]]].
check([L, 0, E], [R1, R2]) -> [[[L, 0], [combine(E, R1), R2]]]; % Previous left exploded
check([L1, L2], [0, R, E]) -> [[[L1, combine(L2, E)], [0, R]]]; % Previous right exploded
check(L, R) when is_list(R) -> [[L, R]];
check([L, R], D) when D == 3 -> 
    if is_integer(R) -> 
        if is_integer(L) -> check([L, R], 4); % Nothing explodes
                true -> [0, R + lists:nth(2, L), hd(L)] % Left Explodes 
        end;
        true -> [L + hd(R), 0, lists:nth(2, R)] % Right explodes   
    end;
check([L, R], D) -> check(check(L, D+1), check(R, D+1));
check(V, _D) when is_integer(V),V > 9 -> [trunc(V/2), ceil(V/2)];
check(V, _D) -> V.


add(L) when length(L) == 1 -> L;
add([N1, N2 | Rest]) -> 
    Add = check(check([N1]++[N2], 0)),
    io:format("~p ~p\n", [Add, [N1]++[N2]]),
    add([[N1]++[N2]] ++ Rest).


part1(Lines) ->
    Numbers = [ convert(Line) || Line <- Lines],
    Result = add(Numbers),
    magnitude(Result).
