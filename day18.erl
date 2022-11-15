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


explode(Number) ->
    Tokens = string:tokens(Number, ","),
    Split = length(Tokens) - find_subunits(Tokens, -5), 
    io:format("~p ~p ~p\n", [Number, Split, Tokens]),
    string:join(lists:sublist(Tokens, max(Split-1, 1), 4), ",").


split(Number, Split_RE) -> 
    {match, [[_, {S, L}, _]]} = re:run(Number, Split_RE, [global]),
    String = string:slice(Number, S, L),
    Value = util:parse({String, integer}),
    Section = string:replace(Number, Value, "[" + trunc(Value/2) + "," + ceil(Value/2) + "]"),
    .


check(Number) -> explode(Number).

add(N1, N2) -> check("[" ++ N2 ++ "," ++ N1 ++ "]").


part1(Lines) ->
    Result = lists:foldl(fun add/2, hd(Lines), tl(Lines)),
    io:format("~p\n", [Result]),
    magnitude(convert(hd(Lines))).
