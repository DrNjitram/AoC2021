-module(day3).

-export([part1/1, part2/1]).

count(Sub, String) -> erlang:length( binary:split(binary:list_to_bin(String), binary:list_to_bin(Sub), [global]) ) - 1.

convert(Value) ->
    list_to_integer(Value, 2).

get_counts(Line) ->
    C1 = count("1", Line),
    C0 = count("0", Line),
    if C1 > C0 -> "1";
        true -> "0"
    end.

get_positional(List) ->
    get_positional(List, []).
get_positional(List, Acc) ->
    if hd(List) == [] ->
        Acc;
        true ->
            Curr = lists:foldl(fun(B, A) -> A ++ [hd(B)] end, "", List),
            Ts = lists:foldl(fun(B, A) -> A ++ [tl(B)] end, [], List),
            get_positional(Ts, Acc ++ [Curr])
    end.


part1(Lines) ->
    Positionals = get_positional(Lines),
	Gamma = convert(lists:flatten(lists:map(fun get_counts/1, Positionals))),
    Epsilon = trunc(math:pow(2, length(hd(Lines)))) - 1 - Gamma,
    Gamma * Epsilon.

part2(Lines) ->
    ok.


