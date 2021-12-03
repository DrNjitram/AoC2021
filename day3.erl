-module(day3).

-export([part1/1, part2/1]).

count(Sub, String) -> erlang:length( binary:split(binary:list_to_bin(String), binary:list_to_bin(Sub), [global]) ) - 1.

convert(Value) ->
    list_to_integer(Value, 2).

get_counts1(Line) ->
    get_counts(Line, "1", "0").
get_counts0(Line) ->
    get_counts(Line, "0", "1").

get_counts(Line, R1, R2) ->
    C1 = count("1", Line),
    C0 = count("0", Line),
    if C1 >= C0 -> R1;
        true -> R2
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


get_subset(L, Positionals, Pos, F) ->
    Most = F(lists:nth(Pos, Positionals)), 
    [X || X <- L, [lists:nth(Pos, X)] == Most].

build_common_string([Loser], _ ,_) ->
    Loser;
build_common_string(L, Position, F) ->
    Pos = get_positional(L),
    SubL = get_subset(L, Pos, Position, F),
    build_common_string(SubL, Position + 1, F).

part1(Lines) ->
    Positionals = get_positional(Lines),
	Gamma = convert(lists:flatten(lists:map(fun get_counts1/1, Positionals))),
    Epsilon = trunc(math:pow(2, length(hd(Lines)))) - 1 - Gamma,
    Gamma * Epsilon.

part2(Lines) ->
    Most = build_common_string(Lines, 1, fun get_counts1/1),
    Least = build_common_string(Lines, 1, fun get_counts0/1),
    A = convert(lists:flatten(Most)),
    B = convert(lists:flatten(Least)),
    A * B.
