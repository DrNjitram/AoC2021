-module(day3).

-export([part1/1, part2/1]).

get_counts(Line, R1) ->
    C1 = util:count("1", Line),
    C0 = length(Line) - C1,
    if C1 >= C0 -> R1; true ->
        if R1 == "1" -> "0"; true -> "1" end
    end.

get_subset(L, Pos, Target) ->
    Most = get_counts([lists:nth(Pos, Line) || Line <- L], Target), 
    [X || X <- L, [lists:nth(Pos, X)] == Most].

build_common_string([Result], _ ,_) ->
    Result;
build_common_string(L, Position, Target) ->
    build_common_string(get_subset(L, Position, Target), Position + 1, Target).

part1(Lines) ->
    Positionals = util:transpose(Lines),
	Gamma = list_to_integer(lists:flatten([get_counts(Line, "1") || Line <- Positionals]), 2),
    Epsilon = trunc(math:pow(2, length(hd(Lines)))) - 1 - Gamma,
    Gamma * Epsilon.

part2(Lines) ->
    O2 = build_common_string(Lines, 1, "1"),
    CO2 = build_common_string(Lines, 1, "0"),
    list_to_integer(lists:flatten(O2), 2) * list_to_integer(lists:flatten(CO2), 2).
