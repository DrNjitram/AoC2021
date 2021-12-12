-module(day10).

-export([part1/1, part2/1]).

-define(SCORES1, #{ $) => 3,$] => 57, $} => 1197, $> => 25137}).
-define(SCORES2, #{ $( => 1,$[ => 2, ${ => 3, $< => 4}).
-define(MATCH, #{ $( => $), $[ => $], ${ => $}, $< => $>}).

determine_valid(Line) -> determine_valid(Line, []).
determine_valid([], Acc) -> Acc;
determine_valid([Char|Rest], Acc) ->
    Open = lists:member(Char, [$[, ${, $<, $(]),
    if Open -> determine_valid(Rest, Acc ++ [Char]);
        true -> 
            Valid = Char == maps:get(lists:last(Acc), ?MATCH),
            if Valid -> 
                determine_valid(Rest, util:last(Acc));
                true -> maps:get(Char, ?SCORES1)
            end
    end.

calculate_score(Line) -> calculate_score(Line, 0).
calculate_score([], Acc) -> Acc;
calculate_score([Char|Rest], Acc) -> calculate_score(Rest, Acc * 5 + maps:get(Char, ?SCORES2)).

parse_input(Lines) -> parse_input(Lines, []).
parse_input([], Acc) -> Acc;
parse_input([Line|Rest], Acc) -> parse_input(Rest, Acc ++ [determine_valid(Line)]).

part1(Lines) ->
    lists:sum([ V || V <- parse_input(Lines), is_integer(V)]).

part2(Lines) ->
    Remaining = [ lists:reverse(V) || V <-parse_input(Lines), is_list(V)],
    Scores =  lists:sort([calculate_score(L) || L <- Remaining]),
    lists:nth(length(Scores) div 2 + 1, Scores).