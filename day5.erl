-module(day5).

-export([part1/1, part2/1]).

apply_position([], Map) ->
    Map;
apply_position([Key|Rest], Map) ->
    case maps:find(Key, Map) of
        {ok, Val} -> apply_position(Rest, Map#{Key := Val + 1});
        error -> apply_position(Rest, Map#{Key => 1}) 
    end.

seq(A, B) when A > B ->
    lists:seq(A, B, -1);
seq(A, B) ->
    lists:seq(A, B).

plot_pipes(Pipes) ->
    plot_pipes(Pipes, #{}).
plot_pipes([], Map) ->
    Map;
plot_pipes([[{X1, Y1}, {X2, Y2}]|Rest], Map) when X1 == X2; Y1 == Y2 ->
    Positions = [ {X, Y} || X <- seq(X1, X2), Y <- seq(Y1, Y2)],
    plot_pipes(Rest, apply_position(Positions, Map));
plot_pipes([_|Pipes], Map) ->
    plot_pipes(Pipes, Map).


plot_pipes2(Pipes) ->
    plot_pipes2(Pipes, #{}).
plot_pipes2([], Map) ->
    Map;
plot_pipes2([[{X1, Y1}, {X2, Y2}]|Rest], Map) when X1 == X2; Y1 == Y2 ->
    Positions = [ {X, Y} || X <- seq(X1, X2), Y <- seq(Y1, Y2)],
    plot_pipes2(Rest, apply_position(Positions, Map));
plot_pipes2([[{X1, Y1}, {X2, Y2}]|Rest], Map) ->
    Positions = [ {X, Y} || {X, Y} <- lists:zip(seq(X1, X2), seq(Y1, Y2))],
    plot_pipes2(Rest, apply_position(Positions, Map)).


parse_pipes(Lines) ->
    [[ list_to_tuple(util:parse(Token, [integer, integer], ",")) || Token <- string:tokens(Line, " -> ")] ||Line <- Lines].

part1(Lines) ->
    maps:size(maps:filter(fun(_, V) -> V>1 end, plot_pipes(parse_pipes(Lines)))).

part2(Lines) ->
    maps:size(maps:filter(fun(_, V) -> V>1 end, plot_pipes2(parse_pipes(Lines)))).