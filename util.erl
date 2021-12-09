-module(util).

-export([parse/2, parse/3, count/2, transpose/1, count_list/2, to_hashmap/1]).

parse({Value, Type})->
    F = list_to_atom("list_to_" ++ atom_to_list(Type)),
    erlang:F(Value).
parse(Lines, Types) when is_list(hd(Lines))->
    [parse(Line, Types) || Line <- Lines];
parse(Line, Types) ->
    parse(Line, Types, " ").
parse(Line, Types, Split) -> 
    [parse(Pair) || Pair <- lists:zip(string:tokens(Line, Split), Types)].

get_char(X, Y, Map) -> list_to_integer([lists:nth(X, lists:nth(Y, Map))]).
to_hashmap(Lines) -> maps:from_list([ {{X, Y}, get_char(X, Y, Lines)} || X <- lists:seq(1, length(hd(Lines))), Y <- lists:seq(1, length(Lines))]).

count_list(Sub, List) -> length([ X || X <- List, X == Sub]).
count(Sub, List) when is_integer(Sub) -> length([ X || X <- List, X == Sub]);
count(Sub, String) when is_atom(Sub)-> erlang:length(string:split(String, atom_to_list(Sub), all)) - 1;
count(Sub, String) -> erlang:length(string:split(String, Sub, all)) - 1.

transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].