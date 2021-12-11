-module(util).

-export([parse/2, parse/3, count/2, transpose/1, count_list/2, to_hashmap/1, print_map/2, get_adjecents_positions/3, get_adjecents/4, get_adjecents/3]).


-define(ADJ8, [{0, -1}, {0, 1}, {1, 0}, {-1, 0}, {-1, -1}, {1, 1}, {1, -1}, {-1, 1}]).
-define(ADJ4, [{0, -1}, {0, 1}, {1, 0}, {-1, 0}]).

parse({Value, Type}) ->
    F = list_to_atom("list_to_" ++ atom_to_list(Type)),
    erlang:F(Value).
parse(Lines, Types) when is_list(hd(Lines))->
    [parse(Line, Types) || Line <- Lines];
parse(Line, Types) ->
    parse(Line, Types, " ").
parse(Line, Types, Split) -> 
    [parse(Pair) || Pair <- lists:zip(string:tokens(Line, Split), Types)].

to_hashmap(Lines) ->
    LineY = lists:zip(Lines, lists:seq(0, length(Lines)-1)),
    CharXY = lists:flatmap(fun ({Line, Y}) ->
        CharX = lists:zip(Line, lists:seq(0, length(Line)-1)),
        [ {{X, Y}, C - $0} || {C, X} <- CharX ]
    end, LineY),
    maps:from_list(CharXY).

count_list(Sub, List) -> length([ X || X <- List, X == Sub]).
count(Sub, List) when is_integer(Sub) -> length([ X || X <- List, X == Sub]);
count(Sub, String) when is_atom(Sub)-> erlang:length(string:split(String, atom_to_list(Sub), all)) - 1;
count(Sub, String) -> erlang:length(string:split(String, Sub, all)) - 1.

transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].


get_adjecents_positions({X0, Y0}, Map, adj8) -> [{X0 + X, Y0 + Y} || {X, Y} <- ?ADJ8, maps:find({X0 + X, Y0 + Y}, Map) /= error];
get_adjecents_positions({X0, Y0}, Map, adj4) -> [{X0 + X, Y0 + Y} || {X, Y} <- ?ADJ4, maps:find({X0 + X, Y0 + Y}, Map) /= error].


get_adjecents({X0, Y0}, Map, Default, adj8) -> [maps:get({X0 + X, Y0 + Y}, Map, Default) || {X, Y} <- ?ADJ8];
get_adjecents({X0, Y0}, Map, Default, adj4) -> [maps:get({X0 + X, Y0 + Y}, Map, Default) || {X, Y} <- ?ADJ4].
get_adjecents({X0, Y0}, Map, adj8) -> [maps:get({X0 + X, Y0 + Y}, Map) || {X, Y} <- ?ADJ8, maps:find({X0 + X, Y0 + Y}, Map) /= error];
get_adjecents({X0, Y0}, Map, adj4) -> [maps:get({X0 + X, Y0 + Y}, Map) || {X, Y} <- ?ADJ4, maps:find({X0 + X, Y0 + Y}, Map) /= error]. 


print_map(Map, {MinX, MinY, MaxX, MaxY}) ->
    [[ $0 + maps:get({X, Y}, Map, 0) || X <- lists:seq(MinX, MaxX) ] ++ "\n" || Y <- lists:seq(MinY, MaxY) ].
