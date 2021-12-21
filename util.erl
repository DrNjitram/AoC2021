-module(util).

-export([parse/2, parse/3, 
    count/2, transpose/1, 
    count_list/2, to_hashmap/1, 
    print_map/2, get_adjecents_positions/3, 
    get_adjecents/4, get_adjecents/3, 
    last/1, parse_edges/1, print_set/1, print_set/2,
    print_binary_map/2, print_binary_map/1, print_map/1,
    count_overlapping/2, get_extend/1, insert/2,
    print_map_path/2
]).


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

parse_edges(Edges) -> parse_edges(Edges, #{}).
parse_edges([], Map) -> Map;
parse_edges([Edge|Rest], Map) ->
    [S, E] = string:tokens(Edge, "-"),
    NewMap = Map#{ S => maps:get(S, Map, []) ++ [E]},
    NewerMap = NewMap#{ E => maps:get(E, NewMap, []) ++ [S]},
    parse_edges(Rest, NewerMap).

count_list(Sub, List) -> length([ X || X <- List, X == Sub]).
count(Sub, List) when is_integer(Sub) -> length([ X || X <- List, X == Sub]);
count(Sub, String) when is_atom(Sub)-> erlang:length(string:split(String, atom_to_list(Sub), all)) - 1;
count(Sub, String) -> erlang:length(string:split(String, Sub, all)) - 1.
count_overlapping(Sub, String) -> count_overlapping(Sub, String, 0).
count_overlapping(_, [], Acc) -> Acc;
count_overlapping(Sub, [First|Rest], Acc) -> 
    case string:prefix([First|Rest], Sub) of
        nomatch -> count_overlapping(Sub, Rest, Acc);
        _ -> count_overlapping(Sub, Rest, Acc + 1)
    end.


transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].


get_adjecents_positions({X0, Y0}, Map, adj8) -> [{X0 + X, Y0 + Y} || {X, Y} <- ?ADJ8, maps:find({X0 + X, Y0 + Y}, Map) /= error];
get_adjecents_positions({X0, Y0}, Map, adj4) -> [{X0 + X, Y0 + Y} || {X, Y} <- ?ADJ4, maps:find({X0 + X, Y0 + Y}, Map) /= error].


get_adjecents({X0, Y0}, Map, Default, adj8) -> [maps:get({X0 + X, Y0 + Y}, Map, Default) || {X, Y} <- ?ADJ8];
get_adjecents({X0, Y0}, Map, Default, adj4) -> [maps:get({X0 + X, Y0 + Y}, Map, Default) || {X, Y} <- ?ADJ4].
get_adjecents({X0, Y0}, Map, adj8) -> [maps:get({X0 + X, Y0 + Y}, Map) || {X, Y} <- ?ADJ8, maps:find({X0 + X, Y0 + Y}, Map) /= error];
get_adjecents({X0, Y0}, Map, adj4) -> [maps:get({X0 + X, Y0 + Y}, Map) || {X, Y} <- ?ADJ4, maps:find({X0 + X, Y0 + Y}, Map) /= error]. 


print_set(_, Set) -> print_set(Set).
print_set(Set) ->
    io:format("~p~n", [sets:to_list(Set)]).  

get_extend(Map) ->
    Positions = maps:keys(Map),
    Xs = lists:map(fun ({X, _}) -> X end, Positions),
    Ys = lists:map(fun ({_, Y}) -> Y end, Positions),
    {lists:min(Xs), lists:min(Ys), lists:max(Xs), lists:max(Ys)}.

print_get_char(0) -> $.;
print_get_char(1) -> $#.
print_get_char(Pos, Path, Map) -> 
    Present = lists:member(Pos, Path),
    if Present -> $#;
        not Present -> $0 + maps:get(Pos, Map)
    end.

print_map(Map) -> print_map(Map, get_extend(Map)). 
print_binary_map(Map) -> print_binary_map(Map, get_extend(Map)). 
print_binary_map(Map, {MinX, MinY, MaxX, MaxY}) ->
    io:format("~s~n",[[[ print_get_char( maps:get({X, Y}, Map, 0)) || X <- lists:seq(MinX, MaxX) ] ++ "\n" || Y <- lists:seq(MinY, MaxY) ]]).
print_map(Map, {MinX, MinY, MaxX, MaxY}) ->
    io:format("~s~n",[[[ $0 + maps:get({X, Y}, Map, 0) || X <- lists:seq(MinX, MaxX) ] ++ "\n" || Y <- lists:seq(MinY, MaxY) ]]).
print_map_path(Map, Path) -> print_map_path(Map, get_extend(Map), Path). 
print_map_path(Map, {MinX, MinY, MaxX, MaxY}, Path) ->
    io:format("~s~n",[[[ print_get_char({X, Y}, Path, Map) || X <- lists:seq(MinX, MaxX) ] ++ "\n" || Y <- lists:seq(MinY, MaxY) ]]).

last(L) -> lists:last(L).

insert(Elem, []) -> [Elem];
insert(Elem, List) -> insert(Elem, List, []).
insert({V, W}, [{V1, W1}], []) when W < W1 -> [{V, W}, {V1, W1}];
insert({V, W}, [{V1, W1}], []) -> [{V1, W1}, {V, W}];
insert(Elem, [Last], Acc) ->  Acc ++ [Last, Elem];
insert({V, W}, [{V1, W1}, {V2, W2}], Acc) when W < W1 -> Acc ++ [{V, W}, {V1, W1} , {V2, W2}];
insert({V, W}, [{V1, W1}|T], Acc) -> insert({V, W}, T, Acc ++ [{V1, W1}]).