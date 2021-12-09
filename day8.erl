-module(day8).

-export([part1/1, part2/1]).

get_numbers() -> #{
    0 => sets:from_list([a, b, c, e, f, g]),
    1 => sets:from_list([c, f]),
    2 => sets:from_list([a, c, d, e, g]),
    3 => sets:from_list([a, c, d, f, g]),
    4 => sets:from_list([b, c, d, f]),
    5 => sets:from_list([a, b, d, f, g]),
    6 => sets:from_list([a, b, d, e, f, g]),
    7 => sets:from_list([a, c, f]),
    8 => sets:from_list([a, b, c, d, e, f, g]),
    9 => sets:from_list([a, b, c, d, f, g])
}.

get_lengths() -> #{
    2 => [1],
    3 => [7],
    4 => [4],
    5 => [2, 3, 5],
    6 => [0, 6, 9],
    7 => [8]
}.

get_unions() -> #{
    2 => sets:from_list([c, f]),
    3 => sets:from_list([a, c, f]),
    4 => sets:from_list([b, c, d, f]),
    5 => sets:from_list([a, b, c, d, e, f, g]),
    6 => sets:from_list([a, b, c, d, e, f, g]),
    7 => sets:from_list([a, b, c, d, e, f, g])
    }.

get_empty_map() -> #{
    $a => sets:from_list([a, b, c, d, e, f, g]),
    $b => sets:from_list([a, b, c, d, e, f, g]), 
    $c => sets:from_list([a, b, c, d, e, f, g]), 
    $d => sets:from_list([a, b, c, d, e, f, g]), 
    $e => sets:from_list([a, b, c, d, e, f, g]), 
    $f => sets:from_list([a, b, c, d, e, f, g]), 
    $g => sets:from_list([a, b, c, d, e, f, g])
}.

get_valid(Line) ->
    [_, T] =  string:tokens(Line, "|"),
    L = string:tokens(T, " "),
    length([X || X <- L, lists:member(length(X), [2, 3, 4, 7])]).

part1(Lines) -> 
    lists:sum([get_valid(Line) || Line <- Lines]).
remove_entries(Map, Key, Values) ->
    Map#{ Key := sets:subtract(maps:get(Key, Map), Values)}.
add_entries(Map, Key, Values) ->
    Map#{ Key := sets:intersection(maps:get(Key, Map), Values)}.

check_subsets(Duplicates, Number) ->
    length([Wires || Wires <- Duplicates, sets:is_subset(Wires, maps:get(Number, get_numbers())) ]).

check_number(Number, Map, Sequence) ->
    RequiredWires = maps:get(Number, get_numbers()),
    TouchedWires = sets:union([ V || {K, V} <- maps:to_list(Map), util:count(K, Sequence) == 1]),
    sets:is_subset(RequiredWires, TouchedWires).

get_values(Map, Values, Sequence) ->
    Duplicates = [ V || {K, V} <- maps:to_list(Map), util:count_list(V, maps:values(Map)) > 1, util:count(K, Sequence) == 0],
    [Number || Number <- Values, check_subsets(Duplicates, Number) == 0, check_number(Number, Map, Sequence)].

eliminate(Map, CorrectValues, Entry) ->
    TouchedWires = sets:union([maps:get(CorrectValue, get_numbers()) || CorrectValue <- CorrectValues]),
    NotFound = [ C || C <- [$a, $b, $c, $d, $e, $f, $g], util:count(C, Entry) == 0],
    NewMap = lists:foldl(fun(Curr, Acc) -> remove_entries(Acc, Curr, TouchedWires) end, Map, NotFound),
    lists:foldl(fun(Curr, Acc) -> add_entries(Acc, Curr, TouchedWires) end, NewMap, Entry).

check_done(Map) ->
    length([ V || V <-maps:values(Map), sets:size(V) > 1]) == 0.

apply_encoding(Entries) -> apply_encoding(Entries, get_empty_map()).
apply_encoding([], Map) -> Map;
apply_encoding([Entry|Rest], Map) ->
    PossibleValues = maps:get(length(Entry), get_unions()),
    NotFound = [ C || C <- [$a, $b, $c, $d, $e, $f, $g], util:count(C, Entry) == 0],
    LPoss = sets:size(PossibleValues),
    PossNumbers = maps:get(length(Entry), get_lengths()),
    if LPoss == 7 -> 

            CorrectValues = get_values(Map, PossNumbers, Entry),
            NewerMap = eliminate(Map, CorrectValues, Entry);
        true ->
            NewMap = lists:foldl(fun(Curr, Acc) -> remove_entries(Acc, Curr, PossibleValues) end, Map, NotFound),
            NewerMap = lists:foldl(fun(Curr, Acc) -> add_entries(Acc, Curr, PossibleValues) end, NewMap, Entry)
    end,
    

    Done = check_done(NewerMap),
    if Done -> NewerMap;
        true -> apply_encoding(Rest, NewerMap)
    end.


apply_result(Wires, [Output]) -> apply_result(Wires, Output, []).
apply_result(_, [], Acc) -> Acc;

apply_result(Wires, [Segment|Rest], Acc) ->
    TouchedWires = sets:union([ V || {K, V} <- maps:to_list(Wires), util:count(K, Segment) == 1]),
    Value = [ Num || {Num, RequiredWires} <- maps:to_list(get_numbers()), RequiredWires == TouchedWires],
    apply_result(Wires, Rest, Acc ++ Value).
    


to_number(List) -> to_number(List, 0).
to_number([], Acc) -> Acc;
to_number([N|Rest], Acc) ->
    to_number(Rest, Acc * 10 + N).


get_output(Line) ->
    [Encoding|Output] = [string:tokens(Token, " ") || Token <- string:tokens(Line, "|")],
    SortedEncoding = lists:sort(fun(A, B) -> length(A) < length(B) end, Encoding),
    Wires = apply_encoding(SortedEncoding),
    Result = apply_result(Wires, Output),
    to_number(Result).


part2(Lines) ->
    lists:sum([get_output(Line) || Line <- Lines]).