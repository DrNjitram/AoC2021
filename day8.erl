-module(day8).

-export([part1/1, part2/1]).

get_numbers() -> #{
    0 => sets:from_list([a, b, c, d, e, f, g]),
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

% be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb

get_valid(Line) ->
    [_, T] =  string:tokens(Line, "|"),
    L = string:tokens(T, " "),
    length([X || X <- L, lists:member(length(X), [2, 3, 4, 7])]).

part1(Lines) -> 
    lists:sum([get_valid(Line) || Line <- Lines]).


update_map([], _, Map) -> Map;
update_map([Char|RChars], Values, Map) ->
    Current_Poss = maps:get(Char, Map),
    LCurr = sets:size(Current_Poss),
    io:format("~p ~n", [Char]),
    print_set(Values),
    print_set(Current_Poss),
    
    if LCurr == 1 -> update_map(RChars, Values, Map);
        true -> 
            Updated_Poss = sets:intersection(Current_Poss, Values),
            print_set(Updated_Poss),       
            update_map(RChars, Values, Map#{Char := Updated_Poss})
    end.

print_set(_, Set) -> print_set(Set).
print_set(Set) ->
    io:format("~p~n", [sets:to_list(Set)]).  

remove_entries(Map, Key, Values) ->
    Map#{ Key := sets:subtract(maps:get(Key, Map), Values)}.
add_entries(Map, Key, Values) ->
    Map#{ Key := sets:intersection(maps:get(Key, Map), Values)}.

apply_encoding(Entries) -> apply_encoding(Entries, get_empty_map()).
apply_encoding([], Map) -> Map;
apply_encoding([Entry|Rest], Map) ->
    PossibleValues = maps:get(length(Entry), get_unions()),
    NotFound = [ C || C <- [$a, $b, $c, $d, $e, $f, $g], util:count(C, Entry) == 0],
    io:format("~p~n", [NotFound]),
    NewMap = lists:foldl(fun(Curr, Acc) -> remove_entries(Acc, Curr, PossibleValues) end, Map, NotFound),
    NewerMap = lists:foldl(fun(Curr, Acc) -> add_entries(Acc, Curr, PossibleValues) end, NewMap, Entry),
    maps:map(fun print_set/2, NewerMap),
    io:format("New Word ~p ~n", [Entry]),
    apply_encoding(Rest, NewerMap).


get_output(Line) ->
    [Encoding|Output] = [string:tokens(Token, " ") || Token <- string:tokens(Line, "|")],
    io:format("~p~n", [Encoding]),
    Wires = apply_encoding(Encoding).

part2(Lines) ->
    get_output("dab ab | cdfeb fcadb cdfeb cdbaf").
    %[get_output(Line) || Line <- Lines]. cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab