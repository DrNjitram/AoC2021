-module(day14).

-export([part1/1, part2/1]).

get_pairs(Template) -> get_pairs(Template, #{}).
get_pairs([_], Map) -> Map;
get_pairs([E1, E2|Rest], Map) -> get_pairs([E2|Rest], Map#{ [E1,E2] => maps:get([E1,E2], Map, 0) + 1}).

apply_pair([E1, E2], [Value], OldPairs, NewPairs) ->  
    Quant = maps:get([E1, E2], OldPairs),
    NewPairs#{ [Value,E2] => maps:get([Value,E2], NewPairs, 0) + Quant, [E1,Value] => maps:get([E1,Value], NewPairs, 0) + Quant}.

apply_rules(_, Pairs, 0) -> Pairs;
apply_rules(Rules, Pairs, Steps) -> 
    apply_rules(Rules, lists:foldl(fun(Elem, Accin) -> apply_pair(Elem, maps:get(Elem, Rules), Pairs, Accin) end, #{}, maps:keys(Pairs)), Steps - 1).
   
count_things(Pairs) -> count_things(Pairs, #{}).
count_things([], Acc) -> maps:map(fun(_, V) -> trunc(math:ceil(V/2)) end, Acc);
count_things([{[E1, E1], Count}|Pairs], Acc) -> count_things(Pairs, Acc#{ [E1] => maps:get([E1], Acc, 0) + 2 * Count});
count_things([{[E1, E2], Count}|Pairs], Acc) -> count_things(Pairs, Acc#{ [E2] => maps:get([E2], Acc, 0) + Count, [E1] => maps:get([E1], Acc, 0) + Count}).

solve(Template, Rules, Steps) ->
    Pairs = get_pairs(Template),
    RuleMap = maps:from_list([ {Pair, Insertion} || [Pair, Insertion] <- [string:tokens(Rule, " -> ") || Rule <- Rules]]),
    FinalPairs = apply_rules(RuleMap, Pairs, Steps),
    Counts = count_things(maps:to_list(FinalPairs)),
    lists:max(maps:values(Counts)) - lists:min(maps:values(Counts)).

part1([Template|Rules]) -> solve(Template, Rules, 10).
    
part2([Template|Rules]) -> solve(Template, Rules, 40).
