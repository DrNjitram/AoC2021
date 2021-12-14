-module(day14).

-export([part1/1, part2/1]).

apply_rules(Template, Rules, Steps) -> apply_rules(Template, Rules, Steps -1, ""). 
apply_rules([Last], _, 0, Acc) -> Acc ++ [Last];
apply_rules([Last], Rules, Steps, Acc) -> io:format("~p~n", [Steps]), apply_rules(Acc ++ [Last], Rules, Steps - 1, "");
apply_rules([E1, E2|Rest], Rules, Steps, Acc) ->
    apply_rules([E2] ++ Rest, Rules, Steps, lists:flatten(Acc ++ [E1,maps:get([E1,E2], Rules, "")])).


get_pairs(Template) -> get_pairs(Template, #{}).
get_pairs([_], Map) -> Map;
get_pairs([E1, E2|Rest], Map) -> get_pairs([E2] ++ Rest, Map#{ [E1,E2] => maps:get([E1,E2], Map, 0) + 1}).

get_answer(Template, Rules, Steps) ->
    ParsedRules = [string:tokens(Rule, " -> ") || Rule <- Rules],   
    RuleMap = maps:from_list([ {Pair, Insertion} || [Pair, Insertion] <- ParsedRules]),
    FinalTemplate = apply_rules(Template, RuleMap, Steps),
    Uniques = sets:from_list(maps:values(RuleMap)),
    Count = [  util:count(U, FinalTemplate) || U <- sets:to_list(Uniques)],
    lists:max(Count) - lists:min(Count).

part1([Template|Rules]) ->
    get_answer(Template, Rules, 10).


apply_pair([E1, E2], Value, Pairs) ->  
    Pair1 = [E1, Value],
    Pair2 = [Value, E2],
    NewPairs = Pairs#{ Pair1 => maps:get(Pair1, Pairs, 0) + 1},
    NewPairs#{ Pair2 => maps:get(Pair2, NewPairs, 0) + 1}.


apply_logic(Pair, Pairs) ->
    Find = maps:find(Pair),
    case Find of 
        error -> Pairs;
        {ok, Value} ->  apply_pair(Pair, Value, Pairs)
    end.

apply_rules(_, Pairs, 0) -> Pairs;
apply_rules(Rules, Pairs, Steps) ->
    Keys = maps:keys(Pairs),
    lists:foldl(fun(Elem, Accin) -> apply_logic(Elem, Accin) end, Pairs, Keys).
   

part2([Template|Rules])  ->
    Pairs = get_pairs(Template),

    get_answer(Template, Rules, 40).
