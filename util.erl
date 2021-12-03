-module(util).

-export([parse/2, count/2]).

parse({Value, Type})->
    F = list_to_atom("list_to_" ++ atom_to_list(Type)),
    erlang:F(Value).
parse(Lines, Types) when is_list(hd(Lines))->
    [parse(Line, Types) || Line <- Lines];
parse(Line, Types) ->
    parse(Line, Types, " ").
parse(Line, Types, Split) -> 
    [parse(Pair) || Pair <- lists:zip(string:tokens(Line, Split), Types)].


count(Sub, String) -> erlang:length(string:split(String, Sub, all)) - 1.