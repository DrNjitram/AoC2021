-module(day1).

-export([part1/1, part2/1]).

fuel_required(Mass) ->
    trunc(Mass / 3) - 2.

part1(Lines) ->
    lists:sum([ fuel_required(list_to_integer(Fuel)) || Fuel <- Lines ]).

all_fuel_required(Fuel, Sum) ->
    Req = fuel_required(Fuel),
    if
        Req =< 0 ->
            Sum;
        Req > 0 ->
            all_fuel_required(Req, Sum + Req)
    end.

both(Line) ->
    Req = fuel_required(list_to_integer(Line)),
    Req + all_fuel_required(Req, 0).

part2(Lines) ->
    lists:sum([ both(Line) || Line <- Lines ]).
