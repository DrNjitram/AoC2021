-module(aoc).

-export([run/1, time/1, run/2, time/2, run/3, time/3, test/1, download_input/1]).

days() -> lists:seq(1, 25).
all() -> 
    [ {D, P} || D <- days(), P <- lists:seq(1, 2) ].

run(all) -> run(all());
run([{D, P}|Rest]) ->
    io:format("Day ~b part ~b~n", [D, P]),
    io:format("~p~n", [run(D, P)]),
    run(Rest);
run([]) -> ok.

time(all) -> time(all());
time([{D, P}|Rest]) ->
    io:format("Day ~b part ~b~n", [D, P]),
    io:format("~p~n", [time(D, P)]),
    time(Rest);
time(Day) when is_integer(Day) -> time(Day, 1, []), time(Day, 2, []);
time([]) -> ok.

run(Day, Part) -> run(Day, Part, []).

%% @doc Runs day 'Day' part 'Part' for 5 seconds, then prints the average runtime.

time(Day, Part) -> time(Day, Part, []).

run(Day, Part, Args) -> run(Day, Part, Args, fun apply/3).
time(Day, Part, Args) -> 
    Start = erlang:system_time(millisecond),
    time(Day, Part, Args, Start, Start, []).
time(Day, Part, Args, Start, Cur, Acc) when Cur - Start < 5000 ->
    NewAcc = [run(Day, Part, Args, fun timer:tc/3)|Acc],
    time(Day, Part, Args, Start, erlang:system_time(millisecond), NewAcc);
time(_, _, _, _, _, Times) ->
    Avg = lists:foldl(fun ({X, _}, Sum) -> X + Sum end, 0, Times) / length(Times),
    % in theory you could put stdev here too. in practice, it doesn't work because of the short times. the min ends up as 0 and the max ends up as 0.016 for example.
    io:format("Average runtime ~.2fms in ~b runs~n", [Avg, length(Times)]),
    [{_,Result}|_] = Times,
    Result.

run(Day, Part, Args, F) ->
    DayS = "day" ++ integer_to_list(Day),
    Module = list_to_atom(DayS),
    Function = list_to_atom("part" ++ integer_to_list(Part)),
    case file:read_file(DayS ++ ".txt") of
        {ok, Data} -> 
            BinaryLines = string:split(Data, ["\n"], all),
            Lines = [ binary_to_list(X) || X <- BinaryLines, size(X) =/= 0],

            Main = "day" ++ integer_to_list(Day) ++ ".erl",
            {ok, MainModule} = compile:file(Main),
            code:purge(MainModule),
            code:load_file(MainModule),
            case F(Module, Function, [Lines] ++ Args) of
                {print, String} ->
                    io:format("~s", [String]),
                    ok;
                Output -> Output
            end;
        {error,enoent} -> erlang:exit("Missing file: " ++ DayS ++ ".txt")
    end.
    

test(all) -> test(days());
test([Day|Rest]) ->
    test(Day),
    test(Rest);
test([]) -> ok;
test(Day) ->
    Main = "day" ++ integer_to_list(Day) ++ ".erl",
    {ok, MainModule} = compile:file(Main),
    code:purge(MainModule),
    code:load_file(MainModule),
    Test = "day" ++ integer_to_list(Day) ++ "_test.erl",
    case filelib:is_file(Test) of
        false ->
            io:format("Day ~p has no tests~n", [Day]);
        true ->
            {ok, TestModule} = compile:file(Test),
            code:purge(TestModule),
            code:load_file(TestModule),
            eunit:test(TestModule)
    end.

download_input(Day) ->
    {ok, SessionID} = file:read_file(".session"),
    DayS = integer_to_list(Day),
    ReqURL = "https://adventofcode.com/2021/day/" ++ DayS ++ "/input",
    OutputFilename = "day" ++ DayS ++ ".txt",
    ok = inets:start(),
    ok = ssl:start(),
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {ReqURL, [{"Cookie", "session=" ++ binary_to_list(SessionID)}]}, [], []),
    ok = file:write_file(OutputFilename, Body),
    ok = inets:stop(),
    ok = ssl:stop().
