-module(day4).

-export([part1/1, part2/1]).

check_win([]) ->
    false;
check_win([Line|Rest]) ->
    Result = lists:all(fun(A) -> lists:nth(2, A) end, Line),
    if Result == true -> true;
        true -> check_win(Rest)
    end.

parse_move([No, true], _) -> [No, true];
parse_move([No, false], No) -> [No, true];
parse_move([No, false], _) -> [No, false].

parse(Moves, Board) ->
    parse_moves(Moves, Board, 1).
parse_moves([], _, NoMoves) ->
    [NoMoves, 0];
parse_moves([Curr|Moves], B, NoMoves) ->
    Board = [[ parse_move(Entry, Curr) || Entry <- Line] || Line <- B],
    Victory = check_win(Board),
    if Victory == true -> [[NoMoves, Curr * lists:sum(lists:flatten([ [ No || [No, false] <- Line] || Line <- Board]))]];
        true -> 
            TVictory = check_win(util:transpose(Board)),
            if TVictory == true -> [[NoMoves, Curr * lists:sum(lists:flatten([ [ No || [No, false] <- Line] || Line <- Board]))]];
                true -> parse_moves(Moves, Board, NoMoves + 1)
            end
    end.

parse_boards(Boards, Moves) ->
    parse_boards(Boards, Moves, []).
parse_boards([], _, Scores) -> Scores;
parse_boards([L1, L2, L3, L4, L5| Rest], Moves, Scores) ->
    parse_boards(Rest, Moves, Scores ++ parse(Moves, [L1, L2, L3, L4, L5])).


part1([M|B]) ->
    Moves = [ list_to_integer(Token) || Token <- string:tokens(M, ",")],
    Boards = [[[list_to_integer(Token), false] || Token <- string:tokens(Line, " ")] || Line <- B], 
    Scores = parse_boards(Boards, Moves),
    tl(hd(lists:sort(fun([MoveA, _], [MoveB, _]) -> MoveA < MoveB end, Scores))).

part2([M|B]) ->
    Moves = [ list_to_integer(Token) || Token <- string:tokens(M, ",")],
    Boards = [[[list_to_integer(Token), false] || Token <- string:tokens(Line, " ")] || Line <- B], 
    Scores = parse_boards(Boards, Moves),
    tl(hd(lists:sort(fun([MoveA, _], [MoveB, _]) -> MoveA > MoveB end, Scores))).

