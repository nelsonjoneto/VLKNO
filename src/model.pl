
:- use_module(library(lists)).

% Example initial_state/2 predicate
initial_state([Player1Type, Player2Type, Player1Name, Player2Name, BoardSize], GameState) :-
    % Initialize the board and place pawns
    initial_board(BoardSize, Board),
    P1C1 = (BoardSize,1),
    P1C2 = (1,BoardSize),
    P2C2 = (BoardSize,BoardSize),
    GameState = [player1, Board, P1C1, P1C2, (1,1), P2C2, Player1Type, Player2Type, Player1Name, Player2Name].

print_game_state(GameState) :-
    format('GameState: ~w~n', [GameState]).

% Initialize the board with a given size
initial_board(BoardSize, Board) :-
    length(Row, BoardSize),
    maplist(=(1), Row),
    length(Board, BoardSize),
    maplist(=(Row), Board).

%valid_moves_pieces(player1, [[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]], (5,1), (1,5), (1,1), (5,5), ListOfMoves).
%valid_moves_pieces(player1, [[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,10,10],[1,1,1,10,1]], (5,1), (1,5), (1,1), (5,5), ListOfMoves).
%valid_moves([player1, [[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]], (5,1), (1,5), (1,1), (5,5), 1], ListOfMoves).
%dfs([[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]], (4,4), (5,1), (1,5), (1,1), (4,4), ReachablePositions).
%value([player1, [[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]], (5,1), (1,5), (1,1), (5,5) | _], player1, Value).
%value([player2, [[1,0,2,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]], (5,2), (1,5), (1,1), (4,4) | _], player1, Value).
%(5,1),(5,2),(2,5),3,5
%choose_move([player1, [[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]], (5,1), (1,5), (1,1), (5,5), computer, computer, '1', '2'], 2, Move).


% Helper predicate to flatten a nested list
flatten([], []).
flatten([L|Ls], FlatL) :-
    !,
    flatten(L, NewL),
    flatten(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten(L, [L]).

max_stack_size(Board, MaxStackSize) :-
    flatten(Board, FlatBoard),
    max_member(MaxStack, FlatBoard),
    number_of_digits(MaxStack, MaxStackSize).

% Calculate the number of digits in a number
number_of_digits(0, 1) :- !.
number_of_digits(Number, Digits) :-
    Number > 0,
    number_of_digits_recursive(Number, Digits).

number_of_digits_recursive(0, 0) :- !.
number_of_digits_recursive(Number, Digits) :-
    Number > 0,
    NextNumber is Number // 10,
    number_of_digits_recursive(NextNumber, NextDigits),
    Digits is NextDigits + 1.