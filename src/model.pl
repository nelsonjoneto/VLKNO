
:- use_module(library(lists)).

% Example initial_state/2 predicate
initial_state(GameConfig, GameState) :-
    % Initialize the board and place pawns
    initial_board(Board),
    max_stack_size(Board, MaxStackSize),
    GameState = [player1, Board, (5,1), (1,5), (1,1), (5,5), MaxStackSize, 1, (-1, -1) | GameConfig].

print_game_state(GameState) :-
    format('GameState: ~w~n', [GameState]).
% Example initial_board/1 predicate
initial_board(Board) :-
    Board = [
        [1,1,1,1,1],
        [1,1,1,1,1],
        [1,1,1,1,1],
        [1,1,1,1,1],
        [1,1,1,1,1]
    ].

% Custom implementation of flatten/2
flatten([], []).
flatten([Head|Tail], FlatList) :-
    flatten(Head, FlatHead),
    flatten(Tail, FlatTail),
    append(FlatHead, FlatTail, FlatList).
flatten(Element, [Element]) :-
    \+ is_list(Element).

max_stack_size(Board, MaxStackSize) :-
    flatten(Board, FlatBoard),
    max_member(MaxStack, FlatBoard),
    number_of_digits(MaxStack, MaxStackSize).

% Calculate the number of digits in a number
number_of_digits(0,1).
number_of_digits(Number, Digits) :-
    Number > 0,
    Digits is floor(log(Number) / log(10)) + 1.