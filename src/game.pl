:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- consult(view).
:- consult(model).
:- consult(io).
:- consult(computer).
:- consult(movement).
:- consult(utils).

/*
play/0
Description: The main predicate that gives access to the game menu, 
allows configuring the game type, player names, difficulty levels, and the board size, and starts the game cycle.
*/
play :-
    display_menu,
    game_configuration([Player1Type, Player2Type, Player1Name, Player2Name, BoardSize, Difficulty1-Difficulty2]),
    initial_state([Player1Type, Player2Type, Player1Name, Player2Name, BoardSize, Difficulty1-Difficulty2], GameState),
    write('Starting game...'), nl,
    format('Player 1: ~w (~w), Player 2: ~w (~w)~n~n', [Player1Name, Player1Type, Player2Name, Player2Type]),
    display_game(GameState),
    game_loop(Difficulty1-Difficulty2, GameState).

/*
game_loop(+Difficulty, +GameState)
Description: The main game loop that alternates turns between players, 
checks for game over conditions, and handles moves for both human and computer players.
*/
% Game ends, print winner and ask for replay
game_loop(_, GameState) :-
    game_over(GameState, Winner), !,
    print_winner_and_ask_replay(Winner).

% Human player's turn
game_loop(0-Difficulty2, GameState) :-
    choose_move(GameState, 0, Move),
    move(GameState, Move, NewGameState),
    display_game(NewGameState), !,
    game_loop(Difficulty2-0, NewGameState).
% Computer player's turn (it tells the user the move it made and waits for the user to press any key to continue)
game_loop(Difficulty1-Difficulty2, GameState) :-
    choose_move(GameState, Difficulty1, Move),
    read_any_key(Move),
    move(GameState, Move, NewGameState),
    display_game(NewGameState), !,
    game_loop(Difficulty2-Difficulty1, NewGameState).

/*
choose_move(+GameState, +Level, -Move)
Description: For human players, choose_move/3 prompts the user to select a valid piece and specify the valid direction of movement. 
It also requires input for the valid position of the stone to be moved and the valid target position where the stone should be placed.
For the random computer player, a random valid move is selected. For the greedy computer player, it chooses a move with the highest value associated.
An optimization was implemented for larger boards, where the greedy computer player only considers initially the value of the available piece moves,
and then only considers the value of the possible moves generated from the highest value piece moves. 
It greatly reduces the number of possible moves to consider in most cases.
*/

%random computer (player1)
choose_move([player1, Board, P1C1, P1C2, P2C1, P2C2, computer | _], 1, Move) :-
    valid_moves([player1, Board, P1C1, P1C2, P2C1, P2C2 | _], ListOfMoves),
    random_member(Move, ListOfMoves).

%greedy computer (player1) without optimization for larger boards
choose_move([player1, Board, P1C1, P1C2, P2C1, P2C2, computer | _], 2, Move) :-
    length(Board, BoardSize),
    BoardSize < 6,
    valid_moves([player1, Board, P1C1, P1C2, P2C1, P2C2 | _], ListOfMoves),
    setof(Value-Mv, NewState^(
        member(Mv, ListOfMoves),
        move([player1, Board, P1C1, P1C2, P2C1, P2C2, computer | _], Mv, NewState),
        value(NewState, player1, Value)
    ), [_V-Move | _]).
%greedy computer (player1) with optimization for larger boards
choose_move([player1, Board, P1C1, P1C2, P2C1, P2C2, computer | _], 2, Move) :-
    % Get all valid piece moves and their values
    valid_moves_pieces(player1, Board, P1C1, P1C2, P2C1, P2C2, ListOfMoves),
    setof(Value-Mv, NewState^(
        member(Mv, ListOfMoves),
        move_piece([player1, Board, P1C1, P1C2, P2C1, P2C2 | _], Mv, NewState),
        value(NewState, player1, Value)
    ), ValueMoves),
    % Get the highest value
    ValueMoves = [HighestValue-_|_],
    % Filter moves with the highest value
    include(=(HighestValue-_), ValueMoves, HighestValueMoves),
    % Get all valid stone moves for the highest value piece moves
    findall((Piece, Direction, OldStonePos, NewStonePos), (
        member(_-Mv, HighestValueMoves),
        Mv = (Piece, Direction),
        valid_moves_stones(Board, P1C1, P1C2, P2C1, P2C2, (Piece, Direction), StoneMoves),
        member((OldStonePos, NewStonePos), StoneMoves)
    ), AllMoves),
    setof(Value-Mv, NewState^(
        member(Mv, AllMoves),
        move([player1, Board, P1C1, P1C2, P2C1, P2C2 | _], Mv, NewState),
        value(NewState, player1, Value)
    ), [_V-Move | _]).

%random computer (player2)
choose_move([player2, Board, P1C1, P1C2, P2C1, P2C2, _, computer | _], 1, Move) :-
    valid_moves([player2, Board, P1C1, P1C2, P2C1, P2C2 | _], ListOfMoves),
    random_member(Move, ListOfMoves).

%greedy computer (player2) without optimization for larger boards
choose_move([player2, Board, P1C1, P1C2, P2C1, P2C2, _, computer | _], 2, Move) :-
    length(Board, BoardSize),
    BoardSize < 6,
    valid_moves([player2, Board, P1C1, P1C2, P2C1, P2C2 | _], ListOfMoves),
    setof(Value-Mv, NewState^(
        member(Mv, ListOfMoves),
        move([player2, Board, P1C1, P1C2, P2C1, P2C2, _, computer | _], Mv, NewState),
        value(NewState, player2, Value)
    ), [_V-Move | _]).
%greedy computer (player2) with optimization for larger boards
choose_move([player2, Board, P1C1, P1C2, P2C1, P2C2, _, computer | _], 2, Move) :-
    % Get all valid piece moves and their values
    valid_moves_pieces(player2, Board, P1C1, P1C2, P2C1, P2C2, ListOfMoves),
    setof(Value-Mv, NewState^(
        member(Mv, ListOfMoves),
        move_piece([player2, Board, P1C1, P1C2, P2C1, P2C2 | _], Mv, NewState),
        value(NewState, player2, Value)
    ), ValueMoves),
    % Get the highest value
    ValueMoves = [HighestValue-_|_],
    % Filter moves with the highest value
    include(=(HighestValue-_), ValueMoves, HighestValueMoves),
    % Get all valid stone moves for the highest value piece moves
    findall((Piece, Direction, OldStonePos, NewStonePos), (
        member(_-Mv, HighestValueMoves),
        Mv = (Piece, Direction),
        valid_moves_stones(Board, P1C1, P1C2, P2C1, P2C2, (Piece, Direction), StoneMoves),
        member((OldStonePos, NewStonePos), StoneMoves)
    ), AllMoves),
    setof(Value-Mv, NewState^(
        member(Mv, AllMoves),
        move([player2, Board, P1C1, P1C2, P2C1, P2C2 | _], Mv, NewState),
        value(NewState, player2, Value)
    ), [_V-Move | _]).

%human (player1)
choose_move([player1, Board, P1C1, P1C2, P2C1, P2C2, human | _], _, Move) :-
    choose_piece_movement(player1, Board, P1C1, P1C2, P2C1, P2C2, (Piece, Direction)),
    get_piece_position(Piece, P1C1, P1C2, P2C1, P2C2, OldPiecePos),
    get_new_piece_positon(OldPiecePos, Direction, NewPiecePos),
    choose_stone_movement(Board, P1C1, P1C2, P2C1, P2C2, NewPiecePos, (OldStonePos, NewStonePos)),
    Move = (Piece, Direction, OldStonePos, NewStonePos).

%human (player2)
choose_move([player2, Board, P1C1, P1C2, P2C1, P2C2, _, human | _], _, Move) :-
    choose_piece_movement(player2, Board, P1C1, P1C2, P2C1, P2C2, (Piece, Direction)),
    get_piece_position(Piece, P1C1, P1C2, P2C1, P2C2, OldPiecePos),
    get_new_piece_positon(OldPiecePos, Direction, NewPiecePos),
    choose_stone_movement(Board, P1C1, P1C2, P2C1, P2C2, NewPiecePos, (OldStonePos, NewStonePos)),
    Move = (Piece, Direction, OldStonePos, NewStonePos).

/*
move(+GameState, +Move, -NewGameState)
Description: Executes a move, receiving the current game state and the move to be executed, 
and returns the new game state after the move is executed. The board and the moved piece's position are updated.
*/

%player1 moves piece w1
move([player1, Board, P1C1, P1C2, P2C1, P2C2 | Rest], (w1, Direction, OldStonePos, NewStonePos), NewGameState) :-
    update_board(Board, OldStonePos, NewStonePos, NewBoard),
    get_new_piece_positon(P1C1, Direction, NewPiecePos),
    NewGameState = [player2, NewBoard, NewPiecePos, P1C2, P2C1, P2C2 | Rest].

%player1 moves piece w2
move([player1, Board, P1C1, P1C2, P2C1, P2C2 | Rest], (w2, Direction, OldStonePos, NewStonePos), NewGameState) :-
    update_board(Board, OldStonePos, NewStonePos, NewBoard),
    get_new_piece_positon(P1C2, Direction, NewPiecePos),
    NewGameState = [player2, NewBoard, P1C1, NewPiecePos, P2C1, P2C2 | Rest].

%player2 moves piece b1
move([player2, Board, P1C1, P1C2, P2C1, P2C2 | Rest], (b1, Direction, OldStonePos, NewStonePos), NewGameState) :-
    update_board(Board, OldStonePos, NewStonePos, NewBoard),
    get_new_piece_positon(P2C1, Direction, NewPiecePos),
    NewGameState = [player1, NewBoard, P1C1, P1C2, NewPiecePos, P2C2 | Rest].

%player2 moves piece b2
move([player2, Board, P1C1, P1C2, P2C1, P2C2 | Rest], (b2, Direction, OldStonePos, NewStonePos), NewGameState) :-
    update_board(Board, OldStonePos, NewStonePos, NewBoard),
    get_new_piece_positon(P2C2, Direction, NewPiecePos),
    NewGameState = [player1, NewBoard, P1C1, P1C2, P2C1, NewPiecePos | Rest].

/*
valid_moves(+GameState, -ListOfMoves)
Description: Receives the current game state and returns a list of all possible valid moves.
It starts by finding all the valid moves for the current player's pieces. 
Each piece move results in a different set of valid stone moves, that are obtained by calculating all the positions 
in the board with the shortest stack of 1+ stones (positions to remove a stone from) and by then calculating all the 
unnocupied positions to place the removed stone.
*/
valid_moves([PlayerTurn, Board, P1C1, P1C2, P2C1, P2C2 | _], ListOfMoves) :-
    valid_moves_pieces(PlayerTurn, Board, P1C1, P1C2, P2C1, P2C2, PieceMoves),
    findall(((Piece, Direction, StoneMovePrevPosition, StoneMoveCurrentPosition)),
            (
                member((Piece, Direction), PieceMoves),
                get_piece_position(Piece, P1C1, P1C2, P2C1, P2C2, PieceMovePrevPosition),
                get_new_piece_positon(PieceMovePrevPosition, Direction, PieceMoveCurrentPosition),
                find_least_stone_positions(Board, PieceMoveCurrentPosition, P1C1, P1C2, P2C1, P2C2, LeastStonePositions),
                member(StoneMovePrevPosition, LeastStonePositions),
                find_unoccupied_positions(Board, StoneMovePrevPosition, PieceMoveCurrentPosition, P1C1, P1C2, P2C1, P2C2, UnoccupiedPositions),
                member(StoneMoveCurrentPosition, UnoccupiedPositions)

            ),
            ListOfMoves).

/*
value(+GameState, +Player, -Value)
Description: Receives the current game state and returns a value measuring how good or bad the current game state is for the given Player. 
The method uses depth-first search (DFS) to determine the number of reachable positions for each piece, 
aiming to maximize the difference between the number of reachable positions for the current player and their opponent 
(each reachable position is worth 1 point). 
It also attempts to maximize the difference in the number of valid piece moves between the two players 
(each valid piece move is worth 1 point).
The objective is to try to trap the other player's pieces, while keeping the player's own pieces free to move.
*/

%value for player1
value([_, Board, P1C1, P1C2, P2C1, P2C2 | _], player1, Value) :-
    reachable_positions_difference(Board, P1C1, P1C2, P2C1, P2C2, player1, ReachablePositionsDifference),
    piece_moves_difference(Board, P1C1, P1C2, P2C1, P2C2, player1, PieceMovesDifference),
    Value is ReachablePositionsDifference + PieceMovesDifference.

%value for player2
value([_, Board, P1C1, P1C2, P2C1, P2C2 | _], player2, Value) :-
    reachable_positions_difference(Board, P1C1, P1C2, P2C1, P2C2, player2, ReachablePositionsDifference),
    piece_moves_difference(Board, P1C1, P1C2, P2C1, P2C2, player2, PieceMovesDifference),
    Value is ReachablePositionsDifference + PieceMovesDifference.

/*
game_over(+GameState, -Winner)
Description: Receives the current game state and verifies whether the game is over, 
in which case it also identifies the winner.
The game ends when a player has no more valid moves to execute.
*/

%checks if there are no moves available for player1
game_over(GameState, Winner) :-
    GameState = [player1, _, _, _, _, _, _, _, _, Winner],
    valid_moves(GameState, []).
    %checks if there are no moves available for player1
game_over(GameState, Winner) :-
    GameState = [player2, _, _, _, _, _, _, _, Winner | _],
    valid_moves(GameState, []).