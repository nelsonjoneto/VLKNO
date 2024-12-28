% Load the predicates from view.pl and model.pl
:- use_module(library(lists)).
:- use_module(view).
:- use_module(model).

% Main predicate to start the game
play :-
    display_menu,
    read(Choice),
    handle_choice(Choice).

% Start the game with the given player types and names.
start_game(Player1Type, Player2Type, Player1Name, Player2Name) :-
    write('Starting game...'), nl,
    format('Player 1: ~w (~w), Player 2: ~w (~w)~n~n', [Player1Name, Player1Type, Player2Name, Player2Type]),
    initial_state([Player1Type, Player2Type, Player1Name, Player2Name], GameState),
    game_loop(GameState).

% Handle the user's choice.
handle_choice(1) :-
    write('You chose Human vs Human'), nl,
    choose_player_names(human, human, Player1Name, Player2Name),
    start_game(human, human, Player1Name, Player2Name).
handle_choice(2) :-
    write('You chose Human vs Computer'), nl,
    choose_player_names(human, computer, Player1Name, Player2Name),
    start_game(human, computer, Player1Name, Player2Name).
handle_choice(3) :-
    write('You chose Computer vs Human'), nl,
    choose_player_names(computer, human, Player1Name, Player2Name),
    start_game(computer, human, Player1Name, Player2Name).
handle_choice(4) :-
    write('You chose Computer vs Computer'), nl,
    start_game(computer, computer, 'PC1', 'PC2').
handle_choice(_) :-
    write('Invalid choice, please choose a valid option (1-4).'), nl,
    play.

% Choose player names
choose_player_names(Player1Type, Player2Type, Player1Name, Player2Name) :-
    (Player1Type = human ->
        write('Enter name for Player 1: '), read(Player1Name)
    ;
        Player1Name = 'PC1'
    ),
    (Player2Type = human ->
        write('Enter name for Player 2: '), read(Player2Name)
    ;
        Player2Name = 'PC2'
    ).



% Example game_loop/1 predicate
game_loop(GameState) :-
    display_game(GameState).
    /*
    (game_over(GameState, Winner) ->
        format('Game over! Winner: ~w~n', [Winner])
    ;
        GameState = [PlayerTurn | _],
        (PlayerTurn = human ->
            % Human player's turn
            write('Enter your move: '), read(Move),
            (valid_move(GameState, Move) ->
                move(GameState, Move, NewGameState),
                game_loop(NewGameState)
            ;
                write('Invalid move, try again.'), nl,
                game_loop(GameState)
            )
        ;
            % Computer player's turn
            choose_move(GameState, 1, Move), % Change level as needed
            move(GameState, Move, NewGameState),
            game_loop(NewGameState)
        )
    ).*/

move(GameState, Move, NewGameState) :-
    GameState = [PlayerTurn, Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize, IsMoving | Rest],
    (IsMoving =:= 1 ->
        move_piece(GameState, Move, NewGameState)
    ;
        move_stone(GameState, Move, NewGameState)
    ).

% Move piece
move_piece(GameState, (Piece, NewPos), NewGameState) :-
    GameState = [PlayerTurn, Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize, IsMoving | Rest],
    (PlayerTurn = player1 ->
        (Piece = p1c1 -> OldPos = P1C1 ; OldPos = P1C2)
    ;
        (Piece = p2c1 -> OldPos = P2C1 ; OldPos = P2C2)
    ),
    valid_move_piece(Board, OldPos, NewPos),
    update_board(Board, OldPos, NewPos, NewBoard),
    (PlayerTurn = player1 ->
        (Piece = p1c1 -> NewGameState = [player2, NewBoard, NewPos, P1C2, P2C1, P2C2, MaxStackSize, 0 | Rest]
        ; NewGameState = [player2, NewBoard, P1C1, NewPos, P2C1, P2C2, MaxStackSize, 0 | Rest])
    ;
        (Piece = p2c1 -> NewGameState = [player1, NewBoard, P1C1, P1C2, NewPos, P2C2, MaxStackSize, 0 | Rest]
        ; NewGameState = [player1, NewBoard, P1C1, P1C2, P2C1, NewPos, MaxStackSize, 0 | Rest])
    ).

% Example game_over/2 predicate
game_over(GameState, Winner) :-
    % Implement logic to check if the game is over and determine the winner
    false.

% Example valid_move/2 predicate
valid_move(GameState, Move) :-
    % Implement logic to validate a move
    true.

% Example move/3 predicate
move(GameState, Move, NewGameState) :-
    % Implement logic to execute a move and update the game state
    NewGameState = GameState.

% Example choose_move/3 predicate
choose_move(GameState, Level, Move) :-
    % Implement logic to choose a move for the computer player
    Move = dummy_move.