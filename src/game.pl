% Load the predicates from view.pl and model.pl
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
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
    read(Choice),
    handle_choice(Choice).

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

% Check if the game is over
game_loop(GameState) :-
    game_over(GameState, Winner), !,
    write('Game over! Winner: '),
    write(Winner), nl.

% Main game loop
game_loop(GameState) :-
    display_game(GameState),
    choose_move(GameState, 1, Move),
    move(GameState, Move, NewGameState),
    game_loop(NewGameState).

% checkar copilot, maybe fazer um metodo para gerar 
% todos os movimentos possíveis logo e cagar para validation
% usar sempre validation para verificar se o movimento é válido 
% em vez de ver se é membro da lista de movimentos possíveis
% update nas duas listas e ver no que dá
choose_move([player1, Board, P1C1, P1C2, P2C1, P2C2, _, IsMoving, PrevPosition , computer | _], 1, Move) :-
    valid_moves([player1, Board, P1C1, P1C2, P2C1, P2C2, _, IsMoving, PrevPosition , computer | _], ListOfMoves),
    random_member(Move, ListOfMoves).
choose_move([player2, Board, P1C1, P1C2, P2C1, P2C2, _, IsMoving, PrevPosition , _, computer | _], 1, Move) :-
    valid_moves([player2, Board, P1C1, P1C2, P2C1, P2C2, _, IsMoving, PrevPosition , _, computer | _], ListOfMoves),
    random_member(Move, ListOfMoves).
choose_move([player1, Board, P1C1, P1C2, P2C1, P2C2, _, 1, _ , human | _], _, Move) :-
    repeat,
    ask_piece(player1, Piece),
    (Piece = w1 -> (OldCol, OldRow) = P1C1 ; Piece = w2 -> (OldCol, OldRow) = P1C2),
    ask_direction(Direction),
    direction_offset(Direction, (ColOffset, RowOffset)),
    NewCol is OldCol + ColOffset,
    NewRow is OldRow + RowOffset,
    Move = (Piece, (NewCol, NewRow)),
    (valid_move_piece(Board, (OldCol, OldRow), (NewCol, NewRow), P1C1, P1C2, P2C1, P2C2) ->
        !
    ;
        write('Invalid move, try again.'), nl,
        fail
    ).
choose_move([player2, Board, P1C1, P1C2, P2C1, P2C2, _, 1, _ , _, human | _], _, Move) :-
    repeat,
    ask_piece(player2, Piece),
    (Piece = b1 -> (OldCol, OldRow) = P2C1 ; Piece = b2 -> (OldCol, OldRow) = P2C2),
    ask_direction(Direction),
    direction_offset(Direction, (ColOffset, RowOffset)),
    NewCol is OldCol + ColOffset,
    NewRow is OldRow + RowOffset,
    Move = (Piece, (NewCol, NewRow)),
    (valid_move_piece(Board, (OldCol, OldRow), (NewCol, NewRow), P1C1, P1C2, P2C1, P2C2) ->
        !
    ;
        write('Invalid move, try again.'), nl,
        fail
    ).
choose_move([_, Board, P1C1, P1C2, P2C1, P2C2, _, 0 , PrevPosition | _], _, Move) :-  
    repeat,
    find_least_stone_positions(Board, PrevPosition, P1C1, P1C2, P2C1, P2C2, LeastStonePositions),
    display_available_places_to_remove_stone(LeastStonePositions),
    ask_stone_from(LeastStonePositions, FromPos),
    ask_stone_to(ToPos),
    Move = (FromPos, ToPos),
    (valid_move_stone(Board, (FromPos), ToPos, PrevPosition, P1C1, P1C2, P2C1, P2C2) ->
        !
    ;
        write('Invalid move, try again.'), nl,
        fail
    ).

ask_stone_from(LeastStonePositions, FromPos) :-
    repeat,
    write('Enter the position to remove a stone from: '), nl,
    read(UserInput),
    ( member(UserInput, LeastStonePositions) ->
        FromPos = UserInput
    ;
        write('Invalid position, try again.'), nl,
        fail
    ).

ask_stone_to(ToPos) :-
    repeat,
    write('Enter the position you want to place a stone in: '), nl,
    read(UserInput),
    ToPos = UserInput.

display_available_places_to_remove_stone(LeastStonePositions) :-
    write('Available positions to remove a stone from: '), nl,
    maplist(display_position, LeastStonePositions),
    nl.

display_position((Col, Row)) :-
    format('(~w,~w) ', [Col, Row]).

ask_piece(player1, Piece) :-
    repeat,
    write('Choose the piece you want to move (w1, w2): '),
    read(UserInput),
    ( member(UserInput, [w1, w2]) ->
        Piece = UserInput
    ;
        write('Invalid piece, try again.'), nl,
        fail
    ).

ask_piece(player2, Piece) :-
    repeat,
    write('Choose the piece you want to move (b1, b2): '),
    read(UserInput),
    (member(UserInput, [b1, b2]) ->
        Piece = UserInput
    ;
        write('Invalid piece, try again.'), nl,
        fail
    ).

ask_direction(Direction) :-
    repeat,
    write('Enter your move (up, down, left, right, up-left, up-right, down-left, down-right): '),
    read(UserInput),
    valid_direction(UserInput),
    !,
    Direction = UserInput.

% Predicate to check if a direction is valid
valid_direction(Direction) :-
    member(Direction, [up, down, left, right, 'up-left', 'up-right', 'down-left', 'down-right']).

move(GameState, Move, NewGameState) :-
    GameState = [PlayerTurn, Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize, IsMoving | Rest],
    (IsMoving =:= 1 ->
        move_piece(GameState, Move, NewGameState)
    ;
        move_stone(GameState, Move, NewGameState)
    ).

% Move piece
move_piece(GameState, (Piece, NewPos), NewGameState) :-
    GameState = [PlayerTurn, Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize, IsMoving, PrevPosition | Rest],
    (PlayerTurn = player1 ->
        (Piece = w1 -> NewGameState = [player1, Board, NewPos, P1C2, P2C1, P2C2, MaxStackSize, 0, P1C1 | Rest]
        ; NewGameState = [player1, Board, P1C1, NewPos, P2C1, P2C2, MaxStackSize, 0, P1C2 | Rest])
    ;
        (Piece = b1 -> NewGameState = [player2, Board, P1C1, P1C2, NewPos, P2C2, MaxStackSize, 0, P2C1 | Rest]
        ; NewGameState = [player2, Board, P1C1, P1C2, P2C1, NewPos, MaxStackSize, 0, P2C2 | Rest])
    ).

% Move stone
move_stone(GameState, (FromPos, ToPos), NewGameState) :-
    GameState = [PlayerTurn, Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize, IsMoving, PrevPosition | Rest],
    update_board(Board, FromPos, ToPos, NewBoard),
    max_stack_size(NewBoard, NewMaxStackSize),
    (PlayerTurn = player1 ->
        NewPlayerTurn = player2
    ;
        NewPlayerTurn = player1
    ),
    NewGameState = [NewPlayerTurn, NewBoard, P1C1, P1C2, P2C1, P2C2, NewMaxStackSize, 1, (-1,-1) | Rest].

% Update board
update_board(Board, (OldCol, OldRow), (NewCol, NewRow), NewBoard) :-
    length(Board, NumRows),
    % Adjust coordinates to start from bottom left
    AdjustedOldRow is NumRows - OldRow + 1,
    AdjustedNewRow is NumRows - NewRow + 1,
    nth1(AdjustedOldRow, Board, OldRowList),
    nth1(OldCol, OldRowList, OldCell),
    NewOldCell is OldCell - 1,
    replace_nested(Board, AdjustedOldRow, OldCol, NewOldCell, TempBoard),
    nth1(AdjustedNewRow, TempBoard, NewRowList),
    nth1(NewCol, NewRowList, NewCell),
    NewNewCell is NewCell + 1,
    replace_nested(TempBoard, AdjustedNewRow, NewCol, NewNewCell, NewBoard).


% Replace element in list
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

% Replace element in nested list
replace_nested([Row|RestRows], 1, Col, NewValue, [NewRow|RestRows]) :-
    replace(Row, Col, NewValue, NewRow).
replace_nested([Row|RestRows], RowIndex, Col, NewValue, [Row|NewRestRows]) :-
    RowIndex > 1,
    RowIndex1 is RowIndex - 1,
    replace_nested(RestRows, RowIndex1, Col, NewValue, NewRestRows).

remove_duplicates([], []).
remove_duplicates([H|T], List) :-
    member(H, T),
    !,
    remove_duplicates(T, List).
remove_duplicates([H|T], [H|T1]) :-
    remove_duplicates(T, T1).

valid_moves_pieces([player1, Board, P1C1, P1C2, P2C1, P2C2 | _], ListOfMoves) :-
    Pieces = [(w1, P1C1), (w2, P1C2)],
    findall((Piece, (NewCol, NewRow)),
            (   member((Piece, (OldCol, OldRow)), Pieces),
                direction_offset(Direction, (ColOffset, RowOffset)),
                NewCol is OldCol + ColOffset,
                NewRow is OldRow + RowOffset,
                valid_move_piece(Board, (OldCol, OldRow), (NewCol, NewRow), P1C1, P1C2, P2C1, P2C2)
            ),
            Moves),
    remove_duplicates(Moves, ListOfMoves).
valid_moves_pieces([player2, Board, P1C1, P1C2, P2C1, P2C2 | _], ListOfMoves) :-
    Pieces = [(b1, P2C1), (b2, P2C2)],
    findall((Piece, (NewCol, NewRow)),
            (   member((Piece, (OldCol, OldRow)), Pieces),
                direction_offset(Direction, (ColOffset, RowOffset)),
                NewCol is OldCol + ColOffset,
                NewRow is OldRow + RowOffset,
                valid_move_piece(Board, (OldCol, OldRow), (NewCol, NewRow), P1C1, P1C2, P2C1, P2C2)
            ),
            Moves),
    remove_duplicates(Moves, ListOfMoves).

% Valid move for piece
valid_move_piece(Board, (OldCol, OldRow), (NewCol, NewRow), P1C1, P1C2, P2C1, P2C2) :-
    (NewCol, NewRow) \= P1C1,
    (NewCol, NewRow) \= P1C2,
    (NewCol, NewRow) \= P2C1,
    (NewCol, NewRow) \= P2C2,
    length(Board, NumRows),
    nth1(1, Board, FirstRow),
    length(FirstRow, NumCols),
    % Check if the new position is within bounds
    NewRow > 0, NewRow =< NumRows,
    NewCol > 0, NewCol =< NumCols,
    (OldRow \= NewRow ; OldCol \= NewCol),
    % Adjust coordinates to start from bottom left
    AdjustedOldRow is NumRows - OldRow + 1,
    AdjustedNewRow is NumRows - NewRow + 1,
    nth1(AdjustedOldRow, Board, OldRowList),
    nth1(OldCol, OldRowList, OldCell),
    nth1(AdjustedNewRow, Board, NewRowList),
    nth1(NewCol, NewRowList, NewCell),
    % Check if the move is valid
    abs(OldRow - NewRow) =< 1,
    abs(OldCol - NewCol) =< 1,
    NewCell =< OldCell + 1,
    NewCell >= OldCell - 1,
    NewCell > 0.


valid_moves_stone([_, Board, P1C1, P1C2, P2C1, P2C2, _, _, PrevPosition | _], ListOfMoves) :-
    find_least_stone_positions(Board, PrevPosition, P1C1, P1C2, P2C1, P2C2, LeastStonePositions),
    findall((FromPos, ToPos),
            (   member(FromPos, LeastStonePositions),
                find_unoccupied_positions(Board, FromPos, PrevPosition, P1C1, P1C2, P2C1, P2C2, UnoccupiedPositions),
                member(ToPos, UnoccupiedPositions)
            ),
            ListOfMoves).

% Check if a position is unoccupied and not the previous position
find_unoccupied_positions(Board, FromPos, PrevPosition, P1C1, P1C2, P2C1, P2C2, UnoccupiedPositions) :-
    length(Board, NumRows),
    numlist(1, NumRows, RowColList),
    findall((Col, Row), (
        between(1, NumRows, AdjustedRow),
        nth1(AdjustedRow, Board, RowList),
        length(RowList, NumCols),
        between(1, NumCols, Col),
        Row is NumRows - AdjustedRow + 1,
        nth1(Col, RowList, Cell),
        Cell > 0,
        (Col, Row) \= PrevPosition,
        (Col, Row) \= P1C1,
        (Col, Row) \= P1C2,
        (Col, Row) \= P2C1,
        (Col, Row) \= P2C2,
        FromPos \= (Col, Row)
    ), UnoccupiedPositions).

% Valid move for stone
valid_move_stone(Board, (FromCol, FromRow), (ToCol, ToRow), PrevPosition, P1C1, P1C2, P2C1, P2C2) :-
    find_least_stone_positions(Board, PrevPosition, P1C1, P1C2, P2C1, P2C2, LeastStonePositions),
    member((FromCol, FromRow), LeastStonePositions),
    valid_unoccupied_position(Board, (ToCol, ToRow), PrevPosition, P1C1, P1C2, P2C1, P2C2).

% Find positions with the least number of stones
find_least_stone_positions(Board, PrevPosition, P1C1, P1C2, P2C1, P2C2, LeastStonePositions) :-
    flatten(Board, FlatBoard),
    exclude(=(0), FlatBoard, NonZeroStones),
    min_list(NonZeroStones, MinStones),
    length(Board, NumRows),
    findall((Col, Row), (
        nth1(AdjustedRow, Board, RowList),
        nth1(Col, RowList, MinStones),
        Row is NumRows - AdjustedRow + 1,
        (Col, Row) \= PrevPosition,
        (Col, Row) \= P1C1,
        (Col, Row) \= P1C2,
        (Col, Row) \= P2C1,
        (Col, Row) \= P2C2
    ), LeastStonePositions).

% Find the minimum element in a list
min_list([Min], Min).
min_list([H|T], Min) :-
    min_list(T, MinTail),
    min(H, MinTail, Min).

% Helper predicate to find the minimum of two numbers
min(X, Y, X) :- X =< Y.
min(X, Y, Y) :- X > Y.

% Check if a position is unoccupied and not the previous position
valid_unoccupied_position(Board, (ToCol, ToRow), PrevPosition, P1C1, P1C2, P2C1, P2C2) :-
    length(Board, NumRows),
    nth1(1, Board, FirstRow),
    length(FirstRow, NumCols),
    % Check if the position is within bounds
    ToRow > 0, ToRow =< NumRows,
    ToCol > 0, ToCol =< NumCols,
    % Adjust coordinates to start from bottom left
    AdjustedRow is NumRows - ToRow + 1,
    nth1(AdjustedRow, Board, RowList),
    nth1(ToCol, RowList, Cell),
    % Check if the cell is empty and not the previous position or occupied by any pieces
    Cell > 0,
    (ToCol, ToRow) \= PrevPosition,
    (ToCol, ToRow) \= P1C1,
    (ToCol, ToRow) \= P1C2,
    (ToCol, ToRow) \= P2C1,
    (ToCol, ToRow) \= P2C2.


valid_moves(GameState, ListOfMoves) :-
    GameState = [_, Board, _, _, _, _, _, IsMoving, _ | _], 
    (IsMoving =:= 1 ->
        valid_moves_pieces(GameState, ListOfMoves)
    ;
        valid_moves_stone(GameState, ListOfMoves)
    ).

% Check if the game is over and identify the winner
game_over(GameState, Winner) :-
    valid_moves(GameState, ListOfMoves),
    (ListOfMoves = [] ->
        GameState = [PlayerTurn | _],
        (PlayerTurn = player1 ->
            Winner = player2
        ;
            Winner = player1
        )
    ;
        fail
    ).

% Direction offsets
direction_offset('up', (0, 1)).
direction_offset('down', (0, -1)).
direction_offset('left', (-1, 0)).
direction_offset('right', (1, 0)).
direction_offset('up-left', (-1, 1)).
direction_offset('up-right', (1, 1)).
direction_offset('down-left', (-1, -1)).
direction_offset('down-right', (1, -1)).