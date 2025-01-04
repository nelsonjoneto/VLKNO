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
start_game(Player1Type, Player2Type, Player1Name, Player2Name, BoardSize, Difficulty1-Difficulty2) :-
    write('Starting game...'), nl,
    format('Player 1: ~w (~w), Player 2: ~w (~w)~n~n', [Player1Name, Player1Type, Player2Name, Player2Type]),
    initial_state([Player1Type, Player2Type, Player1Name, Player2Name, BoardSize], GameState),
    display_game(GameState),
    game_loop(Difficulty1-Difficulty2, GameState).

% Handle the user's choice.
handle_choice(1) :-
    write('You chose Human vs Human'), nl,
    choose_player_names(human, human, Player1Name, Player2Name),
    choose_board_size(BoardSize),
    start_game(human, human, Player1Name, Player2Name, BoardSize, 0-0).
handle_choice(2) :-
    write('You chose Human vs Computer'), nl,
    choose_player_names(human, computer, Player1Name, Player2Name),
    choose_board_size(BoardSize),
    choose_difficulty(pc2, Difficulty),
    start_game(human, computer, Player1Name, Player2Name, BoardSize, 0-Difficulty).
handle_choice(3) :-
    write('You chose Computer vs Human'), nl,
    choose_player_names(computer, human, Player1Name, Player2Name),
    choose_board_size(BoardSize),
    choose_difficulty(pc1, Difficulty),
    start_game(computer, human, Player1Name, Player2Name, BoardSize, Difficulty-0).
handle_choice(4) :-
    write('You chose Computer vs Computer'), nl,
    choose_board_size(BoardSize),
    choose_difficulty(pc1, Difficulty1),
    choose_difficulty(pc2, Difficulty2),
    start_game(computer, computer, 'PC1', 'PC2', BoardSize, Difficulty1-Difficulty2).
handle_choice(_) :-
    write('Invalid choice, please choose a valid option (1-4).'), nl,
    read(Choice),
    handle_choice(Choice).

% Choose player names
choose_player_names(human, human, Player1Name, Player2Name) :-
    write('Enter name for Player 1: '), read(Player1Name),
    write('Enter name for Player 2: '), read(Player2Name).
choose_player_names(human, computer, Player1Name, Player2Name) :-
    write('Enter name for Player 1: '), read(Player1Name),
    Player2Name = 'PC2'.
choose_player_names(computer, human, Player1Name, Player2Name) :-
    Player1Name = 'PC1',
    write('Enter name for Player 2: '), read(Player2Name).

% Choose the difficulty level for the computer player
choose_difficulty(pc1, Difficulty) :-
    display_difficulty_pc1,
    repeat,
    read(DifficultyChoice),
    choose_difficulty_option(DifficultyChoice, Difficulty),
    !.
choose_difficulty(pc2, Difficulty) :-
    display_difficulty_pc2,
    repeat,
    read(DifficultyChoice),
    choose_difficulty_option(DifficultyChoice, Difficulty),
    !.

choose_difficulty_option(1, 1).
choose_difficulty_option(2, 2).
choose_difficulty_option(_, _) :-
    write('Invalid choice, please choose a valid option (1-2).'), nl,
    fail.

% Choose the difficulty level for the computer player
choose_board_size(BoardSize) :-
    display_board_size,
    repeat,
    read(BoardSizeChoice),
    choose_board_size_option(BoardSizeChoice, BoardSize),
    !.

choose_board_size_option(1,5).
choose_board_size_option(2, BoardSize) :-
    write('Enter the size of the board (NxN, between 4 and 7)'), 
    repeat,
    read(CustomSize),
    valid_board_size(CustomSize, BoardSize),
    !.
choose_board_size_option(_, _) :-
    write('Invalid choice, please choose a valid option (1-2).'), nl,
    fail.

valid_board_size(CustomSize, BoardSize) :-
    integer(CustomSize),
    CustomSize >= 4,
    CustomSize =< 7,
    BoardSize is CustomSize.
valid_board_size(_, _) :-
    write('Invalid board size, please choose a valid size (between 4 and 7).'), nl,
    fail.

%implementar diferentes nives de dificuldade para o computador

/*
    é interessante ver também a jogadas que resulta no maior número de movimentos possíveis futuros


    dfs ou bfs para ver a rede conectada de cada jogador, atribuindo pontos
    em caso de empate, vemos as casas à volta da rede, se forem vazias dao 2 pontos ao adversario
    ou pode ser tipo percentagem da rede bloqueada por casas vazias
    se forem ocupadas pelo adversario dao 1 ponto ao adversario,
    se forem ocupadas por stacks nao subiveis, calc

*/
%back dou call do metodo de novo
%ver quais os moves validos para cada peça para o jogador humano e so dar esses como opção
%implementar diferentes tamanhos (done)
%implementar IO gira (ver mansur)
%maybe criar um pause button 


game_loop(_, GameState) :-
    game_over(GameState, Winner), !,
    write('Game over! Winner: '),
    write(Winner), nl.

% Main game loop
game_loop(Difficulty1-Difficulty2, GameState) :-
    choose_move(GameState, Difficulty1, Move),
    move(GameState, Move, NewGameState),
    display_game(NewGameState), !,
    game_loop(Difficulty2-Difficulty1, NewGameState).

% criar metodo level que recebe o nome do jogador e vê o nivel de dificuldade

choose_move([player1, Board, P1C1, P1C2, P2C1, P2C2, computer | _], 1, Move) :-
    valid_moves([player1, Board, P1C1, P1C2, P2C1, P2C2 | _], ListOfMoves),
    random_member(Move, ListOfMoves).
choose_move([player1, Board, P1C1, P1C2, P2C1, P2C2, computer | Rest], 2, Move) :-
    valid_moves([player1, Board, P1C1, P1C2, P2C1, P2C2 | _], ListOfMoves),
    setof(Value-Mv, NewState^(
        member(Mv, ListOfMoves),
        move([player1, Board, P1C1, P1C2, P2C1, P2C2, computer | Rest], Mv, NewState),
        value(NewState, player1, Value)
    ), [_V-Move | _]).
choose_move([player2, Board, P1C1, P1C2, P2C1, P2C2, _, computer | _], 1, Move) :-
    valid_moves([player2, Board, P1C1, P1C2, P2C1, P2C2 | _], ListOfMoves),
    random_member(Move, ListOfMoves).
choose_move([player2, Board, P1C1, P1C2, P2C1, P2C2, _, computer | _], 2, Move) :-
    valid_moves([player2, Board, P1C1, P1C2, P2C1, P2C2 | _], ListOfMoves),
    setof(Value-Mv, NewState^(
        member(Mv, ListOfMoves),
        move([player2, Board, P1C1, P1C2, P2C1, P2C2, _, computer | Rest], Mv, NewState),
        value(NewState, player2, Value)
    ), [_V-Move | _]).
choose_move([player1, Board, P1C1, P1C2, P2C1, P2C2, human | _], _, Move) :-
    ask_piece_and_direction(player1, Board, P1C1, P1C2, P2C1, P2C2, (OldPiecePos, NewPiecePos)),
    ask_stone_movement(Board, P1C1, P1C2, P2C1, P2C2, NewPiecePos, (OldStonePos, NewStonePos)),
    Move = (OldPiecePos, NewPiecePos, OldStonePos, NewStonePos).
choose_move([player2, Board, P1C1, P1C2, P2C1, P2C2, _, human | _], _, Move) :-
    ask_piece_and_direction(player2, Board, P1C1, P1C2, P2C1, P2C2, (OldPiecePos, NewPiecePos)),
    ask_stone_movement(Board, P1C1, P1C2, P2C1, P2C2, NewPiecePos, (OldStonePos, NewStonePos)),
    Move = (OldPiecePos, NewPiecePos, OldStonePos, NewStonePos).
    

ask_stone_movement(Board, P1C1, P1C2, P2C1, P2C2, NewPiecePos, (OldStonePos, NewStonePos)) :-
    find_min_height_stack(Board, NewPiecePos, P1C1, P1C2, P2C1, P2C2, MinHeightStack),
    repeat,
    write('Choose a stack to remove a stone from ("Column,Row")'),
    read(OldStonePos),
    ensure_valid_least_stone_position(Board, OldStonePos, MinHeightStack, NewPiecePos, P1C1, P1C2, P2C1, P2C2), !,
    repeat,
    write('Choose a stack to place a stone in ("Column,Row")'),
    read(NewStonePos),
    ensure_valid_unoccupied_position(Board, OldStonePos, NewPiecePos, NewStonePos, P1C1, P1C2, P2C1, P2C2),
    !.

ask_piece_and_direction(player1, Board, P1C1, P1C2, P2C1, P2C2, (OldPiecePos, NewPiecePos)) :-
    repeat,
    write('Choose a piece to move ("w1", "w2"): '),
    read(InputPiece),
    validate_piece(player1, InputPiece, Piece), !,
    repeat,
    write('Choose the direction ("up", "down", "left", "right", "upleft", "upright", "downleft", "downright"): '),
    read(InputDirection),
    once(validate_direction(InputDirection, Direction)),
    get_piece_position(player1, Piece, P1C1, P1C2, OldPiecePos),
    ensure_valid_move(Board, OldPiecePos, Direction, P1C1, P1C2, P2C1, P2C2, NewPiecePos),
    !.
ask_piece_and_direction(player2, Board, P1C1, P1C2, P2C1, P2C2, (OldPiecePos, NewPiecePos)) :-
    repeat,
    write('Choose a piece to move ("b1", "b2"): '),
    read(InputPiece),
    validate_piece(player2, InputPiece, Piece), !,
    repeat,
    write('Choose the direction ("up", "down", "left", "right", "upleft", "upright", "downleft", "downright"): '),
    read(InputDirection),
    once(validate_direction(InputDirection, Direction)),
    get_piece_position(player2, Piece, P2C1, P2C2, OldPiecePos),
    ensure_valid_move(Board, OldPiecePos, Direction, P1C1, P1C2, P2C1, P2C2, NewPiecePos),
    !.

validate_piece(player1, w1, w1).
validate_piece(player1, w2, w2).
validate_piece(player2, b1, b1).
validate_piece(player2, b2, b2).
validate_piece(_, _, _) :-
    write('Invalid piece, try again.'), nl,
    fail.

get_piece_position(player1, w1, P1C1, _, P1C1).
get_piece_position(player1, w2, _, P1C2, P1C2).
get_piece_position(player2, b1, P2C1, _, P2C1).
get_piece_position(player2, b2, _, P2C2, P2C2).

% Predicate to check if a direction is valid
validate_direction(up, up).
validate_direction(down, down).
validate_direction(left, left).
validate_direction(right, right).
validate_direction('upleft', 'upleft').
validate_direction('upright', 'upright').
validate_direction('downleft', 'downleft').
validate_direction('downright', 'downright').
validate_direction(_,_) :-
    write('Invalid direction, try again.'), nl,
    fail.

% Ensure the move is valid and calculate the new position
ensure_valid_move(Board, (OldCol, OldRow), Direction, P1C1, P1C2, P2C1, P2C2, (NewCol, NewRow)) :-
    direction_offset(Direction, (ColOffset, RowOffset)),
    NewCol is OldCol + ColOffset,
    NewRow is OldRow + RowOffset,
    valid_move_piece(Board, (OldCol, OldRow), (NewCol, NewRow), P1C1, P1C2, P2C1, P2C2).
ensure_valid_move(_, _, _, _, _, _, _, _) :-
    write('Invalid piece move, try again.'), nl,
    fail.

ensure_valid_least_stone_position(Board, OldStonePos, MinHeightStack, NewPiecePos, P1C1, P1C2, P2C1, P2C2) :-
    valid_least_stone_position(Board, OldStonePos, MinHeightStack, NewPiecePos, P1C1, P1C2, P2C1, P2C2).
ensure_valid_least_stone_position(_, _, _, _, _, _, _, _) :-
    write('Invalid stack, try again.'), nl,
    write('You can only pick up a stone from one of the smallest unoccupied stacks on the board, except the one you just moved your piece from.'), nl,
    fail.
ensure_valid_unoccupied_position(Board, OldStonePos, NewPiecePos, NewStonePos, P1C1, P1C2, P2C1, P2C2) :-
    valid_unoccupied_position(Board, OldStonePos, NewPiecePos, NewStonePos, P1C1, P1C2, P2C1, P2C2).
ensure_valid_unoccupied_position(_, _, _, _, _, _, _, _) :-
    write('Invalid stack, try again.'), nl,
    write('You can only place a stone on an unoccupied stack of 1+ stones, except the one you just moved your piece from.'), nl,
    fail.

display_available_places_to_remove_stone(LeastStonePositions) :-
    write('Available positions to remove a stone from: '), nl,
    maplist(display_position, LeastStonePositions),
    nl.

display_position((Col, Row)) :-
    format('(~w,~w) ', [Col, Row]).


move([player1, Board, P1C1, P1C2, P2C1, P2C2 | Rest], (P1C1, NewPiecePos, OldStonePos, NewStonePos), NewGameState) :-
    update_board(Board, OldStonePos, NewStonePos, NewBoard),
    NewGameState = [player2, NewBoard, NewPiecePos, P1C2, P2C1, P2C2 | Rest].
move([player1, Board, P1C1, P1C2, P2C1, P2C2 | Rest], (P1C2, NewPiecePos, OldStonePos, NewStonePos), NewGameState) :-
    update_board(Board, OldStonePos, NewStonePos, NewBoard),
    NewGameState = [player2, NewBoard, P1C1, NewPiecePos, P2C1, P2C2 | Rest].
move([player2, Board, P1C1, P1C2, P2C1, P2C2 | Rest], (P2C1, NewPiecePos, OldStonePos, NewStonePos), NewGameState) :-
    update_board(Board, OldStonePos, NewStonePos, NewBoard),
    NewGameState = [player1, NewBoard, P1C1, P1C2, NewPiecePos, P2C2 | Rest].
move([player2, Board, P1C1, P1C2, P2C1, P2C2 | Rest], (P2C2, NewPiecePos, OldStonePos, NewStonePos), NewGameState) :-
    update_board(Board, OldStonePos, NewStonePos, NewBoard),
    NewGameState = [player1, NewBoard, P1C1, P1C2, P2C1, NewPiecePos | Rest].



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

% Valid move for stone
valid_move_stone(Board, (FromCol, FromRow), (ToCol, ToRow), PrevPosition, P1C1, P1C2, P2C1, P2C2) :-
    find_least_stone_positions(Board, PrevPosition, P1C1, P1C2, P2C1, P2C2, LeastStonePositions),
    member((FromCol, FromRow), LeastStonePositions),
    valid_unoccupied_position(Board, (ToCol, ToRow), PrevPosition, P1C1, P1C2, P2C1, P2C2).


valid_least_stone_position(Board, (Col, Row), MinHeightStack, CurrentPosition, P1C1, P1C2, P2C1, P2C2) :-
    length(Board, NumRows),
    nth1(AdjustedRow, Board, RowList),
    nth1(Col, RowList, MinHeightStack),
    Row is NumRows - AdjustedRow + 1,
    (Col, Row) \= CurrentPosition,
    (Col, Row) \= P1C1,
    (Col, Row) \= P1C2,
    (Col, Row) \= P2C1,
    (Col, Row) \= P2C2.

find_min_height_stack(Board, CurrentPosition, P1C1, P1C2, P2C1, P2C2, MinHeightStack) :- 
    length(Board, NumRows),
    findall(Cell, (
        nth1(AdjustedRow, Board, RowList),
        nth1(Col, RowList, Cell),
        Row is NumRows - AdjustedRow + 1,
        (Col, Row) \= CurrentPosition,
        (Col, Row) \= P1C1,
        (Col, Row) \= P1C2,
        (Col, Row) \= P2C1,
        (Col, Row) \= P2C2,
        Cell > 0
    ), NonZeroStones),
    min_list(NonZeroStones, MinHeightStack).

% Find positions with the least number of stones
find_least_stone_positions(Board, CurrentPosition, P1C1, P1C2, P2C1, P2C2, LeastStonePositions) :-
    find_min_height_stack(Board, CurrentPosition, P1C1, P1C2, P2C1, P2C2, MinHeightStack),
    length(Board, NumRows),
    findall((Col, Row), (
        nth1(AdjustedRow, Board, RowList),
        nth1(Col, RowList, MinHeightStack),
        Row is NumRows - AdjustedRow + 1,
        (Col, Row) \= CurrentPosition,
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
valid_unoccupied_position(Board, FromPos, CurrentPosition, (Col, Row), P1C1, P1C2, P2C1, P2C2) :-
    length(Board, NumRows),
    between(1, NumRows, AdjustedRow),
    nth1(AdjustedRow, Board, RowList),
    between(1, NumRows, Col),
    Row is NumRows - AdjustedRow + 1,
    nth1(Col, RowList, Cell),
    Cell > 0,
    (Col, Row) \= CurrentPosition,
    (Col, Row) \= P1C1,
    (Col, Row) \= P1C2,
    (Col, Row) \= P2C1,
    (Col, Row) \= P2C2,
    FromPos \= (Col, Row).


find_unoccupied_positions(Board, FromPos, CurrentPosition, P1C1, P1C2, P2C1, P2C2, UnoccupiedPositions) :-
    length(Board, NumRows),
    findall((Col, Row), (
        between(1, NumRows, AdjustedRow),
        nth1(AdjustedRow, Board, RowList),
        between(1, NumRows, Col),
        Row is NumRows - AdjustedRow + 1,
        nth1(Col, RowList, Cell),
        Cell > 0,
        (Col, Row) \= CurrentPosition,
        (Col, Row) \= P1C1,
        (Col, Row) \= P1C2,
        (Col, Row) \= P2C1,
        (Col, Row) \= P2C2,
        FromPos \= (Col, Row)
    ), UnoccupiedPositions).

valid_moves([PlayerTurn, Board, P1C1, P1C2, P2C1, P2C2 | _], ListOfMoves) :-
    valid_moves_pieces(PlayerTurn, Board, P1C1, P1C2, P2C1, P2C2, PieceMoves),
    findall(((PieceMovePrevPosition, PieceMoveCurrentPosition, StoneMovePrevPosition, StoneMoveCurrentPosition)),
            (
                member((PieceMovePrevPosition, PieceMoveCurrentPosition), PieceMoves),
                find_least_stone_positions(Board, PieceMoveCurrentPosition, P1C1, P1C2, P2C1, P2C2, LeastStonePositions),
                member(StoneMovePrevPosition, LeastStonePositions),
                find_unoccupied_positions(Board, StoneMovePrevPosition, PieceMoveCurrentPosition, P1C1, P1C2, P2C1, P2C2, UnoccupiedPositions),
                member(StoneMoveCurrentPosition, UnoccupiedPositions)

            ),
            ListOfMoves).

% Depth-first search to find all reachable positions from an initial position
dfs(Board, StartPos, P1C1, P1C2, P2C1, P2C2, Visited) :-
    dfs(Board, [StartPos], [], Visited, P1C1, P1C2, P2C1, P2C2).

% Base case: when the stack is empty, return the visited nodes
dfs(_, [], Visited, Visited, _, _, _, _).

% Recursive case: explore neighbors
dfs(Board, [(OldCol, OldRow)|Rest], Visited, FinalVisited, P1C1, P1C2, P2C1, P2C2) :-
    \+ member((OldCol, OldRow), Visited),
    findall((NewCol, NewRow),
        (
            direction_offset(Direction, (ColOffset, RowOffset)),
            NewCol is OldCol + ColOffset,
            NewRow is OldRow + RowOffset,
            valid_move_piece(Board, (OldCol, OldRow), (NewCol, NewRow), P1C1, P1C2, P2C1, P2C2),
            \+ member((NewCol, NewRow), Visited)
        ),
        NeighborsPositions),
    append(NeighborsPositions, Rest, NewStack),
    dfs(Board, NewStack, [(OldCol, OldRow)|Visited], FinalVisited, P1C1, P1C2, P2C1, P2C2), !.

% If the position is already visited, just proceed with the rest of the stack
dfs(Board, [(_, _)|Rest], Visited, FinalVisited, P1C1, P1C2, P2C1, P2C2) :-
    dfs(Board, Rest, Visited, FinalVisited, P1C1, P1C2, P2C1, P2C2).

reachable_positions_difference(Board, P1C1, P1C2, P2C1, P2C2, player1, ReachablePositionsDifference) :-
    dfs(Board, P1C1, P1C1, P1C2, P2C1, P2C2, ReachablePositionsFromP1C1),
    dfs(Board, P1C2, P1C1, P1C2, P2C1, P2C2, ReachablePositionsFromP1C2),
    dfs(Board, P2C1, P1C1, P1C2, P2C1, P2C2, ReachablePositionsFromP2C1),
    dfs(Board, P2C2, P1C1, P1C2, P2C1, P2C2, ReachablePositionsFromP2C2),
    length(ReachablePositionsFromP1C1, LengthP1C1),
    length(ReachablePositionsFromP1C2, LengthP1C2),
    length(ReachablePositionsFromP2C1, LengthP2C1),
    length(ReachablePositionsFromP2C2, LengthP2C2),
    ReachablePositionsDifference is (LengthP2C1 + LengthP2C2) - (LengthP1C1 + LengthP1C2).

reachable_positions_difference(Board, P1C1, P1C2, P2C1, P2C2, player2, ReachablePositionsDifference) :-
    dfs(Board, P1C1, P1C1, P1C2, P2C1, P2C2, ReachablePositionsFromP1C1),
    dfs(Board, P1C2, P1C1, P1C2, P2C1, P2C2, ReachablePositionsFromP1C2),
    dfs(Board, P2C1, P1C1, P1C2, P2C1, P2C2, ReachablePositionsFromP2C1),
    dfs(Board, P2C2, P1C1, P1C2, P2C1, P2C2, ReachablePositionsFromP2C2),
    length(ReachablePositionsFromP1C1, LengthP1C1),
    length(ReachablePositionsFromP1C2, LengthP1C2),
    length(ReachablePositionsFromP2C1, LengthP2C1),
    length(ReachablePositionsFromP2C2, LengthP2C2),
    ReachablePositionsDifference is (LengthP1C1 + LengthP1C2) - (LengthP2C1 + LengthP2C2).

piece_moves_difference(Board, P1C1, P1C2, P2C1, P2C2, player1, PieceMovesDifference) :-
    valid_moves_pieces(player1, Board, P1C1, P1C2, P2C1, P2C2, PieceMovesPlayer1),
    valid_moves_pieces(player2, Board, P1C1, P1C2, P2C1, P2C2, PieceMovesPlayer2),
    length(PieceMovesPlayer1, LengthPlayer1),
    length(PieceMovesPlayer2, LengthPlayer2),
    PieceMovesDifference is LengthPlayer2 - LengthPlayer1.

piece_moves_difference(Board, P1C1, P1C2, P2C1, P2C2, player2, PieceMovesDifference) :-
    valid_moves_pieces(player1, Board, P1C1, P1C2, P2C1, P2C2, PieceMovesPlayer1),
    valid_moves_pieces(player2, Board, P1C1, P1C2, P2C1, P2C2, PieceMovesPlayer2),
    length(PieceMovesPlayer1, LengthPlayer1),
    length(PieceMovesPlayer2, LengthPlayer2),
    PieceMovesDifference is LengthPlayer1 - LengthPlayer2.

value([_, Board, P1C1, P1C2, P2C1, P2C2 | _], player1, Value) :-
    reachable_positions_difference(Board, P1C1, P1C2, P2C1, P2C2, player1, ReachablePositionsDifference),
    piece_moves_difference(Board, P1C1, P1C2, P2C1, P2C2, player1, PieceMovesDifference),
    Value is ReachablePositionsDifference + PieceMovesDifference.


value([_, Board, P1C1, P1C2, P2C1, P2C2 | _], player2, Value) :-
    reachable_positions_difference(Board, P1C1, P1C2, P2C1, P2C2, player2, ReachablePositionsDifference),
    piece_moves_difference(Board, P1C1, P1C2, P2C1, P2C2, player2, PieceMovesDifference),
    Value is ReachablePositionsDifference + PieceMovesDifference.

valid_move_piece(Board, (OldCol, OldRow), (NewCol, NewRow), P1C1, P1C2, P2C1, P2C2) :-
    (NewCol, NewRow) \= P1C1,
    (NewCol, NewRow) \= P1C2,
    (NewCol, NewRow) \= P2C1,
    (NewCol, NewRow) \= P2C2,
    length(Board, NumRows),
    between(1, NumRows, NewCol),
    between(1, NumRows, NewRow),
    AdjustedOldRow is NumRows - OldRow + 1,
    AdjustedNewRow is NumRows - NewRow + 1,
    nth1(AdjustedOldRow, Board, OldRowList),
    nth1(OldCol, OldRowList, OldCell),
    nth1(AdjustedNewRow, Board, NewRowList),
    nth1(NewCol, NewRowList, NewCell),
    abs(NewCell - OldCell) =< 1,
    NewCell > 0.

valid_moves_pieces(player1, Board, P1C1, P1C2, P2C1, P2C2, ListOfMoves) :-
    Pieces = [P1C1, P1C2],
    length(Board, NumRows),
    findall(((OldCol, OldRow), (NewCol, NewRow)),
          (   
            member((OldCol, OldRow), Pieces),
            direction_offset(Direction, (ColOffset, RowOffset)),
            NewCol is OldCol + ColOffset,
            NewRow is OldRow + RowOffset,
            valid_move_piece(Board, (OldCol, OldRow), (NewCol, NewRow), P1C1, P1C2, P2C1, P2C2)
          ),
          ListOfMoves).
valid_moves_pieces(player2, Board, P1C1, P1C2, P2C1, P2C2, ListOfMoves) :-
    Pieces = [P2C1, P2C2],
    length(Board, NumRows),
    findall(((OldCol, OldRow), (NewCol, NewRow)),
          (   
            member((OldCol, OldRow), Pieces),
            direction_offset(Direction, (ColOffset, RowOffset)),
            NewCol is OldCol + ColOffset,
            NewRow is OldRow + RowOffset,
            valid_move_piece(Board, (OldCol, OldRow), (NewCol, NewRow), P1C1, P1C2, P2C1, P2C2)
          ),
          ListOfMoves).

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
direction_offset('upleft', (-1, 1)).
direction_offset('upright', (1, 1)).
direction_offset('downleft', (-1, -1)).
direction_offset('downright', (1, -1)).