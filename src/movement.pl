:- ensure_loaded('game.pl').

/*
valid_moves_pieces(+Player, +Board, +P1C1, +P1C2, +P2C1, +P2C2, -ListOfMoves)
Description: Finds all valid piece moves for the given player. Each move is represented as a pair (Piece, Direction).
*/

%valid piece moves for player1
valid_moves_pieces(player1, Board, P1C1, P1C2, P2C1, P2C2, ListOfMoves) :-
    findall((Piece, Direction),
          (   
            member(Piece, [w1, w2]),
            get_piece_position(Piece, P1C1, P1C2, P2C1, P2C2, OldPos),
            member(Direction, [up, down, left, right, upleft, upright, downleft, downright]),
            get_new_piece_positon(OldPos, Direction, NewPos),
            valid_move_piece(Board, OldPos, NewPos, P1C1, P1C2, P2C1, P2C2)
          ),
        ListOfMoves).

%valid piece moves for player2
valid_moves_pieces(player2, Board, P1C1, P1C2, P2C1, P2C2, ListOfMoves) :-
    findall((Piece, Direction),
          (   
            member(Piece, [b1, b2]),
            get_piece_position(Piece, P1C1, P1C2, P2C1, P2C2, OldPos),
            member(Direction, [up, down, left, right, upleft, upright, downleft, downright]),
            get_new_piece_positon(OldPos, Direction, NewPos),
            valid_move_piece(Board, OldPos, NewPos, P1C1, P1C2, P2C1, P2C2)
          ),
        ListOfMoves).
/*
move_piece(+GameState, +Piece, +Direction, -NewGameState)
Description: Moves the specified piece in the given direction and updates the game state.
*/

%move piece w1
move_piece([player1, Board, P1C1, P1C2, P2C1, P2C2 | _], (w1, Direction), NewGameState) :-
    get_new_piece_positon(P1C1, Direction, NewPos),
    NewGameState = [player1, Board, NewPos, P1C2, P2C1, P2C2 | _].
%move piece w2
move_piece([player1, Board, P1C1, P1C2, P2C1, P2C2 | _], (w2, Direction), NewGameState) :-
    get_new_piece_positon(P1C2, Direction, NewPos),
    NewGameState = [player1, Board, P1C1, NewPos, P2C1, P2C2 | _].
%move piece b1
move_piece([player2, Board, P1C1, P1C2, P2C1, P2C2 | _], (b1, Direction), NewGameState) :-
    get_new_piece_positon(P2C1, Direction, NewPos),
    NewGameState = [player2, Board, P1C1, P1C2, NewPos, P2C2 | _].
%move piece b2
move_piece([player2, Board, P1C1, P1C2, P2C1, P2C2 | _], (b2, Direction), NewGameState) :-
    get_new_piece_positon(P2C2, Direction, NewPos),
    NewGameState = [player2, Board, P1C1, P1C2, P2C1, NewPos | _].

/*
valid_moves_stones(+Board, +P1C1, +P1C2, +P2C1, +P2C2, +PieceDirection, -ListOfMoves)
Description: Finds all valid stone moves for a given piece move.
*/
valid_moves_stones(Board, P1C1, P1C2, P2C1, P2C2, (Piece, Direction), ListOfMoves) :-
    get_piece_position(Piece, P1C1, P1C2, P2C1, P2C2, OldPiecePos),
    get_new_piece_positon(OldPiecePos, Direction, NewPiecePos),
    find_least_stone_positions(Board, NewPiecePos, P1C1, P1C2, P2C1, P2C2, LeastStonePositions),
    findall((OldStonePos, NewStonePos),
        (
            member(OldStonePos, LeastStonePositions),
            find_unoccupied_positions(Board, OldStonePos, NewPiecePos, P1C1, P1C2, P2C1, P2C2, UnoccupiedPositions),
            member(NewStonePos, UnoccupiedPositions)
        ),
        ListOfMoves).

/*
get_piece_position(+Piece, +P1C1, +P1C2, +P2C1, +P2C2, -Position)
Description: Retrieves the position of the specified piece.
*/
get_piece_position(w1, P1C1, _, _, _, P1C1).
get_piece_position(w2, _, P1C2, _, _, P1C2).
get_piece_position(b1, _, _, P2C1, _, P2C1).
get_piece_position(b2, _, _, _, P2C2, P2C2).

/*
direction_offset(+Direction, -Offset)
Description: Maps a direction to its corresponding offset in terms of column and row changes.
*/
direction_offset(up, (0, 1)).
direction_offset(down, (0, -1)).
direction_offset(left, (-1, 0)).
direction_offset(right, (1, 0)).
direction_offset(upleft, (-1, 1)).
direction_offset(upright, (1, 1)).
direction_offset(downleft, (-1, -1)).
direction_offset(downright, (1, -1)).

/*
get_new_piece_positon(+OldPos, +Direction, -NewPos)
Description: Calculates the new position of a piece based on its current position and the direction of movement.
*/
get_new_piece_positon((OldCol, OldRow), Direction, (NewCol, NewRow)) :-
    direction_offset(Direction, (ColOffset, RowOffset)),
    NewCol is OldCol + ColOffset,
    NewRow is OldRow + RowOffset.

/*
valid_move_piece(+Board, +OldPos, +NewPos, +P1C1, +P1C2, +P2C1, +P2C2)
Description: Checks if moving a piece from OldPos to NewPos is valid.
The new position has to be unnocupied by other pieces, within bounds and the stack of stones (it needs to have at least one stone) 
in it needs to be the same height, 1 stone shorter or 1 stone taller than the stack of stones on the old postion.
*/
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


/*
find_least_stone_positions(+Board, +CurrentPiecePosition, +P1C1, +P1C2, +P2C1, +P2C2, -LeastStonePositions)
Description: Finds the positions unnocupied by pieces with the least number of stones (atleast 1) in board.
It also disregards the position where the piece just moved from.
*/
find_least_stone_positions(Board, CurrentPiecePosition, P1C1, P1C2, P2C1, P2C2, LeastStonePositions) :-
    find_min_height_stack(Board, CurrentPiecePosition, P1C1, P1C2, P2C1, P2C2, MinHeightStack),
    length(Board, NumRows),
    findall((Col, Row), (
        nth1(AdjustedRow, Board, RowList),
        nth1(Col, RowList, MinHeightStack),
        Row is NumRows - AdjustedRow + 1,
        (Col, Row) \= CurrentPiecePosition,
        (Col, Row) \= P1C1,
        (Col, Row) \= P1C2,
        (Col, Row) \= P2C1,
        (Col, Row) \= P2C2
    ), LeastStonePositions).

/*
find_unoccupied_positions(+Board, +StoneMovePrevPosition, +CurrentPiecePosition, +P1C1, +P1C2, +P2C1, +P2C2, -UnoccupiedPositions)
Description: Finds unoccupied positions on the board with atleast one stone, disregarding the position where the piece just moved from
and the position where the stone was removed from.
*/
find_unoccupied_positions(Board, StoneMovePrevPosition, CurrentPiecePosition, P1C1, P1C2, P2C1, P2C2, UnoccupiedPositions) :-
    length(Board, NumRows),
    findall((Col, Row), (
        between(1, NumRows, AdjustedRow),
        nth1(AdjustedRow, Board, RowList),
        between(1, NumRows, Col),
        Row is NumRows - AdjustedRow + 1,
        nth1(Col, RowList, Cell),
        Cell > 0,
        (Col, Row) \= CurrentPiecePosition,
        (Col, Row) \= P1C1,
        (Col, Row) \= P1C2,
        (Col, Row) \= P2C1,
        (Col, Row) \= P2C2,
        StoneMovePrevPosition \= (Col, Row)
    ), UnoccupiedPositions).

/*
find_min_height_stack(+Board, +CurrentPiecePosition, +P1C1, +P1C2, +P2C1, +P2C2, -MinHeightStack)
Description: Finds the minimum height stack of stones on the board, excluding occupied positions 
and the position where the piece just moved from.
*/
find_min_height_stack(Board, CurrentPiecePosition, P1C1, P1C2, P2C1, P2C2, MinHeightStack) :- 
    length(Board, NumRows),
    findall(Cell, (
        nth1(AdjustedRow, Board, RowList),
        nth1(Col, RowList, Cell),
        Row is NumRows - AdjustedRow + 1,
        (Col, Row) \= CurrentPiecePosition,
        (Col, Row) \= P1C1,
        (Col, Row) \= P1C2,
        (Col, Row) \= P2C1,
        (Col, Row) \= P2C2,
        Cell > 0
    ), NonZeroStones),
    min_list(NonZeroStones, MinHeightStack).

/*
valid_unoccupied_position(+Board, +FromPos, +CurrentPosition, +Position, +P1C1, +P1C2, +P2C1, +P2C2)
Description: Checks if a position is unoccupied by pieces, has atleast one stone, 
isn't the position where the piece was moved from or the position where the stone was removed from.
*/
valid_unoccupied_position(Board, FromPos, CurrentPiecePosition, (Col, Row), P1C1, P1C2, P2C1, P2C2) :-
    length(Board, NumRows),
    between(1, NumRows, AdjustedRow),
    nth1(AdjustedRow, Board, RowList),
    between(1, NumRows, Col),
    Row is NumRows - AdjustedRow + 1,
    nth1(Col, RowList, Cell),
    Cell > 0,
    (Col, Row) \= CurrentPiecePosition,
    (Col, Row) \= P1C1,
    (Col, Row) \= P1C2,
    (Col, Row) \= P2C1,
    (Col, Row) \= P2C2,
    FromPos \= (Col, Row).

/*
valid_least_stone_position(+Board, +Position, +CurrentPosition, +UnnocupiedPosition, +P1C1, +P1C2, +P2C1, +P2C2)
Description: Checks if a position is unoccupied by pieces, isn't the position where the piece was moved from.
*/
valid_least_stone_position(Board, (Col, Row), MinHeightStack, CurrentPiecePosition, P1C1, P1C2, P2C1, P2C2) :-
    length(Board, NumRows),
    nth1(AdjustedRow, Board, RowList),
    nth1(Col, RowList, MinHeightStack),
    Row is NumRows - AdjustedRow + 1,
    (Col, Row) \= CurrentPiecePosition,
    (Col, Row) \= P1C1,
    (Col, Row) \= P1C2,
    (Col, Row) \= P2C1,
    (Col, Row) \= P2C2.

/*
update_board(+Board, +OldPos, +NewPos, -NewBoard)
Description: Updates the board by moving a stone from OldPos to NewPos. Adjusts coordinates to start from the bottom left.
*/
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


