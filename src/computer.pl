:- ensure_loaded('game.pl').

/*
dfs(+Board, +StartPos, +P1C1, +P1C2, +P2C1, +P2C2, -Visited)
Description: Depth-first search to find all reachable positions from an initial position (piece position).
*/
dfs(Board, StartPos, P1C1, P1C2, P2C1, P2C2, Visited) :-
    dfs(Board, [StartPos], [], Visited, P1C1, P1C2, P2C1, P2C2).

/*
dfs(+Board, +Stack, +Visited, -FinalVisited, +P1C1, +P1C2, +P2C1, +P2C2)
Description: Base case for depth-first search. When the stack is empty, return the visited nodes.
*/
dfs(_, [], Visited, Visited, _, _, _, _).

/*
dfs(+Board, +Stack, +Visited, -FinalVisited, +P1C1, +P1C2, +P2C1, +P2C2)
Description: Recursive case for depth-first search. Explore neighbors.
*/
dfs(Board, [(OldCol, OldRow)|Rest], Visited, FinalVisited, P1C1, P1C2, P2C1, P2C2) :-
    \+ member((OldCol, OldRow), Visited),
    findall((NewCol, NewRow),
        (
            direction_offset(_, (ColOffset, RowOffset)),
            NewCol is OldCol + ColOffset,
            NewRow is OldRow + RowOffset,
            valid_move_piece(Board, (OldCol, OldRow), (NewCol, NewRow), P1C1, P1C2, P2C1, P2C2),
            \+ member((NewCol, NewRow), Visited)
        ),
        NeighborsPositions),
    append(NeighborsPositions, Rest, NewStack),
    dfs(Board, NewStack, [(OldCol, OldRow)|Visited], FinalVisited, P1C1, P1C2, P2C1, P2C2), !.

/*
dfs(+Board, +Stack, +Visited, -FinalVisited, +P1C1, +P1C2, +P2C1, +P2C2)
Description: If the position is already visited, just proceed with the rest of the stack.
*/
dfs(Board, [(_, _)|Rest], Visited, FinalVisited, P1C1, P1C2, P2C1, P2C2) :-
    dfs(Board, Rest, Visited, FinalVisited, P1C1, P1C2, P2C1, P2C2).

/*
reachable_positions_difference(+Board, +P1C1, +P1C2, +P2C1, +P2C2, +Player, -ReachablePositionsDifference)
Description: Calculates the difference between the number of reachable positions for the pieces of the given player 
and the number of reachable positions for the pieces of his adversary.
*/

%player1.
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

%player2.
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

/*
piece_moves_difference(+Board, +P1C1, +P1C2, +P2C1, +P2C2, +Player, -PieceMovesDifference)
Description: Calculates the difference in the number of valid piece moves for the given player 
and the number of valid piece moves for his adversary.
*/

%player1.
piece_moves_difference(Board, P1C1, P1C2, P2C1, P2C2, player1, PieceMovesDifference) :-
    valid_moves_pieces(player1, Board, P1C1, P1C2, P2C1, P2C2, PieceMovesPlayer1),
    valid_moves_pieces(player2, Board, P1C1, P1C2, P2C1, P2C2, PieceMovesPlayer2),
    length(PieceMovesPlayer1, LengthPlayer1),
    length(PieceMovesPlayer2, LengthPlayer2),
    PieceMovesDifference is LengthPlayer2 - LengthPlayer1.

%player2.
piece_moves_difference(Board, P1C1, P1C2, P2C1, P2C2, player2, PieceMovesDifference) :-
    valid_moves_pieces(player1, Board, P1C1, P1C2, P2C1, P2C2, PieceMovesPlayer1),
    valid_moves_pieces(player2, Board, P1C1, P1C2, P2C1, P2C2, PieceMovesPlayer2),
    length(PieceMovesPlayer1, LengthPlayer1),
    length(PieceMovesPlayer2, LengthPlayer2),
    PieceMovesDifference is LengthPlayer1 - LengthPlayer2.