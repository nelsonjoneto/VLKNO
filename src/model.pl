:- ensure_loaded('game.pl').

/*
initial_state(+GameConfig, -GameState)
Description: Initializes the game state based on the provided game configuration.
The inital state has the board initialized with the specifified size by the player.
Each player's pieces are on opposite corners from eachother. There are two white pieces 
(moved by player 1 in the first turn) and two black pieces. The initial state also stores the player types and names, 
which are necessary for choosing moves and displaying the game.
*/
initial_state([Player1Type, Player2Type, Player1Name, Player2Name, BoardSize | _], GameState) :-
    % Initialize the board and place pawns
    initial_board(BoardSize, Board),
    P1C1 = (BoardSize,1),
    P1C2 = (1,BoardSize),
    P2C2 = (BoardSize,BoardSize),
    GameState = [player1, Board, P1C1, P1C2, (1,1), P2C2, Player1Type, Player2Type, Player1Name, Player2Name].

/*
initial_board(+BoardSize, -Board)
Description: Initializes the board with the given size, filling it with stacks of height 1.
*/
initial_board(BoardSize, Board) :-
    length(Row, BoardSize),
    maplist(=(1), Row),
    length(Board, BoardSize),
    maplist(=(Row), Board).