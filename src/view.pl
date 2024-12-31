
% Display the game state
display_game([player1, Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize, IsMoving, _ , _, _, Player1Name | _]) :-
    display_board(Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize),
    display_stones(Board, P1C1, P1C2, P2C1, P2C2),
    format('Current player: ~w~n', [Player1Name]).
display_game([player2, Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize, IsMoving, _ , _, _, _, Player2Name]) :-
    display_board(Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize),
    display_stones(Board, P1C1, P1C2, P2C1, P2C2),
    format('Current player: ~w~n', [Player2Name]).

%value pode ser o valor total de stones que rodeiam as peças do adversário, tendo em conta o numero de moves disponiveis, algo tipo stones per Move
%pode ser a jogada que gera maior diferença entre esse value para mim e para ele
%acrescentar boolean isMoving

% Display the board with pieces and numeration
display_board(Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize) :-
    length(Board, N),
    number_of_digits(N, MaxRowDigits),
    custom_numlist(1, N, Cols),
    nl, format('~*c', [MaxRowDigits + 1, 32]), print_column_numbers(Cols, MaxStackSize), nl,
    format('~*c', [MaxRowDigits + 1, 32]), print_separator_line(N, MaxStackSize), nl,
    reverse(Cols, RevRows),
    maplist(display_row(Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize, MaxRowDigits), RevRows).

% Print the column numbers above the cells
print_column_numbers([], _).
print_column_numbers([Col|Cols], MaxStackSize) :-
    number_of_digits(Col, Digits),
    BaseWidth is 2 * ((MaxStackSize + 1) // 2) + 6,
    PaddingBeginning is BaseWidth // 2,
    PaddingEnd is BaseWidth - PaddingBeginning - Digits,
    format('~*c~w~*c', [PaddingBeginning, 32, Col, PaddingEnd, 32]),
    print_column_numbers(Cols, MaxStackSize).

% Print the separator line
print_separator_line(0, _) :- write('-').
print_separator_line(N, MaxStackSize) :-
    N > 0,
    Length is 2 * ((MaxStackSize + 1) // 2) + 6,
    print_n_chars(Length, '-'),
    N1 is N - 1,
    print_separator_line(N1, MaxStackSize).

% Helper predicate to print a given character N times
print_n_chars(0, _).
print_n_chars(N, Char) :-
    N > 0,
    write(Char),
    N1 is N - 1,
    print_n_chars(N1, Char).

% Display a row of the board
display_row(Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize, MaxRowDigits, Row) :-
    length(Board, TotalRows),
    AdjustedRow is TotalRows - Row + 1,
    nth1(AdjustedRow, Board, BoardRow),
    number_of_digits(Row, RowDigits),
    format('~w~*c|', [Row, (MaxRowDigits - RowDigits) + 1, 32]),
    length(BoardRow, N),
    custom_numlist(1, N, Cols),
    maplist(display_cell(BoardRow, Row, P1C1, P1C2, P2C1, P2C2, MaxStackSize), Cols),
    nl,
    format('~*c', [MaxRowDigits + 1, 32]), print_separator_line(N, MaxStackSize), nl.

% Display a cell of the board
display_cell(BoardRow, Row, P1C1, P1C2, P2C1, P2C2, MaxStackSize, Col) :-
    nth1(Col, BoardRow, Cell),
    format_cell(Cell, Row, Col, P1C1, P1C2, P2C1, P2C2, MaxStackSize).

% Format the cell content based on the maximum stack size
format_cell(Cell, Row, Col, P1C1, P1C2, P2C1, P2C2, MaxStackSize) :-
    ( (Col, Row) = P1C1 -> format_piece('-W1-', MaxStackSize)
    ; (Col, Row) = P1C2 -> format_piece('-W2-', MaxStackSize)
    ; (Col, Row) = P2C1 -> format_piece('-B1-', MaxStackSize)
    ; (Col, Row) = P2C2 -> format_piece('-B2-', MaxStackSize)
    ; Cell = 0 -> format_lava(MaxStackSize)
    ; format_stack(Cell, MaxStackSize)
    ).


format_lava(MaxStackSize) :-
    TotalPadding is 2 * ((MaxStackSize + 1) // 2) + 5,
    format('~*c|', [TotalPadding, 32]).

% Format the piece content
format_piece(Piece, MaxStackSize) :-
    PaddingBeggining is (MaxStackSize - 1) // 2 + 2,
    PaddingEnd is PaddingBeggining - 1,
    format('~*c~w~*c|', [PaddingBeggining, 32, Piece, PaddingEnd, 32]).

% Format the stack content
format_stack(Stack, MaxStackSize) :-
    number_of_digits(Stack, Digits),
    TotalPadding is MaxStackSize + 3 - Digits + MaxStackSize mod 2,
    PaddingEnd is TotalPadding // 2,
    PaddingBeginning is TotalPadding - PaddingEnd,
    format('~*c(~w)~*c|', [PaddingBeginning, 32, Stack, PaddingEnd, 32]).

% Display the number of stones stacked in each occupied position by each piece
display_stones(Board, P1C1, P1C2, P2C1, P2C2) :-
    stones_at_position(Board, P1C1, StonesW1),
    stones_at_position(Board, P1C2, StonesW2),
    stones_at_position(Board, P2C1, StonesB1),
    stones_at_position(Board, P2C2, StonesB2),
    format('Stones at W1(~w): ~w~n', [P1C1, StonesW1]),
    format('Stones at W2(~w): ~w~n', [P1C2, StonesW2]),
    format('Stones at B1(~w): ~w~n', [P2C1, StonesB1]),
    format('Stones at B2(~w): ~w~n', [P2C2, StonesB2]).

% Get the number of stones at a specific position
stones_at_position(Board, (Col, Row), Stones) :-
    length(Board, TotalRows),
    AdjustedRow is TotalRows - Row + 1,
    nth1(AdjustedRow, Board, BoardRow),
    nth1(Col, BoardRow, Stones).

% Custom implementation of numlist/3
custom_numlist(Low, High, List) :-
    (Low =< High ->
        List = [Low | Rest],
        NextLow is Low + 1,
        custom_numlist(NextLow, High, Rest)
    ;
        List = []
    ).

% Display the game menu.
display_menu :-
    nl,
    write('====================================='), nl,
    write('||          Welcome to VLKNO       ||'), nl,
    write('====================================='), nl,
    write('|| 1. Human vs Human               ||'), nl,
    write('|| 2. Human vs Computer            ||'), nl,
    write('|| 3. Computer vs Human            ||'), nl,
    write('|| 4. Computer vs Computer         ||'), nl,
    write('====================================='), nl,
    write('|| Choose an option (1-4):         ||'), nl,
    write('====================================='), nl.