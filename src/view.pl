
% Display the game state
display_game([player1, Board, P1C1, P1C2, P2C1, P2C2, _, _, Player1Name | _]) :-
    display_board(Board, P1C1, P1C2, P2C1, P2C2),
    display_stones(Board, P1C1, P1C2, P2C1, P2C2),
    format('Current player: ~w~n', [Player1Name]).
display_game([player2, Board, P1C1, P1C2, P2C1, P2C2, _, _, _, Player2Name]) :-
    display_board(Board, P1C1, P1C2, P2C1, P2C2),
    display_stones(Board, P1C1, P1C2, P2C1, P2C2),
    format('Current player: ~w~n', [Player2Name]).

display_board(Board, P1C1, P1C2, P2C1, P2C2) :-
    length(Board, N),
    number_of_digits(N, MaxRowDigits),
    max_stack_size(Board, MaxStackSize),
    nl, format('~*|', [MaxRowDigits + 1]),
    BaseWidthCols is 2 * ((MaxStackSize + 1) // 2) + 6,
    PaddingBeginning is BaseWidthCols // 2,
    PaddingEnd is BaseWidthCols - PaddingBeginning,
    print_column_numbers(1, N, PaddingBeginning, PaddingEnd),
    TotalWidth is BaseWidthCols * N + 1,
    print_separator(MaxRowDigits, TotalWidth),
    display_row(Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize, MaxRowDigits, N, N, TotalWidth).

display_row(_, _, _, _, _, _, _, 0, _, _).
display_row(Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize, MaxRowDigits, Row, TotalColsRows, TotalWidth) :-
    Row > 0,
    AdjustedRow is TotalColsRows - Row + 1,
    nth1(AdjustedRow, Board, BoardRow),
    number_of_digits(Row, RowDigits),
    format('~w~*c|', [Row, (MaxRowDigits - RowDigits) + 1, 32]),
    display_cell(BoardRow, Row, P1C1, P1C2, P2C1, P2C2, MaxStackSize, 1, TotalColsRows),
    print_separator(MaxRowDigits, TotalWidth),
    NewRow is Row - 1,
    display_row(Board, P1C1, P1C2, P2C1, P2C2, MaxStackSize, MaxRowDigits, NewRow, TotalColsRows, TotalWidth).

display_cell(_, _, _, _, _, _, _, Col, TotalColsRows) :-
    Col > TotalColsRows.
display_cell(BoardRow, Row, P1C1, P1C2, P2C1, P2C2, MaxStackSize, Col, TotalColsRows) :-
    nth1(Col, BoardRow, Cell),
    format_cell(Cell, (Col, Row), P1C1, P1C2, P2C1, P2C2, MaxStackSize),
    NewCol is Col + 1,
    display_cell(BoardRow, Row, P1C1, P1C2, P2C1, P2C2, MaxStackSize, NewCol, TotalColsRows).

format_cell(0, _, _, _, _, _, MaxStackSize) :-
    format_lava(MaxStackSize).
format_cell(Cell, P1C1, P1C1, _, _, _, MaxStackSize) :-
    format_piece('-W1-', MaxStackSize).
format_cell(Cell, P1C2, _, P1C2, _, _, MaxStackSize) :-
    format_piece('-W2-', MaxStackSize).
format_cell(Cell, P2C1, _, _, P2C1, _, MaxStackSize) :-
    format_piece('-B1-', MaxStackSize).
format_cell(Cell, P2C2, _, _, _, P2C2, MaxStackSize) :-
    format_piece('-B2-', MaxStackSize).
format_cell(Cell, _, _, _, _, _, MaxStackSize) :-
    format_stack(Cell, MaxStackSize).

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


% Print the separator line
print_separator(MaxRowDigits, TotalWidth) :-
    nl,
    format('~*|', [MaxRowDigits + 1]),
    format('~*c', [TotalWidth, 45]),
    nl.

% Print the column numbers above the cells
print_column_numbers(CurrentCol, NumCols, PaddingBeginning, PaddingEnd) :-
    CurrentCol =< NumCols,
    number_of_digits(CurrentCol, Digits),
    format('~*c~w~*c', [PaddingBeginning, 32, CurrentCol, PaddingEnd - Digits, 32]),
    NextCol is CurrentCol + 1,
    print_column_numbers(NextCol, NumCols, PaddingBeginning, PaddingEnd).
print_column_numbers(CurrentCol, NumCols, _, _) :-
    CurrentCol > NumCols.

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