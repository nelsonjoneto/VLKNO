% filepath: /home/nelson/PFL/prolog/project2/src/io.pl

:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(io).

game_configuration([Player1Type, Player1Type, Player1Name, Player2Name, BoardSize, Difficulty1-Difficulty2]) :-
    choose_gamemode(Player1Type, Player2Type),
    choose_player_names(Player1Type, Player2Type, Player1Name, Player2Name),
    choose_difficulty(player1, Player1Type, Difficulty1),
    choose_difficulty(player2, Player2Type, Difficulty2),
    choose_board_size(BoardSize).

choose_gamemode(Player1Type, Player2Type) :-
    read_menu_option(1, 5, Choice),
    handle_gamemode_choice(Choice, Player1Type, Player2Type).

handle_gamemode_choice(1, human, human) :-
    write('You chose Human vs Human'), nl.
handle_gamemode_choice(2, human, computer) :-
    write('You chose Human vs Computer'), nl.
handle_gamemode_choice(3, computer, human) :-
    write('You chose Computer vs Human'), nl.
handle_gamemode_choice(4, computer, computer) :-
    write('You chose Computer vs Computer'), nl.
handle_gamemode_choice(5, _, _) :-
    write('Exiting the game. Goodbye!').

choose_player_names(human, human, Player1Name, Player2Name) :-
    write('Enter name for Player 1 (letters and numbers only, max 20 characters).'), nl,
    read_playername(Player1Name),
    write('Enter name for Player 2 (letters and numbers only, max 20 characters).'), nl,
    read_playername(Player2Name).
choose_player_names(human, computer, Player1Name, 'PC2') :-
    write('Enter name for Player 1 (letters and numbers only, max 20 characters).'), nl,
    read_playername(Player1Name).
choose_player_names(computer, human, 'PC1', Player2Name) :-
    write('Enter name for Player 2 (letters and numbers only, max 20 characters).'), nl,
    read_playername(Player2Name).
choose_player_names(computer, computer, 'PC1', 'PC2').

choose_difficulty(_, human, 0).
choose_difficulty(player1, computer, Difficulty1) :-
    display_difficulty_pc1,
    read_menu_option(1, 2, Difficulty1).
choose_difficulty(player2, computer, Difficulty2) :-
    display_difficulty_pc2,
    read_menu_option(1, 2, Difficulty2).

choose_board_size(BoardSize) :-
    display_board_size,
    read_menu_option(1,2, Choice),
    handle_board_size_choice(Choice, BoardSize).

handle_board_size_choice(1, 5).
handle_board_size_choice(2, BoardSize) :-
    write('Enter the size of the board (NxN, between 4 and 7): '),
    read_menu_option(4,7, BoardSize).

choose_piece_movement(player1, Board, P1C1, P1C2, P2C1, P2C2, (OldPiecePos, NewPiecePos)) :-
    write('Choose a piece to move (w1, w2): '), nl,
    read_player_piece([w1, w2], Piece),
    write('Choose the direction (up, down, left, right, upleft, upright, downleft, downright) or "back" to choose another piece: '), nl,
    read_direction(Direction),
    get_piece_position(player1, Piece, P1C1, P1C2, OldPiecePos),
    ensure_valid_move(Board, OldPiecePos, Direction, P1C1, P1C2, P2C1, P2C2, NewPiecePos),
    !.

get_piece_position(player1, w1, P1C1, _, P1C1).
get_piece_position(player1, w2, _, P1C2, P1C2).
get_piece_position(player2, b1, P2C1, _, P2C1).
get_piece_position(player2, b2, _, P2C2, P2C2).

clear_buffer :-
    repeat,
    get_char(Char),
    Char = '\n',
    !.

% Read a position in the format Column-Row
%read_pos(Col-Row).
read_pos(Col-Row) :-
    read_column(Col),
    read_dash,
    read_row(Row),
    !.

% An error has occurred, try to read a move from the beginning
read_pos(Col-Row) :-
    write('Invalid position, please try again.'), nl,   
    clear_buffer,
    read_pos(Col-Row).

read_dash :-
    peek_char(Char),
    Char = '-',
    get_char(_).

% Read a column number
read_column(Col) :-
    read_number(0, Col).

read_row(Row) :-
    read_number(0, Row),
    peek_char(Char),
    Char = '\n',
    clear_buffer.
    
read_number(Acc, Number) :-
    peek_code(Code),
    between(48, 57, Code),
    get_code(_),
    !,
    NewAcc is Acc * 10 + Code - 48,
    read_number(NewAcc, Number).

read_number(Number, Number) :-
    Number \= 0.

%read_menu_option(1, 32, Option).
read_menu_option(Min, Max, Option) :-
    read_number(0, Option),
    between(Min, Max, Option),
    peek_char('\n'),
    get_char(_),  
    !.

read_menu_option(Min, Max, Option) :-
    format('Invalid choice, please choose a valid option (~d-~d).~n', [Min, Max]),
	clear_buffer,
    read_menu_option(Min, Max, Option).

read_player_piece(Pieces, Piece) :-
    read_string_aux([], PieceList),
    atom_chars(Piece, PieceList),
    member(Piece, Pieces),
    !.

read_player_piece(Pieces, Piece) :-
    write('Invalid piece. Please try again. Valid pieces: '),
    write(Pieces), nl,
    clear_buffer,
    read_player_piece(Pieces, Piece).

read_player_piece(Pieces, Piece) :-
    write('Invalid piece. Please try again. Valid pieces: '),
    write(Pieces), nl,
    clear_buffer,
    read_player_piece(Pieces, Piece).

read_direction(Direction) :-
    read_string_aux([], DirectionList),
    atom_chars(Direction, DirectionList),
    member(Direction, [up, down, left, right, upleft, upright, downleft, downright, back]),
    !.

read_direction(Direction) :-
    write('Invalid Direction. Please try again. Valid directions: up, down, left, right, upleft, upright, downleft, downright, or "back" to choose another piece.'), nl,
    clear_buffer,
    read_player_piece(Direction).

read_playername(Name) :-
    read_string_aux([], NameList),
    atom_chars(Name, NameList),
    get_char(_),
    !.

read_playername(Name) :-
    write('Invalid player name. Please try again.'), nl,
    clear_buffer,
    read_playername(Name).

read_string_aux(Acc, StringList) :-
    peek_code(Code),
    between(48, 57, Code),       
    length(Acc, Length),
    Length < 20,
    get_char(Char),
    !,
    read_string_aux([Char | Acc], StringList).

read_string_aux(Acc, StringList) :-
    peek_code(Code),
    between(65, 90, Code),      
    length(Acc, Length),
    Length < 20,
    get_char(Char),
    !,
    read_string_aux([Char | Acc], StringList).

read_string_aux(Acc, StringList) :-
    peek_code(Code),
    between(97, 122, Code),   
    length(Acc, Length),
    Length < 20, 
    get_char(Char),
    !,
    read_string_aux([Char | Acc], StringList).

read_string_aux(Acc, StringList) :-
    peek_char('\n'),
    reverse(Acc, StringList),
    length(StringList, Length),
    Length > 0, 
    !.

