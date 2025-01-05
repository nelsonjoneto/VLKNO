% filepath: /home/nelson/PFL/prolog/project2/src/io.pl

:- use_module(library(between)).
:- use_module(library(lists)).

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

read_direction(Directions, Direction) :-
    read_string_aux([], DirectionList),
    atom_chars(Direction, DirectionList),
    member(Direction, Directions),
    !.

read_direction(Directions, Direction) :-
    write('Invalid Direction. Please try again. Valid directions: '),
    write(Directions), nl,
    clear_buffer,
    read_player_piece(Directions, Direction).

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

