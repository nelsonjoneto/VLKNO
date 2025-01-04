% filepath: /home/nelson/PFL/prolog/project2/src/io.pl

:- use_module(library(between)).

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






/*
    ler opçoes de menu (done)
    ler coordenadas 
    ler nome de jogador
    ler direção
*/


read_menu_option(Option, Low, Up) :-
    repeat,
    read_menu_option_helper(0, Low, Up, Option), !.

% Auxiliary predicate to read a number
read_menu_option_helper(NumberAccumulator, Low, Up, Option) :-
    peek_code(Code),
    between(48, 57, Code),
    get_code(_),
    !,
    NewNumberAccumulator is NumberAccumulator * 10 + Code - 48,
    read_menu_option_helper(NewNumberAccumulator, Option, Low, Up).

read_menu_option_helper(Option, Low, Up, Option) :-
    between(Low, Up, Option),
    peek_char(Char),
    Char = '\n',
    clear_buffer,
    !.

read_menu_option_helper(_, Low, Up, _) :-
    format('Invalid choice, please choose a valid option (~d-~d).~n', [Low, Up]),
	clear_buffer,
    fail.

clear_buffer :-
    repeat,
    get_char(Char),
    Char = '\n',
    !.