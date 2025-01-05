:- ensure_loaded('game.pl').

/*
game_configuration(-GameConfig)
Description: Configures the game by choosing the game mode, 
player names, difficulty levels, and board size.
*/
game_configuration([Player1Type, Player2Type, Player1Name, Player2Name, BoardSize, Difficulty1-Difficulty2]) :-
    choose_gamemode(Player1Type, Player2Type),
    choose_player_names(Player1Type, Player2Type, Player1Name, Player2Name),
    choose_difficulty(player1, Player1Type, Difficulty1),
    choose_difficulty(player2, Player2Type, Difficulty2),
    choose_board_size(BoardSize).

/*
choose_gamemode(-Player1Type, -Player2Type)
Description: Prompts the user to choose the game mode 
and sets the player types accordingly.
*/
choose_gamemode(Player1Type, Player2Type) :-
    read_menu_option(1, 4, Choice),
    handle_gamemode_choice(Choice, Player1Type, Player2Type).

/*
handle_gamemode_choice(+Choice, -Player1Type, -Player2Type)
Description: Sets the player types based on the chosen game mode.
*/
handle_gamemode_choice(1, human, human) :-
    write('You chose Human vs Human.'), nl.
handle_gamemode_choice(2, human, computer) :-
    write('You chose Human vs Computer.'), nl.
handle_gamemode_choice(3, computer, human) :-
    write('You chose Computer vs Human.'), nl.
handle_gamemode_choice(4, computer, computer) :-
    write('You chose Computer vs Computer.'), nl.

/*
choose_player_names(+Player1Type, +Player2Type, -Player1Name, -Player2Name)
Description: Prompts the user to enter player names based on the player types.
Named are predetermined for computer players
*/
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

/*
choose_difficulty(+Player, +PlayerType, -Difficulty)
Description: Prompts the user to choose the difficulty level for computer players.
It resturns 0 for human players and allows the user to choose between 1 and 2 (easy and hard) for computer players.
*/
choose_difficulty(_, human, 0).
choose_difficulty(player1, computer, Difficulty1) :-
    display_difficulty_pc1,
    read_menu_option(1, 2, Difficulty1).
choose_difficulty(player2, computer, Difficulty2) :-
    display_difficulty_pc2,
    read_menu_option(1, 2, Difficulty2).

/*
choose_board_size(-BoardSize)
Description: Prompts the user to choose between the classic board size (5x5) 
and a custom board size.
*/
choose_board_size(BoardSize) :-
    display_board_size,
    read_menu_option(1,2, Choice),
    handle_board_size_choice(Choice, BoardSize).

/*
handle_board_size_choice(+Choice, -BoardSize)
Description: Sets the board size between 4 and 7, based on the user's choice.
*/
handle_board_size_choice(1, 5).
handle_board_size_choice(2, BoardSize) :-
    write('Enter the size of the board (NxN, between 4 and 7): '),
    read_menu_option(4,7, BoardSize).

/*
choose_piece_movement(+Player, +Board, +P1C1, +P1C2, +P2C1, +P2C2, -Move)
Description: Prompts the user to choose a piece and a direction to move it and validates the choices.
*/

%player1
choose_piece_movement(player1, Board, P1C1, P1C2, P2C1, P2C2, (Piece, Direction)) :-
    valid_moves_pieces(player1, Board, P1C1, P1C2, P2C1, P2C2, ListOfMoves),
    setof(Piece, D^member((Piece, D), ListOfMoves), ValidPieces),
    write('Choose a piece to move ('), write(ValidPieces), write(').'), nl,
    read_player_piece(ValidPieces, Piece),
    findall(Direction, member((Piece, Direction), ListOfMoves), ValidDirections),
    write('Choose the direction ('), write(ValidDirections), write(').'), nl,
    read_direction(ValidDirections, Direction).

%player2
choose_piece_movement(player2, Board, P1C1, P1C2, P2C1, P2C2, (Piece, Direction)) :-
    valid_moves_pieces(player2, Board, P1C1, P1C2, P2C1, P2C2, ListOfMoves),
    setof(Piece, D^member((Piece, D), ListOfMoves), ValidPieces),
    write('Choose a piece to move ('), write(ValidPieces), write(').'), nl,
    read_player_piece(ValidPieces, Piece),
    findall(Direction, member((Piece, Direction), ListOfMoves), ValidDirections),
    write('Choose the direction ('), write(ValidDirections), write(').'), nl,
    read_direction(ValidDirections, Direction).

/*
choose_stone_movement(+Board, +P1C1, +P1C2, +P2C1, +P2C2, +NewPiecePos, -StoneMove)
Description: Prompts the user to choose a stack to remove a stone from and a stack to place the stone in and validates the choices.
*/
choose_stone_movement(Board, P1C1, P1C2, P2C1, P2C2, NewPiecePos, (OldStonePos, NewStonePos)) :-
    find_min_height_stack(Board, NewPiecePos, P1C1, P1C2, P2C1, P2C2, MinHeightStack),
    repeat,
    write('Choose a stack to remove a stone from (Column-Row)'), nl,
    read_pos(OldStonePos),
    ensure_valid_least_stone_position(Board, OldStonePos, MinHeightStack, NewPiecePos, P1C1, P1C2, P2C1, P2C2), !,
    repeat,
    write('Choose a stack to place a stone in (Column-Row)'), nl,
    read_pos(NewStonePos),
    ensure_valid_unoccupied_position(Board, OldStonePos, NewPiecePos, NewStonePos, P1C1, P1C2, P2C1, P2C2),
    !.

/*
ensure_valid_least_stone_position(+Board, +OldStonePos, +MinHeightStack, +NewPiecePos, +P1C1, +P1C2, +P2C1, +P2C2)
Description: Ensures that the chosen stack to remove a stone from is valid.
*/
ensure_valid_least_stone_position(Board, OldStonePos, MinHeightStack, NewPiecePos, P1C1, P1C2, P2C1, P2C2) :-
    valid_least_stone_position(Board, OldStonePos, MinHeightStack, NewPiecePos, P1C1, P1C2, P2C1, P2C2).
ensure_valid_least_stone_position(_, _, _, _, _, _, _, _) :-
    write('Invalid stack, try again.'), nl,
    write('You can only pick up a stone from one of the smallest unoccupied stacks on the board, except the one you just moved your piece from.'), nl,
    fail.

/*
ensure_valid_unoccupied_position(+Board, +OldStonePos, +NewPiecePos, +NewStonePos, +P1C1, +P1C2, +P2C1, +P2C2)
Description: Ensures that the chosen stack to place a stone in is valid.
*/
ensure_valid_unoccupied_position(Board, OldStonePos, NewPiecePos, NewStonePos, P1C1, P1C2, P2C1, P2C2) :-
    valid_unoccupied_position(Board, OldStonePos, NewPiecePos, NewStonePos, P1C1, P1C2, P2C1, P2C2).
ensure_valid_unoccupied_position(_, _, _, _, _, _, _, _) :-
    write('Invalid stack, try again.'), nl,
    write('You can only place a stone on an unoccupied stack of 1+ stones, except the one you just moved your piece from.'), nl,
    fail.

/*
print_winner_and_ask_replay(+Winner)
Description: Prints the winner of the game and asks the user if they want to play again.
If so, the game is restarted.
*/
print_winner_and_ask_replay(Winner) :-
    write('Game over! Winner: '), write(Winner), nl,
    write('Do you want to play again? (yes/no): '), nl,
    read_replay_answer(Answer),
    check_replay_answer(Answer).

/*
check_replay_answer(+Answer)
Description: Checks the user's answer to whether they want to play again and either restarts the game or ends it.
*/
check_replay_answer(yes) :-
    play.
check_replay_answer(no) :-
    write('Thank you for playing! Goodbye!'), nl.

/*
read_any_key(+Move)
Description: Informs the user about the move the computer just made and prompts them to press any key to continue.
*/
read_any_key((Piece, Direction, (OldCol, OldRow), (NewCol, NewRow))) :-
    write('Hmmm... '), nl,
    write('I think I\'ll move the piece '), write(Piece), write(' in direction '), write(Direction), nl,
    write('I think I\'ll move a stone from ('), write(OldCol), write(','), write(OldRow), write(') to ('), write(NewCol), write(','), write(NewRow), write(')'), nl,
    write('Press any key to continue...'), nl,
    get_char(_).

/*
clear_buffer
Description: Clears the input buffer by reading characters until a newline character is encountered.
*/
clear_buffer :-
    repeat,
    get_char(Char),
    Char = '\n',
    !.

/*
read_pos(-Position)
Description: Reads a position in the format Column-Row. 
Invalid positions are handled by prompting the user to re-enter a valid position.
*/

%valid position
read_pos((Col, Row)) :-
    read_column(Col),
    read_dash,
    read_row(Row),
    !.

% An error has occurred, try to read a move from the beginning
read_pos((Col, Row)) :-
    write('Invalid position, please try again.'), nl,   
    clear_buffer,
    read_pos((Col, Row)).

/*
read_dash
Description: Reads a dash character.
*/
read_dash :-
    peek_char(Char),
    Char = '-',
    get_char(_).

/*
read_column(-Col)
Description: Reads a column number.
*/
read_column(Col) :-
    read_number(0, Col).

/*
read_row(-Row)
Description: Reads a row number and clears the buffer.
*/
read_row(Row) :-
    read_number(0, Row),
    peek_char(Char),
    Char = '\n',
    clear_buffer.

/*
read_number(+Acc, -Number)
Description: Reads a number from the input.
*/
read_number(Acc, Number) :-
    peek_code(Code),
    between(48, 57, Code),
    get_code(_),
    !,
    NewAcc is Acc * 10 + Code - 48,
    read_number(NewAcc, Number).

%number is returned when a char that is not a number is found
read_number(Number, Number) :-
    Number \= 0.

/*
read_menu_option(+Min, +Max, -Option)
Description: Reads a menu option within the specified range.
*/
read_menu_option(Min, Max, Option) :-
    read_number(0, Option),
    between(Min, Max, Option),
    peek_char('\n'),
    get_char(_),  
    !.
%error, prompt the user to choose a valid option.
read_menu_option(Min, Max, Option) :-
    format('Invalid choice, please choose a valid option (~d-~d).~n', [Min, Max]),
	clear_buffer,
    read_menu_option(Min, Max, Option).

/*
read_replay_answer(-Answer)
Description: Reads the user's answer to whether they want to play again.
*/
read_replay_answer(Answer) :-
    read_string_aux([], AnswerList),
    atom_chars(Answer, AnswerList),
    member(Answer, [yes, no]),
    get_char(_),
    !.

%error, prompt the user to choose a valid option.
read_replay_answer(Answer) :-
    write('Invalid awnser. Please try again.'), nl,
    clear_buffer,
    read_replay_answer(Answer).

/*
read_player_piece(+Pieces, -Piece)
Description: Reads the player's chosen piece from the input and validates it.
*/
read_player_piece(Pieces, Piece) :-
    read_string_aux([], PieceList),
    atom_chars(Piece, PieceList),
    member(Piece, Pieces),
    get_char(_),
    !.

%error, prompt the user to choose a valid piece.
read_player_piece(Pieces, Piece) :-
    write('Invalid piece. Please try again. Valid pieces: '),
    write(Pieces), nl,
    clear_buffer,
    read_player_piece(Pieces, Piece).

/*
read_direction(+Directions, -Direction)
Description: Reads the player's chosen direction from the input and validates it.
*/
read_direction(Directions, Direction) :-
    read_string_aux([], DirectionList),
    atom_chars(Direction, DirectionList),
    member(Direction, Directions),
    get_char(_),
    !.

%error, prompt the user to choose a valid direction.
read_direction(Directions, Direction) :-
    write('Invalid Direction. Please try again. Valid directions: '),
    write(Directions), nl,
    clear_buffer,
    read_direction(Directions, Direction).

/*
read_playername(-Name)
Description: Reads a player name from the input according to the specified requirements.
*/
read_playername(Name) :-
    read_string_aux([], NameList),
    atom_chars(Name, NameList),
    get_char(_),
    !.

%error, prompt the user to choose a valid name.
read_playername(Name) :-
    write('Invalid player name. Please try again.'), nl,
    clear_buffer,
    read_playername(Name).

/*
read_string_aux(+Acc, -StringList)
Description: Auxiliary predicate to read a string of letters and numbers, 
ensuring the length does not exceed 20 characters.
*/
read_string_aux(Acc, StringList) :-
    peek_code(Code),
    between(48, 57, Code),      % Numbers 0-9
    length(Acc, Length),
    Length < 20,
    get_char(Char),
    !,
    read_string_aux([Char | Acc], StringList).

read_string_aux(Acc, StringList) :-
    peek_code(Code),
    between(65, 90, Code),     % Uppercase letters A-Z
    length(Acc, Length),
    Length < 20,
    get_char(Char),
    !,
    read_string_aux([Char | Acc], StringList).

read_string_aux(Acc, StringList) :-
    peek_code(Code),
    between(97, 122, Code),    % Lowercase letters a-z
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

