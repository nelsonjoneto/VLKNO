
:- ensure_loaded('game.pl').
/*
min_list(+List, -Min)
Description: Finds the minimum element in a list.
*/
min_list([Min], Min).
min_list([H|T], Min) :-
    min_list(T, MinTail),
    min(H, MinTail, Min).

/*
min(+X, +Y, -Min)
Description: Helper predicate to find the minimum of two numbers.
*/
min(X, Y, X) :- X =< Y.
min(X, Y, Y) :- X > Y.

/*
replace(+List, +Index, +Element, -NewList)
Description: Replaces the element at the specified index in the list with a new element.
*/
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

/*
replace_nested(+NestedList, +RowIndex, +ColIndex, +NewValue, -NewNestedList)
Description: Replaces the element at the specified row and column index in a nested list with a new value.
*/
replace_nested([Row|RestRows], 1, Col, NewValue, [NewRow|RestRows]) :-
    replace(Row, Col, NewValue, NewRow).
replace_nested([Row|RestRows], RowIndex, Col, NewValue, [Row|NewRestRows]) :-
    RowIndex > 1,
    RowIndex1 is RowIndex - 1,
    replace_nested(RestRows, RowIndex1, Col, NewValue, NewRestRows).

/*
flatten(+NestedList, -FlatList)
Description: Flattens a nested list into a flat list.
*/
flatten([], []).
flatten([L|Ls], FlatL) :-
    !,
    flatten(L, NewL),
    flatten(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten(L, [L]).

/*
number_of_digits(+Number, -Digits)
Description: Calculates the number of digits in a number.
*/
number_of_digits(0, 1) :- !.
number_of_digits(Number, Digits) :-
    Number > 0,
    number_of_digits_recursive(Number, Digits).

number_of_digits_recursive(0, 0) :- !.
number_of_digits_recursive(Number, Digits) :-
    Number > 0,
    NextNumber is Number // 10,
    number_of_digits_recursive(NextNumber, NextDigits),
    Digits is NextDigits + 1.

/*
max_stack_size(+Board, -MaxStackSize)
Description: Finds the maximum stack size on the board and calculates the number of digits in it.
*/
max_stack_size(Board, MaxStackSize) :-
    flatten(Board, FlatBoard),
    max_member(MaxStack, FlatBoard),
    number_of_digits(MaxStack, MaxStackSize).