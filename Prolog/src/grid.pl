:- use_module(library(lists)).

% Initialize Grid for Different Sizes
init_grid(4, Grid) :- create_grid(4, Grid).
init_grid(5, Grid) :- create_grid(5, Grid).
init_grid(6, Grid) :- create_grid(6, Grid).
init_grid(8, Grid) :- create_grid(8, Grid).

% Helper to Create NxN Grid Filled with Spaces
create_grid(Size, Grid) :-
    length(Grid, Size),
    maplist(init_row(Size), Grid).

% Initialize a Row of Size N
init_row(Size, Row) :-
    length(Row, Size),
    maplist(=(' '), Row).

% Print the Grid
print_grid(Grid) :-
    nl,
    maplist(print_row, Grid).

% Print a Single Row
print_row(Row) :-
    write('| '),
    maplist(write_cell, Row),
    write('|'), nl.

% Write a Single Cell
write_cell(Cell) :-
    write(Cell), write(' ').

% Update the Grid
update_grid(Row, Col, Symbol, Grid, UpdatedGrid) :-
    nth1(Row, Grid, OldRow),
    replace_in_row(Col, Symbol, OldRow, UpdatedRow),
    replace_in_grid(Row, UpdatedRow, Grid, UpdatedGrid).

% Replace a Value in a Row
replace_in_row(Col, Value, Row, UpdatedRow) :-
    nth1(Col, Row, _, Rest),         % Remove the old value
    nth1(Col, UpdatedRow, Value, Rest). % Insert the new value

% Replace a Row in the Grid
replace_in_grid(Row, NewRow, Grid, UpdatedGrid) :-
    nth1(Row, Grid, _, Rest),        % Remove the old row
    nth1(Row, UpdatedGrid, NewRow, Rest). % Insert the new row
