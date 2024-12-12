% Validate a Move
valid_move(Row, Col, Grid) :-
    nth1(Row, Grid, CurrentRow),
    nth1(Col, CurrentRow, Cell),
    Cell = ' '. % The cell must be empty

% Check for Four Consecutive Symbols (in a List)
four_consecutive(List, Symbol) :-
    append(_, [Symbol, Symbol, Symbol, Symbol | _], List).

% Check Rows for Victory
check_rows(Grid, Symbol) :-
    member(Row, Grid),
    four_consecutive(Row, Symbol).

% Check Columns for Victory
check_columns(Grid, Symbol) :-
    transpose(Grid, TransposedGrid),
    check_rows(TransposedGrid, Symbol).

% Check Diagonals for Victory
check_diagonals(Grid, Symbol) :-
    diagonal(Grid, Diagonal1),
    four_consecutive(Diagonal1, Symbol);
    reverse(Grid, ReversedGrid),
    diagonal(ReversedGrid, Diagonal2),
    four_consecutive(Diagonal2, Symbol).

% Extract Main Diagonal
diagonal(Grid, Diagonal) :-
    findall(Cell, (nth1(I, Grid, Row), nth1(I, Row, Cell)), Diagonal).

% Check for Victory
check_victory(Grid, Symbol) :-
    check_rows(Grid, Symbol);
    check_columns(Grid, Symbol);
    check_diagonals(Grid, Symbol).
