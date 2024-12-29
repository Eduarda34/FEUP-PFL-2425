/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */


:- use_module(library(random)). 
:- use_module(library(lists)).

:- dynamic cell/4.
:- volatile cell/4.

:- dynamic board2_rows/1.
:- volatile board2_rows/1.

:- dynamic board2_cols/1.
:- volatile board2_cols/1.
/*
   cell(BoardID, RowLabel, ColLabel, Symbol).
   Example: cell(1, a, 1, '.') => Board #1 at (A,1) has '.'.

   board2_rows/1 and board2_cols/1 store the *random* row/col order for Board #2.
*/


/* ----------------------------------------------------------------------
   0. DATA & HELPERS
   ---------------------------------------------------------------------- */

board1_rows([a,b,c,d,e,f]).
board1_cols([1,2,3,4,5,6]).


%% We'll shuffle [a,b,c,d,e,f] and [1,2,3,4,5,6] each time for Board #2.

%% random_shuffled_list(+List, -Shuffled)
%%   Shuffles List using random_select/3.
random_shuffled_list([], []).
random_shuffled_list(List, [X|Xs]) :-
    random_select(X, List, Rest),
    random_shuffled_list(Rest, Xs).

% board_rows(+BoardID, -Rows)
% Retrieves the ordered list of rows for the specified board.
board_rows(1, Rs) :- board1_rows(Rs).
board_rows(2, Rs) :- board2_rows(Rs).

% board_cols(+BoardID, -Cols)
% Retrieves the ordered list of columns for the specified board.
board_cols(1, Cs) :- board1_cols(Cs).
board_cols(2, Cs) :- board2_cols(Cs).

/* ----------------------------------------------------------------------
   1. INITIALIZATION
   ---------------------------------------------------------------------- */

%% init_boards/0
%%   1) Clears old data (cells, plus board2_rows/1, board2_cols/1)
%%   2) Creates Board #1 in normal order
%%   3) Creates Board #2 with random row/col ordering
init_boards :-
    retractall(cell(_,_,_,_)),
    retractall(board2_rows(_)),
    retractall(board2_cols(_)),

    % Board #2: pick a random ordering of A–F, 1–6
    board1_rows(AllRows),
    board1_cols(AllCols),
    random_shuffled_list(AllRows, RndRows),
    random_shuffled_list(AllCols, RndCols),
    assertz(board2_rows(RndRows)),
    assertz(board2_cols(RndCols)),

    % Now actually build the cell(...) facts for both boards:
    create_boards.

%% create_board(+BoardID)
%%   Fills the 6×6 grid with '.' for that board,
%%   in whatever row/col labeling that board uses.
create_boards :-
        board1_rows(Rs1),
        board1_cols(Cs1),
        board2_rows(Rs2),
        board2_cols(Cs2),
        fill_cells(1, Rs1, Cs1),
        fill_cells(2, Rs2, Cs2).

fill_cells(_, [], _).
fill_cells(BoardID, [R|Rs], Cols) :-
    fill_one_row(BoardID, R, Cols),
    fill_cells(BoardID, Rs, Cols).

fill_one_row(_, _, []).
fill_one_row(BoardID, RowLabel, [C|Cs]) :-
    assertz(cell(BoardID, RowLabel, C, '.')),
    fill_one_row(BoardID, RowLabel, Cs).

/* ----------------------------------------------------------------------
   2. PICKING A SPACE (COORDINATE) AND PLACING A SYMBOL
   ---------------------------------------------------------------------- */

%% pick_space(+RowLabel, +ColLabel, +Symbol)
%%   Place Symbol in (RowLabel,ColLabel) on Board #1 or #2
%%   *and* the same coordinate on the other board.
pick_space(RowLabel, ColLabel, Symbol) :-
    valid_cell(1, RowLabel, ColLabel),
    update_cell(1, RowLabel, ColLabel, Symbol),
    update_cell(2, RowLabel, ColLabel, Symbol).

pick_space(_, _, _) :-
    write('Invalid move.'),
    nl.

update_cell(BoardID, R, C, Sym) :-
    retractall(cell(BoardID, R, C, _)),
    assertz(cell(BoardID, R, C, Sym)).

/* ----------------------------------------------------------------------
   3. PRINTING THE BOARDS
   ---------------------------------------------------------------------- */
print_boards :-
        board1_rows(Rs1),
        board1_cols(Cs1),
        board2_rows(Rs2),
        board2_cols(Cs2),
        print_col_header(Cs1),
        print_rows(1, Rs1, Cs1),
        print_col_header(Cs2),
        print_rows(2, Rs2, Cs2).
        
print_col_header(Cols) :-
    write(' '),
    print_cols(Cols),
    nl.

print_cols([]).
print_cols([C|Cs]) :-
    format('  ~w ', [C]),
    print_cols(Cs).

print_rows(_, [], _).
print_rows(BoardID, [R|Rs], Cols) :-
    format(' ~w ', [R]),
    print_cells(BoardID, R, Cols),
    nl,
    print_rows(BoardID, Rs, Cols).

print_cells(_, _, []).
print_cells(BoardID, RowLabel, [C|Cs]) :-
    cell(BoardID, RowLabel, C, Symbol),
    format(' ~w ', [Symbol]),
    print_cells(BoardID, RowLabel, Cs).

/* ----------------------------------------------------------------------
   4. VALIDATIONS
   ---------------------------------------------------------------------- */

% valid_cell(BoardID, RowLabel, ColLabel)
% Ensures the cell contains '.'
valid_cell(BoardID, RowLabel, ColLabel) :-
    cell(BoardID, RowLabel, ColLabel, '.').

adjacent_pair([First, Second | _], First, Second).
adjacent_pair([_ | Rest], First, Second) :-
    adjacent_pair(Rest, First, Second).

/* 6.2. Check for Four in a Row Horizontally */

% has_four_horizontal(+BoardID)
% Succeeds if the specified board has at least one horizontal four-in-a-row.
has_four_horizontal(BoardID) :-
    board_rows(BoardID, Rows),
    board_cols(BoardID, Cols),
    member(Row, Rows),
    consecutive_four(Row, Cols, Symbol),
    Symbol \= '.'.

% consecutive_four(+RowLabel, +Cols, -Symbol)
% Checks if there are four consecutive cells in the row with the same non-dot symbol.
consecutive_four(Row, Cols, Symbol) :-
    append(_, [C1, C2, C3, C4 | _], Cols),
    cell(BoardID, Row, C1, Symbol),
    cell(BoardID, Row, C2, Symbol),
    cell(BoardID, Row, C3, Symbol),
    cell(BoardID, Row, C4, Symbol).

/* 6.3. Check for Four in a Row Vertically */

% has_four_vertical(+BoardID)
% Succeeds if the specified board has at least one vertical four-in-a-row.
has_four_vertical(BoardID) :-
    board_cols(BoardID, Cols),
    board_rows(BoardID, Rows),
    member(Col, Cols),
    consecutive_four_vertical(Col, Rows, Symbol),
    Symbol \= '.'.

% consecutive_four_vertical(+ColLabel, +Rows, -Symbol)
% Checks if there are four consecutive cells in the column with the same non-dot symbol.
consecutive_four_vertical(Col, Rows, Symbol) :-
    append(_, [R1, R2, R3, R4 | _], Rows),
    cell(BoardID, R1, Col, Symbol),
    cell(BoardID, R2, Col, Symbol),
    cell(BoardID, R3, Col, Symbol),
    cell(BoardID, R4, Col, Symbol).

/* 6.4. Check for Four in a Row Diagonally */

% has_four_diagonal(+BoardID)
% Succeeds if the specified board has at least one diagonal four-in-a-row.
has_four_diagonal(BoardID) :-
    board_rows(BoardID, Rows),
    board_cols(BoardID, Cols),
    append(_, [R1, R2, R3, R4 | _], Rows),
    append(_, [C1, C2, C3, C4 | _], Cols),
    % Check main diagonal
    cell(BoardID, R1, C1, Symbol),
    cell(BoardID, R2, C2, Symbol),
    cell(BoardID, R3, C3, Symbol),
    cell(BoardID, R4, C4, Symbol),
    Symbol \= '.'.

% Alternatively, check the anti-diagonal
has_four_diagonal(BoardID) :-
    board_rows(BoardID, Rows),
    board_cols(BoardID, Cols),
    append(_, [R1, R2, R3, R4 | _], Rows),
    append(_, [C4, C3, C2, C1 | _], Cols),
    cell(BoardID, R1, C1, Symbol),
    cell(BoardID, R2, C2, Symbol),
    cell(BoardID, R3, C3, Symbol),
    cell(BoardID, R4, C4, Symbol),
    Symbol \= '.'.

/* 6.5. Check for 2x2 Square */

% has_square(+BoardID)
% Succeeds if the specified board has at least one 2x2 square of the same non-dot symbol.
has_square(BoardID) :-
    board_rows(BoardID, Rows),
    board_cols(BoardID, Cols),
    adjacent_pair(Rows, R1, R2),
    adjacent_pair(Cols, C1, C2),
    cell(BoardID, R1, C1, Symbol),
    Symbol \= '.',
    cell(BoardID, R1, C2, Symbol),
    cell(BoardID, R2, C1, Symbol),
    cell(BoardID, R2, C2, Symbol),
    !.  % Cut to prevent backtracking after finding the first square.

/* 6.6. Master Predicate to Check All Losing Conditions */

% has_losing_condition(+BoardID)
% Succeeds if the specified board meets any losing condition.
has_losing_condition(BoardID) :-
    has_four_horizontal(BoardID);
    has_four_vertical(BoardID);
    has_four_diagonal(BoardID);
    has_square(BoardID).

player_loses :-
    ( has_losing_condition(1) ; has_losing_condition(2) ).

/* ----------------------------------------------------------------------
   5. DEMO
   ---------------------------------------------------------------------- */

demo :-
    init_boards,
    print_boards.