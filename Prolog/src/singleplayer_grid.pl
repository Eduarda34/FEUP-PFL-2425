/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(singleplayer_grid,
          [ init_boards/0,
            pick_space/3,
            print_boards/0
          ]).

:- use_module(library(random)). 
:- use_module(library(lists)).
:- use_module(singleplayer_data).
:- use_module(singleplayer_normal_difficulty).

/*
   cell(BoardID, RowLabel, ColLabel, Symbol).
   Example: cell(1, a, 1, '.') => Board #1 at (A,1) has '.'.

   board2_rows/1 and board2_cols/1 store the *random* row/col order for Board #2.
*/

/* ----------------------------------------------------------------------
   1. INITIALIZATION
   ---------------------------------------------------------------------- */

%% init_boards/0
%%   1) Clears old data (cells, plus board2_rows/1, board2_cols/1)
%%   2) Creates Board #1 in normal order
%%   3) Creates Board #2 with random row/col ordering
init_boards :-
    retractall(cell(_,_,_,_)),
    retractall(board_rows(2,_)),
    retractall(board_cols(2,_)),

    % Board #2: pick a random ordering of A–F, 1–6
    board_rows(1,AllRows),
    board_cols(1,AllCols),
    random_shuffled_list(AllRows, RndRows),
    random_shuffled_list(AllCols, RndCols),
    assertz(board_rows(2,RndRows)),
    assertz(board_cols(2,RndCols)),

    % Now actually build the cell(...) facts for both boards:
    create_boards.

%% create_board(+BoardID)
%%   Fills the 6×6 grid with '.' for that board,
%%   in whatever row/col labeling that board uses.
create_boards :-
        board_rows(1,Rs1),
        board_cols(1,Cs1),
        board_rows(2,Rs2),
        board_cols(2,Cs2),
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


update_cell(BoardID, R, C, Sym) :-
    retractall(cell(BoardID, R, C, _)),
    assertz(cell(BoardID, R, C, Sym)).

/* ----------------------------------------------------------------------
   3. PRINTING THE BOARDS
   ---------------------------------------------------------------------- */
print_boards :-
        board_rows(1,Rs1),
        board_cols(1,Cs1),
        board_rows(2,Rs2),
        board_cols(2,Cs2),
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
