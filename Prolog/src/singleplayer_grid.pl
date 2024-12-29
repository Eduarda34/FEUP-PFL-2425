/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- dynamic cell/4.
/*  
   cell(BoardID, RowLabel, ColLabel, Symbol).

   Example: cell(1, a, 1, '.') means:
      On Board #1, coordinate (A,1) holds '.' (empty).
*/

% --- We define a "fixed" labeling for Board #1,
%     and a "randomized" labeling for Board #2 (as in the image).

board1_rows([a,b,c,d,e,f]).
board1_cols([1,2,3,4,5,6]).

% Suppose in your example the second board is labeled
% exactly as in the image: rows B,F,A,D,C,E
% (top to bottom), and columns 5,3,2,4,6,1
% (left to right). You can adapt these if you want to
% truly randomize each time, or follow the picture.
board2_rows([b,f,a,d,c,e]).
board2_cols([5,3,2,4,6,1]).

/* ----------------------------------------------------------------------
   1. INITIALIZATION
   ---------------------------------------------------------------------- */

%% init_boards/0
%%  Clear out old data, then create both boards with '.' in each cell.
init_boards :-
    retractall(cell(_,_,_,_)),
    create_board(1),
    create_board(2).

%% create_board(+BoardID)
%%  Fill 6×6 with '.' for that board.
create_board(BoardID) :-
    RowListPred = (BoardID =:= 1 -> board1_rows(Rs) ; board2_rows(Rs)),
    ColListPred = (BoardID =:= 1 -> board1_cols(Cs) ; board2_cols(Cs)),
    call(RowListPred),  % get the correct row labels for this board
    call(ColListPred),  % get the correct col labels
    fill_cells(BoardID, Rs, Cs).

fill_cells(_, [], _).
fill_cells(BoardID, [R|Rs], ColLabels) :-
    fill_one_row(BoardID, R, ColLabels),
    fill_cells(BoardID, Rs, ColLabels).

fill_one_row(_, _, []).
fill_one_row(BoardID, RowLabel, [C|Cs]) :-
    assertz(cell(BoardID, RowLabel, C, '.')),
    fill_one_row(BoardID, RowLabel, Cs).

/* ----------------------------------------------------------------------
   2. PICKING A SPACE (COORDINATE) AND PLACING A SYMBOL
   ---------------------------------------------------------------------- */

%% pick_space(+BoardID, +RowLabel, +ColLabel, +Symbol)
%%   e.g. pick_space(1, a, 1, 'O').
%%   This means: "Place Symbol in (RowLabel,ColLabel) on Board #1,
%%   and also place Symbol in the same (RowLabel,ColLabel) on Board #2."
%%   (So the two boards stay synchronized by label.)
pick_space(BoardID, RowLabel, ColLabel, Symbol) :-
    (BoardID = 1 ; BoardID = 2),  % must be valid
    update_cell(BoardID, RowLabel, ColLabel, Symbol),
    % also update the *other* board
    OtherBoard is (3 - BoardID),  % if BoardID=1 => Other=2; if=2 => Other=1
    update_cell(OtherBoard, RowLabel, ColLabel, Symbol).

update_cell(BoardID, RowLabel, ColLabel, Symbol) :-
    retractall(cell(BoardID, RowLabel, ColLabel, _)),
    assertz(cell(BoardID, RowLabel, ColLabel, Symbol)).

/* ----------------------------------------------------------------------
   3. PRINTING THE BOARDS
   ---------------------------------------------------------------------- */

%% print_board(+BoardID)
%%  Print the board in the labeling order that belongs to it.
%%  Board #1 => A..F x 1..6
%%  Board #2 => B,F,A,D,C,E x 5,3,2,4,6,1 (or whichever you chose)
print_board(BoardID) :-
    ( BoardID =:= 1 ->
        board1_rows(Rs),
        board1_cols(Cs),
        write('=== BOARD #1 (Fixed Labels) ===')
    ; BoardID =:= 2 ->
        board2_rows(Rs),
        board2_cols(Cs),
        write('=== BOARD #2 (Random Labels) ===')
    ),
    nl,
    print_column_headers(Cs),
    print_rows(BoardID, Rs, Cs),
    nl.

print_column_headers(Cols) :-
    write('      '),
    print_col_list(Cols),
    nl.

print_col_list([]).
print_col_list([C|Cs]) :-
    format('  ~w ', [C]),
    print_col_list(Cs).

print_rows(_, [], _).
print_rows(BoardID, [R|Rs], Cols) :-
    % print row label on the left:
    format(' ~w ', [R]),
    % now print each cell in this row:
    print_cells_in_row(BoardID, R, Cols),
    nl,
    print_rows(BoardID, Rs, Cols).

print_cells_in_row(_, _, []).
print_cells_in_row(BoardID, RowLabel, [C|Cs]) :-
    cell(BoardID, RowLabel, C, Symbol),
    format(' ~w ', [Symbol]),
    print_cells_in_row(BoardID, RowLabel, Cs).

/* ----------------------------------------------------------------------
   4. DEMO
   ---------------------------------------------------------------------- */

demo :-
    init_boards,
    print_board(1),
    print_board(2).