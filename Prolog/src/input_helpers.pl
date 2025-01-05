/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */
                                                                                                                
:- module(input_helpers,
          [ prompt_two_moves/4,
            prompt_move/2,
            prompt_move_singleplayer/2,
            prompt_row/1,
            prompt_row_singleplayer/1,
            prompt_col/1,
            prompt_col_singleplayer/1
          ]).

/*
   prompt_two_moves(-Row1, -Col1, -Row2, -Col2)
   Prompts the user for two moves: the first move (X) and the second move (O),
   each consisting of a row and a column.
 */
prompt_two_moves(Row1, Col1, Row2, Col2) :-
    write('First move (X):'), nl,
    prompt_move(Row1, Col1),
    write('Second move (O):'), nl,
    prompt_move(Row2, Col2).

/*
   prompt_move(-Row, -Col)
   Prompts the user for a single move by requesting the row and column separately.
 */
prompt_move(Row, Col) :-
    prompt_row(Row),
    prompt_col(Col).
/*
   prompt_row(-Row)
   Prompts the user to select a row (a-h) and reads the input. (For multiplayer boards).
 */
prompt_row(Row) :-
    write('Choose the row (a-h): '),
    read(Row).

/*
   prompt_col(-Col)
   Prompts the user to select a column (1-8) and reads the input. (For multiplayer boards).
 */
prompt_col(Col) :-
    write('Choose the column (1-8): '),
    read(Col),
    nl.

/*
   prompt_move_singleplayer(-Row, -Col)
   Prompts the user for a single move in single-player mode by requesting the row and column separately.
*/
prompt_move_singleplayer(Row, Col) :-
    prompt_row_singleplayer(Row),
    prompt_col_singleplayer(Col).

/*
   prompt_row_singleplayer(-Row)
   Prompts the user to select a row (a-f) and reads the input.
*/
prompt_row_singleplayer(Row) :-
    write('Choose the row (a-f): '),
    read(Row).
/*
   prompt_col_singleplayer(-Col)
   Prompts the user to select a column (1-6) and reads the input.
*/
prompt_col_singleplayer(Col) :-
    write('Choose the column (1-6): '),
    read(Col).
