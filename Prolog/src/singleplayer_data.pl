/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(singleplayer_data,
          [ cell/4,
            board_rows/2,
            board_cols/2,
            random_shuffled_list/2
          ]).

:- use_module(library(random)). 

% cell(BoardID, RowLabel, ColLabel, Symbol).
% Example: cell(1, a, 1, '.') => Board #1 at (A,1) has '.'.
:- dynamic cell/4.
:- volatile cell/4.

:- dynamic board2_rows/1.
:- volatile board2_rows/1.

:- dynamic board2_cols/1.
:- volatile board2_cols/1.

:- dynamic board_rows/2.
:- volatile board_rows/2.

:- dynamic board_cols/2.
:- volatile board_cols/2.
% Static definition for Board #1
board1_rows([a,b,c,d,e,f]).
board1_cols([1,2,3,4,5,6]).

% board_rows(+BoardID, -Rows)
% Retrieves the ordered list of rows for the specified board.
board_rows(1, Rs) :- board1_rows(Rs).
board_rows(2, Rs) :- board2_rows(Rs).

% board_cols(+BoardID, -Cols)
% Retrieves the ordered list of columns for the specified board.
board_cols(1, Cs) :- board1_cols(Cs).
board_cols(2, Cs) :- board2_cols(Cs).

%% random_shuffled_list(+List, -Shuffled)
%% Shuffles List using random_select/3.
random_shuffled_list([], []).
random_shuffled_list(List, [X|Xs]) :-
    random_select(X, List, Rest),
    random_shuffled_list(Rest, Xs).

