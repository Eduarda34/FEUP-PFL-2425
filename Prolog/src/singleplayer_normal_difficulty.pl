/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(singleplayer_normal_difficulty,
          [ valid_cell/3,
            player_loses/0,
            player_wins/0
          ]).

:- use_module(singleplayer_data).

% valid_cell(BoardID, RowLabel, ColLabel)
% Ensures the cell contains '.'
valid_cell(BoardID, RowLabel, ColLabel) :-
    cell(BoardID, RowLabel, ColLabel, '.').

adjacent_pair([First, Second | _], First, Second).
adjacent_pair([_ | Rest], First, Second) :-
    adjacent_pair(Rest, First, Second).

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

% has_losing_condition(+BoardID)
% Succeeds if the specified board meets any losing condition.
has_losing_condition(BoardID) :-
    has_four_horizontal(BoardID);
    has_four_vertical(BoardID);
    has_four_diagonal(BoardID);
    has_square(BoardID).

% Succeeds if player has losing conditions.
player_loses :-
    ( has_losing_condition(1) ; has_losing_condition(2) ).

% Succeeds if player's boards are complete.
player_wins :-
    \+ (cell(1, _, _, '.')),
    \+ (cell(2, _, _, '.')).