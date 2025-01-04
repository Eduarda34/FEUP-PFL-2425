:- module(input_helpers,
          [ prompt_two_moves/4,
            prompt_move/2,
            prompt_move_singleplayer/2,
            prompt_row/1,
            prompt_row_singleplayer/1,
            prompt_col/1,
            prompt_col_singleplayer/1
          ]).

% Prompt for two moves (row and column for each)
prompt_two_moves(Row1, Col1, Row2, Col2) :-
    write('First move (X):'), nl,
    prompt_move(Row1, Col1),
    write('Second move (O):'), nl,
    prompt_move(Row2, Col2).

% Prompt for a single move (row and column)
prompt_move(Row, Col) :-
    prompt_row(Row),
    prompt_col(Col).

% Prompt for row
prompt_row(Row) :-
    write('Choose the row (a-h): '),
    read(Row).

% Prompt for column
prompt_col(Col) :-
    write('Choose the column (1-8): '),
    read(Col),
    nl.

% Prompt for a single move in singleplayer (row and column)
prompt_move_singleplayer(Row, Col) :-
    prompt_row_singleplayer(Row),
    prompt_col_singleplayer(Col).

prompt_row_singleplayer(Row) :-
    write('Choose the row (a-f): '),
    read(Row).

prompt_col_singleplayer(Col) :-
    write('Choose the column (1-6): '),
    read(Col).
