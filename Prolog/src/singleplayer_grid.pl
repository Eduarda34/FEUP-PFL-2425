/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- use_module(library(random)).

% Ordered lists for rows (letters) and columns (numbers).
letters([a, b, c, d, e, f]).
numbers([1, 2, 3, 4, 5, 6]).

% Helper predicate: shuffle a list
shuffle_list(List, Shuffled) :-
    random_permutation(List, Shuffled).

% Print the column labels along the top:
print_col_header([]) :-
    nl, !.
print_col_header([C|Cs]) :-
    write(C),
    write('  '),
    print_col_header(Cs).

% Print a single row: row label + placeholder cells.
print_row(_, []) :-
    nl, !.
print_row(RowLabel, [_|TCols]) :-
    write('   .'),  % or any placeholder you like
    print_row(RowLabel, TCols).

% Print each row with its row label in front,
% then placeholders (one for each column).
print_grid_rows([], _).
print_grid_rows([R|TRows], Cols) :-
    % Print the row label first (capitalized letter)
    write(R),
    write(' '),
    print_row(R, Cols), 
    print_grid_rows(TRows, Cols).

% Main predicate to print a single grid, 
% given row labels and column labels.
print_grid(Rows, Cols) :-
    write('       '),  % Some spacing
    print_col_header(Cols),  % Print top header
    print_grid_rows(Rows, Cols).

% Create and print two grids:
%  1) first grid has rows A-F & columns 1-6
%  2) second grid is randomly labeled
create_two_grids :-
    letters(Letters),
    numbers(Numbers),

    % === 1) FIRST GRID (ordered) ===
    write('=== FIRST GRID (ordered) ==='), nl,
    print_grid(Letters, Numbers),
    nl,

    % === 2) SECOND GRID (shuffled) ===
    % We shuffle both letters and numbers for the second grid.
    shuffle_list(Letters, ShuffledLetters),
    shuffle_list(Numbers, ShuffledNumbers),

    write('=== SECOND GRID (randomized) ==='), nl,
    print_grid(ShuffledLetters, ShuffledNumbers),
    nl.