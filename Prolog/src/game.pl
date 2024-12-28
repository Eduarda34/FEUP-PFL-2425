/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- consult(grid).
:- consult(validation).

% Switch Between Players
switch_player('X', 'O').
switch_player('O', 'X').

% Main Game Loop
play_game(Grid, CurrentPlayer) :-
    print_grid(Grid),
    write('Player '), write(CurrentPlayer), write('\'s turn.'), nl,
    write('Enter row: '), read(Row),
    write('Enter column: '), read(Col),
    (
        valid_move(Row, Col, Grid) ->
        (
            update_grid(Row, Col, CurrentPlayer, Grid, UpdatedGrid),
            (
                check_victory(UpdatedGrid, CurrentPlayer) ->
                (
                    print_grid(UpdatedGrid),
                    write('Player '), write(CurrentPlayer), write(' wins!'), nl
                );
                switch_player(CurrentPlayer, NextPlayer),
                play_game(UpdatedGrid, NextPlayer) % Continue the game
            )
        );
        write('Invalid move, try again.'), nl,
        play_game(Grid, CurrentPlayer)
    ).
