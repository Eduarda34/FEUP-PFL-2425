/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

% Arquivo para interagir com o jogador e gerenciar o loop do jogo.
:- module(singleplayer,
          [ play_singleplayer/0
          ]).

:- use_module(singleplayer_grid).
:- use_module(singleplayer_normal_difficulty).
:- use_module(singleplayer_data).


play_singleplayer :-
    init_boards, 
    write('Welcome to Singleplayer Mode!'), nl,
    write('The symbols X and O will alternate automatically.'), nl,
    nl,
    game_loop('X').


game_loop(Symbol) :-
    print_boards,
    format('~w Turn.~n', [Symbol]),
    prompt_move(Row, Col),
    (   pick_space(Row, Col, Symbol) ->
        (   player_loses ->
            print_boards,
            format('You lost the game due to a losing condition with the symbol ~w.~n', [Symbol])
        ;   player_wins ->
            print_boards,
            write('Congratulations! You won the game.'), nl
        ;   next_symbol(Symbol, NextSymbol),
            game_loop(NextSymbol)
        )
    ;   write('Invalid move. Try again.'), nl,
        game_loop(Symbol)
    ).


next_symbol('X', 'O').
next_symbol('O', 'X').


prompt_move(Row, Col) :-
    prompt_row(Row),
    prompt_col(Col).

prompt_row(Row) :-
    write('Choose the row (a-f): '),
    read(Row).
    

prompt_col(Col) :-
    write('Choose the collumn (1-6): '),
    read(Col).

