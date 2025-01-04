/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

% Arquivo para interagir com o jogador e gerenciar o loop do jogo.
:- module(singleplayer,
          [ play_singleplayer/0
          ]).

:- use_module(grid).
:- use_module(input_helpers).
:- use_module(singleplayer_normal_difficulty).

/* 
    Predicado principal para iniciar o jogo.
*/
play_singleplayer :-
    init_boards(Game), 
    write('Welcome to Singleplayer Mode!'), nl,
    write('The symbols X and O will alternate automatically.'), nl,
    nl,
    game_loop(Game, 'X').

/* 
    Loop principal do jogo.
    game_loop(+GameState, +CurrentSymbol)
*/
game_loop(Game, Symbol) :-
    print_boards(Game, true),  % Dynamically print all boards
    format('~w Turn.~n', [Symbol]),
    prompt_move_singleplayer(Row, Col),
    (
        % Attempt to make the move
        pick_space(Row, Col, Symbol, Game, NewGame)
    ->
        (
            % Check game status after the move
            singleplayer_game_over(NewGame, Status),
            (
                Status = won
            ->
                print_boards(NewGame, true),
                write('Congratulations! You won the game.'), nl
            ;   Status = lost
            ->
                print_boards(NewGame, true),
                format('You lost the game due to a losing condition with the symbol ~w.~n', [Symbol]), nl
            ;   % If the game continues, alternate the symbol and keep playing
                next_symbol(Symbol, NextSymbol),
                game_loop(NewGame, NextSymbol)
            )
        )
    ;
        % If the move is invalid, notify and restart the loop with the same symbol
        write('Invalid move. Try again.'), nl,
        game_loop(Game, Symbol)
    ).

next_symbol('X', 'O').
next_symbol('O', 'X').
