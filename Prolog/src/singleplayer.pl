/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

% Arquivo para interagir com o jogador e gerenciar o loop do jogo.
:- module(singleplayer,
          [ play_singleplayer/0
          ]).

:- use_module(singleplayer_grid).
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
    print_boards(Game),
    format('~w Turn.~n', [Symbol]),
    prompt_move(Row, Col),
    (
        % Tentar fazer a jogada.
        % pick_space(+Row, +Col, +Symbol, +Game, -NewGame)
        pick_space(Row, Col, Symbol, Game, NewGame)
    ->
        (
            % Verificar o status do jogo após a jogada.
            singleplayer_game_over(NewGame, Status),
            (
                Status = won
            ->
                print_boards(NewGame),
                write('Congratulations! You won the game.'), nl
            ;   Status = lost
            ->
                print_boards(NewGame),
                format('You lost the game due to a losing condition with the symbol ~w.~n', [Symbol]), nl
            ;   % Caso o jogo continue, alternar o símbolo e continuar o loop.
                next_symbol(Symbol, NextSymbol),
                game_loop(NewGame, NextSymbol)
            )
        )
    ;
        % Se a jogada for inválida, informar e reiniciar o loop com o mesmo símbolo.
        write('Invalid move. Try again.'), nl,
        game_loop(Game, Symbol)
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
    write('Choose the column (1-6): '),
    read(Col).