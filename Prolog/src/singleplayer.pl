/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

% Arquivo para interagir com o jogador e gerenciar o loop do jogo.
:- module(singleplayer,
          [ play_singleplayer/0
          ]).

:- use_module(grid).
:- use_module(input_helpers).
:- use_module(validation).

/*
    play_singleplayer/0
    Starts the single-player game mode. Initializes the game state and manages the game loop.
*/
play_singleplayer :-
    init_boards(Game), 
    write('Welcome to Singleplayer Mode!'), nl,
    write('The symbols X and O will alternate automatically.'), nl,
    nl,
    game_loop(Game, 'X').

/*
    game_loop(+GameState, +CurrentSymbol)
    Manages the game loop for single-player mode.
    - Displays the boards.
    - Prompts the player for a move.
    - Updates the game state.
    - Checks for win, loss, or continuation conditions.
    - Alternates between symbols ('X' and 'O') until the game ends.
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


/*
    next_symbol(+CurrentSymbol, -NextSymbol)
    Alternates between 'X' and 'O' for the next turn.
*/
next_symbol('X', 'O').
next_symbol('O', 'X').
