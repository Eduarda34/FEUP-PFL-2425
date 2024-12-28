/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- consult(grid).
:- consult(validation).
:- consult(game).

% Main Menu
main_menu :-
    write('=== MENU ==='), nl,
    write('1. Start Game'), nl,
    write('2. Instructions'), nl,
    write('3. Exit'), nl,
    write('Choose an option: '),
    read(Option),
    handle_option(Option).

%Main Menu - Número de jogadores
handle_option(1) :-
    write('Choose the number of players:'), nl,
    write('1. 1 Player'), nl,
    write('2. 2 Players'), nl,
    write('3. 3 Players'), nl,
    read(Choice),
    handle_players(Choice).

%Main Menu - Instruções
handle_option(2) :-
    write('=== INSTRUCTIONS ==='), nl,
    write('--> Take turns placing symbols (X or O) on the grid.'), nl,
    write('--> The first player to form four consecutive symbols in a row, column, or diagonal wins.'), nl,
    write('--> Ensure no invalid moves are made.'), nl,
    write('Press any key to exit.'), nl,
    get_char(_),
    get_char(_),
    main_menu.

%Main Menu - Sair do jogo
handle_option(3) :-
    write('Exiting the game...'), nl.

%Main Menu - Opção inválida
handle_option(_) :-
    write('Invalid option, try again.'), nl,
    main_menu.

%Número de jogadores - 1 jogador
handle_players(1) :-
    write('Single-player mode is not implemented yet.'), nl,
    main_menu.

%Número de jogadores - 2 jogadores
handle_players(2) :-
    write('Starting 2-player mode.'), nl,
    init_grid(6, Grid), % Initialize 6x6 grid
    play_game(Grid, 'X').

%Número de jogadores - 3 jogadores
handle_players(3) :-
    write('3-player mode is not implemented yet.'), nl,
    main_menu.
