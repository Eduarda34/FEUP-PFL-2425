/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- use_module(singleplayer).
:- use_module(multiplayer).

c :- consult('/home/miguelvalente/Documents/Faculdade/FEUP-PFL-2425/Prolog/src/main_menu.pl').

% Main Menu
main_menu :-
    write('==== MENU ===='), nl,
    write('1. Start Game'), nl,
    write('2. Instructions'), nl,
    write('3. Exit'), nl,
    write('Choose an option: '),
    read(Option),  % Capture input
    handle_option(Option).

% Main Menu - Número de jogadores
handle_option(1) :-
    write('Choose the number of players:'), nl,
    write('1. 1 Player'), nl,
    write('2. 2 Players'), nl,
    write('3. 3 Players'), nl,
    read(Choice),  % Capture input
    handle_players(Choice),
    main_menu.

% Main Menu - Instruções
handle_option(2) :-
    write('==== INSTRUCTIONS ===='), nl,
    write('--> Take turns placing symbols (X or O) on the grid.'), nl,
    write('--> The first player to form four consecutive symbols in a row, column, or diagonal wins.'), nl,
    write('--> Ensure no invalid moves are made.'), nl,
    write('Press any key to return to the menu.'), nl,
    get_char(_),  % Wait for keypress
    get_char(_),
    main_menu.

% Main Menu - Sair do jogo
handle_option(3) :-
    write('Exiting the game...'), nl.

% Main Menu - Opção inválida
handle_option(_) :-
    write('Invalid option, try again.'), nl,
    main_menu.

% Número de jogadores - 1 jogador
handle_players(1) :-
    write('Starting 1-player mode.'), nl,
    play_singleplayer.

% Número de jogadores - 2 jogadores
handle_players(2) :-
    write('Starting 2-player mode.'), nl,
    play_multiplayer.

% Número de jogadores - 3 jogadores
handle_players(3) :-
    write('Starting 3-player mode.'), nl,
    play_three_player.

% Fallback for invalid player count
handle_players(_) :-
    write('Invalid player choice, returning to menu.'), nl.
