/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- use_module(singleplayer).
:- use_module(multiplayer).
:- use_module(ai_implementation).
:- use_module(computer_vs_computer).

c :- consult('/home/miguelvalente/Documents/Faculdade/FEUP-PFL-2425/Prolog/src/main_menu.pl').

% Main Menu
main_menu :-
    write('==== MENU ===='), nl,
    write('1. Singleplayer'), nl,  % Updated to display a submenu
    write('2. Multiplayer'), nl,
    write('3. Instructions'), nl,
    write('4. Exit'), nl,
    write('Choose an option: '),
    read(Option),
    handle_option(Option).

% Handle top-level menu options
handle_option(1) :- singleplayer_menu.
handle_option(2) :- multiplayer_menu.
handle_option(3) :- display_instructions.
handle_option(4) :- write('Exiting the game...'), nl.
handle_option(_) :-
    write('Invalid option, try again.'), nl,
    main_menu.

% Singleplayer Menu
singleplayer_menu :-
    write('==== SINGLEPLAYER ===='), nl,
    write('1. Classic Singleplayer'), nl,
    write('2. Player vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('Choose an option: '),
    read(Choice),
    handle_singleplayer_choice(Choice).

handle_singleplayer_choice(1) :- % Classic singleplayer
    play_singleplayer.

handle_singleplayer_choice(2) :- % Player vs Computer
    write('Choose difficulty level for the computer:'), nl,
    write('1. Easy'), nl,
    write('2. Medium'), nl,
    write('3. Hard'), nl,
    read(Level),
    play_player_vs_computer(Level).

handle_singleplayer_choice(3) :-
    write('Choose difficulty level for the computers:'), nl,
    write('1. Easy'), nl,
    write('2. Medium'), nl,
    write('3. Hard'), nl,
    read(Level),
    play_computer_vs_computer(Level).

handle_singleplayer_choice(_) :-
    write('Invalid option, returning to the main menu.'), nl,
    main_menu.

% Multiplayer Menu
multiplayer_menu :-
    write('==== MULTIPLAYER ===='), nl,
    write('1. 2 Players'), nl,
    write('2. 3 Players'), nl,
    write('Choose an option: '),
    read(Choice),
    handle_multiplayer_choice(Choice).

handle_multiplayer_choice(1) :- % 2-player mode
    play_multiplayer.

handle_multiplayer_choice(2) :- % 3-player mode
    play_three_player.

handle_multiplayer_choice(_) :-
    write('Invalid option, returning to the main menu.'), nl,
    main_menu.

% Display instructions
display_instructions :-
    write('==== INSTRUCTIONS ===='), nl,
    write('--> Singleplayer Classic: Play against yourself by alternating turns.'), nl,
    write('--> Player vs Computer: Compete against the computer with different difficulty levels.'), nl,
    write('--> Multiplayer: Play against other players (2 or 3 players).'), nl,
    write('Press any key to return to the main menu.'), nl,
    get_char(_),
    get_char(_),
    main_menu.
