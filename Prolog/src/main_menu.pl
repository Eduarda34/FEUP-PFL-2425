/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(main_menu, [
    main_menu/0
]).

:- use_module(game).
:- use_module(grid).
:- use_module(validation).
:- use_module(multiplayer).
:- use_module(singleplayer).
:- use_module(input_helpers).
:- use_module(ai_implementation).
:- use_module(computer_vs_computer).

/*
    main_menu/0
    Displays the main menu and handles user input to navigate through options.
*/
main_menu :-
    write('==== MENU ===='), nl,
    write('1. Singleplayer'), nl,  % Updated to display a submenu
    write('2. Multiplayer'), nl,
    write('3. Instructions'), nl,
    write('4. Load Intermediate Demo State'), nl,
    write('5. Load Ending Demo State'), nl,
    write('6. Exit'), nl,
    write('Choose an option: '),
    read(Option),
    handle_option(Option).

/*
    handle_option(+Option)
    Handles the user's choice from the main menu.
    - Option: The user's input corresponding to the chosen menu option.
*/
handle_option(1) :- singleplayer_menu.
handle_option(2) :- multiplayer_menu.
handle_option(3) :- display_instructions.
handle_option(4) :-
    game:load_demo_state(GameState),
    write('Demo state loaded. Use this state to demonstrate functionality.'), nl,
    play_with_state(GameState).
handle_option(5) :-
    game:ending_state(GameState),
    game:closer_ending_state_rule,
    play_with_state(GameState).
handle_option(6) :- write('Exiting the game...'), nl.
handle_option(_) :-
    write('Invalid option, try again.'), nl,
    main_menu.

/*
    singleplayer_menu/0
    Displays the singleplayer submenu and handles user input to select game modes.
*/
singleplayer_menu :-
    write('==== SINGLEPLAYER ===='), nl,
    write('1. Classic Singleplayer'), nl,
    write('2. Player vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('Choose an option: '),
    read(Choice),
    handle_singleplayer_choice(Choice).

/*
    handle_singleplayer_choice(+Choice)
    Handles the user's choice from the singleplayer submenu.
    - Choice: The user's input corresponding to the chosen singleplayer mode.
*/
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

/*
    multiplayer_menu/0
    Displays the multiplayer submenu and handles user input to select the number of players.
*/
multiplayer_menu :-
    write('==== MULTIPLAYER ===='), nl,
    write('1. 2 Players'), nl,
    write('2. 3 Players'), nl,
    write('Choose an option: '),
    read(Choice),
    handle_multiplayer_choice(Choice).

/*
    handle_multiplayer_choice(+Choice)
    Handles the user's choice from the multiplayer submenu.
    - Choice: The user's input corresponding to the chosen multiplayer mode.
*/
handle_multiplayer_choice(1) :- % 2-player mode
    play_multiplayer.

handle_multiplayer_choice(2) :- % 3-player mode
    play_three_player.

handle_multiplayer_choice(_) :-
    write('Invalid option, returning to the main menu.'), nl,
    main_menu.

/*
    display_instructions/0
    Displays the game instructions to the user.
    Returns to the main menu after the user presses a key.
*/
display_instructions :-
    write('==== INSTRUCTIONS ===='), nl,
    write('--> Take turns placing symbols (X or O) on the grid.'), nl,
    write('--> The player with less consecutive symbols in a row, column, diagonal or squares wins.'), nl,
    write('--> Ensure no invalid moves are made.'), nl,
    write('Press any key to return to the main menu.'), nl,
    get_char(_),
    get_char(_),
    main_menu.