/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(game, 
            [
                play/0,
                initial_state/2,
                display_game/1,
                move/3,
                valid_moves/2,
                game_over/2,
                value/3,
                move_choose/3,
                load_demo_state/1,
                play_with_state/1
                ]).

:- use_module(main_menu).
:- use_module(grid).
:- use_module(ai_implementation).
:- use_module(singleplayer).
:- use_module(multiplayer).
:- use_module(input_helpers).
:- use_module(computer_vs_computer).
:- use_module(validation).

/*
    play/0
    Entry point for the game. This predicate initializes the game by displaying the main menu and
    allowing the user to navigate through the available game modes and configurations.
*/
play :-
    write('Welcome to the Game!'), nl,
    main_menu.

/*
    initial_state(+GameConfig, -GameState)
    Initializes the game state based on the provided configuration. GameConfig includes details such
    as board size, player types, and optional rules. Uses the init_boards/2 predicate from grid.pl to
    generate the initial state of the boards.
*/
initial_state(GameConfig, GameState) :-
    GameConfig = config(BoardSize, _, _),
    init_boards(BoardSize, GameState).

/*
    display_game(+GameState)
    Displays the current game state in a user-friendly format. This predicate utilizes the print_boards/2
    predicate from grid.pl to dynamically visualize the game boards.
*/
display_game(GameState) :-
    print_boards(GameState, true).

/*
    move(+GameState, +Move, -NewGameState)
    Applies a move to the game state if it is valid. The move is represented as a tuple (Row, Col, Symbol).
    It validates the move using valid_move/3 from grid.pl and then updates the state using pick_space/5.
*/
move(GameState, Move, NewGameState) :-
    Move = (Row, Col, Symbol),
    (   valid_move(GameState, Row, Col)
    ->  pick_space(Row, Col, Symbol, GameState, NewGameState)
    ;   write('Invalid move!'), nl, fail
    ).

/*
    valid_moves(+GameState, -ListOfMoves)
    Retrieves all valid moves for the current game state. Uses find_valid_moves/2 from ai_implementation.pl
    to compute the list of valid moves based on the current board configuration.
*/
valid_moves(GameState, ListOfMoves) :-
    find_valid_moves(GameState, ListOfMoves).

/*
    game_over(+GameState, -Winner)
    Determines if the game is over and identifies the winner. It uses multiplayer_game_over/3 from
    validation.pl to check if all boards are full and calculate the winner based on scores.
*/
game_over(GameState, Winner) :-
    (   multiplayer_game_over(GameState, _, Winner)
    ->  true
    ;   Winner = none
    ).

/*
    value(+GameState, +Player, -Value)
    Evaluates the game state for a specific player, providing a numerical value representing how advantageous
    the state is for the player. This uses calculate_player_score/3 from validation.pl.
*/
value(GameState, Player, Value) :-
    calculate_player_score(GameState, Player, Value).

/*
    move_choose(+GameState, +Level, -Move)
    Selects a move for the player or AI based on the specified difficulty level. It uses the choose_move/3
    predicate from ai_implementation.pl to implement random (Level 1) or greedy (Level 2) move strategies.
*/
move_choose(GameState, Level, Move) :-
    choose_move(GameState, Level, Move).

intermediate_state(game(
    board(1, [a, b, c, d, e, f, g, h], [1, 2, 3, 4, 5, 6, 7, 8], [
        cell(a, 1, 'X'), cell(a, 2, 'O'), cell(a, 3, 'X'), cell(a, 4, 'O'), cell(a, 5, 'X'), cell(a, 6, 'O'), cell(a, 7, 'X'), cell(a, 8, 'O'),
        cell(b, 1, 'O'), cell(b, 2, 'X'), cell(b, 3, 'O'), cell(b, 4, 'X'), cell(b, 5, 'O'), cell(b, 6, 'X'), cell(b, 7, 'O'), cell(b, 8, 'X'),
        cell(c, 1, '.'), cell(c, 2, 'O'), cell(c, 3, 'X'), cell(c, 4, 'O'), cell(c, 5, 'X'), cell(c, 6, 'O'), cell(c, 7, 'X'), cell(c, 8, 'O'),
        cell(d, 1, '.'), cell(d, 2, '.'), cell(d, 3, '.'), cell(d, 4, '.'), cell(d, 5, '.'), cell(d, 6, '.'), cell(d, 7, '.'), cell(d, 8, '.'),
        cell(e, 1, '.'), cell(e, 2, '.'), cell(e, 3, '.'), cell(e, 4, '.'), cell(e, 5, '.'), cell(e, 6, '.'), cell(e, 7, '.'), cell(e, 8, '.'),
        cell(f, 1, '.'), cell(f, 2, '.'), cell(f, 3, '.'), cell(f, 4, '.'), cell(f, 5, '.'), cell(f, 6, '.'), cell(f, 7, '.'), cell(f, 8, '.'),
        cell(g, 1, '.'), cell(g, 2, '.'), cell(g, 3, '.'), cell(g, 4, '.'), cell(g, 5, '.'), cell(g, 6, '.'), cell(g, 7, '.'), cell(g, 8, '.'),
        cell(h, 1, '.'), cell(h, 2, '.'), cell(h, 3, '.'), cell(h, 4, '.'), cell(h, 5, '.'), cell(h, 6, '.'), cell(h, 7, '.'), cell(h, 8, '.')
    ]),
    board(2, [a, b, c, d, e, f, g, h], [1, 2, 3, 4, 5, 6, 7, 8], [
        cell(a, 1, 'X'), cell(a, 2, 'O'), cell(a, 3, 'X'), cell(a, 4, 'O'), cell(a, 5, 'X'), cell(a, 6, 'O'), cell(a, 7, 'X'), cell(a, 8, 'O'),
        cell(b, 1, 'O'), cell(b, 2, 'X'), cell(b, 3, 'O'), cell(b, 4, 'X'), cell(b, 5, 'O'), cell(b, 6, 'X'), cell(b, 7, 'O'), cell(b, 8, 'X'),
        cell(c, 1, '.'), cell(c, 2, 'O'), cell(c, 3, 'X'), cell(c, 4, 'O'), cell(c, 5, 'X'), cell(c, 6, 'O'), cell(c, 7, 'X'), cell(c, 8, 'O'),
        cell(d, 1, '.'), cell(d, 2, '.'), cell(d, 3, '.'), cell(d, 4, '.'), cell(d, 5, '.'), cell(d, 6, '.'), cell(d, 7, '.'), cell(d, 8, '.'),
        cell(e, 1, '.'), cell(e, 2, '.'), cell(e, 3, '.'), cell(e, 4, '.'), cell(e, 5, '.'), cell(e, 6, '.'), cell(e, 7, '.'), cell(e, 8, '.'),
        cell(f, 1, '.'), cell(f, 2, '.'), cell(f, 3, '.'), cell(f, 4, '.'), cell(f, 5, '.'), cell(f, 6, '.'), cell(f, 7, '.'), cell(f, 8, '.'),
        cell(g, 1, '.'), cell(g, 2, '.'), cell(g, 3, '.'), cell(g, 4, '.'), cell(g, 5, '.'), cell(g, 6, '.'), cell(g, 7, '.'), cell(g, 8, '.'),
        cell(h, 1, '.'), cell(h, 2, '.'), cell(h, 3, '.'), cell(h, 4, '.'), cell(h, 5, '.'), cell(h, 6, '.'), cell(h, 7, '.'), cell(h, 8, '.')
    ])
)).


ending_state(game(
    board(1, [a, b, c, d, e, f, g, h], [1, 2, 3, 4, 5, 6, 7, 8], [
        cell(a, 1, 'X'), cell(a, 2, 'O'), cell(a, 3, 'X'), cell(a, 4, 'O'), cell(a, 5, 'X'), cell(a, 6, 'O'), cell(a, 7, 'X'), cell(a, 8, 'O'),
        cell(b, 1, 'O'), cell(b, 2, 'X'), cell(b, 3, 'O'), cell(b, 4, 'X'), cell(b, 5, 'O'), cell(b, 6, 'X'), cell(b, 7, 'O'), cell(b, 8, 'X'),
        cell(c, 1, 'X'), cell(c, 2, 'O'), cell(c, 3, 'X'), cell(c, 4, 'O'), cell(c, 5, 'X'), cell(c, 6, 'O'), cell(c, 7, 'X'), cell(c, 8, 'O'),
        cell(d, 1, 'O'), cell(d, 2, 'X'), cell(d, 3, 'O'), cell(d, 4, 'X'), cell(d, 5, 'O'), cell(d, 6, 'X'), cell(d, 7, '.'), cell(d, 8, 'O'),
        cell(e, 1, 'X'), cell(e, 2, 'O'), cell(e, 3, 'X'), cell(e, 4, 'O'), cell(e, 5, 'X'), cell(e, 6, 'O'), cell(e, 7, 'X'), cell(e, 8, 'O'),
        cell(f, 1, 'O'), cell(f, 2, 'X'), cell(f, 3, 'O'), cell(f, 4, 'X'), cell(f, 5, 'O'), cell(f, 6, '.'), cell(f, 7, 'O'), cell(f, 8, 'X'),
        cell(g, 1, 'X'), cell(g, 2, 'O'), cell(g, 3, 'X'), cell(g, 4, 'O'), cell(g, 5, 'X'), cell(g, 6, 'O'), cell(g, 7, 'X'), cell(g, 8, 'O'),
        cell(h, 1, 'O'), cell(h, 2, 'X'), cell(h, 3, 'O'), cell(h, 4, 'X'), cell(h, 5, 'O'), cell(h, 6, 'X'), cell(h, 7, 'O'), cell(h, 8, 'X')
    ]),
    board(2, [a, b, c, d, e, f, g, h], [1, 2, 3, 4, 5, 6, 7, 8], [
        cell(a, 1, 'X'), cell(a, 2, 'O'), cell(a, 3, 'X'), cell(a, 4, 'O'), cell(a, 5, 'X'), cell(a, 6, 'O'), cell(a, 7, 'X'), cell(a, 8, 'O'),
        cell(b, 1, 'O'), cell(b, 2, 'X'), cell(b, 3, 'O'), cell(b, 4, 'X'), cell(b, 5, 'O'), cell(b, 6, 'X'), cell(b, 7, 'O'), cell(b, 8, 'X'),
        cell(c, 1, 'X'), cell(c, 2, 'O'), cell(c, 3, 'X'), cell(c, 4, 'O'), cell(c, 5, 'X'), cell(c, 6, 'O'), cell(c, 7, 'X'), cell(c, 8, 'O'),
        cell(d, 1, 'O'), cell(d, 2, 'X'), cell(d, 3, 'O'), cell(d, 4, 'X'), cell(d, 5, 'O'), cell(d, 6, 'X'), cell(d, 7, '.'), cell(d, 8, 'O'),
        cell(e, 1, 'X'), cell(e, 2, 'O'), cell(e, 3, 'X'), cell(e, 4, 'O'), cell(e, 5, 'X'), cell(e, 6, 'O'), cell(e, 7, 'X'), cell(e, 8, 'O'),
        cell(f, 1, 'O'), cell(f, 2, 'X'), cell(f, 3, 'O'), cell(f, 4, 'X'), cell(f, 5, 'O'), cell(f, 6, '.'), cell(f, 7, 'O'), cell(f, 8, 'X'),
        cell(g, 1, 'X'), cell(g, 2, 'O'), cell(g, 3, 'X'), cell(g, 4, 'O'), cell(g, 5, 'X'), cell(g, 6, 'O'), cell(g, 7, 'X'), cell(g, 8, 'O'),
        cell(h, 1, 'O'), cell(h, 2, 'X'), cell(h, 3, 'O'), cell(h, 4, 'X'), cell(h, 5, 'O'), cell(h, 6, 'X'), cell(h, 7, 'O'), cell(h, 8, 'X')
    ])
)).

/*
    closer_ending_state_rule/0
    Displays instructions for progressing towards the closer ending state.
    Specifies the cells to fill on both boards to reach this state.
*/
closer_ending_state_rule :-
    write('To finish the game, fill the following cells:'), nl,
    write('- (d, 7) on both boards'), nl,
    write('- (f, 6) on both boards'), nl.


/*
    load_demo_state(-GameState)
    Loads a predefined intermediate demo state into the game.
    - GameState: The loaded intermediate state of the game.
*/
load_demo_state(GameState) :-
    intermediate_state(GameState).

/*
    play_with_state(+GameState)
    Starts the game using the provided demo state.
    - GameState: The initial state of the game for demonstration purposes.
*/
play_with_state(GameState) :-
    write('Starting game with demo state...'), nl,
    game_loop_turn([player(1, 1, 0), player(2, 2, 0)], GameState).
