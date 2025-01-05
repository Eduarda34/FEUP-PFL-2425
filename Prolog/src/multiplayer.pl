/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(multiplayer,
          [ 
            play_multiplayer/0,
            play_three_player/0,
            game_loop_turn/2
          ]).

:- use_module(library(lists)).
:- use_module(grid).
:- use_module(input_helpers).
:- use_module(validation).

/*
    play_multiplayer/0
    Entry point for 2-player mode.
    - Initializes the game with 2 boards.
    - Starts the game loop with two players, each associated with their own board and initial score of 0.
*/
play_multiplayer :-
    init_boards(2, Game),
    write('Welcome to 2-Player Mode!'), nl,
    nl,
    game_loop_turn([player(1, 1, 0), player(2, 2, 0)], Game).

/*
    play_three_player/0
    Entry point for 3-player mode.
    - Initializes the game with 3 boards.
    - Starts the game loop with three players, each associated with their own board and initial score of 0.
*/
play_three_player :-
    init_boards(3, Game),
    write('Welcome to 3-Player Mode!'), nl,
    nl,
    game_loop_turn([player(1, 1, 0), player(2, 2, 0), player(3, 3, 0)], Game).

/*
    game_loop_turn(+Players, +Game)
    Processes a turn for the current player and moves to the next.
*/
game_loop_turn([CurrentPlayer | RemainingPlayers], Game) :-
    CurrentPlayer = player(Name, _BoardID, _Score),
    print_boards(Game, true),  % Dynamically print all boards
    format('Player ~w\'s turn.~n', [Name]),
    write('Choose two empty cells to place O and X.'), nl,
    prompt_two_moves(Row1, Col1, Row2, Col2),
    (
        Row1 = Row2, Col1 = Col2
    ->
        write('Cannot choose the same cell for both symbols. Try again.'), nl,
        game_loop_turn([CurrentPlayer | RemainingPlayers], Game)
    ;
        (
            % Attempt to place O and X on the board
            pick_space(Row1, Col1, 'O', Game, GameAfterO),
            pick_space(Row2, Col2, 'X', GameAfterO, GameAfterX)
        ->
            % Update player's score
            update_player_score(GameAfterX, CurrentPlayer, UpdatedPlayer),
            (
                % Check if the game is over
                multiplayer_game_over(GameAfterX, [UpdatedPlayer | RemainingPlayers], Winner)
            ->
                Winner = player(WinnerName, _, WinnerScore),
                format('Game ended! Player ~w won with a score of ~w.~n', [WinnerName, WinnerScore])
            ;
                % Rotate players for the next turn
                append(RemainingPlayers, [UpdatedPlayer], NewPlayerOrder),
                game_loop_turn(NewPlayerOrder, GameAfterX)
            )
        ;
            % Handle invalid moves
            write('Invalid move. Try again.'), nl,
            game_loop_turn([CurrentPlayer | RemainingPlayers], Game)
        )
    ).

/*
    demo/0
    Executes a sequence of example moves and displays the score.
*/
demo :-
    init_boards(Game),
    write('Starting Demo...'), nl,
    print_boards(Game, true),

    pick_space(a, 1, 'X', Game, Game1),
    pick_space(a, 2, 'X', Game1, Game2),
    pick_space(a, 3, 'X', Game2, Game3),
    pick_space(a, 4, 'X', Game3, Game4),
    pick_space(a, 5, 'X', Game4, Game5),
    pick_space(a, 6, 'X', Game5, Game6),
    pick_space(b, 6, 'X', Game6, Game7),
    pick_space(b, 5, 'X', Game7, Game8),
    pick_space(b, 4, 'X', Game8, Game9),
    pick_space(b, 3, 'X', Game9, Game10),
    pick_space(b, 2, 'X', Game10, Game11),
    pick_space(b, 1, 'X', Game11, Game12),

    print_boards(Game12, true),

    update_player_score(Game12, player(1, 1, 0), UpdatedPlayer1),
    UpdatedPlayer1 = player(1, 1, Score1),
    format('Player 1 Score: ~w~n~n', [Score1]).
