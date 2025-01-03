/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(ai_implementation,
          [ play_player_vs_computer/1
          ]).

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(grid).
:- use_module(singleplayer).
:- use_module(multiplayer).
:- use_module(singleplayer_normal_difficulty).

% Entry point for Player vs. Computer mode
play_player_vs_computer(Level) :-
    singleplayer_grid:init_boards(2, Game), % Initialize game with 2 boards
    write('Welcome to Player vs. Computer Mode!'), nl,
    nl,
    game_loop_turn_pvc([player(1, 1, 0), ai(2, 2, Level)], Game).

/*
    game_loop_turn_pvc(+Players, +Game)
    Processes a turn for the current player and alternates turns.
*/
game_loop_turn_pvc([player(Name, BoardID, Score) | RemainingPlayers], Game) :-
    print_boards(Game, true),
    format('Player ~w\'s turn.~n', [Name]),
    write('Choose two empty cells to place O and X.'), nl,
    prompt_two_moves(Row1, Col1, Row2, Col2),
    (
        Row1 = Row2, Col1 = Col2
    ->
        write('Cannot choose the same cell for both symbols. Try again.'), nl,
        game_loop_turn_pvc([player(Name, BoardID, Score) | RemainingPlayers], Game)
    ;
        (
            pick_space(Row1, Col1, 'O', Game, GameAfterO),
            pick_space(Row2, Col2, 'X', GameAfterO, GameAfterX)
        ->
            update_player_score(GameAfterX, player(Name, BoardID, Score), UpdatedPlayer),
            (
                multiplayer_game_over(GameAfterX, [UpdatedPlayer | RemainingPlayers], Winner)
            ->
                Winner = player(WinnerName, _, WinnerScore),
                format('Game ended! Player ~w won with a score of ~w.~n', [WinnerName, WinnerScore])
            ;
                append(RemainingPlayers, [UpdatedPlayer], NewPlayerOrder),
                game_loop_turn_pvc(NewPlayerOrder, GameAfterX)
            )
        ;
            write('Invalid move. Try again.'), nl,
            game_loop_turn_pvc([player(Name, BoardID, Score) | RemainingPlayers], Game)
        )
    ).

% AI Turn
game_loop_turn_pvc([ai(Name, BoardID, Level) | RemainingPlayers], Game) :-
    print_boards(Game, true),
    format('Computer (Player ~w) is making a move...~n', [Name]),
    choose_move(Game, Level, (Row1, Col1, Row2, Col2)),
    (
        pick_space(Row1, Col1, 'O', Game, GameAfterO),
        pick_space(Row2, Col2, 'X', GameAfterO, GameAfterX)
    ->
        update_player_score(GameAfterX, ai(Name, BoardID, Level), UpdatedAI),
        (
            multiplayer_game_over(GameAfterX, [UpdatedAI | RemainingPlayers], Winner)
        ->
            Winner = ai(WinnerName, _, WinnerScore),
            format('Game ended! Computer (Player ~w) won with a score of ~w.~n', [WinnerName, WinnerScore])
        ;
            append(RemainingPlayers, [UpdatedAI], NewPlayerOrder),
            game_loop_turn_pvc(NewPlayerOrder, GameAfterX)
        )
    ;
        write('Computer made an invalid move (should not happen). Restarting turn...'), nl,
        game_loop_turn_pvc([ai(Name, BoardID, Level) | RemainingPlayers], Game)
    ).

/*
    choose_move(+GameState, +Level, -Move)
    Determines the computer's move based on the difficulty level.
*/
choose_move(Game, Level, (Row1, Col1, Row2, Col2)) :-
    Level = 1,
    findall((Row, Col), valid_move(Game, Row, Col), ValidMoves),
    random_permutation(ValidMoves, ShuffledMoves),
    % Ensure there are at least two distinct valid moves
    ShuffledMoves = [(Row1, Col1) | Rest],
    Rest = [(Row2, Col2) | _].

choose_move(Game, Level, (Row1, Col1, Row2, Col2)) :-
    Level = 2,
    findall((Row, Col, Value), (
        valid_move(Game, Row, Col),
        evaluate_move(Game, Row, Col, Value)
    ), ScoredMoves),
    % Ensure there are at least two distinct moves with scores
    sort(3, @>=, ScoredMoves, [(Row1, Col1, _), (Row2, Col2, _)|_]).

/*
    valid_move(+Game, -Row, -Col)
    Ensures the given row and column correspond to an empty cell.
*/
valid_move(Game, Row, Col) :-
    singleplayer_grid:get_board(Game, 2, Board), % AI plays on board 2
    valid_cell(Board, Row, Col).

/*
    evaluate_move(+Game, +Row, +Col, -Value)
    Evaluates the potential value of a move based on the game state.
*/
evaluate_move(Game, Row, Col, Value) :-
    simulate_move(Game, Row, Col, TempGame),
    value(TempGame, 2, Value). % Use value/3 predicate to evaluate the state.

% Simulate placing a symbol
simulate_move(Game, Row, Col, NewGame) :-
    pick_space(Row, Col, 'O', Game, NewGame).
