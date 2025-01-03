/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(ai_implementation,
          [ play_player_vs_computer/1,
            valid_move/3
          ]).

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(grid, [pick_space/6, init_boards/2, get_board/3, print_boards/2]).
:- use_module(grid).
:- use_module(singleplayer).
:- use_module(multiplayer).
:- use_module(singleplayer_normal_difficulty).

% Entry point for Player vs. Computer mode
play_player_vs_computer(Level) :-
    grid:init_boards(2, Game), % Initialize game with 2 boards
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
        valid_move(Game, BoardID, Row1, Col1),
        valid_move(Game, BoardID, Row2, Col2),
        (Row1 \= Row2 ; Col1 \= Col2)
    ->
        pick_space(Row1, Col1, 'O', BoardID, Game, GameAfterO),
        pick_space(Row2, Col2, 'X', BoardID, GameAfterO, GameAfterX),
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
    ).

% AI Turn
game_loop_turn_pvc([ai(Name, BoardID, Level) | RemainingPlayers], Game) :-
    print_boards(Game, true),
    format('Computer (Player ~w) is making a move...~n', [Name]),
    choose_move(Game, Level, (Row1, Col1, Row2, Col2)),
    (
        valid_move(Game, BoardID, Row1, Col1),
        valid_move(Game, BoardID, Row2, Col2),
        (Row1 \= Row2 ; Col1 \= Col2)
    ->
        pick_space(Row1, Col1, 'O', BoardID, Game, GameAfterO),
        pick_space(Row2, Col2, 'X', BoardID, GameAfterO, GameAfterX),
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
        write('Computer chose invalid moves. Restarting turn...'), 
        game_loop_turn_pvc([ai(Name, BoardID, Level) | RemainingPlayers], Game)
    ).

/*
    choose_move(+GameState, +Level, -Move)
    Determines the computer's move based on the difficulty level.
*/
choose_move(Game, Level, (Row1, Col1, Row2, Col2)) :-
    Level = 1, % Random valid moves
    findall((Row, Col), valid_move(Game, 2, Row, Col), ValidMoves), % Use BoardID = 2 for AI
    format('Valid moves for AI: ~w~n', [ValidMoves]),
    random_permutation(ValidMoves, ShuffledMoves),
    select_two_distinct_moves(ShuffledMoves, (Row1, Col1), (Row2, Col2)),
    format('AI chose moves: (~w, ~w) and (~w, ~w)~n', [Row1, Col1, Row2, Col2]).

choose_move(Game, Level, (Row1, Col1, Row2, Col2)) :-
    Level = 2, % Greedy algorithm
    findall((Row, Col, Value), (
        valid_move(Game, 2, Row, Col), % Use BoardID = 2 for AI
        evaluate_move(Game, Row, Col, Value)
    ), ScoredMoves),
    sort(3, @>=, ScoredMoves, [(Row1, Col1, _), (Row2, Col2, _)|_]).

/* select_two_distinct_moves(+ValidMoves, -Move1, -Move2)
   Ensures two distinct moves are selected from the list of valid moves.
*/
select_two_distinct_moves([Move1 | Rest], Move1, Move2) :-
    member(Move2, Rest). % Ensure Move2 is distinct from Move1.

/*
    valid_move(+Game, -Row, -Col)
    Ensures the given row and column correspond to an empty cell.
*/
valid_move(Game, BoardID, Row, Col) :-
    get_board(Game, BoardID, Board),
    board_is_empty(Board, Row, Col).

% Helper: Checks if a specific cell in a board is empty.
board_is_empty(board(_, _, _, Cells), Row, Col) :-
    member(cell(Row, Col, '.'), Cells).

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
