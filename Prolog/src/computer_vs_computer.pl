/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(computer_vs_computer, [
    play_computer_vs_computer/1
]).

:- use_module(grid).
:- use_module(input_helpers).
:- use_module(ai_implementation).
:- use_module(validation).

% Entry point for Computer vs Computer mode
play_computer_vs_computer(Level) :-
    init_boards(2, GameState),
    default_rows(Rows), default_cols(Cols),
    findall(cell(Row, Col, '.'), (member(Row, Rows), member(Col, Cols)), AllValidMoves),
    write('Starting Computer vs Computer Mode!'), nl,
    write('Computer 1: Player 1 | Computer 2: Player 2'), nl,
    computer_vs_computer_loop(GameState, [AllValidMoves, AllValidMoves], 'Computer 1', Level).

% Game loop alternating turns between two computers
computer_vs_computer_loop(GameState, [Player1ValidMoves, Player2ValidMoves], CurrentPlayer, Level) :-
    % Print the boards at the start of each turn
    print_boards(GameState, true),
    format('~w\'s turn!~n', [CurrentPlayer]),
    % Determine the current player and their moves
    ( CurrentPlayer = 'Computer 1' ->
        ValidMoves = Player1ValidMoves,
        OpponentMoves = Player2ValidMoves,
        PlayerMarker1 = 'X',
        PlayerMarker2 = 'O',
        NextPlayer = 'Computer 2'
    ; CurrentPlayer = 'Computer 2' ->
        ValidMoves = Player2ValidMoves,
        OpponentMoves = Player1ValidMoves,
        PlayerMarker1 = 'X',
        PlayerMarker2 = 'O',
        NextPlayer = 'Computer 1'
    ),

    % Check if the current player has valid moves
    ( ValidMoves \= []
    ->  % Make two moves
        random_select(cell(Row1, Col1, '.'), ValidMoves, TempMoves),
        random_select(cell(Row2, Col2, '.'), TempMoves, RemainingMoves),
        format('~w chose: (~w, ~w) and (~w, ~w)~n', [CurrentPlayer, Row1, Col1, Row2, Col2]),

        % Apply moves to both boards
        pick_space(Row1, Col1, PlayerMarker1, 1, GameState, TempGameState1),
        pick_space(Row1, Col1, PlayerMarker1, 2, TempGameState1, TempGameState2),
        pick_space(Row2, Col2, PlayerMarker2, 1, TempGameState2, TempGameState3),
        pick_space(Row2, Col2, PlayerMarker2, 2, TempGameState3, NewGameState),

        % Remove used cells from both lists
        exclude(==(cell(Row1, Col1, '.')), OpponentMoves, OppMovesAfterFirst),
        exclude(==(cell(Row2, Col2, '.')), OppMovesAfterFirst, UpdatedOpponentMoves),
        exclude(==(cell(Row1, Col1, '.')), RemainingMoves, UpdatedMoves1),
        exclude(==(cell(Row2, Col2, '.')), UpdatedMoves1, UpdatedCurrentMoves),

        % Reassign the updated move lists
        ( CurrentPlayer = 'Computer 1' ->
            NewMoveLists = [UpdatedCurrentMoves, UpdatedOpponentMoves]
        ; CurrentPlayer = 'Computer 2' ->
            NewMoveLists = [UpdatedOpponentMoves, UpdatedCurrentMoves]
        ),

        % Check if game is over or continue
        ( multiplayer_game_over(NewGameState, [player('Computer 1', 1, 0), player('Computer 2', 2, 0)], Winner)
        ->  Winner = player(WinnerName, _, WinnerScore),
            print_boards(NewGameState, true),  % Print final state of the boards
            format('Game over! ~w wins with a score of ~w!~n', [WinnerName, WinnerScore])
        ;   computer_vs_computer_loop(NewGameState, NewMoveLists, NextPlayer, Level)
        )
    ;   % No valid moves for current player
        write(CurrentPlayer), write(' has no valid moves. Skipping turn.'), nl,
        computer_vs_computer_loop(GameState, [Player1ValidMoves, Player2ValidMoves], NextPlayer, Level)
    ).

% exclude(+Goal, +List, -Filtered)
% Removes elements from List that satisfy Goal, returning the remaining elements in Filtered.
exclude(_, [], []).
exclude(Goal, [H|T], Filtered) :-
    (   call(Goal, H)
    ->  exclude(Goal, T, Filtered)
    ;   Filtered = [H|Rest],
        exclude(Goal, T, Rest)
    ).