:- module(computer_vs_computer, [
    play_computer_vs_computer/1
]).

:- use_module(grid).
:- use_module(input_helpers).
:- use_module(ai_implementation).
:- use_module(singleplayer_normal_difficulty).

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
    (CurrentPlayer = 'Computer 1' -> BoardID = 1, ValidMoves = Player1ValidMoves, OpponentMoves = Player2ValidMoves;
     CurrentPlayer = 'Computer 2' -> BoardID = 2, ValidMoves = Player2ValidMoves, OpponentMoves = Player1ValidMoves),

    % Check if the current player has valid moves
    (ValidMoves \= []
    ->  % Make two moves
        random_select(cell(Row1, Col1, '.'), ValidMoves, TempMoves),
        random_select(cell(Row2, Col2, '.'), TempMoves, RemainingMoves),
        format('~w chose: (~w, ~w) and (~w, ~w)~n', [CurrentPlayer, Row1, Col1, Row2, Col2]),
        pick_space(Row1, Col1, 'X', BoardID, GameState, TempGameState1),
        pick_space(Row2, Col2, 'O', BoardID, TempGameState1, NewGameState),

        % Update the valid move lists
        exclude(==(cell(Row1, Col1, '.')), RemainingMoves, UpdatedMoves1),
        exclude(==(cell(Row2, Col2, '.')), UpdatedMoves1, UpdatedValidMoves),
        (CurrentPlayer = 'Computer 1'
        -> NewMoveLists = [UpdatedValidMoves, OpponentMoves], NextPlayer = 'Computer 2';
           NewMoveLists = [OpponentMoves, UpdatedValidMoves], NextPlayer = 'Computer 1'),

        % Check game status or continue
        (multiplayer_game_over(NewGameState, [player('Computer 1', 1, 0), player('Computer 2', 2, 0)], Winner)
        ->  Winner = player(WinnerName, _, WinnerScore),
            print_boards(NewGameState, true),  % Print the final state of the boards
            format('Game over! ~w wins with a score of ~w!~n', [WinnerName, WinnerScore])
        ;   computer_vs_computer_loop(NewGameState, NewMoveLists, NextPlayer, Level))
    ;   % No valid moves for current player
        write(CurrentPlayer), write(' has no valid moves. Skipping turn.'), nl,
        (CurrentPlayer = 'Computer 1' -> NextPlayer = 'Computer 2'; NextPlayer = 'Computer 1'),
        computer_vs_computer_loop(GameState, [Player1ValidMoves, Player2ValidMoves], NextPlayer, Level)).

% exclude(+Goal, +List, -Filtered)
% Removes elements from List that satisfy Goal, returning the remaining elements in Filtered.
exclude(_, [], []).
exclude(Goal, [H|T], Filtered) :-
    (   call(Goal, H)
    ->  exclude(Goal, T, Filtered)
    ;   Filtered = [H|Rest],
        exclude(Goal, T, Rest)
    ).

check_game_status(GameState, CurrentPlayer, Level) :-
    (   multiplayer_game_over(GameState, [player('Player 1', 1, 0), player('Player 2', 2, 0)], Winner)
    ->  Winner = player(Name, _, Score),
        print_boards(GameState, true),
        format('Game over! ~w wins with the fewest lines and squares, score: ~w!~n', [Name, Score])
    ;   (\+ all_boards_full(GameState)
        ->  (CurrentPlayer = 'player' -> NextPlayer = 'computer'; NextPlayer = 'player'),
            player_vs_computer_loop(GameState, NextPlayer, Level)
        ;   write('Game ended in a draw!'), nl)
    ).
