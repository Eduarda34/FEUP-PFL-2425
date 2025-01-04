:- module(player_vs_computer,
          [ play_player_vs_computer/1,
            choose_move/3
          ]).

:- use_module(grid).
:- use_module(singleplayer_normal_difficulty).
:- use_module(library(random)).

% Entry point for the Player vs Computer game
play_player_vs_computer(Level) :-
    init_boards(2, GameState), % Initialize the game state
    write('Starting Player vs Computer Game!'), nl,
    write('You are X and O, and the computer is X and O.'), nl,
    player_vs_computer_loop(GameState, 'player', Level).

% Main loop for alternating turns between the player and the computer
player_vs_computer_loop(GameState, 'player', Level) :- % Player's turn
    print_boards(GameState, true),
    write('Your turn!'), nl,
    prompt_two_moves(Row1, Col1, Row2, Col2),
    (
        pick_space(Row1, Col1, 'X', 1, GameState, TempGameState1),
        pick_space(Row1, Col1, 'X', 2, TempGameState1, TempGameState2),
        pick_space(Row2, Col2, 'O', 1, TempGameState2, TempGameState3),
        pick_space(Row2, Col2, 'O', 2, TempGameState3, NewGameState)
    ->
        check_game_status(NewGameState, 'player', Level)
    ;
        write('Invalid moves. Try again.'), nl,
        player_vs_computer_loop(GameState, 'player', Level)
    ).

player_vs_computer_loop(GameState, 'computer', Level) :- % Computer's turn
    print_boards(GameState, true),
    write('Computer\'s turn!'), nl,
    find_valid_moves(GameState, ValidMoves),
    format('Valid moves available: ~w~n', [ValidMoves]), % Debugging output
    (   length(ValidMoves, L), L >= 2
    ->  random_select(cell(Row1, Col1, '.'), ValidMoves, TempMoves),
        random_select(cell(Row2, Col2, '.'), TempMoves, _),
        format('Computer chose: (~w, ~w) and (~w, ~w)~n', [Row1, Col1, Row2, Col2]),
        pick_space(Row1, Col1, 'X', 1, GameState, TempGameState1),
        pick_space(Row1, Col1, 'X', 2, TempGameState1, TempGameState2),
        pick_space(Row2, Col2, 'O', 1, TempGameState2, TempGameState3),
        pick_space(Row2, Col2, 'O', 2, TempGameState3, NewGameState),
        check_game_status(NewGameState, 'computer', Level)
    ;   (length(ValidMoves, L), L == 0
        -> write('No valid moves left. Game over!'), nl,
           print_boards(GameState, true),
           write('It\'s a draw!'), nl
        ; write('Computer cannot make a valid move.'), nl,
          player_vs_computer_loop(GameState, 'computer', Level))
    ).

% Checks the game state after a move and continues or ends the game
check_game_status(GameState, CurrentPlayer, Level) :-
    singleplayer_game_over(GameState, Status),
    (
        Status = won
    ->
        print_boards(GameState, true),
        format('Game over! ~w wins!~n', [CurrentPlayer])
    ;   Status = lost
    ->
        print_boards(GameState, true),
        format('Game over! ~w loses!~n', [CurrentPlayer])
    ;   % Continue the game, alternate turns
        (CurrentPlayer = 'player' -> NextPlayer = 'computer'; NextPlayer = 'player'),
        player_vs_computer_loop(GameState, NextPlayer, Level)
    ).

% choose_move(+GameState, +Level, -Move)
choose_move(GameState, 1, Move) :- % Level 1: Random valid move
    find_valid_moves(GameState, ValidMoves),
    random_select(cell(Row, Col, '.'), ValidMoves, _),
    Move = (Row, Col).

choose_move(GameState, 2, Move) :- % Level 2: Greedy best move
    findall(
        (Score, Row, Col),
        (valid_cell(GameState, Row, Col), simulate_move(GameState, Row, Col, Score)),
        ScoredMoves
    ),
    sort(1, @>=, ScoredMoves, [(BestScore, Row, Col) | _]),
    format('Chose move with score ~w: (~w, ~w)~n', [BestScore, Row, Col]),
    Move = (Row, Col).

% Finds all valid moves
find_valid_moves(GameState, ValidMoves) :-
    findall(cell(Row, Col, '.'), (
        valid_row_col(Row, Col),
        get_symbol(GameState, 1, Row, Col, '.'),
        get_symbol(GameState, 2, Row, Col, '.')), ValidMoves).

% Ensures the row and column are within valid bounds
valid_row_col(Row, Col) :-
    member(Row, [a, b, c, d, e, f, g, h]),
    member(Col, [1, 2, 3, 4, 5, 6, 7, 8]).

% Simulates a move and calculates the score for Level 2
simulate_move(GameState, Row, Col, Score) :-
    pick_space(Row, Col, 'X', 1, GameState, NewGameState),
    calculate_player_score(NewGameState, 1, Score).

% Prompts the player for two moves
prompt_two_moves(Row1, Col1, Row2, Col2) :-
    write('First move (X):'), nl,
    prompt_move(Row1, Col1),
    write('Second move (O):'), nl,
    prompt_move(Row2, Col2).

% Prompts the player for a row and column
prompt_move(Row, Col) :-
    prompt_row(Row),
    prompt_col(Col).

prompt_row(Row) :-
    write('Choose the row (a-h): '),
    read(Row).

prompt_col(Col) :-
    write('Choose the column (1-8): '),
    read(Col).
