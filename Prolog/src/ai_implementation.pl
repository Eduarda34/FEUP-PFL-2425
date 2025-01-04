:- module(ai_implementation,
          [ play_player_vs_computer/1,
            choose_move/3,
            find_valid_moves/2,
            find_valid_moves/3,
            random_select/3,
            valid_move/2,
            valid_row_col/2,
            simulate_move/3
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
        Row1 = Row2, Col1 = Col2
    ->
        write('Cannot choose the same cell for both symbols. Try again.'), nl,
        player_vs_computer_loop(GameState, 'player', Level)
    ;
        (
            valid_move(GameState, Row1, Col1),
            valid_move(GameState, Row2, Col2)
        ->
            pick_space(Row1, Col1, 'X', 1, GameState, TempGameState1),
            pick_space(Row1, Col1, 'X', 2, TempGameState1, TempGameState2),
            pick_space(Row2, Col2, 'O', 1, TempGameState2, TempGameState3),
            pick_space(Row2, Col2, 'O', 2, TempGameState3, NewGameState),
            check_game_status(NewGameState, 'player', Level)
        ;
            write('Invalid moves or coordinates already filled. Try again.'), nl,
            player_vs_computer_loop(GameState, 'player', Level)
        )
    ).

player_vs_computer_loop(GameState, 'computer', Level) :- % Computer's turn
    print_boards(GameState, true),
    write('Computer\'s turn!'), nl,
    find_valid_moves(GameState, ValidMoves),
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
    (   multiplayer_game_over(GameState, [player('Player 1', 1, 0), player('Player 2', 2, 0)], Winner)
    ->  Winner = player(Name, _, Score),
        print_boards(GameState, true),
        format('Game over! ~w wins with the fewest lines and squares, score: ~w!~n', [Name, Score])
    ;   (\+ all_boards_full(GameState)
        ->  (CurrentPlayer = 'player' -> NextPlayer = 'computer'; NextPlayer = 'player'),
            player_vs_computer_loop(GameState, NextPlayer, Level)
        ;   write('Game ended in a draw!'), nl)
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
        default_rows(Rows), default_cols(Cols), % Get all possible rows and columns
        member(Row, Rows),
        member(Col, Cols),
        get_symbol(GameState, 1, Row, Col, '.'), % Check if cell is empty on board 1
        get_symbol(GameState, 2, Row, Col, '.')  % Check if cell is empty on board 2
    ), ValidMoves),
    (ValidMoves = []
    -> write('No valid moves found.'), nl
    ;  format('Valid moves: ~w~n', [ValidMoves])).

find_valid_moves(GameState, OpponentMoves, ValidMoves) :-
    default_rows(Rows),
    default_cols(Cols),
    findall(cell(Row, Col, '.'), (
        member(Row, Rows),
        member(Col, Cols),
        \+ member(cell(Row, Col, '.'), OpponentMoves), % Exclude opponent's moves
        get_symbol(GameState, 1, Row, Col, '.'),
        get_symbol(GameState, 2, Row, Col, '.')
    ), ValidMoves),
    (ValidMoves = []
    -> write('No valid moves found.~n')
    ;  format('Valid moves: ~w~n', [ValidMoves])).

valid_move(GameState, Row, Col) :-
    get_symbol(GameState, 1, Row, Col, '.'),
    get_symbol(GameState, 2, Row, Col, '.').

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
