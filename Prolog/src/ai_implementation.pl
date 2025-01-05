/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(ai_implementation,
          [ 
            play_player_vs_computer/1,
            choose_move/3,
            find_valid_moves/2,
            find_valid_moves/3,
            random_select/3,
            valid_move/3,
            valid_row_col/2,
            simulate_move/4
          ]).

:- use_module(library(random)).
:- use_module(grid).
:- use_module(input_helpers).
:- use_module(validation).

/*
    other_player(+CurrentPlayer, -OtherPlayer)
    Utility to switch between 'computer' and 'player'.
*/
other_player('computer', 'player').
other_player('player', 'computer').

/*
    play_player_vs_computer(+Level)
    Main entry point for a Player vs Computer game at the given AI Level.
    Initializes the boards and starts the main game loop.
*/
play_player_vs_computer(Level) :-
    init_boards(2, GameState), % Initialize the game state
    write('Starting Player vs Computer Game!'), nl,
    player_vs_computer_loop(GameState, 'player', Level).

/*
    player_vs_computer_loop(+GameState, +CurrentPlayer, +Level)
    Main loop that alternates turns between the player and the computer.
*/
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
        nl,
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

/*
    check_game_status(+GameState, +CurrentPlayer, +Level)
    Checks the game status after a move to determine if the game has ended or continues.
*/
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

/*
    choose_move(+GameState, +Level, -Move)
    Determines a move based on the selected AI Level:
      - Level 1: Random valid move.
      - Level 2: Greedy, picks best immediate score.
      - Level 3: Minimax with alpha-beta pruning.
*/
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

choose_move(GameState, 3, Move) :- 
    Depth = 10,
    CurrentPlayer = computer, 
    minimax_decision(GameState, Depth, CurrentPlayer, Move).

/*
    find_valid_moves(+GameState, -ValidMoves)
    Collects all empty cells ('.') from both boards, returning them in ValidMoves.
*/
find_valid_moves(GameState, ValidMoves) :-
    findall(cell(Row, Col, '.'), (
        default_rows(Rows), default_cols(Cols),
        member(Row, Rows),
        member(Col, Cols),
        get_symbol(GameState, 1, Row, Col, '.'),
        get_symbol(GameState, 2, Row, Col, '.')
    ), ValidMoves).

/*
    find_valid_moves(+GameState, +OpponentMoves, -ValidMoves)
    Collects all empty cells from both boards, excluding those in OpponentMoves.
*/
find_valid_moves(GameState, OpponentMoves, ValidMoves) :-
    default_rows(Rows),
    default_cols(Cols),
    findall(cell(Row, Col, '.'), (
        member(Row, Rows),
        member(Col, Cols),
        \+ member(cell(Row, Col, '.'), OpponentMoves),
        get_symbol(GameState, 1, Row, Col, '.'),
        get_symbol(GameState, 2, Row, Col, '.')
    ), ValidMoves).

/*
    valid_move(+GameState, +Row, +Col)
    Checks if the specified Row, Col is empty on both boards (hence valid).
*/
valid_move(GameState, Row, Col) :-
    get_symbol(GameState, 1, Row, Col, '.'),
    get_symbol(GameState, 2, Row, Col, '.').

/*
    valid_row_col(+Row, +Col)
    Ensures the Row and Col are within the acceptable range ('a'..'h' and 1..8).
*/
valid_row_col(Row, Col) :-
    member(Row, [a, b, c, d, e, f, g, h]),
    member(Col, [1, 2, 3, 4, 5, 6, 7, 8]).

/*
    simulate_move(+GameState, +Row, +Col, -Score)
    Simulates placing 'X' in one board and calculates the resulting player score.
*/
simulate_move(GameState, Row, Col, Score) :-
    pick_space(Row, Col, 'X', 1, GameState, NewGameState),
    calculate_player_score(NewGameState, 1, Score).

/*
    all_possible_moves(+GameState, -Moves)
    Generates all possible pairs of distinct empty cells for the minimax algorithm.
*/
all_possible_moves(GameState, Moves) :-
    find_valid_moves(GameState, ValidCells),
    findall(move((R1, C1), (R2, C2)),
            (   member(cell(R1, C1, '.'), ValidCells),
                member(cell(R2, C2, '.'), ValidCells),
                (R1, C1) \= (R2, C2)
            ),
            Moves).

/*
    minimax_decision(+GameState, +Depth, +Player, -BestMove)
    Entry point for minimax with alpha-beta pruning to determine the best move.
*/
minimax_decision(GameState, Depth, Player, BestMove) :-
    (   
        all_possible_moves(GameState, [])
    ->  BestMove = none
    ;   Alpha is -1000000,
        Beta  is  1000000,
        minimax(GameState, Depth, Alpha, Beta, Player, BestMove, _BestScore)
    ).

/*
    minimax(+GameState, +Depth, +Alpha, +Beta, +Player, -Move, -Score)
    Minimax algorithm with alpha-beta pruning to evaluate game states.
*/
minimax(GameState, Depth, _, _, _, BestMove, BestScore) :-
    (   
        Depth = 0
    ;   game_is_over(GameState)
    ),
    !,
    evaluate(GameState, Score),
    BestMove = none,
    BestScore = Score.

minimax(GameState, Depth, Alpha, Beta, Player, BestMove, BestScore) :-
    all_possible_moves(GameState, Moves),
    Moves \= [],
    NextDepth is Depth - 1,
    ( Player = computer
    ->  find_best_move_min(Moves, GameState, NextDepth, Alpha, Beta, Player, none, 1000000, BestMove, BestScore)
    ;   find_best_move_max(Moves, GameState, NextDepth, Alpha, Beta, Player, none, -1000000, BestMove, BestScore)
    ).

/*
    find_best_move_max(+Moves, +GameState, +Depth, +Alpha, +Beta, +Player, +TempBestMove, +TempBestScore, -BestMove, -BestScore)
    Iterates over Moves to find the best one for a maximizing player.
*/
find_best_move_max([], _, _, _, _, _, BestMove, BestScore, FinalMove, FinalScore) :-
    FinalMove = BestMove,
    FinalScore = BestScore,
    !.

find_best_move_max([Move|Moves], GameState, Depth, Alpha, Beta, Player, TempBestMove, TempBestScore, BestMove, BestScore) :-
    apply_move(GameState, Move, Player, NextGameState),
    other_player(Player, Opponent),
    minimax(NextGameState, Depth, Alpha, Beta, Opponent, _ReturnedMove, Score),
    (   
        Score > TempBestScore
    ->  NewBestScore = Score,
        NewBestMove  = Move,
        NewAlpha     is max(Alpha, Score)
    ;   NewBestScore = TempBestScore,
        NewBestMove  = TempBestMove,
        NewAlpha     = Alpha
    ),
    (NewAlpha >= Beta
    ->  BestMove = NewBestMove,
        BestScore = NewBestScore
    ;   find_best_move_max(Moves, GameState, Depth, NewAlpha, Beta, Player, NewBestMove, NewBestScore, BestMove, BestScore)
    ).

/*
    find_best_move_min(+Moves, +GameState, +Depth, +Alpha, +Beta, +Player, +TempBestMove, +TempBestScore, -BestMove, -BestScore)
    Iterates over Moves to find the best one for a minimizing player.
*/
find_best_move_min([], _, _, _, _, _, BestMove, BestScore, FinalMove, FinalScore) :-
    FinalMove = BestMove,
    FinalScore = BestScore,
    !.

find_best_move_min([Move|Moves], GameState, Depth, Alpha, Beta, Player, TempBestMove, TempBestScore, BestMove, BestScore) :-
    apply_move(GameState, Move, Player, NextGameState),
    other_player(Player, Opponent),
    minimax(NextGameState, Depth, Alpha, Beta, Opponent, _ReturnedMove, Score),
    (   
        Score < TempBestScore
    ->  NewBestScore = Score,
        NewBestMove  = Move,
        NewBeta      is min(Beta, Score)
    ;   NewBestScore = TempBestScore,
        NewBestMove  = TempBestMove,
        NewBeta      = Beta
    ),
    (Alpha >= NewBeta
    ->  BestMove = NewBestMove,
        BestScore = NewBestScore
    ;   find_best_move_min(Moves, GameState, Depth, Alpha, NewBeta, Player, NewBestMove, NewBestScore, BestMove, BestScore)
    ).

/*
    apply_move(+GameState, +move((Rx, Cx), (Ro, Co)), +Player, -NextGameState)
    Applies the move (placing 'X' and 'O') for the current Player on both boards.
*/
apply_move(GameState, move((Rx, Cx), (Ro, Co)), Player, NextGameState) :-
    (
      Player = computer 
    -> SymbolX = 'X', SymbolO = 'O'
    ;  SymbolX = 'X', SymbolO = 'O'
    ),
    pick_space(Rx, Cx, SymbolX, 1, GameState, TempState1),
    pick_space(Rx, Cx, SymbolX, 2, TempState1, TempState2),
    pick_space(Ro, Co, SymbolO, 1, TempState2, TempState3),
    pick_space(Ro, Co, SymbolO, 2, TempState3, NextGameState).

/*
    evaluate(+GameState, -Score)
    Simple heuristic: difference between the computer's score (board 1) and the human's score (board 2).
*/
evaluate(GameState, Score) :-
    calculate_player_score(GameState, 1, ComputerScore),
    calculate_player_score(GameState, 2, HumanScore),
    Score is ComputerScore - HumanScore.

/*
    game_is_over(+GameState)
    Checks if the game is over by either a winner or full boards.
*/
game_is_over(GameState) :-
    multiplayer_game_over(GameState, _, _)
    ; 
    all_boards_full(GameState).