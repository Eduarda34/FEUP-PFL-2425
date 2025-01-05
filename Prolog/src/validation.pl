/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(validation,
          [ valid_cell/3,
            singleplayer_game_over/2,
            update_player_score/3,
            multiplayer_game_over/3,
            all_boards_full/1,
            calculate_player_score/3
          ]).

:- use_module(grid).
:- use_module(library(between)).
:- use_module(library(lists)).
/*
   valid_cell(BoardID, RowLabel, ColLabel)
   Ensures the cell contains '.'
   
 */
valid_cell(board(_, Rows, Cols, Cells), Row, Col) :-
    member(Row, Rows),
    member(Col, Cols),
    member(cell(Row, Col, '.'), Cells).

/*
    adjacent_pair(+List, -First, -Second)
    Checks if First and Second are adjacent elements in the List.
 */
adjacent_pair([First, Second | _], First, Second).
adjacent_pair([_ | Rest], First, Second) :-
    adjacent_pair(Rest, First, Second).

/*
    has_four_horizontal(+Game, +BoardID)
    Succeeds if the specified board contains at least one horizontal sequence of four identical symbols.
*/
has_four_horizontal(Game, BoardID) :-
    get_board(Game, BoardID, board(BoardID, Rows, Cols, _)),
    member(Row, Rows),
    consecutive_four(Game, BoardID, Row, Cols, Symbol),
    Symbol \= '.'.

/*
    consecutive_four(+Game, +BoardID, +Row, +Cols, -Symbol)
    Checks if there are four consecutive cells in the specified row with the same non-empty symbol.
*/
consecutive_four(Game, BoardID, Row, [C1, C2, C3, C4 | _], Symbol) :-
    get_symbol(Game, BoardID, Row, C1, Symbol),
    get_symbol(Game, BoardID, Row, C2, Symbol),
    get_symbol(Game, BoardID, Row, C3, Symbol),
    get_symbol(Game, BoardID, Row, C4, Symbol).

consecutive_four(Game, BoardID, Row, [_ | RestCols], Symbol) :-
    consecutive_four(Game, BoardID, Row, RestCols, Symbol).

/*
    has_four_vertical(+Game, +BoardID)
    Succeeds if the specified board contains at least one vertical sequence of four identical symbols.
*/
has_four_vertical(Game, BoardID) :-
    get_board(Game, BoardID, board(BoardID, Rows, Cols, _)),
    member(Col, Cols),
    consecutive_four_vertical(Game, BoardID, Col, Rows, Symbol),
    Symbol \= '.'.

/*
    consecutive_four_vertical(+Game, +BoardID, +Col, +Rows, -Symbol)
    Checks if there are four consecutive cells in the specified column with the same non-empty symbol.
*/
consecutive_four_vertical(Game, BoardID, Col, [R1, R2, R3, R4 | _], Symbol) :-
    get_symbol(Game, BoardID, R1, Col, Symbol),
    get_symbol(Game, BoardID, R2, Col, Symbol),
    get_symbol(Game, BoardID, R3, Col, Symbol),
    get_symbol(Game, BoardID, R4, Col, Symbol).

consecutive_four_vertical(Game, BoardID, Col, [_ | RestRows], Symbol) :-
    consecutive_four_vertical(Game, BoardID, Col, RestRows, Symbol).

/*
    has_four_diagonal(+Game, +BoardID)
    Succeeds if the specified board contains at least one diagonal sequence of four identical symbols.
*/
has_four_diagonal(Game, BoardID) :-
    get_board(Game, BoardID, board(BoardID, Rows, Cols, _)),
    length(Rows, NRows),
    length(Cols, NCols),

    MaxRow is NRows - 3,
    MaxCol is NCols - 3,
    between(1, MaxRow, StartRowNum),
    between(1, MaxCol, StartColNum),
    check_diagonal(Game, BoardID, StartRowNum, StartColNum, Symbol),
    Symbol \= '.'.

has_four_diagonal(Game, BoardID) :-
    get_board(Game, BoardID, board(BoardID, Rows, Cols, _)),
    length(Rows, NRows),
    length(Cols, NCols),

    MaxRow is NRows - 3,
    between(1, MaxRow, StartRowNum),
    between(4, NCols, StartColNum),
    check_antidiagonal(Game, BoardID, StartRowNum, StartColNum, Symbol),
    Symbol \= '.'.

/*
    check_diagonal(+Game, +BoardID, +StartRowNum, +StartColNum, -Symbol)
    Checks for a main diagonal sequence of four identical symbols starting from (StartRow, StartCol).
*/
check_diagonal(Game, BoardID, StartRowNum, StartColNum, Symbol) :-
    get_board(Game, BoardID, board(BoardID, Rows, Cols, _)),
    nth1(StartRowNum, Rows, Row1),
    nth1(StartColNum, Cols, Col1),

    R2 is StartRowNum + 1,
    C2 is StartColNum + 1,
    nth1(R2, Rows, Row2),
    nth1(C2, Cols, Col2),

    R3 is R2 + 1,
    C3 is C2 + 1,
    nth1(R3, Rows, Row3),
    nth1(C3, Cols, Col3),

    R4 is R3 + 1,
    C4 is C3 + 1,
    nth1(R4, Rows, Row4),
    nth1(C4, Cols, Col4),

    get_symbol(Game, BoardID, Row1, Col1, Symbol),
    get_symbol(Game, BoardID, Row2, Col2, Symbol),
    get_symbol(Game, BoardID, Row3, Col3, Symbol),
    get_symbol(Game, BoardID, Row4, Col4, Symbol).

/*
    check_antidiagonal(+Game, +BoardID, +StartRowNum, +StartColNum, -Symbol)
    Checks for a secondary diagonal sequence of four identical symbols starting from (StartRow, StartCol).
*/
check_antidiagonal(Game, BoardID, StartRowNum, StartColNum, Symbol) :-
    get_board(Game, BoardID, board(BoardID, Rows, Cols, _)),
    nth1(StartRowNum, Rows, Row1),
    nth1(StartColNum, Cols, Col1),

    R2 is StartRowNum + 1,
    C2 is StartColNum - 1,
    nth1(R2, Rows, Row2),
    nth1(C2, Cols, Col2),

    R3 is R2 + 1,
    C3 is C2 - 1,
    nth1(R3, Rows, Row3),
    nth1(C3, Cols, Col3),

    R4 is R3 + 1,
    C4 is C3 - 1,
    nth1(R4, Rows, Row4),
    nth1(C4, Cols, Col4),

    get_symbol(Game, BoardID, Row1, Col1, Symbol),
    get_symbol(Game, BoardID, Row2, Col2, Symbol),
    get_symbol(Game, BoardID, Row3, Col3, Symbol),
    get_symbol(Game, BoardID, Row4, Col4, Symbol).


/*
    has_square(+Game, +BoardID)
    Succeeds if the specified board contains at least one 2x2 square of identical symbols.
*/
has_square(Game, BoardID) :-
    get_board(Game, BoardID, board(BoardID, Rows, Cols, _)),
    adjacent_pair(Rows, R1, R2),
    adjacent_pair(Cols, C1, C2),
    get_symbol(Game, BoardID, R1, C1, Symbol),
    Symbol \= '.',
    get_symbol(Game, BoardID, R1, C2, Symbol),
    get_symbol(Game, BoardID, R2, C1, Symbol),
    get_symbol(Game, BoardID, R2, C2, Symbol).

/*
    has_losing_condition(+Game, +BoardID)
    Succeeds if the specified board meets any losing condition.
*/
has_losing_condition(Game, BoardID) :-
    has_four_horizontal(Game, BoardID);
    has_four_vertical(Game, BoardID);
    has_four_diagonal(Game, BoardID);
    has_square(Game, BoardID).


/*
    singleplayer_loses(+Game)
    Succeeds if any of the boards in the game meet a losing condition.
*/
singleplayer_loses(Game) :-
    has_losing_condition(Game, 1);
    has_losing_condition(Game, 2).

/*
    all_boards_full(+Game)
    Succeeds if both boards in the game are completely filled (no empty cells).
*/
all_boards_full(Game) :-
    get_board(Game, 1, board(1, _, _, Cells1)),
    get_board(Game, 2, board(2, _, _, Cells2)),
    \+ member(cell(_, _, '.'), Cells1),
    \+ member(cell(_, _, '.'), Cells2).


/*
    singleplayer_game_over(+Game, -Status)
    Determines the game's status for single-player mode. 
    - Status can be:
        * 'lost' if the player loses on any board,
        * 'won' if all boards are full, or
        * 'ongoing' if the game is still in progress.
*/
singleplayer_game_over(Game, Status) :-
    (
        singleplayer_loses(Game)
    ->
        Status = lost
    ;   all_boards_full(Game)
    ->
        Status = won
    ;   Status = ongoing
    ).

/*
    calculate_player_score(+Game, +BoardID, -Score)
    Calculates the player's score for the given board based on the number of:
        - Horizontal lines of four identical symbols.
        - Vertical lines of four identical symbols.
        - Diagonal lines of four identical symbols.
        - 2x2 squares of identical symbols.
    Returns the total score.
*/
calculate_player_score(Game, BoardID, Score) :-
    % Calculate the number of horizontal lines
    findall(1, has_four_horizontal(Game, BoardID), HorizontalScores),
    % Calculate the number of vertical lines
    findall(1, has_four_vertical(Game, BoardID), VerticalScores),
    % Calculate the number of diagonal lines
    findall(1, has_four_diagonal(Game, BoardID), DiagonalScores),
    % Calculate the number of 2x2 squares
    findall(1, has_square(Game, BoardID), SquareScores),
    % Sum all scores
    append([HorizontalScores, VerticalScores, DiagonalScores, SquareScores], AllScores),
    % Total score
    length(AllScores, Score).


/*
    update_player_score(+Game, +BoardID, +Player, -UpdatedPlayer)
    Updates a player's score based on the current board state.
    - Player: Original player in the form player(Name, BoardID, OldScore).
    - UpdatedPlayer: Updated player in the form player(Name, BoardID, NewScore).
*/
update_player_score(Game, player(Name,BoardID, OldScore), player(Name,BoardID, NewScore)) :-
    calculate_player_score(Game,BoardID, Score),
    NewScore is OldScore + Score.


/*
    determine_winner(+Players, -Winner)
    Determines the player with the lowest score from a list of players.
    - Players: A list of players in the form player(Name, BoardID, Score).
    - Winner: The player with the smallest score.
*/
determine_winner([Player1, Player2], Winner) :-
    Player1 = player(Name1, _, Score1),
    Player2 = player(Name2, _, Score2),
    format('Player 1: ~w with score ~w~n', [Name1, Score1]),
    format('Player 2: ~w with score ~w~n', [Name2, Score2]),
    (   Score1 =< Score2
    ->  Winner = Player1
    ;   Winner = Player2).


/*
    calculate_scores(+Game, +Players, -ScoredPlayers)
    Calculates and updates the scores for all players based on the current game state.
    - Players: List of players in the form player(Name, BoardID, _).
    - ScoredPlayers: Updated list of players with calculated scores.
*/
calculate_scores(Game, Players, ScoredPlayers) :-
    findall(
        player(Name, BoardID, Score),
        (
            member(player(Name, BoardID, _), Players),
            calculate_player_score(Game, BoardID, Score)
        ),
        ScoredPlayers
    ).

/*
    multiplayer_game_over(+Game, +Players, -Winner)
    Determines the winner in multiplayer mode when all boards are full.
    - Game: Current game state.
    - Players: List of players.
    - Winner: The player with the lowest score.
*/
multiplayer_game_over(Game, Players, Winner) :-
    all_boards_full(Game),
    calculate_scores(Game, Players, ScoredPlayers),
    determine_winner(ScoredPlayers, Winner).
