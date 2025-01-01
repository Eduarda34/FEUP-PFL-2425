/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(singleplayer_normal_difficulty,
          [ valid_cell/3,
            singleplayer_game_over/2,
            update_player_score/3,
            multiplayer_game_over/3
          ]).

:- use_module(singleplayer_grid).
:- use_module(library(between)).
:- use_module(library(lists)).

% valid_cell(BoardID, RowLabel, ColLabel)
% Ensures the cell contains '.'
valid_cell(board(_, Rows, Cols, Cells), Row, Col) :-
    member(Row, Rows),
    member(Col, Cols),
    member(cell(Row, Col, '.'), Cells).

% adjacent_pair(+List, -First, -Second)
% Verifica se First e Second são elementos adjacentes em List.
adjacent_pair([First, Second | _], First, Second).
adjacent_pair([_ | Rest], First, Second) :-
    adjacent_pair(Rest, First, Second).

% has_four_horizontal(+Game, +BoardID)
% Succeeds se o tabuleiro especificado tiver pelo menos uma sequência horizontal de quatro símbolos iguais.
has_four_horizontal(Game, BoardID) :-
    get_board(Game, BoardID, board(BoardID, Rows, Cols, _)),
    member(Row, Rows),
    consecutive_four(Game, BoardID, Row, Cols, Symbol),
    Symbol \= '.'.

% consecutive_four(+Game, +BoardID, +Row, +Cols, -Symbol)
% Verifica se há quatro células consecutivas na linha com o mesmo símbolo não vazio.
consecutive_four(Game, BoardID, Row, [C1, C2, C3, C4 | _], Symbol) :-
    get_symbol(Game, BoardID, Row, C1, Symbol),
    get_symbol(Game, BoardID, Row, C2, Symbol),
    get_symbol(Game, BoardID, Row, C3, Symbol),
    get_symbol(Game, BoardID, Row, C4, Symbol).

consecutive_four(Game, BoardID, Row, [_ | RestCols], Symbol) :-
    consecutive_four(Game, BoardID, Row, RestCols, Symbol).

% has_four_vertical(+Game, +BoardID)
% Succeeds se o tabuleiro especificado tiver pelo menos uma sequência vertical de quatro símbolos iguais.
has_four_vertical(Game, BoardID) :-
    get_board(Game, BoardID, board(BoardID, Rows, Cols, _)),
    member(Col, Cols),
    consecutive_four_vertical(Game, BoardID, Col, Rows, Symbol),
    Symbol \= '.'.

% consecutive_four_vertical(+Game, +BoardID, +Col, +Rows, -Symbol)
% Verifica se há quatro células consecutivas na coluna com o mesmo símbolo não vazio.
consecutive_four_vertical(Game, BoardID, Col, [R1, R2, R3, R4 | _], Symbol) :-
    get_symbol(Game, BoardID, R1, Col, Symbol),
    get_symbol(Game, BoardID, R2, Col, Symbol),
    get_symbol(Game, BoardID, R3, Col, Symbol),
    get_symbol(Game, BoardID, R4, Col, Symbol).

consecutive_four_vertical(Game, BoardID, Col, [_ | RestRows], Symbol) :-
    consecutive_four_vertical(Game, BoardID, Col, RestRows, Symbol).

% has_four_diagonal(+Game, +BoardID)
% Succeeds se o tabuleiro especificado tiver pelo menos uma sequência diagonal de quatro símbolos iguais.
has_four_diagonal(Game, BoardID) :-
    get_board(Game, BoardID, board(BoardID, Rows, Cols, _)),
    length(Rows, NRows),
    length(Cols, NCols),

    % Diagonais principais (top-left to bottom-right)
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

    % Diagonais secundárias (top-right to bottom-left)
    MaxRow is NRows - 3,
    between(1, MaxRow, StartRowNum),
    between(4, NCols, StartColNum),
    check_antidiagonal(Game, BoardID, StartRowNum, StartColNum, Symbol),
    Symbol \= '.'.

% check_diagonal(+Game, +BoardID, +StartRowNum, +StartColNum, -Symbol)
% Verifica uma sequência diagonal principal de quatro símbolos iguais a partir de (StartRow, StartCol).
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

% check_antidiagonal(+Game, +BoardID, +StartRowNum, +StartColNum, -Symbol)
% Verifica uma sequência diagonal secundária de quatro símbolos iguais a partir de (StartRow, StartCol).
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



% has_square(+Game, +BoardID)
% Succeeds se o tabuleiro especificado tiver pelo menos um quadrado 2x2 de símbolos iguais.
has_square(Game, BoardID) :-
    get_board(Game, BoardID, board(BoardID, Rows, Cols, _)),
    adjacent_pair(Rows, R1, R2),
    adjacent_pair(Cols, C1, C2),
    get_symbol(Game, BoardID, R1, C1, Symbol),
    Symbol \= '.',
    get_symbol(Game, BoardID, R1, C2, Symbol),
    get_symbol(Game, BoardID, R2, C1, Symbol),
    get_symbol(Game, BoardID, R2, C2, Symbol).

% has_losing_condition(+Game, +BoardID)
% Succeeds se o tabuleiro especificado atender a qualquer condição de perda.
has_losing_condition(Game, BoardID) :-
    has_four_horizontal(Game, BoardID);
    has_four_vertical(Game, BoardID);
    has_four_diagonal(Game, BoardID);
    has_square(Game, BoardID).

% player_loses(+Game)
% Succeeds se qualquer um dos tabuleiros atender a condições de perda.
singleplayer_loses(Game) :-
    has_losing_condition(Game, 1);
    has_losing_condition(Game, 2).

% player_wins(+Game)
% Succeeds se ambos os tabuleiros estiverem completos (sem células vazias).
all_boards_full(Game) :-
    get_board(Game, 1, board(1, _, _, Cells1)),
    get_board(Game, 2, board(2, _, _, Cells2)),
    \+ member(cell(_, _, '.'), Cells1),
    \+ member(cell(_, _, '.'), Cells2).


% game_status(+Game, -Status)
% Verifica o status do jogo baseado no estado atual.
% Status pode ser 'ongoing', 'won', 'lost'.
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
    calculate_player_score(+Game,+BoardID, -Score)
    Calcula a pontuação do jogador com base nas linhas e quadrados formados.
*/
calculate_player_score(Game,BoardID, Score) :-
    % Calcula o número de sequências horizontais
    findall(1, has_four_horizontal(Game,BoardID), HorizontalScores),
    % Calcula o número de sequências verticais
    findall(1, has_four_vertical(Game,BoardID), VerticalScores),
    % Calcula o número de sequências diagonais
    findall(1, has_four_diagonal(Game,BoardID), DiagonalScores),
    % Calcula o número de quadrados 2x2
    findall(1, has_square(Game,BoardID), SquareScores),
    % Soma todas as pontuações
    append([HorizontalScores, VerticalScores, DiagonalScores, SquareScores], AllScores),
    length(AllScores, Score).

/*
    update_player_score(+Game, +BoardID,+Player, -UpdatedPlayer)
    Atualiza a pontuação do jogador com base no novo estado do tabuleiro.
*/
update_player_score(Game, player(Name,BoardID, OldScore), player(Name,BoardID, NewScore)) :-
    calculate_player_score(Game,BoardID, Score),
    NewScore is OldScore + Score.

/*
    determine_winner(+Players, -Winner)
    Determina o jogador com a menor pontuação.
    Players: Lista de jogadores na forma player(Name,BoardID, Score).
    Winner: Jogador com a menor pontuação.
*/
determine_winner([Player1, Player2], Winner) :-
    Player1 = player(_, _, Score1),
    Player2 = player(_, _, Score2),
    (   Score1 =< Score2
    ->  Winner = Player1
    ;   Winner = Player2
    ).

multiplayer_game_over(Game,Players, Result) :-
    all_boards_full(Game),
    determine_winner(Players,Result).