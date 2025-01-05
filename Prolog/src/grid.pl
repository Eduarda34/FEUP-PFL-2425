/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(grid,
          [ default_cols/1,
            default_rows/1,
            init_boards/1,
            init_boards/2,
            pick_space/5,
            pick_space/6,
            get_board/3,
            get_symbol/5,
            print_col_header/1,
            print_rows/3,
            print_boards/2
          ]).

:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(validation).

/*
   cell(BoardID, RowLabel, ColLabel, Symbol).
   Exemplo: cell(1, a, 1, '.') => Tabuleiro #1 na posição (A,1) contém '.'.

   board2_rows e board2_cols são gerenciados na estrutura de estado.
*/

/* 
   Representação do Estado do Jogo:
   game(Board1, Board2, Board3)

   Board:
   board(BoardID, Rows, Cols, Cells)

   Cells é uma lista de cell(Row, Col, Symbol).
*/

singleplayer_rows([a,b,c,d,e,f]).
singleplayer_cols([1,2,3,4,5,6]).
default_rows([a,b,c,d,e,f,g,h]).
default_cols([1,2,3,4,5,6,7,8]).

%% random_shuffled_list(+List, -Shuffled)
%% Shuffles List using random_select/3.
random_shuffled_list([], []).
random_shuffled_list(List, [X|Xs]) :-
    random_select(X, List, Rest),
    random_shuffled_list(Rest, Xs).

% get_board(+Game, +BoardID, -Board)
% Retrieves the BoardID-th board from the game state.
get_board(Game, BoardID, Board) :-
    arg(BoardID, Game, Board).

% get_symbol(+Game, +BoardID, +Row, +Col, -Symbol)
% Retrieves the symbol at the given Row and Col in the specified BoardID.
get_symbol(Game, BoardID, Row, Col, Symbol) :-
    get_board(Game, BoardID, board(BoardID, _, _, Cells)),
    member(cell(Row, Col, Symbol), Cells).

%% init_boards(-State)
%% Initializes the game state with either two or three boards.
init_boards(game(Board1, Board2)) :-
    singleplayer_rows(AllRows),
    singleplayer_cols(AllCols),
    random_shuffled_list(AllRows, ShuffledRows1),
    random_shuffled_list(AllCols, ShuffledCols1),
    initialize_cells(ShuffledRows1, ShuffledCols1, Cells1),
    Board1 = board(1, ShuffledRows1, ShuffledCols1, Cells1),
    random_shuffled_list(AllRows, ShuffledRows2),
    random_shuffled_list(AllCols, ShuffledCols2),
    initialize_cells(ShuffledRows2, ShuffledCols2, Cells2),
    Board2 = board(2, ShuffledRows2, ShuffledCols2, Cells2).

init_boards(2, game(Board1, Board2)) :- % For 2-player mode
    default_rows(AllRows),
    default_cols(AllCols),

    % Initialize Board 1
    random_shuffled_list(AllRows, ShuffledRows1),
    random_shuffled_list(AllCols, ShuffledCols1),
    initialize_cells(ShuffledRows1, ShuffledCols1, Cells1),
    Board1 = board(1, ShuffledRows1, ShuffledCols1, Cells1),

    % Initialize Board 2
    random_shuffled_list(AllRows, ShuffledRows2),
    random_shuffled_list(AllCols, ShuffledCols2),
    initialize_cells(ShuffledRows2, ShuffledCols2, Cells2),
    Board2 = board(2, ShuffledRows2, ShuffledCols2, Cells2).

init_boards(3, game(Board1, Board2, Board3)) :- % For 3-player mode
    default_rows(AllRows),
    default_cols(AllCols),

    % Initialize Board 1
    random_shuffled_list(AllRows, ShuffledRows1),
    random_shuffled_list(AllCols, ShuffledCols1),
    initialize_cells(ShuffledRows1, ShuffledCols1, Cells1),
    Board1 = board(1, ShuffledRows1, ShuffledCols1, Cells1),

    % Initialize Board 2
    random_shuffled_list(AllRows, ShuffledRows2),
    random_shuffled_list(AllCols, ShuffledCols2),
    initialize_cells(ShuffledRows2, ShuffledCols2, Cells2),
    Board2 = board(2, ShuffledRows2, ShuffledCols2, Cells2),

    % Initialize Board 3
    random_shuffled_list(AllRows, ShuffledRows3),
    random_shuffled_list(AllCols, ShuffledCols3),
    initialize_cells(ShuffledRows3, ShuffledCols3, Cells3),
    Board3 = board(3, ShuffledRows3, ShuffledCols3, Cells3).

%% initialize_cells(+Rows, +Cols, -Cells)
%% Initializes the cells with the symbol '.'.
initialize_cells(Rows, Cols, Cells) :-
    findall(cell(R, C, '.'), (member(R, Rows), member(C, Cols)), Cells).

/* ----------------------------------------------------------------------
   2. SELEÇÃO DE ESPAÇO (COORDENADA) E COLOCAÇÃO DE SÍMBOLO
   ---------------------------------------------------------------------- */

/* 
   pick_space(+RowLabel, +ColLabel, +Symbol, +State, -NewState)
   Places the Symbol at the specified Row and Col in the game state.
*/

%% Definir símbolos válidos
valid_symbol('X').
valid_symbol('O').
valid_symbol('.').

/* 
   pick_space(+RowLabel, +ColLabel, +Symbol, +State, -NewState)
   Places the Symbol at the specified Row and Col across all boards in the game state.
*/
pick_space(RowLabel, ColLabel, Symbol, Game, NewGame) :-
    valid_symbol(Symbol),
    % Verifica se a célula é válida e está vazia no Tabuleiro #1
    functor(Game, game, NumBoards),
    place_symbol_on_all_boards(Game, NumBoards, RowLabel, ColLabel, Symbol, NewGame).
pick_space(Row, Col, Symbol, BoardID, game(Board1, Board2), game(NewBoard1, Board2)) :-
    BoardID =:= 1,
    update_board(Board1, Row, Col, Symbol, NewBoard1),
    !. % Ensure the board update succeeds before proceeding
pick_space(Row, Col, Symbol, BoardID, game(Board1, Board2), game(Board1, NewBoard2)) :-
    BoardID =:= 2,
    update_board(Board2, Row, Col, Symbol, NewBoard2),
    !. % Ensure the board update succeeds before proceeding

/*
   place_symbol_on_all_boards(+Game, +NumBoards, +Row, +Col, +Symbol, -NewGame)
   Recursively attempts to place the symbol on all boards.
*/
place_symbol_on_all_boards(Game, 0, _, _, _, Game). % Base case: No more boards to update.
place_symbol_on_all_boards(Game, BoardID, Row, Col, Symbol, UpdatedGame) :-
    BoardID > 0,
    get_board(Game, BoardID, Board),
    (   valid_cell(Board, Row, Col) % Check if the cell is valid for placement
    ->  update_board(Board, Row, Col, Symbol, UpdatedBoard),
        replace_board(Game, BoardID, UpdatedBoard, IntermediateGame)
    ;   IntermediateGame = Game % If not valid, move to the next board
    ),
    PrevBoardID is BoardID - 1,
    place_symbol_on_all_boards(IntermediateGame, PrevBoardID, Row, Col, Symbol, UpdatedGame).

/*
   replace_board(+Game, +BoardID, +UpdatedBoard, -NewGame)
   Replaces the specified board in the game state.
*/
replace_board(Game, BoardID, UpdatedBoard, NewGame) :-
    functor(Game, game, NumBoards),
    functor(NewGame, game, NumBoards),
    arg(BoardID, NewGame, UpdatedBoard), % Replace the specific board
    copy_boards_except(Game, NewGame, BoardID, NumBoards).

/*
   copy_boards_except(+Game, +NewGame, +ReplaceID, +CurrentID)
   Copies all boards from the original game state to the new game state,
   except for the replaced board.
*/
copy_boards_except(_, _, _, 0).
copy_boards_except(Game, NewGame, ReplaceID, CurrentID) :-
    CurrentID > 0,
    (   CurrentID \= ReplaceID
    ->  arg(CurrentID, Game, Board),
        arg(CurrentID, NewGame, Board)
    ;   true
    ),
    PrevID is CurrentID - 1,
    copy_boards_except(Game, NewGame, ReplaceID, PrevID).

%% update_board(+Board, +Row, +Col, +Symbol, -NewBoard)
%% Updates the specified cell with the given symbol.
update_board(board(ID, Rows, Cols, Cells), Row, Col, Symbol, board(ID, Rows, Cols, NewCells)) :-
    maplist(update_cell(Row, Col, Symbol), Cells, NewCells).

%% update_cell(+Row, +Col, +Symbol, +cell(R, C, S), -NewCell)
%% Replaces the symbol in the cell if the Row and Col match.
update_cell(Row, Col, Symbol, cell(R, C, S), cell(R, C, NewS)) :-
    (   R = Row, C = Col
    ->  NewS = Symbol
    ;   NewS = S
    ).

/* ----------------------------------------------------------------------
   3. IMPRESSÃO DOS TABULEIROS
   ---------------------------------------------------------------------- */

/* 
   print_boards(+State, +PlayerMode)
   Dynamically prints the appropriate number of boards in the game state.
*/
print_boards(Game, _) :-
    functor(Game, game, NumBoards), % Get the number of boards in the game state
    print_all_boards(Game, 1, NumBoards).

/*
   print_all_boards(+Game, +CurrentBoardID, +TotalBoards)
   Iteratively prints all boards in the game state up to TotalBoards.
*/
print_all_boards(_, Current, Total) :-
    Current > Total. % Base case: No more boards to print.
print_all_boards(Game, Current, Total) :-
    get_board(Game, Current, Board), % Get the current board
    print_board(Board, Current),     % Print the board with the player ID
    nl,
    Next is Current + 1,
    print_all_boards(Game, Next, Total).

/*
   print_board(+Board, +PlayerID)
   Prints a single board associated with a specific player.
*/
print_board(board(_, Rows, Cols, Cells), PlayerID) :-
    format('--- Player ~w ---~n', [PlayerID]),
    nl,
    print_col_header(Cols),
    print_rows(Rows, Cols, Cells).

%% print_col_header(+Cols)
%% Prints the column header.
print_col_header(Cols) :-
    write('   '),
    print_cols(Cols),
    nl,
    write('  '),
    print_separator(Cols),
    nl.

%% print_cols(+Cols)
%% Prints column names with spacing.
print_cols([]).
print_cols([C|Cs]) :-
    format(' ~w ', [C]),
    print_cols(Cs).

%% print_separator(+Cols)
%% Prints a separator line under the column header.
print_separator([]).
print_separator([_|Cs]) :-
    write('---'),
    print_separator(Cs).

%% print_rows(+Rows, +Cols, +Cells)
%% Prints each row of the board.
print_rows([], _, _).
print_rows([R|Rs], Cols, Cells) :-
    format('~w |', [R]),
    print_cells(R, Cols, Cells),
    nl,
    print_rows(Rs, Cols, Cells).

%% print_cells(+Row, +Cols, +Cells)
%% Prints the cells in a specific row.
print_cells(_, [], _).
print_cells(Row, [C|Cs], Cells) :-
    (   member(cell(Row, C, Symbol), Cells)
    ->  format(' ~w ', [Symbol])
    ;   format(' ~w ', [' . '])
    ),
    print_cells(Row, Cs, Cells).
