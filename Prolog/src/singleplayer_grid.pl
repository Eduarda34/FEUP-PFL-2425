/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(singleplayer_grid,
          [ init_boards/1,
            pick_space/5,
            print_boards/1,
            get_board/3,
            get_symbol/5,
            print_col_header/1,
            print_rows/3,
            print_boards/2
               
          ]).
    
:- use_module(library(random)). 
:- use_module(library(lists)).
:- use_module(singleplayer_normal_difficulty).

/*
   cell(BoardID, RowLabel, ColLabel, Symbol).
   Exemplo: cell(1, a, 1, '.') => Tabuleiro #1 na posição (A,1) contém '.'.

   board2_rows e board2_cols são gerenciados na estrutura de estado.
*/


/* 
   Representação do Estado do Jogo:
   game(Board1, Board2)

   Board:
   board(BoardID, Rows, Cols, Cells)

   Cells é uma lista de cell(Row, Col, Symbol).
*/

default_rows([a,b,c,d,e,f]).
default_cols([1,2,3,4,5,6]).

%% random_shuffled_list(+List, -Shuffled)
%% Shuffles List using random_select/3.
random_shuffled_list([], []).
random_shuffled_list(List, [X|Xs]) :-
    random_select(X, List, Rest),
    random_shuffled_list(Rest, Xs).

% get_board(+Game, +BoardID, -Board)
get_board(game(Board1, _), 1, Board1).
get_board(game(_, Board2), 2, Board2).

% get_symbol(+Game, +BoardID, +Row, +Col, -Symbol)
get_symbol(game(Board1, Board2), BoardID, Row, Col, Symbol) :-
    get_board(game(Board1, Board2), BoardID, board(BoardID, _, _, Cells)),
    member(cell(Row, Col, Symbol), Cells).

%% init_boards(-State)
%% Inicializa os tabuleiros.
init_boards(game(Board1, Board2)) :-
    default_rows(AllRows),
    default_cols(AllCols),
    random_shuffled_list(AllRows, ShuffledRows1),
    random_shuffled_list(AllCols, ShuffledCols1),
    initialize_cells(ShuffledRows1, ShuffledCols1, Cells1),
    Board1 = board(1, ShuffledRows1, ShuffledCols1, Cells1),
    random_shuffled_list(AllRows, ShuffledRows2),
    random_shuffled_list(AllCols, ShuffledCols2),
    initialize_cells(ShuffledRows2, ShuffledCols2, Cells2),
    Board2 = board(2, ShuffledRows2, ShuffledCols2, Cells2).

%% initialize_cells(+Rows, +Cols, -Cells)
%% Inicializa as células com o símbolo '.'.
initialize_cells(Rows, Cols, Cells) :-
    findall(cell(R, C, '.'), (member(R, Rows), member(C, Cols)), Cells).

/* ----------------------------------------------------------------------
   2. SELEÇÃO DE ESPAÇO (COORDENADA) E COLOCAÇÃO DE SÍMBOLO
   ---------------------------------------------------------------------- */

/* 
   pick_space(+RowLabel, +ColLabel, +Symbol, +State, -NewState)
   Coloca o Symbol na posição (RowLabel, ColLabel) nos Tabuleiros #1 e #2.
*/

%% Definir símbolos válidos
valid_symbol('X').
valid_symbol('O').
valid_symbol('.').
% Adicione mais símbolos conforme necessário

pick_space(RowLabel, ColLabel, Symbol, game(Board1, Board2), game(NewBoard1, NewBoard2)) :-
    valid_symbol(Symbol),
    % Verifica se a célula é válida e está vazia no Tabuleiro #1
    (   valid_cell(Board1, RowLabel, ColLabel)
    ->  % Atualiza Tabuleiro #1
        update_board(Board1, RowLabel, ColLabel, Symbol, NewBoard1),
        % Atualiza Tabuleiro #2
        update_board(Board2, RowLabel, ColLabel, Symbol, NewBoard2)
    ;   % Se a célula não for válida ou já estiver ocupada
        fail
    ).


%% update_board(+Board, +Row, +Col, +Symbol, -NewBoard)
%% Atualiza a célula especificada com o novo símbolo.
update_board(board(ID, Rows, Cols, Cells), Row, Col, Symbol, board(ID, Rows, Cols, NewCells)) :-
    maplist(update_cell(Row, Col, Symbol), Cells, NewCells).

%% update_cell(+Row, +Col, +Symbol, +cell(R, C, S), -NewCell)
%% Substitui o símbolo se a linha e a coluna corresponderem.
update_cell(Row, Col, Symbol, cell(R, C, S), cell(R, C, NewS)) :-
    (   R = Row, C = Col
    ->  NewS = Symbol
    ;   NewS = S
    ).

/* ----------------------------------------------------------------------
   3. IMPRESSÃO DOS TABULEIROS
   ---------------------------------------------------------------------- */

print_boards(game(Board1, Board2),_) :-
    print_board(Board1,'1'),
    nl,
    print_board(Board2,'2').
/* 
   print_boards(+State)
   Imprime ambos os tabuleiros.
*/
print_boards(game(Board1, Board2)) :-
    print_board(Board1),
    nl,
    print_board(Board2).

print_board(board(_, Rows, Cols, Cells),Player) :-
    format('--- Player ~w ---~n', [Player]),
    print_col_header(Cols),
    print_rows(Rows, Cols, Cells).

%% print_board(+Board)
%% Imprime um único tabuleiro.
print_board(board(ID, Rows, Cols, Cells)) :-
    format('--- Board ~w ---~n', [ID]),
    print_col_header(Cols),
    print_rows(Rows, Cols, Cells).

%% print_col_header(+Cols)
%% Imprime o cabeçalho das colunas.
print_col_header(Cols) :-
    write('   '),
    print_cols(Cols),
    nl,
    write('  '),
    print_separator(Cols),
    nl.

%% print_cols(+Cols)
%% Imprime cada coluna com espaçamento.
print_cols([]).
print_cols([C|Cs]) :-
    format(' ~w ', [C]),
    print_cols(Cs).

%% print_separator(+Cols)
%% Imprime uma linha separadora após o cabeçalho das colunas.
print_separator([]).
print_separator([_|Cs]) :-
    write('---'),
    print_separator(Cs).

%% print_rows(+Rows, +Cols, +Cells)
%% Imprime cada linha do tabuleiro.
print_rows([], _, _).
print_rows([R|Rs], Cols, Cells) :-
    format('~w |', [R]),
    print_cells(R, Cols, Cells),
    nl,
    print_rows(Rs, Cols, Cells).

%% print_cells(+Row, +Cols, +Cells)
%% Imprime os símbolos das células em uma linha específica.
print_cells(_, [], _).
print_cells(Row, [C|Cs], Cells) :-
    (   member(cell(Row, C, Symbol), Cells)
    ->  format(' ~w ', [Symbol])
    ;   format(' ~w ', [' . '])
    ),
    print_cells(Row, Cs, Cells).
