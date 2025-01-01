/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */


:- use_module(library(lists)).
:- use_module(singleplayer_grid).
:- use_module(singleplayer_normal_difficulty).

play_multiplayer :-
    init_boards(Game), 
    write('Welcome to Multiplayer Mode!'), nl,
    nl,
    game_loop_turn([player(1, 1, 0),player(2, 2, 0)],Game).

/*
    game_loop_turn(+Players,+Game)
    Processa um turno de jogo para o jogador atual.
*/
game_loop_turn([player(Name, BoardID, Score), player(OtherName, OtherBoardID, OtherScore)], Game) :-
    print_boards(Game, true),
    format('Turno de ~w.~n', [Name]),
    write('Escolha duas células vazias para colocar O e X.'), nl,
    prompt_two_moves(Row1, Col1, Row2, Col2),
    (
        Row1 = Row2, Col1 = Col2
    ->
        write('Não é possível escolher a mesma célula para ambos os símbolos. Tente novamente.'), nl,
        game_loop_turn([player(Name, BoardID, Score), player(OtherName, OtherBoardID, OtherScore)], Game)
    ;
        (
            % Tentativa de inserir os símbolos O e X
            pick_space(Row1, Col1, 'O', Game, GameAfterO),
            pick_space(Row2, Col2, 'X', GameAfterO, GameAfterX)
        ->
            % Se ambas as inserções forem bem-sucedidas, atualiza a pontuação
            update_player_score(GameAfterX, player(Name, BoardID, Score), UpdatedPlayer),
            
            (
                % Verifica se o jogo terminou
                multiplayer_game_over(GameAfterX,[UpdatedPlayer, player(OtherName, OtherBoardID, OtherScore)], Winner)
            ->
                % Extrai o nome do vencedor para exibição
                Winner = player(WinnerName, _, WinnerScore),
                format('Game ended! Player ~w won.~n', [WinnerName]),
                format('With a score of ~w.~n',[WinnerScore])
            ;
                % Alterna a ordem dos jogadores para o próximo turno
                game_loop_turn([player(OtherName, OtherBoardID, OtherScore), UpdatedPlayer], GameAfterX)
            )
        ;
            % Se qualquer inserção falhar, informa e reinicia o turno para o mesmo jogador
            write('Invalid move. Try again.'), nl,
            game_loop_turn([player(Name, BoardID, Score), player(OtherName, OtherBoardID, OtherScore)], Game)
    )).


/*
    prompt_two_moves(-Row1, -Col1, -Row2, -Col2)
    Solicita ao jogador que escolha duas células vazias.
*/
prompt_two_moves(Row1, Col1, Row2, Col2) :-
    write('First move:'),
    prompt_move(Row1, Col1),
    write('Second move:'),
    prompt_move(Row2, Col2).

prompt_move(Row, Col) :-
    prompt_row(Row),
    prompt_col(Col).

prompt_row(Row) :-
    write('Choose the row (a-f): '),
    read(Row).
    

prompt_col(Col) :-
    write('Choose the collumn (1-6): '),
    read(Col).

/*
    demo/0
    Executa uma sequência de jogadas de exemplo e exibe a pontuação.
*/
demo :-
    init_boards(Game),
    write('Starting Demo...'), nl,
    print_boards(Game, true),
    

    pick_space(a, 1, 'X', Game, Game1),
    pick_space(a, 2, 'X', Game1, Game2),
    pick_space(a, 3, 'X', Game2, Game3),
    pick_space(a, 4, 'X', Game3, Game4),
    pick_space(a, 5, 'X', Game4, Game5),
    pick_space(a, 6, 'X', Game5, Game6),
    pick_space(b, 6, 'X', Game6, Game7),
    pick_space(b, 5, 'X', Game7, Game8),
    pick_space(b, 4, 'X', Game8, Game9),
    pick_space(b, 3, 'X', Game9, Game10),
    pick_space(b, 2, 'X', Game10, Game11),
    pick_space(b, 1, 'X', Game11, Game12),

    print_boards(Game12, true),
    

    update_player_score(Game12, player(1,1, 0), UpdatedPlayer1),
    UpdatedPlayer1 = player(1, 1, Score1),
    format('Pontuação de Player1: ~w~n~n', [Score1]).
