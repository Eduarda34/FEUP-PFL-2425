main_menu :-
    write('=== MENU ==='), nl,
    write('1. Iniciar jogo'), nl,
    write('2. Ver instruções'), nl,
    write('3. Sair'), nl,
    write('Escolhe uma opção: '),
    read(Option),
    handle_option(Option).

handle_option(1) :-
    write('a. 1 jogador'), nl,
    write('b. 2 jogadores'), nl,
    write('c. 3 jogadores'), nl,
    read(anotherOp),
    handle_op(anotherOp).

handle_op(a) :-
    write('A entrar no jogo - 1 jogador'), nl,
    write('Escolha a dificuldade do jogo:'), nl,
    write('1. Fácil'), nl,
    write('2. Médio'), nl,
    write('3. Difícil'), nl,
    read(dificuldade),
    handle_dif(dificuldade).

handle_dif(1) :-
    write('Escolheste a dificuldade "Fácil"!'), nl,
    start_game.

handle_dif(2) :-
    write('Escolheste a dificuldade "Médio"!'), nl,
    start_game.

handle_dif(3) :-
    write('Escolheste a dificuldade "Difícil"!'), nl,
    start_game.

handle_op(b) :-
    write('A entrar no jogo - 2 jogadores'), nl,
    start_game.

handle_op(c) :-
    write('A entrar no jogo - 3 jogadores'), nl,
    start_game.    

%escrever as instruções
handle_option(2) :-
    write('=== INSTRUÇÕES ==='), nl,
    write('1. X'), nl,
    write('2. Y'), nl,
    main_menu.

handle_option(3) :-
    write('A sair do jogo...'), nl.

handle_option(_) :-
    write('Opção inválida. Tenta novamente.'), nl,
    main_menu.

start_game :-
    write('Bem-vindo ao Doblin!'), nl.
    write('Boa sorte!'), nl.
    %chamar diferentes tabuleiros para os diferentes números de jogadores