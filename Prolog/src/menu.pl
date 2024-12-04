main_menu :-
    write('=== MENU ==='), nl,
    write('1. Iniciar jogo'), nl,
    write('2. Ver instruções'), nl,
    write('3. Sair'), nl,
    write('Escolhe uma opção: '),
    read(Option),
    handle_option(Option).

handle_option(1) :-
    write('A entrar no jogo!'), nl,
    start_game.
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
