main_menu :-
    write('=== MENU DO JOGO ==='), nl,
    write('1. Iniciar jogo'), nl,
    write('2. Ver instruções'), nl,
    write('3. Sair'), nl,
    write('Escolha uma opção: '),
    read(Option),
    handle_option(Option).

handle_option(1) :-
    write('Jogo iniciado!'), nl,
    start_game.
handle_option(2) :-
    write('=== INSTRUÇÕES ==='), nl,
    write('1. Faça isso.'), nl,
    write('2. Faça aquilo.'), nl,
    main_menu.
handle_option(3) :-
    write('Saindo do jogo...'), nl.
handle_option(_) :-
    write('Opção inválida. Tente novamente.'), nl,
    main_menu.

start_game :-
    write('Bem-vindo ao jogo!'), nl.
