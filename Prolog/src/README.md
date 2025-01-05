# PFL - Project 2

Group: T16_G07

Game: Doblin


## Group Members:
Luís Filipe Moura Oliveira da Silveira Contreiras - up202108742 --> 33.3%

Maria Eduarda Pacheco Mendes Araújo - up202004473 --> 33.3%

Miguel Ângelo Pacheco Valente - up201704608 --> 33.3%


## Installation and Execution:
To run the game in Linux you need to consult the game.pl file (consult('path/to/game.pl').) and then run "play." in the console


## Description of the game:
In the singleplayer version of this game, the player must fill in two grids with Xs and Os without making a line or square of four of the same symbol. But the two grids are connected: by putting a symbol in one grid, it is also placed in the same coordinates in the other grid.

Using almost the same mechanism, the multiplayer version is longer, has atmost 3 boards and has an 8x8 board. The game ends when all boards are full and the objetive is to have less lines and squares in their grids than their opponents.


## Considerations for game extensions:
The size of the board varies depending on the number of players the user wants. For single-player games we have a 6x6 board, on the other hand, if needed we could easily modify the game implementation to work with a 4x4 or 5x5 board if a 6x6 board is too difficult. In the case of multiplayer, we have as many boards as the number of players are given, with all of these being 8x8.


## Game Logic:
### Game Configuration Representation:
The game configuration specifies the initial setup, including board size, player types, and optional rules.
It is represented as config(BoardSize, PlayerTypes, Rules), where:
BoardSize indicates the number of boards (e.g., 2 for standard play).
PlayerTypes specifies the type of each player (human, computer).
Rules contains any additional game parameters.
This configuration is used by initial_state/2 to initialize the game state. For example, the board size determines the number of boards created using the init_boards/2 predicate.


### Internal Game State Representation:
The game state tracks the current state of the game, including board contents, active players, and turn information.
It is represented as game(Board1, Board2, ...), where each board is defined as board(BoardID, Rows, Cols, Cells):
BoardID is the identifier (e.g., 1 or 2).
Rows and Cols are lists of row and column labels.
Cells is a list of cell(Row, Col, Symbol) terms representing each cell's state.
Examples:
Initial State: All cells are initialized with . to indicate they are empty.
Intermediate State: Cells contain X or O after moves are made.
Final State: Boards are full or a win condition is met.


### Move Representation:
A move describes a player's action, specifying the location and symbol to be placed.
It is represented as a tuple (Row, Col, Symbol), where:
Row and Col specify the cell's position.
Symbol is the piece being placed (X or O).
The move/3 predicate validates the move using valid_move/3 and applies it using pick_space/5.


### User Interaction:
The menu system, implemented in main_menu.pl, allows players to choose game modes (Singleplayer, Multiplayer, etc.).
User input is read, and the corresponding game mode is invoked.
Input prompts for moves are handled by input_helpers.pl, ensuring rows and columns are within valid ranges.
For example, prompt_row/1 and prompt_col/1 validate the chosen row and column.
Error handling ensures invalid moves are rejected with clear messages, allowing players to retry.


## Conclusions:
During this project, we were able to apply the knowledge we had acquired in class, from the menu to the dynamics of the game.

The game allows us to play single player, multiplayer for 2 or 3 people. We also implemented the option of player vs computer and computer vs computer. Possible improvements we could make would be dinamically add boards according to the number of players and make further AI improvements.

## Bibliography:
Rules: https://boardgamegeek.com/boardgame/308153/doblin
Minimax Information: https://www.geeksforgeeks.org/minimax-algorithm-in-game-theory-set-1-introduction/