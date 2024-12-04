tabuleiro :: [[char]]
player1 :: char
player2 :: char

initTab(Tab) ->
    Tab = [['_', '_', '_', '_', '_', '_',],
        ['_', '_', '_', '_', '_', '_'],
        ['_', '_', '_', '_', '_', '_'],
        ['_', '_', '_', '_', '_', '_'],
        ['_', '_', '_', '_', '_', '_'],
        ['_', '_', '_', '_', '_', '_']]


printTab(Tab) ->
    io:format("~n", [])
    for (I in 1 to 9),
        do (for (J in 1 to 9),
            do (
                io:format("~s", Tab[I][J])
            )),
        io:format("~n", [])
    end