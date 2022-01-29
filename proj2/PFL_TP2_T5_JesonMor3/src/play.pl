%initial_state(-GameState, +Size)
/*
Retorna o GameState inicial com o tamanho fornecido
*/
initial_state(GameState, Size) :- generate_board(GameState, Size).


%initialize(-GameState, +Size)
/*
Retorna o GameState criado e o exibe
*/
initialize(GameState, Size):-
    initial_state(GameState, Size),
    display_game(GameState).

/*
Se o jogo não acabou, imprima o turno do jogador atual, se o jogador atual for um bot, o programa dorme por 1 segundo, 
o jogador escolhe um movimento de acordo com seu tipo, faz o movimento e imprime o tabuleiro depois, 
e chama o predicado de jogo para fazer o inimigo jogar
*/

play(GameState, Size, Player, _):-
    game_over(GameState, Size, Player ,_).


play(GameState, Size, Player, PlayerType, EnemyType):-
    choose_move(GameState, Size, Player, PlayerType, Move),
    move(GameState, Player, Move, NewGameState),
    display_game(NewGameState),
    game_over(NewGameState, Size, Player, PlayerType, EnemyType, Move).

game_over(GameState, Size, Player) :-
    checkWinner(GameState, Size, Player, Winner), !, printWinner(Winner).

game_over(GameState, Size, Player, _, _, Move) :-
    checkWinner(GameState, Size, Player, Move, Winner), !, printWinner(Winner).


game_over(GameState, Size, Player, PlayerType, EnemyType, _) :- 
    Enemy is -Player,
    !, play(GameState,  Size, Enemy, EnemyType, PlayerType).


checkWinner(GameState, Size, Player, Winner) :-
    getPlayerPieces(GameState, Size, Player, ListOfPositions),
    isEmpty(ListOfPositions),
    Winner is -Player.


checkWinner(_, _, Player, Move, Winner):-
    getSelAndMovePosition(Move, SelRow-SelColumn, _),
    SelRow is 4,
    SelColumn is 4,
    Winner is Player.
