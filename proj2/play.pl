:- include('input.pl').
:- include('main.pl').
:- include('move.pl').
:- include('bot.pl').

%initial_state(-GameState, +Size)
/*
Returns the initial GameState with the given Size
*/
initial_state(GameState, Size) :- generateBoard(GameState, Size).


%initialize(-GameState, +Size)
/*
Returns created GameState and displays it
*/
initialize(GameState, Size):-
    initial_state(GameState, Size),
    display_game(GameState).

%play(+GameState, +Size, +Player, +PlayerType, +EnemyType)
/*
Verifies if the game is over (a player created a path)
and prints the Winner
*/
% play(GameState, Size, Player, _, _):-
%     game_over(GameState, Size, Player, Winner), !, printWinner(Winner).

/*
If game is not over,
print the current player turn, 
if the current player is a bot, the program sleeps for 1 second,
player chooses a move according to its type, makes the move and prints the board afterwards,
and call the play predicate to make the enemy play
*/
play(GameState, Size, Player, PlayerType, EnemyType):-
    %printTurn(Player),
    select_move(GameState, Size, Player, PlayerType, Move),
    move(GameState, Player, Move, NewGameState),
    display_game(NewGameState),
    Enemy is -Player,
    write(Enemy), nl,
    write(EnemyType), nl,
    !, play(NewGameState, Size, Enemy, EnemyType, PlayerType).


first_play(1, Size, Difficulty) :- initialize(GameState, Size),
                                   play(GameState, Size, 1, 'Player', 'Easy').