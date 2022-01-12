/*:- consult('JesonBoard.pl'). %File that generates the initial board



%Starts the game
play :- write('Welcome to the Swack game! Lets have some fun'), nl , nl, chooseMode.
*/
%mainMenu/0
/*
Displays initial menu reads the option input and checks if it is valid and acts accordingly to the option chosen
*/
%Chooses the mode of the game
chooseMode :-
    write('\33\[2J'),
    write('\n __________________________________________________________\n'),
    write('|     _                        ____                      _ |\n'),
    write('|    | | ___  ___  ___  _ __  | __ )  ___   __ _ _ __ __| ||\n'),
    write('| _  | |/ _ || __|| _ ||  _  ||  _ || _  || _  | |__/ _   ||\n'),
    write('|| |_| |  __/|__ | (_) | | | || |_) | (_)||(_| | | | (_|  ||\n'),
    write('||____/|____||___/|___||_| |_||____/|____||__,_|_| |__,___||\n'),
    write('|                                                          |\n'),
    write('|                      !WELCOME!                           |\n'),
    write('|                                                          |\n'),
    write('|                       1. Play                            |\n'),
    write('|                                                          |\n'),
    write('|                       2. Rules                           |\n'),
    write('|                                                          |\n'),
    write('|                       0. Exit                            |\n'),
    write('|                                                          |\n'),
    write('|__________________________________________________________|\n').
	%selectMenuOption(3, ValidOption),
    %mainMenuAction(ValidOption).

%startMode(1) :- !, initial(Board), nl, write('Human vs Human'), asserta(gameMode(1)), nl, write('And game begins!'), nl, nl, nl.