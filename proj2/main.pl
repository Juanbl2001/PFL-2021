:-include('board.pl'). %File that generates the initial board
:-use_module(library(between)).
:-use_module(library(system)).
%Starts the game
play :- write('Welcome to the JesonBoard game! Lets have some fun'), nl , nl, chooseMode.

%mainMenu/0
/*
Displays initial menu reads the option input and checks if it is valid and acts accordingly to the option chosen
*/
%Chooses the mode of the game

selectMenuOption(NumOptions, ValidOption) :-
    write('\nInsert option:\n'),
    repeat,
    readMenuOption(OptionInput),
    validateMenuOption(OptionInput, ValidOption, NumOptions), !.

readMenuOption(Option) :-
    write(' -> '),
    get_code(Option),
    Option\=10.

validateMenuOption(OptionInput, ValidOption, NumOptions) :-
    peek_char('\n'),
    ValidOption is OptionInput - 48,
    between(0, NumOptions, ValidOption),
    skip_line.

%validateMenuOption(+OptionInput,-ValidOption,+NumOptions)
/*
If the verification above fails, then it outputs a error message and the user is asked for a new input
*/
validateMenuOption(_, _, _) :-
    write('\nInvalid option! Try again:\n'), skip_line, fail.

chooseMode :-
    repeat,
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
    write('|__________________________________________________________|\n'),
	selectMenuOption(2, ValidOption),
    mainMenuAction(ValidOption).

mainMenuAction(1) :-
    playMode.

mainMenuAction(2) :-
    rulesMode.

mainMenuAction(0) :-
    endMode.

rulesMode :-
    write('\33\[2J'),
    write('\n __________________________________________________________\n'),
    write('|     _                        ____                      _ |\n'),
    write('|    | | ___  ___  ___  _ __  | __ )  ___   __ _ _ __ __| ||\n'),
    write('| _  | |/ _ || __|| _ ||  _  ||  _ || _  || _  | |__/ _   ||\n'),
    write('|| |_| |  __/|__ | (_) | | | || |_) | (_)||(_| | | | (_|  ||\n'),
    write('||____/|____||___/|___||_| |_||____/|____||__,_|_| |__,___||\n'),
    write('|                                                          |\n'),
    write('|                        Rules                             |\n'),
    write('|                                                          |\n'),
    write('|  HORSES - Each horse moves like the Chess knight         |\n'),
    write('|  (i.e., one cell on an orthogonal direction then one     |\n'),
    write('|  cell diagonally forward on that direction; it may jump).|\n'),
    write('|                                                          |\n'),
    write('|  GOAL - Wins the player that first occupy the center cell|\n'),
    write('|  and then leaves it or captures all enemy horses.        |\n'),
    write('|                                                          |\n'),
    write('|                      0. Go Back                          |\n'),
    write('|__________________________________________________________|\n'),
    selectMenuOption(0, _),
    chooseMode.


endMode :-
    write('\33\[2J'),
    write('\n __________________________________________________________\n'),
    write('|     _                        ____                      _ |\n'),
    write('|    | | ___  ___  ___  _ __  | __ )  ___   __ _ _ __ __| ||\n'),
    write('| _  | |/ _ || __|| _ ||  _  ||  _ || _  || _  | |__/ _   ||\n'),
    write('|| |_| |  __/|__ | (_) | | | || |_) | (_)||(_| | | | (_|  ||\n'),
    write('||____/|____||___/|___||_| |_||____/|____||__,_|_| |__,___||\n'),
    write('|                                                          |\n'),
    write('|                                                          |\n'),
    write('|                                                          |\n'),
    write('|                                                          |\n'),
    write('|                  Thank you for playing!                  |\n'),
    write('|                                                          |\n'),
    write('|                        Filipe Pinto                      |\n'),
    write('|                        Juan Bellon                       |\n'),
    write('|                                                          |\n'),
    write('|                                                          |\n'),
    write('|                                                          |\n'),
    write('|__________________________________________________________|\n'),
    sleep(5),
    write('\33\[2J'),
    halt(10).

playMode :-
    write('\33\[2J'),
    write('\n __________________________________________________________\n'),
    write('|     _                        ____                      _ |\n'),
    write('|    | | ___  ___  ___  _ __  | __ )  ___   __ _ _ __ __| ||\n'),
    write('| _  | |/ _ || __|| _ ||  _  ||  _ || _  || _  | |__/ _   ||\n'),
    write('|| |_| |  __/|__ | (_) | | | || |_) | (_)||(_| | | | (_|  ||\n'),
    write('||____/|____||___/|___||_| |_||____/|____||__,_|_| |__,___||\n'),
    write('|                                                          |\n'),
    write('|                                                          |\n'),
    write('|                                                          |\n'),
    write('|                                                          |\n'),
    write('|                  1. Human vs. Human                      |\n'),
    write('|                                                          |\n'),
    write('|                  2. Human vs. Computador                 |\n'),
    write('|                                                          |\n'),
    write('|                  0. Return to Main Menu                  |\n'),
    write('|                                                          |\n'),
    write('|__________________________________________________________|\n'),
    selectMenuOption(2, ValidOption),
    menuAction(ValidOption).

menuAction(1) :-
    %boardSizeMenu(8),
    write('\n\n\tHave fun!\n\n'),
    initialize(GameState, Size),
    play(GameState, Size, 1, 'Player', 'Player'),
    enterContinue,
    mainMenu.
    
%menuAction(+ValidOption)
/*
Displays the board size menu, checks if the input is valid, 
displays the bot difficulty menu, checks again if the input is valid,
displays the choose first player menu, checks again if the input is valid
and acts accordingly to all the choices made by input
*/
menuAction(2) :-
    boardSizeMenu(Size), !,
    botDificultyMenu(Difficulty),
    chooseFirstPlayerMenu(First),
    firstAction(First, Size, Difficulty),
    enterContinue,
    mainMenu.

%startMode(1) :- !, initial(Board), nl, write('Human vs Human'), asserta(gameMode(1)), nl, write('And game begins!'), nl, nl, nl.