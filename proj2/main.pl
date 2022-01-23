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

%chooseMode/0
/*
Displays initial menu reads the option input and checks if it is valid and acts accordingly to the option chosen
*/
%Chooses the mode of the game
chooseMode :-
    repeat,
    write('\33\[2J'),
    write('\n __________________________________________________________\n'),
    write('|        _                                                 |\n'),
    write('|       | | ___  ___  ___  _ __   _ __ ____  _____  ____   |\n'),
    write('|    _  | |/ _ || __|| _ ||  _  ||  _   _  ||  _  ||  __|  |\n'),
    write('|   | |_| |  __/|__ | (_) | | | || | | | | || (_) || |     |\n'),
    write('|   |____/|____||___/|___||_| |_||_| |_| |_||_____||_|     |\n'),
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
    write('|        _                                                 |\n'),
    write('|       | | ___  ___  ___  _ __   _ __ ____  _____  ____   |\n'),
    write('|    _  | |/ _ || __|| _ ||  _  ||  _   _  ||  _  ||  __|  |\n'),
    write('|   | |_| |  __/|__ | (_) | | | || | | | | || (_) || |     |\n'),
    write('|   |____/|____||___/|___||_| |_||_| |_| |_||_____||_|     |\n'),
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
    write('|        _                                                 |\n'),
    write('|       | | ___  ___  ___  _ __   _ __ ____  _____  ____   |\n'),
    write('|    _  | |/ _ || __|| _ ||  _  ||  _   _  ||  _  ||  __|  |\n'),
    write('|   | |_| |  __/|__ | (_) | | | || | | | | || (_) || |     |\n'),
    write('|   |____/|____||___/|___||_| |_||_| |_| |_||_____||_|     |\n'),
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
    write('|        _                                                 |\n'),
    write('|       | | ___  ___  ___  _ __   _ __ ____  _____  ____   |\n'),
    write('|    _  | |/ _ || __|| _ ||  _  ||  _   _  ||  _  ||  __|  |\n'),
    write('|   | |_| |  __/|__ | (_) | | | || | | | | || (_) || |     |\n'),
    write('|   |____/|____||___/|___||_| |_||_| |_| |_||_____||_|     |\n'),
    write('|                                                          |\n'),
    write('|                                                          |\n'),
    write('|                                                          |\n'),
    write('|                                                          |\n'),
    write('|                  1. Human vs. Human                      |\n'),
    write('|                                                          |\n'),
    write('|                  2. Human vs. Bot                        |\n'),
    write('|                                                          |\n'),
    write('|                  3. Bot vs. Bot                          |\n'),
    write('|                                                          |\n'),
    write('|                  0. Return to Main Menu                  |\n'),
    write('|                                                          |\n'),
    write('|__________________________________________________________|\n'),
    selectMenuOption(3, ValidOption),
    menuAction(ValidOption).

menuAction(1) :-
    write('\n\n\tHave fun!\n\n'),
    initialize(GameState, 9),
    play(GameState, 9, 1, 'Player', 'Player'),
    enterContinue,
    chooseMode.
    
%menuAction(+ValidOption)
/*
Displays the board size menu, checks if the input is valid, 
displays the bot difficulty menu, checks again if the input is valid,
displays the choose first player menu, checks again if the input is valid
and acts accordingly to all the choices made by input
*/
menuAction(2) :-
    write('\n\n\tHave fun!\n\n'),
    initialize(GameState, 9),
    play(GameState, 9, 1, 'Player', 'Easy'),
    enterContinue,
    chooseMode.

menuAction(3) :-
    write('\n\n\tHave fun!\n\n'),
    initialize(GameState, 9),
    play(GameState, 9, 1, 'Easy', 'Easy'),
    enterContinue,
    chooseMode.
menuAction(0):-
    chooseMode.

enterContinue:-
	write('\nPress ENTER to continue.'),
    skip_line.
