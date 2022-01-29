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
Se a verificação falha, manda uma mensagem de erro e pede ao utilizador para inserir um novo input.
*/
validateMenuOption(_, _, _) :-
    write('\nInvalid option! Try again:\n'), skip_line, fail.

%chooseMode/0
/*
Exibe o menu inicial lê a opção de entrada e verifica se é válida e age de acordo com a opção escolhida
*/

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

%rulesMode/0
/*
Mostra as regras do jogo ao utilizador, clicar em zero para voltar ao menu inicial
*/
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

%endMode/0
/*
Ecrã para a saida do jogo, espera 5 segundos e a seguir limpa.
*/
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
    write('\33\[2J').

%playMode/0
/*
Ecrã para a seleção de jogo, 3 opções de jogo + a possibildade de regressar ao menu inicial
*/
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

menuAction(2) :-
    firstMoveMode.

menuAction(3) :-
    write('\n\n\tHave fun!\n\n'),
    initialize(GameState, 9),
    play(GameState, 9, 1, 'Easy', 'Easy'),
    enterContinue,
    chooseMode.

menuAction(0):-
    chooseMode.

%firstMoveMode/0
/*
Ecrã para a seleção de quem começa o jogo, 2 (jogador ou bot) + a possibildade de regressar ao menu inicial
*/
firstMoveMode :-
    write('\33\[2J'),
    write('\n __________________________________________________________\n'),
    write('|        _                                                 |\n'),
    write('|       | | ___  ___  ___  _ __   _ __ ____  _____  ____   |\n'),
    write('|    _  | |/ _ || __|| _ ||  _  ||  _   _  ||  _  ||  __|  |\n'),
    write('|   | |_| |  __/|__ | (_) | | | || | | | | || (_) || |     |\n'),
    write('|   |____/|____||___/|___||_| |_||_| |_| |_||_____||_|     |\n'),
    write('|                                                          |\n'),
    write('|                                                          |\n'),
    write('|                     Who goes First?                      |\n'),
    write('|                                                          |\n'),
    write('|                    1. Human (O)                          |\n'),
    write('|                                                          |\n'),
    write('|                    2. Bot (X)                            |\n'),
    write('|                                                          |\n'),
    write('|                    0. Return Back                        |\n'),
    write('|                                                          |\n'),
    write('|__________________________________________________________|\n'),
    selectMenuOption(2, ValidOption),
    menuFirstMove(ValidOption).

menuFirstMove(0):-
    playMode.

menuFirstMove(1):-
    write('\n\n\tHave fun!\n\n'),
    initialize(GameState, 9),
    play(GameState, 9, 1, 'Player', 'Easy'),
    enterContinue,
    chooseMode.

menuFirstMove(2):-
    write('\n\n\tHave fun!\n\n'),
    initialize(GameState, 9),
    play(GameState, 9, 1, 'Easy', 'Player'),
    enterContinue,
    chooseMode.


enterContinue:-
	write('\nPress ENTER to continue.'),
    skip_line.
