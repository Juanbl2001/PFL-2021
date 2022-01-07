:- consult('JesonBoard.pl'). %File that generates the initial board



%Starts the game
play :- write('Welcome to the Swack game! Lets have some fun'), nl , nl, chooseMode.

%Chooses the mode of the game
chooseMode :-
	    write('Select the mode which you want to play:'), nl,
		write('Write 1 for Human vs Human'), nl,
		write('Write 2 for Human vs Computer'), nl,
		write('Write 3 for Computer vs Human'), nl,
		write('Write 4 for Computer vs Computer'), nl,
		read(Mode), nl, nl, startMode(Mode).

startMode(1) :- !, initial(Board), nl, write('Human vs Human'), asserta(gameMode(1)), nl, write('And game begins!'), nl, nl, nl.
