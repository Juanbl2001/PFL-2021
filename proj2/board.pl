% board([[1,1,1,1,1,1,1,1],
%       [0,0,0,0,0,0,0,0],
%       [0,0,0,0,0,0,0,0],
%       [0,0,0,0,0,0,0,0],
%       [0,0,0,0,3,0,0,0],  %initial state
%       [0,0,0,0,0,0,0,0],
%       [0,0,0,0,0,0,0,0],
%       [0,0,0,0,0,0,0,0],
%       [2,2,2,2,2,2,2,2]]).

/*1 representa as peças do jogador 1
  2 representa as peças do jogador 2
  3 representa o centro do tabuleiro*/

  /*[ [1,0,1,1,1,1,1,1],
      [0,0,0,0,0,0,0,0],
      [0,0,1,0,0,0,0,0],
      [0,0,0,0,0,0,0,0],
      [0,0,0,0,3,0,0,0],
      [0,0,0,0,0,0,0,0],
      [0,0,2,0,0,0,0,0],
      [0,0,0,0,0,0,0,0],
      [2,0,2,2,2,2,2,2]]*/

  /*[ [1,0,1,1,1,1,1,1],
      [0,0,0,0,0,0,0,0],
      [0,0,1,0,0,0,0,0],
      [0,0,0,0,0,0,0,0],
      [0,0,0,2,3,0,0,0],
      [0,0,0,0,0,0,0,0],
      [0,0,0,0,0,0,0,0],
      [0,0,0,0,0,0,0,0],
      [2,0,2,2,2,2,2,2]]*/

  /*[ [1,0,1,1,1,1,1,1],
      [0,0,0,0,0,0,0,0],
      [0,0,0,0,0,0,0,0],
      [0,0,0,0,0,0,0,0],
      [0,0,0,1,3,0,0,0],   %o jogador 1 capturou uma peça do jogador 2
      [0,0,0,0,0,0,0,0],   %intermediate state
      [0,0,0,0,0,0,0,0],
      [0,0,0,0,0,0,0,0],
      [2,0,2,2,2,2,2,2]]*/

%generateBoard(+GameState,+Size)
/*
Generates a  GameState with given Size
*/
generateBoard(GameState, Size):-
    buildBoard([], GameState, Size, 0, 1).

%buildBoard(+InitialBoard,-FinalBoard,+Size,+RowIndex,+Cell)
/*
Creates a board row by row with given Size
*/
buildBoard(FinalBoard, FinalBoard, Size, Size, _).
buildBoard(InitialBoard, FinalBoard, Size, RowIndex, Cell):-
    RowIndex =:= 0,
    buildRow([], BuiltRow, Size, 0, 1),
    append(InitialBoard, BuiltRow, UpdatedBoard),
    NewRowIndex is RowIndex+1,
    buildBoard(UpdatedBoard, FinalBoard, Size, NewRowIndex, 0).

buildBoard(InitialBoard, FinalBoard, Size, RowIndex, Cell):-
    RowIndex =:= 8,
    buildRow([], BuiltRow, Size, 0, -1),
    append(InitialBoard, BuiltRow, UpdatedBoard),
    NewRowIndex is RowIndex+1,
    buildBoard(UpdatedBoard, FinalBoard, Size, NewRowIndex, 0).

buildBoard(InitialBoard, FinalBoard, Size, RowIndex, Cell):-
    RowIndex =\= 8, RowIndex =\= 0, RowIndex =\= 4, RowIndex < 10,
    buildRow([], BuiltRow, Size, 0, 0),
    append(InitialBoard, BuiltRow, UpdatedBoard),
    NewRowIndex is RowIndex+1,
    buildBoard(UpdatedBoard, FinalBoard, Size, NewRowIndex, 0).

buildBoard(InitialBoard, FinalBoard, Size, RowIndex, Cell):-
    RowIndex =:= 4,
    buildMiddleRow([], BuiltRow, Size, 0, 0),
    append(InitialBoard, BuiltRow, UpdatedBoard),
    NewRowIndex is RowIndex+1,
    buildBoard(UpdatedBoard, FinalBoard, Size, NewRowIndex, 0).
/*
Creates a board's row based on board Size and row initial Cell
*/
buildRow(Row, BuiltRow, Size, Size, _):- BuiltRow=[Row].
buildRow(Row, BuiltRow, Size, ColIndex, Cell):-
    append(Row, [Cell], UpdatedRow),
    NewColIndex is ColIndex+1,
    buildRow(UpdatedRow, BuiltRow, Size, NewColIndex, Cell).

buildMiddleRow(Row, BuiltRow, Size, Size, _):- BuiltRow=[Row].
buildMiddleRow(Row, BuiltRow, Size, ColIndex, Cell):-
    ColIndex =:= 4,
    append(Row, [3], UpdatedRow),
    NewColIndex is ColIndex+1,
    buildMiddleRow(UpdatedRow, BuiltRow, Size, NewColIndex, Cell).

buildMiddleRow(Row, BuiltRow, Size, ColIndex, Cell):-
    ColIndex =\= 4,
    append(Row, [Cell], UpdatedRow),
    NewColIndex is ColIndex+1,
    buildMiddleRow(UpdatedRow, BuiltRow, Size, NewColIndex, Cell).

%character(+Character,-Representation)
character(0,' '). %character for an empty space
character(-1,'X'). %character representing the player1 piece
character(1,'O'). %character representing the player2 piece
character(3,'?'). %auxiliar character for middle


%get_letter(+Row, -Letter)
/*
Gets Letter corresponding to the given Row index
*/
get_letter(Row, Letter) :-
	NewRow is Row + 65,
	char_code(Letter, NewRow).

%display_game(+Board)
/*
Prints the board header, matrix and bottom
*/
display_game(Board) :-
    length(Board, Size),
    nl,
    printBoardHeader(Size),
    printMatrix(Board, 0, Size),
    printBoardBottom(Size).

%printBoardHeader(+Size)
/*
Prints the columns indicator,
a line of X's on the top of the board representing the top side of the blue player,
and the separators
*/
printBoardHeader(Size) :-
    write('       '),
    printHeaderNumbers(1, Size), /* Columns indicator */
    write('       '),
    printSeparator(1, Size),
    write('       '),
    printXLine(1, Size), /* Print Xs on the top side */
    printBoardRowSeparator(Size).

%printHeaderNumbers(+Current,+Size)
/*
Print the columns indicators with the separated by | and spaces
*/
printHeaderNumbers(Current, Size) :- Current=:=Size+1, write('|\n').
printHeaderNumbers(Current, Size) :-
    write('| '), write(Current), write(' '), CurrentN is Current+1,
    printHeaderNumbers(CurrentN, Size).

%printSeparator(+Current,+Size)
/*
Prints the separator
*/
printSeparator(Current, Size) :- Current=:=Size+1, write('+\n').
printSeparator(Current, Size) :-
    write('+---'), CurrentN is Current+1,
    printSeparator(CurrentN, Size).

%printXLine(+Current,+Size)
/*
Prints a line of X's  representing one side of the blue player
*/
printXLine(Current, Size) :- Current=:=Size+1, write(' \n').
printXLine(Current, Size) :-
    write('   '), CurrentN is Current+1,
    printXLine(CurrentN, Size).

%printBoardRowSeparator(+Size)
/*
Prints the separators
*/
printBoardRowSeparator(Size) :-
    write('---+   '),
    printSeparator(1, Size).

%printBoardBottom(+Size)
/*
Prints a line of X's on the bottom of the board representing the bottom side of the blue player 
*/
printBoardBottom(Size) :-
    write('       '),
    printXLine(1, Size). 

%printMatrix(+Board,+N,+Size)
/*
Prints the matrix representing the board with the row indicators,
a line of O's on the left side of the board representing the left side of the red player,
and the separators
*/
printMatrix([], _, _).
printMatrix([Head|Tail], N, Size):-
    write(' '),
    get_letter(N, Row), % Row indicator
    write(Row),
    write(' |   | '),  % Print Os on the left side
    printRow(Head),
    nl,
    printBoardRowSeparator(Size),
    N1 is N + 1,
    printMatrix(Tail, N1, Size).

%printRow(+List)
/*
Prints a line of O's on the right side of the board representing the right side of the red player
*/
printRow([]):-
    write(' '). 

%printRow(+List)
/*
Prints a list representing a matrix row
*/
printRow([Head|Tail]) :-
    character(Head, S),
    write(S),
    write(' | '),
    printRow(Tail).