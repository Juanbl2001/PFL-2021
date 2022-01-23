%generateBoard(+GameState,+Size)
/*
Gera um GameState com tamanho determinado
*/
generateBoard(GameState, Size):-
    buildBoard([], GameState, Size, 0, 1).

%buildBoard(+InitialBoard,-FinalBoard,+Size,+RowIndex,+Cell)
/*
Cria um quadro linha por linha com o tamanho fornecido (vai ser sempre 9)
*/
buildBoard(FinalBoard, FinalBoard, Size, Size, _).

%Se for a linha zero vai ser os valores do player 1
buildBoard(InitialBoard, FinalBoard, Size, RowIndex, _):-
    RowIndex =:= 0,
    buildRow([], BuiltRow, Size, 0, 1),
    append(InitialBoard, BuiltRow, UpdatedBoard),
    NewRowIndex is RowIndex+1,
    buildBoard(UpdatedBoard, FinalBoard, Size, NewRowIndex, 0).

%Se for a linha 8 (a última) vai ser os valores do player 1
buildBoard(InitialBoard, FinalBoard, Size, RowIndex, _):-
    RowIndex =:= 8,
    buildRow([], BuiltRow, Size, 0, -1),
    append(InitialBoard, BuiltRow, UpdatedBoard),
    NewRowIndex is RowIndex+1,
    buildBoard(UpdatedBoard, FinalBoard, Size, NewRowIndex, 0).


%Se não for nenhuma das outras linhas/posições, insere o valor zero
buildBoard(InitialBoard, FinalBoard, Size, RowIndex, _):-
    RowIndex =\= 8, RowIndex =\= 0, RowIndex =\= 4, RowIndex < 10,
    buildRow([], BuiltRow, Size, 0, 0),
    append(InitialBoard, BuiltRow, UpdatedBoard),
    NewRowIndex is RowIndex+1,
    buildBoard(UpdatedBoard, FinalBoard, Size, NewRowIndex, 0).~

%Se na posição do centro insere o valor associado
buildBoard(InitialBoard, FinalBoard, Size, RowIndex, _):-
    RowIndex =:= 4,
    buildMiddleRow([], BuiltRow, Size, 0, 0),
    append(InitialBoard, BuiltRow, UpdatedBoard),
    NewRowIndex is RowIndex+1,
    buildBoard(UpdatedBoard, FinalBoard, Size, NewRowIndex, 0).
/*
Cria a linha de um quadro com base no tamanho do quadro e na célula inicial da linha
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
Obtém a letra correspondente ao índice de linha fornecido
*/
get_letter(Row, Letter) :-
	NewRow is Row + 65,
	char_code(Letter, NewRow).

%display_game(+Board)
/*
Imprime o cabeçalho, a matriz e o fundo do quadro
*/
display_game(Board) :-
    length(Board, Size),
    nl,
    printBoardHeader(Size),
    printMatrix(Board, 0, Size),
    printBoardBottom(Size).

%printBoardHeader(+Size)
/*
Imprime o indicador de colunas,
uma linha de Xs no topo do tabuleiro representando 
o lado superior do jogador 2 e os separadores
*/
printBoardHeader(Size) :-
    write('       '),
    printHeaderNumbers(1, Size), /* Indicador de coluna */
    write('       '),
    printSeparator(1, Size),
    write('       '),
    printXLine(1, Size), /* Print Xs on the top side */
    printBoardRowSeparator(Size).

%printHeaderNumbers(+Current,+Size)
/*
Imprima os indicadores de colunas com os separados por | e espaços
*/
printHeaderNumbers(Current, Size) :- Current=:=Size+1, write('|\n').
printHeaderNumbers(Current, Size) :-
    write('| '), write(Current), write(' '), CurrentN is Current+1,
    printHeaderNumbers(CurrentN, Size).

%printSeparator(+Current,+Size)
/*
Imprime o separador
*/
printSeparator(Current, Size) :- Current=:=Size+1, write('+\n').
printSeparator(Current, Size) :-
    write('+---'), CurrentN is Current+1,
    printSeparator(CurrentN, Size).

%printXLine(+Current,+Size)
/*
Imprime uma linha de Xs representando um lado do jogador azul
*/
printXLine(Current, Size) :- Current=:=Size+1, write(' \n').
printXLine(Current, Size) :-
    write('   '), CurrentN is Current+1,
    printXLine(CurrentN, Size).

%printBoardRowSeparator(+Size)
/*
Imprime o separador
*/
printBoardRowSeparator(Size) :-
    write('---+   '),
    printSeparator(1, Size).

%printBoardBottom(+Size)
/*
Imprime uma linha de X na parte inferior do tabuleiro representando a parte inferior do jogador azul
*/
printBoardBottom(Size) :-
    write('       '),
    printXLine(1, Size). 

%printMatrix(+Board,+N,+Size)
/*
Imprime a matriz que representa o quadro com os indicadores de linha,
uma linha de O's no lado esquerdo do tabuleiro representando o lado esquerdo do jogador 1,
e os separadores
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
Imprime uma linha de O's no lado direito do tabuleiro representando o lado direito do jogador 2
*/
printRow([]):-
    write(' '). 

%printRow(+List)
/*
Imprime uma lista representando uma linha da matriz
*/
printRow([Head|Tail]) :-
    character(Head, S),
    write(S),
    write(' | '),
    printRow(Tail).