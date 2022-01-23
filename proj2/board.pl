%generate_board(+GameState,+Size)
/*
Gera um GameState com tamanho determinado
*/
generate_board(GameState, Size):-
    build_board([], GameState, Size, 0, 1).

%build_board(+InitialBoard,-FinalBoard,+Size,+RowIndex,+Cell)
/*
Cria um quadro linha por linha com o tamanho fornecido (vai ser sempre 9)
*/
build_board(FinalBoard, FinalBoard, Size, Size, _).

%Se for a linha zero vai ser os valores do player 1
build_board(InitialBoard, FinalBoard, Size, RowIndex, _):-
    RowIndex =:= 0,
    build_row([], BuiltRow, Size, 0, 1),
    append(InitialBoard, BuiltRow, UpdatedBoard),
    NewRowIndex is RowIndex+1,
    build_board(UpdatedBoard, FinalBoard, Size, NewRowIndex, 0).

%Se for a linha 8 (a última) vai ser os valores do player 1
build_board(InitialBoard, FinalBoard, Size, RowIndex, _):-
    RowIndex =:= 8,
    build_row([], BuiltRow, Size, 0, -1),
    append(InitialBoard, BuiltRow, UpdatedBoard),
    NewRowIndex is RowIndex+1,
    build_board(UpdatedBoard, FinalBoard, Size, NewRowIndex, 0).


%Se não for nenhuma das outras linhas/posições, insere o valor zero
build_board(InitialBoard, FinalBoard, Size, RowIndex, _):-
    RowIndex =\= 8, RowIndex =\= 0, RowIndex =\= 4, RowIndex < 10,
    build_row([], BuiltRow, Size, 0, 0),
    append(InitialBoard, BuiltRow, UpdatedBoard),
    NewRowIndex is RowIndex+1,
    build_board(UpdatedBoard, FinalBoard, Size, NewRowIndex, 0).

%Se na posição do centro insere o valor associado
build_board(InitialBoard, FinalBoard, Size, RowIndex, _):-
    RowIndex =:= 4,
    build_middle_row([], BuiltRow, Size, 0, 0),
    append(InitialBoard, BuiltRow, UpdatedBoard),
    NewRowIndex is RowIndex+1,
    build_board(UpdatedBoard, FinalBoard, Size, NewRowIndex, 0).
/*
Cria a linha de um quadro com base no tamanho do quadro e na célula inicial da linha
*/
build_row(Row, BuiltRow, Size, Size, _):- BuiltRow=[Row].
build_row(Row, BuiltRow, Size, ColIndex, Cell):-
    append(Row, [Cell], UpdatedRow),
    NewColIndex is ColIndex+1,
    build_row(UpdatedRow, BuiltRow, Size, NewColIndex, Cell).

build_middle_row(Row, BuiltRow, Size, Size, _):- BuiltRow=[Row].
build_middle_row(Row, BuiltRow, Size, ColIndex, Cell):-
    ColIndex =:= 4,
    append(Row, [3], UpdatedRow),
    NewColIndex is ColIndex+1,
    build_middle_row(UpdatedRow, BuiltRow, Size, NewColIndex, Cell).

build_middle_row(Row, BuiltRow, Size, ColIndex, Cell):-
    ColIndex =\= 4,
    append(Row, [Cell], UpdatedRow),
    NewColIndex is ColIndex+1,
    build_middle_row(UpdatedRow, BuiltRow, Size, NewColIndex, Cell).

%character(+Character,-Representation)
character(0,' '). % 0 representa os espaços vazios
character(-1,'X'). %-1 represente o jogador X
character(1,'O'). %1 represente o jogador O
character(3,'?'). %3 representa o caracter ? que por sua vez representa o centro


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
    print_board_header(Size),
    printMatrix(Board, 0, Size),
    printBoardBottom(Size).

%print_board_header(+Size)
/*
Imprime o indicador de colunas,
uma linha de Xs no topo do tabuleiro representando 
o lado superior do jogador 2 e os separadores
*/
print_board_header(Size) :-
    write('       '),
    print_header_numbers(1, Size), /* Indicador de coluna */
    write('       '),
    print_separator(1, Size),
    write('       '),
    print_xLine(1, Size), /* Print Xs on the top side */
    print_board_row_separator(Size).

%print_header_numbers(+Current,+Size)
/*
Imprima os indicadores de colunas com os separados por | e espaços
*/
print_header_numbers(Current, Size) :- Current=:=Size+1, write('|\n').
print_header_numbers(Current, Size) :-
    write('| '), write(Current), write(' '), CurrentN is Current+1,
    print_header_numbers(CurrentN, Size).

%print_separator(+Current,+Size)
/*
Imprime o separador
*/
print_separator(Current, Size) :- Current=:=Size+1, write('+\n').
print_separator(Current, Size) :-
    write('+---'), CurrentN is Current+1,
    print_separator(CurrentN, Size).

%print_xLine(+Current,+Size)
/*
Imprime uma linha de Xs representando um lado do jogador azul
*/
print_xLine(Current, Size) :- Current=:=Size+1, write(' \n').
print_xLine(Current, Size) :-
    write('   '), CurrentN is Current+1,
    print_xLine(CurrentN, Size).

%print_board_row_separator(+Size)
/*
Imprime o separador
*/
print_board_row_separator(Size) :-
    write('---+   '),
    print_separator(1, Size).

%printBoardBottom(+Size)
/*
Imprime uma linha de X na parte inferior do tabuleiro representando a parte inferior do jogador azul
*/
printBoardBottom(Size) :-
    write('       '),
    print_xLine(1, Size). 

%printMatrix(+Board,+N,+Size)
/*
Imprime a matriz que representa o quadro com os indicadores de linha,
uma linha de O's no lado esquerdo do tabuleiro representando o lado esquerdo do jogador 1,
e os separadores
*/
printMatrix([], _, _).
printMatrix([Head|Tail],N, Size):-
    write(' '),
    get_letter(N, Row), % Row indicator
    write(Row),
    write(' |   | '),  % Print Os on the left side
    print_row(Head),
    nl,
    print_board_row_separator(Size),
    N1 is N + 1,
    printMatrix(Tail, N1, Size).

%print_row(+List)
/*
Imprime uma linha de O's no lado direito do tabuleiro representando o lado direito do jogador 2
*/
print_row([]):-
    write(' '). 

%print_row(+List)
/*
Imprime uma lista representando uma linha da matriz
*/
print_row([Head|Tail]) :-
    character(Head, S),
    write(S),
    write(' | '),
    print_row(Tail).