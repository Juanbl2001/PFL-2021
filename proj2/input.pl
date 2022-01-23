%readRow(+Row)
/*
Lê o código de entrada da linha, ignorando novas linhas (código ASCII 10)
*/
readRow(Row) :-
    write('  -> Row    '),
    get_code(Row),
    Row\=10.

%readColumn(+Column)
/*
Lê o código de entrada da coluna, ignorando novas linhas (código ASCII 10)
*/
readColumn(Column) :-
    write('  -> Column '),
    get_code(Column),
    Column\=10.

%validateRow(+RowInput,-NewRow,+Size)
/*
Verifica se a entrada da linha é válida calculando seu índice, convertendo código ascii em número,
sendo a primeira linha A com índice 0 o código ascii para A é 65; código ascii para a é 
97 o índice tem que estar dentro dos limites da placa o próximo caractere tem que ser uma 
nova linha (senão 2 caracteres na entrada, falhando assim)
*/
validateRow(RowInput, NewRow, Size) :-
    peek_char('\n'),
    (
        (   %upper case letter
            RowInput < 97,
            NewRow is RowInput - 65
        );
        (   %lower case letter
            RowInput >= 97,
            NewRow is RowInput - 97
        )
    ),
    Valid is Size-1,
    between(0, Valid, NewRow),
    skip_line.

%validateRow(+RowInput,-NewRow,+Size)
/*
Se a verificação acima falhar, ela exibirá uma mensagem de erro e o usuário será solicitado a inserir uma nova entrada
*/
validateRow(_, _, _) :-
    write('\n! That row is not valid. Choose again !\n\n'), skip_line, fail.

%validateColumn(+ColumnInput,-NewColumn,+Size)
/*
Verifica se a entrada da coluna é válida calculando o seu índice, convertendo o código ascii em número, 
sendo a primeira coluna 1 índice 0 o índice tem que estar dentro dos limites da placa o próximo caractere 
tem que ser uma nova linha (senão 2 caracteres na entrada , falhando assim)
*/
validateColumn(ColumnInput, NewColumn, Size) :-
    peek_char('\n'),
    NewColumn is ColumnInput - 49,
    Valid is Size-1,
    between(0, Valid, NewColumn),
    skip_line.

%validateColumn(+ColumnInput,-NewColumn,+Size)
/*
Se a verificação acima falhar, ela exibirá uma mensagem de erro e o usuário será solicitado a inserir uma nova entrada
*/
validateColumn(_, _, _) :-
    write('\n! That column is not valid. Choose again !\n\n'), skip_line, fail.

%manageRow(-NewRow, +Size)
/*
Lê a linha de entrada e verifica se está entre os limites da placa
*/
manageRow(NewRow, Size) :-
    repeat,
    readRow(Row),
    validateRow(Row, NewRow, Size).

%manageColumn(-NewColumn,+Size)
/*
Lê a coluna de entrada e verifica se está entre os limites da placa
*/
manageColumn(NewColumn, Size) :-
    repeat,
    readColumn(Column),
    validateColumn(Column, NewColumn, Size).

%manageInputs(-NewRow,-NewColumn,+Size)
/*
Lê e verifica as entradas de linha e coluna
*/
manageInputs(NewRow, NewColumn, Size) :-
    manageRow(NewRow, Size),
    manageColumn(NewColumn, Size), !.

%selectPiece(+Board,+Size,+Player,-SelectedPosition)
/*
O jogador seleciona a peça que deseja mover as entradas são verificadas
se estão dentro dos limites do tabuleiro, se o jogador está selecionando
sua própria peça e se há algum movimento possível para essa peça
*/
selectPiece(Board, Size, Player, SelectedRow-SelectedColumn):-
    repeat,
    write('\nSelect piece:\n'),
    manageInputs(SelectedRow, SelectedColumn, Size),
    validateContent(Board, Size, SelectedRow-SelectedColumn, Player).

%movePiece(+Board,+Size,+Player,+SelectedPosition,-MovePosition)
/*
O jogador seleciona a posição para a peça que deseja mover as entradas 
são verificadas se estão dentro dos limites do tabuleiro e se o movimento é válido
*/
movePiece(Board, Size, Player, SelectedRow-SelectedColumn, MoveRow-MoveColumn):-
    repeat,
    write('\nMove to:\n'),
    manageInputs(MoveRow, MoveColumn, Size),
    tryMove(Board, Size, Player, SelectedRow-SelectedColumn, MoveRow-MoveColumn).

%printWinner(+Player)
/*
Imprime uma mensagem formatada para a vitória do jogador 1
*/
printWinner(1):-
	write('\n!!! Player1 (O) won !!!\n\n').

%printWinner(+Player)
/*
Imprime uma mensagem formatada para a vitória do jogador 2
*/
printWinner(-1):-
	write('\n!!! Player2 (X) won !!!\n\n').