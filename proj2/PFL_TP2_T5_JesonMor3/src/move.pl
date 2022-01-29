%move(+GameState, +Player, +Move, -NewGameState)
/*
Move quando os movimentos disponíveis (e neste caso Mover é [SelectedRow-SelectedColumn, MoveRow-MoveColumn]),
substituindo no tabuleiro a posição selecionada com espaço vazio e a posição de movimento 
com a peça do jogador, devolvendo o tabuleiro após o movimento
*/
move(GameState, Player, Move, NewGameState):-
    getSelAndMovePosition(Move, SelRow-SelColumn, FinalRow-FinalColumn),
    replaceInMatrix(GameState, SelRow, SelColumn, 0, UpdatedGameState),
    replaceInMatrix(UpdatedGameState, FinalRow, FinalColumn, Player, NewGameState).


%replaceInMatrix(+Matrix, +Row, +Column, +Value, -FinalMatrix)
/*
Substitui o valor em determinada linha e coluna da matriz
*/
replaceInMatrix(Matrix, Row, Column, Value, FinalMatrix) :-
	nth0(Row, Matrix, RowsList),
	replaceInList(Column, RowsList, Value, NewRows),
	replaceInList(Row, Matrix, NewRows, FinalMatrix).


%getSelAndMovePosition(+Move,-SelPosition,-MovPosition)
/*
Divide as duas posições
*/
getSelAndMovePosition(Move, SelPosition, MovPosition):-
	nth0(0, Move, SelPosition),
	nth0(1, Move, MovPosition).

%choose_move(+GameState, +Size, +Player, +PlayerType, -Move)
/*
Seleciona uma peça e uma posição para mover se houver movimentos disponíveis para o jogador, 
retornando o movimento selecionado
*/
choose_move(GameState, Size, Player, 'Player', [SelectedPosition, MovePosition]):-
    valid_moves(GameState, Size, Player, _),
    selectPiece(GameState, Size, Player, SelectedPosition),
    movePiece(GameState, Size, Player, SelectedPosition, MovePosition).


valid_moves(GameState, Size, Player, ListOfMoves):-
    getPlayerPieces(GameState, Size, Player, ListOfPositions),
    getPossibleMoves(GameState, Size, Player, ListOfPositions, ListOfMoves),
	\+isEmpty(ListOfMoves).

%getPlayerPieces(+GameState,+Size,+Player,-ListOfPositions)
/*
Retorna em ListOfPositions todas as posições onde há uma peça do jogador
*/
getPlayerPieces(GameState, Size, Player, ListOfPositions) :-
	getPlayerPieces(GameState, Size, 0, 0, Player, [], ListOfPositions), !.

%Caso base
getPlayerPieces(_, Size, Row, Column, _, ListOfPositions, ListOfPositions):-
	checkLastRow(Row, Column, Size).

%Se for o jogador que é essa célula, então anexe essa posição e passe para a próxima posição
getPlayerPieces(GameState, Size, Row, Column, Player, ListInterm, ListOfPositions):-
	getValue(GameState, Row, Column, Player),
	append(ListInterm, [Row-Column], NewList),
	nextPosition(Row, Column, Size, NextRow, NextColumn),
	getPlayerPieces(GameState, Size, NextRow, NextColumn, Player, NewList, ListOfPositions).

%Se não for o jogador naquela célula, avance para a próxima posição
getPlayerPieces(GameState, Size, Row, Column, Player, ListInterm, ListOfPositions):-
	nextPosition(Row, Column, Size, NextRow, NextColumn),
	getPlayerPieces(GameState, Size, NextRow, NextColumn, Player, ListInterm, ListOfPositions).

%getPossibleMoves(+GameState, +Size, +Player, +Positions, -ListOfPossibleMoves)
/*
Para todas as posições passadas, ele verifica os movimentos disponíveis para cada
*/

getPossibleMoves(GameState, Size, Player, Positions, ListOfPossibleMoves):-
	getPossibleMoves(GameState, Size, Player, Positions, [], ListOfPossibleMoves).

getPossibleMoves(_, _, _, [], ListOfPossibleMoves, ListOfPossibleMoves).
getPossibleMoves(GameState, Size, Player, [Row-Column|PosRest], ListInterm, ListOfPossibleMoves):-
	checkMove(GameState, Size, Row, Column, Player, Moves),
	appendMoves(Row-Column, Moves, CurrentMoves),
	appendNotEmpty(ListInterm, CurrentMoves, NewList),
	getPossibleMoves(GameState, Size, Player, PosRest, NewList, ListOfPossibleMoves), !.



%checkMove(+GameState, +Size, +SelectedRow, +SelectedColumn, +Player, -ListOfMoves)
/*
Verifica todos os movimentos "L" possíveis a partir da posição especificada.
Retorna uma lista com todos os movimentos possíveis para aquela peça (como [MoveRow1-MoveColumn1, ...])
*/
checkMove(GameState, Size, SelRow, SelColumn, Player, ListOfMoves) :-
    checkUpLeftMove(GameState, Size, SelRow, SelColumn, Player, UpMoveLeft),
	checkUpRightMove(GameState, Size, SelRow, SelColumn, Player, UpMoveRight),

    checkDownLeftMove(GameState, Size, SelRow, SelColumn, Player, DownMoveLeft),
	checkDownRightMove(GameState, Size, SelRow, SelColumn, Player, DownMoveRight),

    checkLeftUpMove(GameState, Size, SelRow, SelColumn, Player, LeftMoveUp),
	checkLeftDownMove(GameState, Size, SelRow, SelColumn, Player, LeftMoveDown),

    checkRightUpMove(GameState, Size, SelRow, SelColumn, Player, RightMoveUp),
	checkRightDownMove(GameState, Size, SelRow, SelColumn, Player, RightMoveDown),

    appendNotEmpty([], UpMoveLeft, L),
	appendNotEmpty(L, UpMoveRight, L1),
	appendNotEmpty(L1, DownMoveLeft, L2),
	appendNotEmpty(L2, DownMoveRight, L3),
	appendNotEmpty(L3, LeftMoveUp, L4),
	appendNotEmpty(L4, LeftMoveDown, L5),
	appendNotEmpty(L5, RightMoveUp, L6),
	appendNotEmpty(L6, RightMoveDown, ListOfMoves), !.


checkUpLeftMove(GameState, Size, Row, Col, Player, UpLeftMove):-
    Row>0,
	NewRow is Row - 2,
	NewCol is Col - 1,
	checkRowCol(Size, NewRow, NewCol),
    \+isPlayer(GameState, NewRow, NewCol, Player),
    UpLeftMove = [NewRow-NewCol].

checkUpLeftMove(_, _, _, _, _, []).

checkUpRightMove(GameState, Size, Row, Col, Player, UpRightMove):-
    Row>0,
	NewRow is Row - 2,
	NewCol is Col + 1,
	checkRowCol(Size, NewRow, NewCol),
    \+isPlayer(GameState, NewRow, NewCol, Player),
    UpRightMove = [NewRow-NewCol].

checkUpRightMove(_, _, _, _, _, []).

checkDownLeftMove(GameState, Size, Row, Col, Player, DownLeftMove):-
    Row<Size, 
	NewRow is Row + 2,
	NewCol is Col - 1,
	checkRowCol(Size, NewRow, NewCol),
    \+isPlayer(GameState, NewRow, NewCol, Player),
    DownLeftMove = [NewRow-NewCol].

checkDownLeftMove(_, _, _, _, _, []).

checkDownRightMove(GameState, Size, Row, Col, Player, DownRightMove):-
    Row<Size,
	NewRow is Row + 2,
	NewCol is Col + 1,
	checkRowCol(Size, NewRow, NewCol),
    \+isPlayer(GameState, NewRow, NewCol, Player),
    DownRightMove = [NewRow-NewCol].

checkDownRightMove(_, _, _, _, _, []).

checkLeftUpMove(GameState, Size, Row, Col, Player, LeftUpMove):-
    Col>0,
	NewCol is Col - 2,
	NewRow is Row - 1,
	checkRowCol(Size, NewRow, NewCol),
    \+isPlayer(GameState, NewRow, NewCol, Player),
    LeftUpMove = [NewRow-NewCol].

checkLeftUpMove(_, _, _, _, _, []).

checkLeftDownMove(GameState, Size, Row, Col, Player, LeftDownMove):-
    Col>0,
	NewCol is Col - 2,
	NewRow is Row + 1,
	checkRowCol(Size, NewRow, NewCol),
	\+isPlayer(GameState, NewRow, NewCol, Player),
    LeftDownMove = [NewRow-NewCol].

checkLeftDownMove(_, _, _, _, _, []).

checkRightUpMove(GameState, Size, Row, Col, Player, RightUpMove):-
    Col<Size,
	NewCol is Col + 2,
	NewRow is Row - 1,
	checkRowCol(Size, NewRow, NewCol),
    \+isPlayer(GameState, NewRow, NewCol, Player),
    RightUpMove = [NewRow-NewCol].

checkRightUpMove(_, _, _, _, _, []).

checkRightDownMove(GameState, Size, Row, Col, Player, RightDownMove):-
    Col<Size,
	NewCol is Col+2,
	NewRow is Row + 1,
	checkRowCol(Size, NewRow, NewCol),
    \+isPlayer(GameState, NewRow, NewCol, Player),
    RightDownMove = [NewRow-NewCol].

checkRightDownMove(_, _, _, _, _, []).


checkRowCol(Size, Row, Col) :- Row >= 0, Row < Size,
							   Col >= 0, Col < Size.
					 		


isPlayer(Board, Row, Column, Player) :-
	getValue(Board, Row, Column, Value),
	Value is Player.

getValue(Matrix, Row, Column, Value) :- 	
	nth0(Row, Matrix, RowList),
	nth0(Column, RowList, Value).


%appendMoves(+Pos,+Moves,-RetList)
/*
Retorna em RetList uma lista de sublistas com movimentos para cada posição como [PositionRow-PosionColumn, MoveRow-MoveColumn], ...]
*/
appendMoves(_, [], []).
appendMoves(Pos, Moves, RetList):-
	appendMoves(Pos, Moves, [], RetList).

appendMoves(_, [], RetList, RetList).
appendMoves(Pos, [Move | T], AuxList, RetList):-
	CompleteMove = [Pos, Move],
	append([CompleteMove], AuxList, NewAuxList),
	appendMoves(Pos, T, NewAuxList, RetList).


%appendNotEmpty(+L1,+L2,-L12)
/*
Se a lista L2 fornecida não estiver vazia, anexe-a a L1 e o resultado é L12
Caso base, se L2 for uma lista vazia, o resultado será L1
*/
appendNotEmpty(L1, [], L1).
appendNotEmpty(L1, L2, L12):-
	append(L1, L2, L12).


%nextPosition(+Row,+Column,+Size,-NextRow,-NextColumn)
/*
Se o final da coluna não foi alcançado, avance para a próxima coluna, permanecendo na mesma linha
*/
nextPosition(Row, Column, Size, Row, NextColumn):-
    NextColumn is Column + 1,
    NextColumn \== Size.
	
%nextPosition(+Row,+Column,+Length,-NextRow,-NextColumn)
/*
Se o final da coluna foi alcançado, avance para a próxima linha, começando na primeira column(0)
*/
nextPosition(Row, Column, Size, NextRow, 0):-
    NextColumn is Column + 1,
    NextColumn == Size,
    NextRow is Row + 1.


checkLastRow(Row, Column, Size) :- Row is Size,
                                  Column is 0.

								  
%isEmpty(+List)
/*
Verifica se a Lista está vazia
*/
isEmpty([]).

validateContent(Board, Size, SelectedRow-SelectedColumn, Player) :- validatePlayer(Board, SelectedRow-SelectedColumn, Player),
                                                                    validateMove(Board, Size, SelectedRow-SelectedColumn, Player, _).


validatePlayer(Board, SelectedRow-SelectedColumn, Player) :- getValue(Board, SelectedRow, SelectedColumn, Player).
validatePlayer(_, _, _) :- write('\n! That is not your piece. Choose again !\n'), fail.


validateMove(Board, Size, SelectedRow-SelectedColumn, Player, ListOfMoves) :- 
	checkMove(Board, Size, SelectedRow, SelectedColumn, Player, ListOfMoves),
	\+isEmpty(ListOfMoves).

validateMove(_, _, _, _, _) :- write('\n! No available moves for this piece. Choose again !\n'), fail.

tryMove(Board, Size, Player, SelectedRow-SelectedColumn, MoveRow-MoveColumn) :-     
    checkMove(Board, Size, SelectedRow, SelectedColumn, Player, ListOfMoves),
    member(MoveRow-MoveColumn, ListOfMoves).

tryMove(_, _, _, _, _) :- write('\n! You can´t move to that position. Choose again !\n'), fail.



%replaceInList(+Index, +List, +Element, -NewList)
/*
Substitui um elemento em uma lista em um índice especificado por elemento
*/
replaceInList(Index, List, Element, NewList) :-
	nth0(Index, List, _, Rest),
	nth0(Index, NewList, Element, Rest).