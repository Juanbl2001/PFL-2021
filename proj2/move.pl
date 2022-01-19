:- use_module(library(lists)).

%move(+GameState, +Player, +Move, -NewGameState)
/*
Move when available moves (and in this case Move is [SelectedRow-SelectedColumn, MoveRow-MoveColumn]),
replacing on board the selected position with empty space and moving position with player piece,
returning the board after the move
*/
move(GameState, Player, Move, NewGameState):-
    getSelAndMovePosition(Move, SelRow-SelColumn, FinalRow-FinalColumn),
    replaceInMatrix(GameState, SelRow, SelColumn, 0, UpdatedGameState),
    replaceInMatrix(UpdatedGameState, FinalRow, FinalColumn, Player, NewGameState).

/*
Move when no available moves (and in this case Move is SelectedRow-SelectedColumn),
replacing selected position on board with empty space,
returning the board after the remove
*/
move(GameState, _, Row-Column, NewGameState):-
    replaceInMatrix(GameState, Row, Column, 0, NewGameState).



%replaceInMatrix(+Matrix, +Row, +Column, +Value, -FinalMatrix)
/*
Replaces Value in given Row and Column of the Matrix
*/
replaceInMatrix(Matrix, Row, Column, Value, FinalMatrix) :-
	nth0(Row, Matrix, RowsList),
	replaceInList(Column, RowsList, Value, NewRows),
	replaceInList(Row, Matrix, NewRows, FinalMatrix).


%getSelAndMovePosition(+Move,-SelPosition,-MovPosition)
/*
Returns the current and the moving positions
*/
getSelAndMovePosition(Move, SelPosition, MovPosition):-
	nth0(0, Move, SelPosition),
	nth0(1, Move, MovPosition).

%select_move(+GameState, +Size, +Player, +PlayerType, -Move)
/*
Selects a piece and a position to move if there are available moves for the player,
returning the move selected
*/
select_move(GameState, Size, Player, 'Player', [SelectedPosition, MovePosition]):-
    valid_moves(GameState, Size, Player, _),
    selectPiece(GameState, Size, Player, SelectedPosition),
    movePiece(GameState, Size, Player, SelectedPosition, MovePosition).

/*
If no available moves then select a piece to remove, returning the move selected
*/
select_move(GameState, Size, Player, 'Player', SelectedPosition):-
    removePiece(GameState, Size, Player, SelectedPosition).



valid_moves(GameState, Size, Player, ListOfMoves):-
	write('TESTE1'),
    getPlayerPieces(GameState, Size, Player, ListOfPositions),
    getPossibleMoves(GameState, Size, Player, ListOfPositions, ListOfMoves),
	\+isEmpty(ListOfMoves).

%getPlayerPieces(+GameState,+Size,+Player,-ListOfPositions)
/*
Retuns on ListOfPositions the all the positions where there are a player's piece
*/
getPlayerPieces(GameState, Size, Player, ListOfPositions) :-
	getPlayerPieces(GameState, Size, 0, 0, Player, [], ListOfPositions), !.

%Base case, when the position is Row=8 Column=0, it stops (end of the board)
getPlayerPieces(_, Size, Row, Column, _, ListOfPositions, ListOfPositions):-
	checkLastRow(Row, Column, Size).

%If it is the player is that cell, then append that position and pass to the next position
getPlayerPieces(GameState, Size, Row, Column, Player, ListInterm, ListOfPositions):-
	getValue(GameState, Row, Column, Player),
	append(ListInterm, [Row-Column], NewList),
	nextPosition(Row, Column, Size, NextRow, NextColumn),
	getPlayerPieces(GameState, Size, NextRow, NextColumn, Player, NewList, ListOfPositions).

%If it is not the player in that cell, avance to the next position
getPlayerPieces(GameState, Size, Row, Column, Player, ListInterm, ListOfPositions):-
	nextPosition(Row, Column, Size, NextRow, NextColumn),
	getPlayerPieces(GameState, Size, NextRow, NextColumn, Player, ListInterm, ListOfPositions).

%getPossibleMoves(+GameState, +Size, +Player, +Positions, -ListOfPossibleMoves)
/*
For all the positions passed, it checks the available moves for each
the listOfPossibleMoves stores the moves as as [[SelectedRow-SelectedColumn, MoveRow-MoveColumn], ...]
*/

getPossibleMoves(GameState, Size, Player, Positions, ListOfPossibleMoves):-
	getPossibleMoves(GameState, Size, Player, Positions, [], ListOfPossibleMoves).

getPossibleMoves(_, _, _, [], ListOfPossibleMoves, ListOfPossibleMoves).
getPossibleMoves(GameState, Size, Player, [Row-Column|PosRest], ListInterm, ListOfPossibleMoves):-
	% write(Size),nl,
	% write(Row-Column), nl,
	checkMove(GameState, Size, Row, Column, Player, Moves),
	appendMoves(Row-Column, Moves, CurrentMoves),
	appendNotEmpty(ListInterm, CurrentMoves, NewList),
	getPossibleMoves(GameState, Size, Player, PosRest, NewList, ListOfPossibleMoves), !.



%checkMove(+GameState, +Size, +SelectedRow, +SelectedColumn, +Player, -ListOfMoves)
/*
Checks all possible "L" moves from the given position
Returns a list with all the possible moves for that piece (as [MoveRow1-MoveColumn1, ...])
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
    isEnemy(GameState, NewRow, NewCol, Player),
    UpLeftMove = [NewRow-NewCol].

checkUpLeftMove(_, _, _, _, _, []).

checkUpRightMove(GameState, Size, Row, Col, Player, UpRightMove):-
    Row>0,
	NewRow is Row - 2,
	NewCol is Col + 1,
	checkRowCol(Size, NewRow, NewCol),
    isEnemy(GameState, NewRow, NewCol, Player),
    UpRightMove = [NewRow-NewCol].

checkUpRightMove(_, _, _, _, _, []).

checkDownLeftMove(GameState, Size, Row, Col, Player, DownLeftMove):-
    Row<Size,
	NewRow is Row + 2,
	NewCol is Col - 1,
	checkRowCol(Size, NewRow, NewCol),
    isEnemy(GameState, NewRow, NewCol, Player),
    DownLeftMove = [NewRow-NewCol].

checkDownLeftMove(_, _, _, _, _, []).

checkDownRightMove(GameState, Size, Row, Col, Player, DownRightMove):-
    Row<Size,
	NewRow is Row + 2,
	NewCol is Col + 1,
	checkRowCol(Size, NewRow, NewCol),
    isEnemy(GameState, NewRow, NewCol, Player),
    DownRightMove = [NewRow-NewCol].

checkDownRightMove(_, _, _, _, _, []).

checkLeftUpMove(GameState, Size, Row, Col, Player, LeftUpMove):-
    Col>0,
	NewCol is Col - 2,
	NewRow is Row - 1,
	checkRowCol(Size, NewRow, NewRow),
    isEnemy(GameState, NewRow, NewCol, Player),
    LeftUpMove = [NewRow-NewCol].

checkLeftUpMove(_, _, _, _, _, []).

checkLeftDownMove(GameState, Size, Row, Col, Player, LeftDownMove):-
    Col>0,
	NewCol is Col - 2,
	NewRow is Row + 1,
	checkRowCol(Size, NewRow, NewCol),
	isEnemy(GameState, NewRow, NewCol, Player),
    LeftDownMove = [NewRow-NewCol].

checkLeftDownMove(_, _, _, _, _, []).

checkRightUpMove(GameState, Size, Row, Col, Player, RightUpMove):-
    Col<Size,
	NewCol is Col + 2,
	NewRow is Row - 1,
	checkRowCol(Size, NewRow, NewCol),
    isEnemy(GameState, NewRow, NewCol, Player),
    RightUpMove = [NewRow-NewCol].

checkRightUpMove(_, _, _, _, _, []).

checkRightDownMove(GameState, Size, Row, Col, Player, RightDownMove):-
    Col<Size,
	NewCol is Col+2,
	NewRow is Row + 1,
	checkRowCol(Size, NewRow, NewCol),
    isEnemy(GameState, NewRow, NewCol, Player),
    RightDownMove = [NewRow-NewCol].

checkRightDownMove(_, _, _, _, _, []).


checkRowCol(Size, Row, Col) :- Row >= 0, Row < Size,
							   Col >= 0, Col < Size.
					 		


%isEnemy(+Board,+Row,+Column,+Player)
/*
Checks if board value in the given position (row and column) is the current player's enemy
*/
isEnemy(Board, Row, Column, Player) :-
    getValue(Board, Row, Column, Enemy),
    Enemy is Player + 1.


getValue(GameState, Row, Column, Value) :- 	
	nth0(Row, Matrix, RowList),
	nth0(Column, RowList, Value).


%appendMoves(+Pos,+Moves,-RetList)
/*
Return on RetList a list of sublists with for each position moves like [PositionRow-PosionColumn, MoveRow-MoveColumn], ...]
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
If given L2 list is not empty, append it to L1 and result is L12
Base case, if L2 is an empty list then the result is L1
*/
appendNotEmpty(L1, [], L1).
appendNotEmpty(L1, L2, L12):-
	append(L1, L2, L12).


%nextPosition(+Row,+Column,+Size,-NextRow,-NextColumn)
/*
If the end of the column has not been reached, avance to the next collumn, remaining in the same row
*/
nextPosition(Row, Column, Size, Row, NextColumn):-
    NextColumn is Column + 1,
    NextColumn \== Size.
	
%nextPosition(+Row,+Column,+Length,-NextRow,-NextColumn)
/*
If the end of the column has been reached, avance to the next row, starting in the first column(0)
*/
nextPosition(Row, Column, Size, NextRow, 0):-
    NextColumn is Column + 1,
    NextColumn == Size,
    NextRow is Row + 1.


checkLastRow(Row, Column, Size) :- Row is Size,
                                  Column is 0.

								  
%isEmpty(+List)
/*
Checks if List is empty
*/
isEmpty([]).

validateContent(Board, Size, SelectedRow-SelectedColumn, Player) :- validatePlayer(Board, SelectedRow-SelectedColumn, Player),
                                                                    validateMove(Board, Size, SelectedRow-SelectedColumn, Player, _).


validatePlayer(Board, SelectedRow-SelectedColumn, Player) :- getValue(Board, SelectedRow, SelectedColumn, Player).
validatePlayer(_, _, _) :- write('\n! That is not your piece. Choose again !\n'), fail.


validateMove(Board, Size, SelectedRow-SelectedColumn, Player, ListOfMoves) :- write(Size),nl,
 																			  write(SelectedRow-SelectedColumn), nl,
																			   checkMove(Board, Size, SelectedRow, SelectedColumn, Player, ListOfMoves),
                                                                               \+isEmpty(ListOfMoves).

validateMove(_, _, _, _, _) :- write('\n! No available moves for this piece. Choose again !\n'), fail.


%replaceInList(+Index, +List, +Element, -NewList)
/*
Replaces an element in a List at a specified Index with Element
*/
replaceInList(Index, List, Element, NewList) :-
	nth0(Index, List, _, Rest),
	nth0(Index, NewList, Element, Rest).