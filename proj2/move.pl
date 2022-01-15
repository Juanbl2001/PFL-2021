

move(GameState, Player, Move, NewGameState):-


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
    checkDownMove(GameState, Size, SelRow, SelColumn - 1, Player, DownMoveLeft),
	checkDownMove(GameState, Size, SelRow, SelColumn + 1, Player, DownMoveRight),

    checkUpMove(GameState, Size, SelRow, SelColumn - 1, Player, UpMoveLeft),
	checkUpMove(GameState, Size, SelRow, SelColumn + 1, Player, UpMoveRight),

    checkLeftMove(GameState, Size, SelRow + 1, SelColumn, Player, LeftMoveUp),
	checkLeftMove(GameState, Size, SelRow - 1, SelColumn, Player, LeftMoveDown),

    checkRightMove(GameState, Size, SelRow + 1, SelColumn, Player, RightMoveUp),
	checkRightMove(GameState, Size, SelRow - 1, SelColumn, Player, RightMoveDown),

    appendNotEmpty([], DownMoveLeft, L),
	appendNotEmpty(L, DownMoveRight, L1),
	appendNotEmpty(L1, UpMoveLeft, L2),
	appendNotEmpty(L2, UpMoveRight, L3),
	appendNotEmpty(L3, LeftMoveUp, L4),
	appendNotEmpty(L4, LeftMoveDown, L5),
	appendNotEmpty(L5, RightMoveUp, L6),
	appendNotEmpty(L6, RightMoveDown, ListOfMoves), !.


checkUpMove(GameState, _, Row, Col, Player, UpMove):-
    Row>0,
	NewRow is Row-2,
    isEnemy(GameState, NewRow, Col, Player),
    UpMove = [NewRow-Col].

checkUpMove(_, _, _, _, _, []).

checkDownMove(GameState, Size, Row, Col, Player, DownMove):-
    Row<Size,
	NewRow is Row+2,
    isEnemy(GameState, NewRow, Col, Player),
    DownMove = [NewRow-Col].

checkDownMove(_, _, _, _, _, []).

checkLeftMove(GameState, _, Row, Col, Player, LeftMove):-
    Col>0,
	NewCol is Col-2,
    isEnemy(GameState, Row, NewCol, Player),
    LeftMove = [Row-NewCol].

checkLeftMove(_, _, _, _, _, []).

checkRightMove(GameState, Size, Row, Col, Player, RightMove):-
    Col<Size,
	NewCol is Col+2,
    isEnemy(GameState, Row, NewCol, Player),
    RightMove = [Row-NewCol].

checkRightMove(_, _, _, _, _, []).


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
nextPosition(Row, Column, Size, NextRow, NextColumn):-
    NextColumn is Column + 1,
    NextColumn == Size,
    NextColumn is 0,
    NextRow is Row + 1.


checkLastRow(Row, Column, Size) :- Row is Size,
                                  Column is 0.

								  
%isEmpty(+List)
/*
Checks if List is empty
*/
isEmpty([]).