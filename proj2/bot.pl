%choose_move(GameState, Size, Player, Move)*/
/*
Selects a piece and a position to move (bot) if there are available moves for the player,
returning the move selected
Also prints the selected move
*/
choose_move(GameState, Size, Player, Move):-
    valid_moves(GameState, Size, Player, ListOfPossibleMoves),
    sleep(1),
    movePiecePositionBot(GameState, Size, Player, ListOfPossibleMoves, Move),
    getSelAndMovePosition(Move, SelPosition, MovPosition),
    write('\nSelected: '), printPosition(SelPosition), nl,
    write('\nMoved to: '), printPosition(MovPosition), nl.

/*
If no available moves then select a piece to remove (bot), returning the move selected
Also prints the selected move
*/
choose_move(GameState, Size, Player, Move):-
    getPlayerInMatrix(GameState, Size, Player, ListOfPositions),
    sleep(1),
    removePiecePositionBot(GameState, Size, Player, ListOfPositions, Move),
    write('\nRemoved: '), printPosition(Move), nl.


%movePiecePositionBot(+GameState, +Size, +Player, +Level, +ListOfPossibleMoves, -SelectedMove)
/*
Select a random Move from the ListOfPossibleMoves, 
returning the move selected
*/
movePiecePositionBot(_, _, _, ListOfPossibleMoves, SelectedMove):-
    random_member(SelectedMove, ListOfPossibleMoves).

/*
Select the current best move (highest value)
returning the move selected
*/


%removePiecePositionBot(+GameState, +Size, +Player, +Level, +ListOfPositions, -Move)
/*
Select a random position of the current player positions to remove the piece
returning the position selected
*/
removePiecePositionBot(_, _, _, ListOfPositions, SelPosition):-
    random_member(SelPosition, ListOfPositions).
*
%getFFSpots(+GameState, +Size, -ListOfFFSpots)
/*
Returna a list with all the independent Flood Fill spots positions of the board
*/
getFFSpots(GameState, Size, ListOfFFSpots):-
    getFFSpots(GameState, Size, 0, 0, ListOfFFSpots).

%getFFSpots(+GameState, +Size, +Row, +Column, -ListOfFFSpots)
/*
Base case, when the position is Row=8 Column=0, it stops (end of the board)
*/
getFFSpots(_, Size, Row, Column, []):-
    checkEndPosition(Row, Column, Size).
/*
Tries to Flood Fill current position and calls itself recursively in the next position
*/
getFFSpots(GameState, Size, Row, Column, ListOfFFSpots):-
    tryFloodFill(GameState, Size, Row, Column, UpdatedGameState),
    nextPosition(Row, Column, Size, NextRow, NextColumn),
    getFFSpots(UpdatedGameState, Size, NextRow, NextColumn, TempFFSpots),
    append(TempFFSpots, [Row-Column], ListOfFFSpots).

/*
If Flood Fill failed try again in the next position
*/
getFFSpots(GameState, Size, Row, Column, ListOfFFSpots):-
    nextPosition(Row, Column, Size, NextRow, NextColumn),
    getFFSpots(GameState, Size, NextRow, NextColumn, ListOfFFSpots).


%getSpotsValues(+GameState, +Size, +ListOfFFSpots, -ListOfValues)
/*
Base case, when the ListOfFFSpots is empty ListOfValues is empty as well
*/
getSpotsValues(_, _, [], []).

/*
Flood Fills the spot in the head of ListOfFFSpots list and gets its value
Calls itself recursively with the tail of the list
*/
getSpotsValues(GameState, Size, [Row-Column|RestFFSpots], ListOfValues):-
    floodFill(GameState, Size, Row, Column, 0, 2, UpdatedGameState),
    getValuesInAllRows(UpdatedGameState, Size, ListOfRowsValues),
    sequenceOfNon0(ListOfRowsValues, SequenceValue),
    getSpotsValues(GameState, Size, RestFFSpots, TempValues),
    append(TempValues, [SequenceValue], ListOfValues).
    

%getValuesInAllRows(+GameState, +Size, -ListResult)
/*
Returns a list with the numbers of Flood Fill characters for each row
*/
getValuesInAllRows(GameState, Size, ListOfRowsValues):-
    getValuesInAllRows(GameState, Size, 0, ListOfRowsValues).

%getValuesInAllRows(+GameState, +Size, +RowIndex, -ListResult)
/*
Base case, RowIndex is equal to board size which means every row was counted
*/
getValuesInAllRows(_, Size, Size, []).

/*
Counts the number of Flood Fill characters in the current row (head of the list)
Call itself recursively to get the other rows counts
*/
getValuesInAllRows([Row|RestRows], Size, RowIndex, ListOfRowsValues):-
    countElement(2, Row, Amount),
    NextRowIndex is RowIndex+1,
    getValuesInAllRows(RestRows, Size, NextRowIndex, TempRowsValues),
    append(TempRowsValues, [Amount], ListOfRowsValues).