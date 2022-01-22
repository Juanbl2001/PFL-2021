%choose_move(GameState, Size, Player, Move)*/
/*
Selects a piece and a position to move (bot) if there are available moves for the player,
returning the move selected
Also prints the selected move
*/
:- use_module(library(random)).

select_move(GameState, Size, Player, Level, Move):-
    valid_moves(GameState, Size, Player, ListOfPossibleMoves),
    sleep(1),
    movePiecePositionBot(GameState, Size, Player, Level, ListOfPossibleMoves, Move),
    getSelAndMovePosition(Move, SelPosition, MovPosition),
    write('\nSelected: '), printPosition(SelPosition), nl,
    write('\nMoved to: '), printPosition(MovPosition), nl.

/*
If no available moves then select a piece to remove (bot), returning the move selected
Also prints the selected move
*/
select_move(GameState, Size, Player, Level, Move):-
    getPlayerInMatrix(GameState, Size, Player, ListOfPositions),
    sleep(1),
    removePiecePositionBot(GameState, Size, Player, Level, ListOfPositions, Move),
    write('\nRemoved: '), printPosition(Move), nl.


%movePiecePositionBot(+GameState, +Size, +Player, +Level, +ListOfPossibleMoves, -SelectedMove)
/*
Select a random Move from the ListOfPossibleMoves, 
returning the move selected
*/
movePiecePositionBot(_, _, _, 'Easy', ListOfPossibleMoves, SelectedMove):-
    random_member(SelectedMove, ListOfPossibleMoves).

removePiecePositionBot(_, _, _, 'Easy', ListOfPositions, SelPosition):-
    random_member(SelPosition, ListOfPositions).


%removePiecePositionBot(+GameState, +Size, +Player, +Level, +ListOfPositions, -Move)
/*
Select a random position of the current player positions to remove the piece
returning the position selected
*/

printPosition([]).
printPosition(Row-Column):-
	get_letter(Row, RowL),
	get_number(Column, ColumnL),
	format(" ~w~w ", [RowL,ColumnL]).

%printPositionList(+Position)
/*
Prints a list of several Positions (represented as [Position1, Position2, ...])
*/
printPositionsList([]).
printPositionsList([H|T]):-
	printPosition(H),
	printPositionsList(T).

%get_letter(+Row, -Letter)
/*
Gets Letter corresponding to the given Row index
*/
get_letter(Row, Letter) :-
	NewRow is Row + 65,
	char_code(Letter, NewRow).

%get_letter(+Column, -Number)
/*
Gets Number corresponding to the given Column index
*/
get_number(Column, Number) :-
	NewColumn is Column + 49,
	char_code(Number, NewColumn).