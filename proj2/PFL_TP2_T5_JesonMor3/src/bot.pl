%choose_move(GameState, Size, Player, Move)*/
/*
Seleciona uma peça e uma posição para mover (bot) se houver movimentos 
disponíveis para o jogador,retornando o movimento selecionado. 
Também imprime o movimento selecionado
*/
choose_move(GameState, Size, Player, Level, Move):-
    valid_moves(GameState, Size, Player, ListOfPossibleMoves),
    sleep(1),
    movePiecePositionBot(GameState, Size, Player, Level, ListOfPossibleMoves, Move),
    member(Move, ListOfPossibleMoves),
    getSelAndMovePosition(Move, SelPosition, MovPosition),
    write('\nSelected: '), printPosition(SelPosition), nl,
    write('\nMoved to: '), printPosition(MovPosition), nl.


%movePiecePositionBot(+GameState, +Size, +Player, +Level, +ListOfPossibleMoves, -SelectedMove)
/*
    Selecione um movimento aleatório do ListOfPossibleMoves, 
    retornando o movimento selecionado
*/
movePiecePositionBot(_, _, _, 'Easy', ListOfPossibleMoves, SelectedMove):-
    random_member(SelectedMove, ListOfPossibleMoves).


printPosition([]).
printPosition(Row-Column):-
	get_letter(Row, RowL),
	get_number(Column, ColumnL),
	format(" ~w~w ", [RowL,ColumnL]).

%printPositionList(+Position)
/*
    Imprime uma lista de várias posições (representadas como [Position1, Position2, ...])
*/
printPositionsList([]).
printPositionsList([H|T]):-
	printPosition(H),
	printPositionsList(T).

%get_number(+Column, -Number)
/*
    Obtém o número correspondente ao índice de coluna fornecido
*/
get_number(Column, Number) :-
	NewColumn is Column + 49,
	char_code(Number, NewColumn).