
initial(Board):- makingBoard(9, 9, [], Board).



makingBoard( _ , 0, Board, Board). %This is final state, where there are no more lines to be written, so BoardUnderConstruction will be equal to Board
%Note: even index lines will begin with a white piece, and odd index ones with a black piece

makingBoard(LineLength, RemainingLines, BoardUnderConstruction, Board) :- 
										X is LineLength - RemainingLines, X =:= 0, %Checking if it's an even index line
										lineGenerator(black, LineLength, Line), %Creating a the line itself, begining with a white piece
										append(BoardUnderConstruction, [Line], NewBoardUnderConstruction), %Appeding this line to our incomplete board
										NewRemainingLines is RemainingLines - 1, %We've just written one line, so we subtract one from the remaining lines to be written
										makingBoard(LineLength, NewRemainingLines, NewBoardUnderConstruction, Board). %Recursive call, to write the remaining lines

makingBoard(LineLength, RemainingLines, BoardUnderConstruction, Board) :- 
										X is LineLength - RemainingLines, X =:= 9, %Checking if it's an odd index line
										lineGenerator(white, LineLength, Line), %Creating a the line itself, begining with a black piece
										append(BoardUnderConstruction, [Line], NewBoardUnderConstruction), %Appeding this line to our incomplete board
										NewRemainingLines is RemainingLines - 1, %We've just written one line, so we subtract one from the remaining lines to be written
										makingBoard(LineLength, NewRemainingLines, NewBoardUnderConstruction, Board). %Recursive call, to write the remaining lines


makingBoard(LineLength, RemainingLines, BoardUnderConstruction, Board) :- 
										X is LineLength - RemainingLines, X \= 9, X \= 0 %Checking if it's an odd index line
										lineGenerator(white, LineLength, Line), %Creating a the line itself, begining with a black piece
										append(BoardUnderConstruction, [Line], NewBoardUnderConstruction), %Appeding this line to our incomplete board
										NewRemainingLines is RemainingLines - 1, %We've just written one line, so we subtract one from the remaining lines to be written
										makingBoard(LineLength, NewRemainingLines, NewBoardUnderConstruction, Board). %Recursive call, to write the remaining lines

                                

%First, we're going to begin by building the line from scratch
lineGenerator(InicialColour, LineLength, Line) :- lineGenerator(InicialColour, LineLength, [], Line).


lineGenerator( _ , 0, Line, Line). %This is final state, where there are no more cells to be written, so LineUnderConstruction will be equal to Line

lineGenerator(white, LineLength, LineUnderConstruction, Line) :- 
														append(LineUnderConstruction, [[1, white]], NewLineUnderConstruction), %Writting the cell in the line, in this case a white piece
														NewLineLength is LineLength - 1, %We've just written one cell, so we subtract one from the remaining cells to be written
														lineGenerator(black, NewLineLength, NewLineUnderConstruction, Line). %Recursive call, to build the rest of the line, this time the cell to be written must have a black piece
														
lineGenerator(black, LineLength, LineUnderConstruction, Line) :- 
														append(LineUnderConstruction, [[1, black]], NewLineUnderConstruction), %Writting the cell in the line, in this case a black piece
														NewLineLength is LineLength - 1, %We've just written one cell, so we subtract one from the remaining cells to be written
														lineGenerator(white, NewLineLength, NewLineUnderConstruction, Line). %Recursive call, to build the rest of the line, this time the cell to be written must have a black piece


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

gameState(initial). %inicial state
playIO(gameState).