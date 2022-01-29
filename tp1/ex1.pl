female(grace).
female(dede).
female(gloria).
female(barb).
female(claire).
female(manny).
female(cameron).
female(pameron).
female(haley).
female(lily).
female(poppy).
male(frank).
male(phil).
male(jay).
male(javier).
male(merle).
male(mitchell).
male(joe).
male(bo).
male(dylan).
male(alex).
male(luke).
male(rexford).
male(calhoun).
male(george).
parent(grace, phil).
parent(frank, phil).
parent(dede, claire).
parent(jay, claire).
parent(dede, mitchell).
parent(jay, mitchell).
parent(gloria, joe).
parent(jay, joe).
parent(gloria, manny).
parent(javier, manny).
parent(barb, cameron).
parent(merle, cameron).
parent(barb, pameron).
parent(merle, pameron).
parent(phil, haley).
parent(claire, haley).
parent(phil, alex).
parent(claire, alex).
parent(phil, luke).
parent(claire, luke).
parent(mitchell, lily).
parent(cameron, lily).
parent(mitchell, rexford).
parent(cameron, rexford).
parent(pameron, calhoun).
parent(bo, calhoun).
parent(dylan, george).
parent(haley, george).
parent(dylan, poppy).
parent(haley, poppy).

father(X,Y):-male(X), parent(X,Y).
mother(X,Y):-female(X), parent(X,Y).
grandparent(X,Z):-parent(X,Y), parent(Y,Z).
grandmother(X,Z):-female(X), parent(X,Y), parent(Y,Z).
grandfather(X,Z):-male(X), parent(X,Y), parent(Y,Z).
siblings(X,Y):- father(A,X),father(A,Y), mother(B,X), mother(B,Y), X\=Y.
uncle(X,Y):-siblings(X,A),parent(A,Y).
uncle(X,Y):-halfSiblings(X,A),parent(A,Y).
%halfSiblings(X,Y):-father(A,X),father(A,Y),mother(B,X),mother(C,Y),B\=C.
%halfSiblings(X,Y):-father(B,X),father(C,Y),mother(A,X),mother(A,Y),B\=C.
halfSiblings(X,Y):-parent(A,X),parent(A,Y),parent(B,X),parent(C,Y), B\=C,A\=B,A\=C.
ancestor(X,Y):-parent(X,Y).
ancestor(X,Y):-parent(Z,Y),ancestor(X,Z).
descendant(X,Y):-parent(Y,X).
descendant(X,Y):-parent(Z,X),descendant(Z,Y).