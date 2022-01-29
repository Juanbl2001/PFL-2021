%a
fatorial(0,1).

fatorial(N,F):- N>0,
    N1 is N-1,
    fatorial(N1,PrevFac),
    F is PrevFac*N.

%b
somaRec(1,1).
somaRec(N,Sum):-
        N>0,
        N1 is N-1,
        somaRec(N1,PrevSum),
        Sum is PrevSum+N.

%c
fibonacci(0,0).
fibonacci(1,1).
fibonacci(N,F):-
        N>1,
        N1 is N-1,
        N2 is N-2,
        fibonacci(N1,F1),
        fibonacci(N2,F2),
        F is F1+F2.

%d


divisible(X,Y):-
        (sqrt(X))>=Y,
        0 is (X mod Y).

divisible(X,Y):-
        (sqrt(X))>=Y,
        NewY is Y+1,
        divisible(X,NewY).

isPrime(N):-
    N>=2,
    Div is 2,
    \+ divisible(N,Div).


