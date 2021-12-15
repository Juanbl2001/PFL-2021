fatorial(0,1).
fatorial(N,F) :-
        N>0,
        Prev is N-1,
        fatorial(Prev,R),
        F is R*N.

somaRec(0,0).
somaRec(1,1).
somaRec(N,S):-
        N>0,
        Prev is N-1,
        somaRec(Prev,R),
        S is R+N.

fibonacci(0,0).
fibonacci(1,1).
fibonacci(N,S):-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fibonacci(N1, S1),
        fibonacci(N2, S2),
        S is S1 + S2.


isPrime( 2 ) .
isPrime( P ) :-
        P > 2 ,
        isDivisible( P , P-1 ) .

isDivisible( P , X ) :-
        X > 1,
        P mod X =\= 0 ,
        isDivisible( P , X-1 ) .
isDivisible( _ , X ) :-
        1 is X .
