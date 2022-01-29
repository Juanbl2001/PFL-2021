jogo(1,sporting,porto,1-2).
jogo(1,maritimo,benfica,2-0).
jogo(2,sporting,benfica,0-2).
jogo(2,porto,maritimo,1-0).
jogo(3,maritimo,sporting,1-1).
jogo(3,benfica,porto,0-2).
treinadores(porto,[[1-3]-sergio_conceicao]).
treinadores(sporting,[[1-2]-silas,[3-3]-ruben_amorim]).
treinadores(benfica,[[1-3]-bruno_lage]).
treinadores(maritimo,[[1-3]-jose_gomes]).

n_treinadores(E,N):-
        treinadores(E,T),
        length(T,N).

treinadorInLista(Treinador, [Jornadas-Treinador | _], Jornadas).
treinadorInLista(Treinador, [_ | Rest], Jornadas) :- treinadorInLista(Treinador, Rest, Jornadas).

n_jornadas_treinador(Treinador, NumeroJornadas) :-
    treinadores(_, Treinadores),
    treinadorInLista(Treinador, Treinadores, [PrimeiraJornada-UltimaJornada]),
    NumeroJornadas is UltimaJornada - PrimeiraJornada + 1.

ganhou(Jornada, EquipaVencedora, EquipaDerrotada) :-
    jogo(Jornada, EquipaVencedora, EquipaDerrotada, GolosCasa-GolosFora),
    GolosCasa > GolosFora.
ganhou(Jornada, EquipaVencedora, EquipaDerrotada) :-
    jogo(Jornada, EquipaDerrotada, EquipaVencedora, GolosCasa-GolosFora),
    GolosCasa < GolosFora.

:-op(180, fx, o).
:-op(200, xfx, venceu).

o X venceu o Y :- ganhou(_, X, Y).

predX(N,N,_).
predX(N,A,B):-
    !,
    A \= B,
    A1 is A + sign(B - A),
    predX(N,A1,B).