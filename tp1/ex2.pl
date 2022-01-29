leciona(algoritmos,adalberto).
leciona(base_de_dados,bernardete).
leciona(compiladores,capitolino).
leciona(estatistica,diogenes).
leciona(redes,ermelinda).

frequenta(algoritmos,alberto).
frequenta(algoritmos,bruna).
frequenta(algoritmos,cristina).
frequenta(algoritmos,diogo).
frequenta(algoritmos,eduarda).

frequenta(base_de_dados,antonio).
frequenta(base_de_dados,bruno).
frequenta(base_de_dados,cristina).
frequenta(base_de_dados,duarte).
frequenta(base_de_dados,eduardo).

frequenta(compiladores,alberto).
frequenta(compiladores,bernardo).
frequenta(compiladores,clara).
frequenta(compiladores,diana).
frequenta(compiladores,eurico).

frequenta(estatistica,antonio).
frequenta(estatistica,bruna).
frequenta(estatistica,claudio).
frequenta(estatistica,duarte).
frequenta(estatistica,eva).

frequenta(redes,alvaro).
frequenta(redes,beatriz).
frequenta(redes,claudio).
frequenta(redes,diana).
frequenta(redes,eduardo).

aluno(X,Y):-
    frequenta(U,X), 
    leciona(U,Y).

professor(X,Y):-
    aluno(Y,X).

colega(X,Y):-
    leciona(_,Y),
    leciona(_,X),
    X\=Y.

colega(X,Y):-
    frequenta(U,Y),
    frequenta(U,X),
    X\=Y.
