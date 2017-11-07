% --------------------------------------------- %
% ---------------- Ejercicio 1 ---------------- %
% --------------------------------------------- %
padre(juan,carlos).
padre(juan,luis).
padre(carlos,daniel).
padre(carlos,diego).
padre(luis,pablo).
padre(luis,manuel).
padre(luis,ramiro).

%abuelo(?X,?Y)
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

%hijo(?X,?Y)
hijo(X,Y) :- padre(Y,X).

%hermano(?X,?Y)
hermano(X,Y) :- padre(Z,X), padre(Z,Y), X \= Y.

%descendiente(?X,?Y)
descendiente(X,Y) :- hijo(X,Y).
descendiente(X,Y) :- hijo(X,Z), descendiente(Z,Y).

%ancestro(X,X).
ancestro(X,Y) :- padre(X,Y).
%ancestro(X,Y) :- padre(Z,Y), padre(X,Z).
ancestro(X,Y) :-  padre(X,Z), ancestro(Z,Y).

% --------------------------------------------- %
% ---------------- Ejercicio 2 ---------------- %
% --------------------------------------------- %
vecino(X,Y,[_|Ls]) :- vecino(X,Y,Ls).
vecino(X,Y, [X|[Y|_]]).

% --------------------------------------------- %
% ---------------- Ejercicio 3 ---------------- %
% --------------------------------------------- %
natural(0).
natural(suc(X)) :- natural(X).
menorOIgual(X,X).
menorOIgual(X,suc(Y)) :- menorOIgual(X,Y).

% --------------------------------------------- %
% ---------------- Ejercicio 4 ---------------- %
% --------------------------------------------- %
%concatenar(?Lista1,?Lista2,?Lista3)
concatenar([],Xs,Xs).
%concatenar(Xs,[],Xs).
concatenar([X|Xs],Ys,[X|Ls]) :- concatenar(Xs,Ys,Ls).

% --------------------------------------------- %
% ---------------- Ejercicio 5 ---------------- %
% --------------------------------------------- %
%I)
%last(?L,?U)
%last([U],U).
%last([_|Xs],U) :- last(Xs,U).
last(L,U) :- concatenar(_,[U],L).

%II)
%reverse(+L1,-L2)
reverse([],[]).
reverse([X|Xs],Ls) :- reverse(Xs,Ys), concatenar(Ys,[X],Ls).

%III)
%maxList(+L,-M)
maxList([X],X).
maxList([X|Xs],M) :- M > X, maxList(Xs,M).
maxList([X|Xs],X) :- M =< X, maxList(Xs,M).

%minList(+L,-M)




%IV)
%prefijo(?P,+L)
prefijo(Xs,L) :- concatenar(Xs,_,L), Xs \= [].

%V)
%sufijo(?P,+L)
sufijo(Xs,L) :- concatenar(_,Xs,L), Xs \= [].

%VI)
%sublista(?P,+L)
sublista([],[]).
sublista(L,Xs) :- sufijo(P,Xs), prefijo(L,P), Xs \= [].


%VI)
pertenece(X,L) :- sublista([X],L).


% --------------------------------------------- %
% ---------------- Ejercicio 6 ---------------- %
% --------------------------------------------- %
%aplanar(+Xs,-Yx)
aplanar([],[]).
aplanar([[]|Xs],Ys) :- aplanar(Xs,Ys).
%aplanar([X],[X]) :- X \= [_|_].
aplanar([X|Xs],Ys) :- X \=[] , X \= [_|_], aplanar(Xs,Y), concatenar([X],Y,Ys).
aplanar([X|Xs],Ys) :- X = [_|_], aplanar(X,Y), aplanar(Xs,Ls), concatenar(Y,Ls,Ys).

% --------------------------------------------- %
% ---------------- Ejercicio 7 ---------------- %
% --------------------------------------------- %
%palindromo(+L1,-L2)
palindromo(L1,Res) :- reverse(L1,L2), concatenar(L1,L2,Res).

%doble(?L1,?L2)
doble([],[]).
doble([X|Xs],Ys) :- Ys = [X,X|L], doble(Xs,L).

%iesimo(?I, +L, -X)
iesimo(1,[X|_],X).
iesimo(N,[_|Xs],Y) :- N > 1, B is N - 1, iesimo(B,Xs,Y).

% --------------------------------------------- %
% ---------------- Ejercicio 8 ---------------- %
% --------------------------------------------- %
desde(X,X).
desde(X,Y) :- nonvar(Y), Y > X.
desde(X,Y) :- var(Y), N is X + 1, desde(N,Y).


% --------------------------------------------- %
% ---------------- Ejercicio 9 ---------------- %
% --------------------------------------------- %

%interseccion(+L1, +L2, -L3)
interseccion(_,[],[]).
interseccion([],_,[]).
%interseccion([X|Xs], [Y|Ys], Ls) :- X \= Y, interseccion([X|Xs],Ys,Ls).
%interseccion([X|Xs], [X|Ys], [X|Ls]) :- interseccion(Xs,Ys,Ls).
interseccion([X|Xs], L, [X|Ys]) :- pertenece(X,L), borrar(L,X,L2), borrar(Xs,X,L3), interseccion(L3,L2,Ys).
interseccion([X|Xs], L, Ys) :- not(pertenece(X,L)), borrar(Xs,X,L3), interseccion(L3,L,Ys).

%split(N, L, L1, L2)
%split(0,L,[],L).
split(N,L,L1,L2) :- length(L,Len), Len >= N, length(L1,N), concatenar(L1,L2,L).
    
%borrar(+L, +E, -L2)
borrar([],_,[]).
borrar([X|Xs], X, Ys) :- borrar(Xs,X,Ys).
borrar([X|Xs], E, [X|Ys]) :- X \= E, borrar(Xs,E,Ys).

%sacarDuplicados+L1, -L2)
sacarDuplicados([],[]).
sacarDuplicados([X|Xs],[X|Ys]) :- borrar(Xs,X,Ls), sacarDuplicados(Ls,Ys).

%reparto(+L, +N, -Llistas)
reparto(Xs, 1, [Xs]).
reparto([],N,[[]|Ys]) :- N > 1, N1 is N - 1, reparto([],N1,Ys). 
reparto([X|Xs], N, [[X]|Ys]) :- N > 1, N1 is N - 1, reparto(Xs,N1,Ys).

%reparto(Xs,N,Ys) :- length(Ys,N), forall(member(M,Ys), sublista(M,Xs)), aplanar(Ys,Xs).

%repartoSinVacias(+L, -Llistas)
%reparto(Xs, N, Ys) :- length(Ys,.
%reparto([],N,[[]|Ys]) :- N > 1, N1 is N - 1, reparto([],N1,Ys). 
%reparto([X|Xs], N, [[X]|Ys]) :- N > 1, N1 is N - 1, reparto(Xs,N1,Ys).


% --------------------------------------------- %
% ---------------- Ejercicio 10 --------------- %
% --------------------------------------------- %

%intercalar(L1, L2, L3)
intercalar(L1, L2, L3) :- intercalarAux(1,L1,L2,L3).

%intercalarAux(N,L1,L2,L3)
intercalarAux(_,[],L2,L2).
intercalarAux(_,L1,[],L1).
intercalarAux(1,[X|Xs],L2,[X|L3]) :- L2 \= [], intercalarAux(2,Xs,L2,L3).
intercalarAux(2,L1,[X|Xs],[X|L3]) :- L1 \= [], intercalarAux(1,L1,Xs,L3).


% --------------------------------------------- %
% ---------------- Ejercicio 11 --------------- %
% --------------------------------------------- %

% PREGUNTAR



% --------------------------------------------- %
% ---------------- Ejercicio 12 --------------- %
% --------------------------------------------- %

vacio(nill).

raiz(bin(_,R,_),R).

altura(nill,0).
altura(bin(I,_,D), Res) :- altura(I,N1), altura(D,N2), Res is 1 + max(N1,N2).

cantidadDeNodos(nill,0).
cantidadDeNodos(bin(I,_,D), Res) :- cantidadDeNodos(I,N1), cantidadDeNodos(D,N2), Res is 1 + N1 + N2.



% --------------------------------------------- %
% ---------------- Ejercicio 13 --------------- %
% --------------------------------------------- %

%inorder(+AB, -Lista)
inorder(nill,[]).
inorder(bin(I,R,D), Res) :- inorder(I,Is), inorder(D,Ds), concatenar(Is,[R],L), concatenar(L,Ds,Res).



% --------------------------------------------- %
% ---------------- Ejercicio 14 --------------- %
% --------------------------------------------- %

coprimos(X,Y) :- desde(2,N), between(2,N,X), Y is N - X, Y \= 1, 1 is gcd(X,Y).



% --------------------------------------------- %
% ---------------- Ejercicio 15 --------------- %
% --------------------------------------------- %

cuadradoSemiLatino(N,X) :- desde(0,I), length(X,N), forall(member(M,X), length(M,N)). 
